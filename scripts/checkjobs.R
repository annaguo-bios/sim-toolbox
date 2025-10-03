# Create a single stdin connection to reuse
stdin_con <- file("stdin", open="r")

# Function to read input that works with Rscript
read_input <- function(prompt, con=stdin_con) {
  cat(prompt)
  return(readLines(con, n=1))
}

## default paramaters ## ====
estimator <- c('TMLE','Onestep','EstEquation')
n.vec <- c(500,1000,2000)
nsim <- 1000
missjob <- data.frame(n=integer(),
                      i=integer())

cat("\n--- Collecting inputs ---\n")
# sample size
n_input <- read_input(paste0("Enter sample size to check (comma-separated), or press enter to use default (default: ", paste(n.vec, collapse=", "), "): "))

if (n_input != ""){
  n.vec <- as.numeric(unlist(strsplit(n_input, ",")))
  cat("Using sample sizes:", paste(n.vec, collapse=", "), "\n")
} else {
  cat("Using default sample sizes:", paste(n.vec, collapse=", "), "\n")
}

# estimator
estimator_input <- read_input(paste0("Enter estimators to check (comma-separated), or press enter to use default (default: ", paste(estimator, collapse=", "), "): "))
if (estimator_input != ""){
  estimator <- unlist(strsplit(estimator_input, ","))
  cat("Using estimators:", paste(estimator, collapse=", "), "\n")
} else {
  cat("Using default estimators:", paste(estimator, collapse=", "), "\n")
}

# nsim
nsim_input <- read_input(paste0("Enter number of simulations (nsim) (default: ", nsim, "): "))
if (nsim_input != ""){
  nsim <- as.numeric(nsim_input)
  cat("Using nsim:", nsim, "\n")
} else {
  cat("Using default nsim:", nsim, "\n")
}

## check all exist jobs: e/output/output_n_i.Rdata ##====
# if i>nsim, rename the job with i <- i%%nsim
cat("\n--- Check for duplicated/ill-named files ---\n")
duplicated_results <- c()
renamed <- c()

for (e in estimator){
  output_dir <- file.path(e, "/output")
  
  if (dir.exists(output_dir)){
    files <- list.files(output_dir, pattern="^output_.*\\.Rdata$", full.names=TRUE)
    
    for (file in files){
      basename <- basename(file)
      
      # Pattern: output_n_i.Rdata
      matches <- regmatches(basename, regexec("^output_([0-9]+)_([0-9]+)\\.Rdata$", basename))
      
      if (length(matches[[1]]) == 3){
        n_val <- as.numeric(matches[[1]][2])
        i_val <- as.numeric(matches[[1]][3])
        
        # If i > nsim, rename the file
        if (i_val > nsim){
          adjusted_i <- i_val %% nsim
          duplicated_results <- c(duplicated_results, paste0("output_", n_val, "_", i_val, ".Rdata")) # keep a record of the name
          
          if (adjusted_i == 0) adjusted_i <- nsim # 2000%%1000 would be renamed 1000
          
          new_filename <- file.path(output_dir, paste0("output_", n_val, "_", adjusted_i, ".Rdata"))
          
          
          if (!file.exists(new_filename)){ # only rename if the new filename doesn't already exist
            file.rename(file, new_filename)
            renamed <- c(renamed, T)
            cat("Renamed:", basename, "->", basename(new_filename), "\n")
            
          } else {
            cat("Warning: Target file already exists, skipping rename of", basename, "\n")
            renamed <- c(renamed, F)
          }
        }
      }
    }
  }
}

if (length(duplicated_results) > 0){
  cat("Total files renamed:", sum(renamed), "out of", length(duplicated_results), "\n")
  write.csv(data.frame(duplicated_results, renamed), "duplicated_results.csv",row.names = F, col.names = F)
} else {
  cat("No files needed renaming.\n")
}

## check the existence of results ##====
cat("\n--- Check missing results ---\n")
for (e in estimator){
  for (n in n.vec){
    for (i in 1:nsim){
      output_file <- file.path(e, "/output/", paste0("output_", n, "_", i, ".Rdata"))
      
      if (!file.exists(output_file)){ # check if the file exists
       
         # If not, record (n,i)
        missjob <- rbind(missjob,
                         data.frame(n=n,
                                    i=i))
      }
    }
  }
}

# Remove duplicate entries (same n,i combination)
missjob <- unique(missjob)

# Print summary
cat("Total missing jobs found:", nrow(missjob), "\n")

## rewrite job files to produce the results that are missing ##====
cat("\n--- Writing job to generate missing results ---\n")
if(file.exists("joblist_n1.txt")){
  cat("Default job template file 'joblist_n1.txt' found.\n")
  job_template_file <- "joblist_n1.txt"
} else {
  job_template_file <- read_input("Default job template file 'joblist_n1.txt' not found. Enter the name of the file containing job format (default: joblist_n1.txt): ")
}


TIME_default <- read_input("Enter TIME value for calculating job index (default: 0): ")
TIME <- ifelse(TIME_default == "", 0, as.numeric(TIME_default))

output_jobfile <- read_input("Enter output job file name (default: error.txt): ")
if (output_jobfile == "") output_jobfile <- "error.txt"


# Close the stdin connection
close(stdin_con)

# Read the template job format
if (!file.exists(job_template_file)){
  stop(paste("Job template file", job_template_file, "does not exist!"))
}

# Read the first line as template
template_line <- readLines(job_template_file, n=1)

# Generate job lines for missing jobs
if (nrow(missjob) > 0){
  job_lines <- c()
  
  for (idx in 1:nrow(missjob)){
    n_val <- missjob$n[idx]
    i_val <- missjob$i[idx]
    
    # Calculate the adjusted index
    adjusted_i <- i_val + nsim * TIME
    
    # Parse the template and replace 3rd and 4th arguments
    parts <- strsplit(template_line, "\\s+")[[1]]
    parts[3] <- as.character(n_val)
    parts[4] <- as.character(adjusted_i)
    
    job_lines <- c(job_lines, paste(parts, collapse=" "))
  }
  
  # Split into multiple files if needed (1000 rows per file)
  max_rows_per_file <- 1000
  n_files <- ceiling(length(job_lines) / max_rows_per_file)
  
  if (n_files == 1){
    write.table(job_lines, file = paste0(output_jobfile,'_n1.txt') ,quote = F, col.names = F, row.names = F)
    cat("Wrote", length(job_lines), "jobs to", output_jobfile, "\n")
  } else {
    for (file_idx in 1:n_files){
      start_idx <- (file_idx - 1) * max_rows_per_file + 1
      end_idx <- min(file_idx * max_rows_per_file, length(job_lines))
      
      # Create filename with suffix
      file_base <- sub("\\.txt$", "", output_jobfile)
      current_file <- paste0(file_base, '_n',file_idx, ".txt")
      
      writeLines(job_lines[start_idx:end_idx], current_file)
      cat("Wrote", end_idx - start_idx + 1, "jobs to", current_file, "\n")
    }
  }
} else {
  cat("No missing jobs found. No output file created.\n")
}