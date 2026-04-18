# Create a single stdin connection to reuse
stdin_con <- file("stdin", open="r")

read_input <- function(prompt, con=stdin_con) {
  cat(prompt)
  return(readLines(con, n=1))
}

## default parameters ## ====
estimator   <- c('TMLE','Onestep','EstEquation')
n.vec       <- c(500, 1000, 2000)
nsim        <- 1000
missjob     <- data.frame(n=integer(), i=integer())

output_folder_default     <- "output"
filename_template_default <- "output_{n}_{i}.Rdata"

cat("
+------------------------------------------------------------------+
|          check_and_resubmit.R  --  Quick Reference               |
+------------------------------------------------------------------+
| DIR    [estimator/]<output_folder>/<filename>                     |
|        estimator='none' -> no estimator subfolder                 |
| FILE   {n}=sample size, {i}=sim index  (regex auto-derived)       |
|        e.g. data_n{n}_seed{i}.Rdata -> data_n1000_seed42.Rdata    |
| JOBLINES  use joblist_n1.txt as the default reference for joblines|
| I usually use 'Rscript main.R {n} {i}' in joblist_n1.txt.         |
+------------------------------------------------------------------+
")

cat("\n--- Collecting inputs ---\n")

n_input <- read_input(paste0("Sample sizes (default: ", paste(n.vec, collapse=", "), "): "))
if (n_input != "") n.vec <- as.numeric(unlist(strsplit(n_input, ",")))
cat("  n:", paste(n.vec, collapse=", "), "\n")

estimator_input <- read_input(paste0("Estimator folders (default: ", paste(estimator, collapse=", "), "; 'none' for no subfolder): "))
if (estimator_input == "none") {
  estimator <- NULL
} else if (estimator_input != "") {
  estimator <- unlist(strsplit(estimator_input, ","))
}
cat("  estimator:", if (is.null(estimator)) "none" else paste(estimator, collapse=", "), "\n")

nsim_input <- read_input(paste0("nsim (default: ", nsim, "): "))
if (nsim_input != "") nsim <- as.numeric(nsim_input)
cat("  nsim:", nsim, "\n")

output_folder_input <- read_input(paste0("Output folder (default: ", output_folder_default, "): "))
output_folder <- if (output_folder_input != "") output_folder_input else output_folder_default
cat("  output_folder:", output_folder, "\n")

filename_template_input <- read_input(paste0("Filename template (default: ", filename_template_default, "): "))
filename_template <- if (filename_template_input != "") filename_template_input else filename_template_default
cat("  filename_template:", filename_template, "\n")

# auto-derive regex from template
template_to_regex <- function(tmpl) {
  r <- gsub("\\.", "\\\\.", tmpl)
  r <- gsub("\\{n\\}", "(?P<n>[0-9]+)", r)
  r <- gsub("\\{i\\}", "(?P<i>[0-9]+)", r)
  paste0("^", r, "$")
}
filename_regex <- template_to_regex(filename_template)
cat("  filename_regex (auto):", filename_regex, "\n")

## helpers ## ====

make_output_dir <- function(e) {
  if (is.null(e)) output_folder else file.path(e, output_folder)
}

make_filename <- function(n_val, i_val) {
  s <- gsub("\\{n\\}", n_val, filename_template)
  s <- gsub("\\{i\\}", i_val, s)
  s
}

# Parse n and i from a filename; returns list(n=..., i=...) or NULL
parse_filename <- function(bn) {
  r_regex <- gsub("\\(\\?P<", "(?<", filename_regex) # convert to R perl syntax
  m <- regexpr(r_regex, bn, perl=TRUE)
  if (m == -1) return(NULL)
  starts  <- attr(m, "capture.start")
  lengths <- attr(m, "capture.length")
  names_  <- attr(m, "capture.names")
  get_group <- function(gname) {
    idx <- which(names_ == gname)
    if (length(idx) == 0) return(NA)
    substr(bn, starts[idx], starts[idx] + lengths[idx] - 1)
  }
  list(n = as.numeric(get_group("n")),
       i = as.numeric(get_group("i")))
}

## check for duplicated / ill-named files ## ====
cat("\n--- Check for duplicated/ill-named files ---\n")
duplicated_results <- c()
renamed            <- c()

estimator_list <- if (is.null(estimator)) list(NULL) else as.list(estimator)

for (e in estimator_list) {
  output_dir <- make_output_dir(e)
  if (dir.exists(output_dir)) {
    files <- list.files(output_dir, full.names=TRUE)
    for (file in files) {
      bn     <- basename(file)
      parsed <- parse_filename(bn)
      if (is.null(parsed)) next
      n_val <- parsed$n
      i_val <- parsed$i
      if (i_val > nsim) {
        adjusted_i <- i_val %% nsim
        if (adjusted_i == 0) adjusted_i <- nsim
        duplicated_results <- c(duplicated_results, bn)
        new_filename <- file.path(output_dir, make_filename(n_val, adjusted_i))
        if (!file.exists(new_filename)) {
          file.rename(file, new_filename)
          renamed <- c(renamed, TRUE)
          cat("Renamed:", bn, "->", basename(new_filename), "\n")
        } else {
          cat("Warning: target exists, deleting duplicate:", bn, "\n")
          renamed <- c(renamed, FALSE)
          file.remove(file)
        }
      }
    }
  }
}

if (length(duplicated_results) > 0) {
  cat("Total renamed:", sum(renamed), "out of", length(duplicated_results), "\n")
  write.csv(data.frame(duplicated_results, renamed), "duplicated_results.csv", row.names=FALSE)
} else {
  cat("No files needed renaming.\n")
}

## check for missing results ## ====
cat("\n--- Check missing results ---\n")

for (e in estimator_list) {
  output_dir <- make_output_dir(e)
  for (n in n.vec) {
    for (i in 1:nsim) {
      output_file <- file.path(output_dir, make_filename(n, i))
      if (!file.exists(output_file)) {
        missjob <- rbind(missjob, data.frame(n=n, i=i))
      }
    }
  }
}

missjob <- unique(missjob)
cat("Total missing jobs found:", nrow(missjob), "\n")

## write job files for missing results ## ====
cat("\n--- Writing jobs to generate missing results ---\n")

if (file.exists("joblist_n1.txt")) {
  cat("Job template 'joblist_n1.txt' found.\n")
  job_template_file <- "joblist_n1.txt"
} else {
  job_template_file <- read_input("Job template not found. Enter template filename: ")
}

TIME_input <- read_input("TIME value for job index offset (default: 0): ")
TIME <- if (TIME_input == "") 0 else as.numeric(TIME_input)

output_jobfile <- read_input("Output job file name (default: error_n{1,2,3,...}.txt): ")
if (output_jobfile == "") output_jobfile <- "error.txt"

close(stdin_con)

if (!file.exists(job_template_file)) {
  stop(paste("Job template file", job_template_file, "does not exist!"))
}

template_line <- readLines(job_template_file, n=1)

if (nrow(missjob) > 0) {
  job_lines <- c()
  for (idx in 1:nrow(missjob)) {
    n_val      <- missjob$n[idx]
    i_val      <- missjob$i[idx]
    adjusted_i <- i_val + nsim * TIME
    parts    <- strsplit(template_line, "\\s+")[[1]]
    parts[3] <- as.character(n_val)
    parts[4] <- as.character(adjusted_i)
    job_lines <- c(job_lines, paste(parts, collapse=" "))
  }

  max_rows_per_file <- 1000
  n_files <- ceiling(length(job_lines) / max_rows_per_file)

  if (n_files == 1) {
    out_path <- paste0(sub("\\.txt$", "", output_jobfile), "_n1.txt")
    writeLines(job_lines, out_path)
    cat("Wrote", length(job_lines), "jobs to", out_path, "\n")
  } else {
    for (file_idx in 1:n_files) {
      start_idx    <- (file_idx - 1) * max_rows_per_file + 1
      end_idx      <- min(file_idx * max_rows_per_file, length(job_lines))
      current_file <- paste0(sub("\\.txt$", "", output_jobfile), "_n", file_idx, ".txt")
      writeLines(job_lines[start_idx:end_idx], current_file)
      cat("Wrote", end_idx - start_idx + 1, "jobs to", current_file, "\n")
    }
  }
} else {
  cat("No missing jobs found. No output file created.\n")
}
