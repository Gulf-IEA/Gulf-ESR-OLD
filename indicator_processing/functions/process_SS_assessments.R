#' Process SEDAR Stock Synthesis Assessment Outputs
#'
#' This function searches a specified root directory for Stock Synthesis `Report.sso` files,
#' processes each using `r4ss::SS_output()`, and saves the outputs and any failures.
#'
#' @param root_dir Character. Root directory containing assessment folders. 
#'   Default is the shared Google Drive main folder path
#' @param output_dir Character. Directory where the results and failed assessments will be saved.
#'   Default is "indicator_data/stock assessment output plots and data V2".
#'
#' @return No return value. Writes an RDS file and a text file of failed assessments.
#'
#' @details 
#' Requires the following packages:
#' \itemize{
#'   \item \strong{r4ss} for reading SS model outputs.
#' }
#'
#' @importFrom r4ss SS_output
#' @export
process_SS_assessments <- function(
    root_dir = "G:/.shortcut-targets-by-id/1ixDqh6nB2x_6OmR4yevLRsssxIZhJlni/Assessment Report.sso files",
    output_dir = "indicator_data/stock assessment output plots and data V2"
) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Find all Report.sso files
  sso_files <- list.files(root_dir, recursive = TRUE, full.names = TRUE, pattern = "Report\\.sso$")
  assessment_dirs <- unique(dirname(sso_files))
  
  output_list <- list()
  failed_assessments <- character()
  
  for (dir in assessment_dirs) {
    message("Processing: ", dir)
    
    tryCatch({
      base <- SS_output(
        dir = dir,
        printstats = TRUE,
        covar = TRUE,
        cormax = 0.70,
        forecast = FALSE
      )
      
      # Extract the last two parts of the directory path
      path_parts <- strsplit(dir, .Platform$file.sep)[[1]]
      species <- tail(path_parts, 2)
      list_name <- paste(species, collapse = "_")
      
      # Assign with clean name
      output_list[[list_name]] <- base
      
    }, error = function(e) {
      warning("Failed to process: ", dir, "\n  Reason: ", e$message)
      failed_assessments <<- c(failed_assessments, dir)
    })
  }
  
  # Save output files
  failed_path <- file.path(output_dir, "failed_assessments.txt")
  rds_path <- file.path(output_dir, "SS_outputs_all.rds")
  
  writeLines(failed_assessments, failed_path)
  saveRDS(output_list, rds_path)
  
  message("Processing complete.\nSaved failed assessments to: ", failed_path, 
          "\nSaved results to: ", rds_path)
}
