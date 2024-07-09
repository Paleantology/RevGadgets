#' Read trace
#'
#' Reads in MCMC log files
#'
#' Reads in one or multiple MCMC log files from the same analysis
#' and discards a user-specified burn-in, compatible with multiple monitor
#' types. If the trace contains vectors of vectors and the user does not specify
#' format = "complex", readTrace() will read in those columns as factors
#' rather than as numeric vectors.
#'
#' @param paths (vector of character strings; no default) File path(s) to trace
#' file.
#' @param format (single character string; default = simple) Indicates type of
#' MCMC trace, complex indicates cases where trace contains vectors of vectors/
#' matrices - mnStochasticVariable monitor will sometimes be of this type.
#' @param delim (single character string; default = "\\t") Delimiter of file.
#' @param burnin (single numeric value; default = 0.1) Fraction of generations
#' to  discard (if value provided is between 0 and 1) or number of generations
#' (if value provided is greater than 1).
#' @param check.names (logical; default = FALSE) Passed to utils::read.table();
#' indicates if utils::read.table() should check column names and replace
#' syntactically invalid characters.
#' @param ... (various) Additional arguments passed to utils::read.table().
#'
#' @return List of dataframes (of length 1 if only 1 log file provided).
#'
#' @examples
#' # read and process a single trace file
#'
#' \donttest{
#' 
#' # Example usage:
#' file <- "../simple/part_run_1.log"
#' parsed_df <- readAndParseJSON(file)
#' View the parsed and unnested data frame
#' View(parsed_df)
#' }
#'
#' @export
#' 
#' 



readTrace <- function(paths,
                      format = "simple",
                      delim = "\t",
                      burnin = 0.1,
                      check.names = FALSE,
                      verbose = TRUE,
                      ...) {
  
  # Enforce argument matching and checks
  if (!is.character(paths)) {
    stop("All paths must be character strings.")
  }
  
  # Check if files exist
  if (!all(file.exists(paths))) {
    missing_files <- paths[!file.exists(paths)]
    stop("The following files do not exist:\n", paste(missing_files, collapse = "\n"))
  }
  
  # Ensure format is either "simple" or "complex" or "json"
  format <- match.arg(format, choices = c("simple", "complex", "json"))
  
  if (!is.character(delim) || nchar(delim) != 1)
    stop("delim must be a single character string")
  
  if (!is.numeric(burnin) || burnin < 0)
    stop("burnin must be a single positive numeric value")
  
  num_paths <- length(paths)
  
  # Check that the file headings match for all traces
  header <- vector("list", num_paths)
  for (i in 1:num_paths) {
    if (format == "json") {
      # Use your custom function to read and parse JSON
      json_data <- readAndParseJSON(paths[i])
      header[[i]] <- names(json_data)
    } else {
      header[[i]] <- colnames(
        utils::read.table(
          file = paths[i],
          header = TRUE,
          sep = delim,
          check.names = check.names,
          nrows = 0,
          ...
        )
      )
    }
  }
  
  all_headers <- unique(unlist(header))
  for (i in seq_len(length(header))) {
    if (!setequal(all_headers, header[[i]])) {
      stop("Not all headers of trace files match")
    }
  }
  
  # Read in the traces
  output <- vector("list", num_paths)
  for (i in 1:num_paths) {
    if (verbose) {
      message(paste0("Reading in log file ", i, "\n"))
    }
    
    if (format == "json") {
      # Use your custom function to read and parse JSON
      out <- readAndParseJSON(paths[i])
      out <- as.data.frame(out)  # Convert JSON data to data frame if necessary
    } else if (format == "complex") {
      stop("Complex trace type currently not supported")
    } else {
      out <- utils::read.table(
        file = paths[i],
        header = TRUE,
        sep = delim,
        check.names = check.names,
        ...
      )
    }
    
    if (burnin >= nrow(out))
      stop("Burnin larger than provided trace file")
    
    if (burnin >= 1) {
      output[[i]] <- out[(burnin + 1):nrow(out),]
    } else if (burnin < 1 & burnin > 0) {
      discard <- ceiling(burnin * nrow(out))
      output[[i]] <- out[(discard + 1):nrow(out),]
    } else if (burnin == 0) {
      output[[i]] <- out
    } else {
      stop("Invalid burnin value")
    }
  }
  
  # Return output
  return(output)
}

# Example for reading a JSON formatted log file 
#result_json_log <- readTrace(paths = "../simple/primates_cytb_GTR.log") 
result_json_log <- readTrace(paths = "../simple/part_run_1.log", format = "json") 
view(result_json_log)
