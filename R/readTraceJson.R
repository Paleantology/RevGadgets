library(jsonlite)

readTrace <- function(paths,
                      format = "simple",
                      delim = "\t",
                      burnin = 0.1,
                      check.names = FALSE,
                      ...) {
  
  # Check if paths are character strings
  if (!is.character(paths)) {
    stop("All paths must be character strings.")
  }
  
  # Check if files exist
  if (!all(file.exists(paths))) {
    stop("Some files do not exist.")
  }
  
  # Check burnin is numeric and positive
  if (!is.numeric(burnin) || burnin < 0) {
    stop("Burnin must be a non-negative numeric value.")
  }
  
  num_paths <- length(paths)
  
  # Function to read file and determine if it's JSON or text
  read_file <- function(path) {
    content <- readLines(path)
    is_json <- grepl("^\\{.*\\}$", content[1])
    if (is_json) {
      data <- lapply(content, function(x) fromJSON(x, flatten = TRUE))
      data <- do.call(rbind, data)
      data <- as.data.frame(data)
    } else {
      data <- read.table(
        file = path,
        header = TRUE,
        sep = delim,
        check.names = check.names,
        ...
      )
    }
    return(data)
  }
  
  # Check headers match for all traces
#  header <- vector("list", num_paths)
#  for (i in 1:num_paths) {
#    file_info <- read_file(paths[i])
#    header[[i]] <- colnames(file_info$data)
 # }
  
#  all_headers <- unique(unlist(header))
#  for (i in seq_len(length(header))) {
 #   if (!setequal(all_headers, header[[i]])) {
  #    stop("Not all headers of trace files match.")
#    }
 # }
  
  # Read in the traces
#  output <- vector("list", num_paths)
  for (i in 1:num_paths) {
    message(paste0("Reading in log file ", i, "\n"))
    
    out <- read_file(paths[i])
    
    if (burnin >= nrow(out))
      stop("Burnin larger than provided trace file.")
    
    if (burnin >= 1) {
      output[[i]] <- out[(burnin + 1):nrow(out),]
    } else if (burnin < 1 & burnin > 0) {
      discard <- ceiling(burnin * nrow(out))
      output[[i]] <- out[(discard + 1):nrow(out),]
    } else if (burnin == 0) {
      output[[i]] <- out
    } else {
      stop("Invalid burnin value.")
    }
    
    # Print the first 10 entries
   # print(head(output[[i]], 10))
    
    # Determine output file path with .long extension
    output_file <- gsub("\\.log$", ".long", basename(paths[i]), ignore.case = TRUE)
    
    
    # Save output to .long file in working directory
    write.table(out, file = output_file, sep = "\t", row.names = FALSE)
    cat("Output saved to file:", output_file, "\n")
  }



}

# Example usage
setwd("..")

file_single <- file.path("C:/Users/SuZamii/Documents/bio-summer/simple/part_run_1.log")


if (file.exists(file_single)) {
  one_trace <- readTrace(paths = file_single)

} else {
  cat("The file", file_single, "does not exist.\n")
}
