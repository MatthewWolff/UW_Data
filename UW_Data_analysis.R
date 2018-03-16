library(tidyverse)

file_path = "~/desktop/UWData"

load_data <- function(path) {
  csv_files <- list.files(path, ".csv", full.names = T)
  suppressMessages(dat <- lapply(csv_files, read_csv ))
  names(dat) <- sapply(basename(csv_files), function(x) gsub(".csv", "", x))
}

load_data(file_path)

