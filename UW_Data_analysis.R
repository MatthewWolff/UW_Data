library(tidyverse)

load_data <- function(path) {
  csv_files <- list.files(path, ".csv", full.names = T)
  suppressMessages(dat <- lapply(csv_files, read_csv ))
  names(dat) <- sapply(basename(csv_files), function(x) gsub(".csv", "", x))
  dat
}

pull_gender_data <- function(data_frames){
  graduates <- data_frames$DegreesEarned %>% 
    transmute(
      Degree = Degree,
      Graduates = (Male + Female),
      `Male Proportion` = Male/Graduates) 
}

gender_dominance <- function(graduates, male,  bin_size=100, minimum_graduates=20) {
  binned <- graduates %>%  
    filter(Graduates >= minimum_graduates) %>%
    mutate(
      Bin = Graduates %/% bin_size * bin_size,
      Grads = paste(Bin,'-',Bin + bin_size, sep=""))
  if(male)
    sorted <- arrange(binned,desc(Bin), desc(`Male Proportion`))
  else
    sorted <- arrange(binned, desc(Bin), `Male Proportion`)
  select(sorted, Degree, Grads, everything(), -Graduates, -Bin)
}

most_gendered <- function(graduates, male){
  gendered <- graduates %>%  
    filter(Graduates >= minimum_graduates) %>%
    arrange(`Male Proportion`) %>%
    select(Degree, `Male Proportion`, Graduates)
  if(!male){
    gendered
  } else {
    as_tibble(apply(gendered, 2, rev))
  }
}

file_path = "~/github/UWData/data"
dfs <- load_data(file_path)
graduates <- pull_gender_data(dfs)
m <- gender_dominance(graduates, male=T)
f <- gender_dominance(graduates, male=F)
men <- most_gendered(graduates, male=F)

