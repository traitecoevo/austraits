
## code to prepare `DATASET` dataset goes here

library(tidyverse)
source("R/usage.R")

austraits_all <- readRDS("../austraits/ignore/austraits-2.1.0.rds")
dataset_id <- c("Falster_2003", "Falster_2005_1", "Falster_2005_2", "Zanne_2009")

austraits <- extract_dataset(austraits_all, dataset_id)

usethis::use_data(austraits, overwrite = TRUE)
