library(tidyverse)
library(reshape2)

source("configs.R")

summary_stats <- function(educ,
                          perwt=rep(1, length(educ))) {
  d11 <- sum((educ == 11) * perwt) / sum(perwt)
  d10 <- d11 + sum((educ == 10) * perwt) / sum(perwt)
  d09 <- d10 + sum((educ == 09) * perwt) / sum(perwt)
  d08 <- d09 + sum((educ == 08) * perwt) / sum(perwt)
  d07 <- d08 + sum((educ == 07) * perwt) / sum(perwt)
  d06 <- d07 + sum((educ == 06) * perwt) / sum(perwt)
  d05 <- d06 + sum((educ == 05) * perwt) / sum(perwt)
  d04 <- d05 + sum((educ == 04) * perwt) / sum(perwt)
  d03 <- d04 + sum((educ == 03) * perwt) / sum(perwt)
  d02 <- d03 + sum((educ == 02) * perwt) / sum(perwt)
  d01 <- d02 + sum((educ == 01) * perwt) / sum(perwt)
  d00 <- d01 + sum((educ == 00) * perwt) / sum(perwt)
  return(data.frame(d00, d01, d02, d03, d04, d05, d06, d07, d08, d09, d10, d11))
}

summary_stats_2 <- function(educ,
                            perwt=rep(1, length(educ)),
                            grades=c(07, 08, 09, 10, 11)) {
  college <- sum((educ %in% grades) * perwt)
  college.prop <- sum((educ %in% grades) * perwt) / sum(perwt)
  return(data.frame(college, college.prop))
}


# read data
read_data <- function(filename) {
  read.csv(filename)
}

# first summary
summarize_stats_1 <- function(dataset) {
  dataset %>%
    group_by(YEAR, RACE) %>%
    do(summary_stats(.$EDUC, .$PERWT))
}

# second summary
summarize_stats_2 <- function(dataset) {
  data %>%
    group_by(YEAR, RACE) %>%
    do(summary_stats_2(.$EDUC, .$PERWT, grades=college4.grades))
}
