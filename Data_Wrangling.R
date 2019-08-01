library(tidyverse)

files <- list.files(path="/Users/dennisgong/Desktop/Fantasy Football", 
                    pattern="*.csv", full.names=TRUE, recursive=FALSE)

teams <- files[grep("team", files)]
fantasy <- files[grep("fantasy", files)]
stats <- files[grep("fantasy|team", files, invert = TRUE)]

#Used for individual stats
read_stats <- function(file) {
  table <- read_csv(file, skip = 2)
  header <- scan(file, nlines = 1, what = character(), sep = ",")
  colnames(table) <- header
  table
}

#Used for fantasy stats
read_fantasy <- function(file) {
  table <- read_csv(file, skip = 2)
  header <- scan(file, nlines = 1, what = character(), sep = ",")
  header2 <- scan(file, skip = 1, nlines = 1, what = character(), sep = ",")
  colnames(table) <- paste(header, header2, sep = "_")
  table
}

#Used for team stats
read_team <- function(file) {
  the_table <- read_csv(file, skip = 1)
  header <- scan(file, nlines = 1, what = character(), sep = ",")
  colnames(the_table) <- header
  colnames(the_table)[1] <- substr(file, 44, nchar(file)-4)
  the_table <- the_table[!grepl("FC", the_table$W),]
  the_table
}


#Names: remove last 3, and first 43
team_tibbles <- lapply(teams, read_team)
stats_tibbles <- lapply(stats, read_stats)
fantasy_tibbles <- lapply(fantasy, read_fantasy)
