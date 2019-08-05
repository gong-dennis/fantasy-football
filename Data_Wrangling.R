library(tidyverse)
library(ggplot2)
library(broom)

files <- list.files(path="Files", 
                    pattern="*.csv", full.names=TRUE, recursive=FALSE)

teams <- files[grep("team", files)]
fantasy <- files[grep("fantasy", files)]
rushing <- files[grep("rushing", files)]
passing <- files[grep("passing", files)]
receiving <- files[grep("receiving", files)]

#Used for receiving stats
read_receiving <- function(file) {
  table <- read_csv(file)
  header <- scan(file, nlines = 1, what = character(), sep = ",")
  colnames(table) <- header
  colnames(table)[1] <- substr(file, 7, nchar(file)-4)
  table <- table %>% filter(Rk != "Rk")
  table <- table[1:250,]
  table[,3] <- lapply(table[,3], gsub, pattern = "[*+]", replacement = "")
  table
}

#Used for rushing data
read_rushing <- function(file) {
  table <- read_csv(file, skip = 1)
  header <- scan(file, nlines = 1, what = character(), sep = ",")
  header2 <- scan(file, skip = 1, nlines = 1, what = character(), sep = ",")
  colnames(table) <- paste(header, header2, sep = "_")
  colnames(table)[1] <- substr(file, 7, nchar(file)-4)
  colnames(table)[2:6] <- header2[2:6]
  table <- table %>% filter(Rk != "Rk")
  table <- table[1:100,]
  table[,3] <- lapply(table[,3], gsub, pattern = "[*+]", replacement = "")
  table
}

#Used for passing data
read_passing <- function(file) {
  table <- read_csv(file)
  header <- scan(file, nlines = 1, what = character(), sep = ",")
  colnames(table) <- header
  colnames(table)[1] <- substr(file, 7, nchar(file)-4)
  table <- table %>% filter(Rk != "Rk")
  table <- table[1:50,]
  table[,3] <- lapply(table[,3], gsub, pattern = "[*+]", replacement = "")
  table
}

#Used for fantasy stats
read_fantasy <- function(file) {
  table <- read_csv(file, skip = 1)
  header <- scan(file, nlines = 1, what = character(), sep = ",")
  header2 <- scan(file, skip = 1, nlines = 1, what = character(), sep = ",")
  colnames(table) <- paste(header, header2, sep = "_")
  colnames(table)[2:6] <- header2[2:6]
  colnames(table)[1] <- "Season"
  table[,1] <- as.numeric(substr(file, 15, nchar(file)-4))
  table <- table %>% filter(Rk != "Rk")
  table <- table[1:150,]
  table[,3] <- lapply(table[,3], gsub, pattern = "[*+]", replacement = "")
  table
}

#Used for team stats
read_team <- function(file) {
  the_table <- read_csv(file, skip = 1)
  header <- scan(file, nlines = 1, what = character(), sep = ",")
  colnames(the_table) <- header
  the_table <- the_table[!grepl("FC", the_table$W),]
  the_table[,2] <- lapply(the_table[,2], gsub, pattern = "[*+]", replacement = "")
  colnames(the_table)[1] <- substr(file, 7, nchar(file)-4)
  the_table
}

#Put the tables into nice format
team_tibbles <- lapply(teams, read_team)
passing_tibbles <- lapply(passing, read_passing)
rushing_tibbles <- lapply(rushing, read_rushing)
receiving_tibbles <- lapply(receiving, read_receiving)
fantasy_tibbles <- lapply(fantasy, read_fantasy)


#Combine AFC and NFC

is_AFC <- function(a_tibble) {
  colnames(a_tibble)[1] %>% startsWith("0")
}
AFC <- team_tibbles[unlist(lapply(team_tibbles, is_AFC))]
NFC <- team_tibbles[!unlist(lapply(team_tibbles, is_AFC))]
colnames(AFC)[1] <- "Index"

full_team_tibbles <- list()
for (i in 1:length(AFC)) {
  colnames(AFC[[i]])[1] <- substr(colnames(AFC[[i]])[1], nchar(colnames(AFC[[i]])[1]) - 3, 
                                  nchar(colnames(AFC[[i]])[1]))
  colnames(NFC[[i]])[1] <- substr(colnames(NFC[[i]])[1], nchar(colnames(NFC[[i]])[1]) - 3, 
                                  nchar(colnames(NFC[[i]])[1]))
  full_team_tibbles[[i]] <- rbind(AFC[[i]], NFC[[i]])
}



