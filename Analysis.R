#Analysis - lme4
library(lme4)
library(caret)
library(skimr)

#Get unique variables names
fantasy_names <- colnames(fantasy_tibbles[[1]])[2:length(colnames(fantasy_tibbles[[1]]))]
passing_names <- colnames(passing_tibbles[[1]])[2:length(colnames(passing_tibbles[[1]]))]
rushing_names <- colnames(rushing_tibbles[[1]])[2:length(colnames(rushing_tibbles[[1]]))]
receiving_names <- colnames(receiving_tibbles[[1]])[2:length(colnames(receiving_tibbles[[1]]))]

all_names <- unique(c(fantasy_names, rushing_names, receiving_names, passing_names))

#Master fantasy stat table
all_fantasy <- do.call(rbind, fantasy_tibbles)
all_fantasy[,6:ncol(all_fantasy)] <- sapply(all_fantasy[,6:ncol(all_fantasy)],as.numeric)
all_fantasy[,2] <- lapply(all_fantasy[,2], as.numeric)

all_fantasy <- all_fantasy %>% mutate(PPR_PPG = Fantasy_PPR/Games_G)
all_fantasy <- all_fantasy %>% mutate(TargetPG = Receiving_Tgt/Games_G)
all_fantasy <- all_fantasy %>% mutate(RushAttPG = Rushing_Att/Games_G)
all_fantasy <- all_fantasy %>% mutate(PassAttPG = Passing_Att/Games_G)
all_fantasy <- all_fantasy %>% mutate(Age2 = Age^2)

RB <- all_fantasy %>% filter(all_fantasy$FantPos == "RB") %>% group_by(Player) %>% arrange(Rk)
QB <- all_fantasy %>% filter(all_fantasy$FantPos == "QB") %>% group_by(Player) %>% arrange(Rk)
WR <- all_fantasy %>% filter(all_fantasy$FantPos == "WR") %>% group_by(Player) %>% arrange(Rk)
TE <- all_fantasy %>% filter(all_fantasy$FantPos == "TE") %>% group_by(Player) %>% arrange(Rk)

RB <- RB %>% select(-c("Passing_Cmp", "Games_GS", "Passing_Att", "Passing_Yds", "Passing_TD", 
                       "Passing_Int", "Fumbles_FL", "Fantasy_FantPt", "Fantasy_FDPt", "Fantasy_DKPt"))
WR <- WR %>% select(-c("Passing_Cmp", "Games_GS", "Passing_Att", "Passing_Yds", "Passing_TD", 
                       "Passing_Int", "Fumbles_FL", "Fantasy_FantPt", "Fantasy_FDPt", "Fantasy_DKPt"))
QB <- QB %>% select(-c("Games_GS", "Receiving_Tgt", "Receiving_Rec", "Receiving_Yds", "Receiving_Y/R",
                       "Rushing_Y/A", "Receiving_TD", "Fumbles_FL", "Fantasy_FantPt", "Fantasy_FDPt", 
                       "Fantasy_DKPt"))
TE <- TE %>% select(-c("Passing_Cmp", "Games_GS", "Passing_Att", "Passing_Yds", "Passing_TD", 
                       "Passing_Int", "Fumbles_FL", "Fantasy_FantPt", "Fantasy_FDPt", "Fantasy_DKPt"))

RB_age_age2_model <- lmer(PPR_PPG ~ Age + Age2 + RushAttPG + TargetPG + (1|Player), data=RB)
WR_age_age2_model <- lmer(PPR_PPG ~ Age + Age2 + TargetPG + (1|Player), data=WR)
TE_age_age2_model <- lmer(PPR_PPG ~ Age + Age2 + TargetPG + (1|Player), data=TE)
QB_age_age2_model <- lmer(PPR_PPG ~ Age + Age2 + PassAttPG + (1|Player), data=QB)
