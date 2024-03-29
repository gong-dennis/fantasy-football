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

#Weights
weights <- data.frame(seq(1:10), unique(all_fantasy$Season))
colnames(weights) <- c("Weight", "Season")
all_fantasy <- all_fantasy %>% full_join(weights, by = "Season")

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


RB_age_age2_model <- lmer(PPR_PPG ~ Age + Age2 + RushAttPG + TargetPG + (1|Player), weights = Weight, data=RB)
WR_age_age2_model <- lmer(PPR_PPG ~ Age + Age2 + TargetPG + (1|Player), weights = Weight, data=WR)
TE_age_age2_model <- lmer(PPR_PPG ~ Age + Age2 + TargetPG + (1|Player), weights = Weight, data=TE)
QB_age_age2_model <- lmer(PPR_PPG ~ Age + Age2 + PassAttPG + (1|Player), weights = Weight, data=QB)

current <- all_fantasy %>% filter(Season == 2018) %>% select(Player, FantPos, Age, 
                                                             PassAttPG, RushAttPG, TargetPG, Games_G)

current <- current %>% mutate(Age = Age + 1) %>% mutate(Age2 = Age^2)

RB_current <- current %>% filter(current$FantPos == "RB") %>% select(-c(PassAttPG, FantPos))
RB_predictions <- data.frame(RB_current$Player, predict(RB_age_age2_model, RB_current))
colnames(RB_predictions) <- c("Player", "Projection")
RB_predictions <- RB_predictions %>% mutate(PPR_Pts = Projection*16) %>% arrange(-Projection)

WR_current <- current %>% filter(current$FantPos == "WR") %>% select(-c(PassAttPG, FantPos, RushAttPG))
WR_predictions <- data.frame(WR_current$Player, predict(WR_age_age2_model, WR_current))
colnames(WR_predictions) <- c("Player", "Projection")
WR_predictions <- WR_predictions %>% mutate(PPR_Pts = Projection*16) %>% arrange(-Projection)

TE_current <- current %>% filter(current$FantPos == "TE") %>% select(-c(PassAttPG, FantPos, RushAttPG))
TE_predictions <- data.frame(TE_current$Player, predict(TE_age_age2_model, TE_current))
colnames(TE_predictions) <- c("Player", "Projection")
TE_predictions <- TE_predictions %>% mutate(PPR_Pts = Projection*16) %>% arrange(-Projection)

QB_current <- current %>% filter(current$FantPos == "QB") %>% select(-c(RushAttPG, FantPos, TargetPG))
QB_predictions <- data.frame(QB_current$Player, predict(QB_age_age2_model, QB_current))
colnames(QB_predictions) <- c("Player", "Projection")
QB_predictions <- QB_predictions %>% mutate(PPR_Pts = Projection*16) %>% arrange(-Projection)

VBD_PPR <- all_fantasy %>% select(FantPos, Season, Fantasy_VBD, Fantasy_PosRank, Fantasy_PPR)

RB_VBD_PPR <- VBD_PPR %>% filter(Fantasy_PosRank == 24) %>% filter(FantPos == "RB")
RB_VBD_PPR <- mean(RB_VBD_PPR$Fantasy_PPR)

WR_VBD_PPR <- VBD_PPR %>% filter(Fantasy_PosRank == 30) %>% filter(FantPos == "WR")
WR_VBD_PPR <- mean(WR_VBD_PPR$Fantasy_PPR)

TE_VBD_PPR <- VBD_PPR %>% filter(Fantasy_PosRank == 8) %>% filter(FantPos == "TE")
TE_VBD_PPR <- mean(TE_VBD_PPR$Fantasy_PPR)

QB_VBD_PPR <- VBD_PPR %>% filter(Fantasy_PosRank == 8) %>% filter(FantPos == "QB")
QB_VBD_PPR <- mean(QB_VBD_PPR$Fantasy_PPR)

QB_predictions$VBD <- QB_predictions$PPR_Pts - QB_VBD_PPR
RB_predictions$VBD <- RB_predictions$PPR_Pts - RB_VBD_PPR
WR_predictions$VBD <- WR_predictions$PPR_Pts - WR_VBD_PPR
TE_predictions$VBD <- TE_predictions$PPR_Pts - TE_VBD_PPR

top_150 <- rbind(QB_predictions, RB_predictions, WR_predictions, TE_predictions) %>% arrange(-VBD)

write.table(top_150, file = "top-150")


