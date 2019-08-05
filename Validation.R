#Validation

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


