#Validation - Testing accuracy from model generated from data up to 2017 on 2018 outcomes. Reached 30% error


test_2018 <- all_fantasy %>% filter(Season == 2018) %>% select(Player, FantPos, Age, PPR_PPG, Age2)
val_2017 <- all_fantasy %>% filter(Season == 2017) %>% select(Player, PassAttPG, RushAttPG, TargetPG)

#Rookies and seasonally injured players do not make the cut
test_2018 <- full_join(test_2018, val_2017, by = "Player")
incomplete_2018 <- apply(test_2018, 1, count_NA) == 3 |  apply(test_2018, 1, count_NA) == 4
test_2018 <- test_2018[!incomplete_2018,]

#Test on RB data
RB_2018 <- test_2018 %>% filter(test_2018$FantPos == "RB") %>% select(-c(PassAttPG, FantPos))
RB_2018_pred <- data.frame(RB_2018$Player, predict(RB_age_age2_model, RB_2018), RB_2018$PPR_PPG)
colnames(RB_2018_pred) <- c("Player", "Projection", "Actual")
RB_2018_pred$Difference <- abs(RB_2018_pred$Projection - RB_2018_pred$Actual)
RB_2018_pred$Percent_Error <- RB_2018_pred$Difference / RB_2018_pred$Actual

RB_Accuracy <- sum(RB_2018_pred$Difference)/sum(RB_2018_pred$Actual)
RB_Accuracy2 <- mean(RB_2018_pred$Percent_Error)


#Have not implemented for WRs yet. Can be done by simply copying code from RB section
WR_2018 <- test_2018 %>% filter(test_2018$FantPos == "WR") %>% select(-c(PassAttPG, FantPos, RushAttPG))
WR_predictions <- data.frame(WR_current$Player, predict(WR_age_age2_model, WR_current))
colnames(WR_predictions) <- c("Player", "Projection")
WR_predictions <- WR_predictions %>% mutate(PPR_Pts = Projection*16) %>% arrange(-Projection)

TE_2018 <- test_2018 %>% filter(test_2018$FantPos == "TE") %>% select(-c(PassAttPG, FantPos, RushAttPG))
TE_predictions <- data.frame(TE_current$Player, predict(TE_age_age2_model, TE_current))
colnames(TE_predictions) <- c("Player", "Projection")
TE_predictions <- TE_predictions %>% mutate(PPR_Pts = Projection*16) %>% arrange(-Projection)

QB_2018 <- test_2018 %>% filter(test_2018$FantPos == "QB") %>% select(-c(RushAttPG, FantPos, TargetPG))
QB_predictions <- data.frame(QB_current$Player, predict(QB_age_age2_model, QB_current))
colnames(QB_predictions) <- c("Player", "Projection")
QB_predictions <- QB_predictions %>% mutate(PPR_Pts = Projection*16) %>% arrange(-Projection)