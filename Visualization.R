#Visualization

top_150

top_150$normal_rk <- 1 - seq(1:nrow(top_150))/nrow(top_150)

ggplot(top_150, aes(VBD, normal_rk)) + geom_point()
