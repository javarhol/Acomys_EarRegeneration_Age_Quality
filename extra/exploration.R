threes <- filter(cartregen, AgeCat2 == "3 - 6 mo")
sevens <- filter(cartregen, AgeCat2 == "7 - 9 mo")
twenties <- filter(cartregen, AgeCat2 == "> 22 mo")

cor.test(threes$Slide_per, threes$perc_cartregen_length, method = "spearman")
cor.test(sevens$Slide_per, sevens$perc_cartregen_length, method = "spearman")
cor.test(twenties$Slide_per, twenties$perc_cartregen_length, method = "spearman")

cor.test(threes$Slide_per, threes$Count.x, method = "spearman")
cor.test(sevens$Slide_per, sevens$Count.x, method = "spearman")
cor.test(twenties$Slide_per, twenties$Count.x, method = "spearman")

hist(cartregen_means$mean_perc_cartregen_length)
shapiro.test(cartregen_means$mean_perc_cartregen_length)

kruskal.test(mean_perc_cartregen_length ~ AgeCat2, data = cartregen)
library(FSA)
dunnTest(mean_perc_cartregen_length ~ AgeCat2, data = cartregen_means, method = "holm")

cartregen_noctrl <- filter(cartregen, AgeCat2 != "Ctrl")
cartregen_noctrl <- cartregen_noctrl[,c(1,3,13,15)]
cartregen_noctrl$AgeCat2 <- as.factor(cartregen_noctrl$AgeCat2)
cartregen_noctrl$AnimalID <- as.factor(cartregen_noctrl$AnimalID)
cartregen_noctrl$Slide_per <- as.numeric(cartregen_noctrl$Slide_per)
cartregen_noctrl$perc_cartregen_length <- as.numeric(cartregen_noctrl$perc_cartregen_length)

cartregen_noctrl$AgeCat2 <- factor(cartregen_noctrl$AgeCat2, levels = c("3 - 6 mo", "7 - 9 mo", "> 22 mo"))

slide_mdl <- lme(perc_cartregen_length ~ Slide_per * AgeCat2,
                 random = ~1|AnimalID, data = cartregen_noctrl)

summary(slide_mdl)

leveneTest(perc_cartregen_length ~ AgeCat2, data = cartregen_noctrl)
