library(tidyverse)
library(ggbeeswarm)
library(cowplot)
library(ggsci)
library(see)


# data
measures <- read.csv("data/measurements_012924.csv", header = TRUE)
daystoclose <- read.csv("data/EarDaysToClose_Data.csv")

measures <- measures %>% 
  select(AnimalID, Slide, Name, Num.points, Length..µm) %>% 
  left_join(daystoclose[,c(2,3,7,8)]) %>% 
  mutate(AgeInMonths = (AgeAtInjury/30.417))

measures$AgeCat[is.na(measures$AgeCat)] <- "Ctrl"
measures$AgeCat <- factor(measures$AgeCat, levels = c("Ctrl", "3 to 4 months", "4.5 to 6 months", "6.5 to 9 months", "Greater than 22 months"))

#make adjusted slide number. 
##Count the number of slides per animalid, 

slidecount <- measures %>% 
  filter(Name == "Regenerated Length") %>% 
  group_by(AnimalID) %>% 
  summarise(slidecount = n())

#assign a count to each slide number, 

measures1 <- left_join(measures, slidecount, by = "AnimalID")

#divide count by number of slides per animal id, 

slidenumber <- measures1 %>% 
  filter(Name == "Regenerated Length") %>% 
  group_by(AnimalID) %>% 
  mutate(Slide_adj = rank(Slide)) %>% 
  select(AnimalID, Slide, Slide_adj, slidecount) %>% 
  unique() %>% 
  mutate(Slide_per = 100*(Slide_adj/slidecount))

measures2 <- left_join(measures1, slidenumber, by = c("AnimalID", "Slide", "slidecount"))
#########

cartilage <- measures2 %>% 
  filter(Name == "Cartilage Length") %>% 
  group_by(AnimalID, Slide, Slide_per, Sex, AgeInMonths, AgeCat, Name) %>% 
  summarise(Length = sum(Length..µm), Count = n())
  #summarise(Area = sum(Area.µm.2), Perimeter = sum(Perimeter.µm))

regenarea <- measures2 %>% 
  filter(Name == "Regenerated Length") %>% 
  group_by(AnimalID, Slide, Slide_per, Sex, AgeInMonths, AgeCat, Name) %>% 
  summarise(Length = sum(Length..µm), Count = n())

adipocytes <- measures2 %>% 
  filter(Name == "Adipocytes") %>% 
  group_by(AnimalID, Slide, Slide_per, Sex, AgeInMonths, AgeCat, Name) %>% 
  summarise(count = sum(Num.points))

cartregen <- cartilage %>% 
  left_join(regenarea, by = c("AnimalID", "Slide", "Slide_per", "Sex", "AgeInMonths", "AgeCat")) %>% 
  mutate(perc_cartregen_length = 100*(Length.x / Length.y))

adiporegen <- adipocytes %>% 
  left_join(regenarea, by = c("AnimalID", "Slide", "Slide_per", "Sex", "AgeInMonths", "AgeCat")) %>% 
  mutate(count_area = count / Length)

cartregen$AnimalID_Slide <- paste(cartregen$AnimalID, cartregen$Slide, sep = "-")

##########
cartregen_means <- cartregen %>% 
  #mutate(AgeCat2 = case_when(AgeCat == "3 to 4 months" ~ "3 - 6 mo",
                             #AgeCat == "4.5 to 6 months" ~ "3 - 6 mo",
                             #AgeCat == "6.5 to 9 months" ~ "7 - 9 mo",
                             #AgeCat == "Greater than 22 months" ~ "> 22 mo",
                             #AgeCat == "Ctrl" ~ "Ctrl")) %>% 
  group_by(AnimalID, AgeCat) %>% 
  summarise(mean_perc_cartregen_length = mean(perc_cartregen_length), mean_count=mean(Count.x))

#cartregen_means$AgeCat2 <- factor(cartregen_means$AgeCat2, levels = c("Ctrl", "3 - 6 mo", "7 - 9 mo", "> 22 mo"))

#cartregen <- cartregen %>% 
  #mutate(AgeCat2 = case_when(AgeCat == "3 to 4 months" ~ "3 - 6 mo",
                             #AgeCat == "4.5 to 6 months" ~ "3 - 6 mo",
                             #AgeCat == "6.5 to 9 months" ~ "7 - 9 mo",
                             #AgeCat == "Greater than 22 months" ~ "> 22 mo",
                             #AgeCat == "Ctrl" ~ "Ctrl"))

#cartregen$AgeCat2 <- factor(cartregen$AgeCat2, levels = c("Ctrl", "3 - 6 mo", "7 - 9 mo", "> 22 mo"))



## single point plots

p1 <- ggplot(cartregen, aes(AgeCat, perc_cartregen_length, fill = AgeCat)) +
  geom_boxplot(alpha=0.8) +
  geom_beeswarm(cex = 2, size = 2) +
  scale_y_continuous(labels=function(x) paste0(x,"%"))+
  scale_fill_social() +
  ylab("Cartilage Length / \nRegenerated Length") + xlab("") +
  theme_minimal_hgrid() + theme(axis.text.x=element_text(angle=90, vjust=0.75),
                                text = element_text(family = "Source Sans 3"), legend.position = "none")

p2 <- ggplot(cartregen, aes(AgeCat2, Count.x, fill = AgeCat2)) +
  geom_boxplot(alpha=0.8)+
  geom_beeswarm(cex = 2, size = 2) +
  scale_fill_manual(values = c("white", "#0021A5", "#FA4616", "#D32737"))+
  ylab("Cartilage count \nPer Regenerated Length") + xlab("") +
  theme_minimal_hgrid() + theme(axis.text.x=element_text(angle=90, vjust=0.75),
                                text = element_text(family = "Source Sans 3"), legend.position = "none")

lineplot <- ggplot(cartregen, aes(Slide_per, perc_cartregen_length, color = AgeCat2)) +
  geom_point()+
  geom_smooth(method = lm, se=FALSE)+
  scale_color_manual(values = c("grey", "#0021A5", "#FA4616", "#D32737"))+
  scale_y_continuous(labels=function(x) paste0(x,"%"))+
  scale_x_continuous(labels=function(x) paste0(x,"%"))+
  ylab("Cartilage Length / \nRegenerated Length") + xlab("Depth of Injury") +
  theme_cowplot() + theme(text = element_text(family = "Source Sans 3"), legend.position = "none")

mean_adiporegen <- adiporegen %>% 
  mutate(AgeCat2 = case_when(AgeCat == "3 to 4 months" ~ "3 - 6 mo",
                             AgeCat == "4.5 to 6 months" ~ "3 - 6 mo",
                             AgeCat == "6.5 to 9 months" ~ "7 - 9 mo",
                             AgeCat == "Greater than 22 months" ~ "> 22 mo",
                             AgeCat == "Ctrl" ~ "Ctrl")) %>% 
  group_by(AnimalID, AgeCat2) %>% 
  summarise(mean_count_area = mean(count_area), count = mean(count))

adiporegen <- adiporegen %>% 
  mutate(AgeCat2 = case_when(AgeCat == "3 to 4 months" ~ "3 - 6 mo",
                             AgeCat == "4.5 to 6 months" ~ "3 - 6 mo",
                             AgeCat == "6.5 to 9 months" ~ "7 - 9 mo",
                             AgeCat == "Greater than 22 months" ~ "> 22 mo",
                             AgeCat == "Ctrl" ~ "Ctrl"))

mean_adiporegen$AgeCat2 <- factor(mean_adiporegen$AgeCat2, levels = c("Ctrl", "3 - 6 mo", "7 - 9 mo", "> 22 mo"))
adiporegen$AgeCat2 <- factor(adiporegen$AgeCat2, levels = c("Ctrl", "3 - 6 mo", "7 - 9 mo", "> 22 mo"))

p3 <- ggplot(adiporegen, aes(AgeCat2, count, fill = AgeCat2)) +
  geom_boxplot(alpha=0.8) +
  geom_beeswarm(cex = 2, size = 2) +
  scale_fill_manual(values = c("white", "#0021A5", "#FA4616", "#D32737"))+
  ylab("Adipocyte count") + xlab("") +
  theme_minimal_hgrid() + theme(axis.text.x=element_text(angle=90, vjust=0.75),
                                text = element_text(family = "Source Sans 3"), legend.position = "none")

plot123 <- plot_grid(p1, lineplot, p2, p3, align = "v", ncol = 2, labels = "AUTO")
ggsave2("Figures/QuPathPlot.png", dpi=300, width=180, height=180, unit="mm" )

###############
#cartregen
hist(cartregen_means$mean_perc_cartregen_length)
shapiro.test(cartregen_means$mean_perc_cartregen_length)

kruskal.test(mean_perc_cartregen_length ~ AgeCat2, data = cartregen_means)
library(FSA)
dunnTest(mean_perc_cartregen_length ~ AgeCat2, data = cartregen_means, method = "holm")

hist(cartregen_means$mean_count)
shapiro.test(cartregen_means$mean_count)

kruskal.test(mean_count ~ AgeCat2, data = cartregen_means)
dunnTest(mean_count ~ AgeCat2, data = cartregen_means, method = "holm")

hist(mean_adiporegen$count)
shapiro.test(mean_adiporegen$count)

kruskal.test(count ~ AgeCat2, data = mean_adiporegen)
dunnTest(count ~ AgeCat2, data = mean_adiporegen, method = "holm")

##################
#nerve
nerve <- read.csv("nerve_110823_jv_jt.csv", header = TRUE)

nerve <- nerve %>% 
  group_by(AnimalID, AgeCat) %>% 
  summarise(sum_count = sum(Num.points))

nerve$AgeCat <- factor(nerve$AgeCat, levels = c("3 - 4 mo", "> 22 mo"))

plotn <- ggplot(nerve, aes(AgeCat, sum_count, fill = AgeCat)) +
  stat_summary(geom = "bar", position = "identity", fun = "mean", color = "black", alpha=0.8) +
  geom_beeswarm(cex = 3, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = c("#0021A5", "#D32737"))+
  ylab("Axon count") + xlab("") +
  theme_minimal_hgrid(font_size = 20) + theme(text = element_text(family = "Source Sans 3"), legend.position = "none")

plotnn <- plot_grid(plotn)
ggsave2("Figures/NervePlot.png", dpi=300, width=180, height=180, unit="mm" )

#################
#muscle
muscle <- read_excel("~/Library/CloudStorage/GoogleDrive-justinvarholick@gmail.com/My Drive/UF/Ear Fidelity Project/Ear_Muscle_Checklist.xlsx")

cartregen_min <- select(cartregen, AnimalID, Slide, Sex, Length.y, AgeCat2)


muscle2 <- muscle %>% 
  filter(`uninj muscle` == 1) %>% 
  left_join(cartregen_min, by=c("AnimalID", "Slide")) %>% 
  group_by(AnimalID, Slide, AgeCat2, `uninj muscle`, Length.y) %>% 
  summarise(muscle_per = 100*(`fiber count`/Length.y))

ggplot(muscle2, aes(AgeCat2, muscle_per, fill = AgeCat2)) +
  geom_boxplot(alpha=0.8) +
  geom_beeswarm(cex = 2, size = 2) +
  scale_y_continuous(labels=function(x) paste0(x,"%"))+
  scale_fill_manual(values = c("white", "#0021A5", "#FA4616", "#D32737"))+
  ylab("Muscle fiber count / \nRegenerated Length") + xlab("") +
  theme_minimal_hgrid() + theme(axis.text.x=element_text(angle=90, vjust=0.75),
                                text = element_text(family = "Source Sans 3"), legend.position = "none")



