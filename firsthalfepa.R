#Install and Load Pakcages
install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
library(tidyverse)
library(nflfastR)
library(ggimage)

#Load in NFL Logos CSV
nfllogos <- read_csv("/Users/dylanwilkerson/Documents/CSV:XLSX/NFL Logos.csv")

#Load 2023 play-by-play
data <- load_pbp(2023)

#Filter for first half data
firsthalf <- data %>%
  filter(qtr < 3)

#Drop NA's and filter for offense
firsthalfepaoff <- firsthalf %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(posteam) %>%
  summarise(total_epaoff = sum(epa))

#Drop NA's and filter for defense
firsthalfepadef <- firsthalf %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(defteam) %>%
  summarise(total_epadef = sum(epa))

#Rename columns
colnames(firsthalfepaoff)[1] = "team"
colnames(firsthalfepadef)[1] = "team"

#Join offense and defense
firsthalfepa <- left_join(firsthalfepaoff, firsthalfepadef, by = "team")

#Join full data with logos
final <- left_join(firsthalfepa, nfllogos, by = "team")

#Get mean EPA for the plot
meanepa<-mean(final$total_epaoff)

#Create plot
firsthalfplot <- ggplot(final, aes(total_epaoff, total_epadef)) +
  geom_image(aes(image = logo), size = .1) +
  geom_hline(yintercept = meanepa) +
  geom_vline(xintercept = meanepa) +
  labs(title = "EPA Earned/Allowed in the First Half",
       subtitle = "Created by Dylan Wilkerson/@wilkersonadylan",
       x = "Total EPA Gained on Offense",
       y = "Total EPA Allowed on Defense") +
  theme_light() +
  geom_text(aes(x=-75, y=40, label="Bad Offense, Bad Defense"),
            size = 5) +
  geom_text(aes(x=-75, y=-30, label="Bad Offense, Good Defense"),
            size = 5) +
  geom_text(aes(x= 40, y=-60, label="Good Offense, Good Defense"),
            size = 5) +
  geom_text(aes(x= 40, y=65, label="Good Offense, Bad Defense"),
            size = 5)

#Print plot
firsthalfplot
 

  

