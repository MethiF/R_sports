#Tile Map#

#Load packages#
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(ggimage)
library(ggtext)
library(readxl)
library(magick)
library(dplyr)

#Add Monsterrat font#
library(showtext)
font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()


#Read data from file#
data <- read_excel("C:/Users/frme/OneDrive - Folkehelseinstituttet/Dokumenter/PhD/R_sports/memberships.xlsx")


#I have downloaded all images and added them to "Link". URL to all images are found in Pictogram column#

#Create long dataset#
df_long <- data %>%
  pivot_longer(
    cols = `2016`:`2022`,         # Specify the columns that contain the years
    names_to = "Year",            # Name the new column 'Year'
    values_to = "Member"          # Name the new column for the values 'Member'
  )

#Create groups for color fills#
df_long <- df_long %>%
  mutate(Group = case_when(
    Member > 100000              ~ "> 100 000",
    Member > 50000 & Member <= 100000  ~ "50 000 - 100 000",
    Member > 25000 & Member <= 50000   ~ "25 000 - 50 000",
    Member > 10000 & Member <= 25000   ~ "10 000 - 25 000",
    Member > 5000 & Member <= 10000    ~ "5 000 - 10 000",
    Member > 1000 & Member <= 5000     ~ "1 000 - 5 000",
    Member <= 1000                     ~ "< 1 000"
  ))

#Create factors#
df_long <- df_long %>%
  mutate(Group = factor(Group,
                        levels = c("< 1 000", 
                                   "1 000 - 5 000", 
                                   "5 000 - 10 000", 
                                   "10 000 - 25 000", 
                                   "25 000 - 50 000", 
                                   "50 000 - 100 000", 
                                   "> 100 000")))

#Add spaces between the different colors#
df_long <- df_long %>%
  mutate(y_pos = rep(c(1:6, 8:9, 11:19, 21:30, 32:39, 41:57, 59:62), each = 7)[1:n()])


#Create palette#
pal<-viridis(n=7, option="viridis", direction = -1)

#Create graph#
ggplot(data = df_long, mapping=aes(x=as.character(Year), y=y_pos, fill = Group)) +
  geom_tile(color = "white", width=.9, height=.9)+
  geom_text(mapping=aes(label=Name, y=y_pos, x=-6.5), size=3, hjust=0, color="#909090", family = "Montserrat") +
  geom_image(mapping=aes(x = -7, y = y_pos, image=Link), size = 0.015)+
  scale_x_discrete(breaks=as.character(unique(df_long$Year)), labels = c("'16", "'17", "'18", "'19", "'20", "'21", "'22"), expand=expansion(mult=c(1.5,0.1), add=c(1,1)), position = "top")+
  scale_y_reverse() +
  theme_void() +
  scale_fill_manual(values = pal) +
  labs(x="", y="", fill="Total members", title = "Trends in Sports Memberships by Year in Norway (2016–2022)")+
  theme(text=element_text(family="Montserrat"),
        axis.text.x = element_text(size=8, vjust = -13.5),
        axis.ticks=element_blank(),
        panel.background = element_blank())


ggsave("sportmembers.svg",width=10, height=20)
