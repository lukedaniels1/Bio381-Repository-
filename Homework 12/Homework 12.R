# Homework 12 
# Luke Daniels 


library(ggplot2)
library(patchwork)
library(maps) 
head(USArrests)
View(USArrests)
library(readxl)
library(openxlsx)

write.xlsx(USArrests, '~/Desktop/Data.xlsx')


crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

USArrests$State <- USArrests$ID

states_map <- map_data("state")
map1 <- ggplot(crimes, aes(map_id = state)) + geom_map(aes(fill = Murder), map = states_map) + expand_limits(x = states_map$long, y = states_map$lat)
last_plot() + coord_map()
map1

states_map2 <- map_data("state")
map2 <- ggplot(crimes, aes(map_id = state)) + geom_map(aes(fill = Assault), map = states_map2) + expand_limits(x = states_map$long, y = states_map$lat)
last_plot() + coord_map()
map2

map1 +map2

Data <- read_excel("Data.xlsx")
View(Data)

options(scipen=999)  # turn-off scientific notation like 1e+48
theme_set(theme_bw())  # pre-set the bw theme.
data("USArrests", package = "ggplot2")


# Scatterplot
gg <- ggplot(Data, aes(x=UrbanPop, y=Assault)) + 
  geom_point(aes(col=State)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Population Vs Assault", 
       y="Assalt", 
       x="UrbanPopulation", 
       title="Scatterplot")

plot(gg)


g3 <- ggplot(Data, aes(x=UrbanPop, y=Assault)) + 
  geom_point(aes(col=State, size = Rape)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Population Vs Assault", 
       y="Assalt", 
       x="UrbanPopulation", 
       title="Scatterplot")

plot(g3)


g4 <- ggplot(Data,
       aes(UrbanPop, Murder, label = as.character(State))) + theme_classic() +
  geom_text(check_overlap = TRUE, size = 3) 
  
g4


g5 <- ggplot(Data,
             aes(UrbanPop, Murder, label = as.character(State))) + theme_classic() +
  geom_text(angle =45, size = 3)

g4

map1 + Plot1 - g5 + plot_layout(ncol = 1)
