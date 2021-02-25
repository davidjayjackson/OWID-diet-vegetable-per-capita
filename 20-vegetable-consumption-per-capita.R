library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

vegetable_consumption <- read.csv("./DATA/20-vegetable-consumption-per-capita.csv")
colnames(vegetable_consumption) <- c("Country","Code","Year","Vegetables")

### Worldwide vegetable Consumption per Capita
vegetable_consumption %>% filter(Country == "World") %>%
  ggplot() + geom_line(aes(x=Year,y=Vegetables)) +
  scale_y_continuous(labels=comma) +
  labs(title="Worldwide Vegetable Consumption Per Capita by Year",
       subtitle = "(1961 - 2017)",
       y="Kilogram/Person/Year")

## Top Five Countries with highest per cap consumption

vegie_top <- vegetable_consumption %>% filter(Year =="2017") %>%  top_n(5,Vegetables)
vegie_top <- as.data.frame(vegie_top)

ggplot(vegie_top) + geom_col(aes(x=reorder(Country,Vegetables),y=Vegetables)) + coord_flip() +
  labs(title="Top Five Countries Rated by Per Capita Consumption")

### Plot Bottom Five Countries by Per Capita Consumption

vegie_bottom <- vegetable_consumption %>% filter(Year =="2017") %>%  top_n(-5,Vegetables)
vegie_bottom <- as.data.frame(vegie_bottom)

ggplot(vegie_bottom) + geom_col(aes(x=reorder(Country,Vegetables),y=Vegetables)) + coord_flip() +
  labs(title="Bottom Five Countries Rata by Per Capita Consumption")