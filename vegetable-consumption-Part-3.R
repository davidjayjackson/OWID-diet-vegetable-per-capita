library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

vegetable_consumption <- read.csv("./DATA/20-vegetable-consumption-per-capita.csv")
colnames(vegetable_consumption) <- c("Country","Code","Year","Vegetables")

### Calculate and plot average per capita consumption by Year

veggie_average <- vegetable_consumption %>% group_by(Year) %>%
  summarise(Average = mean(Vegetables),
            Median = median(median(Vegetables)),
            Maximum = max(Vegetables))


veggie_average <- as.data.frame(veggie_average)
  ggplot(veggie_average) + geom_line(aes(x=Year,y=Average,col="Average")) +
  geom_line(aes(x=Year,y=Median,col="Median")) +
    geom_line(aes(x=Year,y=Maximum,col="Maximum")) +
    scale_y_log10() + labs(title="Vegetable Consumtion Per Capita",
                           y="Mean/Median/Maximum")
  
  
  ## Top Five Countries with highest per cap consumption
  
  vegie_top <- vegetable_consumption %>% filter(Year =="2017") %>%  top_n(-5,Vegetables)
  vegie_top <- as.data.frame(vegie_top)
  
  ### Bottom Five Countries by Per Capita Vegetable consumption 
  
  countries_top_five <- vegie_top %>% select(Country) %>% left_join(vegetable_consumption,by="Country")
  head(countries_top_five)
  tail(countries_top_five)
  
  ### Plots of Bottom Five Countries ranked by Per Capita Consumption
  
  ggplot(countries_top_five) + geom_line(aes(x=Year,y=Vegetables,col=Country)) +
    labs(title = "Vegetables Consumption Kg/Person/Year",subtitle = "( Bottom 5 countries)",
         y="Vegetables Consumed per person Kg")
  
  #### Break Out Bottom 5 to individual Plots
  
  ggplot(countries_top_five) + geom_col(aes(x=Year,y=Vegetables)) +
    facet_wrap(~Country,ncol=2,scale="free_y") + labs(title="Bottom Five Countries by Per Capita ",
                                subtitle = "(Vegetable Consumption per Capita)",
                                y="Vegetables by Kg/Person/Year") +
    geom_line(data=veggie_average,aes(x=Year,y=Average,col="Mean"))