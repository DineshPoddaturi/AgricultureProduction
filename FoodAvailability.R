require(tidyverse)
require(readxl)
require(lubridate)
require(gganimate)
require(magick)
require(gifski)
require(shiny)

require(shinythemes)
require(plotly)
require(crosstalk)
require(readr)
require(ggvis)
require(maps)
require(mapproj)
require(stringr)
require(rsconnect)

### Reading data
dairyPath <- "Data/FoodAvailabilityData/DairyProducts.xlsx"
allDairyAvailability<- lapply(excel_sheets(dairyPath), read_excel, path = path)

meatPath <- "Data/FoodAvailabilityData/RedMeatPoultryFish.xlsx"
allmeatAvailability<- lapply(excel_sheets(meatPath), read_excel, path = meatPath)

veggiesPath <- "Data/FoodAvailabilityData/FreshVegetables.xlsx"
allVeggiesAvailability<- lapply(excel_sheets(veggiesPath), read_excel, path = veggiesPath)

cornD <- read_excel("Data/ProductionData/Feed_Grains_Excel_Corn.xlsx") %>% as.data.frame()

## Cleaning and retrieving data
cheese <- cheeseData(cheese = allDairyAvailability)
icecream <- frozenDairyData(dairyFrozen = allDairyAvailability)
topmeats <- leadingMeatData(leadingMeat = allmeatAvailability)
allmeats <- allMeatData(allMeat = allmeatAvailability)
greens <- leafyGreensData(greens = allVeggiesAvailability)
cruciferous <- cruciferousData(cruciferous = allVeggiesAvailability)
roots <- rootsData(roots = allVeggiesAvailability)
cornUse <- cornUseData(corn = cornD)

##### Functions that return the data required for plots

cheeseData <- function(cheese){
  
  # path <- "Data/FoodAvailabilityData/DairyProducts.xlsx"
  # allDairyAvailability<- lapply(excel_sheets(path), read_excel, path = path)
  
  cheese_tot <- allDairyAvailability[[4]] %>% as.data.frame()
  names(cheese_tot) <- c("Year", "U.S.Populatin_million", "Production", "Imports", "BeginingStocks",
                         "TotalSupply", "Exports", "Ship_U.S_Territories", "EndingStocks",
                         "FoodAvail_USDA_Donations", "FoodAvail_Total_Not_Including_USDA_donations",
                         "FoodAvail_Total_Including_USDA_donations", "Percapita")
  cheese_tot <- cheese_tot[-(1:6),]
  # cheese_tot <- cheese_tot %>% as.numeric()
  cheese_tot <- sapply(cheese_tot, as.numeric) 
  cheese_tot <- round(cheese_tot,4) %>% as.data.frame()
  
  cheese_tot <- allDairyAvailability[[4]] %>% as.data.frame()
  names(cheese_tot) <- c("Year", "U.S.Populatin_million", "Production", "Imports", "BeginingStocks",
                         "TotalSupply", "Exports", "Ship_U.S_Territories", "EndingStocks",
                         "FoodAvail_USDA_Donations", "FoodAvail_Total_Not_Including_USDA_donations",
                         "FoodAvail_Total_Including_USDA_donations", "Percapita")
  cheese_tot <- cheese_tot[-(1:6),]
  cheese_percap_1 <- allDairyAvailability[[7]] %>% as.data.frame()
  cheese_percap_1 <- cheese_percap_1[-(1:4),]
  names(cheese_percap_1) <- c("Year", "U.S.Pop_million", "Cheddar_USA_Type", "Other_USA_Type",
                              "Total_USA_Type", "Provolone_Itl_Type", "Romano_Itl_Type", "Parmesan_Itl_Type", 
                              "Mozzarella_Itl_Type","Ricotta_Itl_Type", "Other_Itl_Type", "Total_Itl_Type", 
                              "Swiss", "Brick", "Muenster", "Cream_Neufchatel",
                              "Blue", "Hispanic", "Other", "Total", "ConsumedAs_Nat_Cheese", "ConsumedAs_Processed_Cheese",
                              "Consumed_Total", "NA","Processed_Cheese", "FoodSpreads_Cheese", "Total_Processed_Cheese",
                              "CheeseContant")
  
  cheese_percap_1 <- sapply(cheese_percap_1, as.numeric) %>% as.data.frame()
  cheese_percap_1 <- round(cheese_percap_1,4) %>% select(-"NA")
  cheese_percap_1 <- cheese_percap_1 %>% filter(Year<=1994)
  
  cheese_percap_2 <- allDairyAvailability[[8]] %>% as.data.frame()
  cheese_percap_2 <- cheese_percap_2[-(1:4),]
  names(cheese_percap_2) <- c("Year", "U.S.Pop_million", "Cheddar_USA_Type", "Other_USA_Type",
                              "Total_USA_Type", "Mozzarella_Itl_Type", "Other_Itl_Type", "Total_Itl_Type", 
                              "Swiss", "Brick", "Muenster", "Cream_Neufchatel","Blue", "Hispanic", "Other", 
                              "Total", "Total_OtherThan_American_NonCowCheese(Excl)", "Imported_Cheese_NotCows",
                              "Nat_Cheese_Total", "NA","Processed_Cheese", "FoodSpreads_Cheese", 
                              "Total_Processed_Cheese")
  cheese_percap_2 <- sapply(cheese_percap_2, as.numeric) %>% as.data.frame()
  cheese_percap_2 <- round(cheese_percap_2,4) %>% select(-"NA")
  cheese_percap_2 <- cheese_percap_2 %>% filter(Year<=2019)
  
  
  cheese_percap_1 <- cheese_percap_1 %>% mutate(
    Other_Itl_Type = Provolone_Itl_Type + Romano_Itl_Type + Parmesan_Itl_Type + Ricotta_Itl_Type + Other_Itl_Type) %>% 
    select(-Provolone_Itl_Type, - Romano_Itl_Type, - Parmesan_Itl_Type, - Ricotta_Itl_Type)
  
  ##### cheese_percap contains all the available cheese per capita in the U.S.
  cheese_percap <- bind_rows(cheese_percap_1, cheese_percap_2)
  
  ### I will subset the above data frame to get the columns that have all data.
  cheese_percap_subset <- cheese_percap %>% select(Year, Cheddar_USA_Type, Other_USA_Type, Mozzarella_Itl_Type, Other_Itl_Type,
                                                   Swiss, Brick, Muenster, Cream_Neufchatel, Blue, Hispanic, Processed_Cheese, 
                                                   FoodSpreads_Cheese)
  
  ## Now I convert the data from wide to long. Working woth long data is easier for visualization 
  cheese_percap_subset <- cheese_percap_subset %>% gather(CheeseType, PerCapitaAvail, -c(Year))
  
  return(cheese_percap_subset)
  
  # cheese_percap_plot <- cheese_percap_subset %>% 
  #     ggplot(aes(x=Year, y=PerCapitaAvail,color=CheeseType)) + geom_line() + 
  #     labs(x="Year", y="Cheese Available Per Capita (in pounds)")
  # 
  # cheese_animated_plot <- cheese_percap_plot + geom_point(size=2) + transition_reveal(Year)
  # 
  # cheese_animated_plot <- cheese_animated_plot + theme_light(base_size = 16)
  # 
  # cheese_animated_plot <- cheese_animated_plot + theme(panel.grid.major = element_blank(), 
  #                                                      panel.grid.minor = element_blank())
  # 
  # cheese_animated_plot <- cheese_animated_plot + theme(legend.position="none") + 
  #     geom_label(aes(x = Year, y=PerCapitaAvail, label=CheeseType))+ 
  #     labs(x="Year", y="Pounds")+
  #     ggtitle(paste0("Cheese Available (Per Capita) in the U.S.")) 
  # 
  # cheese_animation <- animate(cheese_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20,
  #                             renderer = gifski_renderer("CheeseAnimation.gif"))
  
}

frozenDairyData <- function(dairyFrozen){
  # path <- "Data/FoodAvailabilityData/DairyProducts.xlsx"
  # allDairyAvailability<- lapply(excel_sheets(path), read_excel, path = path)
  
  frozenDairy <- allDairyAvailability[[12]] %>% as.data.frame()
  
  frozenDairy <- frozenDairy[-(1:5),]
  names(frozenDairy) <- c("Year", "U.S.Pop_million", "RegIC_Tot", "RegIC_PCC",
                          "LowFatIC_Tot", "LowFatIC_PCC", "NonFatIC_Tot", "NonFatIC_PCC",
                          "Sherbet_Tot", "Sherbet_PCC", "MellorineMix_Total", "MellorineMix_PCC",
                          "FroYo_Tot", "FroYo_PCC", "WaterJuice_Tot", "WaterJuice_PCC", "Other_Tot",
                          "Other_PCC", "TotFrozenDairy_Tot", "TotFrozenDairy_PCC", "NA", "NA1")
  
  frozenDairy <- sapply(frozenDairy, as.numeric) %>% as.data.frame()
  frozenDairy <- round(frozenDairy,4) %>% select(-"NA", -"NA1", -"WaterJuice_Tot", -"WaterJuice_PCC")
  
  frozenDairy <- frozenDairy %>% filter(Year<=2019)
  
  
  frozenDairy_totals <- frozenDairy %>% select(Year, RegIC_Tot, LowFatIC_Tot, NonFatIC_Tot,
                                               Sherbet_Tot, MellorineMix_Total, FroYo_Tot)
  names(frozenDairy_totals) <- c("Year", "Reg IceCream", "Low Fat IceCream", "Non Fat IceCream", "Sherbet", 
                                 "Mellorine Mix", "FroYo")
  frozenDairy_totals <- frozenDairy_totals %>% gather(Type, Pounds, -Year)
  
  return(frozenDairy_totals)
  
  # frozenDairy_plot <- frozenDairy_totals %>% ggplot(aes(x = Year, y = Pounds, color = Type)) + geom_line()
  # 
  # frozenDairy_animated_plot <- frozenDairy_plot + geom_point(size=2) + transition_reveal(Year)
  # 
  # frozenDairy_animated_plot <- frozenDairy_animated_plot + theme_light(base_size = 16)
  # 
  # frozenDairy_animated_plot <- frozenDairy_animated_plot + theme(panel.grid.major = element_blank(), 
  #                                                                panel.grid.minor = element_blank())
  # 
  # frozenDairy_animated_plot <- frozenDairy_animated_plot + theme(legend.position="none") + 
  #     geom_label(aes(x = Year, y=Pounds, label=Type))+ 
  #     labs(x="Year", y="Pounds")+
  #     ggtitle("Frozen Dairy Available in the U.S. (million pounds)") 
  # 
  # frozenDairy_animation <- animate(frozenDairy_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20,
  #                                  renderer = gifski_renderer("frozenDairyAnimation.gif"))
  
}

leadingMeatData <- function(leadingMeat){
  #### With the following code we can read all the sheets in the excel file. 
  # meatPath <- "Data/FoodAvailabilityData/RedMeatPoultryFish.xlsx"
  # allmeatAvailability<- lapply(excel_sheets(meatPath), read_excel, path = meatPath)
  
  #### Leading Meat(Boneless weight)
  meatLeading <- allmeatAvailability[[5]] %>% as.data.frame()
  names(meatLeading) <- c("Year", "U.S.Pop_million", "Beef_PCC", "Pork_PCC",
                          "Chicken_PCC", "Total_LeadingMeat_PCC")
  meatLeading <- meatLeading[-(1:5),]
  
  meatLeading <- sapply(meatLeading, as.numeric) %>% as.data.frame()
  meatLeading <- round(meatLeading,4) %>% filter(Year<=2019)
  
  leadingMeat_Long <- meatLeading %>% select(-U.S.Pop_million, -Total_LeadingMeat_PCC)  %>% gather(Type,Value, -Year)
  
  return(leadingMeat_Long)
  
  # leadingMeat_plot <- leadingMeat_Long %>% ggplot(aes(x=Year, y = Value, color = Type)) + geom_line()
  # 
  # leadingMeat_animated_plot <- leadingMeat_plot + geom_point(size=2) + transition_reveal(Year)
  # 
  # leadingMeat_animated_plot <- leadingMeat_animated_plot + theme_light(base_size = 16)
  # 
  # leadingMeat_animated_plot <- leadingMeat_animated_plot + theme(panel.grid.major = element_blank(), 
  #                                                                panel.grid.minor = element_blank())
  # 
  # leadingMeat_animated_plot <- leadingMeat_animated_plot + theme(legend.position="none") + 
  #     geom_label(aes(x = Year, y=Value, label=Type))+ 
  #     labs(x="Year", y="Pounds")+
  #     ggtitle(paste0("Leading Meat Available (Per Capita) in the U.S. ")) 
  # 
  # leadingMeat_animated_plot <- animate(leadingMeat_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20,
  #                                      renderer = gifski_renderer("LeadingMeatAnimation.gif"))
  
}

allMeatData <- function(allMeat){
  #### With the following code we can read all the sheets in the excel file. 
  # meatPath <- "Data/FoodAvailabilityData/RedMeatPoultryFish.xlsx"
  # allmeatAvailability<- lapply(excel_sheets(meatPath), read_excel, path = meatPath)
  
  meatBoneless <- allmeatAvailability[[4]] %>% as.data.frame()
  names(meatBoneless) <- c("Year", "U.S.Pop_million", "Beef_PCC", "Veal_PCC", "Pork_PCC", "Lamb_PCC", "Total_RedMeat",
                           "Chicken_PCC", "Turkey_PCC", "Total_Poultry", "Fish_ShellFish_PCC", "Total_Meat_PCC")
  meatBoneless <- meatBoneless[-(1:5),]
  
  meatBoneless <- sapply(meatBoneless, as.numeric) %>% as.data.frame()
  meatBoneless <- round(meatBoneless,4)
  
  #### I am only getting per capita availability of boneless meat
  meatBoneless_subset <- meatBoneless %>% transmute(Year = Year, Population = U.S.Pop_million, Beef = Beef_PCC, Veal = Veal_PCC, 
                                                    Pork = Pork_PCC, Lamb = Lamb_PCC, Chicken = Chicken_PCC, 
                                                    Fish = Fish_ShellFish_PCC) %>% filter(Year <= 2019)
  
  meatBoneless_long <- meatBoneless_subset %>% select(-Population) %>% gather(Type, Value, -Year)
  
  return(meatBoneless_long)
  
  # meatBoneless_plot <- meatBoneless_long %>% ggplot(aes(x=Year, y=Value, color = Type)) + geom_line()
  # 
  # meatBoneless_animated_plot <- meatBoneless_plot + geom_point(size=2) + transition_reveal(Year)
  # 
  # meatBoneless_animated_plot <- meatBoneless_animated_plot + theme_light(base_size = 16)
  # 
  # meatBoneless_animated_plot <- meatBoneless_animated_plot + theme(panel.grid.major = element_blank(), 
  #                                                                  panel.grid.minor = element_blank())
  # 
  # meatBoneless_animated_plot <- meatBoneless_animated_plot + theme(legend.position="none") + 
  #     geom_label(aes(x = Year, y=Value, label=Type))+ 
  #     labs(x="Year", y="Pounds")+
  #     ggtitle(paste0("Meat Available (Per Capita) in the U.S. ")) 
  # 
  # meatBoneless_animated_plot <- animate(meatBoneless_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20,
  #                                       renderer = gifski_renderer("BonelessMeatAnimation.gif"))
  
}

leafyGreensData <- function(greens){
  # veggiesPath <- "Data/FoodAvailabilityData/FreshVegetables.xlsx"
  # allVeggiesAvailability<- lapply(excel_sheets(veggiesPath), read_excel, path = veggiesPath)
  
  allVeggiesAvail_PCC_FarmWeight <- allVeggiesAvailability[[2]]
  allVeggiesAvail_PCC_FarmWeight <- allVeggiesAvail_PCC_FarmWeight %>% as.data.frame()
  allVeggiesAvail_PCC_FarmWeight <- allVeggiesAvail_PCC_FarmWeight[-(1:2),]
  allVeggiesAvail_PCC_FarmWeight <- sapply(allVeggiesAvail_PCC_FarmWeight, as.numeric) %>% as.data.frame()
  allVeggiesAvail_PCC_FarmWeight <- round(allVeggiesAvail_PCC_FarmWeight,4)
  
  allVeggiesAvail_PCC_Greens <- allVeggiesAvail_PCC_FarmWeight %>% select(Year, `Collard greens`, Kale, `Lettuce head`,
                                                                          `Romaine and leaf`, `Mustard greens`, Spinach,
                                                                          `Turnip greens`)
  names(allVeggiesAvail_PCC_Greens) <- c("Year", "Collard Greens", "Kale", "Lettuce Head", "Romaine", "Mustard Greens", "Spinach",
                                         "Turnip Greens")
  
  allVeggiesAvail_PCC_Greens <- allVeggiesAvail_PCC_Greens %>% gather(Vegetable, Availability, -Year)
  
  return(allVeggiesAvail_PCC_Greens)
  
  # allVeggiesAvail_PCC_Greens_plot <- allVeggiesAvail_PCC_Greens %>% ggplot(aes(x=Year, y = Availability, color = Vegetable)) + geom_line()
  # 
  # allVeggiesAvail_PCC_Greens_plot <- allVeggiesAvail_PCC_Greens_plot + geom_point(size=2) + transition_reveal(Year)
  # 
  # allVeggiesAvail_PCC_Greens_plot <- allVeggiesAvail_PCC_Greens_plot + theme_light(base_size = 16)
  # 
  # allVeggiesAvail_PCC_Greens_plot <- allVeggiesAvail_PCC_Greens_plot + theme(panel.grid.major = element_blank(), 
  #                                                                            panel.grid.minor = element_blank())
  # 
  # allVeggiesAvail_PCC_Greens_Animated_plot <- allVeggiesAvail_PCC_Greens_plot + theme(legend.position="none") + 
  #     geom_label(aes(x = Year, y=Availability, label=Vegetable), nudge_x =1, size=4, hjust=0)+ 
  #     labs(x="Year", y="Pounds")+
  #     ggtitle(paste0("Percapita Leafy Greens Availability in the U.S."))
  # 
  # allVeggiesAvail_PCC_Greens_Animated_plot <- animate(allVeggiesAvail_PCC_Greens_Animated_plot, height = 500, width = 1000,fps=6, 
  #                                                     start_pause=5, end_pause=20, renderer = gifski_renderer("VeggiesAvailGreen.gif"))
  
  
}

cruciferousData <- function(cruciferous){
  # veggiesPath <- "Data/FoodAvailabilityData/FreshVegetables.xlsx"
  # allVeggiesAvailability<- lapply(excel_sheets(veggiesPath), read_excel, path = veggiesPath)
  
  allVeggiesAvail_PCC_FarmWeight <- allVeggiesAvailability[[2]]
  allVeggiesAvail_PCC_FarmWeight <- allVeggiesAvail_PCC_FarmWeight %>% as.data.frame()
  allVeggiesAvail_PCC_FarmWeight <- allVeggiesAvail_PCC_FarmWeight[-(1:2),]
  allVeggiesAvail_PCC_FarmWeight <- sapply(allVeggiesAvail_PCC_FarmWeight, as.numeric) %>% as.data.frame()
  allVeggiesAvail_PCC_FarmWeight <- round(allVeggiesAvail_PCC_FarmWeight,4)
  
  allVeggiesAvail_PCC_Cruciferous <- allVeggiesAvail_PCC_FarmWeight %>% select(Year, Cabbage, Cauliflower, Broccoli, `Brussels sprouts`)
  names(allVeggiesAvail_PCC_Cruciferous) <- c("Year", "Cabbage", "Cauliflower", "Broccoli", "Brussels Sprouts")
  
  allVeggiesAvail_PCC_Cruciferous <- allVeggiesAvail_PCC_Cruciferous %>% gather(Vegetable, Availability, -Year)
  
  return(allVeggiesAvail_PCC_Cruciferous)
  
  # allVeggiesAvail_PCC_Cruciferous_plot <- allVeggiesAvail_PCC_Cruciferous %>% ggplot(aes(x=Year, y = Availability, color = Vegetable)) + geom_line()
  # 
  # allVeggiesAvail_PCC_Cruciferous_plot <- allVeggiesAvail_PCC_Cruciferous_plot + geom_point(size=2) + transition_reveal(Year)
  # 
  # allVeggiesAvail_PCC_Cruciferous_plot <- allVeggiesAvail_PCC_Cruciferous_plot + theme_light(base_size = 16)
  # 
  # allVeggiesAvail_PCC_Cruciferous_plot <- allVeggiesAvail_PCC_Cruciferous_plot + theme(panel.grid.major = element_blank(), 
  #                                                                                      panel.grid.minor = element_blank())
  # 
  # allVeggiesAvail_PCC_Cruciferous_Animated_plot <- allVeggiesAvail_PCC_Cruciferous_plot + theme(legend.position="none") + 
  #     geom_label(aes(x = Year, y=Availability, label=Vegetable), nudge_x =1, size=4, hjust=0)+ 
  #     labs(x="Year", y="Pounds")+
  #     ggtitle(paste0("Percapita Cruciferous Vegetable Availability in the U.S."))
  # 
  # allVeggiesAvail_PCC_Cruciferous_Animated_plot <- animate(allVeggiesAvail_PCC_Cruciferous_Animated_plot, height = 500, width = 1000,fps=6, 
  #                                                          start_pause=5, end_pause=20, 
  #                                                          renderer = gifski_renderer("VeggiesAvailCruciferous.gif"))
  
}

rootsData <- function(roots){
  # veggiesPath <- "Data/FoodAvailabilityData/FreshVegetables.xlsx"
  # allVeggiesAvailability<- lapply(excel_sheets(veggiesPath), read_excel, path = veggiesPath)
  
  allVeggiesAvail_PCC_FarmWeight <- allVeggiesAvailability[[2]]
  allVeggiesAvail_PCC_FarmWeight <- allVeggiesAvail_PCC_FarmWeight %>% as.data.frame()
  allVeggiesAvail_PCC_FarmWeight <- allVeggiesAvail_PCC_FarmWeight[-(1:2),]
  allVeggiesAvail_PCC_FarmWeight <- sapply(allVeggiesAvail_PCC_FarmWeight, as.numeric) %>% as.data.frame()
  allVeggiesAvail_PCC_FarmWeight <- round(allVeggiesAvail_PCC_FarmWeight,4)
  
  allVeggiesAvail_PCC_Roots <- allVeggiesAvail_PCC_FarmWeight %>% select(Year, Carrots, Garlic, Onions, Potatoes, Radishes, `Sweet potatoes`)
  names(allVeggiesAvail_PCC_Roots) <- c("Year", "Carrots", "Garlic", "Onions", "Potatoes", "Radishes", "Sweet Potatoes")
  
  allVeggiesAvail_PCC_Roots <- allVeggiesAvail_PCC_Roots %>% gather(Vegetable, Availability, -Year)
  
  return(allVeggiesAvail_PCC_Roots)
  
  # allVeggiesAvail_PCC_Roots_plot <- allVeggiesAvail_PCC_Roots %>% ggplot(aes(x=Year, y = Availability, color = Vegetable)) + geom_line()
  # 
  # allVeggiesAvail_PCC_Roots_plot <- allVeggiesAvail_PCC_Roots_plot + geom_point(size=2) + transition_reveal(Year)
  # 
  # allVeggiesAvail_PCC_Roots_plot <- allVeggiesAvail_PCC_Roots_plot + theme_light(base_size = 16)
  # 
  # allVeggiesAvail_PCC_Roots_plot <- allVeggiesAvail_PCC_Roots_plot + theme(panel.grid.major = element_blank(), 
  #                                                                          panel.grid.minor = element_blank())
  # 
  # allVeggiesAvail_PCC_Roots_Animated_plot <- allVeggiesAvail_PCC_Roots_plot + theme(legend.position="none") + 
  #     geom_label(aes(x = Year, y=Availability, label=Vegetable), nudge_x =1, size=4, hjust=0)+ 
  #     labs(x="Year", y="Pounds")+
  #     ggtitle(paste0("Percapita Roots Vegetable Availability in the U.S."))
  # 
  # allVeggiesAvail_PCC_Roots_Animated_plot <- animate(allVeggiesAvail_PCC_Roots_Animated_plot, height = 500, width = 1000,fps=6, 
  #                                                    start_pause=5, end_pause=20, 
  #                                                    renderer = gifski_renderer("VeggiesAvailRoots.gif"))
  
}

cornUseData <- function(corn){
  
  # corn <- read_excel("Data/ProductionData/Feed_Grains_Excel_Corn.xlsx") %>% as.data.frame()
  
  corn <- corn %>% select(Year, Attribute, Unit, Amount)
  # unique(corn$Attribute)
  # 
  # corn_prices <- corn %>% filter(Attribute=="Prices received by farmers")
  # 
  # corn_acerage <- corn %>% filter(Attribute=="Planted acreage")
  # 
  # corn_yield_acre <- corn %>% filter(Attribute == "Yield per harvested acre")
  
  
  corn_use <- corn %>% filter(Attribute %in% c('Food, alcohol, and industrial use', 'Seed use',
                                               'Feed and residual use', 'High-fructose corn syrup (HFCS) use',
                                               'Glucose and dextrose use', 'Starch use', 
                                               'Alcohol for beverages and manufacturing use'))
  
  corn_use <- corn_use %>% select(Year, Attribute, Amount)
  corn_use <- corn_use %>% spread(Attribute, Amount)
  names(corn_use) <- c('Year', 'Alcohol_Beverages_Manufacturing', 'Feed', 'Food_Alcohol_Industrial',
                       'Glucose_Dextrose', 'High-Fructose_Corn_Syrup', 'Seed', 'Starch')
  corn_use <- corn_use %>% gather(Type, Value, -Year)
  
  return(corn_use)
  # corn_use_plot <- corn_use %>% ggplot(aes(x = Year, y = Value, color = Type)) + geom_line()
  # 
  # corn_use_animated_plot <- corn_use_plot + geom_point(size=2) + transition_reveal(Year)
  # 
  # corn_use_animated_plot <- corn_use_animated_plot + theme_light(base_size = 16)
  # 
  # corn_use_animated_plot <- corn_use_animated_plot + theme(panel.grid.major = element_blank(), 
  #                                                          panel.grid.minor = element_blank())
  # 
  # corn_use_animated_plot <- corn_use_animated_plot + theme(legend.position="none") + 
  #     geom_label(aes(x = Year, y=Value, label=Type))+ 
  #     labs(x="Year", y="Million bushels")+
  #     ggtitle(paste0("Corn use in the U.S.")) 
  # corn_use_animated_plot <- animate(corn_use_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20,
  #                                   renderer = gifski_renderer("CornUseAnimation.gif"))
  
}




# Define UI for Per capita Food Availability  - Unite States
ui <- fluidPage(
  # shinythemes::themeSelector(),
  # Application title
  titlePanel("Food Availability/Usage in the United States"),
  
  sidebarPanel(
    selectInput("FoodType",label="Food Type",choices=c("Cheese","Frozen Dairy",
                                                       "Leading Meat (boneless)","All Meat (boneless)",
                                                       "Leafy Greens","Cruciferous Vegetables","Root Veggies",
                                                       "Corn")),
    fluidRow(
      column(7,actionButton("stop","Stop")),
      column(3,actionButton("play","Play"))
    )
  ),
  
  # Showing all the plots
  mainPanel(
    tabsetPanel(
      tabPanel("Availability",plotlyOutput("Availability"))
    )
  )
)



# Defining server logic 
server <- function(input, output) {
  
  datasetInput <- reactive({
    
    # here we read the selected Indicator to use the appropriate data
    if(input$FoodType=="Cheese"){
      
      dataV <- cheese
      
    } else if(input$FoodType=="Frozen Dairy"){
      
      dataV <- icecream
      
    }else if(input$FoodType=="Leading Meat (boneless)"){
      
      dataV <- topmeats
      
    } else if(input$FoodType=="All Meat (boneless)"){
      
      dataV <- allmeats
      
    } else if(input$FoodType=="Leafy Greens"){
      
      dataV <- greens
      
    } else if(input$FoodType=="Cruciferous Vegetables"){
      
      dataV <- cruciferous
      
    } else if(input$FoodType=="Root Veggies"){
      
      dataV <- roots
      
    } else if(input$FoodType=="Corn"){
      
      dataV <- cornUse
      
    } 
    
    data <- dataV
    data
    
  })
  
  output$Availability <- renderPlot({
    
    dataSelected <- datasetInput()
    
    if(input$FoodType=="Cheese"){
      
      cheese_percap_plot <- dataSelected %>%
        ggplot(aes(x=Year, y=PerCapitaAvail,color=CheeseType)) + geom_line() +
        labs(x="Year", y="Cheese Available Per Capita (in pounds)")
      
      cheese_animated_plot <- cheese_percap_plot + geom_point(size=2) + transition_reveal(Year)
      
      cheese_animated_plot <- cheese_animated_plot + theme_light(base_size = 16)
      
      cheese_animated_plot <- cheese_animated_plot + theme(panel.grid.major = element_blank(),
                                                           panel.grid.minor = element_blank())
      
      cheese_animated_plot <- cheese_animated_plot + theme(legend.position="none") +
          geom_label(aes(x = Year, y=PerCapitaAvail, label=CheeseType))+
          labs(x="Year", y="Pounds")+
          ggtitle(paste0("Cheese Available (Per Capita) in the U.S."))
      # 
      # animation <- animate(cheese_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20)
      
      animation <- cheese_animated_plot
      
    }else if(input$FoodType=="Frozen Dairy"){
      
      frozenDairy_plot <- dataSelected %>% ggplot(aes(x = Year, y = Pounds, color = Type)) + geom_line()
      
      frozenDairy_animated_plot <- frozenDairy_plot + geom_point(size=2) + transition_reveal(Year)
      
      frozenDairy_animated_plot <- frozenDairy_animated_plot + theme_light(base_size = 16)
      
      frozenDairy_animated_plot <- frozenDairy_animated_plot + theme(panel.grid.major = element_blank(),
                                                                     panel.grid.minor = element_blank())
      
      frozenDairy_animated_plot <- frozenDairy_animated_plot + theme(legend.position="none") +
          geom_label(aes(x = Year, y=Pounds, label=Type))+
          labs(x="Year", y="Pounds")+
          ggtitle("Frozen Dairy Available in the U.S. (million pounds)")
      # 
      # animation <- animate(frozenDairy_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20)
      
      animation <- frozenDairy_animated_plot
      
    }else if(input$FoodType=="Leading Meat (boneless)"){
      
      leadingMeat_plot <- dataSelected %>% ggplot(aes(x=Year, y = Value, color = Type)) + geom_line()
      
      leadingMeat_animated_plot <- leadingMeat_plot + geom_point(size=2) + transition_reveal(Year)
      
      leadingMeat_animated_plot <- leadingMeat_animated_plot + theme_light(base_size = 16)
      
      leadingMeat_animated_plot <- leadingMeat_animated_plot + theme(panel.grid.major = element_blank(),
                                                                     panel.grid.minor = element_blank())
      
      leadingMeat_animated_plot <- leadingMeat_animated_plot + theme(legend.position="none") +
          geom_label(aes(x = Year, y=Value, label=Type))+
          labs(x="Year", y="Pounds")+
          ggtitle(paste0("Leading Meat Available (Per Capita) in the U.S. "))
      # 
      # animation <- animate(leadingMeat_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20)
      
      animation <- leadingMeat_animated_plot
      
    }else if(input$FoodType=="All Meat (boneless)"){
      
      meatBoneless_plot <- dataSelected %>% ggplot(aes(x=Year, y=Value, color = Type)) + geom_line()
      
      meatBoneless_animated_plot <- meatBoneless_plot + geom_point(size=2) + transition_reveal(Year)
      
      meatBoneless_animated_plot <- meatBoneless_animated_plot + theme_light(base_size = 16)
      
      meatBoneless_animated_plot <- meatBoneless_animated_plot + theme(panel.grid.major = element_blank(),
                                                                       panel.grid.minor = element_blank())
      
      meatBoneless_animated_plot <- meatBoneless_animated_plot + theme(legend.position="none") +
          geom_label(aes(x = Year, y=Value, label=Type))+
          labs(x="Year", y="Pounds")+
          ggtitle(paste0("Meat Available (Per Capita) in the U.S. "))
      # 
      # animation <- animate(meatBoneless_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20)
      
      animation <- meatBoneless_animated_plot
      
      
    }else if(input$FoodType=="Leafy Greens"){
      
      allVeggiesAvail_PCC_Greens_plot <- dataSelected %>% ggplot(aes(x=Year, y = Availability, color = Vegetable)) + geom_line()
      
      allVeggiesAvail_PCC_Greens_plot <- allVeggiesAvail_PCC_Greens_plot + geom_point(size=2) + transition_reveal(Year)
      
      allVeggiesAvail_PCC_Greens_plot <- allVeggiesAvail_PCC_Greens_plot + theme_light(base_size = 16)
      
      allVeggiesAvail_PCC_Greens_plot <- allVeggiesAvail_PCC_Greens_plot + theme(panel.grid.major = element_blank(),
                                                                                 panel.grid.minor = element_blank())
      
      allVeggiesAvail_PCC_Greens_Animated_plot <- allVeggiesAvail_PCC_Greens_plot + theme(legend.position="none") +
          geom_label(aes(x = Year, y=Availability, label=Vegetable), nudge_x =1, size=4, hjust=0)+
          labs(x="Year", y="Pounds")+
          ggtitle(paste0("Percapita Leafy Greens Availability in the U.S."))
      # 
      # animation <- animate(allVeggiesAvail_PCC_Greens_Animated_plot, height = 500, width = 1000,fps=6,
      #                                                     start_pause=5, end_pause=20)
      
      animation <- allVeggiesAvail_PCC_Greens_plot
      
    }else if(input$FoodType=="Cruciferous Vegetables"){
      
      
      allVeggiesAvail_PCC_Cruciferous_plot <- dataSelected %>% ggplot(aes(x=Year, y = Availability, color = Vegetable)) + geom_line()
      
      allVeggiesAvail_PCC_Cruciferous_plot <- allVeggiesAvail_PCC_Cruciferous_plot + geom_point(size=2) + transition_reveal(Year)
      
      allVeggiesAvail_PCC_Cruciferous_plot <- allVeggiesAvail_PCC_Cruciferous_plot + theme_light(base_size = 16)
      
      allVeggiesAvail_PCC_Cruciferous_plot <- allVeggiesAvail_PCC_Cruciferous_plot + theme(panel.grid.major = element_blank(),
                                                                                           panel.grid.minor = element_blank())
      
      allVeggiesAvail_PCC_Cruciferous_Animated_plot <- allVeggiesAvail_PCC_Cruciferous_plot + theme(legend.position="none") +
          geom_label(aes(x = Year, y=Availability, label=Vegetable), nudge_x =1, size=4, hjust=0)+
          labs(x="Year", y="Pounds")+
          ggtitle(paste0("Percapita Cruciferous Vegetable Availability in the U.S."))
      # 
      # animation <- animate(allVeggiesAvail_PCC_Cruciferous_Animated_plot, height = 500, width = 1000,fps=6,
      #                                                          start_pause=5, end_pause=20)
      
      animation <- allVeggiesAvail_PCC_Cruciferous_plot
      
      
    }else if(input$FoodType=="Root Veggies"){
      
      allVeggiesAvail_PCC_Roots_plot <- dataSelected %>% ggplot(aes(x=Year, y = Availability, color = Vegetable)) + geom_line()
      
      allVeggiesAvail_PCC_Roots_plot <- allVeggiesAvail_PCC_Roots_plot + geom_point(size=2) + transition_reveal(Year)
      
      allVeggiesAvail_PCC_Roots_plot <- allVeggiesAvail_PCC_Roots_plot + theme_light(base_size = 16)
      
      allVeggiesAvail_PCC_Roots_plot <- allVeggiesAvail_PCC_Roots_plot + theme(panel.grid.major = element_blank(),
                                                                               panel.grid.minor = element_blank())
      
      allVeggiesAvail_PCC_Roots_Animated_plot <- allVeggiesAvail_PCC_Roots_plot + theme(legend.position="none") +
          geom_label(aes(x = Year, y=Availability, label=Vegetable), nudge_x =1, size=4, hjust=0)+
          labs(x="Year", y="Pounds")+
          ggtitle(paste0("Percapita Roots Vegetable Availability in the U.S."))
      # 
      # animation <- animate(allVeggiesAvail_PCC_Roots_Animated_plot, height = 500, width = 1000,fps=6,
      #                                                    start_pause=5, end_pause=20)
      
      animation <- allVeggiesAvail_PCC_Roots_plot
      
    }else if(input$FoodType=="Corn"){
      
      corn_use_plot <- dataSelected %>% ggplot(aes(x = Year, y = Value, color = Type)) + geom_line()
      
      corn_use_animated_plot <- corn_use_plot + geom_point(size=2) + transition_reveal(Year)
      
      corn_use_animated_plot <- corn_use_animated_plot + theme_light(base_size = 16)
      
      corn_use_animated_plot <- corn_use_animated_plot + theme(panel.grid.major = element_blank(),
                                                               panel.grid.minor = element_blank())
      
      corn_use_animated_plot <- corn_use_animated_plot + theme(legend.position="none") +
          geom_label(aes(x = Year, y=Value, label=Type))+
          labs(x="Year", y="Million bushels")+
          ggtitle(paste0("Corn use in the U.S."))
      # animation <- animate(corn_use_animated_plot, height = 500, width = 1000,fps=6, start_pause=5, end_pause=20)
      
      animation <- corn_use_animated_plot
      
    }
    # ggplotly(animation,height = 500)
    animation
  })
  
}


# Bind ui and server together
shinyApp(ui, server)









