# Data Visualization: Internet Access in NYC
# Author: Danya Sherbini
# Source: Citizen's Committee for Children of New York / US Census Bureau (https://data.cccnewyork.org/data/download#0,13/1300)
# Date: Aug 29, 2021
###############################################################################

library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("viridis")
library(viridis) # for diff color palettes/gradients
library(hrbrthemes) # for diff themes


############################
##### Working directory ####
############################

setwd("/Users/danya/Documents/Personal/2. School/DPSS 2021/R Programming/R Practice/Internet and Rent Data")
income <- read_csv("Household Internet Access by Income.csv")
rent <- read_csv("Monthly Rent.csv")
internet <- read_csv("Household Internet Access.csv")
burden <- read_csv("Median Rent Burden.csv")
rent2 <- read_csv("Median Monthly Rent.csv")
income2 <- read_csv("Median Incomes.csv")
boro <- read_csv("Boroughs.csv")

############################
####### Data cleaning ######
############################

# income
income <- income %>%
  spread(key = DataFormat, value = Data) %>%
  arrange(Fips) %>%
  filter(Fips <=503) %>%
  filter(TimeFrame==2019)
  
income <- income %>%
  rename(neighborhood = Location, year = TimeFrame, num_at_income_level = Number, pct_at_income_level = Percent, income_level = Income)

# income2

income2 <- income2 %>%
  arrange(Fips) %>%
  filter(Fips <=503) %>%
  filter(TimeFrame==2019) %>%
  filter(`Household Type`=="All Households") %>%
  rename(neighborhood = Location, hhs_type = `Household Type`, year = TimeFrame, median_income = Data) %>%
  select(neighborhood,year,median_income,Fips)

# rent
rent <- rent  %>%
  spread(key = DataFormat, value = Data) %>%
  arrange(Fips) %>%
  filter(Fips <=503) %>%
  filter(TimeFrame==2019)

rent <- rent %>%
  rename(neighborhood = Location, year = TimeFrame, no_hhs_at_rent_level = Number, pct_hhs_at_rent_level = Percent, rent_level = `Rent Level`)

#rent2

rent2 <- rent2 %>%
  arrange(Fips) %>%
  filter(Fips <=503) %>%
  filter(TimeFrame==2019) %>%
  rename(neighborhood = Location, year = TimeFrame, median_rent = Data) %>%
  select(neighborhood,year,median_rent,Fips)

#internet 
internet <- internet  %>%
  spread(key = DataFormat, value = Data) %>%
  arrange(Fips) %>%
  filter(Fips <=503) %>%
  filter(TimeFrame==2019)

internet <- internet %>%
  rename(neighborhood = Location, year = TimeFrame, num_hhs_wo_internet = Number, pct_hhs_wo_internet = Percent)

#burden
burden <- burden %>%
  spread(key = DataFormat, value = Data) %>%
  arrange(Fips) %>%
  filter(Fips <=503) %>%
  filter(TimeFrame==2019)

burden <- burden %>%
  rename(neighborhood= Location, year = TimeFrame, rent_burden = Percent)


# joining to get the full, combined data
full_data <- income %>%
  left_join(income2) %>%
  left_join(rent) %>%
  left_join(rent2) %>%
  left_join(internet) %>%
  left_join(burden) %>%
  left_join(boro)


############################
##### Making the plots #####
############################

# Starting with a scatter plot - showing correlation between median rent burden (i.e. the median % of income spent on rent in a neighborhood) by the number of households in that neighborhood without internet access.

p1 <- ggplot(full_data, aes(x= num_hhs_wo_internet, y= rent_burden)) +
  geom_point()+
  geom_smooth()
p1

# Adding a variable and making into a bubble plot.
p2 <- ggplot(full_data, aes(x= num_hhs_wo_internet, y= rent_burden, size = median_rent, fill=borough)) +
  geom_point(alpha=0.3, shape=21, color="black") +
  scale_size(range = c(4, 18), name="Median Monthly Rent ($)")+
  scale_fill_viridis(name="Borough",alpha = 1, begin = .1,end = .5,direction = -1,discrete = T, option = "H")+
  labs(title="Does Rent Burden Influence Household Access to Internet in NYC?", x="Number of Households w/o Internet   Access", y="Median Rent Burden (%)")+
  theme_classic()

p2

# The issue with p2 is that the legend for the borough color is too light, so I tried to make the color legend brighter/darker: 
p2.2 <- ggplot(full_data, aes(x= num_hhs_wo_internet, y= rent_burden, size = median_rent, fill=borough)) +
  geom_point(alpha=0.3, shape=21, color="black") +
  scale_size(range = c(4, 18), name="Median Monthly Rent ($)")+
  scale_fill_viridis(name="Borough",alpha = 1, begin = .1,end = 1,direction = -1,discrete = T, option = "D", guide = guide_legend(override.aes = list(size = 3,alpha = 1)))+
  labs(title="Does Rent Burden Influence Household Access to Internet in NYC?", x="Number of Households w/o Internet   Access", y="Median Rent Burden (%)")+
  theme_classic()

p2.2

# Trying out without viridis() bc I don't really need a gradient color scale for borough since it's discrete.
my_colors <- c("#FFF933","#33FF77","#33ECFF","#8D33FF","#FF33BB") # trying out diff manual color schemes
my_colors2 <- c("red","yellow","green","#33ECFF","orange") # trying out diff manual color schemes

# Using discrete color scheme to replot the bubble plot
p3 <- ggplot(full_data, aes(x= num_hhs_wo_internet, y= rent_burden, size = median_rent, fill=borough)) +
  geom_point(alpha=0.3, shape=21, color="black") +
  scale_size(range = c(2, 16), name="Median Monthly Rent ($)")+
  scale_fill_manual(values = my_colors2,
                                guide = guide_legend(override.aes = list(size = 3,alpha = 1)))+
  labs(title="Rent Burden and Household Internet Access in NYC Neighborhoods", x="Number of Households w/o Internet Access", y="Median Rent Burden (%)")+
  theme_classic()

p3

# Testing out a different theme, played around with scale of the bubbles. FINAL PLOT! 
p4 <- ggplot(full_data, aes(x= num_hhs_wo_internet, y= rent_burden, size = median_rent, fill=borough)) +
  geom_point(alpha=0.3, shape=21, color="black") +
  scale_size(range = c(2, 16), name="Median Monthly Rent ($)")+
  scale_fill_manual(values = my_colors2,
                    guide = guide_legend(override.aes = list(size = 3,alpha = 1)))+
  labs(title="Rent Burden and Household Internet Access in NYC Neighborhoods", x="Number of Households w/o Internet Access", y="Median Rent Burden (%)")+
  theme_ipsum()

p4
  


