library(tidyverse)
library(data.table)
library(openxlsx)
library(maps)
library(countrycode)
library(pacman)
library(ggplot2)
library(data.table)
library(rnaturalearth)
library(gganimate)
library(gifski)
library(ggthemes)
library(ggiraph)



rm(list = ls())

## Download Official repo for the tidytuesday project.
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
#write.ex (coffee_ratings,"D:/CEU Classes/DV2/Final_Project/coffee_data.csv")

## Confirm the database to data table.
coffee_ratings <- data.table(coffee_ratings)
str(coffee_ratings)
#write.xlsx(coffee_ratings,"D:/CEU Classes/DV2/Final_Project/coffee_data.xlsx")

## Delete the unnecessary samples from country of origin column.
coffee_ratings$country_of_origin<- gsub("Cs*\\(.*", "",  coffee_ratings$country_of_origin)

## Finding the country code. 
coffee_ratings$country_code <- countrycode(coffee_ratings$country_of_origin, origin = "country.name",
                                           destination = "iso3c")

## Find the number of countries and the average of rating by country of origin.
Avr_rating <- coffee_ratings[,.(count = .N, mean_country_rating = mean(total_cup_points),
                         adm0_a3 = country_code, species = species), by = country_of_origin]

##################################################################################
## 1)
## Creating the data map.
world <- ne_countries(scale = "medium", returnclass = "sf")

## Merge the data files.
Final_coffee_ratting <- merge(world, Avr_rating, by= "adm0_a3")

## Mapping the data using geom_sf by species.
ggplot(data = Final_coffee_ratting) +
  borders() +
  geom_sf(aes(fill = mean_country_rating, geometry = geometry)) +
  scale_fill_viridis_c(trans = "sqrt") +
  theme(legend.position = "bottom") +
  coord_sf(ylim = c(-50, 80)) +
  facet_wrap(~ species, nrow = 2) +
  labs(title = "Average Coffee Rating:",
       subtitle = "By country and species",
       fill = "Average Rating / 100",
       caption = "Data: tidytuesday - https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-07/readme.md",
       x = "",
       y = "")

################################################################
## 2)
## Create the company cups columns (average of total cups) by species and country code.
company_cups <- coffee_ratings[, .(.N,total_cups = round(mean(total_cup_points, na.rm = TRUE), digits = 2)),
                               by =.(species, country_code)]

## The information that included in tooltip.
text <- paste("Country:", company_cups$country_code,
              "<br>Total cups: ", company_cups$total_cups)

## Crate point interactive graph using girafe.
company_plot <- ggplot(company_cups, aes(total_cups,
                                         N,
                                         color = species,
                                         tooltip = text )) +
  geom_point_interactive() +
  scale_color_brewer(type = 'qual', palette = 'Dark2', direction = 10) +
  theme_gdocs() +
  scale_fill_gdocs() +
  labs(title = "Average Coffee Rating:",
       subtitle = "By country and species",
       fill = "Average Rating / 100",
       caption = "Please: Select a point for more details",
       x = "",
       y = "")

girafe(ggobj = company_plot)
################################################################################
##3)
## Cleaning the expiration column to just return the year's expiration.
experation_year <- coffee_ratings[,.(.N, species = species, year = gsub(".*,","",coffee_ratings$expiration))]
## Counting the expiration year for each year.
experation_year<- experation_year %>% select(species, year) %>% count(year, species)

exp_plot <- ggplot(data = experation_year, aes( x = species, y = n , fill= year)) +
  geom_col() +
  theme_stata() + scale_fill_stata() +
  theme( legend.position = "none") +
  transition_time(as.integer(year)) +
  labs(title =  "Average Coffee Rating in year( {frame_time} )",
       subtitle = "By species",
       caption = "Data: tidytuesday - https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-07/readme.md",
       x = "",
       y = "")
  

animate(exp_plot, renderer = gifski_renderer())


##############################################################

## Create the year column from grading date
coffee_ratings$year = substring(gsub(".*,","",coffee_ratings$grading_date),2,5)

## 
producer_rank <- coffee_ratings[!is.na(producer), .(.N,
                                    avg_aroma =  round(mean(aroma),digits = 2),
                                    avg_flavor = round(mean(flavor),digits = 2),
                                    avg_aftertaste = round(mean(aftertaste),digits = 2),
                                    avg_acidity = round(mean(acidity),digits = 2),
                                    avg_body = round(mean(body),digits = 2),
                                    avg_balance =  round(mean(balance),digits = 2),
                                    avg_uniformity = round(mean(uniformity),digits = 2),
                                    avg_clean_cup = round(mean(clean_cup),digits = 2),
                                    avg_sweetness = round(mean(sweetness),digits = 2),
                                    avg_cupper_points = round(mean(cupper_points),digits = 2)),
                                by = .(year, species)]

ggplot(producer_rank, aes(avg_aroma, avg_flavor, color = species )) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(size = 6) +
  theme_bw() +
  theme(legend.position = "top")+
  transition_time(as.integer(year)) +
  shadow_wake(wake_length = .1, alpha = FALSE) +
  labs(title = "year: {frame_time}")


########################################################################
## 4) 

## The change of bags tested over year in different bean color.
## The colors of coffee with the count of bags numbers by color & year.
coffee_color <- coffee_ratings[!color == "None"][!is.na(color), .(number = .N, Number_of_bags = sum(number_of_bags)),
                                                 by = .(Color_of_bean = color, year)]
## The change of the bags numbers accourding to the years 
Color <- ggplot(coffee_color, aes(x = year, y = Number_of_bags, color = Color_of_bean)) +
  stat_summary(fun = sum,geom='line',aes(group = Color_of_bean,
                                         color = Color_of_bean), size = 1.3) +
  theme_base() +
  theme(legend.position = "bottom") + 
  xlab("Year") + ylab("Number of bags tested") +
  labs(subtitle = "Number of bags tested over year with different bean color") +
  transition_reveal(as.integer(year))
  
## Plot the graph in animation mode.
animate(Color, renderer = gifski_renderer())
################################################################################
## 5)
## Creating a category table counts the average of one & two category defects by the year.
categorys <- coffee_ratings[,.(.N, category_one_defects = round(mean(category_one_defects),digits = 2),
                               category_two_defects = round(mean(category_two_defects),digits = 2)), by = year]

## Create two lines for each category defect on one plot which shows the different between them.
category_plot <- ggplot( categorys, aes(x=year,  group = 1, tooltip = "text")) + 
  geom_line(aes(y=as.character(category_two_defects), col="category_two_defects"),size = 1.5) + 
  geom_line(aes(y=as.character(category_one_defects), col="category_one_defects"),size = 1.5) + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_brewer(type = 'seq', palette = 'Reds', direction = 10) +
  labs(title="Category Defects", 
       y="Category Defects",
       x="Year")
girafe(ggobj = category_plot)



################################################################################