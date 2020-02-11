
# packages

library(tidyverse)
library(skimr)

# read in the data

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

# explore the data

glimpse(hotels)

summary(hotels)

names(hotels)

head(hotels)

# missing values

sum(is.na(hotels))  # 4 missing values

hotels %>%
  purrr::map_df(~ sum(is.na(.)))  # children - 4 missing values

# descriptive statistics

skimmed_data <- skim_to_wide(hotels)

View(skimmed_data)

# data viz
# if the booking was canceled or not

# Proportions

percentData <- hotels %>% group_by(hotel, arrival_date_year) %>% count(is_canceled) %>%
  mutate(ratio=scales::percent(n/sum(n)))

# the plot

ggplot(percentData,aes(x=factor(hotel), y = n, fill=factor(is_canceled)))+
  geom_bar(position="fill", stat = "identity")+
  geom_text(data=percentData, aes(y = n, label= ratio), 
            position=position_fill(0.5), color = "black")+
  scale_fill_manual(values = c("gray", "red"))+
  scale_y_continuous(labels=scales::percent)+
  facet_grid( .~ arrival_date_year)+
  theme_classic() +
  labs(title = "Hotels Booking Cancellation Ratio per Year",
       subtitle = "~booking was canceled (red) or not (gray)~",
       caption = "Source: Antonio, Almeida, and Nunes, 2019\n Code by: @magwanjiru", 
       x = "Hotel", y = "Distribution") +
  theme(legend.position="none",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size=12, face = "italic", hjust = 0.5),
        plot.caption = element_text(size = 8, face = "italic", color = "blue"))




