
library(tidyverse)
library(dplyr)
library(mice)
library(grid)


harvest_2020 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2020.csv')
harvest_2021 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2021.csv')
planting_2020 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/planting_2020.csv')
planting_2021 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/planting_2021.csv')
spending_2020 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/spending_2020.csv')
spending_2021 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/spending_2021.csv')


#deleting dates and brandnames and merge together to one dataset
str(harvest_2020)

harvest20 <- harvest_2020 %>%
  select(-date)

harvest21 <- harvest_2021 %>%
  select(-date)

harvest21 <- harvest21 %>%
  mutate(vegetable = as.factor(vegetable))

harvest_summarized_21 <- harvest21 %>%
  group_by(vegetable) %>%
  summarise(total_weight = sum(weight, na.rm = TRUE))

harvest20 <- harvest20 %>%
  mutate(vegetable = as.factor(vegetable))

harvest_summarized_20 <- harvest20 %>%
  group_by(vegetable) %>%
  summarise(total_weight = sum(weight, na.rm = TRUE))

# Rename the weight columns in each dataset
harvest_summarized_20 <- harvest_summarized_20 %>% rename(weight20 = total_weight)
harvest_summarized_21 <- harvest_summarized_21 %>% rename(weight21 = total_weight)

# Merge the datasets using full_join
harvest_combined <- full_join(harvest_summarized_20, harvest_summarized_21, by = "vegetable")

# View the merged dataset
print(harvest_combined)

harvest_combined <- harvest_combined %>%
  mutate(weight_difference = weight21 - weight20)
print(harvest_combined)

md.pattern(harvest_combined)
sapply(harvest_combined, function(x) sum(is.na(x)))

harvest_cleaned <- na.omit(harvest_combined)

print(harvest_cleaned)
print(harvest_cleaned, n = 22)

spending20 <- spending_2020 %>%
  select(-variety, -eggplant_item_number, -brand, -price_with_tax)

spending20 <- spending_2020 %>%
  mutate(vegetable = as.factor(vegetable))

spending20 <- spending20 %>%
  group_by(vegetable) %>%
  summarise(total_price = sum(price, na.rm = TRUE))

spending21 <- spending_2021 %>%
  select(-variety, -brand, -price_with_tax)

spending21 <- spending21 %>%
  mutate(vegetable = as.factor(vegetable))

spending21 <- spending21 %>%
  group_by(vegetable) %>%
  summarise(total_price = sum(price, na.rm = TRUE))

spending_cleaned <- full_join(spending20, spending21, by = "vegetable")

md.pattern(spending_cleaned)

spending_cleaned$costs_20 <- spending_cleaned$total_price.x
spending_cleaned$costs_21 <- spending_cleaned$total_price.y

md.pattern(spending_cleaned)

spending_cleaned <- spending_cleaned %>%
  select(-total_price.x, -total_price.y)

md.pattern(spending_cleaned)

garden <- full_join(harvest_cleaned, spending_cleaned, by = "vegetable")

garden_output <- na.omit(garden)

print(garden, n = 44)

#calculating differences between years in relation to money spent
garden_kpi <- garden_output %>%
  mutate(outputoncosts20 = weight20 %/% costs_20) %>%
  mutate(outputoncosts21 = weight21 %/% costs_21) %>%
  mutate(change = as.numeric(outputoncosts21 %/% outputoncosts20))

garden_kpi2 <- mutate(garden_output, change2 = round(outputoncosts20 / outputoncosts21, 4))


garden_kpi2 <- garden_output %>%
  mutate(outputoncosts20 = weight20 %/% costs_20) %>%
  mutate(outputoncosts21 = weight21 %/% costs_21) %>%
  mutate(garden_output, change2 = round(outputoncosts21 / outputoncosts20, 4))

garden_kpi2

garden_kpi2 <- garden_kpi2 %>%
  arrange(desc(change2))

garden_kpi2

garden_kpi3 <- garden_kpi2 %>%
  mutate(percent_column = ifelse(change2 < 1, paste0((change2 - 1) * 100), paste0((change2 - 1) * 100)))

garden_kpi3

garden_kpi3$percent_column <- as.double(garden_kpi3$percent_column)

garden_kpi3



garden_kpi3$vegetable <- factor(garden_kpi3$vegetable, levels = garden_kpi3$vegetable[order(garden_kpi3$percent_column)])




ggplot(garden_kpi3, aes(x = vegetable, y = percent_column, fill = factor(sign(percent_column)))) +
  geom_bar(stat = "identity", position = "identity", color = "#8AACA6") +
  scale_fill_manual(values = c("#963A3A", "#6DA564"), guide = FALSE) +
  theme_minimal() +
  labs(
    x = "Vegetable",
    y = "Change from previous year, relative to spent money in %",
    title = "Income from expenses",
    subtitle = "Change in revenue from 2020 to 2021 per $ spent"
  ) +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate("text", x = Inf, y = -Inf, label = "Graphic: Jarkko Schaad | Source: Lisa's Vegetable Garden Data", 
           hjust = 1.1, vjust = -1.5, size = 3)



