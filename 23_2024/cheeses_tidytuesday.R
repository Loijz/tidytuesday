cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')

library(tidyverse)
library(dplyr)
library(ggplot2)

switzerland <- cheeses %>%
  filter(country == "Switzerland")

print(switzerland)

ggplot(cheeses, aes(milk)) +
  geom_bar()

cheeses %>%
  filter(milk == "moose")

cheeses

summary(cheeses$country)
unique(cheeses$country)

cheeses <- cheeses %>%
  mutate(country = case_when(
    str_detect(country, "England") ~ "UK",
    str_detect(country, "Ireland") ~ "UK",
    str_detect(country, "Scotland") ~ "UK",
    str_detect(country, "United Kingdom") ~ "UK",
    str_detect(country, "Wales") ~ "UK",
    str_detect(country, "Great Britain") ~ "UK",
    TRUE ~ country  # Keep all other values unchanged
  ))

# Define the list of European countries
european_countries <- c("Austria", "Germany", "Belgium", "Bulgaria", "Denmark", "France", "Netherlands", 
                        "Switzerland", "Italy", "Croatia", "Cyprus", "Czech Republic", "Finland", 
                        "Iceland", "Norway", "Sweden", "UK", "Greece", 
                        "Holland", "Hungary", "Lithuania", "Poland", "Portugal", "Romania", 
                        "Serbia", "Spain", "Sweden", "Turkey")

# Filter the dataset for European countries
europe <- cheeses %>%
  filter(country %in% european_countries)

# View the result
print(europe)

unique(europe$country)

unique_europe <- europe %>%
  count(country)

print(unique_europe)

# Plotting with geom_bar()
ggplot(unique_europe, aes(x = country, y = n, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Cheeses by Country",
       x = "Country",
       y = "Number of Cheeses") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text for better readability



mapdata <- map_data("world")
view(mapdata)

unique_europe <- unique_europe %>%
  rename(region = country)

macedonia <- mapdata %>%
  filter(str_detect(region, regex("macedonia", ignore_case = TRUE)))

# Print the filtered data
print(macedonia)

empty_countries <- tibble(
  region = c("Albania", "Hungary", "Norway", "Bosnia and Herzegovina", "Belarus", "Latvia", "Estonia", "Ukraine", "Moldova",
             "Ireland", "Kosovo", "Liechtenstein", "Luxembourg", "Montenegro", "North Macedonia", "Slovenia", "Slovakia"),
  n = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

unique_europe <- bind_rows(unique_europe, empty_countries)


mapdata1 <- left_join(mapdata, unique_europe, by="region")

mapdata1


mapdata1 <- mapdata1 %>%
  rename(total_count = n)

head(mapdata1)

mapdata2 <- mapdata1 %>%
  filter(!is.na(mapdata1$total_count))
view(mapdata2)

map1 <- ggplot(mapdata2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = total_count), color = "black", size = 0.5)

map1

map2 <- map1 +
  scale_fill_gradientn(
    name = "Amount of\ndifferent cheeses", 
    colours = c("grey", "red", "orange", "yellow"),  # Custom colors
    values = scales::rescale(c(0, 50, 100, 150, 200)),  # Define the breaks
    na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank(),
        plot.background = element_rect(fill = "lightblue", color = "lightblue"),  # Set plot background color
        panel.background = element_rect(fill = "lightblue", color = "lightblue"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Bold and center title
        legend.position = "left",  # Default legend position
        legend.justification = "center",
        legend.key.size = unit(0.5, "lines"),
        legend.box.background = element_rect(fill = "white", color = "black"),  # White rectangle for legend background
        legend.text = element_text(size = 8),  # Smaller font size for legend text# Move legend to the left
        legend.box.margin = margin(0, 10, 10, 0),
        legend.margin = margin(0, 0, 0, 10),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 40, 20)) +  # Adjust legend margin
  labs(title = "Which European countries does \n cheese.com sell its cheese from?") +
  annotate("text", x = -Inf, y = -Inf, label = "Graphic: Jarkko Schaad | Source: cheeses.com (tidytuesday)", 
           hjust = -0.5, vjust = -0.5, size = 4)

map2

ggsave("cheese_map.jpg", plot = map2, width = 7, height = 6, dpi = 300, device = "jpg")
