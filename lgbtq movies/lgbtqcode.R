


library(tidyverse)            # All things tidy

# Final plot tools
library(scales)
library(fontawesome)
library(ggtext)
library(showtext)
library(colorspace)
library(patchwork)
#install.packages("ggstream")
library(ggstream)
library(patchwork)
library(magick)

lgbtq_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-25/lgbtq_movies.csv')

#write_csv(lgbtq_movies, "lgbtq_movies.csv")


number_to_display = 11

#table
lgbtq_movies |> 
 summarytools::dfSummary() |> 
 summarytools::view()

#new table df1 with columns i need for the visualization
df1 <- lgbtq_movies |> 
  mutate(
    year = year(release_date),
    month = month(release_date),
    day = wday(release_date, label = TRUE),
    language = original_language,
    .keep = "none"
  )

#summarise years after 1990 by language
df2 <- df1 |> 
  filter(year >= 1990) |> 
  count(year, language) 

#filtering the most used languages
languages <- df2 |> 
  group_by(language) |> 
  summarise(n = sum(n)) |> 
  slice_max(order_by = n, n = number_to_display) |> 
  pull(language)

full_names = c("English", "Spanish", "French", "German", 
               "Portuguese", "Japanese", "Italian", "Korean", 
               "Chinese", "Tagalog", "Swedish", "Others")

#summarise all other languages to "others"
lan_df <- tibble(
  language = c(languages, "others"),
  full_name = full_names
)

#creating dataset for visualisation
plotdf1 <- df2 |> 
  mutate(
    language = if_else(
      language %in% languages,
      language,
      "others"
    )
  ) |> 
  group_by(year, language) |> 
  summarise(n = sum(n)) |> 
  group_by(year) |> 
  mutate(
    lang_prop = round(100*n/sum(n), 1),
    lang_prop = if_else(lang_prop > 5, lang_prop, NA)
  ) |> 
  left_join(lan_df) |> 
  mutate(
    full_name = fct(full_name, levels = full_names)
  )

#grouping by year
plotdf2 <- plotdf1 |> 
  group_by(year) |> 
  summarise(total = sum(n))


# style aestetics
# Font for titles
font_add_google("Gelasio",
                family = "title_font"
) 

# Font for the caption
font_add_google("Gruppo",
                family = "caption_font"
) 

# Font for plot text
font_add_google("Gruppo",
                family = "body_font"
) 

showtext_auto(FALSE)

bg_col <- "white"
# Credits for coffeee palette
##library(paletteer)

mypal <- paletteer::paletteer_d("PrettyCols::Rainbow")

text_col <-  "#7D1717"
text_hil <-  "#F77F00"

bts = 50

#titles
plot_supertitle <- "LGBTQ+ Movies on the rise"

plot_title <- "LGBTQ+ Movies by Language (1990-2022)"

plot_caption <- paste0(
  "tidytuesday project - tidyrainbow", 
  " | Jarkko Schaad", 
  " | GitHub: Loijz")


plot_subtitle <- str_wrap("The number of LGBTQ+ movies have rised steadily till the Corona-Drop", 80) #the number after comma indicates how many characters will be on the same line


#plot
g <- plotdf1 |> 
  ggplot(
    mapping = aes(
      x = year, 
      y = n
    )
  ) + 
  geom_col(
    mapping = aes(fill = full_name),
    colour = "#EAE2B7",
    linewidth = 0.5
  ) + #text for columns
  geom_text(
    data = plotdf2,
    mapping = aes(
      y = total,
      label = total
    ), 
    colour = text_col,
    nudge_y = 5,
    family = "body_font",
    size = bts / 10
  ) +  #title
  annotate(
    geom = "text",
    label = plot_supertitle,
    x = 1990, y = 500,
    hjust = 0,
    vjust = 0,
    family = "title_font",
    size = 0.5 * bts,
    colour = text_hil
  ) + #subtitle
  annotate(
    geom = "text",
    label = plot_title,
    x = 1990, y = 470,
    hjust = 0,
    vjust = 0,
    family = "title_font",
    size = bts / 6,
    colour = text_hil
  ) + #text
  annotate(
    geom = "text",
    label = plot_subtitle,
    x = 1990, y = 460,
    hjust = 0,
    vjust = 1,
    family = "body_font",
    size = bts / 6,
    lineheight = 0.3,
    colour = text_col
  ) +
  scale_x_continuous(
    expand = expansion(0),
    breaks = seq(1990, 2022, 4)
  ) + 
  scale_y_continuous(
    expand = expansion(c(0, 0.05))
  ) + #colour fill
  scale_fill_manual(
    values = mypal
  ) + #legend
  guides(
    fill = guide_legend(
      title = "Languages",
      override.aes = list(
        pch = 20,
        size = 6
      )
    )
  ) +
  labs(
    caption = plot_caption
  ) +
  theme_classic(
    base_family = "body_font",
    base_size = bts * 0.5
  ) +
  theme(
    legend.position = c(0.001, 0.7),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    axis.line.y = element_blank(),
    axis.ticks.length.x = unit(0, "mm"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(colour = text_col, size = bts * 0.4),
    axis.line.x = element_line(linewidth = 0.5, linetype = 1, colour = text_col),
    axis.ticks.x = element_blank(),
    plot.caption = element_textbox(hjust = 0.5, family = "caption_font", colour = text_col),
    legend.title = element_text(colour = text_col, lineheight = 0.3, hjust = 0, margin = margin(0,0,5,0, "mm"), size = bts*0.6),
    legend.text = element_text(colour = text_col, margin = margin(0,0,0,1, "mm")),
    legend.background = element_rect(fill = "transparent", colour = "transparent")
    )



print(g)



ggsave(
  filename = "tidy_rainbow.jpg",
  plot = g,
  width = 400,    # Best Twitter Aspect Ratio = 4:5
  height = 500,   
  units = "mm",
  bg = bg_col
)







