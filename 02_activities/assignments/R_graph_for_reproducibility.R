library (ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv('/Users/levi/Documents/Documents/Graduate School Stuff/DSI certification class/DSI coding/visualization/02_activities/assignments/1314-006_violent_crime_rates_csv_2008-12.csv')
df2 <- df %>% filter(X.1 == "Total assaults against a peace officer (25,35)")


df_long <- df2 %>%
  rename(
    `2008` = Violent.Crime.Rates..per.100.000.population.,
    `2009` = X.2,
    `2010` = X.3,
    `2011` = X.4,
    `2012` = X.5
  ) %>%
  pivot_longer(
    cols = c(`2008`, `2009`, `2010`, `2011`, `2012`),
    names_to = "Year",
    values_to = "Rate"
  )


df_long_collapsed <- bind_rows(
  df_long %>%
    filter(grepl("Ontario", X)) %>%
    group_by(Year) %>%
    summarise(Rate = sum(Rate, na.rm = TRUE),
              X.1 = first(X.1),
              .groups = "drop") %>%
    mutate(X = "Ontario"),
  df_long %>%
    filter(!grepl("Ontario", X))
)

df_long_collapsed <- df_long_collapsed %>%
  mutate(X = trimws(gsub("\\s*\\([0-9,]+\\)$", "", X)))

ggplot(df_long_collapsed, aes(x = X, y = Rate, color = Year)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), size = 2) +
  scale_color_viridis_d() +
  labs(title = "Assaults on a Peace Officer By Province, 2008-2012", 
       x = "Province", y = "Violent Crime Rate (per 100,000)", 
       color = "Year", 
       caption = "Nunavut is the most dangerous province to be a police officer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 14, color = "blue"),
        axis.title.y = element_text(size = 14, color = "red"),
        plot.title = element_text(size = 20, color = 'blue', hjust = 0.5),
        plot.caption = element_text(size = 16, color = 'red', hjust = 0.5))


ggsave("/Users/levi/Documents/Documents/Graduate School Stuff/DSI certification class/DSI coding/visualization/02_activities/assignments/violent_crime_by_province.png",
       width = 10, height = 6, dpi = 300)
