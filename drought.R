


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(ggtext)
library(ggimage)


# Load theme Ivo ----------------------------------------------------------



theme_ivo <- function() {
  theme_minimal(base_size = 12, base_family = "Chivo") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#fffff9", color = "#fffff9"),
      plot.caption = element_markdown(size = 9.5, hjust = .5)
    )
}

asp_ratio <- 1.618


# Data Wrangler -----------------------------------------------------------



drought <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv")
states <- readr::read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>%
  add_row(State = "Puerto Rico", Abbreviation = "PR") %>%
  rename(state = State, state_abb = Abbreviation)
drought_lvl <- tibble(
  drought_lvl = c("None", "D0", "D1", "D2", "D3", "D4"),
  label = c(
    "None", "Abnormally Dry", "Moderate", "Severe",
    "Extreme", "Exceptional"
  )
)



df <- left_join(drought, states) %>%
  left_join(drought_lvl) %>%
  filter(area_pct != 0 & drought_lvl != "None") %>%
  mutate(year = year(valid_start)) %>%
  select(state, label, area_pct, year) %>%
  group_by(state, label, year) %>%
  distinct(area_pct)


# Graphic Plot ------------------------------------------------------------




df %>% ggplot(aes(x = year, y = area_pct, fill = label)) +
  scale_fill_manual(
    values = c("#a32325", "#dc5053", "#e57d76", "#e57d76", "#f0d19d"), " ",
    guide = guide_legend(override.aes = list(size = 3.5)),
    labels = c("Abnormally dry", "Moderate", "Severe", "Extreme", "Exceptional")
  ) +
  geom_quasirandom(size = .8, alpha = .65, width = .25, shape = 21) +
  facet_wrap(~ fct_reorder(state, -area_pct),
    nrow = 13
  ) +
  theme_ivo() +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = "%")) +
  labs(
    x = "",
    y = ""
  ) +
  labs(
    title = "<span style='color:#6D0000'>**U.S.**</span> <span style='color:#6D0000'>**Drought**</span>",
    subtitle = "<span style='color:#a32325'>**Percent of states in drought by category and year | 2000 - 2021**</span>",
    caption = "<br>**Datos**: *NY Times and CNN*  **Gráfico**: *Ivo Villanueva*"
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8.5),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, -10, -10, -75),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7),
    strip.text = element_text(face = "bold"),
    plot.title = element_markdown(size = 20, hjust = 0.5),
    plot.subtitle = element_markdown(face = "bold", size = 14, hjust = 0.5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = unit(c(.5, .5, 1, .5), "lines")
  )
ggsave("Drought.png", w = 9 * asp_ratio, h = 17, dpi = 300, type = "cairo-png")


# Tidytuesday US Droughts Week 30 -----------------------------------------------------
# Ivo Villanueva ----------------------------------------------------------
