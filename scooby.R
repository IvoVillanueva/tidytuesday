# load libraries---------------


library(tidyverse)
library(gt)
library(ggtext)
library(extrafont)


# Load data ---------------------------------------------------------------


scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

scoobydoo <- scoobydoo %>% select(matches("caught|unmask|captured"),-unmask_other, -caught_other, -caught_not) %>% mutate_if(is.character, as.factor)

scoobydoo <- scoobydoo %>% pivot_longer(everything(),names_to ="actor", values_to  = "count") %>%
            filter(count== TRUE) %>%
            group_by(actor) %>%
            count(actor) %>%
            separate(actor, c("accion", "name"), sep= "_")  %>%
            pivot_wider(names_from = accion, values_from = n)


# data table with images --------------------------------------------------



tribble(~name1, ~name,  ~photo,
       "Daphne Blake", "daphnie", "https://i.pinimg.com/originals/a1/78/f4/a178f42cfb9d18833f53a50872054a9b.png",
        "Shaggy Rogers", "shaggy", "https://i.pinimg.com/originals/75/ea/70/75ea70d79bd65cda73b77d01b7810c3c.png",
        "Scooby-Doo", "scooby", "https://i.pinimg.com/originals/15/97/75/159775852fef90b1d7fea7d1676bf7fe.png",
        "Velma Dinkley", "velma", "https://i.pinimg.com/originals/43/3c/21/433c21edc87a9a103f32913313c9648d.png",
        "Fred Jones","fred", "https://i.pinimg.com/originals/f0/9d/b4/f09db4286957cb30400175e48b55e147.png") -> s


# Joint tables ------------------------------------------------------------




df <- left_join(scoobydoo,s)

# Code for Name1/name------


combine_word <- function(name1, name){
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:12px'>{name1}</span></div>
        <div style='font-size:12px;line-height:18px;vertical-align:middle;'><span style='width:20px; height:20px;vertical-align:middle'>{name}</span></div>"
  )
}


# scobby doo data table ---------------------------------------------------




df %>%
  mutate(capavg= captured/sum(captured),
         cauavg = caught/sum(caught),
         unmavg = unmask/sum(unmask),
         combo = combine_word(name1, name),
         combo = map(combo, gt::html)) %>%
  select(photo, combo, captured, capavg, caught, cauavg, unmask, unmavg) %>%
  gt()  %>%
  cols_label(
    photo = "",
    combo = gt::html("<span style='font-weight:bold;font-size:12px'>NAME</span>"),
    captured = gt::html("<span style='font-weight:bold;font-size:12px'>TIMES</span>"),
    capavg = gt::html("<span style='font-weight:bold;font-size:12px'>AVG</span>"),
    caught = gt::html("<span style='font-weight:bold;font-size:12px'>TIMES</span>"),
    cauavg = gt::html("<span style='font-weight:bold;font-size:12px'>AVG</span>"),
    unmask = gt::html("<span style='font-weight:bold;font-size:12px'>TIMES</span>"),
    unmavg = gt::html("<span style='font-weight:bold;font-size:12px'>AVG</span>")

  ) %>%
  tab_spanner(
    label =  md("CAPTURED"),
    columns = c(captured,
                   capavg)
  )  %>%
  tab_spanner(
    label =  md("CAUGHT"),
    columns = c(caught,
                   cauavg)
  )  %>%
  tab_spanner(
    label =  md("UNMASK"),
    columns = c(unmask,
                   unmavg)
  )  %>%

  tab_header(
    title = md("<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Scooby_doo_logo.png/200px-Scooby_doo_logo.png' style='height:50px;'><br>ADVENTURE STATS"),
    subtitle = md(paste0("<img src='https://www.jing.fm/clipimg/full/92-921612_scooby-doo-scooby-doo-gang-in-van.png' style='height:50px;'><br> to ",  format(Sys.Date(), format= "%d %B, %Y")))

  ) %>%
  text_transform(
    locations = cells_body(c(photo)),
    fn = function(x) {
      web_image(url = x,
                height = px(26.5))
    }
  ) %>%
  fmt_percent(
    columns = c(capavg, cauavg, unmavg),
    decimals = 1
  )  %>%


  cols_align(
    align = "center",
    columns = c(captured:unmavg)
  ) %>%
  data_color(
    columns = c(capavg, cauavg, unmavg),
    colors = scales::col_numeric(
      palette = c("#F7921E","#A091C6", "#D0D61B","#6352A3"),
      domain = NULL
    ) )%>%


  opt_row_striping() %>%


  tab_options(
    table.background.color = "#f4f4f4",
    column_labels.font.size =12,
    table.font.size =11,
    heading.title.font.size  = 22.652,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 12,
    table.font.names = "Scooby Doo",
    table.font.color = 'black',
    table.border.top.color = "transparent",
    data_row.padding = px(1),
    footnotes.font.size = 10,
    source_notes.font.size = 10,
    footnotes.padding = px(1)
  ) %>%
  tab_source_note(
    source_note = md( "<div><b>Grafica por</b> : <i>\n Ivo Villanueva<i>
                       <div><b>Datos por</b> : \n<i>Scoobypedia y tidytuesdayR<i>")) %>%
  gtsave("scooby.html")
