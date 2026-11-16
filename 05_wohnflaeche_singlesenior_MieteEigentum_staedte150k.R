library(tidyverse)
library(ggtext)
library(janitor)

destatis_blau <- c("#bfe1ff","#9dceff","#76b4ff","#5294e6","#266ec3","#00439a","#00175a")
destatis_rot  <- c("#ffcace","#ffacb3","#ff8290","#e75e70","#c0304c","#8f0027","#47000c")
destatis_grau <- c("#dedede","#cacaca","#b1b1b1","#929292","#6f6f6f","#484848","#1f1f1f")

format_string <- function(x) {
  x <- sub(" m²","",x)
  # An Leerzeichen umbrechen (Leerzeichen -> Zeilenumbruch)
  x <- sub(" - ", "\n–\n", x)
  x <- sub("Unter 30","Unter\n30\u2009qm",x)
  x <- sub(" und mehr","+\nqm",x)
  return(x)
}

wohnfl_sort <- c(
  "Unter 30 m²",
  "30 - 39 m²",
  "40 - 49 m²",
  "50 - 59 m²",
  "60 - 69 m²",
  "70 - 79 m²",
  "80 - 89 m²",
  "90 - 99 m²",
  "100 - 109 m²",
  "110 - 119 m²",
  "120 - 129 m²",
  "130 - 139 m²",  
  "140 - 149 m²",
  "150 - 159 m²",
  "160 - 169 m²",
  "170 - 179 m²",
  "180 m² und mehr",
  "Insgesamt"
)

preferredLabels <- c(
  "Unter 30 m²",
  "180 m² und mehr",
  "50 - 59 m²",
  "120 - 129 m²"
)

wohnfl <- readRDS("daten/1000A-3096_staedte50k.rds") |> 
  clean_names() |> 
  filter(x2_variable_attribute_code %in% c("NUTZ-MIETE","NUTZ-EIGEN"),
         x3_variable_attribute_label != "Insgesamt",
         x4_variable_attribute_code == "HSH-SENIOR-02",
         )

bev <- readRDS("daten/1000A-0001_staedte50k.rds") |> 
  filter(value > 15e4) |> 
  clean_names() |> 
  select(x1_variable_attribute_code, value) |> 
  rename(ewz_2022 = value)

wohnfl <- bev |> 
  left_join(wohnfl)


wohnfl$x3_variable_attribute_label <- factor(wohnfl$x3_variable_attribute_label, 
                                             levels = wohnfl_sort)

p <- wohnfl |> 
  # Sortierung nach Einwohnerzahl
  group_by(x1_variable_attribute_code) |> 
  mutate(sort_value = ewz_2022) |> 
  ungroup() |> 
  mutate(x1_variable_attribute_label = fct_reorder(x1_variable_attribute_label, sort_value, .desc = TRUE)) |> 
  
  ggplot(aes(x= x3_variable_attribute_label, y=value/1000, fill = x2_variable_attribute_label)) +

    geom_col(position = position_dodge(width=.9), width=.85) +
    scale_fill_manual(values = c("Von Eigentümer/-in bewohnt" = destatis_blau[4], 
                                 "Zu Wohnzwecken vermietet (auch mietfrei)" = destatis_rot[4]),
                      labels = c(
                        "Zu Wohnzwecken vermietet (auch mietfrei)" = "Zu Wohnzwecken vermietet\n(auch mietfrei)",
                        "Von Eigentümer/-in bewohnt" = "Von Eigentümer/-in\nbewohnt"
                      )) +
    facet_wrap(~ x1_variable_attribute_label, scales = "free_y",
               labeller = labeller(
                 x1_variable_attribute_label = \(x) sub(",.*", "", x))) +
  
    labs(title = "Wohnfläche alleinlebender Senioren 2022 – Miete vs. Eigentum",
         x=NULL, y=NULL,
         fill = "Städte ab 150.000 Ew",
         caption = "Zensusdatenbank Tabelle 1000A-3096") +
    
    theme_minimal(base_size=18) +
    theme(plot.title = element_text(size = 18),
          text = element_text(size = 9),
          legend.position = c(1.015, -0.02),
          legend.justification = c(1, 0),
          legend.key.spacing.y = unit(.5, "mm"),
          legend.key.width = unit(3, "mm"),
          legend.text = element_text(margin = margin(l = 2)),
          legend.title = element_markdown(size=9, margin = margin(0,0,3,0)),
          strip.text = element_text(face = "plain", hjust=0, size=10,
                                    margin = margin(0,0,3,0)),
          panel.spacing.x = unit(6, "mm"),
          plot.caption = element_markdown(hjust = 1, size = 9,
                                              margin = margin(12,0,0,0)),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()
         )

ggsave("img/05_wohnflaeche_singlesenior_MieteEigentum_staedte150k.png", 
        plot = p, height=2160, width=3840, units = "px")


