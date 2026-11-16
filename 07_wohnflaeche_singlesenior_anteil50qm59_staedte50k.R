library(tidyverse)
library(ggtext)
library(janitor)

destatis_blau <- c("#bfe1ff","#9dceff","#76b4ff","#5294e6","#266ec3","#00439a","#00175a")
destatis_rot  <- c("#ffcace","#ffacb3","#ff8290","#e75e70","#c0304c","#8f0027","#47000c")
destatis_grau <- c("#dedede","#cacaca","#b1b1b1","#929292","#6f6f6f","#484848","#1f1f1f")

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

format_string <- function(x) {
  x <- sub(" m²","",x)
  # An Leerzeichen umbrechen
  x <- sub(" - ", "\n–\n", x)
  x <- sub("Unter 30","Unter\n30\u2009qm",x)
  x <- sub(" und mehr","+\nqm",x)
  return(x)
}

wohnfl <- readRDS("daten/1000A-3096_staedte50k.rds") |> 
  clean_names() |> 
  filter(x4_variable_attribute_code == "HSH-SENIOR-02",
         x2_variable_attribute_label == "Insgesamt") 

# Ost-West Einfärbung
wohnfl <- wohnfl |> 
  mutate(
    landInt=as.integer(substr(x1_variable_attribute_code, 1, 2))
  )

bev <- readRDS("daten/1000A-0001_staedte50k.rds") |> 
  clean_names() |> 
  select(x1_variable_attribute_code, value) |> 
  rename(ewz_2022 = value)

wohnfl <- bev |> 
  left_join(wohnfl)

wohnfl$x3_variable_attribute_label <- factor(wohnfl$x3_variable_attribute_label, 
                                             levels = wohnfl_sort)

wohnfl |> 
  # Prozente
  group_by(x1_variable_attribute_code) |> 
    mutate(
      total = value[x3_variable_attribute_label == "Insgesamt"],
      pct = 100 * value / total,
      pct_klein = 100 * value[x3_variable_attribute_label == "50 - 59 m²"] / total
    ) |> 
  ungroup() |> 
  filter(x3_variable_attribute_label !="Insgesamt",
         pct_klein > 25) |> 
  # facet sorting
  mutate(
    shortName = paste0(sub(",.*", "", x1_variable_attribute_label),"\n", round(ewz_2022/1000,0)," 000 Ew"),
    shortName = fct_reorder(shortName, pct_klein, .desc = TRUE)
  ) |> 
  
  ggplot(aes(x= x3_variable_attribute_label, y=pct, fill=landInt<11)) +
  facet_wrap(~ shortName) +
  geom_col(position = position_dodge(width=.9), width=.85) +
  scale_x_discrete(breaks = wohnfl_sort[c(1,17)],
                   labels = \(x) format_string(x)) +
  scale_fill_manual(values = c("TRUE" = destatis_blau[4], 
                               "FALSE" = destatis_rot[4]),
                    guide = NULL) +
  
  labs(title = "Wohnfläche alleinlebender Senioren 2022 (Städte ab 50 Tsd Ew)",
       subtitle = "in Prozent, sortiert nach Anteil 50–59qm, Anteil >25%", x=NULL, y=NULL,
       caption = "Zensusdatenbank Tabelle 1000A-3096"
       ) +
  
  theme_minimal(base_size=16) +
  theme(axis.text = element_text(size = 10),
        plot.subtitle = element_markdown(margin = margin(0,0,5,0)),
        plot.caption = element_markdown(hjust = 1, size = 9,
                                        margin = margin(0,0,0,0)),
        strip.text = element_text(face = "plain", 
                                  margin = margin(6,0,3,0),
                                  hjust=0, size=12),
        panel.spacing.x = unit(8, "mm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
  )

ggsave("img/07_wohnflaeche_singlesenior_anteil50qm59_staedte50k.png", 
       height=2160, width=3840, units = "px")


