library(tidyverse)
library(ggtext)
library(janitor)
library(marimekko)

df <- readRDS("daten/4000W-2008_staedte500k.rds") |> 
  clean_names() |> 
  rename(Baujahr = x3_variable_attribute_label,
         Wohnfläche = x2_variable_attribute_label) |> 
  filter(
    Baujahr != "Insgesamt",
    Wohnfläche != "Insgesamt"
  )

bev <- readRDS("daten/1000A-0001_staedte50k.rds") |> 
        filter(value > 5e5) |> 
        clean_names() |> 
        select(x1_variable_attribute_code, value) |> 
        rename(ewz_2022 = value)

df <- df |> 
      left_join(bev)

sort_wohnfl <- c(
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
  "180 m² und mehr"
)

destatis_blau <- c("#bfe1ff","#9dceff","#76b4ff","#5294e6","#266ec3","#00439a","#00175a")
destatis_rot  <- c("#ffcace","#ffacb3","#ff8290","#e75e70","#c0304c","#8f0027","#47000c")
destatis_grau <- c("#dedede","#cacaca","#b1b1b1","#929292","#6f6f6f","#484848","#1f1f1f")

sort_bauj <- c(
  "Vor 1919" = destatis_grau[4],
  "1919 - 1948" = destatis_grau[2],
  "1949 - 1978" = destatis_rot[3],
  "1979 - 1990" = destatis_rot[5],
  "1991 - 2000" = destatis_blau[1],
  "2001 - 2010" = destatis_blau[3],
  "2011 - 2019" = destatis_blau[5],
  "2020 und später" = destatis_blau[7]
)

format_string <- function(x) {
  x <- sub(" m²","",x)
  # An Leerzeichen umbrechen (Leerzeichen -> Zeilenumbruch)
  x <- sub(" - ", "\n–\n", x)
  x <- sub("Unter 30","Unter\n30qm",x)
  x <- sub(" und mehr","+\nqm",x)
  #return(ifelse(x %in% my_labels, x, ""))€
  return(x)
}

format_vonbis <- function(x) {
  # An Leerzeichen umbrechen (Leerzeichen -> Zeilenumbruch)
  x <- sub(" - ", "\n–\n", x)
  return(x)
}

df$Wohnfläche <- format_string(df$Wohnfläche)
df$Wohnfläche <- factor(df$Wohnfläche, levels = format_string(sort_wohnfl))
df$Baujahr <- factor(df$Baujahr, levels = names(sort_bauj))
df$Baujahr <- fct_recode(df$Baujahr,
                          "2020 ..." = "2020 und später")

sort_bauj <- c(
  "Vor 1919" = destatis_grau[4],
  "1919 - 1948" = destatis_grau[2],
  "1949 - 1978" = destatis_rot[3],
  "1979 - 1990" = destatis_rot[4],
  "1991 - 2000" = destatis_blau[1],
  "2001 - 2010" = destatis_blau[3],
  "2011 - 2019" = destatis_blau[5],
  "2020 ..." = destatis_blau[7]
)

df |> 
  group_by(x1_variable_attribute_code) |> 
  mutate(sort_value = ewz_2022) |> 
  ungroup() |> 
  mutate(x1_variable_attribute_label = fct_reorder(x1_variable_attribute_label, sort_value, .desc = TRUE)) |> 
  ggplot() +
  geom_marimekko(aes(weight = value), gap = 0.002, alpha = 1,
                 formula = ~ Wohnfläche|Baujahr) +
  facet_wrap(~ x1_variable_attribute_label,
             labeller = labeller(x1_variable_attribute_label = \(x) sub(",.*$", "", x))) +
  scale_fill_manual(values = sort_bauj) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Wohnungen 2022 nach Wohnfläche und Baujahr",
       #subtitle = "nach Wohnfläche und Baujahr",
       caption = "Zensusdatenbank<br>Tabelle 4000W-2008",
       x = NULL, y = NULL) +
  theme_minimal(base_size=18) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "plain", 
                                  margin = margin(3,0,6,0),
                                  hjust=0, size=12),
        axis.text = element_text(size = 11),
        plot.caption = element_markdown(hjust = 1.15, size = 9),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
  )

ggsave("img/02_marimekko_flaeche_baujahr_staedte500k.png", 
       height=2160, width=3840, units = "px")

