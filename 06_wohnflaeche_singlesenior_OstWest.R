library(tidyverse)
library(ggtext)
library(janitor)

destatis_blau <- c("#bfe1ff","#9dceff","#76b4ff","#5294e6","#266ec3","#00439a","#00175a")
destatis_rot  <- c("#ffcace","#ffacb3","#ff8290","#e75e70","#c0304c","#8f0027","#47000c")
destatis_grau <- c("#dedede","#cacaca","#b1b1b1","#929292","#6f6f6f","#484848","#1f1f1f")

format_string <- function(x) {
  x <- sub(" m²","",x)
  # An Leerzeichen umbrechen
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
  "100 - 109 m²",
  "140 - 149 m²"
)

wohnfl <- readRDS("daten/1000A-3096_bl.rds") |> 
  clean_names() |> 
  filter(x2_variable_attribute_label == "Insgesamt",
         x4_variable_attribute_code == "HSH-SENIOR-02",
  )


# Ost-West filter
wohnfl <- wohnfl |> 
  mutate(
    landInt=as.integer(x1_variable_attribute_code)
  )

wohnfl$x3_variable_attribute_label <- factor(wohnfl$x3_variable_attribute_label, 
                                             levels = wohnfl_sort)

wohnfl |> 
  mutate(ostwest = ifelse(landInt<11,"West","Ost")) |> 
  group_by(ostwest, x3_variable_attribute_label) |> 
    summarise(
      s_value = sum(value),
    ) |> 
  ungroup() |> 
  group_by(ostwest) |> 
  mutate(total = s_value[x3_variable_attribute_label == "Insgesamt"],
         pct = 100 * s_value / total) |> 
  ungroup() |> 
  filter(x3_variable_attribute_label !="Insgesamt") |> 

  ggplot(aes(x= x3_variable_attribute_label, y=pct, fill = ostwest)) +

    geom_col(position = position_dodge(width=.9), width=.85) +
    scale_x_discrete(breaks = preferredLabels,
                     labels=function(x){format_string(x)}) +
    scale_y_continuous(expand=c(0,0), labels= \(x) round(x,0)) +
    scale_fill_manual(values = c("West" = destatis_blau[4], 
                                 "Ost" = destatis_rot[4]),
                      guide = NULL) +
    
    labs(title = "Wohnfläche alleinlebender Senioren 2022",
         subtitle = "in Prozent", x=NULL, y=NULL,
         caption = "Zensusdatenbank Tabelle 1000A-3096") +
    
    annotate("text", x = 17.6, y= 4.2, label = "West", hjust= 1, fontface = "bold") +
    annotate("text", x = 3.2 , y= 22, label = "Ost", hjust=.5, fontface = "bold") +
  
    theme_minimal(base_size=18) +
    theme(plot.margin = margin(20, 40, 0, 40),
          axis.text = element_text(size = 14),
          plot.subtitle = element_markdown(margin = margin(0,0,20,0)),
          plot.caption = element_markdown(hjust = 1, size = 9,
                                          margin = margin(16,0,0,0)),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()
         )

ggsave("img/06_wohnflaeche_singlesenior_OstWest.png", 
        height=2160, width=3840, units = "px")


