library(tidyverse)
library(ggtext)
library(janitor)

destatis_blau <- c("#bfe1ff","#9dceff","#76b4ff","#5294e6","#266ec3","#00439a","#00175a")
destatis_rot  <- c("#ffcace","#ffacb3","#ff8290","#e75e70","#c0304c","#8f0027","#47000c")
destatis_grau <- c("#dedede","#cacaca","#b1b1b1","#929292","#6f6f6f","#484848","#1f1f1f")

eigentumsform_farben <- c(
  "Bund oder Land" = destatis_grau[6],
  "Kommune oder kommunales Wohnungsunternehmen" = destatis_grau[5],
  "Wohnungsgenossenschaft" = destatis_grau[3],
  "Organisation ohne Erwerbszweck (z.B. Kirche)" = destatis_grau[2],
  "Privatperson/-en" = destatis_blau[3],
  "Gemeinschaft von Wohnungseigentümer/-innen" = destatis_blau[4],
  "Privatwirtschaftliches Wohnungsunternehmen" = destatis_rot[4],
  "Anderes privatwirtschaftliches Unternehmen" = destatis_rot[5]
)

df <- readRDS("daten/4000W-0006_staedte500k.rds") |> 
  clean_names() |> 
  filter(
    x2_variable_attribute_label != "Insgesamt", # Haushaltstyp
  )

bev <- readRDS("daten/1000A-0001_staedte50k.rds") |> 
  filter(value > 5e5) |> 
  clean_names() |> 
  select(x1_variable_attribute_code, value) |> 
  rename(ewz_2022 = value)

df <- df |> 
  left_join(bev)|> 
  arrange(desc(ewz_2022))

p <- df |> 
  mutate(x1_variable_attribute_label = fct_inorder(x1_variable_attribute_label),
         x2_variable_attribute_label = factor(x2_variable_attribute_label,
                                              levels = (names(eigentumsform_farben)))) |> 
  ggplot() +
    geom_col(aes(x=x2_variable_attribute_label, y=value, fill=x2_variable_attribute_label)) +
    scale_fill_manual(values = eigentumsform_farben) +
    guides(fill = guide_legend(ncol = 2, byrow = FALSE, reverse = TRUE)) +
  
    scale_y_continuous(breaks = seq(0,14,2)) +
    scale_x_discrete(labels = NULL) + 
    coord_flip() +
  
    facet_wrap(~ x1_variable_attribute_label, 
               labeller = labeller(x1_variable_attribute_label = \(x) sub(",.*", "", x))
               ) +

    labs(title ="Nettokaltmiete in €/m² nach Eigentumsform des Gebäudes 2022",
         caption = "Zensusdatenbank Tabelle 4000W-0006",
      fill = NULL, x=NULL, y=NULL) +  
  theme_minimal(base_size=18) +

  theme(
        strip.text = element_text(face = "plain", 
                                  margin = margin(6,0,6,9),
                                  hjust=0, size=12),
        panel.grid.major.x = element_line(
          color = "#bbb",
          linewidth = 0.5
        ),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 10),
        plot.caption = element_markdown(hjust = 1, size = 9,
                                        margin = margin(0,0,0,0)),
        legend.position = "bottom",
        legend.key.spacing.y = unit(1.1, "mm"),
        legend.key.spacing.x = unit(15, "mm"),
        legend.key.height = unit(3.5, "mm"),
        legend.key.width = unit(4.5, "mm"),
        legend.text = element_text(size = 10)
  )

ggsave("img/11_miete_eigentumsform_staedte500k.png", 
       plot = p, 
       height=2160, width=3840, units = "px")

