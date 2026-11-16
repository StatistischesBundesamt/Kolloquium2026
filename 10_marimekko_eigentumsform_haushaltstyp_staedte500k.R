library(tidyverse)
library(ggtext)
library(janitor)
library(marimekko)

destatis_blau <- c("#bfe1ff","#9dceff","#76b4ff","#5294e6","#266ec3","#00439a","#00175a")
destatis_rot  <- c("#ffcace","#ffacb3","#ff8290","#e75e70","#c0304c","#8f0027","#47000c")
destatis_grau <- c("#dedede","#cacaca","#b1b1b1","#929292","#6f6f6f","#484848","#1f1f1f")

df <- readRDS("daten/5000H-2005_staedte500k.rds") |> 
  clean_names() |> 
  filter(
    x2_variable_attribute_label != "Insgesamt", # Haushaltstyp
    x3_variable_attribute_label != "Insgesamt", # Eigentumsform
  ) |>
  mutate(
    Haushaltstyp = sub("Mehrpersonenhaushalte ohne Kernfamilie", "WG", x2_variable_attribute_label),
    Haushaltstyp = sub(" ", "\n", Haushaltstyp)
  ) 

bev <- readRDS("daten/1000A-0001_staedte50k.rds") |> 
  filter(value > 5e5) |> 
  clean_names() |> 
  select(x1_variable_attribute_code, x1_variable_attribute_label, value) |> 
  rename(ewz_2022 = value) |> 
  arrange(desc(ewz_2022))


df <- df |> 
  left_join(bev)


sort_hhtyp <- c(
  "Einpersonenhaushalte\n(Singlehaushalte)",
  "Alleinerziehende\nElternteile",
  "Paare\nohne Kind(er)",
  "Paare\nmit Kind(ern)",
  "WG"
)

eigentumsform_farben <- c(
  "Bund, Land, Kommune" = destatis_grau[5],
  "Genossenschaft, Kirche" = destatis_grau[3],
  "Privatperson, WEG" = destatis_blau[3],
  "Wohnungsunternehmen" = destatis_rot[4]
)

df <- df |> 
  mutate(
    Eigentumsform = case_when(
      x3_variable_attribute_label %in% c("Bund oder Land","Kommune oder kommunales Wohnungsunternehmen") ~ "Bund, Land, Kommune",
      x3_variable_attribute_label %in% c("Privatperson/-en", "Gemeinschaft von Wohnungseigentümer/-innen") ~ "Privatperson, WEG",
      x3_variable_attribute_label %in% c("Privatwirtschaftliches Wohnungsunternehmen","Anderes privatwirtschaftliches Unternehmen") ~ "Wohnungsunternehmen",
      x3_variable_attribute_label %in% c("Wohnungsgenossenschaft", "Organisation ohne Erwerbszweck (z.B. Kirche)") ~ "Genossenschaft, Kirche"
    )
  ) |> 
  group_by(x1_variable_attribute_label, Haushaltstyp, Eigentumsform) |> 
    summarise(s_value = sum(value, na.rm = TRUE))

# Sortierung
df$Haushaltstyp = factor(df$Haushaltstyp, levels = sort_hhtyp)
df$x1_variable_attribute_label = factor(df$x1_variable_attribute_label, levels = bev$x1_variable_attribute_label)



p <- df |> 
  ggplot() + 
    geom_marimekko(aes(fill = Eigentumsform, weight = s_value),
                   formula = ~ Haushaltstyp | Eigentumsform,
                   gap = 0.005, alpha = 1) +
    scale_fill_manual(values = eigentumsform_farben,
                      guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~ x1_variable_attribute_label,
               labeller = labeller(x1_variable_attribute_label = \(x) sub(",.*$", "", x))) +
  
  labs(title ="Typ des privaten Haushalts (Familien) nach Eigentumsform des Gebäudes 2022",
       caption = "Zensusdatenbank Tabelle 5000H-2005") +
  theme_minimal(base_size=18) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "plain", 
                                  margin = margin(0,0,3,3),
                                  hjust=0, size=12),
        legend.key.spacing.y = unit(1, "mm"),
        legend.key.height = unit(9, "mm"),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        plot.caption = element_markdown(hjust = 1.3, size = 9,
                                        margin = margin(0,0,0,0)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank()
  )

ggsave("img/10_marimekko_eigentumsform_haushaltstyp_staedte500k.png", 
       plot = p, 
       height=2160, width=3840, units = "px")

