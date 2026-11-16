library(restatis)
library(dplyr)

###################################################
#
# Dieses Script lädt alle benötigten Daten aus der 
# Zensusdatenbank per API mit dem restatis Paket 
#
# Daten werden als .rds abgelegt analog zum
# manuellen Download von flat_csv via
# https://ergebnisse.zensus2022.de/
#
###################################################


# Einwohnerzahlen >50.000
df1000a0001 <- gen_table(database="zensus", "1000A-0001", language = "de") |> 
                      filter(value_variable_label=="Personen") |> 
                      mutate(value = as.numeric(value)) |> 
                      filter(value > 5e4)

saveRDS(df1000a0001, "daten/1000A-0001_staedte50k.rds")

# AGS der Städte >500 Tsd Ew
df500k <- df1000a0001 |> 
            filter(value > 5e5) |> 
            select(`1_variable_attribute_code`)

ags500k <- paste(df500k$`1_variable_attribute_code`, collapse = ",")

# Städte >50 Tsd Ew
df50k <- df1000a0001 |> 
  filter(value > 5e4) |> 
  select(`1_variable_attribute_code`)



# Wohnungen nach Wohnfläche und Baujahr
df4000w2008dg <- gen_table(database="zensus", "4000W-2008", language = "de",
                         classifyingvariable1 = "GEBBJ7",
                         regionalvariable = "GEODL1") |> 
                mutate(value = as.numeric(value))

saveRDS(df4000w2008dg, "daten/4000W-2008_dg.rds")


df4000w2008 <- gen_table(database="zensus", "4000W-2008", language = "de",
                         classifyingvariable1 = "GEBBJ7",
                         regionalvariable = "GEOGM4",
                         regionalkey = ags500k) |> 
               mutate(value = as.numeric(value))

saveRDS(df4000w2008, "daten/4000W-2008_staedte500k.rds")


# Wohnfläche alleinlebender Senioren - Deutschland insgesamt
df1000a3096dg <- gen_table(database="zensus", "1000A-3096", language = "de",
                           classifyingvariable1 = "HSHSE2",
                           classifyingkey1 = "HSH-SENIOR-02",
                           classifyingvariable2 = "WHGNZ2",
                           classifyingkey2 = "NUTZ-EIGEN,NUTZ-MIETE",
                           regionalvariable = "GEODL1") |> 
                  mutate(value = as.numeric(value))

saveRDS(df1000a3096dg, "daten/1000A-3096_dg.rds")

# Wohnfläche alleinlebender Senioren - Bundesländer, Ost-West-Vergleich
df1000a3096bl <- gen_table(database="zensus", "1000A-3096", language = "de",
                           classifyingvariable1 = "HSHSE2",
                           classifyingkey1 = "HSH-SENIOR-02",
                           classifyingvariable2 = "WHGNZ2",
                           classifyingkey2 = "NUTZ-EIGEN,NUTZ-MIETE",
                           regionalvariable = "GEOBL1") |> 
                  mutate(value = as.numeric(value))

saveRDS(df1000a3096bl, "daten/1000A-3096_bl.rds")


# funktioniert per API aktuell nur bis Kreisebene
#df1000a3096lk <- gen_table(database="zensus", "1000A-3096", language = "de",
#                           classifyingvariable1 = "HSHSE2",
#                           classifyingkey1 = "HSH-SENIOR-02",
#                           classifyingvariable2 = "WHGNZ2",
#                           classifyingkey2 = "NUTZ-EIGEN,NUTZ-MIETE",
#                           regionalvariable = "GEOLK4",
#                           regionalkey = "*") |> 
#                 mutate(value = as.numeric(value))

# Gemeindetabelle via Web-Interface laden 
# https://ergebnisse.zensus2022.de/datenbank/online/table/1000A-3096
# alle 11.000 Gemeinden ausgewählt, entpackt 693 MB
df1000a3096 <- readr::read_csv2("tmp/1000A-3096_de_flat.csv")

# abfiltern auf Städte >50 Tsd Ew
df1000a3096 <- df1000a3096 |> 
                right_join(df50k) |> 
                mutate(value = as.numeric(value))

saveRDS(df1000a3096, "daten/1000A-3096_staedte50k.rds")


# Haushaltstyp (Familien) und Eigentumsform des Gebäudes
df5000h2005dg <- gen_table(database="zensus", "5000H-2005", language = "de",
                           classifyingvariable1 = "GEBEG1",
                           classifyingkey1 = "*",
                           regionalvariable = "GEODL1") |> 
                  mutate(value = as.numeric(value))

saveRDS(df5000h2005dg, "daten/5000H-2005_dg.rds")

df5000h2005 <- gen_table(database="zensus", "5000H-2005", language = "de",
                           classifyingvariable1 = "GEBEG1",
                           classifyingkey1 = "*",
                           regionalvariable = "GEOGM4",
                           regionalkey = ags500k) |> 
                  mutate(value = as.numeric(value))

saveRDS(df5000h2005, "daten/5000H-2005_staedte500k.rds")


# Nettokaltmiete in €/qm nach Eigentumsform des Gebäudes
df4000w0006 <- gen_table(database="zensus", "4000W-0006", language = "de",
                         regionalvariable = "GEOGM4",
                         regionalkey = ags500k) |> 
  mutate(value = as.numeric(sub(",",".",value)))

saveRDS(df4000w0006, "daten/4000W-0006_staedte500k.rds")
