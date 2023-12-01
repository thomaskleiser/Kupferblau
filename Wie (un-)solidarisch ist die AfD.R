#### 1. Laden der fuer die Datenanalyse relevanten Packete: ####

library(haven)
library(readr)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(patchwork)

#### 2. Festelgung des Arbeitsverzeichnis: ####
setwd("C:/Users/thoma/Documents/Data")

#### 3. Import der fuer die Datenanalyse relevanten Datensaetze: ####

# Erstellung des Detensatz fuer die letzten Umfrage-Ergebnisse zur Bundestagswahl (online unter: https://www.wahlrecht.de/umfragen/):
sonntagsfrage <- data.frame(partei = c("CDU/CSU","SPD","GRÜNE","FDP","LINKE","AfD"),
                            anteil = c(26,19,14,7,5,20)
                            )

# Import des Chapell Hill Expert Survey Datensatz fuer Parteien (online unter: https://www.chesdata.eu/s/1999-2019_CHES_dataset_meansv3.csv):
ches_party <- read_csv("CHES_Data_v3.csv") %>%
  select(year,
         country_id = country,
         region = eastwest,
         party_id,
         party_abb = party,
         manifesto_id = cmp_id,
         party_family = family,
         position_general = lrgen,
         position_economy = lrecon,
         position_culture = galtan,
         importance_economy = lrecon_salience,
         blur_economy = lrecon_blur,
         expert
  )

ches_party_germany <- ches_party %>% filter(country_id == 3)

# Import des Datensatz Manifesto Projects fuer Parteiprogramme (online unter: https://manifestoproject.wzb.eu/datasets):
mp_parties <- read_csv("MP_Datas_v2023a.csv") %>%
  select(country_id = country,
         country_name = countryname,
         party_id = party,
         party_abb = partyabbrev,
         party_name = partyname,
         date,
         welfare,
         left_right = rile)

mp_parties <- mp_parties %>%
  mutate(year = as.numeric(str_extract(date,"\\d{4}"))) %>%
  relocate(year, .after = party_name) %>%
  select(-date)

mp_parties_germany <- mp_parties %>%
  filter(country_name == "Germany")

#### 4. Erstellung des Themes: ####

# ... fuer Saeulendiagramme:
theme_kupfer1 <- theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
                       plot.subtitle = element_text(size = 20, hjust = 0.5),
                       plot.caption = element_text(size = 15),
                       axis.title.x = element_blank(),
                       axis.title.y = element_text(size = 15, face = "bold"),
                       axis.text = element_text(size = 12.5, face = "bold"),
                       axis.line = element_line(color = "black"),
                       legend.title = element_text(face = "bold", hjust = 0.5),
                       legend.background = element_rect(color = "black"),
                       panel.grid.major.y = element_line(color = "grey20"),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank()
                       )

# ... fuer Streudiagramme:
theme_kupfer3 <- theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
                       plot.subtitle = element_text(size = 20, hjust = 0.5),
                       plot.caption = element_text(size = 15),
                       axis.title = element_text(size = 15, face = "bold"),
                       axis.text = element_text(size = 12.5, face = "bold"),
                       axis.line = element_line(color = "black"),
                       legend.title = element_text(face = "bold", hjust = 0.5),
                       legend.background = element_rect(color = "black"),
                       panel.grid.major = element_line(color = "grey20"),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank()
                       )

#### 5. Visualisierung: ####

# Visualisierung der letzten Umfrageergebnisse zur Bundestagswahl:
plot4 <- sonntagsfrage %>%
  ggplot( aes(x = reorder(partei,desc(anteil)), y = anteil)) +
  geom_bar(stat = "identity",
           width = 0.5,
           fill = c("#000000","#e2001a", "#1fa12d","#ffed00","#dc0000",
                    "#009fe1")
) +
  geom_text( aes(y = anteil + 1, label = str_c(anteil,"%")),
             size = 5,
             fontface = "bold") +
  labs(title = "Die AfD in den Umfragen",
       subtitle = "Wahlabsicht, Anteil in Prozent (1.7.2023)",
       y = "Wahlabsicht in %",
       caption = "Quelle: INSA") +
  theme_kupfer1

ggdraw(plot4) + 
  draw_image("C:/Users/thoma/Documents/Kupferblau/logo/kupferblau_logo.jpg",
             x = 0.45,
             y = 0.40,
             scale = 0.2)

# Visualisierung der sozio-oekonomische und sozio-kulturellen links-rechts-Orientierung deutscher Parteien
plot1.1 <- ches_party_germany %>%
  filter(year == 2014 & !(party_abb %in% c("Piraten","DieTier","NPD")) ) %>%
  ggplot( aes(x = position_economy, y = position_culture)) +
  geom_point(size = 2.5) +
#  geom_text(aes(x = position_economy, y = position_culture + 0.5, label = party_abb)) +
  geom_hline(yintercept = 5, color = "black") +
  geom_vline(xintercept = 5, color = "black") +
  geom_text(x = 1, y = 5.25, label = "staats-zentriert", size = 3.5, color = "grey25", fontface = "italic") +
  geom_text(x = 1, y = 4.75, label = "(links)", size = 3.5, color = "grey25", fontface = "italic") +
  geom_text(x = 9, y = 5.25, label = "markt-zentriert", size = 3.5, color = "grey25", fontface = "italic") +
  geom_text(x = 9, y = 4.75, label = "(rechts)", size = 3.5, color = "grey25", fontface = "italic") +
  geom_text(x = 4.75, y = 8.75, label = "kollektivistisch", size = 3.5, color = "grey25", fontface = "italic", angle = 90) +
  geom_text(x = 5.25, y = 8.75, label = "(rechts)", size = 3.5, color = "grey25", fontface = "italic", angle = 90) +
  geom_text(x = 4.75, y = 1.25, label = "individualistisch", size = 3.5, color = "grey25", fontface = "italic", angle = 90) +
  geom_text(x = 5.25, y = 1.25, label = "(links", size = 3.5, color = "grey25", fontface = "italic", angle = 90) +
  lims(x = c(0,10),
       y = c(0,10)) +
  labs(subtitle = "Links-Rechts-Verortung \n (Expertenbefragung, 2014)",
       x = "sozio-ökonomisch",
       y = "sozio-kulturell") +
  theme_kupfer3 +
  theme(plot.subtitle = element_text(size = 17.5, face = "italic"),
        axis.title = element_text(size= 12.5)
        )

plot1.2 <- ches_party_germany %>%
  filter(year == 2019 & !(party_abb %in% c("Piraten","DieTier")) ) %>%
  ggplot( aes(x = position_economy, y = position_culture)) +
  geom_point(size = 2.5) +
#  geom_text(aes(x = position_economy, y = position_culture + 0.5, label = party_abb)) +
  geom_hline(yintercept = 5, color = "black") +
  geom_vline(xintercept = 5, color = "black") +
  geom_text(x = 1, y = 5.25, label = "staats-zentriert", size = 3.5, color = "grey25", fontface = "italic") +
  geom_text(x = 1, y = 4.75, label = "(links)", size = 3.5, color = "grey25", fontface = "italic") +
  geom_text(x = 9, y = 5.25, label = "markt-zentriert", size = 3.5, color = "grey25", fontface = "italic") +
  geom_text(x = 9, y = 4.75, label = "(rechts)", size = 3.5, color = "grey25", fontface = "italic") +
  geom_text(x = 4.75, y = 8.75, label = "kollektivistisch", size = 3.5, color = "grey25", fontface = "italic", angle = 90) +
  geom_text(x = 5.25, y = 8.75, label = "(rechts)", size = 3.5, color = "grey25", fontface = "italic", angle = 90) +
  geom_text(x = 4.75, y = 1.25, label = "individualistisch", size = 3.5, color = "grey25", fontface = "italic", angle = 90) +
  geom_text(x = 5.25, y = 1.25, label = "(links", size = 3.5, color = "grey25", fontface = "italic", angle = 90) +
  lims(x = c(0,10),
       y = c(0,10)) +
  labs(subtitle = "Links-Rechts-Verortung \n (Expertenbefragung, 2019)",
       x = "sozio-ökonomisch",
       y = "sozio-kulturell",
       caption = "Quelle: Chapell Hill Expert Survey") +
  theme_kupfer3 + 
  theme(plot.subtitle = element_text(size = 17.5, face = "italic"),
        axis.title = element_text(size= 12.5)
        )

plot1 <- plot1.1 + plot1.2 +
  plot_annotation(title = "Links-Rechts-Orientierung, Parteien im Vergleich") &
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5))

plot1

# Visualisierung der Wichtigkeit sozio-oekonomischer Themene fuer deutsche Parteien:
plot2.1 <- ches_party_germany %>%
  filter(year == 2019 & !(party_abb %in% c("Piraten","DieTier")) ) %>%
  ggplot( aes(x = reorder(factor(party_abb,
                                 levels = c("AfD","CDU","CSU","FDP","GRUNEN",
                                            "LINKE","SPD"),
                                 labels = c("AfD","CDU","CSU","FDP","GRÜNE",
                                            "LINKE","SPD")
                                 ),
                          desc(importance_economy)),
              y = importance_economy)
              ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.5,
           fill =c("#000000","#008ac5","#dc0000","#009fe1","#1fa12d",
                   "#e2001a","#ffed00")
           ) +
  lims(y = c(0,10)) +
  labs(subtitle = "Wichtigkeit wirtschaftlicher Themen
       (Expertenbefragung, 2019)",
       y = "geschätzte Wichtigkeit (0-10)*",
       caption = "*0 = nicht wichtig,
       10 = sehr wichtig") +
  theme_kupfer1 +
  theme(plot.subtitle = element_text(size = 17.5, face = "italic"),
        axis.title = element_text(size= 12.5)
        )

plot2.2 <- ches_party_germany %>%
  filter(year == 2019 & !(party_abb %in% c("Piraten","DieTier")) ) %>%
  ggplot( aes(x = reorder(factor(party_abb,
                                 levels = c("AfD","CDU","CSU","FDP","GRUNEN",
                                            "LINKE","SPD"),
                                 labels = c("AfD","CDU","CSU","FDP","GRÜNE",
                                            "LINKE","SPD")
  ),
  desc(blur_economy)),
  y = blur_economy)
  ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.5,
           fill = c("#000000","#008ac5","#dc0000","#009fe1","#1fa12d",
                    "#e2001a","#ffed00")
  ) +
  lims(y = c(0,10)) +
  labs(subtitle = "Undeindeutigkeit der wirtschaftlichen Positionen
       (Expertenbefragung, 2019)",
       y = "geschätzte Undeindeutigkeit (0-10)*",
       caption = "*0 = sehr eindeutig,
       10 = sehr uneindeutig
       Quelle: Chapel Hill Expert Survey") +
  theme_kupfer1 +
  theme(plot.subtitle = element_text(size = 17.5, face = "italic"),
        axis.title = element_text(size= 12.5)
        )

plot2 <- plot2.1 + plot2.2 +
plot_annotation(title = "Wirtschaftliche Themen, Wichtigkeit und Uneindeutigkeit, Parteien im Vergleich") &
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

plot2

# Visualisierung der Nennung einzelner Themen in den Parteipgrogrammen:
plot3 <- mp_parties_germany %>%
  filter(year == 2021 & party_abb != "SSW") %>%
  ggplot( aes(x = reorder(party_abb,desc(welfare)), y = welfare) ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.5,
           fill = c("#1fa12d","#dc0000","#e2001a","#ffed00","#000000",
                    "#009fe1")
           ) +
  geom_text( aes(y = welfare + 1, label = str_c(round(welfare),"%")),
             size = 5,
             fontface = "bold") +
  scale_x_discrete(labels = c("LINKE","SPD","GRÜNE","FDP","CDU/CSU","AfD")) +
  labs(title = "Wichtigkeit von Wohlfahrtstaat im Wahlkampf",
       subtitle = "Anteil wohlfahrtstaatlicher Themen in Wahlprogrammen (2021)",
       y = "Anteil in %",
       caption = "Quelle: Manifesto Project") +
  theme_kupfer1

ggdraw(plot3) + 
  draw_image("C:/Users/thoma/Documents/Kupferblau/logo/kupferblau_logo.jpg",
             x = 0.45,
             y = 0.40,
             scale = 0.2)