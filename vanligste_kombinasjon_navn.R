##### Hva er den vanligste kombinasjonen av fornavn og etternavn i Norge?

# Laster inn libraries
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(vroom)
library(stringr)
library(broom)
library(jtools)
library(forcats)

## Path til dataene
path <- "YOUR_PATH" # Sett inn hvor data er lagret på din datamaskin

## Laster inn data
jenter <- as_tibble(vroom(str_c(path, "fornavn_jenter.csv"), col_types = cols()))
gutter <- as_tibble(vroom(str_c(path, "fornavn_gutter.csv"), col_types = cols()))
etternavn <- as_tibble(vroom(str_c(path, "etternavn.csv"), col_types = cols()))

# Befolkningsvariabel
pop_norge <- 5391369 # Befolkning første kvartal 2020

# Renser data
jenter$antall <- as.numeric(as.character(jenter$personer_2020))
gutter$antall <- as.numeric(as.character(gutter$personer_2020))
etternavn$antall <- as.numeric(etternavn$personer_2020)

# Slår sammen fornavn gutter og jenter til en datatabell
gutter$kvinne <- 0
jenter$kvinne <- 1
fornavn <- rbind(gutter, jenter)

# Velger ut 20 vanligste etternavn, og 10 vanligste fornavn
etternavn <- etternavn[with(etternavn,order(-antall)),]
etternavn <- etternavn[1:20,]

fornavn <- fornavn[with(fornavn,order(-antall)),]
fornavn <- fornavn[1:20,]
fornavn[3,"fornavn"] <- "Bjørn" #Setter in "ø" i navnet



#### Del 1: Regner ut vanligst fornavn og etternavn


# Regner ut forekomst per 1000 innbygger
fornavn$per_1000 <- fornavn$antall/(pop_norge/1000)
etternavn$per_1000 <- etternavn$antall/(pop_norge/1000)

# Lager graf om de vanligste navnene
top_fornavn <- fornavn %>% 
  mutate(fornavn = fct_reorder(fornavn, per_1000)) %>% 
  ggplot(., aes(x = fornavn, y = per_1000, fill = as.factor(kvinne))) + 
  geom_col() +
  labs(title = "Vanligste fornavn per 1000 innbygger i Norge",
       x = "",
       y = "") +
  scale_fill_manual(values=alpha(c("#FFCC66", "#669933"),.6),
                    labels = c("Mann", "Kvinne")) +
  coord_flip() +
  geom_text(aes(label = round(per_1000, digits = 1)), position = position_nudge(y = 0.25), size = 3) +
  theme_apa() +
  theme(axis.ticks.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position="bottom")
  


top_etternavn <- etternavn %>% 
  mutate(etternavn = fct_reorder(etternavn, per_1000)) %>% 
  ggplot(., aes(x = etternavn, y = per_1000)) + 
  geom_col(fill = "red", alpha = 0.6) +
  labs(title = "Vanligste etternavn per 1000 innbygger i Norge",
       x = "",
       y = "") +
  scale_fill_manual(values=alpha(c("red"),.6),
                    labels = c("Mann", "Kvinne")) +
  coord_flip() +
  geom_text(aes(label = round(per_1000, digits = 1)), position = position_nudge(y = 0.1), size = 3) +
  theme_apa() +
  theme(axis.ticks.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position="bottom")

### Del 2: Beregner sannsynlighet for å ha gitt kombinasjon fornavn og etternavn
# p(kombinasjon) = p(fornavn) * p(etternavn)


# Regner ut sannsynlighet for å ha et gitt fornavn eller etternavn
fornavn$prop_fornavn <- (fornavn$antall/pop_norge)
etternavn$prop_etternavn = (etternavn$antall/pop_norge)


# Slår sammen alle mulige kombinasjoner av fornavn og etternavn
idx <- rep(1:20, each = 20)
fornavn_dup <- fornavn[idx,]

navnkomb <- cbind(fornavn_dup, etternavn)

# Regner ut predikert antall, per 100 000
navnkomb$predikert_antall <- ((navnkomb$prop_fornavn*navnkomb$prop_etternavn)*pop_norge) # Antar uniform sannsynlighetsfordeling
navnkomb$predikert_antall_100000 <- navnkomb$predikert_antall/(pop_norge/100000) # Forekomst per 100 000 innbygger


# Lager graf
top_navnkomb <- navnkomb %>% 
  select(fornavn, etternavn, predikert_antall_100000) %>% 
  mutate(fornavn = fct_reorder(fornavn, predikert_antall_100000)) %>%
  mutate(etternavn = fct_reorder(etternavn, predikert_antall_100000, .desc = TRUE)) %>%
  ggplot(., aes(x = fornavn, y = etternavn, fill = predikert_antall_100000)) + 
  geom_tile() +
  labs(title = "Hva er den vanligste navnekombinasjonen i Norge?",
       subtitle="Predikert forekomst per 100 000 innbygger",
       x = "",
       y = "") +
  scale_y_discrete(position = 'right') +
  coord_flip() +
  scale_fill_gradient(low="white", high="red") +
  geom_text(aes(label = round(predikert_antall_100000, digits = 1)), size = 3) +
  theme_apa() +
  theme(axis.ticks.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")


# Lagrer grafer som jpg-fil
ggsave("vanligst_fornavn.jpeg",
       plot = top_fornavn,
              height = 170,
              width = 270,
              units = "mm")

ggsave("vanligst_etternavn.jpeg",
       plot = top_etternavn,
       height = 170,
       width = 270,
       units = "mm")

ggsave("vanligst_navnkomb.jpeg",
       plot = top_navnkomb,
       height = 170,
       width = 270,
       units = "mm")



