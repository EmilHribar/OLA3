#Opgave 5 – Data Science modellen og DMI’s API
library(httr)
library(jsonlite)

#Opgave 5.2 – Undersøg de data API’et returnerer.

##API endpoint#
url <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items"

###API Nøgle 
api_key <- "b4ffb9af-2ad8-468c-9f24-15c9e5577480"

# Send GET-anmodning til API'et
response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))

### Tjek koden
status_code(response)

#Parse JSON-indholdet
data <- fromJSON(content(response, "text", encoding = "UTF-8"))

## Vis datastrukturen
str(data)

#Ekstraher relevante felter fra dataene
observations <- data$features

#Hvilken enhed angiver man ”visibility” i?
# I meter

#Hvor ofte opdateres ”wind_max”? 
# Hvert 10. minut

# Hvilken by gemmer sig bag stationen med id’et 05272? 
# Brande


#Opgave 5.3 – Andre API’er

#DMI har også et andet API, hvor man kan trække en vejrudsigt. Angiv linket til API’ets ”endpoint”.
#Hvor mange ”collections” kan man spørge til? Hvilket ID har Lille Bælt? Hvilket filformat ender
#man med at få ”forecasten” i?

library(httr)
library(jsonlite)

#API- nøglen
api_key <- "1883872b-edbe-4ddb-aff5-d00876af5164"

#### 1: Endpoint for at hente alle collections fra DMI ####
collections_endpoint <- "https://dmigw.govcloud.dk/v1/forecastdata/collections"

# Send GET-anmodning
response <- GET(
  url = collections_endpoint,
  add_headers("X-Gravitee-Api-Key" = api_key)
)

# Parse og udskriv resultatet - laver JSOM svar til en dataframe
collections_data <- content(response, as = "text", encoding = "UTF-8")
collections_parsed <- fromJSON(collections_data, flatten = TRUE)
collections_df <- as.data.frame(collections_parsed$collections)

# Udskriver alle collections for at få et overblik
print(collections_df)

#### 2: finder ud af, hvor mange collections der er ####
num_collections <- nrow(collections_df)
cat("Antal collections:", num_collections, "\n")


# finder lillebælt
print(collections_df) # for at få et overblik over datasættet, samt for at se hvilket ID lillebælt har
# ved at lillebælt har dkss_lb
lillebaelt_id <- collections_df[collections_df$id == "dkss_lb", "id"]
cat("ID for Lillebælt collection:", lillebaelt_id, "\n")


# tjek filformat type:
#Definer forecast endpoint
forecast_endpoint <- "https://dmigw.govcloud.dk/v1/forecastdata/collections/forecast"
response <- GET(url = forecast_endpoint, add_headers("X-Gravitee-Api-Key" = api_key))

#Udskriv Content-Type fra headeren
content_type <- headers(response)$`content-type`
print(content_type)


# Opgave 5.4 – En graf med mangler
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Definer dine stationId'er Aarhus og Anholt
station_ids <- c("06074", "06079")  # Tilføj flere stationer, hvis nødvendigt

# Opret en tom liste til at gemme data fra hver station
data_list <- list()

# API nøgle
api_key <- "9eaa8b44-d20c-4910-aa38-762e9eced7f3"

# Loop igennem hver station og hent data for vindretning
for (station in station_ids) {
  # Definer URL med den aktuelle stationId
  url <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                "parameterId=wind_dir_past1h&datetime=2023-10-01T00:00:00Z/2023-10-31T23:59:59Z&stationId=", station)
  
  # Send GET-anmodning
  response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
  
  # Parse JSON-indhold og gem i listen
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  data_list[[station]] <- as.data.frame(data$features)
}

# Kombiner alle data frames i én samlet data frame
combined_data <- bind_rows(data_list)

# Opret en ny kolonne med kun datoen fra observed (fjerner klokkeslæt)
combined_data <- combined_data %>%
  mutate(date = as.Date(properties$observed, format = "%Y-%m-%dT%H:%M:%SZ"))

# Beregn gennemsnitlig vindretning per dag og per station
daily_wind_dir <- combined_data %>%
  group_by(date, stationId = properties$stationId) %>%
  summarize(avg_wind_dir = mean(properties$value, na.rm = TRUE))  # Gennemsnit pr. dag

# Konverter stationId til numerisk, hvis det er en faktor eller karakter
daily_wind_dir <- daily_wind_dir %>%
  mutate(stationId = as.numeric(as.factor(stationId)),  # Konverter til numerisk
         angle_rad = avg_wind_dir * pi / 180,           # Konverter til radianer
         x_end = cos(angle_rad) * 0.5,                  # Beregn x-komponenten og juster længde
         y_end = sin(angle_rad) * 0.5)                  # Beregn y-komponenten og juster længde



# Loop igennem hver station og hent data for vindstyrke
for (station in station_ids) {
  # Definer URL med den aktuelle stationId
  url <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                "parameterId=wind_max_per10min_past1h&datetime=2023-10-01T00:00:00Z/2023-10-31T23:59:59Z&stationId=", station)
  
  # Send GET-anmodning
  response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
  
  # Parse JSON-indhold og gem i listen
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  data_list[[station]] <- as.data.frame(data$features)
}

# Opret en ny data frame til vindstyrke ved at kombinere data for alle stationer fra data_list
combined_data_strength <- bind_rows(data_list)

# Opret en ny kolonne med kun datoen fra 'observed' (fjerner klokkeslæt)
combined_data_strength <- combined_data_strength %>%
  mutate(date = as.Date(properties$observed, format = "%Y-%m-%dT%H:%M:%SZ"))

# Beregn gennemsnitlig vindstyrke per dag og per station
daily_wind_strength <- combined_data_strength %>%
  group_by(date, stationId = properties$stationId) %>%
  summarize(avg_wind_strength = mean(properties$value, na.rm = TRUE))  # Gennemsnit pr. dag


# Opret en data frame til at matche de numeriske og tekst-baserede stationId'er
station_mapping <- data.frame(
  numeric_id = c(1, 2),
  text_id = c("06074", "06079")
)

# Slå de to data frames sammen baseret på matching af de numeriske og tekst ID'er
daily_wind_dir <- daily_wind_dir %>%
  left_join(station_mapping, by = c("stationId" = "numeric_id")) %>%
  rename(stationId_text = text_id)  # Omdøb til stationId_text for at undgå navnekonflikter

# Brug stationId_text i join med daily_wind_strength
combined_data <- left_join(daily_wind_strength, daily_wind_dir, by = c("date", "stationId" = "stationId_text"))

# Variabel for pilens længde
arrow_length <- 0.8

# Plot af vindstyrke og vindretning
ggplot(combined_data, aes(x = date, color = factor(stationId))) +
  # Linjegraf for gennemsnitlig vindstyrke
  geom_line(aes(y = avg_wind_strength), linewidth = 1) +
  
  # Tilføj pile for vindretningen
  geom_segment(
    aes(
      xend = date + arrow_length * cos(angle_rad),     # Ændring i x-retning
      y = avg_wind_strength,
      yend = avg_wind_strength + arrow_length * sin(angle_rad), # Ændring i y-retning
      color = factor(stationId)
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    size = 0.6,
    lineend = "round"
  ) +
  
  # Titel og akse-labels
  labs(
    title = "I oktober 2023 er den gennemsnitlige vindstyrke højest på Anholt",
    x = "Dato - Oktober 2023",
    y = "Vindstyrke (m/s)",
    color = "Station"
  ) +
  
  # Æstetik
  scale_color_manual(values = c("06074" = "blue", "06079" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )+
  guides(color = guide_legend(title = "Station ID")) # Tilføj en specificeret legend for stationerne

