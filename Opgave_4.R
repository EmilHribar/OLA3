#Opgave 4 – Stabilitet i jeres forbrugertillidsindikator

# Opgave 1 - Forarbejde
{
  library(dkstat)
  library(dplyr)
  library(stringr)
  library(readr)
  library(ggplot2)
  library(tidyr)
  
  #Forbrugertillidsindikatorer og fremtidig vækst i husholdningernes forbrugsudgift
  #Opgave 1.1 – Kombinationsalgoritme i R
  #Lav alle kombinationer af de 12 spørgsmål i forbrugerundersøgelsen af DST. I skal bruge data fra 1.
  #kvartal 2000 til og med 4. kvartal 2024
  
  #Her vil jeg berøre 2 datsæt. Det ene FORV1 og NKN3. 
  #Research goalet er, at jeg skal undersøge om DI's eller DST's FTI er den bedste
  #indikator til at belyse forbruget udvikling 
  
  #Forbrguertillid
  #1. Data retrieving 
  #vi finder datasæt fodpm
  FORV1Meta=dst_meta("FORV1")
  
  my_query <- list(
    INDIKATOR="*",
    Tid="*"
  )
  
  #hent tabel
  FORV1=dst_get_data(table = "FORV1", query = my_query)
  
  #2. Data cleaning
  
  #Lav til lang format
  FORV1_wide <- FORV1 %>%
    pivot_wider(names_from = INDIKATOR, values_from = value)
  
  #Fjern række 1 til 303 = til 2000, måned 1
  FORV1_wide <- FORV1_wide[-(1:303), ]
  
  #Beregn gennemsnittet af Forbrugertillidsindikatoren_DI
  FORV1_wide$Forbrugertillidsindikatoren_DI <- rowMeans(FORV1_wide[, c(3, 5, 7, 11)], na.rm = TRUE)
  
  #Ændring af kolonne rækkefølge
  FORV1_wide<-FORV1_wide[,c(1:2,15,3:14)]
  
  #Nyt navn til DF
  FORV1_med_DI<-FORV1_wide
  
  #Afrund kolonnen 'Forbrugertillidsindikatoren_DI' til 1 decimal
  FORV1_med_DI$Forbrugertillidsindikatoren_DI <- round(FORV1_med_DI$Forbrugertillidsindikatoren_DI, 1)
  
  #Omdøb kolonne 2 i FORV1_med_DI til Forbrugertillidsindikatoren_DST
  colnames(FORV1_med_DI)[2] <- "Forbrugertillidsindikatoren_DST"
  
  #Aggregering fra måneder til kvartaler
  #Konverter måneder til kvartalers startdatoer i formatet "YYYY-MM-DD"
  FORV1_med_DI$TID <- as.Date(cut(as.Date(FORV1_med_DI[[1]]), "quarter"))
  
  # Identificer alle numeriske kolonner undtagen TID (kolonne 1)
  numeric_cols <- sapply(FORV1_med_DI, is.numeric)
  
  # Opret en ny dataframe med gennemsnit pr. kvartal for alle numeriske kolonner
  FORV_1_kvartaler <- aggregate(. ~ TID, data = FORV1_med_DI[, c("TID", names(FORV1_med_DI)[numeric_cols])], FUN = mean, na.rm = TRUE)
  
  # Afrund alle numeriske kolonner til 1 decimal
  FORV_1_kvartaler[, -1] <- round(FORV_1_kvartaler[, -1], 1)  # -1 for at undgå TID-kolonnen
  
  #Fjern række 98 - 100, da det skal matche forbrug, som kun går til 2024K2
  FORV_1_kvartaler <- FORV_1_kvartaler[-c( 99:100), ]
  
  # Juster indekserne, hvis spørgsmål 6 starter i kolonne 10
  kolonner_til_omvendt_fortegn <- c(9, 11)  # Ændring baseret på str eller colnames
  
  # Ændring af fortegnet ved at multiplicere værdierne i de relevante kolonner med -1
  FORV_1_kvartaler[, kolonner_til_omvendt_fortegn] <- FORV_1_kvartaler[, kolonner_til_omvendt_fortegn] * -1
  
  #færdig df for forv1
  FORV1_færdig<-FORV_1_kvartaler
  
  #______________________________
  
  #Forbrug
  #1. Data retrieving
  #vi finder datasæt fodpm
  NKN3Meta=dst_meta("NKN3")
  
  my_query <- list(
    TRANSAKT="*",
    PRISENHED="*",
    Tid="*"
  )
  
  #hent tabel
  NKN3=dst_get_data(table = "NKN3", query = my_query)
  
  #2. Datacleaing
  #Behold rækkerne fra 103 til 204
  NKN3 <- NKN3[103:204, ]
  
  #3.Beregn den årlige udvikling ved at sammenligne med 4 kvartaler tidligere
  NKN3$Forbrugets_årlig_udvikling <- (NKN3$value - c(rep(NA, 4), NKN3$value[1:(nrow(NKN3) - 4)])) / 
    c(rep(NA, 4), NKN3$value[1:(nrow(NKN3) - 4)]) * 100
  
  # Fjern de første 4 rækker fra NKN3, da de nu er NA-værdier
  NKN3 <- NKN3[-c(1:4), ]
  
  #4. lidt datacleaning igen
  # Fjern kolonne 1 og 2 fra NKN3
  NKN3 <- NKN3[, -c(1, 2)]
  
  # Afrund kolonnen Årlig_udvikling til 1 decimal
  Færdig_NKN3<-NKN3$Forbrugets_årlig_udvikling <- round(Færdig_NKN3<-NKN3$Forbrugets_årlig_udvikling, 1)
  
  #Nyt navn til færdig
  Færdig_NKN3<-NKN3
  
  #_____________________________________
  
  #Sammenkobl de 2 dataframes til én, med alle 3 nøgletal
  
  # Merge de to dataframes uden at nævne kolonnerne
  FORV1_og_NKN3_samlet <- merge(
    Færdig_NKN3,      # Den første dataframe
    FORV1_færdig,       # Den anden dataframe
    by = "TID",        # Fælles kolonne i begge dataframes
    all = TRUE         # Beholder alle rækker fra begge dataframes (juster om nødvendigt)
  )
  
  #_____________________________
  
  # Opret en tom liste til at gemme de samlede indikatorer
  samlede_indikatorer_liste <- list()
  kombinationer_liste <- list()  # Gem kombinationerne af spørgsmål
  
  # Generer alle kombinationer af spørgsmål fra kolonne 7 og frem
  for (k in 1:(ncol(FORV1_og_NKN3_samlet) - 5)) {
    kombinationer <- combn(6:ncol(FORV1_og_NKN3_samlet), k)
    for (i in 1:ncol(kombinationer)) {
      # Vælg de aktuelle spørgsmål baseret på kolonneindeks
      valgte_spørgsmål <- FORV1_og_NKN3_samlet[, kombinationer[, i], drop = FALSE]
      
      # Beregn gennemsnittet af de valgte spørgsmål for at skabe en samlet indikator
      samlet_indikator <- rowMeans(valgte_spørgsmål, na.rm = TRUE)
      
      # Gem den samlede indikator og kombinationen i listerne
      samlede_indikatorer_liste[[length(samlede_indikatorer_liste) + 1]] <- samlet_indikator
      kombinationer_liste[[length(kombinationer_liste) + 1]] <- kombinationer[, i]
    }
  }
  
  # Definer DI's ønskede kombination af spørgsmål: kolonner 1, 3, 5 og 9 (adjusted for index shift)
  di_kombination <- c(6, 8, 10, 14)
  
  # Nu kan vi køre regressioner for hver af de samlede indikatorer
  model_results <- list()
  di_r_squared <- NA  # Opret variabel til DI's R-squared
  
  for (j in 1:length(samlede_indikatorer_liste)) {
    # Hent den samlede indikator og den tilhørende kombination
    samlet_indikator <- samlede_indikatorer_liste[[j]]
    kombination <- kombinationer_liste[[j]]
    
    # Lav en data.frame med Y-variablen og den valgte samlede indikator
    data_model <- data.frame(
      Forbrugets_årlig_udvikling = FORV1_og_NKN3_samlet$Forbrugets_årlig_udvikling,
      Samlet_indikator = samlet_indikator
    )
    
    # Fjern NA-værdier
    data_model <- na.omit(data_model)
    
    # Byg modellen
    model <- lm(Forbrugets_årlig_udvikling ~ Samlet_indikator, data = data_model)
    
    # Gem R-squared og kombination
    model_results[[length(model_results) + 1]] <- list(
      r_squared = summary(model)$r.squared,
      kombination = kombination,
      model_summary = summary(model)
    )
    
    # Tjek om denne kombination matcher DI's specifikation
    if (all(sort(kombination) == sort(di_kombination))) {
      di_r_squared <- summary(model)$r.squared
    }
  }
  
  # Sorter resultaterne efter R-squared i faldende rækkefølge
  sorted_results <- model_results[order(sapply(model_results, function(x) x$r_squared), decreasing = TRUE)]
  
  # Find DI's rangplacering
  di_index <- which(sapply(sorted_results, function(x) all(sort(x$kombination) == sort(di_kombination))))
  di_ranking <- ifelse(length(di_index) > 0, di_index, NA)
  
  # Træk de 10 bedste kombinationer ud
  top_10 <- sorted_results[1:10]
  
  # Opret en tom matrix til at gemme resultatet
  #result_matrix <- matrix("", nrow = 11, ncol = 14)  # Lav plads til de 10 bedste + DI's indikator
  #colnames(result_matrix) <- c("Placering", paste("Spm.", 1:12), "R-squared")
  
  # Udfyld data i tabellen med de 10 bedste kombinationer
  #for (i in 1:10) {
  #komb <- top_10[[i]]$kombination - 5  # Omdan kolonneindeks til spørgsmål 1-12
  #row <- ifelse(1:12 %in% komb, "✔", "")  # Lav kryds for inkluderede spørgsmål
  #result_matrix[i, ] <- c(i, row, round(top_10[[i]]$r_squared, 4))  # Tilføj placering og R-squared
  
  
  # Tilføj DI's indikator med rang og dens R-squared til den sidste række
  #di_row <- c(paste("DI's indikator (Rank:", di_ranking, ")"), ifelse(1:12 %in% c(1, 3, 5, 9), "✔", ""), round(di_r_squared, 4))
  #result_matrix[11, ] <- di_row
  
  # Konverter matrix til en data.frame for bedre visning
  #result_df <- as.data.frame(result_matrix)
  
  # Udskriv den opdaterede tabel
  # print(result_df)
}
# __________________________________


#Opgave 4.1 – Test af model fra opgave 1

#Undersøg stabiliteten af jeres fundne indikator fra opgave 1.
#Giv en grundig forklaring på opsætning til at undersøge stabiliteten.

{

#Beregning af Forbrugertillidsindikatoren_rank1
FORV1_og_NKN3_samlet$Forbrugertillidsindikatoren_rank1 <- rowMeans(FORV1_og_NKN3_samlet[, c(6, 12, 14, 15)], na.rm = TRUE)

#Ændring af kolonne rækkefølge
FORV1_og_NKN3_samlet<-FORV1_og_NKN3_samlet[,c(1:5,18,6:17)]

# Antal rækker i det oprindelige datasæt
total_rækker <- 98  # Vi ved, at der er 98 rækker

# Sikrer, at `TID` er i datoformat i hele datasættet
FORV1_og_NKN3_samlet$TID <- as.Date(FORV1_og_NKN3_samlet$TID)

# Liste til at gemme R-squared værdier og koefficienter for hver periode
stabilitets_resultater <- list()

# Kør regression for successivt kortere perioder, der starter fra 98 rækker
for (slut_række in total_rækker:65) {
  # Kør regressionen for de første `slut_række` rækker direkte på `FORV1_og_NKN3_samlet`
  model <- lm(Forbrugets_årlig_udvikling ~ Forbrugertillidsindikatoren_rank1, data = FORV1_og_NKN3_samlet[1:slut_række, ])
  
  # Gem R-squared, intercept og koefficient
  stabilitets_resultater[[length(stabilitets_resultater) + 1]] <- list(
    r_squared = summary(model)$r.squared,
    b0 = coef(model)[1],  # Intercept
    b1 = coef(model)[2]   # Koefficient
  )
}

# Beregn gennemsnit og standardafvigelse for R-squared
r_squared_values <- sapply(stabilitets_resultater, function(x) x$r_squared)
mean_r_squared <- mean(r_squared_values)
sd_r_squared <- sd(r_squared_values)

# Stabilitetsindikator
stabilitetsindikator <- mean_r_squared / sd_r_squared

cat("Gennemsnitlig R-squared:", mean_r_squared, "\n")
cat("Standardafvigelse af R-squared:", sd_r_squared, "\n")
cat("Stabilitetsindikator (R-squared gennemsnit / standardafvigelse):", stabilitetsindikator, "\n")


#Forsøg med vores FTI Rank 1
#Gennemsnitligt R-squared: 0.2868286  
#Standardafvigelse af R-squared: 0.0291334
#Stabilitetsindikator (R-squared gennemsnit / R-squared std. afvigelse): 9.845356  

#Forsøg med BAUM DI
#Gennemsnitligt R-squared: 0.4198537 
#Standardafvigelse af R-squared: 0.05882739
#Stabilitetsindikator (R-squared gennemsnit / R-squared std. afvigelse): 7.137044

# Opret tomme lister til at gemme R-squared værdier for hver periode
rsquared_liste <- list()

# Kør stabilitetstesten ved gradvist at reducere antallet af kvartaler
for (slut in 98:65) {
  # Udvælg data for den nuværende periode
  periode_data <- FORV1_og_NKN3_samlet[1:slut, ]
  
  
  # Byg modellen for den fundne indikator i perioden
  model <- lm(Forbrugets_årlig_udvikling ~ Forbrugertillidsindikatoren_DI, data = periode_data)
  
  # Beregn R-squared for perioden og gem det
  rsquared <- summary(model)$r.squared
  rsquared_liste[[length(rsquared_liste) + 1]] <- rsquared
}

# Konverter R-squared listen til en numerisk vektor for beregning af gennemsnit og standardafvigelse
rsquared_værdier <- unlist(rsquared_liste)

# Beregn gennemsnit og standardafvigelse for R-squared værdierne
rsquared_gennemsnit <- mean(rsquared_værdier, na.rm = TRUE)
rsquared_std_afvigelse <- sd(rsquared_værdier, na.rm = TRUE)

# Beregn stabilitetsindikator
stabilitetsindikator <- rsquared_gennemsnit / rsquared_std_afvigelse
}

# Udskriv resultater
cat("Gennemsnitligt R-squared:", rsquared_gennemsnit, "\n")
cat("Standardafvigelse af R-squared:", rsquared_std_afvigelse, "\n")
cat("Stabilitetsindikator (R-squared gennemsnit / R-squared std. afvigelse):", stabilitetsindikator, "\n")


