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
#---------------

#Opgave 1.4 – Forudsigelser med afsæt i jeres indikatorer
#Forudsig udviklingen i husholdningernes forbrugsudgift i 3. og 4. kvartal 2024. (Hint: brug jeres
#svar i opgave 2 fra OLA 2 sammen med jeres svar fra de forrige opgaver. Vær opmærksom på, om
#tallene fra forbrugerundersøgelsen for oktober er udgivet og I har den første måned i 4. kvartal)

{
# Opret en ny dataframe for at finde ud af værdi i rank 1 FTI 2024 Q3

# Træk kolonne 1 (TID) og de relevante spørgsmål: 1, 2, 3, 5, 6, 9, 11, 12
# Spørgsmål 1 svarer til kolonne 4 og frem i FORV1_wide
spm_kolonner.1 <- c(1, 6, 12, 14, 15)  # TID (kolonne 1) + spørgsmål (kolonner 4 og frem)

# Træk kun rækkerne 295 til 297
FORV1.4.2023.Q3 <- FORV1_wide[283:285, spm_kolonner.1]

# Omdan TID-kolonnen til datoformatet, hvor alle rækker sættes til starten af Q3 2024 (2024-07-01)
FORV1.4.2023.Q3$TID <- as.Date("2023-07-01")

# Beregn gennemsnittet af alle spørgsmål for at danne en samlet faktor
# Vi ekskluderer TID-kolonnen fra gennemsnitsberegningen
FTI_rank_1 <- rowMeans(FORV1.4.2023.Q3[, -1], na.rm = TRUE)  # Beregn gennemsnit for alle spørgsmål

# Opret en ny dataframe med TID og FTI rank 1
FORV1.4_quarter.2023.Q3 <- data.frame(
  `TID` = as.Date("2024-07-01"),  # Sæt TID til 2024-07-01
  `FTI rank 1` = mean(FTI_rank_1)  # Beregn gennemsnit af FTI rank 1 for hele kvartalet
)

# Udskriv resultatet for at kontrollere
print(FORV1.4_quarter.2023.Q3)

# Opret en ny dataframe for at finde ud af værdi i rank 1 FTI 2024 okt

# Træk kolonne 1 (TID) og de relevante spørgsmål: 1, 2, 3, 5, 6, 9, 11, 12
# Spørgsmål 1 svarer til kolonne 4 og frem i FORV1_wide
spm_kolonner.1 <- c(1, 6, 12, 14, 15)  # TID (kolonne 1) + spørgsmål (kolonner 4 og frem)

# Træk kun rækkerne 298
FORV1.4.2023.Q4 <- FORV1_wide[286:288, spm_kolonner.1]

# Omdan TID-kolonnen til datoformatet, hvor alle rækker sættes til starten af Q3 2024 (2024-07-01)
FORV1.4.2023.Q4$TID <- as.Date("2023-10-01")

# Beregn gennemsnittet af alle spørgsmål for at danne en samlet faktor
# Vi ekskluderer TID-kolonnen fra gennemsnitsberegningen
FTI_rank_1 <- rowMeans(FORV1.4.2023.Q4[, -1], na.rm = TRUE)  # Beregn gennemsnit for alle spørgsmål

# Opret en ny dataframe med TID og FTI rank 1
FORV1.4.2023.Q4 <- data.frame(
  `TID` = as.Date("2023-10-01"),  # Sæt TID til 2024-07-01
  `FTI rank 1` = mean(FTI_rank_1)  # Beregn gennemsnit af FTI rank 1 for hele kvartalet
)

# Udskriv resultatet for at kontrollere
print(FORV1.4.2023.Q4)

#TID FTI.rank.1
#1 2023-07-01   6.3

#TID FTI.rank.1
#1 2023-10-01   6.066667

#                  Estimate   Std. Error  t value   Pr(>|t|)
# (Intercept)      -1.7666831 0.42383660 -4.168312 6.720683e-05
# Samlet_indikator  0.2763107 0.03143346  8.790337 5.888157e-14


#1. DST forudsagte årlige udvikling af forbruget
#Hent koefficienterne fra model_DST
intercept_vores_FTI <- -1.7666831  # Intercept(b0)
coef_vores_FTI <- 0.2763107  # Koeficienten (b1)

#Værdien af Forbrugertillidsindikatoren_DST for 3. kvartal 2024
FTI_vores_2023Q3 <- 6.3

#Værdien af Forbrugertillidsindikatoren_DST for 4. kvartal 2024
FTI_vores_2023Q4 <- 6.066667

#Forudsig den årlige udvikling af forbruget Q3
forudsigelse_Q3 <- intercept_vores_FTI + coef_vores_FTI * FTI_vores_2023Q3

#Forudsig den årlige udvikling af forbruget Q4
forudsigelse_Q4 <- intercept_vores_FTI + coef_vores_FTI * FTI_vores_2023Q4

#Forudsigelsen:
print(forudsigelse_Q3)
print(forudsigelse_Q4)

#Vores model forudsiger en årlig vækst i forbruget på 1.559884% for 3. kvartal 2024.
#Vores model forudsiger en årlig vækst i forbruget på 0.7598015% for 4. kvartal 2024.

#Forbrug i 2023 Q3
forbrug_Q3 <- 286.8

#Forbrug i 2023 Q4
forbrug_Q4 <- 291.7

#Beregn forbruget i 2024 Q3
forudsigelse_Q3 <- forbrug_Q3 * (1 + forudsigelse_Q3 / 100)

#Beregn forbruget i 2024 Q4
forudsigelse_Q4 <- forbrug_Q4 * (1 + forudsigelse_Q4 / 100)

#Udskriv resultaterne
cat("Forbruget i 2023 Q3 (FTI rank 1):", forudsigelse_Q3, "\n")
cat("Forbruget i 2023 Q4 (FTI rank 1):", forudsigelse_Q4, "\n")

#Forbruget i 2023 Q3 (FTI rank 1): 286.7256
#Forbruget i 2023 Q4 (FTI rank 1): 291.4363  

}

# Opgave 1.5 - Sammenlign med en mikroøkonomisk indikator
{ #Find den bedste indikator, der alene består af mikroøkonomiske spørgsmål i 
  # forbrugertillidsundersøgelsen og sammenlign indikatoren med jeres tidligere svar i opgave 1.
  
# Mikro spg 1,2,5,9,10,11,12
  
# Vi lave subset til de rigtige spørgsmål samt forbrug
  # Udvælg de ønskede kolonner med kolonnenumre
  # Her antager vi, at de første fem kolonner ikke skal bruges og vi ønsker kolonnerne 1, 2, 3, 4, 5 samt 6, 7, 10, 14, 15, 16, og 17 fra mikrospørgsmålene
  
  # Lav subset af data med de ønskede kolonner
  opgave_1_5 <- FORV1_og_NKN3_samlet[, c(1, 3, 6, 7, 10, 14, 15, 16, 17)]
  
  
  # Opret en tom liste til at gemme de samlede indikatorer
  samlede_indikatorer_liste_1.5 <- list()
  kombinationer_liste_1.5 <- list()  # Gem kombinationerne af spørgsmål
  
  # Generer alle kombinationer af spørgsmål fra kolonne 7 og frem
  for (k in 1:(ncol(opgave_1_5) - 2)) {
    kombinationer_1.5 <- combn(3:ncol(opgave_1_5), k)
    for (i in 1:ncol(kombinationer_1.5)) {
      # Vælg de aktuelle spørgsmål baseret på kolonneindeks
      valgte_spørgsmål_1.5 <- opgave_1_5[, kombinationer_1.5[, i], drop = FALSE]
      
      # Beregn gennemsnittet af de valgte spørgsmål for at skabe en samlet indikator
      samlet_indikator_1.5 <- rowMeans(valgte_spørgsmål_1.5, na.rm = TRUE)
      
      # Gem den samlede indikator og kombinationen i listerne
      samlede_indikatorer_liste_1.5[[length(samlede_indikatorer_liste_1.5) + 1]] <- samlet_indikator_1.5
      kombinationer_liste_1.5[[length(kombinationer_liste_1.5) + 1]] <- kombinationer_1.5[, i]
    }
  }
  
  # Nu kan vi køre regressioner for hver af de samlede indikatorer
  model_results_1.5 <- list()
  
  for (j in 1:length(samlede_indikatorer_liste_1.5)) {
    # Hent den samlede indikator og den tilhørende kombination
    samlet_indikator_1.5 <- samlede_indikatorer_liste_1.5[[j]]
    kombination_1.5 <- kombinationer_liste_1.5[[j]]
    
    # Lav en data.frame med Y-variablen og den valgte samlede indikator "Test til videre brug"
    data_model_1.5 <- data.frame(
      Forbrugets_årlig_udvikling = opgave_1_5$Forbrugets_årlig_udvikling,
      Samlet_indikator_1.5 = samlet_indikator_1.5
    )
    
    # Byg modellen
    model_1.5 <- lm(Forbrugets_årlig_udvikling ~ Samlet_indikator_1.5, data = data_model_1.5)
    
    # Gem R-squared og kombination
    model_results_1.5[[length(model_results_1.5) + 1]] <- list(
      r_squared_1.5 = summary(model_1.5)$r.squared,
      kombination_1.5 = kombination_1.5,
      model_summary_1.5 = summary(model_1.5)
    )
  }
  
}  
  # Sorter resultaterne efter R-squared i faldende rækkefølge
  sorted_results_1.5 <- model_results_1.5[order(sapply(model_results_1.5, function(x) x$r_squared_1.5), decreasing = TRUE)]


