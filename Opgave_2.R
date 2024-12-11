# Opgave 2 - Principal Component Analysis

# Opgave 2.1 – PCA regression
{
# Lav en PCA regression på jeres data fra opgave 1, hvor Y er jeres årlige 
# realvækst i husholdningernes forbrugsudgift og X er alle de 12 spørgsmål fra 
# DST’s forbrugerforventningsundersøgelsen.

library(pls)

# Fit PCR model
pcr.fit <- pcr(Forbrugets_årlig_udvikling ~
                 `Familiens økonomiske situation i dag, sammenlignet med for et år siden` +
                 `Familiens økonomiske  situation om et år, sammenlignet med i dag` +
                 `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` +
                 `Danmarks økonomiske situation om et år, sammenlignet med i dag` +
                 `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` +
                 `Priser i dag, sammenlignet med for et år siden` +
                 `Priser om et år, sammenlignet med i dag` +
                 `Arbejdsløsheden om et år, sammenlignet med i dag` +
                 `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.` +
                 `Anser det som fornuftigt at spare op i den nuværende økonomiske situation` +
                 `Regner med at kunne spare op i de kommende 12 måneder` +
                 `Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`,
               data = FORV1_og_NKN3_samlet, scale = TRUE, validation = "CV")

# Summary of PCR model
summary(pcr.fit)

# Extract loadings
loadings.pcr.fit <- pcr.fit$loadings

# Calculate sum of squared loadings for the first 12 components
w.indicators.1 <- loadings.pcr.fit[1:12]^2                     
sum(w.indicators.1)
}
w.indicators.1

# Opgave 2.2 - De vigtigste indikatorer
{
# Hvilke 3 spørgsmål i forbrugerforventningsundersøgelsen har de højeste vægte? 
# Diskutér om spørgsmålene giver analytisk mening.
# De mest vægtede spørgsmål er 1,2 og 9.
  
  # De tre spg giver mening, da de også indgår i både DST's og DI's FTI.
  
}

# Opgave 2.3 - Forudsig forbruget
{
# Med afsæt i jeres vægte fra opgave 2.1 skal I beregne den årlige realvækst i 
# husholdningernes forbrugsudgift for 3. kvartal 2024. Hvad er årsagen til I 
# ikke helt tror på jeres beregnede årlige realvækst i husholdningernes 
# forbrugsudgift for 3. kvartal 2023? Hvad skyldes det underlige resultat?
  {  
# Vi starter med at lave et subset på forbruget og spg 9.
Opgave_2.3 <- FORV1_og_NKN3_samlet[,c(1,3,6:17)]

  # Opret en ny dataframe for at finde ud af værdi i rank 1 FTI 2024 Q3
  
  # Træk kun rækkerne 295 til 297
  FORV1.2.3.2024.Q3 <- FORV1_wide[295:297, c(1,4:ncol(FORV1_wide))]
  
  # Omdan TID-kolonnen til datoformatet, hvor alle rækker sættes til starten af Q3 2024 (2024-07-01)
  FORV1.2.3.2024.Q3$TID <- as.Date("2024-07-01")
  
  # Beregn gennemsnittet af alle spørgsmål for at danne en samlet faktor
  # Vi ekskluderer TID-kolonnen fra gennemsnitsberegningen
  FTI.2.3 <- rowMeans(FORV1.2.3.2024.Q3[, -1], na.rm = TRUE)  # Beregn gennemsnit for alle spørgsmål
  
  # Opret en ny dataframe med TID og FTI rank 1
  FORV1.2.3_quarter.2024.Q3 <- data.frame(
    `TID` = as.Date("2024-07-01"),  # Sæt TID til 2024-07-01
    `FTI 2.3` = mean(FTI.2.3)  # Beregn gennemsnit af FTI rank 1 for hele kvartalet
  )
  
  # Udskriv resultatet for at kontrollere
  print(FORV1.2.3_quarter.2024.Q3)
  
  # Opret en ny dataframe for at finde ud af værdi i rank 1 FTI 2024 okt

  # Træk kun rækkerne 298
  FORV1.2.3.okt <- FORV1_wide[298, c(1,4:ncol(FORV1_wide))]
  
  # Omdan TID-kolonnen til datoformatet, hvor alle rækker sættes til starten af Q3 2024 (2024-07-01)
  FORV1.2.3.okt$TID <- as.Date("2024-10-01")
  
  # Beregn gennemsnittet af alle spørgsmål for at danne en samlet faktor
  # Vi ekskluderer TID-kolonnen fra gennemsnitsberegningen
  FTI.2.3.okt <- rowMeans(FORV1.2.3.okt[, -1], na.rm = TRUE)  # Beregn gennemsnit for alle spørgsmål
  
  # Opret en ny dataframe med TID og FTI rank 1
  FORV1.2.3_quarter.2024.okt <- data.frame(
    `TID` = as.Date("2024-10-01"),  # Sæt TID til 2024-07-01
    `FTI 2.3.okt` = mean(FTI.2.3.okt)  # Beregn gennemsnit af FTI rank 1 for hele kvartalet
  )
}
  
  # Udskriv resultatet for at kontrollere
  print(FORV1.2.3_quarter.2024.Q3)
  
  # Udskriv resultatet for at kontrollere
  print(FORV1.2.3_quarter.2024.okt)
  {
  # Antag at Opgave_2.3 er dit dataframe og w.indicators.1 er givet
  # Vælg de relevante kolonner til det vægtede gennemsnit
  relevante_kolonner <- Opgave_2.3[, 3:14]
  
  # Beregn det vægtede gennemsnit
  Opgave_2.3$FTI_vægtet <- rowSums(relevante_kolonner * w.indicators.1)
  
  # Tjek om den nye kolonne er tilføjet korrekt
  head(Opgave_2.3)
  
  # Udfør lineær regression
  model <- lm(Forbrugets_årlig_udvikling ~ FTI_vægtet, data = Opgave_2.3)
  
  # Vis summary af modellen
  summary(model)
  }
# Coefficients:
#             Estimate  Std. Error t value Pr(>|t|)    
# (Intercept)  -0.4290     0.4130  -1.039    0.302    
# FTI_vægtet    0.1794     0.0315   5.696 1.34e-07 ***
  
  # TID FTI.2.3
  # 1 2024-07-01      11
  
  # TID FTI.2.3.okt
  # 1 2024-10-01         10.43333
  
  #1. DST forudsagte årlige udvikling af forbruget
  #Hent koefficienterne fra model_DST
  intercept_vægtet_FTI <- -0.4290  # Intercept(b0)
  coef_vægtet_FTI <- 0.1794  # Koeficienten (b1)
  
  #Værdien af Forbrugertillidsindikatoren_DST for 3. kvartal 2024
  FTI_vægtet_2024Q3 <- 11
  
  #Værdien af Forbrugertillidsindikatoren_DST for 4. kvartal 2024
  FTI_vægtet_2024Q4 <- 10.43333
  
  #Forudsig den årlige udvikling af forbruget Q3
  forudsigelse_vægtet_Q3 <- intercept_vægtet_FTI + coef_vægtet_FTI * FTI_vægtet_2024Q3
  
  #Forudsig den årlige udvikling af forbruget Q4
  forudsigelse_vægtet_Q4 <- intercept_vægtet_FTI + coef_vægtet_FTI * FTI_vægtet_2024Q4
  
  #Forudsigelsen:
  print(forudsigelse_vægtet_Q3)
  print(forudsigelse_vægtet_Q4)
  
  #Vores model forudsiger en årlig vækst i forbruget på 1.5444% for 3. kvartal 2024.
  #Vores model forudsiger en årlig vækst i forbruget på 1.442739% for 4. kvartal 2024.
  
  #Forbrug i 2024 Q2
  forbrug_Q2 <- 290.2
  
  #Beregn forbruget i 2024 Q3
  forudsigelse_vægtet_realvækst_Q3 <- forbrug_Q2 * (1 + forudsigelse_vægtet_Q3 / 100)
  
  #Beregn forbruget i 2024 Q4
  forudsigelse_vægtet_realvækst_Q4 <- forudsigelse_vægtet_realvækst_Q3 * (1 + forudsigelse_vægtet_Q4 / 100)
  
  #Udskriv resultaterne
  cat("Forbruget i 2024 Q3 (FTI vægtet):", forudsigelse_vægtet_realvækst_Q3, "\n")
  cat("Forbruget i 2024 Q4 (FTI vægtet):", forudsigelse_vægtet_realvækst_Q4, "\n")
  
  #Forbruget i 2024 Q3 (FTI vægtet): 294.6818 
  #Forbruget i 2024 Q4 (FTI vægtet): 298.9333  
  
  
  
}

# Opgave 2.4 – Forudsigelser fra DI
# Hvad forventede DI den årlige realvækst i husholdningernes forbrugsudgift er i
# 2016? (se artiklen: ”Forbruget fortsætter fremgangen i 2016”) 
# Hvad endte den årlige realvækst i husholdningernes forbrugsudgift med reelt at være?


(4.2+3.9+2.2+2.8)/4
3.275

