# Opgave 3 – Julehandel i 2024

# Opgave 3.1 – Forudsigelse
# Lav en Machine Learning model, der med afsæt i DST’s forbrugertillidsindikator
# , kan forudsige om julehandlen i 2024 er større end i 2023.

NKN3_3_1 <- Færdig_NKN3

# Ny kolonne med NA værdier
NKN3_3_1$Op_Ned <- NA

# Funktion til at afgøre om der er stigning eller fald i den årlige udvikling
Op_Ned_Funktion <- function(column){
  diff <- column - lag(column, 4) 
  ifelse(column>= 0, "op", "ned")
}

# Anvend funktionen på data
NKN3_3_1$Op_Ned <- Op_Ned_Funktion(NKN3_3_1$Forbrugets_årlig_udvikling)


#Aggregering fra måneder til kvartaler
# Konverter måneder til kvartalers startdatoer i formatet "YYYY-MM-DD"
FORV1_3.2$TID <- as.Date(cut(as.Date(FORV1_3.2[[1]]), "quarter"))

# Udvælg de relevante kolonner
selected_columns <- FORV1_3.2[, c(3,4,5,6,7)]

# Opret en ny dataframe med gennemsnit pr. kvartal
FORV1_3.2 <- aggregate(selected_columns, by = list(FORV1_3.2$TID), FUN = mean, na.rm = TRUE)

# Omdøb kolonnen Group.1 til TID, da det nu er tidspunktet for kvartaler
colnames(FORV1_3.2)[1] <- "TID"

Samlet3.2 <- merge(
  FORV1_3.2[, c("TID","Familiens økonomiske situation i dag, sammenlignet med for et år siden",
                "Familiens økonomiske  situation om et år, sammenlignet med i dag" ,
                "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                "Danmarks økonomiske situation om et år, sammenlignet med i dag"  ,
                "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket")], 
  NKN3_3_1[, c("TID", "Op_Ned")], # Udvælg kolonner fra anden dataframe
  by.x = "TID", # Kolonnen i FORV1_færdig
  by.y = "TID" # Kolonnen i Færdig_NKN3
)

# Konverter Op_Ned til binær kode
# Her antager vi at "op" skal være 1 og "ned" skal være 0
Samlet3.2$Op_Ned_binær <- ifelse(Samlet3.2$Op_Ned == "op", 1, 0)

# Multiple logistisk regression
logistisk_model <- glm(
  Op_Ned_binær ~ `Familiens økonomiske situation i dag, sammenlignet med for et år siden` +
    `Familiens økonomiske  situation om et år, sammenlignet med i dag` +
    `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` +
    `Danmarks økonomiske situation om et år, sammenlignet med i dag`+
    `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`, 
  data = Samlet3.2, 
  family = binomial
)

# Vis opsummering af modellen
summary(logistisk_model)

# Funktion til at beregne sandsynlighed manuelt baseret på koefficienter fra modellen
manual_logistic_prediction <- function(X1, X2, X3, X4, X5, model) {
  
  # Træk koefficienterne fra modellen
  intercept <- coef(model)[1]
  coef_X1 <- coef(model)[2]
  coef_X2 <- coef(model)[3]
  coef_X3 <- coef(model)[4]
  coef_X4 <- coef(model)[5]
  coef_X5 <- coef(model)[6]
  
  # Beregn logit-værdien
  logit_p <- intercept + (coef_X1 * X1) + (coef_X2 * X2) + (coef_X3 * X3) + (coef_X4 * X4) + (coef_X5 * X5)
  
  # Konverter logit-værdien til sandsynlighed
  p <- 1 / (1 + exp(-logit_p))
  
  # Returner sandsynligheden
  return(p)
}

# Beregning af seneste 10 års udvikling i DST's FTI (Lavet i Excel)
{ U.DST.FTI.2013_23 <- FORV1_3.2[53:96,]
  
  # Dataframe: U.DST.FTI.2013_23
  # Variabler: Q3 og Q4 gennemsnit
  q3_means <- c()  # Tom vektor til Q3 gennemsnit
  q4_means <- c()  # Tom vektor til Q4 gennemsnit
  
  # Loop gennem kolonnerne (bortset fra den første kolonne "TID")
  for (col in 2:ncol(U.DST.FTI.2013_23)) {
    # Q3: Hver tredje række startende fra 3 (3, 6, 9, ...)
    q3_rows <- seq(3, nrow(U.DST.FTI.2013_23), by = 4)
    q3_mean <- mean(U.DST.FTI.2013_23[q3_rows, col], na.rm = TRUE)
    q3_means <- c(q3_means, q3_mean)
    
    # Q4: Hver tredje række startende fra 4 (4, 7, 10, ...)
    q4_rows <- seq(4, nrow(U.DST.FTI.2013_23), by = 4)
    q4_mean <- mean(U.DST.FTI.2013_23[q4_rows, col], na.rm = TRUE)
    q4_means <- c(q4_means, q4_mean)
  }
  
  # Resultater som dataframe for let sammenligning
  mean_comparison <- data.frame(
    Kolonne = names(U.DST.FTI.2013_23)[2:6],
    Q3_Gennemsnit = q3_means,
    Q4_Gennemsnit = q4_means
  )
  
  print(mean_comparison)
  
(q4_means-q3_means)/q3_means*100

}



#Sæt værdier for X1, X2, X3, X4 "Forkert værdi"
#X1_value <- -8.9  # Familiens økonomiske situation i dag, sammenlignet med for et år siden
#X2_value <- -1  # Familiens økonomiske  situation om et år, sammenlignet med i dag
#X3_value <- -9.8  # Danmarks økonomiske situation i dag, sammenlignet med for et år siden
#X4_value <- -8.5  # Danmarks økonomiske situation om et år, sammenlignet med i dag
#X5_value <- -16.1  # Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket

# Gns værdier siden 2013 Q4 (- 2022 Q4)
#X1_value <- 2.03  # Familiens økonomiske situation i dag, sammenlignet med for et år siden
#X2_value <- 10.66  # Familiens økonomiske  situation om et år, sammenlignet med i dag
#X3_value <- 1.4  # Danmarks økonomiske situation i dag, sammenlignet med for et år siden
#X4_value <- 2.56  # Danmarks økonomiske situation om et år, sammenlignet med i dag
#X5_value <- -10.98  # Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket

# Gns værdier siden 2013 Q4 (- 2020 Q4 + 2022 Q4)
X1_value <- 1.62  # Familiens økonomiske situation i dag, sammenlignet med for et år siden
X2_value <- 9.54  # Familiens økonomiske  situation om et år, sammenlignet med i dag
X3_value <- 4.36  # Danmarks økonomiske situation i dag, sammenlignet med for et år siden
X4_value <- 3.43  # Danmarks økonomiske situation om et år, sammenlignet med i dag
X5_value <- -10.33  # Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket

# Beregn sandsynligheden for "op"
sandsynlighed_op <- manual_logistic_prediction(X1_value, X2_value, X3_value, X4_value, X5_value, logistisk_model)

# Udskriv resultatet
cat("Sandsynlighed for 'op':", sandsynlighed_op, "\n")

# Bestem om udfaldet er "op" eller "ned"
if (sandsynlighed_op > 0.5) {
  cat("Forudsigelse: Op\n")
} else {
  cat("Forudsigelse: Ned\n")
}

#Sandsynlighed for 'op': 0.9326499  
#Forudsigelse: Op


# Opgave 3.2 – Validering af model 
# Lav en vurdering af validiteten af jeres model fra opgave 3.1.

# Forudsig sandsynligheder for hele datasæt (DI) 
Samlet3.2$predicted_prob_DST <- manual_logistic_prediction(
  Samlet3.2$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`,
  Samlet3.2$`Familiens økonomiske  situation om et år, sammenlignet med i dag`,
  Samlet3.2$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`,
  Samlet3.2$`Danmarks økonomiske situation om et år, sammenlignet med i dag`,
  Samlet3.2$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
  logistisk_model
)

# Round funktion på kolonne
Samlet3.2$predicted_prob_DST <- round(Samlet3.2$predicted_prob_DST,2)

# Konverter sandsynlighederne til binære værdier for den oprindelige model
Samlet3.2$predicted_op_ned_DST <- ifelse(Samlet3.2$predicted_prob_DST > 0.5, 1, 0)

# Confusion matrix for den oprindelige model (DI)
confusion_matrix_DST <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_DST)
cat("Confusion Matrix for DST modellen:\n")

# Tilføj labels til confusion matrixen
rownames(confusion_matrix_DST) <- c("Faktisk: Ned (0)", "Faktisk: Op (1)")
colnames(confusion_matrix_DST) <- c("Forudsagt: Ned (0)", "Forudsagt: Op (1)")

print(confusion_matrix_DST)

# Funktion til beregning af accuracy, precision og recall
beregn_metrikker <- function(confusion_matrix) {
  TP <- confusion_matrix[2, 2]
  TN <- confusion_matrix[1, 1]
  #FP <- confusion_matrix[1, 2]
  #FN <- confusion_matrix[2, 1]
  
  accuracy <- (TP + TN) / sum(confusion_matrix)
  #precision <- TP / (TP + FP)
  #recall <- TP / (TP + FN)
  
  cat("Accuracy:", round(accuracy * 100, 2), "%\n")
  #cat("Precision:", round(precision * 100, 2), "%\n")
  #cat("Recall:", round(recall * 100, 2), "%\n")
}

# Beregn metrikker for DI modellen
cat("\nMetrikker for DST modellen:\n")
beregn_metrikker(confusion_matrix_DST)

# Roc - Kurve

# Installer nødvendige pakker hvis du ikke allerede har dem
install.packages("pROC")

library(pROC)

# Lav en ROC-kurve for den oprindelige model (DI)
roc_Dst <- roc(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_prob_DST)


# Plot ROC-kurven for den oprindelige model uden akser og etiketter
plot(roc_Dst, col = "blue", main = "ROC-kurve for DST", print.auc = TRUE,
     lwd = 2, axes = FALSE, xlab = "", ylab = "")

# Tilføj akser manuelt uden etiketter
axis(1, cex.axis = 1.5)  # X-aksen (False Positive Rate)
axis(2, cex.axis = 1.5)  # Y-aksen (True Positive Rate)

# Tilføj brugerdefinerede akseetiketter med mtext()
mtext("False Positive Rate (1 - Specificity)", side = 1, line = 3, cex = 1.5)  # X-aksen tekst
mtext("True Positive Rate (Sensitivity)", side = 2, line = 3, cex = 1.5)      # Y-aksen tekst



#_______________________________________________


#3.4 Potentielle forbedringer af model
{
  
  # Baseline Cutoff på 0.5
  Samlet3.2$predicted_op_ned_baseline <- ifelse(Samlet3.2$predicted_prob_DI > 0.5, 1, 0)
  
  # Cutoff på 0.3
  Samlet3.2$predicted_op_ned_cutoff_03 <- ifelse(Samlet3.2$predicted_prob_DI > 0.45, 1, 0)
  
  # Cutoff på 0.7
  Samlet3.2$predicted_op_ned_cutoff_07 <- ifelse(Samlet3.2$predicted_prob_DI > 0.55, 1, 0)
  
  # Confusion matrix for baseline (cutoff = 0.5)
  confusion_matrix_baseline <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_baseline)
  print("Confusion Matrix - Baseline (cutoff = 0.5):")
  print(confusion_matrix_baseline)
  
  # Confusion matrix for cutoff = 0.4
  confusion_matrix_cutoff_03 <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_cutoff_03)
  print("Confusion Matrix - Cutoff 0.45:")
  print(confusion_matrix_cutoff_03)
  
  # Confusion matrix for cutoff = 0.6
  confusion_matrix_cutoff_07 <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_cutoff_07)
  print("Confusion Matrix - Cutoff 0.55:")
  print(confusion_matrix_cutoff_07)
  
  # Beregn accuracy for de forskellige cutoffs
  accuracy_baseline <- sum(diag(confusion_matrix_baseline)) / sum(confusion_matrix_baseline)
  accuracy_cutoff_03 <- sum(diag(confusion_matrix_cutoff_03)) / sum(confusion_matrix_cutoff_03)
  accuracy_cutoff_07 <- sum(diag(confusion_matrix_cutoff_07)) / sum(confusion_matrix_cutoff_07)
  
  # Print accuracy for hver cutoff
  cat("Accuracy - Baseline (cutoff 0.5):", accuracy_baseline, "\n")
  cat("Accuracy - Cutoff 0.45:", accuracy_cutoff_03, "\n")
  cat("Accuracy - Cutoff 0.55:", accuracy_cutoff_07, "\n")
  
}