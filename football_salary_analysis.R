library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(writexl)
library(PerformanceAnalytics)
library(corrplot)
library(car)
library(leaps)
library(lmtest)
library(nlme)
library(AER)
library(sandwich)
library(dplyr)
library(outliers)

library(readxl)
JDD_Salaires_rapport <- read_excel("C:/Users/arthu/OneDrive/Bureau/Master/Économétrie linéaire avancée/Projet_métrie/JDD_Salaires_rapport.xlsx")
View(JDD_Salaires_rapport)

JDD_Salaires_rapport$PIB <- as.numeric(gsub(",", ".", gsub(" ", "", JDD_Salaires_rapport$PIB))) # car prblm avec les données de PIB 
JDD_Salaires_rapport$UEFA <- as.numeric(gsub(",", ".", gsub(" ", "", JDD_Salaires_rapport$UEFA))) # car prblm avec les données de UEFA 


library(tidyverse)
library(ggplot2)
library(gridExtra)

# Analyse descriptive univariée ----

## Variables qualitatives ----

### camembert postes ----
couleur <- c("ATTAQUANT" = "pink", "DEFENSEUR" = "lightgreen", "MILIEU" = "lightblue")


nbr_attaquant <- sum(JDD_Salaires_rapport$ATTAQUANT, na.rm = TRUE)
nbr_defenseur <- sum(JDD_Salaires_rapport$DEFENSEUR, na.rm = TRUE)
nbr_milieu <- sum(JDD_Salaires_rapport$MILIEU, na.rm = TRUE)


data_postes <- data.frame(
 postes = c("ATTAQUANT", "DEFENSEUR", "MILIEU"),
 n = c(nbr_attaquant, nbr_defenseur, nbr_milieu)
)

data_postes$pourcentage <- (data_postes$n / (nbr_attaquant + nbr_defenseur + nbr_milieu) )*100


ggplot(data_postes) +
 aes(x = "", y = n, fill = postes) +
 geom_bar(stat = "identity", width = 1) +
 coord_polar("y") +
 scale_fill_manual(values = couleur) +
 theme_void() +
 labs(fill = "Répartition des différents postes")+
 geom_text(aes(label = paste0(round(pourcentage, 1), "%")),
           position = position_stack(vjust = 0.5))

#### pour afficher la répartition des tout les différents postes ----


#table(JDD_Salaires_rapport$Poste)


data_poto <- data.frame(
 postes = c("Ailier droit",       "Ailier gauche",      "Arrière droit",      "Arrière gauche",     "Avant-centre",      
            "Défenseur central",  "Deuxième attaquant","Milieu central",     "Milieu défensif",    "Milieu droit",      
            "Milieu gauche",      "Milieu offensif" ),
 n = c(45, 43, 26, 29, 73, 89, 4, 68, 44, 2, 2, 43)
)

data_poto$pourcentage <- (data_poto$n / 468 )*100


ggplot(data_poto) +
 aes(x = "", y = n, fill = postes) +
 geom_bar(stat = "identity", width = 1) +
 coord_polar("y") +
 theme_void() +
 labs(fill = "Répartition des différents postes")+
 geom_text(aes(label = paste0(round(pourcentage, 1), "%")),
           position = position_stack(vjust = 0.5))




### camembert Droitier ----
couleurs <- c("Droitier" = "orange", "Gaucher" = "lightblue")


nbr_droitier <- sum(JDD_Salaires_rapport$DROITIER, na.rm = TRUE)
nbr_gaucher <- (nbr_attaquant + nbr_defenseur + nbr_milieu) - nbr_droitier


data_droitier <- data.frame(
 pied = c("Droitier", "Gaucher"),
 n = c(nbr_droitier, nbr_gaucher)
)

data_droitier$pourcentage <- (data_droitier$n / (nbr_attaquant + nbr_defenseur + nbr_milieu) )*100


ggplot(data_droitier) +
 aes(x = "", y = n, fill = pied) +
 geom_bar(stat = "identity", width = 1) +  
 scale_fill_manual(values = couleurs) +     
 coord_polar("y") +                        
 theme_light() +
 labs(fill = "Pied dominant")+
 geom_text(aes(label = paste0(round(pourcentage, 1), "%")),
           position = position_stack(vjust = 0.5))


### camembert TOP5 ----
colour <- c("Oui" = "gold", "Non" = "grey")


nbre_5 <- sum(JDD_Salaires_rapport$TOP5, na.rm = TRUE)
nbre <- (nbr_attaquant + nbr_defenseur + nbr_milieu) - nbre_5


data_TOP5 <- data.frame(
 TOP5 = c("Oui", "Non"),
 n = c(nbre_5, nbre)
)

data_TOP5$pourcentage <- (data_TOP5$n / (nbr_attaquant + nbr_defenseur + nbr_milieu) )*100


ggplot(data_TOP5) +
 aes(x = "", y = n, fill = TOP5) +
 geom_bar(stat = "identity", width = 1) +  
 scale_fill_manual(values = colour) +     
 coord_polar("y") +                        
 theme_light() +
 labs(fill = "Joueurs faisant partie du TOP5")+
 geom_text(aes(label = paste0(round(pourcentage, 1), "%")),
           position = position_stack(vjust = 0.5))




## Variables quantitatives ----



#Salaire
sort(JDD_Salaires_rapport$SALAIRE, decreasing = TRUE) 
order(JDD_Salaires_rapport$SALAIRE, decreasing = TRUE)

JDD <- JDD_Salaires_rapport[-c(296),] #la valeur 100 est trop élevée pour une bonne analyse graphique

JDD |>
 ggplot() +
 aes(x = SALAIRE) +
 geom_histogram(bins = 30, binwidth = 0.66, fill = "brown") +
 theme_classic()   

#Age
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = AGE) +
 geom_histogram(bins = 30, binwidth = 1, fill = "brown") +
 theme_classic() 
#répartition gaussienne

#But
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = BUT) +
 geom_histogram(bins = 30, binwidth = 1.5, fill = "brown") +
 theme_classic() 


#STAT
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = STAT) +
 geom_histogram(bins = 30, binwidth = 0.075, fill = "brown") +
 theme_classic()

#MATCH
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = MATCH) +
 geom_histogram(bins = 50, binwidth = 1, fill = "brown") +
 theme_classic()

#NATION
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = NATION) +
 geom_histogram(bins = 50, binwidth = 5, fill = "brown") +
 theme_classic()

#LDC
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = LDC) +
 geom_histogram(bins = 50, binwidth = 5, fill = "brown") +
 theme_classic()

#INSTA

sort(JDD_Salaires_rapport$INSTA, decreasing = TRUE) 
order(JDD_Salaires_rapport$INSTA, decreasing = TRUE)

JDD_insta <- JDD_Salaires_rapport[-c(296,   4,   3,  88),] # même chose que pour salaires

JDD_insta |>
 ggplot() +
 aes(x = INSTA) +
 geom_histogram(bins = 30, binwidth = 1.5, fill = "brown") +
 theme_classic()

#BLESSURE
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = BLESSURE) +
 geom_histogram(bins = 60, binwidth = 3, fill = "brown") +
 theme_classic()

#xAG
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = xAG) +
 geom_histogram(bins = 15, binwidth = 0.66, fill = "brown") +
 theme_classic()

#VALEUR
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = VALEUR) +
 geom_histogram(bins = 300, binwidth = 5, fill = "brown") +
 theme_classic()

#CONTRAT
g17 <- JDD_Salaires_rapport |>
 ggplot() +
 aes(x = CONTRAT) +
 geom_histogram(bins = 50, binwidth = 1, fill = "brown") +
 theme_classic()
#répartition gaussienne

#CLASSEMENT
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = CLASSEMENT) +
 geom_histogram(bins = 30, binwidth = 1, fill = "brown") +
 theme_classic()

#PrgR
sort(JDD_Salaires_rapport$PrgR, decreasing = TRUE) 
order(JDD_Salaires_rapport$PrgR, decreasing = TRUE)
JDD_PrgR <- JDD_Salaires_rapport[-c(6, 126),]

JDD_PrgR |>
 ggplot() +
 aes(x = PrgR) +
 geom_histogram(bins = 100, binwidth = 5, fill = "brown") +
 theme_classic()

#PIB

JDD_Salaires_rapport |>
 ggplot(aes(x = PIB)) +
 geom_histogram(bins = 20, fill = "brown") +
 theme_classic() 


#JAUNE
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = JAUNE) +
 geom_histogram(bins = 30, binwidth = 1, fill = "brown") +
 theme_classic()

#ROUGE
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = ROUGE) +
 geom_histogram(bins = 3, binwidth = 1, fill = "brown") +
 theme_classic()

#PrgC
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = PrgC) +
 geom_histogram(bins = 200, binwidth = 5, fill = "brown") +
 theme_classic()

#UEFA
JDD_Salaires_rapport |>
 ggplot() +
 aes(x = UEFA) +
 geom_histogram(bins = 20, binwidth = 0.5, fill = "brown") +
 theme_classic()



### On affiche les graphs ----

gPIB <- ggplot(JDD_Salaires_rapport, aes(x = PIB)) + geom_histogram(bins = 20, fill = "brown") + theme_classic() 
g1 <- ggplot(JDD, aes(x = SALAIRE)) + geom_histogram(bins = 30, binwidth = 0.66, fill = "brown") + theme_classic()
g2 <- ggplot(JDD_Salaires_rapport, aes(x = AGE)) + geom_histogram(bins = 30, binwidth = 1, fill = "brown") + theme_classic()
g3 <- ggplot(JDD_Salaires_rapport, aes(x = BUT)) + geom_histogram(bins = 30, binwidth = 1.5, fill = "brown") + theme_classic()
g4 <- ggplot(JDD_Salaires_rapport, aes(x = STAT)) + geom_histogram(bins = 30, binwidth = 0.075, fill = "brown") + theme_classic()
g5 <- ggplot(JDD_Salaires_rapport, aes(x = MATCH)) + geom_histogram(bins = 50, binwidth = 1, fill = "brown") + theme_classic()
g6 <- ggplot(JDD_Salaires_rapport, aes(x = NATION)) + geom_histogram(bins = 50, binwidth = 5, fill = "brown") + theme_classic()
g7 <- ggplot(JDD_Salaires_rapport, aes(x = LDC)) + geom_histogram(bins = 50, binwidth = 5, fill = "brown") + theme_classic()
g8 <- ggplot(JDD_insta, aes(x = INSTA)) + geom_histogram(bins = 30, binwidth = 1.5, fill = "brown") + theme_classic()


g9 <- ggplot(JDD_Salaires_rapport, aes(x = BLESSURE)) + geom_histogram(bins = 60, binwidth = 3, fill = "brown") + theme_classic()
g10 <- ggplot(JDD_Salaires_rapport, aes(x = xAG)) + geom_histogram(bins = 15, binwidth = 0.66, fill = "brown") + theme_classic()
g11 <- ggplot(JDD_Salaires_rapport, aes(x = VALEUR)) + geom_histogram(bins = 300, binwidth = 5, fill = "brown") + theme_classic()
g12 <- ggplot(JDD_Salaires_rapport, aes(x = CLASSEMENT)) + geom_histogram(bins = 30, binwidth = 1, fill = "brown") + theme_classic()
g13 <- ggplot(JDD_Salaires_rapport, aes(x = PrgR)) + geom_histogram(bins = 100, binwidth = 5, fill = "brown") + theme_classic()
g14 <- ggplot(JDD_Salaires_rapport, aes(x = JAUNE)) + geom_histogram(bins = 30, binwidth = 1, fill = "brown") + theme_classic()
g15 <- ggplot(JDD_Salaires_rapport, aes(x = ROUGE)) + geom_histogram(bins = 3, binwidth = 1, fill = "brown") + theme_classic()
g16 <- ggplot(JDD_Salaires_rapport, aes(x = PrgC)) + geom_histogram(bins = 200, binwidth = 5, fill = "brown") + theme_classic()
g18 <- ggplot(JDD_Salaires_rapport, aes(x = MOYENNE_CONTRAT)) + geom_histogram(bins = 15, binwidth = 0.5, fill = "brown") + theme_classic()
g19 <- ggplot(JDD_Salaires_rapport, aes(x = UEFA)) + geom_histogram(bins = 200, binwidth = 15, fill = "brown") + theme_classic()


grid.arrange(g2, g3, g4, g5, g6, g7, g8,g9, gPIB, ncol = 3)  # Pour afficher les graphiques sur une même page
grid.arrange(g10, g11, g12, g13, g14, g15, g16, g17,g18, g19, ncol = 3) # en 2 fois car trop de graphiques



### Création tableau  ----
library(writexl)

tabl_univarie <- list()
for (variable in names(JDD_Salaires_rapport)) {
 stats <- summary(JDD_Salaires_rapport[[variable]])
 stats <- as.vector(stats)   # On convertit en vecteur pour éviter des problèmes de structure (pas obligatoire)
 tabl_univarie[[variable]] <- stats
}

# on transforme la liste en un data.frame et on  transpose pour aligner les lignes et colonnes
tabl_univarie <- as.data.frame(do.call(rbind, tabl_univarie))

colnames(tabl_univarie) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")

tabl_univarie$Variable <- rownames(tabl_univarie)

head(tabl_univarie) # il faut qu'on enlève les qualitatives

#write_xlsx(tabl_univarie, "C:/Users/arthu/OneDrive/Bureau/Master/summary_stat_instrument.xlsx")

# j'ai oublié les ecarts-type
sd_val <- sapply(JDD_Salaires_rapport, function(x) sd(x, na.rm = TRUE))

sd_df <- data.frame(Variable = names(sd_val), Standard_Deviation = sd_val)

print(sd_df) # a mettre dans le tableau


### Valeurs atypiques ----

library(outliers)

#### Boîte à moustache ----

boxplot(JDD_Salaires_rapport$SALAIRE, xlab="Salaires")
boxplot(JDD$SALAIRE, xlab="Salaires") # sachant que la valeur 100 a été enlevé car moche sinon
boxplot(JDD_Salaires_rapport$AGE, xlab="Age")
boxplot(JDD_Salaires_rapport$BUT, xlab="But")
boxplot(JDD_Salaires_rapport$STAT, xlab="Statistiques")
boxplot(JDD_Salaires_rapport$MATCH, xlab="Match")
boxplot(JDD_Salaires_rapport$NATION, xlab="Nation")
boxplot(JDD_Salaires_rapport$INSTA, xlab="INSTA")
boxplot(JDD_Salaires_rapport$BLESSURE, xlab="BLESSURE")
boxplot(JDD_Salaires_rapport$xAG, xlab="xAG")
boxplot(JDD_Salaires_rapport$CONTRAT, xlab="Contrat")
boxplot(JDD_Salaires_rapport$CLASSEMENT, xlab="CLASSEMENT")
boxplot(JDD_Salaires_rapport$PrgR, xlab="PrgR")
boxplot(JDD_Salaires_rapport$PrgC, xlab="PrgC")
boxplot(JDD_Salaires_rapport$PIB, xlab="PIB")
boxplot(JDD_Salaires_rapport$JAUNE, xlab="JAUNE")
boxplot(JDD_Salaires_rapport$ROUGE, xlab="ROUGE")
boxplot(JDD_Salaires_rapport$VALEUR, xlab="VALEUR")
ggplot(JDD_Salaires_rapport, aes(x = "", y = MOYENNE_CONTRAT)) + geom_boxplot() + xlab("MOYENNE_CONTRAT")
ggplot(JDD_Salaires_rapport, aes(x = "", y = UEFA)) + geom_boxplot() + xlab("UEFA")



b1 <- ggplot(JDD, aes(x = "", y = SALAIRE)) + geom_boxplot() + xlab("Salaires")
b2 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = AGE)) + geom_boxplot() + xlab("Age")
b3 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = BUT)) + geom_boxplot() + xlab("But")
b4 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = STAT)) + geom_boxplot() + xlab("Statistiques")
b5 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = MATCH)) + geom_boxplot() + xlab("Match")
b6 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = NATION)) + geom_boxplot() + xlab("Nation")
b7 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = INSTA)) + geom_boxplot() + xlab("INSTA")
b8 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = BLESSURE)) + geom_boxplot() + xlab("BLESSURE")
b9 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = xAG)) + geom_boxplot() + xlab("xAG")
b10 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = JDD_Salaires_rapport$CONTRAT)) + geom_boxplot() + xlab("CONTRAT")
b11 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = CLASSEMENT)) + geom_boxplot() + xlab("CLASSEMENT")
b12 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = PrgR)) + geom_boxplot() + xlab("PrgR")
b13 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = PrgC)) + geom_boxplot() + xlab("PrgC")
b14 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = PIB)) + geom_boxplot() + xlab("PIB")
b15 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = JAUNE)) + geom_boxplot() + xlab("JAUNE")
b16 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = ROUGE)) + geom_boxplot() + xlab("ROUGE")
b17 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = VALEUR)) + geom_boxplot() + xlab("VALEUR")
b18 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = MOYENNE_CONTRAT)) + geom_boxplot() + xlab("MOYENNE_CONTRAT")
b19 <- ggplot(JDD_Salaires_rapport, aes(x = "", y = UEFA)) + geom_boxplot() + xlab("UEFA")


# Afficher les graphiques par groupes
grid.arrange(b1, b2, b3, b4, b5, b6, b7, b8, b9, ncol = 3)  
grid.arrange(b10, b11, b12, b13, b14, b15, b16, b17,b18, b19, ncol = 3)  

#### Test ESD ----
#Pour le test ESD, val atypiques, quand Test Stat. > Critical Val.


# Ne pas oublier de spécifier au départ la valeur de y 
i = 4    # => on teste la possibilité d'avoir au plus 4 valeurs atypiques
y = JDD_Salaires_rapport$MOYENNE_CONTRAT   # <--- à modifier en fonction de la variable à tester
rval = function(y){
 ares = abs(y - mean(y))/sd(y)
 df = data.frame(y, ares)
 r = max(df$ares)
 list(r, df)}
n = length(y)
alpha = 0.05
lam= c(1:20)
R = c(1:20)
for (i in 1:20){
 if(i==1){
  rt = rval(y)
  R[i] = unlist(rt[1])
  df = data.frame(rt[2])
  newdf = df[df$ares!=max(df$ares),]}
 else if(i!=1){
  rt = rval(newdf$y)
  R[i] = unlist(rt[1])
  df = data.frame(rt[2])
  newdf = df[df$ares!=max(df$ares),]}
 ## Calcul de la valeur critique
 p = 1 - alpha/(2*(n-i+1))
 t = qt(p,(n-i-1))
 lam[i] = t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))
}
## Affichage des résultats
newdf = data.frame(c(1:20),R,lam)
names(newdf)=c("No. Outliers","Test Stat.", "Critical Val.")
newdf

sort(JDD_Salaires_rapport$MOYENNE_CONTRAT, decreasing = TRUE) 

order(JDD_Salaires_rapport$MOYENNE_CONTRAT, decreasing = TRUE)

#### Test Grubbs ----

grubbs.test(JDD_Salaires_rapport$PIB)

sort(JDD_Salaires_rapport$PIB, decreasing = TRUE) 

order(JDD_Salaires_rapport$PIB, decreasing = TRUE)


#### On enlève toutes les valeurs atypiques ----

JDD_clean <- JDD_Salaires_rapport[-c(1, 2, 3, 4, 5, 6, 8, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 32, 33, 34, 38, 41, 44, 46, 47, 49, 50, 51, 53, 54, 55, 57, 59, 60, 63, 67, 68, 70, 72, 73, 74, 88, 92, 102, 104, 106, 116, 117, 118, 119, 120, 121, 122, 126, 140, 143, 147, 160, 171, 172, 183, 187, 199, 203, 215, 225, 227, 233, 241, 264, 268, 269, 286, 294, 295, 296, 297, 301, 343, 353, 356, 357, 373, 389, 390, 391, 395, 425, 428, 429, 463),]


##### Graph clean ----

gcPIB <- ggplot(JDD_clean, aes(x = PIB)) + geom_histogram(bins = 20, fill = "brown") + theme_classic() 
gc1 <- ggplot(JDD_clean, aes(x = SALAIRE)) + geom_histogram(bins = 30, binwidth = 0.66, fill = "brown") + theme_classic()
gc2 <- ggplot(JDD_clean, aes(x = AGE)) + geom_histogram(bins = 30, binwidth = 1, fill = "brown") + theme_classic()
gc3 <- ggplot(JDD_clean, aes(x = BUT)) + geom_histogram(bins = 30, binwidth = 1.5, fill = "brown") + theme_classic()
gc4 <- ggplot(JDD_clean, aes(x = STAT)) + geom_histogram(bins = 30, binwidth = 0.075, fill = "brown") + theme_classic()
gc5 <- ggplot(JDD_clean, aes(x = MATCH)) + geom_histogram(bins = 50, binwidth = 1, fill = "brown") + theme_classic()
gc6 <- ggplot(JDD_clean, aes(x = NATION)) + geom_histogram(bins = 50, binwidth = 5, fill = "brown") + theme_classic()
gc7 <- ggplot(JDD_clean, aes(x = LDC)) + geom_histogram(bins = 50, binwidth = 5, fill = "brown") + theme_classic()
gc8 <- ggplot(JDD_clean, aes(x = INSTA)) + geom_histogram(bins = 30, binwidth = 1.5, fill = "brown") + theme_classic()


gc9 <- ggplot(JDD_clean, aes(x = BLESSURE)) + geom_histogram(bins = 60, binwidth = 3, fill = "brown") + theme_classic()
gc10 <- ggplot(JDD_clean, aes(x = xAG)) + geom_histogram(bins = 15, binwidth = 0.66, fill = "brown") + theme_classic()
gc11 <- ggplot(JDD_clean, aes(x = VALEUR)) + geom_histogram(bins = 300, binwidth = 5, fill = "brown") + theme_classic()
gc12 <- ggplot(JDD_clean, aes(x = CLASSEMENT)) + geom_histogram(bins = 30, binwidth = 1, fill = "brown") + theme_classic()
gc13 <- ggplot(JDD_clean, aes(x = PrgR)) + geom_histogram(bins = 100, binwidth = 5, fill = "brown") + theme_classic()
gc14 <- ggplot(JDD_clean, aes(x = JAUNE)) + geom_histogram(bins = 30, binwidth = 1, fill = "brown") + theme_classic()
gc15 <- ggplot(JDD_clean, aes(x = ROUGE)) + geom_histogram(bins = 3, binwidth = 1, fill = "brown") + theme_classic()
gc16 <- ggplot(JDD_clean, aes(x = PrgC)) + geom_histogram(bins = 200, binwidth = 5, fill = "brown") + theme_classic()
gc17 <- ggplot(JDD_clean, aes(x = CONTRAT)) + geom_histogram(bins = 200, binwidth = 5, fill = "brown") + theme_classic()

grid.arrange(gc2, gc3, gc4, gc5, gc6, gc7, gc8,gc9, gcPIB, ncol = 3) 
grid.arrange(gc10, gc11, gc12, gc13, gc14, gc15, gc16, gc17, ncol = 3)  

##### Création tableau clean ----

tabl_univarie_clean <- list()
for (variable in names(JDD_clean)) {
 stats <- summary(JDD_clean[[variable]])
 stats <- as.vector(stats)   
 tabl_univarie_clean[[variable]] <- stats
}

tabl_univarie_clean <- as.data.frame(do.call(rbind, tabl_univarie_clean))

colnames(tabl_univarie_clean) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")

tabl_univarie_clean$Variable <- rownames(tabl_univarie_clean)

head(tabl_univarie_clean) 

#write_xlsx(tabl_univarie, "C:/Users/arthu/OneDrive/Bureau/Master/summary_stat_instrument_clean.xlsx")

sd_val_clean <- sapply(JDD_clean, function(x) sd(x, na.rm = TRUE))

sd_df_clean <- data.frame(Variable = names(sd_val_clean), Standard_Deviation = sd_val_clean)

print(sd_df_clean) # a mettre dans le tableau encore une fois

# Analyse descriptive bivariée ----

## Matrice de corrélation quanti - quanti ----

library(PerformanceAnalytics)
library(corrplot)

mes_quanti <- JDD_Salaires_rapport[, c("SALAIRE", "AGE", "BUT", "STAT", "MATCH", "NATION", "LDC", "INSTA", "BLESSURE", "xAG",
                                       "CONTRAT", "CLASSEMENT", "PrgC", "PrgR", "PIB", "JAUNE", "ROUGE", "VALEUR", "MOYENNE_CONTRAT", "UEFA")] 

corr_mat <- cor(mes_quanti, use = "pairwise.complete.obs", method = "spearman")
corrplot(corr_mat,type="upper")
## Matrice de corrélation quali - quali ----


variables_correlation <- JDD_Salaires_rapport[, c("MILIEU", "ATTAQUANT", "DEFENSEUR", "TOP5", "DROITIER")]

correlation_matrix <- cor(variables_correlation, use = "pairwise.complete.obs")

corrplot(correlation_matrix, method = "circle", type = "upper",
         addCoef.col = "black")  

## Test pour quanti - quali ----


# Diviser les groupes
group0 <- JDD_Salaires_rapport$SALAIRE[JDD_Salaires_rapport$TOP5 == 0]
group1 <- JDD_Salaires_rapport$SALAIRE[JDD_Salaires_rapport$TOP5 == 1]

# Tester la normalité dans chaque groupe
shapiro.test(group0)  # Normalité pour les non top5
shapiro.test(group1)  # Normalité pour les top5



wilcox.test(JDD_Salaires_rapport$SALAIRE ~ JDD_Salaires_rapport$TOP5)

## Boxplot quanti-quali ----
boxplot(JDD_Salaires_rapport$SALAIRE ~ JDD_Salaires_rapport$TOP5, 
        main = "Distribution des salaires par TOP5",
        xlab = "TOP5", ylab = "SALAIRE", col = c("lightblue", "lightgreen"))



top <- ggplot(JDD_Salaires_rapport, aes(x = as.factor(TOP5), y = SALAIRE)) +
 geom_violin(fill = "lightblue", color = "darkblue") +
 geom_boxplot(width = 0.1, color = "black", alpha = 0.5) + 
 labs(title = "Distribution des salaires par TOP5",
      x = "TOP5", y = "Salaire") +
 theme_minimal()

droitier <- ggplot(JDD_Salaires_rapport, aes(x = as.factor(DROITIER), y = SALAIRE)) +
 geom_violin(fill = "lightblue", color = "darkblue") +
 geom_boxplot(width = 0.1, color = "black", alpha = 0.5) + 
 labs(x = "DROITIER", y = "Salaire") +
 theme_minimal()

attaquant <- ggplot(JDD_Salaires_rapport, aes(x = as.factor(ATTAQUANT), y = SALAIRE)) +
 geom_violin(fill = "lightblue", color = "darkblue") +
 geom_boxplot(width = 0.1, color = "black", alpha = 0.5) + 
 labs(x = "ATTAQUANT", y = "Salaire") +
 theme_minimal()
milieu <- ggplot(JDD_Salaires_rapport, aes(x = as.factor(MILIEU), y = SALAIRE)) +
 geom_violin(fill = "lightblue", color = "darkblue") +
 geom_boxplot(width = 0.1, color = "black", alpha = 0.5) + 
 labs( x = "MILIEU", y = "Salaire") +
 theme_minimal()
defenseur <- ggplot(JDD_Salaires_rapport, aes(x = as.factor(DEFENSEUR), y = SALAIRE)) +
 geom_violin(fill = "lightblue", color = "darkblue") +
 geom_boxplot(width = 0.1, color = "black", alpha = 0.5) + 
 labs(x = "DEFENSEUR", y = "Salaire") +
 theme_minimal()

grid.arrange(droitier, attaquant, milieu, defenseur, ncol = 3)

# Corrélation ----

cor(JDD_Salaires_rapport[, c( "BUT", "STAT", "MATCH", "NATION", 
                              "LDC", "INSTA", "BLESSURE",
                              "xAG", "CONTRAT","CLASSEMENT", "VALEUR", "PrgC","JAUNE","ROUGE","PIB","PrgR")], 
    use = "complete.obs", method = c("spearman"))


# On refait en enlevant des variables car problème de multicolinéarite

cor(JDD_Salaires_rapport[, c( "STAT", "MATCH", "NATION", 
                              "LDC", "INSTA", "BLESSURE","CONTRAT","PrgC","PrgR","PIB","JAUNE","ROUGE",
                              "CLASSEMENT", "VALEUR")], 
    use = "complete.obs", method = c("spearman"))

# Modèle économétrique ----

JDD_Salaires_rapport <- JDD_Salaires_rapport[JDD_Salaires_rapport$MATCH>=10, ]
JDD_Salaires_rapport<- JDD_Salaires_rapport[(JDD_Salaires_rapport$SALAIRE<100),]


library(leaps)
reg0<-(lm(SALAIRE~1,data=JDD_Salaires_rapport))

reg<-lm(SALAIRE~  STAT + MATCH + `NATION`+ LDC + INSTA+ BLESSURE + DROITIER+ CONTRAT+ TOP5 + CLASSEMENT+ 
         PIB+JAUNE+ROUGE+PrgC+ATTAQUANT+VALEUR,data=JDD_Salaires_rapport)

## STEP ----

### le modèle forward ----
step(reg0, scope=list(lower=reg0, upper=reg),data=JDD_Salaires_rapport, direction="forward") # sélection en avant démarre avec le modèle initial et on ajoute les var une a une pour voir qui impacte le plus l'AIC ( quand on a peu de var)
# sélection en avant démarre avec le modèle initial et on ajoute les variables une a une pour voir
# qui impacte le plus l'AIC ( quand on a peu de var)

reg1<-lm(formula = SALAIRE ~ LDC + VALEUR + NATION + PrgC + STAT + 
          JAUNE + CONTRAT + INSTA + BLESSURE + CLASSEMENT, data = JDD_Salaires_rapport)

### le modèle backward ----

step(reg, data=JDD_Salaires_rapport,direction="backward")

reg2<-lm(formula = SALAIRE ~ STAT + MATCH + NATION + LDC + INSTA + 
          CONTRAT + JAUNE + PrgC + VALEUR, data = JDD_Salaires_rapport)

modo <- lm(formula = SALAIRE ~ LDC + VALEUR + NATION + CONTRAT + PrgC + 
            INSTA + TOP5 + BLESSURE + STAT + JAUNE + ATTAQUANT + PIB, 
           data = JDD_Salaires_rapport)

### le modèle both ----

step(reg0, scope = list(upper=reg),data=JDD_Salaires_rapport,direction="both")

reg1<- lm(formula = SALAIRE ~ LDC + VALEUR + NATION + PrgC + STAT + 
           JAUNE + CONTRAT + INSTA + BLESSURE + CLASSEMENT, data = JDD_Salaires_rapport)

summary(reg1) 

library(car)
vif(reg1)

library(lmtest)
reset(reg1)

bptest(reg1)
residus<-residuals(reg1)
ks_test_result <- ks.test(residus, "pnorm", mean = mean(residus), sd = sd(residus))
print(ks_test_result)

# On supprime pour suivre une loi normal
par(mfrow=c(1,1))
plot(reg2)
JDD_Salaires_rapport<-JDD_Salaires_rapport[-c(306,270,111),] ###### Aleksandar Mitrović ,Sergej Milinković-Savić, Ivan Toney 
JDD_Salaires_rapport<-JDD_Salaires_rapport[-c(56,229,83),] ######  Dušan Vlahović Rúben Neves Moussa Diaby 
JDD_Salaires_rapport<-JDD_Salaires_rapport[-c(71,350,69),] ###### Jack Grealish Kevin Danso Ousmane Dembélé

print(JDD_Salaires_rapport[c(306, 270, 111), "Player"])
print(JDD_Salaires_rapport[c(56,229,83), "Player"]) 
print(JDD_Salaires_rapport[c(71,350,69), "Player"])

# On refait les tests apres avoir supprimé 9 joueurs
reg1<- lm(formula = SALAIRE ~ LDC + VALEUR + NATION + PrgC + STAT + 
           JAUNE + CONTRAT + INSTA + BLESSURE + CLASSEMENT, data = JDD_Salaires_rapport)

summary(reg1) 

vif(reg1)

reset(reg1)

bptest(reg1)

residus<-residuals(reg1)
ks_test_result <- ks.test(residus, "pnorm", mean = mean(residus), sd = sd(residus))
print(ks_test_result)

plot(cooks.distance(reg1),type="h")

## Origine de l'hétéroscédasticité ----

residualPlots(reg2)

### Méthode des Moindres Carrés pondéré  ----

library(nlme)

# Je re supprime ces 4 joueurs pour la loi normal
plot(reg1)
print(JDD_Salaires_rapport[c(329,350), "Player"]) # Ansu Fati Iñaki Williams
JDD_Salaires_rapport<-JDD_Salaires_rapport[-c(329,350),] 

print(JDD_Salaires_rapport[c(273,316), "Player"]) # Leon Goretzka, Antony 

JDD_Salaires_rapport<-JDD_Salaires_rapport[-c(273,316),] 

reg1<- lm(formula = SALAIRE ~ LDC + VALEUR + NATION + PrgC + STAT + 
           JAUNE + CONTRAT + INSTA + BLESSURE + CLASSEMENT, data = JDD_Salaires_rapport, weights= 1/VALEUR)

summary(reg1) 

vif(reg1)

reset(reg1)

bptest(reg1)
residus<-residuals(reg1)
ks_test_result <- ks.test(residus, "pnorm", mean = mean(residus), sd = sd(residus))
print(ks_test_result)

plot(cooks.distance(reg1),type="h") # Meilleur modèle 


residus<-residuals(reg1)
ks_test_result <- ks.test(residus, "pnorm", mean = mean(residus), sd = sd(residus))
print(ks_test_result)


### Doubles Moindres Carrés ----
library(AER)
library(sandwich)

# Nos erreurs de sont homoscédastiques donc =
reg_DMC2 <- ivreg(SALAIRE ~ LDC + VALEUR + NATION + PrgC + STAT + 
                   JAUNE + CONTRAT + INSTA + BLESSURE + CLASSEMENT | LDC + NATION + PrgC + STAT + 
                   JAUNE+ INSTA + BLESSURE + CLASSEMENT+ AGE + UEFA + MOYENNE_CONTRAT
                  , data = JDD_Salaires_rapport)
summary(reg_DMC2, diagnostics = TRUE )


##### Forme finale modèle ----
reg1<- lm(formula = SALAIRE ~ LDC + VALEUR + NATION + PrgC + STAT + 
           JAUNE + CONTRAT + INSTA + BLESSURE + CLASSEMENT, data = JDD_Salaires_rapport, weights= 1/VALEUR)

summary(reg1)
#Graph des residus
residuals <- resid(reg1)
hist(residuals, main = "Histogramme des résidus", xlab = "Résidus", breaks = 20)

######  Graph QQ plot des résidus et cook ----
qqnorm(residuals, main = "QQ-plot des résidus")
qqline(residuals, col = "red")

plot(cooks.distance(reg1),type="h")
###### Test statistiques ----
vif(reg1)

reset(reg1)

bptest(reg1)

residus<-residuals(reg1)
ks_test_result <- ks.test(residus, "pnorm", mean = mean(residus), sd = sd(residus))
print(ks_test_result)

hist(residus,
     main = "Histogramme des Résidus", 
     xlab = "Résidus", 
     ylab = "Fréquence", 
     breaks = 10,       
     col = "lightblue", 
     border = "black")

# Prédictions ----
JDD_Salaires_rapport$SALAIRE_pred <- predict(reg1, newdata = JDD_Salaires_rapport)



# Graphique avec points remplacés par numéros d'observations
plot(JDD_Salaires_rapport$SALAIRE, JDD_Salaires_rapport$SALAIRE_pred,
     xlab = "Valeurs Réelles (SALAIRE)", 
     ylab = "Valeurs Prédites (SALAIRE)", 
     main = "Prédictions vs Réalité (SALAIRE)",
     type = "n")  # Type "n" pour ne pas afficher les points

# Pour ajouter les numéros des observations
text(JDD_Salaires_rapport$SALAIRE, JDD_Salaires_rapport$SALAIRE_pred, labels = 1:nrow(JDD_Salaires_rapport), col = "blue", cex = 0.7)
abline(0, 1, col = "red", lwd = 2)  

pred <- ggplot(JDD_Salaires_rapport, aes(x = SALAIRE_pred)) + geom_histogram(bins = 25, binwidth = 0.5, fill = "lightblue") + theme_classic() + xlab("Répartition Salaires obs")
obs <- ggplot(JDD_Salaires_rapport, aes(x = SALAIRE)) + geom_histogram(bins = 30, binwidth = 1.5, fill = "blue") + theme_classic() + xlab("Répartition Salaires préd")

grid.arrange(pred, obs, ncol=2)

