# Initialisation des différentes expériences
library(readxl)
chemin <- "C:/Users/taksh/OneDrive - Universite Evry Val d'Essonne/LDD/Biologie/L3/Régulation Expression Génique Eucaryotes/Quete_annexe/TD3_donnees_brutes_etudiants.xlsx"
exp1 <- read_excel(chemin, 
                      sheet = "données_brutes_étudiants", 
                      range = "A9:D25"
                      )
exp2 <- read_excel(chemin, 
                   sheet = "données_brutes_étudiants", 
                   range = "A27:D39"
)
exp3 <- read_excel(chemin, 
                   sheet = "données_brutes_étudiants", 
                   range = "A42:D54"
)
exp4 <- read_excel(chemin, 
                   sheet = "données_brutes_étudiants", 
                   range = "A57:D69"
)
# J'enlève les informations qui ne me seront pas nécessaires pour la suite
exp0<-exp1[-1,]
exp1<-exp0[-4,]
exp0<-exp1[-7,]
exp1<-exp0[-10,]
remove(exp0)

# Normalisation des résultats
exp1$Normal <- exp1$firefly/exp1$renilla
exp2$Normal <- exp2$firefly/exp2$renilla
exp3$Normal <- exp3$Firefly/exp3$renilla
exp4$Normal <- exp4$Firefly/exp4$renilla

# Moyennes et écart-types de chaque condition biologique de chaque expérience
moy_exp1 <- tapply(exp1$Normal,exp1$Sample,mean)
moy_exp2 <- tapply(exp2$Normal,exp2$Sample,mean)
moy_exp3 <- tapply(exp3$Normal,exp3$Sample,mean)
moy_exp4 <- tapply(exp4$Normal,exp4$Sample,mean)

et_exp1 <- tapply(exp1$Normal,exp1$Sample,sd)
et_exp2 <- tapply(exp2$Normal,exp2$Sample,sd)
et_exp3 <- tapply(exp3$Normal,exp3$Sample,sd)
et_exp4 <- tapply(exp4$Normal,exp4$Sample,sd)

# Léger souci de traitement (résolu)
moy_exp4 <- moy_exp4[-1]
et_exp4 <- et_exp4[-1]

# Histogrammes 
graphe1 <- barplot(moy_exp1, ylab = "Moyennes Luciférase/Renilla", 
                   main = "Moyennes Luciférase/Rénilla pour chaque 
                   condition biologique (Expérience 1)", 
                   ylim = c(0,(max(moy_exp1)+1)))
arrows(graphe1, moy_exp1 - et_exp1, graphe1, moy_exp1 + et_exp1, 
       angle= 90, code =3, length =0.1)

graphe2 <- barplot(moy_exp2, ylab = "Moyennes Luciférase/Renilla", 
                   main = "Moyennes Luciférase/Rénilla pour chaque 
                   condition biologique (Expérience 2)", 
                   ylim = c(0,(max(moy_exp2) + 1)))
arrows(graphe2, moy_exp2 - et_exp2, graphe2, moy_exp2 + et_exp2, 
       angle= 90, code =3, length =0.1)

graphe3 <- barplot(moy_exp3, ylab = "Moyennes Luciférase/Renilla", 
                   main = "Moyennes Luciférase/Rénilla pour chaque 
                   condition biologique (Expérience 3)", 
                   ylim = c(0,(max(moy_exp3) + 1)))
arrows(graphe3, moy_exp3 - et_exp3, graphe3, moy_exp3 + et_exp3, 
       angle= 90, code =3, length =0.1)

graphe4 <- barplot(moy_exp4, ylab = "Moyennes Luciférase/Renilla", 
                   main = "Moyennes Luciférase/Rénilla pour chaque 
                   condition biologique (Expérience 4)", 
                   ylim = c(0,(max(moy_exp4) + 1)))
arrows(graphe4, moy_exp4 - et_exp4, graphe4, moy_exp4 + et_exp4, 
       angle= 90, code =3, length =0.1)

# Histogramme ultime
# Normalisation
moy_exp1.1 <- moy_exp1 / moy_exp1["vv+"] 
moy_exp2.1 <- moy_exp2 / moy_exp2["vv+"]
moy_exp3.1 <- moy_exp3 / moy_exp3["vv+"]
moy_exp4.1 <- moy_exp4 / moy_exp4["vv+"]

cvs <- c(moy_exp1.1["cvs+"], moy_exp2.1["cvs+"], moy_exp3.1["cvs+"] ,moy_exp4.1["cvs+"])
sad <- c(moy_exp1.1["sad+"], moy_exp2.1["sad+"], moy_exp3.1["sad+"] ,moy_exp4.1["sad+"])
MOCK <- c(moy_exp1.1["vv+"], moy_exp2.1["vv+"], moy_exp3.1["vv+"] ,moy_exp4.1["vv+"])

# Niveau moyen d'activation du promoteur du gène de l'IFN
m_CVS <- mean(cvs)
e_CVS <- sd(cvs)
m_SAD <- mean(sad)
e_SAD <- sd(sad)
m_MOCK <- mean(MOCK)
e_MOCK <- sd(MOCK)

mega_m <- c(m_CVS, m_SAD, m_MOCK)
mega_e <- c(e_CVS, e_SAD, e_MOCK)
titre <- c("pCVS", "pSAD", "MOCK")
mega_moyennes <- tapply(mega_m, titre, sum)
mega_ecart <- tapply(mega_e, titre, sum)

# ENFIN L'HISTOGRAMME
grapheULT <- barplot(mega_moyennes, 
                   main = "Niveau moyen d'activation du 
                   promoteur du gène de l'IFN")
axis(2, at = seq(0,(max(mega_moyennes) + 0.2), by = 0.2))
arrows(grapheULT, mega_moyennes - mega_ecart, grapheULT, mega_moyennes + mega_ecart, 
       angle= 90, code =3, length =0.1)
