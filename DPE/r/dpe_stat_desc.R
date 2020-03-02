
# Librairires -------------------------------------------------------------
library("corrplot") # corrélation
library("ggplot2") # data visualisation
library("forcats") # factor manipulation
library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("cowplot") # export plots

# Data --------------------------------------------------------------------

dpe <- read.csv("DPE/data/DPE_Batiments Publics.csv", sep = ";")

# Data cleansing ----------------------------------------------------------

# correction des types de données retournées par R
a_corriger <- c(
  "consommation_energie", "estimation_ges", "surface_habitable",
  # "en_surface", "en_souterrain",
  "shon", "surface_utile", "surface_thermique_parties_communes",
  "surface_baies_orientees_est_ouest", "surface_baies_orientees_nord",
  "surface_baies_orientees_sud", "surface_parois_verticales_opaques_deperditives",
  "surface_planchers_bas_deperditifs", "surface_planchers_hauts_deperditifs",
  "consommation_energie_finale", "consommation_energie_primaire"
)

for (i in a_corriger) {
  dpe[, i] <- as.numeric(dpe[, i])
}

# Correction - valeurs manquantes
for (na_type in c("NULL", "")) {
  dpe[dpe == na_type] <- NA
}

# Correction des années abérantes 
dpe <- dpe %>% 
  mutate(annee_construction = ifelse(
    test = annee_construction < 1700,
    yes = NA,
    no = ifelse(
      test = annee_construction < 1945,
      yes = "1700 - 1944",
      no = annee_construction
  )))

# Correction - factor
dpe$tr001_modele_dpe_id <- as.factor(dpe$tr001_modele_dpe_id)
dpe$annee_construction <- as.factor(dpe$annee_construction)


# Statistiques descriptives -----------------------------------------------

# Nombre de lignes
dim(dpe)[1]

# Nombre de colonnes
dim(dpe)[2]

# Noms des variables
names(dpe)

# Apperçu des données
head(dpe)

# Résumé statistique
summary(dpe)

# Valeurs manquantes
vm <- sapply(
  X = dpe,
  function(x) sum(is.na(x))
) 

vm <- data.frame(var = names(vm), nb_na = vm)

ggplot(vm, mapping = aes(x = fct_reorder(var, nb_na, .desc = T), y = nb_na)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(nb_na/1000,1))) +
  labs(
    x = "",
    y = "Nombre de valeurs manquantes"
  ) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 12)
  )

# Sélection des variables
to_rm <- vm[vm$nb_na>2500,][,1]
dpe <- dpe %>% 
  select(-to_rm)

# Analyses des variables quantitatives ------------------------------------

# Corrélation entre les variables
variables_quanti <- sapply(
  X = dpe,
  function(x) is.numeric(x)
)

dpe_quanti <- dpe[, variables_quanti]
mat_cor <- cor(dpe_quanti[, -3]) # on enlève l'année de construction
corrplot(mat_cor, type = "upper", tl.cex = 0.5)

# Densité

# remove default cowplot theme
theme_set(theme_grey())

plotDensity <- function(data, var) {
  ggplot(
    data = data,
    mapping = aes_string(x = var)
  ) +
    geom_density() +
    labs(y = "Densité") +
    theme(
      axis.title.x = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      axis.text.y = element_text(size = 8)
    )
}

density_plots <- lapply(
  X = names(dpe_quanti)[-3],
  FUN = function(name) plotDensity(dpe_quanti, name)
)

plot_grid(plotlist = density_plots, nrow = 5)



# Analyse des variables qualitatives --------------------------------------

# Distribution des années
ggplot(data = dpe, aes(x = annee_construction)) +
  geom_bar() +  
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 12))

# Proportion

# Histogramme de fréquence


# Distribution des classes consommation énergie
round(prop.table(table(dpe$classe_consommation_energie)) * 100, 2)

# Représentation graphique
ggplot(data = dpe, aes(x = classe_consommation_energie)) +
  geom_bar()

# Distribution des classes estimation ges
round(prop.table(table(dpe$classe_estimation_ges)) * 100, 2)

# Représentation graphique
ggplot(data = dpe, aes(x = classe_estimation_ges)) +
  geom_bar()



# Analyse bivariée --------------------------------------------------------

# Année de construction x conso d'énergie
ggplot(dpe, mapping = aes(x = annee_construction, y = ))

# ACP ---------------------------------------------------------------------




# Clustering --------------------------------------------------------------

# Sélection des variables

# K-means


# CAH


# Clustering données mixtes -----------------------------------------------




# Modélisation ------------------------------------------------------------

# Prévision de la notation énergétique d'un bâtiment
