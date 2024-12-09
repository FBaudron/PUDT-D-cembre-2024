#' ---
#' title: "Typologie villages"
#' author: "Frédéric Baudron"
#' date: "November 18th, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(janitor)
library(FactoMineR)
library(factoextra)
library(vegan)
library(ade4)
library(dendextend)
library(gtsummary)
library(randomForest)
library(dplyr)
library(sf)
library(terra)
library(geodata)
library(ggpubr)


# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\PUDT\\Enquête village\\")

data = read.xlsx("Enquête village avec données spatiales.xlsx", sheet = 1)

data = clean_names(data)


# DATA MANIPULATION-------------------------------------------------------------

typo = data[, c(4:6, 8:10, 16:19, 24:33, 35:37, 39:40, 50:51, 53:61, 63:66, 68:70, 75, 88, 92, 100:104)]

names(typo)

names(typo)[c(4:44, 47:48, 50:51)] = c("latitude", "longitude", "altitude", "nb_cases", "nb_habitants", "nb_prop_terriens",
                        "autochtones", "agriculture", "arboriculture", "pisciculture", "elevage", "charbon",
                        "peche", "chasse", "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce",
                        "neoruraux", "main_oeuvre_ext", "groupements", "nb_groupements", "intrants_equip",
                        "mecanisation", "entraide", "savoir", "transformation", "commercialisation", "projets",
                        "prod_agric", "tracteur", "acheteurs", "pieds", "kavaki", "voiture_camion", "route_vente",
                        "marche", "brazza_pnoire", "projet_agric", "altitude", "pluvio", "carbone_sol", "sable")

typo$autochtones = as.numeric(typo$autochtones == "Oui")
typo$neoruraux = as.numeric(typo$neoruraux == "Oui")
typo$main_oeuvre_ext = as.numeric(typo$main_oeuvre_ext == "Oui")
typo$groupements = as.numeric(typo$groupements == "Oui")
typo$tracteur = as.numeric(typo$tracteur == "Oui")
typo$projet_agric = as.numeric(typo$projet_agric == "Oui")

typo$nb_groupements[is.na(typo$nb_groupements)] = 0


typo$nb_prop_terriens = typo$nb_prop_terriens / typo$nb_habitants

typo$type2 =  ifelse(typo$type == "défriche (10 - 30 %) sur couvert forestier discontinu", "Défriche dense sous couvert forestier discontinu", NA)
typo$type2 =  ifelse(typo$type == "défriche dense (+ de 30 %) sur couvert forestier discontinu", "Défriche dense sous couvert forestier discontinu", typo$type2)

typo$type2 =  ifelse(typo$type == "défriche forestière (25-50%)", "Défriche en zone forestière", typo$type2)
typo$type2 =  ifelse(typo$type == "défriche forestière dense (+ de 50%)", "Défriche en zone forestière", typo$type2)
typo$type2 =  ifelse(typo$type == "défriche forestière peu dense (- de 25%)", "Défriche en zone forestière", typo$type2)

typo$type2 =  ifelse(typo$type == "défriche marginale sur couvert forestier discontinu", "Défriche peu dense sous couvert forestier discontinu", typo$type2)
typo$type2 =  ifelse(typo$type == "défriche peu dense (- de 10%) sur couvert forestier discontinu", "Défriche peu dense sous couvert forestier discontinu", typo$type2)

typo$type2 =  ifelse(typo$type == "parcellaire mécanisé", "Agriculture de savane mécanisée", typo$type2)
typo$type2 =  ifelse(typo$type == "parcellaire mécanisé récent (après 2015)", "Agriculture de savane mécanisée", typo$type2)

typo$type2 =  ifelse(typo$type == "petit parcellaire dense", "Savanes densément cultivées", typo$type2)


typo$esquisse =  ifelse(typo$type2 == "Agriculture de savane mécanisée", "Savanes", NA)
typo$esquisse =  ifelse(typo$type2 == "Savanes densément cultivées", "Savanes", typo$esquisse)
typo$esquisse =  ifelse(typo$type2 == "Défriche en zone forestière", "Forêt", typo$esquisse)
typo$esquisse =  ifelse(typo$type2 == "Défriche dense sous couvert forestier discontinu", "Mosaïques", typo$esquisse)
typo$esquisse =  ifelse(typo$type2 == "Défriche peu dense sous couvert forestier discontinu", "Mosaïques", typo$esquisse)

types_esq = typo[, c(45, 1:44, 47:51, 46, 52:53)]


# write.xlsx(types_esq, file = "Types villages esquisse.xlsx")


# TYPOLOGY----------------------------------------------------------------------

# Normalization of continuous variables

par(mar = c(3, 3, 3, 3))

data_typo = typo

# hist(typo$nb_cases)
typo$nb_cases = log10(typo$nb_cases + (0.5 * min(typo$nb_cases[typo$nb_cases > 0])))
# hist(typo$nb_cases)
# hist(typo$nb_habitants)
typo$nb_habitants = log10(typo$nb_habitants + (0.5 * min(typo$nb_habitants[typo$nb_habitants > 0])))
# hist(typo$nb_habitants)
# hist(typo$nb_prop_terriens)
typo$nb_prop_terriens = log10(typo$nb_prop_terriens + (0.5 * min(typo$nb_prop_terriens[typo$nb_prop_terriens > 0])))
# hist(typo$nb_prop_terriens)
# hist(typo$altitude)
# hist(typo$pluvio)
# hist(typo$route)
typo$route = log10(typo$route + (0.5 * min(na.omit(typo$route)[na.omit(typo$route) > 0])))
# hist(typo$route)
# hist(typo$carbone_sol)
# hist(typo$sand)


# PRINCIPAL COORDINATE ANALYSIS & HIERARCHICHAL CLUSTER ANALYSIS----------------

# names(which(sapply(typo, anyNA)))

typo = typo[, c(7:26, 36:40, 44:53)]

typo = na.omit(typo)


# the scaled Euclidean dissimilarity between continuous geospatial variables
dEuc_geo = vegdist(typo[, c("altitude", "pluvio", "carbone_sol", "sable"
                            )], method = "gower") 

# need to create a dissimilairty matrix for tyoe, which has 10 categories, not 2
typo$esquisse = as.factor(typo$esquisse)

nobs <- length(typo$esquisse)
desq <- matrix(NA, nobs, nobs)
for (i in 1:nobs){
  for (j in 1:nobs){
    desq[i,j] <- 1 - (typo$esquisse[i] == typo$esquisse[j])
  }
}
desq <- as.dist(desq)


# the scaled Euclidean dissimilarity between continuous variables related to population
dEuc_pop = vegdist(typo[, c(
  # "nb_cases",
  "route", "nb_habitants", "nb_prop_terriens"
                            )], method = "gower") 

# dissimilarity between binary variables
dBin_act = dist.binary(typo[, c(
  "agriculture",
  "arboriculture", "pisciculture", "elevage",
  "charbon", "peche", "chasse", "sciage", "cueillette",
  "emploi", "mine", "briqueterie",
  "commerce",
  "autochtones", "neoruraux", "main_oeuvre_ext", "groupements",
  "tracteur", "projet_agric"
  )], method = 2)


# Combine environmental variables
d_env = (4 * dEuc_geo^2 + desq^2)/5

# Combine socio-economic variables
d_soc = (3 * dEuc_pop^2 + 18*desq^2)/21

# Combine all dissimilarities
dAll = (d_env + d_soc) / 2

# Transforming the matrix of dissimilarities to a matrix of distances
distAll = sqrt(2 * dAll)

pco = cmdscale(distAll, eig = TRUE, k = 10) 

# cumsum(pco$eig) / sum(pco$eig) 
barplot(pco$eig[1:20])

# choosing 3 dimensions
pco_var = pco$points[, 1:2]

hc_pco = hclust(dist(pco_var), method = "complete")
# plot(hc_pco, hang = -1)
grpPCO = cutree(hc_pco, k = 3)


hdend = as.dendrogram(hc_pco)
hdend = color_branches(hdend, k = 3)
hdend = color_labels(hdend, k = 3)
plot(hdend)


plot(pco$points[,1], pco$points[,2], col=grpPCO)
plot(pco$points[,1], pco$points[,3], col=grpPCO)
plot(pco$points[,2], pco$points[,3], col=grpPCO)

typo$village_type = grpPCO

data_typo = merge(data_typo, typo[, c(27, 36)], by = "index", all.x = TRUE)

data_typo$village_type = as.factor(data_typo$village_type)

table(data_typo$village_type)

# write.xlsx(data_typo, file = "Typologie de villages.xlsx")


# INTERPRETATION OF THE FARM TYPES----------------------------------------------

names(data_typo)

data_typo$esquisse = as.factor(data_typo$esquisse)

rf_type = randomForest(village_type ~ ., data = na.omit(data_typo[, c("nb_cases", "nb_habitants", "nb_prop_terriens", "autochtones", "agriculture", 
                                                              "arboriculture", "pisciculture", "elevage", "charbon", "peche", "chasse", 
                                                              "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce", "neoruraux",
                                                              "main_oeuvre_ext", "groupements", "tracteur", "projet_agric", "altitude",
                                                              "pluvio", "route", "carbone_sol", "sable", "esquisse", "village_type")]), ntree = 1500)

print(rf_type)

# importance(rf_type)

varImpPlot(rf_type)


data_typo[, c("nb_cases", "nb_habitants", "nb_prop_terriens", "autochtones", "agriculture", 
                "arboriculture", "pisciculture", "elevage", "charbon", "peche", "chasse", 
                "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce", "neoruraux",
                "main_oeuvre_ext", "groupements", "tracteur", "projet_agric", "altitude",
                "pluvio", "route", "carbone_sol", "sable", "esquisse", "village_type")] %>%
  tbl_summary(
    by = village_type,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    # type = list(peche ~ "continuous"),
    digits = all_continuous() ~ 1)



# data_typo = data_typo[!is.na(data_typo$esquisse),]

rf_type = randomForest(esquisse ~ ., data = na.omit(data_typo[, c("nb_cases", "nb_habitants", "nb_prop_terriens", "autochtones", "agriculture", 
                                                                      "arboriculture", "pisciculture", "elevage", "charbon", "peche", "chasse", 
                                                                      "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce", "neoruraux",
                                                                      "main_oeuvre_ext", "groupements", "tracteur", "projet_agric", "altitude",
                                                                      "pluvio", "route", "carbone_sol", "sable", "esquisse")]), ntree = 1500)

print(rf_type)

# importance(rf_type)

varImpPlot(rf_type)


data_typo[, c("nb_cases", "nb_habitants", "nb_prop_terriens", "autochtones", "agriculture", 
              "arboriculture", "pisciculture", "elevage", "charbon", "peche", "chasse", 
              "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce", "neoruraux",
              "main_oeuvre_ext", "groupements", "tracteur", "projet_agric", "altitude",
              "pluvio", "route", "carbone_sol", "sable", "esquisse")] %>%
  tbl_summary(
    by = esquisse, 
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    # type = list(groupement ~ "continuous"),
    digits = all_continuous() ~ 1)

class(data_typo$nb_cases)

compare_means(pluvio ~ esquisse,  data = data_typo[, c("nb_cases", "nb_habitants", "nb_prop_terriens", "autochtones", "agriculture", 
                                                       "arboriculture", "pisciculture", "elevage", "charbon", "peche", "chasse", 
                                                       "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce", "neoruraux",
                                                       "main_oeuvre_ext", "groupements", "tracteur", "projet_agric", "altitude",
                                                       "pluvio", "route", "carbone_sol", "sable", "esquisse")])



my_comparisons <- list( c("Forêt", "Mosaïques"), c("Mosaïques", "Savanes"), c("Forêt", "Savanes") )

ggboxplot(data_typo[, c("nb_cases", "nb_habitants", "nb_prop_terriens", "autochtones", "agriculture", 
                        "arboriculture", "pisciculture", "elevage", "charbon", "peche", "chasse", 
                        "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce", "neoruraux",
                        "main_oeuvre_ext", "groupements", "tracteur", "projet_agric", "altitude",
                        "pluvio", "route", "carbone_sol", "sable", "esquisse")],
          x = "esquisse", y = "pluvio", color = "esquisse", palette = "jco") + 
   stat_compare_means(comparisons = my_comparisons)

# ggsave("Pluvio.jpeg", units = "cm", width = 15, height = 12, dpi = 320)


ggboxplot(subset(data_typo[, c("nb_cases", "nb_habitants", "nb_prop_terriens", "autochtones", "agriculture", 
                        "arboriculture", "pisciculture", "elevage", "charbon", "peche", "chasse", 
                        "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce", "neoruraux",
                        "main_oeuvre_ext", "groupements", "tracteur", "projet_agric", "altitude",
                        "pluvio", "route", "carbone_sol", "sable", "esquisse")], nb_cases < 500),
          x = "esquisse", y = "nb_cases", color = "esquisse", palette = "jco") + 
  stat_compare_means(comparisons = my_comparisons)

# ggsave("Nombre de cases.jpeg", units = "cm", width = 15, height = 12, dpi = 320)


ggboxplot(subset(data_typo[, c("nb_cases", "nb_habitants", "nb_prop_terriens", "autochtones", "agriculture", 
                        "arboriculture", "pisciculture", "elevage", "charbon", "peche", "chasse", 
                        "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce", "neoruraux",
                        "main_oeuvre_ext", "groupements", "tracteur", "projet_agric", "altitude",
                        "pluvio", "route", "carbone_sol", "sable", "esquisse")], nb_prop_terriens < 0.7),
          x = "esquisse", y = "nb_prop_terriens", color = "esquisse", palette = "jco") + 
  stat_compare_means(comparisons = my_comparisons)

# ggsave("Fraction de proprietaires terriens.jpeg", units = "cm", width = 15, height = 12, dpi = 320)




data_sum = aggregate(. ~ esquisse, FUN = sum, na.rm = TRUE, data = data_typo[, c("autochtones", "elevage", "chasse", "sciage",
                                                                           "mine", "neoruraux","tracteur", "projet_agric",
                                                                       "esquisse")])

data_mean = aggregate(. ~ esquisse, FUN = mean, na.rm = TRUE, data = data_typo[, c("autochtones", "elevage", "chasse", "sciage",
                                                                                 "mine", "neoruraux","tracteur", "projet_agric",
                                                                                 "esquisse")])

prop.test(data_sum$tracteur, c(59, 60, 32))

ggbarplot(data_mean,
          x = "esquisse", y = "tracteur",  
  color = "esquisse", palette = "jco",
  position = position_dodge(0.8)) +
  annotate("text", x = 2, y = 0.85, label = "p-value = 2.051e-11")

ggsave("% villages avec tracteur.jpeg", units = "cm", width = 15, height = 12, dpi = 320)


library("scales")
show_col(pal_jco("default")(10))








data_sf = st_as_sf(data_typo, coords = c("longitude", "latitude"), crs ="+proj=longlat + datum = WGS84 + no_defs")
data_sf = as(data_sf, "Spatial")
data_sf = vect(data_sf)
data_sf = project(data_sf, "EPSG:4326")

cog0 = gadm(country = 'COG', level = 0, path = 'D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions')
cog1 = gadm(country = 'COG', level = 1, path = 'D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions')
cog2 = gadm(country = 'COG', level = 2, path = 'D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions')

data_sf$esquisse = as.factor(data_sf$esquisse)

plot(cog1)
plot(data_sf, pch = 20, cex = 2, col=c("orange","orangered", "darkred")[data_sf$esquisse],
     legend = TRUE, axes=F, add=T) 


pool = subset(cog1, subset = cog1$NAME_1 == "Pool")
boue = subset(cog1, subset = cog1$NAME_1 == "Bouenza")

plot(pool)
plot(data_sf, pch = 20, cex = 2, col=c("orange","orangered", "darkred")[data_sf$esquisse],
     legend = TRUE, axes=F, add=T) 

plot(boue)
plot(data_sf, pch = 20, cex = 2, col=c("orange","orangered", "darkred")[data_sf$esquisse],
     legend = TRUE, axes=F, add=T) 


