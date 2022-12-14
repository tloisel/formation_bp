rm(list = ls())

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("MASS")) install.packages("MASS")


library(tidyverse)
library(dplyr)
library(forcats)
library(MASS)

# DEFINITION DES FONCTIONS ------------

fonction_de_stat_agregee <- function(a, b = "moyenne", ignoreNA,...) {
  checkvalue <- F
  for (x in c("moyenne", "variance", "ecart-type", "sd", "ecart type")) {
    checkvalue <- (checkvalue | b == x)
  }
  if (checkvalue == FALSE) stop("statistique non supportée")
  
  if (b == "moyenne") {
    x <- mean(a, na.rm = ignoreNA, ...)
  } else if (b == "ecart-type" | b == "sd" | b == "ecart type") {
    x <- sd(b, na.rm = ignoreNA, ...)
  } else if (a == "variance") {
    x <- var(a, na.rm = ignoreNA, ...)
  }
  return(x)
}

decennie_a_partir_annee <- function(ANNEE) {
  return(ANNEE - ANNEE %%
           10)
}

recode_na <- function(data,variable_name,value){
  data %>% dplyr::mutate(
    !!rlang::sym(variable_name) := na_if(!!rlang::sym(variable_name),value))
}
# IMPORT DES DONNES ------------
# j'importe les données avec read_csv2 parce que c'est un csv avec des ; 
# et que read_csv attend comme separateur des ,
df <- readr::read_csv2(
  "individu_reg.csv",
  col_names=TRUE,
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2",
                "cs3", "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur")
)


# PREPROCESSING DES VARIABLES------------


df = df %>% mutate(
  na38=na_if(na38,"ZZ"),
  trans=na_if(trans,"Z"),
  tp=na_if(tp,"Z"),
  ur=factor(ur),
  sexe=fct_recode(factor(sexe), "Homme" = "1", "Femme" = "2"),
  aged=as.numeric(aged)
)

df[!is.na(df$naf08) & endsWith(df$naf08, "ZZ"), "naf08"] <- NA



# STATISTIQUES DESCRIPTIVES ------------
# combien de professions
print("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs1[!is.na(cs1)])))))
print("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs2[!is.na(cs2)])))))
print("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs3[!is.na(cs3)])))))

print.data.frame <- summarise(group_by(df, aged), n())
print(print.data.frame)

# fonction de stat agregee
fonction_de_stat_agregee(rnorm(10),ignoreNA=TRUE)
fonction_de_stat_agregee(rnorm(10), "ecart-type",ignoreNA=TRUE)
fonction_de_stat_agregee(rnorm(10), "variance",ignoreNA=TRUE)


fonction_de_stat_agregee(df %>% filter(sexe == "Homme") %>% mutate(aged = aged) %>% pull(aged), na.rm = T)
fonction_de_stat_agregee(df %>% filter(sexe == "Femme") %>% mutate(aged = aged) %>% pull(aged), na.rm = T)
fonction_de_stat_agregee(df %>% filter(sexe == "Homme" & couple == "2") %>% mutate(aged = aged) %>% pull(aged), na.rm = T)
fonction_de_stat_agregee(df %>% filter(sexe == "Femme" & couple == "2") %>% mutate(aged = aged) %>% pull(aged), na.rm = T)



# GRAPHIQUES ------------
df %>%
  select(aged) %>%
  ggplot(.) +
  geom_histogram(aes(x = 5 * floor(aged / 5)), stat = "count")

ggplot(df[as.numeric(df$aged) > 50, c(3, 4)], aes(
  x = aged, # x = aged - aged %% 5,
  y = ..density.., fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
), alpha = 0.2) +
  geom_histogram() # position = "dodge") + scale_fill_viridis_d()


# part d'homme dans chaque cohort
ggplot(df %>% group_by(as.numeric(aged, sexe)) %>% summarise(SH_sexe = n()) %>% group_by(aged) %>% summarise(SH_sexe = SH_sexe / sum(SH_sexe))) %>% filter(sexe == 1) + geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") + geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") + coord_cartesian(c(0, 100))
# correction (qu'il faudra retirer)
# ggplot(
#   df %>% group_by(aged, sexe) %>% summarise(SH_sexe = n()) %>% group_by(aged) %>% mutate(SH_sexe = SH_sexe/sum(SH_sexe)) %>% filter(sexe==1)
# ) + geom_bar(aes(x = aged, y = SH_sexe), stat="identity") + geom_point(aes(x = aged, y = SH_sexe), stat="identity", color = "red") + coord_cartesian(c(0,100))


# stats surf par statut
df3 <- tibble(df |> group_by(couple, surf) %>% summarise(x = n()) %>% group_by(couple) |> mutate(y = 100 * x / sum(x)))
ggplot(df3) %>%
  geom_bar(aes(x = surf, y = y, color = couple), stat = "identity", position = "dodge")

# stats trans par statut
df3 <- tibble(df |> group_by(couple, trans) %>% summarise(x = n()) %>% group_by(couple) |> mutate(y = 100 * x / sum(x)))
p <- ggplot(df3) +
  geom_bar(aes(x = trans, y = y, color = couple), stat = "identity", position = "dodge")

dir.create("output")


ggsave(p, "output/p.png")

api_pwd <- yaml::read_yaml("secrets.yaml")$api_pwd

# MODELISATION ------------

df3 <- df [,c("surf", "cs1", "ur", "couple", "aged")] %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = T)
df3[, "cs1"] <- factor(df3$cs1)
polr(surf ~ cs1 + factor(ur), df3 %>% filter(couple == "2" && as.numeric(aged > 40 && aged < 60)))





