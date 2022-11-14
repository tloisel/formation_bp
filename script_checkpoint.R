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

source("R/functions.R", encoding = "UTF-8")

# IMPORT DONNEES ----------------------------

df2 <- readr::read_csv2(
  "individu_reg.csv",
  col_select = c("region", "aemm", "aged", "anai",
                 "catl", "cs1", "cs2", "cs3", "couple", "na38",
                 "naf08", "pnai12", "sexe", "surf", "tp", "trans",
                 "ur"))

# FEATURE ENGINEERING -------------------------

# TRAITEMENT VALEURS MANQUANTES ==================


df2[df2$na38 == "ZZ", "na38"] <- NA
df2[df2$trans == "Z", "trans"] <- NA
df2[df2$tp == "Z", "tp"] <- NA
df2[endsWith(df2$naf08, "ZZ"), "naf08"] <- NA


# TYPES EN FACTEUR ===================

df2 <- df2 %>%
  mutate(across(
    c(-region, -aemm, -aged, -anai),
    as.factor)
  )

df2 <- df2 %>%
  mutate(age = as.numeric(aged))

df2$sexe <- df2$sexe %>%
  fct_recode(Homme = "1", Femme = "2")


# STATISTIQUES DESCRIPTIVES -------------------

# COMPTE PROFESSIONS =================

summarise_stat(df2,"cs1")
summarise_stat(df2,"cs2")
summarise_stat(df2,"cs3")

# STATISTIQUES AGE ======================

summarise(group_by(df2, age), n())


df2 %>%
  dplyr::select(age) %>%
  ggplot(.) + geom_histogram(aes(x = 5 * floor(age / 5)),
                             stat = "count")

ggplot(df2[as.numeric(df2$aged) > 50, ],
       aes(x = as.numeric(aged),
           y = ..density..,
           fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
       ),
       alpha = 0.2) + geom_histogram()



# part d'homme dans chaque cohorte ===================

temp <- df2 %>%
  group_by(age, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(age) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  dplyr::filter(sexe == "Homme")

ggplot(temp) +
  geom_bar(aes(x = as.numeric(age),
               y = SH_sexe), stat = "identity") +
  geom_point(aes(x = as.numeric(age),
                 y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


# stats surf par statut ==================

plot_hist_var_by_group(df2,"surf","couple")

# stats trans par statut ===================

plot_hist_var_by_group(df2,"trans","couple")

# STATS AGREGEES =================

stats_agregees(df2 %>%
                 filter(sexe == "Homme") %>%
                 mutate(aged = as.numeric(aged)) %>%
                 pull(aged), na.rm = TRUE)
stats_agregees(df2 %>%
                 filter(sexe == "Femme") %>%
                 mutate(aged = as.numeric(aged)) %>%
                 pull(aged), na.rm = TRUE)
stats_agregees(df2 %>%
                 filter(sexe == "Homme" & couple == "2") %>%
                 mutate(aged = as.numeric(aged)) %>%
                 pull(aged), na.rm = TRUE)
stats_agregees(df2 %>%
                 filter(sexe == "Femme" & couple == "2") %>%
                 mutate(aged = as.numeric(aged)) %>%
                 pull(aged), na.rm = TRUE)


# MODELISATION ----------------------------


df3 <- df2 %>%
  dplyr::select(surf, cs1, ur, couple, age) %>%
  filter(surf != "Z")

polr(surf ~ cs1 + factor(ur),
     df3 %>%
       filter(
         couple == 2 &
           age > 40 &
           age < 60)
)
