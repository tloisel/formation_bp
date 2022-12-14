library(arrow)
library(dplyr)
library(rlang)
library(forcats)
library(MASS)
library(ggplot2)
library(gt)

# IMPORT DONNEES ----------------------------
df2 <- tibble(tar_load(survey_sample_24))
#tar_load(pwd_api)
# FEATURE ENGINEERING -------------------------

# TRAITEMENT VALEURS MANQUANTES ==================

df2 <- recode_as_na(df2, "na38", "ZZ")
df2 <- recode_as_na(df2, "trans", "Z")
df2 <- recode_as_na(df2, "tp", "Z")
df2[endsWith(df2$naf08, "ZZ"), "naf08"] <- NA


# TYPES EN FACTEUR ===================

df2 <- df2 |>
  mutate(across(
    c(-region, -aemm, -aged, -anai),
    as.factor)
  )

df2 <- df2 |>
  mutate(age = as.numeric(aged))

df2$sexe <- df2$sexe |>
  fct_recode(Homme = "1", Femme = "2")


# STATISTIQUES DESCRIPTIVES -------------------

# COMPTE PROFESSIONS =================

# combien de professions
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs3[!is.na(cs3)])))))
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs2[!is.na(cs2)])))))
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs1[!is.na(cs1)])))))


# STATISTIQUES AGE ======================

summarise(group_by(df2, age), n())


df2 |>
  dplyr::select(age) |>
  ggplot() + geom_histogram(aes(x = 5 * floor(age / 5)),
                            stat = "count")

ggplot(df2[as.numeric(df2$aged) > 50,],
       aes(x = as.numeric(aged),
           y = ..density..,
           fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
       ),
       alpha = 0.2) + geom_histogram()

stats_age <- df2 |> 
  group_by(decennie = decennie_a_partir_annee(age)) |>
  summarise(n())

table_age <- gt(stats_age) |>
  tab_header(
    title = "Distribution des âges dans notre population"
  ) |>
  fmt_number(
    columns = `n()`,
    sep_mark = " ",
    decimals = 0
  ) |>
  cols_label(
    decennie = "Tranche d'âge",
    `n()` = "Population"
  )

# part d'homme dans chaque cohorte ===================


temp <- part_total(df2) |> filter(sexe == "Homme")

ggplot(temp) +
  geom_bar(aes(x = as.numeric(age),
               y = share), stat = "identity") +
  geom_point(aes(x = as.numeric(age),
                 y = share), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


# stats surf par statut ==================

df3 <- part_total(df2, "couple", "surf")

ggplot(df3) +
  geom_bar(aes(x = surf, y = share, color = couple),
           stat = "identity", position = "dodge")

# stats trans par statut ===================

df3 <- part_total(df2, "couple", "trans")

ggplot(df3) + geom_bar(aes(x = trans, y = share, color = couple),
                       stat = "identity", position = "dodge")


# STATS AGREGEES =================

df3_homme <- df2 |>
  filter(sexe == "Homme")
df3_femme <- df2 |>
  filter(sexe != "Homme")
df3_homme_couple <- df3_homme |> filter(couple == 2)
df3_femme_couple <- df3_femme |> filter(couple == 2)

lapply(
  list(df3_homme$age,
       df3_femme$age,
       df3_homme_couple$age,
       df3_femme_couple$age), stats_agregees, na.rm = TRUE
)


# MODELISATION ----------------------------


df3 <- df2 |>
  dplyr::select(surf, cs1, ur, couple, age) |>
  filter(surf != "Z")

polr(surf ~ cs1 + factor(ur),
     df3 |>
       filter(
         couple == 2 &
           age > 40 &
           age < 60)
)