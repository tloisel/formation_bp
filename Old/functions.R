decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}
# fonction de stat agregee
stats_agregees <- function(a, b = "moyenne",
                           ...) {
  match.arg(b,
            c("moyenne",
              "variance",
              "ecart-type",
              "sd",
              "ecart type")
  )
  switch(b,
         moyenne = mean(a, ...),
         variance = var(a, ...),
         sd(a, ...)
  )
}
stats_agregees(rnorm(10))
stats_agregees(rnorm(10), "ecart type")
stats_agregees(rnorm(10), "variance")

# fonction pour compter les professions
summarise_stat <- function(data,variable){
  print("Nombre de professions :")
  print(summarise(data, length(unique(data[[variable]][!is.na(data[[variable]])]))))
}

# fonction pour créer un graphique...
plot_hist_var_by_group <- function(data,variable,group){
  temp <- df2 |>
    group_by(!!rlang::sym(variable), !!rlang::sym(group)) %>%
    summarise(x = n()) %>%
    group_by(!!rlang::sym(group)) |>
    mutate(y = 100 * x / sum(x),
           x_var = !!rlang::sym(variable),
           group_var = !!rlang::sym(group))
  
  ggplot(temp) +
    geom_bar(aes(x = x_var, y = y, color = group_var),
             stat = "identity", position = "dodge")
  
}