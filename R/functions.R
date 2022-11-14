decennie_a_partir_annee <- function(annee) {
  #' Transforme annee en decennie
  #' 
  #' @param annee Annee en question
  #' @return La décennie en question
  #' @example 
  #' decennie_a_partir_annee(2011)
  #' decennie_a_partir_annee(2010)
  return(annee - annee %% 10)
}

stats_agregees <- function(x, statistique = "moyenne",
                           ...) {
  #' Statistiques agregees
  #' 
  #' @param statistique Statistique désirée. Les valeurs autorisées
  #'  sont 'moyenne', 'variance', 'ecart-type', 'sd', 'ecart type'
  #' @inheritParams mean
  #' 
  #' @examples
  #' stats_agregees(rnorm(10))
  #' stats_agregees(rnorm(10), "cart type")
  #' stats_agregees(rnorm(10), "ecart type")
  #' stats_agregees(rnorm(10), "variance")
  
  match.arg(statistique,
            c("moyenne",
              "variance",
              "ecart-type",
              "sd",
              "ecart type")
  )
  
  switch(statistique,
         moyenne = mean(x, ...),
         variance = var(x, ...),
         sd(x, ...)
  )
  
}



recode_as_na <- function(df, var_name, value){
  #' Recode some values as NAs
  #' 
  #' @param df `data.frame` we should start from
  #' @param var_name Variable name
  #' @param value Value that should be replaced
  #' @seealso dyplr::na_if
  
  df |>
    mutate({{ var_name }} := na_if(!!rlang::sym(var_name), value))
}


part_total <- function(df2, var_groupe = "age", var_interet = "sexe"){
  df2 |>
    group_by(!!!syms(c(var_groupe, var_interet))) |>
    summarise(share = n()) |>
    group_by(!!sym(var_groupe)) |>
    mutate(share = share / sum(share))
}

read_yaml_secret <- function(filename="secrets.yaml"){
  return(yaml::read_yaml(filename))
}

read_from_parquet <- function(filename="individu_reg.parquet"){
  return(arrow::read_parquet(
    "individu_reg.parquet",
    col_select = c("region", "aemm", "aged", "anai",
                   "catl", "cs1", "cs2", "cs3", "couple", "na38",
                   "naf08", "pnai12", "sexe", "surf", "tp", "trans",
                   "ur") ))
}


