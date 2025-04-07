library(minioclient)
library(quarto)

setwd("work")

#' Downloading data from gripiradar web
message("Downloading data")
mc_alias_set(
  "gripiradar",
  "s3.hpc.ut.ee",
  access_key = Sys.getenv("GRIPIRADAR_ACCESS_KEY"),
  secret_key = Sys.getenv("GRIPIRADAR_SECRET_KEY")
)
mc_cp("gripiradar/gripiradar-ut-ee", ".", recursive = TRUE)

#' Rendering gripiradar dashboards in three languages
message("Starting to render")
quarto_render("index.qmd")
