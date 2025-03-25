library(minioclient)
library(quarto)

#' Pst!, don't tell to Jenny Bryan
setwd("work")

#' Downloading data from gripiradar web
mc_alias_set(
  "gripiradar", 
  "s3.hpc.ut.ee", 
  access_key = Sys.getenv("GRIPIRADAR_ACCESS_KEY"), 
  secret_key = Sys.getenv("GRIPIRADAR_SECRET_KEY")
)
mc_cp("gripiradar/gripiradar-ut-ee", ".", recursive = TRUE)

#' Rendering gripiradar dashboards in three languages
for (lang in c("ee", "en", "ru")) {
  message("Starting to render")
  quarto_render(
    "gripiradar.qmd", 
    execute_params = list("lang" = lang), 
    output_file = paste0("gripiradar_", lang, ".html")
    )
}
