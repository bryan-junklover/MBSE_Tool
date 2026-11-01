# ModelBench: the thin Shiny shell. All logic lives in the modelbench package.
if (dir.exists("../modelbench")) pkgload::load_all("../modelbench", quiet = TRUE, helpers = FALSE) else library(modelbench)
library(mbdiagram)
shiny::shinyApp(app_ui(), app_server)
