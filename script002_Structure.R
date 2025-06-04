# 

devtools::load_all()
devtools::document()  # Actualiza documentación
devtools::build()     # Arma el paquete para mi
devtools::install()
.rs.restartR()

devtools::check()     # Verifica el paquete
devtools::test()      # Ejecuta pruebas


######################################
# Desinstalar el paquete
remove.packages("Rscience.GeneralLM")
remove.packages("Rscience.menu")
remove.packages("Rscience.import")

# Limpiar el caché de devtools
devtools::clean_dll()
.rs.restartR()


# Instalar nuevamente
remotes::install_github("deliaspanigo/Rscience.import")
remotes::install_github("deliaspanigo/Rscience.menu")
devtools::install()
########################################
library(Rscience.menu)
library(Rscience.import)
library(Rscience.GeneralLM)
Rscience.GeneralLM::run_app()



devtools::install()
