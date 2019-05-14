#pak::pkg_install('tidyverse')
# failed 404 error

#https://rstudio.github.io/renv/articles/renv.html

library(devtools)
install_github("rstudio/renv")

renv::init()


# how to install packages, but these go to C:\Users\wmu43954\Documents\R\R-3.6.0\library
remotes::install_cran('shiny')
remotes::install_cran('shinyjs')
remotes::install_cran('leaflet')
remotes::install_cran('mapview')
remotes::install_cran('tidyverse')
remotes::install_cran('sf')
remotes::install_cran('plotly')
#remotes::install_cran(raster)
remotes::install_cran('DT')
remotes::install_cran('readxl')
remotes::install_cran('RColorBrewer')
remotes::install_cran('FSA')
remotes::install_cran('lubridate')
remotes::install_cran('magrittr')
remotes::install_cran('testthat')

renv::snapshot() # whenever add new packages or update packages
#install.packages('dplyr')

install.packages('shiny')
install.packages('shinyjs')
install.packages('leaflet')
install.packages('mapview')
install.packages('tidyverse')
install.packages('sf')
install.packages('plotly')
#install.packages(raster)
install.packages('DT')
install.packages('readxl')
install.packages('RColorBrewer')
install.packages('FSA')
install.packages('lubridate')
install.packages('magrittr')
install.packages('testthat')

renv::history()
