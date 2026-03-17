# Côté-Gendreau 2026
# This file takes care of the parish files

# Packages we need
list_packages <- c("dplyr","readr","tibble","tidytable","readxl","geosphere")

# Unload all other packages to make sure functions work as expected
if (length(sessionInfo()$otherPkgs) != 0) {
  to_detach <- names(sessionInfo()$otherPkgs)[!names(sessionInfo()$otherPkgs) %in% list_packages]
  if (length(to_detach) != 0) {
    invisible(suppressWarnings(lapply(paste("package:", to_detach, sep=""),
                                      detach,
                                      character.only = TRUE,
                                      unload = TRUE)))
  }
}

installed <- list_packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(list_packages[!installed])
}

lapply(list_packages[!(list_packages %in% names(sessionInfo()$otherPkgs))], library, character.only = TRUE)

options(scipen = 999)

# Clean the file of parishes

parishes <- read_xlsx("data_raw/RPQA.Paroisses.2025-09-01.xlsx")

parishes <- parishes %>% 
  filter(sansCoord == 0 & !is.na(lat)) %>% 
  select(CodeLocation=codeParoisse,name=Graphie,lat,lng,year_par=anneeOuverture,province_state=province_etat)

write_csv(parishes,"data_modif/parishes.csv")

distance_matrix <- function(file) {
  
  parishes <- file %>% 
    select(CodeLocation,lng,lat) %>% 
    filter(!is.na(lng) & !is.na(lat)) %>% 
    mutate_all(~as.numeric(.x))
  
  matrix_distances = distm(as.matrix(parishes[,c("lng","lat")]), fun=distHaversine)/1000
  colnames(matrix_distances) <- parishes$CodeLocation
  rownames(matrix_distances) <- parishes$CodeLocation
  
  write_csv(as.data.frame(matrix_distances) %>% rownames_to_column("Par_A"), "data_modif/dist_matrix.csv")
}

distance_matrix(parishes)

rm(list = ls())
