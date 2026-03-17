# Côté-Gendreau 2026
# This file cleans family timelines from 'outlier' records

# Packages we need
list_packages <- c("dplyr","readr","tidytable","lubridate")

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

intervals_u <- read_csv("register/intervals_u.csv")
dist_matrix <- read_csv("data_modif/dist_matrix.csv") %>% 
  pivot_longer(cols = -Par_A, names_to = "Par_B", values_to = "distance") %>% 
  mutate_all(~as.numeric(.x))

clean_unions <- function(dist_thresh = 10, df = NULL) {
  
  ptm <- proc.time()
  
  colnames <- colnames(df)
  
  intervals_u_clean <- df %>% 
    group_by(idU) %>% 
    mutate(n_acts_lead = lead(n_acts),
           n_acts_lag = lead(n_acts)) %>% 
    left_join(select(dist_matrix,distance=distance,CodeLocation_before=Par_A,CodeLocation=Par_B)) %>%
    group_by(idU,CodeLocation) %>% 
    mutate(tot_acts = sum(n_acts)) %>% 
    group_by(idU) %>% 
    mutate(tot_acts_lag = lag(tot_acts)) %>% 
    mutate(del = if_else(n_acts == 1 & ((n_acts_lag > 1 | n_acts_lead > 1) & tot_acts < tot_acts_lag) & CodeLocation_before == CodeLocation_after,1,0,missing=0)) %>% 
    mutate(loc_lead = lead(CodeLocation),
           loc_lag = lag(CodeLocation)) %>% 
    filter(!(del == 1 & distance >= dist_thresh)) %>% 
    mutate(idloc = consecutive_id(CodeLocation)) %>% 
    group_by(idU,idloc) %>% 
    summarize(CodeLocation = first(CodeLocation),
              Date_from = first(Date_from),
              Date_to = last(Date_to),
              Date_first = first(Date_first),
              Date_last = last(Date_last),
              n_acts = sum(n_acts),
              CodeLocation_before = first(CodeLocation_before),
              CodeLocation_after = last(CodeLocation_after),
              del = first(del),
              only_uncertain = min(only_uncertain)) %>% 
    mutate(del_lag = lag(del)) %>% 
    filter(del != 1) %>% 
    mutate(idloc = consecutive_id(CodeLocation)) %>% 
    group_by(idU,idloc) %>% 
    summarize(CodeLocation = first(CodeLocation),
              Date_from = first(Date_from),
              Date_to = last(Date_to),
              Date_first = first(Date_first),
              Date_last = last(Date_last),
              n_acts = sum(n_acts)+sum(del_lag,na.rm=T),
              CodeLocation_before = first(CodeLocation_before),
              CodeLocation_after = last(CodeLocation_after),
              only_uncertain = min(only_uncertain)) %>% 
    mutate(length_time = Date_to-Date_from) %>% 
    group_by(idU) %>% 
    mutate(n_tot = n(),
           rank = row_number(),
           Date_to = if_else(n_tot == rank & Date_last <= Date_to, Date_last, Date_to)) %>% 
    ungroup() %>% 
    select(-idloc,n_tot,rank) %>% 
    relocate(colnames)
  
  intervals_u_clean <- intervals_u_clean %>% 
    group_by(idU) %>% 
    mutate(n_acts_lead = lead(n_acts),
           n_acts_lag = lead(n_acts)) %>% 
    left_join(select(dist_matrix,distance=distance,CodeLocation_before=Par_A,CodeLocation=Par_B)) %>%
    group_by(idU,CodeLocation) %>% 
    mutate(tot_acts = sum(n_acts)) %>% 
    group_by(idU) %>% 
    mutate(tot_acts_lag = lag(tot_acts)) %>% 
    mutate(del = if_else(n_acts == 1 & ((n_acts_lag > 1 | n_acts_lead > 1) & tot_acts < tot_acts_lag) & CodeLocation_before == CodeLocation_after,1,0,missing=0)) %>% 
    mutate(loc_lead = lead(CodeLocation),
           loc_lag = lag(CodeLocation)) %>% 
    filter(!(del == 1 & distance >= dist_thresh)) %>% 
    mutate(idloc = consecutive_id(CodeLocation)) %>% 
    group_by(idU,idloc) %>% 
    summarize(CodeLocation = first(CodeLocation),
              Date_from = first(Date_from),
              Date_to = last(Date_to),
              Date_first = first(Date_first),
              Date_last = last(Date_last),
              n_acts = sum(n_acts),
              CodeLocation_before = first(CodeLocation_before),
              CodeLocation_after = last(CodeLocation_after),
              del = first(del),
              only_uncertain = min(only_uncertain)) %>% 
    mutate(del_lag = lag(del)) %>% 
    filter(del != 1) %>% 
    mutate(idloc = consecutive_id(CodeLocation)) %>% 
    group_by(idU,idloc) %>% 
    summarize(CodeLocation = first(CodeLocation),
              Date_from = first(Date_from),
              Date_to = last(Date_to),
              Date_first = first(Date_first),
              Date_last = last(Date_last),
              n_acts = sum(n_acts)+sum(del_lag,na.rm=T),
              CodeLocation_before = first(CodeLocation_before),
              CodeLocation_after = last(CodeLocation_after),
              only_uncertain = min(only_uncertain)) %>% 
    mutate(length_time = Date_to-Date_from) %>% 
    group_by(idU) %>% 
    mutate(n_tot = n(),
           rank = row_number(),
           Date_to = if_else(n_tot == rank & Date_last <= Date_to, Date_last, Date_to)) %>% 
    ungroup() %>% 
    select(-idloc,n_tot,rank) %>% 
    relocate(colnames)
  
  write_csv(intervals_u_clean,"register/intervals_u_clean.csv")
  
  time_min <- (proc.time() - ptm)[3]/60

  print(paste("Cleaned in ",time_min, " minutes."))
}

intervals_u_clean <- clean_unions(df = intervals_u)

rm(list = ls())
