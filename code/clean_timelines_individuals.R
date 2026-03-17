# Côté-Gendreau 2026
# This file cleans individual timelines from 'outlier' records

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

intervals_i <- read_csv("register/intervals_i.csv")
inds <- read_csv("data_modif/inds_DOB_est.csv",guess_max = 5000)

intervals_i <- intervals_i %>% 
  left_join(select(inds,DOD,idI=idInd)) %>% 
  mutate(Date_to = dplyr::if_else(Date_to > DOD, DOD, Date_to, missing = Date_to),
         right = dplyr::if_else(right == "error" & Date_to == DOD, "DOD", right, missing = right))

dist_matrix <- read_csv("data_modif/dist_matrix.csv") %>% 
  pivot_longer(cols = -Par_A, names_to = "Par_B", values_to = "distance") %>% 
  mutate_all(~as.numeric(.x))

clean_register_individual <- function(dist_thresh = 10, interpolation = 0.5) {
  
  ptm <- proc.time()
  
  intervals_i <- intervals_i %>% 
    mutate(across(c(premarriage,childhood,youth,adulthood,idU,only_uncertain), ~if_else(is.na(.x),0,.x))) %>% 
    select(idI:right)
  
  intervals_i_clean <- intervals_i %>% 
    group_by(idI) %>% 
    mutate(CodeLocation_before = lag(CodeLocation),
           CodeLocation_after = lead(CodeLocation),
           n_acts_lead = lead(n_acts),
           n_acts_lag = lead(n_acts),
           idU_lead = lead(idU),
           idU_lag = lag(idU),
           border = if_else(idU != idU_lead | idU != idU_lag,1,0,missing=0)) %>% 
    group_by(idI,idU) %>% 
    mutate(Date_to = dplyr::if_else(idU != idU_lag & n_acts == 1 & idU != 0, Date_from, Date_to, missing = Date_to),
           Date_to_lag = lag(Date_to),
           Date_from = dplyr::if_else(!is.na(Date_to_lag) & Date_from != Date_to_lag + ddays(1), Date_to_lag + ddays(1), Date_from)) %>% 
    left_join(select(dist_matrix,distance=distance,CodeLocation_before=Par_A,CodeLocation_after=Par_B)) %>%
    group_by(idI) %>% 
    mutate(Date_from_lead = lead(Date_from)) %>% 
    mutate(del = dplyr::if_else(border == 1 & (n_acts == 1 | Date_first > Date_from_lead) & (n_acts_lag > 1 | n_acts_lead > 1) & CodeLocation_before == CodeLocation_after,1,0,missing=0)) %>% 
    filter(!(del == 1 & distance >= dist_thresh)) %>% 
    mutate(idloc = consecutive_id(CodeLocation)) %>% 
    group_by(idI,idloc) %>% 
    summarize(CodeLocation = first(CodeLocation),
              Date_from = first(Date_from),
              Date_to = last(Date_to),
              Date_first = first(Date_first),
              Date_last = last(Date_last),
              n_acts = sum(n_acts),
              CodeLocation_before = first(CodeLocation_before),
              CodeLocation_after = last(CodeLocation_after),
              del = first(del),
              only_uncertain = min(only_uncertain),
              childhood = max(childhood),
              adulthood = max(adulthood),
              youth = max(youth),
              premarriage = max(premarriage),
              left = first(left),
              right = last(right),
              idU_first = first(idU),
              idU_last = last(idU)) %>% 
    mutate(del_lag = lag(del)) %>% 
    filter(del != 1) %>% 
    mutate(idloc = consecutive_id(CodeLocation)) %>% 
    group_by(idI,idloc) %>% 
    summarize(CodeLocation = first(CodeLocation),
              Date_from = first(Date_from),
              Date_to = last(Date_to),
              Date_first = first(Date_first),
              Date_last = last(Date_last),
              n_acts = sum(n_acts)+sum(del_lag,na.rm=T),
              CodeLocation_before = first(CodeLocation_before),
              CodeLocation_after = last(CodeLocation_after),
              only_uncertain = min(only_uncertain),
              childhood = max(childhood),
              adulthood = max(adulthood),
              youth = max(youth),
              premarriage = max(premarriage),
              left = first(left),
              right = last(right),
              idU_first = first(idU_first),
              idU_last = last(idU_last)) %>% 
    group_by(idI) %>% 
    mutate(Date_from_lead = lead(Date_from),
           childhood_lead = lead(childhood),
           adulthood_lead = lead(adulthood),
           gap = Date_from_lead - Date_to) %>% 
    mutate(Date_to = dplyr::case_when(is.na(Date_from_lead) | Date_to == Date_from_lead - ddays(1) ~ Date_to,
                               !is.na(adulthood_lead) & adulthood_lead == 1 & childhood == 1 ~ Date_from_lead - ddays(1),
                               T ~ Date_to + gap*interpolation)) %>% 
    mutate(Date_to_lag = lag(Date_to),
           Date_from = dplyr::case_when(is.na(Date_to_lag) | Date_from == Date_to_lag + ddays(1) ~ Date_from,
                                 T ~ Date_to_lag + ddays(1)),
           length_time = time_length(Date_to-Date_from,"days")) %>% 
    ungroup() %>% 
    select(idI, idU_first, idU_last, CodeLocation, Date_from, Date_to, length_time, n_acts, Date_first, Date_last, CodeLocation_before, CodeLocation_after, 
           only_uncertain, premarriage, childhood, youth, adulthood,
           left,right)
  
  write_csv(intervals_i_clean,"register/intervals_i_clean.csv") 
  
  time_min <- (proc.time() - ptm)[3]/60

  print(paste("Cleaned in ",time_min, " minutes."))
}

clean_register_individual()

rm(list = ls())
