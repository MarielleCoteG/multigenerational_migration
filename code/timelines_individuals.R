# Côté-Gendreau 2026
# This file combines family timelines and non-family records (e.g. deaths of single individuals) to build individual timelines

# Packages we need
list_packages <- c("dplyr","readr","tidytable","lubridate","ecospace")

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

build_interval_ind <- function(interpolation = 0.5, df = NULL) {
  
  sort <- df
  
  if (length(unique(sort$CodeLocation)) == 1) {
    
    intervals_i <- sort %>% 
      dplyr::summarize(CodeLocation = first(CodeLocation),
                Date_from = first(Date_mod_precise),
                Date_to = last(Date_mod_precise),
                length_time = time_length(Date_to - Date_from,"days"),
                n_acts = dplyr::n()-1) %>% 
      mutate(Date_first = Date_from,
             Date_last = Date_to) %>% 
      select(CodeLocation,Date_from,Date_to,length_time,n_acts,Date_first,Date_last)
    
    return(intervals_i)
  }
  
  sort <- sort %>% 
    select(CodeLocation,Date_mod_precise,parish_opening) %>% 
    mutate(loc_lead = lead(CodeLocation),
           loc_lag = lag(CodeLocation)) %>% 
    mutate(A = as.integer(loc_lead != CodeLocation & !is.na(loc_lead)),
           B = as.integer(loc_lag != CodeLocation & !is.na(loc_lag))) %>% 
    mutate(id_loc = consecutive_id(CodeLocation))
  
  sort <- sort %>% 
    group_by(id_loc) %>% 
    mutate(n_acts = n(),
           Date_first = first(Date_mod_precise),
           Date_last = last(Date_mod_precise)) %>% 
    ungroup() %>% 
    filter(A == 1 | B == 1 | is.na(loc_lead) | is.na(loc_lag)) %>% 
    mutate(across(c(Date_mod_precise,CodeLocation,parish_opening), ~lead(.x),
                  .names = "{.col}_lead")) %>% 
    mutate(Date_mod_precise_lag = lag(Date_mod_precise)) %>% 
    filter(A == 1 | is.na(loc_lead) | is.na(loc_lag)) %>% 
    mutate(gap = dplyr::if_else(A == 1, Date_mod_precise_lead - Date_mod_precise, NA),
           gap_opening = dplyr::if_else(A == 1 & dplyr::between(parish_opening_lead, Date_mod_precise, Date_mod_precise_lead), Date_mod_precise_lead - parish_opening_lead, NA)) %>% 
    mutate(Date_move = pmin(dplyr::if_else(is.na(gap_opening),
                                    as.Date(Date_mod_precise + interpolation*gap),
                                    as.Date(parish_opening_lead + interpolation*gap_opening)),
                            Date_mod_precise_lead))
  
  intervals_i <- sort %>% 
    mutate(Move_lag = lag(Date_move)) %>% 
    filter(A == 1 | is.na(loc_lead)) %>% 
    mutate(Date_from = dplyr::case_when(is.na(Move_lag) & is.na(Date_mod_precise_lag) ~ Date_mod_precise,
                                 is.na(Move_lag) ~ Date_mod_precise_lag, 
                                 T ~ Move_lag+ddays(1)),
           Date_to = dplyr::if_else(is.na(Date_move),Date_mod_precise,Date_move)) %>% 
    select(CodeLocation,Date_from,Date_to,n_acts,Date_first,Date_last) %>% 
    mutate(CodeLocation_before = lag(CodeLocation),
           CodeLocation_after = lead(CodeLocation),
           length_time = time_length(Date_to - Date_from,"days"))
  
  return(intervals_i)
}

register_ind <- function(idI, age_young_f = 14, age_young_m = 14, interpolation = 0.5, silent = F) {
  
  ind <- inds_for_intervals %>% filter(idInd == idI)
  
  if (silent == F & ind$i %% 5000 == 0) {
    print(ind$i)
  }
  
  DOD <- ind$DOD
  DOD_min <- ind$DOD_min
  DOD_unif <- dplyr::if_else(is.na(DOD),DOD_min,DOD)
  
  if (is.na(DOD_unif)) {
    
    skeleton <- tibble(idI=integer(),idU=integer(),CodeLocation=integer(),
                       Date_from=date(),Date_to=date(),
                       length_time=integer(),n_acts=integer(),
                       Date_first=date(),Date_last=date(),
                       only_uncertain=integer(),premarriage=integer(),childhood=integer(),youth=integer(),adulthood=integer(),
                       rank=integer(),left=character(),right=character())
    return(skeleton)
  }
  
  DOB <- ind$DOB
  DOB_max <- ind$DOB_max
  DOB_mean <- ind$DOB_mean
  DOB_unif <- dplyr::if_else(is.na(DOB),dplyr::if_else(is.na(DOB_mean),DOB_max,DOB_mean), DOB)
  idFather <- ind$idFather
  idMother <- ind$idMother
  idU_own <- unions_long %>% filter(idInd == idI) %>% pull(idUnion)
  idU_parents <- unions %>% filter((idHusband == idFather | idWife == idMother) & (Date_end >= DOB_unif | is.na(Date_end))) %>% pull(idUnion)
  Sex = ind$Sex
  
  premarriage <- tibble(premarriage=integer())
  childhood <- tibble(childhood=integer())
  youth <- tibble(youth=integer())
  adulthood <- tibble(adulthood=integer())

  # First case: unmarried
  if(!is.na(DOB_unif) & length(idU_own) == 0 & length(idU_parents) != 0) {
    
    end_childhood <- dplyr::if_else(Sex=="f",age_young_f,age_young_m)
    end_childhood <- min(DOD_unif,as.Date(DOB_unif+dyears(end_childhood)-ddays(1)),na.rm=T)
    
    childhood <- intervals_u %>% 
      select(idU,CodeLocation,Date_from,Date_to,length_time,n_acts,Date_first,Date_last,only_uncertain,
             parish_opening) %>% 
      filter(idU %in% idU_parents) %>% 
      filter(Date_to >= DOB_unif) %>% 
      filter(Date_from <= end_childhood) %>% 
      mutate(lag = Date_from - lag(Date_to)) %>% 
      mutate(Date_from = dplyr::if_else(row_number() == 1 & Date_from <= DOB_unif, DOB_unif, Date_from),
             Date_to = dplyr::if_else(n() == row_number() & Date_to >= end_childhood, end_childhood, Date_to),
             childhood = 1,
             adulthood = 0) 
    
    if (nrow(childhood) != 0) {
      
      if(is.finite(min(childhood$lag,na.rm=T)) & min(childhood$lag,na.rm=T) < 0) {
        inds_error <- c(inds_error,idI)
        childhood <- tibble(childhood=integer())
      }
    }
    
    if (nrow(childhood) != 0) {
      
      if (end_childhood < DOD_unif | childhood %>% slice(n()) %>% pull(Date_to) < DOD_unif) {
        
        recs <- mentions_single %>% 
          filter(idInd == idI, Date_mod_precise > end_childhood) %>% 
          select(Date_mod_precise,CodeLocation,parish_opening)
        
        youth <- childhood %>% 
          slice(n()) %>% 
          mutate(Date_mod_precise = Date_to + ddays(1)) %>% 
          select(Date_mod_precise,CodeLocation,parish_opening) %>% 
          bind_rows(recs)
        
        youth <- build_interval_ind(df = youth, interpolation = interpolation) %>% 
          mutate(youth = 1,
                 Date_to = dplyr::if_else(Date_to > DOD_unif, DOD_unif, Date_to))
      }
    }
  }
  
  # Second case: married
  if (length(idU_own) != 0) {
    
    adulthood <- intervals_u %>% 
      filter(idU %in% idU_own) %>% 
      filter(is.na(DOD_unif) | Date_from <= DOD_unif) %>% 
      mutate(Date_to = dplyr::if_else(n() == row_number() & !is.na(DOD_unif) & Date_to >= DOD_unif, DOD_unif, Date_to)) %>% 
      mutate(lag = Date_from - lag(Date_to),
             childhood = 0,
             adulthood = 1)
    
    if(nrow(adulthood) != 0) { 
      
      if (length(idU_parents) != 0 & !is.na(DOB_unif)) {
        
        fm_date <- adulthood %>% slice(1) %>% pull(Date_from)
        
        if (is.finite(min(adulthood$lag,na.rm=T)) &  min(adulthood$lag,na.rm=T) < 0) {
          inds_error <- c(inds_error,idI)
          adulthood <- tibble(adulthood=integer())
        }
        
        end_childhood <- dplyr::if_else(Sex=="f",age_young_f,age_young_m)
        end_childhood <- min(DOD_unif,DOB_unif+dyears(end_childhood)-ddays(1))
        
        recs <- mentions_single %>% 
          filter(idInd == idI, Date_mod_precise > end_childhood, Date_mod_precise < fm_date) %>% 
          select(Date_mod_precise,CodeLocation,parish_opening)
        
        if (nrow(recs) != 0 & length(idU_parents) != 0) {
          
          childhood <- intervals_u %>% 
            select(idU,CodeLocation,Date_from,Date_to,length_time,n_acts,Date_first,Date_last,only_uncertain,parish_opening) %>% 
            filter(idU %in% idU_parents) %>% 
            filter(Date_to >= DOB_unif) %>% 
            filter(Date_from <= end_childhood) %>% 
            mutate(lag = Date_from - lag(Date_to)) %>% 
            mutate(Date_from = dplyr::if_else(row_number() == 1 & Date_from < DOB_unif, DOB_unif, Date_from),
                   Date_to = dplyr::if_else(n() == row_number() & Date_to >= end_childhood, end_childhood, Date_to),
                   childhood = 1,
                   adulthood = 0)
          
          if (is.finite(min(childhood$lag,na.rm=T)) & min(childhood$lag,na.rm=T) < 0) {
            inds_error <- c(inds_error,idI)
            childhood <- tibble(chilhood=integer())
          }
          
          if (nrow(childhood) != 0) {
            
            youth <- childhood %>% 
              slice(n()) %>% 
              mutate(Date_mod_precise = Date_to + ddays(1)) %>% 
              select(Date_mod_precise,CodeLocation,parish_opening) %>% 
              bind_rows(recs)
            
            youth <- build_interval_ind(df = youth, interpolation = interpolation) %>% 
              mutate(youth = 1)
          }
        } 
        
        if (nrow(recs) != 0 & nrow(childhood) == 0) {
          
          youth <- adulthood %>% 
            slice(1) %>% 
            mutate(Date_mod_precise = Date_from - ddays(1)) %>% 
            select(Date_mod_precise,CodeLocation,parish_opening)
          
          youth <- bind_rows(recs,youth)
          
          youth <- build_interval_ind(df = youth, interpolation = interpolation) %>% 
            mutate(youth = 1)
          
        } else if (nrow(recs) == 0 & length(idU_parents != 0)) {
          
          childhood <- intervals_u %>% 
            filter(idU %in% idU_parents) %>% 
            filter(Date_to >= DOB_unif) %>% 
            filter(Date_from < fm_date) %>% 
            mutate(lag = Date_from - lag(Date_to)) %>% 
            mutate(Date_from = dplyr::if_else(row_number() == 1 & Date_from < DOB_unif, DOB_unif, Date_from),
                   Date_to = dplyr::if_else(n() == row_number() & Date_to >= fm_date, fm_date-ddays(1), Date_to),
                   childhood = 1,
                   adulthood = 0)
          
          if (is.finite(min(childhood$lag,na.rm=T)) & min(childhood$lag,na.rm=T) < 0) {
            inds_error <- c(inds_error,idI)
            childhood <- tibble(childhood=integer())
          }
        }
      }
    }
  }
  
  # if illegitimate
  if (nrow(childhood) != 0) {
    if (childhood %>% slice(1) %>% pull(Date_from) != DOB & !is.na(DOB)) {
      
      rec <- ind$idAct_b
      
      if (!is.na(rec)) {
        
        rec <- mentions %>% 
          select(CodeLocation,Date_mod_precise,idInd,idAct,Role_revised,parish_opening) %>% 
          filter(idAct == rec,Role_revised == 1) %>% 
          select(-c(idAct,Role_revised))
        
        if (!is.na(rec$Date_mod_precise) & rec$Date_mod_precise == childhood %>% slice(1) %>% pull(Date_from)) {
          
          childhood <- childhood %>% 
            mutate(Date_from = dplyr::if_else(Date_from == rec$Date_mod_precise, DOB_unif, Date_from))
          
        } else {
          
          premarriage <- childhood %>% 
            slice(1) %>% 
            mutate(Date_mod_precise = Date_from - ddays(1)) %>% 
            select(Date_mod_precise,CodeLocation,parish_opening) 
          
          premarriage <- bind_rows(rec,premarriage)
          
          premarriage <- build_interval_ind(df = premarriage) %>% 
            mutate(premarriage = 1)
        }
      }
    }
  }
  
  
  
  interval_i <- bind_cols(idI = idI, bind_rows(premarriage,childhood,youth,adulthood))
  
  if (nrow(interval_i) != 0) {
    interval_i <- interval_i %>% 
      mutate(rank = row_number()) %>% 
      mutate(left = dplyr::case_when(rank == 1 & Date_from == DOB ~ "DOB",
                              rank == 1 & Date_from == DOB_mean ~ "DOB_mean",
                              rank == 1 & Date_from == DOB_max ~ "DOB_max",
                              rank == 1 & adulthood == 1 ~ "FM",
                              rank == 1 & youth == 1 ~ "premarital",
                              rank == 1 & childhood == 1 & Date_from > DOB ~ "illegitimate",
                              rank == 1 ~ "error",
                              T ~ NA),
             right = dplyr::case_when(rank == n() & Date_to == DOD ~ "DOD",
                               rank == n() & Date_to == DOD_min ~ "DOD_min",
                               rank == n() ~ "error",
                               T ~ NA)) %>% 
      select(idI,idU,CodeLocation,Date_from,Date_to,length_time,n_acts,
             Date_first,Date_last,
             only_uncertain,premarriage,childhood,youth,adulthood,rank,left,right)
      
  }
  
  return(as.data.frame(interval_i))
}

load_register_individual <- function(age_young_f = 14, age_young_m = 14) {
  
  inds <<- read_csv("data_modif/inds_DOB_est.csv", guess_max = 5000)
  parishes <- read_csv("data_modif/parishes.csv") %>% 
    mutate(parish_opening = ymd(paste0(year_par,"0101")),
           parish_opening = dplyr::if_else(is.na(parish_opening),ymd("16000101"),parish_opening))
  mentions <<- read_csv("data_modif/mentions_mod.csv") %>% 
    filter(!(CodeLocation %in% c(0,22222,2,7600,6909,7900:7904)) & (CodeLocation < 8000 | CodeLocation >= 20000)) %>% 
    filter(idInd != -1) %>% 
    mutate(Date_mod_precise = ymd(Date_mod_precise)) %>% 
    left_join(select(parishes,CodeLocation,parish_opening)) 
  unions <<- read_csv("data_modif/unions_bounds_mod.csv")
  unions_long <<- unions %>% 
    pivot_longer(c(idHusband,idWife),names_to = "Spouse", values_to = "idInd")
  intervals_u <<- read_csv("register/intervals_u_clean.csv") %>% 
    left_join(select(parishes,CodeLocation,parish_opening)) %>% 
    arrange(Date_from)
  
  unions_in_int <- unions %>% 
    filter(idUnion %in% intervals_u$idU)
  
  inds_union_first <- unions_in_int %>% 
    dplyr::left_join(select(intervals_u,idU,Date_from),join_by(idUnion == idU),multiple="first") %>% 
    pivot_longer(c(idHusband,idWife),values_to="idInd",names_to="Gender") %>% 
    arrange(Date_from) %>% 
    distinct(idInd,.keep_all=T)
  
  inds_union_last <- unions_in_int %>% 
    dplyr::left_join(select(intervals_u,idU,Date_to),join_by(idUnion == idU),multiple="last") %>% 
    pivot_longer(c(idHusband,idWife),values_to="idInd",names_to="Gender") %>% 
    arrange(desc(Date_to)) %>% 
    distinct(idInd,.keep_all=T)
  
  bs_illeg <<- mentions %>% 
    filter(Type %in% c("b","s") & Role_revised %in% c(3,4)) %>% 
    dplyr::left_join(select(inds_union_first,Date_from,idInd), join_by(idInd == idInd), multiple = "first") %>% 
    mutate(across(c(Date_mod_precise,Date_from), ~ymd(.x))) %>% 
    filter(Date_mod_precise < Date_from | is.na(Date_from)) %>% 
    select(idAct,CodeLocation,Date_mod_precise,idInd)
  
  mentions_single <<- mentions %>% 
    select(Role_revised,idAct,CodeLocation,Date_mod_precise,idInd,parish_opening) %>% 
    filter(Role_revised == 1) %>% 
    left_join(select(inds_union_first,Date_from,idInd)) %>% 
    left_join(select(inds,idFather,idMother,idInd)) %>% 
    left_join(select(inds_union_last,Date_to_mother=Date_to,idMother=idInd)) %>% 
    left_join(select(inds_union_last,Date_to_father=Date_to,idFather=idInd)) %>% 
    mutate(across(c(Date_mod_precise,Date_from,Date_to_mother,Date_to_father), ~ymd(.x))) %>% 
    mutate(Date_to = pmax(Date_to_mother,Date_to_father,na.rm=T)) %>% 
    filter(Date_mod_precise < Date_from | is.na(Date_from) | Date_mod_precise > Date_to | is.na(Date_to)) %>% 
    left_join(select(inds,DOB,DOB_mean,DOB_max,idInd,Sex)) %>% 
    mutate(DOB_unif = dplyr::if_else(is.na(DOB),dplyr::if_else(is.na(DOB_mean),DOB_max,DOB_mean), DOB),
           DOB_unif = ymd(DOB_unif)) %>% 
    mutate(end_childhood = dplyr::if_else(Sex=="f",age_young_f,age_young_m)) %>% 
    mutate(cutoff = DOB_unif + dyears(end_childhood)) %>% 
    filter(Date_mod_precise >= cutoff | Date_mod_precise > Date_to) %>% 
    bind_rows(bs_illeg) %>% 
    select(idAct,CodeLocation,Date_mod_precise,idInd,parish_opening) 
  
  return()
}


register_individual <- function(age_young_f = 14, age_young_m = 14, interpolation = 0.5, slice = F, nslice = 1000, chunk = F, nchunk = 1) {
  
  ptm <- proc.time()
  
  load_register_individual(age_young_f = age_young_f, age_young_m = age_young_m)
  
  inds_error <<- c()
  
  if (slice == T) {
    inds <- slice_sample(inds,n=nslice)
  }
  
  if (chunk == T) {
    if (nchunk == 1) {
      inds <- slice(inds,1:floor(nrow(inds)/2))
    }
    
    if (nchunk == 2) {
      inds <- slice(inds,(floor(nrow(inds)/2)+1):nrow(inds))
    }
  }
  
  inds_for_intervals <<- inds %>% 
    filter(OutOfPop == 0) %>% 
    select(idInd,idFather,idMother,DOB,DOB_max,DOB_mean,DOD,DOD_min,Sex,idAct_b) %>% 
    filter(!is.na(DOD_min)) %>% 
    mutate(DOB_unif = dplyr::if_else(is.na(DOB),dplyr::if_else(is.na(DOB_mean),DOB_max,DOB_mean), DOB),
           DOD_unif = dplyr::if_else(is.na(DOD),DOD_min,DOD)) %>% 
    filter(DOB_unif <= DOD_unif) %>% 
    mutate(i = row_number())
  
  remove(inds)
  
  intervals_i <- lapply(inds_for_intervals$idInd, register_ind, interpolation = interpolation)
  
  intervals <- rbind_listdf(lists = intervals_i)
            
  if (slice == T) {
    file_name <- paste0("register/intervals_i_",nslice,".csv")
  } else if (chunk == T) {
    file_name <- paste0("register/intervals_i_",nchunk,".csv")
  } else {
    file_name <- paste0("register/intervals_i.csv")
  }
  
  write_csv(intervals,file_name)
  
  if (slice == T) {
    error_file_name <- paste0("register/inds_error_",nslice,".csv")
  } else if (chunk == T) {
    error_file_name <- paste0("register/inds_error_",nchunk,".csv")
  } else {
    error_file_name <- paste0("register/inds_error.csv")
  }
  
  inds_error <- inds_error %>% 
    as.data.frame() %>% 
    write_csv(error_file_name)
  
  time_hours <- (proc.time() - ptm)[3]/3600

  return(paste0("Success! Built timelines for ", length(unique(intervals$idI)), " individuals in ", round(time_hours,digits=2), " hours."))
}

register_individual()

rm(list = ls())
