# Côté-Gendreau 2026
# This file makes lists of individual and family moves
## with variables that indicate whether it is a new location or not in the family history

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

# Download necessary data

unions_bounds <- read_csv("data_modif/unions_bounds_mod.csv")

inds <- read_csv("data_modif/inds_DOB_est.csv", guess_max = 5000) %>% 
  filter(idInd %in% unions_bounds$idHusband | idInd %in% unions_bounds$idWife) %>% 
  select(idInd,idFather,idMother,DOB,DOB_mean,DateU_first,DOD,Sex)

intervals_i <- read_csv("register/intervals_i_clean.csv")
intervals_u <- read_csv("register/intervals_u_clean.csv") %>% 
  left_join(select(unions_bounds,idU=idUnion,idHusband,idWife))

dist_matrix <- read_csv("data_modif/dist_matrix.csv") %>% 
  pivot_longer(cols = -Par_A, names_to = "Par_B", values_to = "distance") %>% 
  mutate_all(~as.numeric(.x))

# INDIVIDUAL MOVES

intervals_i <- intervals_i %>% 
  group_by(idI) %>% 
  mutate(CodeLocation_before = lag(CodeLocation),
         CodeLocation_after = lead(CodeLocation),
         Date_last_before = lag(Date_last)) %>% 
  ungroup()

# Get whether new location in own life, parent life, grandparent life

intervals_i <- intervals_i %>% 
  dplyr::left_join(inds %>% select(-Sex), join_by(idI == idInd)) %>% 
  dplyr::left_join(inds %>% filter(Sex == "m") %>% 
                     select(idInd, idPGF = idFather, idPGM = idMother),
                   join_by(idFather == idInd)) %>% 
  dplyr::left_join(inds %>% filter(Sex == "f") %>% 
                     select(idInd, idMGF = idFather, idMGM = idMother),
                   join_by(idMother == idInd)) %>% 
  group_by(idI,CodeLocation) %>% 
  mutate(new_loc_own = if_else(row_number() >= 2, 0, 1)) %>% 
  ungroup()

intervals_i <- intervals_i %>% 
  mutate(idInt = row_number())

intervals_i_long <- intervals_i %>%
  pivot_longer(cols = c(idFather, idMother, idPGF, idPGM, idMGF, idMGM),
               names_to = "relation", values_to = "idAsc",
               names_prefix = "id") %>% 
  select(idInt, idI, CodeLocation, Date_last_before, relation, idAsc)

intervals_i_anc <- intervals_i_long %>%
  dplyr::left_join(intervals_i %>% select(idI, CodeLocation, Date_first_asc = Date_first),
                   join_by(idAsc == idI, CodeLocation, Date_last_before > Date_first_asc),
                   multiple = "first") %>% 
  mutate(new_loc_asc = if_else(is.na(Date_first_asc), 1, 0)) %>% 
  select(idInt, relation, new_loc_asc) %>% 
  pivot_wider(names_from = relation, values_from = new_loc_asc, values_fill = 1, 
              names_glue = "new_loc_{relation}") %>% 
  mutate(new_loc_p = pmin(new_loc_Father,new_loc_Mother),
         new_loc_gp = pmin(new_loc_PGF,new_loc_PGM,new_loc_MGF,new_loc_MGM),
         new_loc_asc = pmin(new_loc_p,new_loc_gp))

intervals_i <- intervals_i %>% 
  left_join(intervals_i_anc)

# Get moves

moves_i <- intervals_i %>% 
  mutate(childhood_lead = lead(childhood),
         adulthood_lead = lead(adulthood),
         childhood_move = if_else(childhood == 1 & childhood_lead == 1,1,0),
         premarital_move = if_else(childhood == 1 & adulthood_lead == 1, 1, 0),
         adulthood_move = if_else(adulthood == 1 & adulthood_lead == 1,1,0),
         new_loc_own = lead(new_loc_own),
         new_loc_p = lead(new_loc_p),
         new_loc_gp = lead(new_loc_gp),
         new_loc_asc = lead(new_loc_asc)) %>% 
  filter(!is.na(CodeLocation_after)) %>% 
  mutate(Date_move = Date_to) %>% 
  select(idI, Par_A = CodeLocation, Par_B = CodeLocation_after, Date_move, childhood_move, adulthood_move, premarital_move,
         new_loc_own, new_loc_p, new_loc_gp, new_loc_asc) %>%
  left_join(select(dist_matrix,distance,Par_A,Par_B)) %>% 
  write_csv("register/moves_i.csv")

# FAMILY MOVES

# Get whether new location in own life, parent life, grandparent life

inds_short <- inds %>% 
  select(idInd,idFather,idMother,Sex)

intervals_u <- intervals_u %>% 
  dplyr::left_join(inds_short %>% select(-Sex) %>% rename_with(~ paste0(.x,"_man")), 
            by = c("idHusband" = "idInd_man")) %>% 
  dplyr::left_join(inds_short %>% select(-Sex) %>% rename_with(~ paste0(.x,"_woman")), 
            by = c("idWife" = "idInd_woman")) %>% 
  dplyr::left_join(inds %>% filter(Sex == "m") %>% 
              select(idInd, idPGF_man = idFather, idPGM_man = idMother),
            by = c("idFather_man" = "idInd")) %>% 
  dplyr::left_join(inds %>% filter(Sex == "f") %>% 
              select(idInd, idMGF_man = idFather, idMGM_man = idMother),
            by = c("idMother_man" = "idInd")) %>% 
  dplyr::left_join(inds %>% filter(Sex == "m") %>% 
              select(idInd, idPGF_woman = idFather, idPGM_woman = idMother),
            by = c("idFather_woman" = "idInd")) %>% 
  dplyr::left_join(inds %>% filter(Sex == "f") %>% 
              select(idInd, idMGF_woman = idFather, idMGM_woman = idMother),
            by = c("idMother_woman" = "idInd"))

intervals_u <- intervals_u %>% 
  mutate(idInt = row_number()) %>% 
  group_by(idU) %>% 
  mutate(CodeLocation_before = lag(CodeLocation),
         CodeLocation_after = lead(CodeLocation),
         Date_last_before = lag(Date_last)) %>% 
  ungroup()

intervals_u_long <- intervals_u %>%
  pivot_longer(cols = c(idHusband,idWife,
                        idFather_man, idMother_man, idPGF_man, idPGM_man, idMGF_man, idMGM_man,
                        idFather_woman, idMother_woman, idPGF_woman, idPGM_woman, idMGF_woman, idMGM_woman),
               names_to = "relation", values_to = "idAsc",
               names_prefix = "id") %>% 
  select(idInt, idU, CodeLocation, Date_last_before, relation, idAsc)

intervals_u_anc <- intervals_u_long %>%
  dplyr::left_join(intervals_i %>% select(idI, CodeLocation, Date_first_asc = Date_first),
            join_by(idAsc == idI, CodeLocation, Date_last_before > Date_first_asc),
            multiple = "first") %>% 
  mutate(new_loc_asc = if_else(is.na(Date_first_asc), 1, 0)) %>% 
  select(idInt, relation, new_loc_asc) %>% 
  pivot_wider(names_from = relation, values_from = new_loc_asc, values_fill = 1, 
              names_glue = "new_loc_{relation}") %>% 
  mutate(new_loc_p_man = pmin(new_loc_Father_man,new_loc_Mother_man),
         new_loc_p_woman = pmin(new_loc_Father_woman,new_loc_Mother_woman),
         new_loc_gp_man = pmin(new_loc_PGF_man,new_loc_PGM_man,new_loc_MGF_man,new_loc_MGM_man),
         new_loc_gp_woman = pmin(new_loc_PGF_woman,new_loc_PGM_woman,new_loc_MGF_woman,new_loc_MGM_woman),
         new_loc_own = pmin(new_loc_Husband,new_loc_Wife),
         new_loc_asc = pmin(new_loc_p_man,new_loc_p_woman,new_loc_gp_man,new_loc_gp_woman),
         new_loc = pmin(new_loc_own,new_loc_asc))

intervals_u <- intervals_u %>% 
  left_join(intervals_u_anc)

moves_u <- intervals_u %>% 
  group_by(idU) %>% 
  mutate(across(starts_with("new_loc"), ~lead(.x))) %>% 
  filter(!is.na(CodeLocation_after)) %>% 
  mutate(Date_move = Date_to) %>% 
  select(idU, Par_A = CodeLocation, Par_B = CodeLocation_after, Date_move,
         new_loc_own, new_loc_asc, new_loc) %>%
  left_join(select(dist_matrix,distance,Par_A,Par_B)) %>% 
  write_csv("register/moves_u_analysis.csv")

rm(list = ls())
