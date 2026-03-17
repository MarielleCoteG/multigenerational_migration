# Côté-Gendreau 2026
# This file constructs the analytic samples with all the variables needed

# Packages we need
list_packages <- c("dplyr","readr","tidytable","geosphere","lubridate")

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

u <- read_csv("data_modif/unions_bounds_mod.csv")

inds <- read_csv("data_modif/inds_DOB_est.csv", guess_max = 5000) %>% 
  filter(idInd %in% u$idHusband | idInd %in% u$idWife) %>% 
  select(idInd,idFather,idMother,DOB,DOB_mean,flag_DOB_est,
         DOB_min,DOB_max,DateU_first,
         flag_DOD_est,DOD,DOD_max,Sex,
         Immigrant,nsibs_em_sex,brank_em_sex)

intervals_i <- read_csv("register/intervals_i_clean.csv")

moves_i <- read_csv("register/moves_i.csv")

dist_matrix <- read_csv("data_modif/dist_matrix.csv") %>% 
  pivot_longer(cols = -Par_A, names_to = "Par_B", values_to = "distance") %>% 
  mutate_all(~as.numeric(.x))

parishes <- read_csv("data_modif/parishes.csv") %>% 
  mutate(distance_Mtl = mapply(function(lng1, lat1) distm(c(-73.55681, 45.50466), c(lng1, lat1), fun=distHaversine), lng,lat)/1000,
         distance_Qc = mapply(function(lng1, lat1) distm(c(lng1, lat1), c(-71.2057, 46.81363), fun=distHaversine), lng,lat)/1000,
         closest_Mtl = distance_Mtl < distance_Qc)

moves_u <- read_csv("register/moves_u_analysis.csv")

intervals_u <- read_csv("register/intervals_u_clean.csv") %>% 
  left_join(select(u,idU = idUnion,idHusband,idWife))

# get first child-related events
bd <- read_csv("data_modif/bd_young.csv") %>% 
  select(idFather,idMother,Date_first_bd=Date_mod_precise) %>% 
  arrange(Date_first_bd)

# filter out moves identified before the first child-related event, which have to do with migration around the time of marriage
## which is hard to time and might in fact have occurred before marriage (i.e. not joint couple migration)
family_moves <- moves_u %>% 
  left_join(select(u,idU=idUnion,idHusband,idWife,DateU,DOD_husband,DOD_max_husband,DOD_wife,DOD_max_wife)) %>% 
  filter(Date_move <= pmin(DOD_husband,DOD_max_husband,DOD_wife,DOD_max_wife,ymd(19620101),na.rm=T)) %>% 
  select(-c(DOD_husband,DOD_max_husband,DOD_wife,DOD_max_wife)) %>% 
  dplyr::left_join(bd,join_by(idHusband==idFather,idWife==idMother),multiple="first") %>% 
  filter(Date_move >= Date_first_bd) %>% 
  select(-Date_first_bd)

# Function to build the sample of couples
## dist_thresh : distance threshold for a move to count as a migration
## window : number of years after marriage where we look for moves
## survival : number of years after marriage where we require both spouses to be observed
build_unions <- function(dist_thresh = 10, window = 20, survival = 10, save_without_survival = F) {

  # target moves
  target_moves_u <- family_moves %>% 
    filter(distance >= dist_thresh) %>% 
    filter(between(Date_move - DateU,dyears(0), dyears(window))) %>%
    select(idU, Par_A, Par_B, Date_move_u = Date_move, new_loc, new_loc_own, new_loc_asc) %>% 
    left_join(select(parishes,Par_A=CodeLocation,prov_A=province_state)) %>% 
    left_join(select(parishes,Par_B=CodeLocation,prov_B=province_state)) %>% 
    mutate(out_of_qc = if_else(prov_A != "Québec" | prov_B != "Québec", 1, 0)) %>% 
    dplyr::group_by(across(-c(prov_A, prov_B, Par_A, Par_B, out_of_qc, Date_move_u, new_loc, new_loc_own, new_loc_asc))) %>%
    dplyr::summarise(
      Date_move_u = first(Date_move_u),
      Par_A = first(Par_A),
      Par_B = first(Par_B),
      new_loc_own = max(new_loc_own, na.rm = TRUE),
      new_loc_asc = max(new_loc_asc, na.rm = TRUE),
      new_loc = max(new_loc, na.rm = TRUE),
      num_moves = dplyr::n(),
      out_of_qc = max(out_of_qc, na.rm = TRUE),
      .groups = "drop")
  
  if (dist_thresh == 10) {
    write_csv(target_moves_u,"data_modif/target_moves_u_10km.csv")
  }
  
  # find out if couples have ever migrated (including after the observation period)
  moves_ever <- family_moves %>% 
    filter(distance >= dist_thresh) %>% 
    filter(Date_move - DateU >= dyears(0)) %>% 
    dplyr::group_by(idU) %>% 
    dplyr::summarize(Date_move_ever = first(Date_move))
  
  # get maximum distance moved by couples (not necessarily their first move)
  moves_distance <- family_moves %>% 
    filter(between(Date_move - DateU,dyears(0), dyears(window))) %>% 
    select(idU,distance) %>% 
    dplyr::group_by(idU) %>% 
    dplyr::summarize(distance = max(distance))
  
  # get moves by individual instead of union (for parents and grandparents of focal couples)
  moves_u_long <- family_moves %>%
    filter(distance >= dist_thresh) %>% 
    select(-c(DateU,distance)) %>% 
    select(-idU) %>% 
    pivot_longer(c(idHusband,idWife),names_to = "Spouse", values_to = "idInd") %>% 
    select(-Spouse) %>% 
    dplyr::group_by(across(-c(Par_A, Par_B, Date_move, new_loc, new_loc_own, new_loc_asc))) %>%
    dplyr::summarize(Date_move_last = last(Date_move), 
                     Date_move_first = first(Date_move), 
                     Par_A = first(Par_A),
                     Par_B = first(Par_B),
                     new_loc = max(new_loc, na.rm = TRUE),
                     num_moves = dplyr::n(),
                     .groups = "drop") %>% 
    left_join(select(inds,idInd,DateU_first)) %>% 
    mutate(mig_u = if_else(between(Date_move_first - DateU_first,dyears(0), dyears(window)),1,0))
  
  # to get the age at childhood migration
  movers <- moves_i %>% 
    filter(distance >= dist_thresh) %>% 
    left_join(select(inds,idI=idInd,DateU_first))
  
  inds_mig <- inds %>% 
    filter(idInd %in% (intervals_i %>% pull(idI) %>% unique())) %>% 
    mutate(DOB = if_else(!is.na(DOB),DOB,DOB_mean)) %>% 
    dplyr::left_join(select(filter(movers,childhood_move == 1), idInd=idI, Date_move_ch = Date_move),
                     multiple = "last") %>% 
    mutate(Age_move_ch = time_length(Date_move_ch - DOB,"years"),
           mig_cat4 = case_when(Age_move_ch < 5 ~ "0-4",
                                Age_move_ch < 10 ~ "5-9",
                                Age_move_ch < 15 ~ "10-14",
                                !is.na(Age_move_ch) ~ "15-19",
                                T ~ "none")) %>% 
    dplyr::left_join(select(intervals_i,idInd=idI,end_obs = Date_to),
                     multiple = "last") %>% 
    mutate(obs_postmar = end_obs - DateU_first,
           obs_postmar = time_length(obs_postmar,"years")) %>% 
    left_join(select(moves_u_long,idInd,mig_u,Date_move_last)) %>%
    mutate(mig_u = if_else(is.na(mig_u),0,mig_u))
  
  inds_asc_mig <- select(inds_mig,idInd,idFather,idMother,
                         mig_u,Date_move_last,
                         imm=Immigrant)
  
  # attach migration information about ascendants
  inds_mig <- inds_mig %>% 
    dplyr::left_join(rename_with(inds_asc_mig,~ paste0(.x,"_f")), join_by("idFather" == "idInd_f")) %>% 
    dplyr::left_join(rename_with(inds_asc_mig,~ paste0(.x,"_m")), join_by("idMother" == "idInd_m")) %>% 
    dplyr::left_join(rename_with(inds_asc_mig,~ paste0(.x,"_pgf")), join_by("idFather_f" == "idInd_pgf")) %>% 
    dplyr::left_join(rename_with(inds_asc_mig,~ paste0(.x,"_pgm")), join_by("idMother_f" == "idInd_pgm")) %>% 
    dplyr::left_join(rename_with(inds_asc_mig,~ paste0(.x,"_mgf")), join_by("idFather_m" == "idInd_mgf")) %>% 
    dplyr::left_join(rename_with(inds_asc_mig,~ paste0(.x,"_mgm")), join_by("idMother_m" == "idInd_mgm")) %>% 
    mutate(across(c(idFather_f,idMother_f,idFather_m,idMother_m), ~if_else(.x==-1,NA,.x))) %>% 
    mutate(mig_p = if_else(mig_u_f == 1 | mig_u_m == 1, 1, 0),
           mig_prebirth_f = case_when(is.na(DOB) ~ NA,
                                      !is.na(Date_move_last_f) & Date_move_last_f < DOB  ~ 1,
                                      T ~ 0),
           mig_prebirth_m = case_when(is.na(DOB) ~ NA,
                                      !is.na(Date_move_last_m) & Date_move_last_m < DOB ~ 1,
                                      T ~ 0),
           mig_prebirth_p = pmax(mig_prebirth_f,mig_prebirth_m),
           mig_prebirth_p = if_else(mig_cat4 != "none",0,mig_prebirth_p),
           mig_pgp = pmax(mig_u_pgf,mig_u_pgm),
           mig_mgp = pmax(mig_u_mgf,mig_u_mgm),
           mig_cat4 = factor(mig_cat4, levels = c("none","0-4","5-9","10-14","15-19"), ordered = TRUE),
           mig_cat2 = case_when(mig_cat4 %in% c("0-4","5-9") ~ "0-9",
                                mig_cat4 %in% c("10-14","15-19") ~ "10-19",
                                T ~ "none"),
           mig_cat2_pb = case_when(mig_cat2 != "none" ~ mig_cat2,
                                   mig_prebirth_p == 1 ~ "prebirth",
                                   T ~ "none"),
           mig_cat2_pb = factor(mig_cat2_pb, levels = c("none","prebirth","0-9","10-19"), ordered = TRUE)) %>% 
    mutate(imm_gen = case_when(Immigrant == 1 ~ 1,
                               imm_f == 1 | imm_m == 1 ~ 2,
                               imm_pgm == 1 | imm_pgf == 1 | imm_mgf == 1 | imm_mgm == 1 ~ 3,
                               T ~ 4))
  
  inds_mig <<- inds_mig
  
  # attach all that individual-level and ascendant-level information to our focal couples
  
  unions <- u %>% 
    dplyr::inner_join(select(intervals_u,idU,start_obs_u = Date_from),join_by(idUnion == idU), multiple = "first") %>% 
    dplyr::inner_join(select(intervals_u,idU,end_obs_u = Date_to),join_by(idUnion == idU), multiple = "last") %>% 
    dplyr::inner_join(select(intervals_i,idI,end_obs_man = Date_to),join_by(idHusband == idI), multiple = "last") %>% 
    dplyr::inner_join(select(intervals_i,idI,end_obs_woman = Date_to),join_by(idWife == idI), multiple = "last") %>% 
    filter(rank_h == 1 & rank_w == 1) %>% 
    dplyr::inner_join(select(bd,idFather,idMother),join_by(idHusband==idFather,idWife==idMother),
                      multiple = "first") %>% 
    dplyr::inner_join(inds_mig %>% filter(Sex == "m") %>% 
                        select(idInd,idFather,idMother,idPGF = idFather_f, idPGM = idMother_f, idMGF = idFather_m, idMGM = idMother_m,
                               DOB,DOD,DOD_max,mig_u,mig_p,mig_prebirth_p,mig_pgp,mig_mgp,
                               mig_cat2_pb,obs_postmar,brank_em_sex,nsibs_em_sex,imm_gen) %>% 
                        rename_with(~ paste0(.x,"_man")),join_by(idHusband == idInd_man)) %>% 
    dplyr::inner_join(inds_mig %>% filter(Sex == "f") %>% 
                        select(idInd,idFather,idMother,idPGF = idFather_f, idPGM = idMother_f, idMGF = idFather_m, idMGM = idMother_m,
                               DOB,DOD,DOD_max,mig_u,mig_p,mig_prebirth_p,mig_pgp,mig_mgp,
                               mig_cat2_pb,obs_postmar,brank_em_sex,nsibs_em_sex,imm_gen) %>% 
                        rename_with(~ paste0(.x,"_woman")),join_by(idWife == idInd_woman)) %>% 
    drop_na(mig_pgp_man,mig_mgp_man,mig_pgp_woman,mig_mgp_woman,mig_p_man,mig_p_woman,mig_u_man,mig_u_woman) %>%
    left_join(select(target_moves_u,idUnion=idU,Par_A,Par_B,Date_move_u,new_loc_own,new_loc_asc,new_loc,out_of_qc,num_moves)) %>% 
    dplyr::left_join(select(moves_ever,idUnion=idU,Date_move_ever)) %>% 
    left_join(select(moves_distance,idUnion=idU,distance)) %>% 
    mutate(died_before_surv = if_else(time_length(pmin(DOD_man,DOD_max_man,DOD_woman,DOD_max_woman,ymd(20000101),na.rm=T) - start_obs_u, "years") >= survival, 0, 1))
  
  # apply all restrictions except for the survival one
  unions <- unions %>% 
    left_join(select(parishes,CodeLocation,province_state,year_parish=year_par,lng,lat,closest_Mtl)) %>% 
    mutate(year_mar = year(DateU),
           year_mar2 = year_mar**2, 
           year_parish2 = year_parish**2,
           mig_u = if_else(is.na(Date_move_u), 0, 1),
           mig_u_restr = if_else(mig_u == 1 & new_loc == 1, 1, 0),
           mar_out_qc = if_else(province_state == "Québec",0,1),
           out_of_qc = pmax(out_of_qc,mar_out_qc,na.rm=T),
           distance = if_else(is.na(distance),0,distance)) %>% 
    mutate(mig_gp_man = pmax(mig_pgp_man,mig_mgp_man),
           mig_gp_woman = pmax(mig_pgp_woman,mig_mgp_woman),
           mig_gpp_man = pmax(mig_pgp_man,mig_mgp_man,mig_prebirth_p_man),
           mig_gpp_woman = pmax(mig_pgp_woman,mig_mgp_woman,mig_prebirth_p_woman)) %>% 
    mutate(n_migp = mig_p_man + mig_p_woman,
           n_miggp = mig_pgp_man + mig_mgp_man + mig_pgp_woman + mig_mgp_woman) %>% 
    mutate(mig_cat2_pb_max = pmax(mig_cat2_pb_man, mig_cat2_pb_woman)) %>% 
    mutate(num_moves = if_else(is.na(num_moves),0,num_moves)) %>% 
    mutate(mig_u_restr = if_else(mig_u == 1 & new_loc_asc == 1 & new_loc_own == 1, 1, 0)) %>% 
    filter(is.na(out_of_qc) | out_of_qc == 0) %>%
    filter(QualityDate == 1 & !(CodeLocation %in% c(0,22222)))
  
  if (save_without_survival == T) {
    write_csv(unions,paste0("data_modif/unions_analysis_",dist_thresh,"km_",window,"_all.csv"))
  }
  
  # apply the final restriction and save this as the main data for analysis
  unions <- unions %>% 
    filter(time_length(pmin(Date_end,end_obs_u,ymd(18620101),na.rm=T) - start_obs_u, "years") >= survival) %>% 
    write_csv(paste0("data_modif/unions_analysis_",dist_thresh,"km_",window,"_",survival,".csv"))
  
}

build_unions(dist_thresh = 5, window = 20, survival = 10)
build_unions(dist_thresh = 25, window = 20, survival = 10)
build_unions(dist_thresh = 10, window = 20, survival = 20)
build_unions(dist_thresh = 10, window = 10, survival = 10)
build_unions(dist_thresh = 10, window = 20, survival = 10, save_without_survival = T)

unions_int <- read_csv("data_modif/unions_analysis_10km_20_10.csv")

pl_brothers <- unions_int %>% 
  arrange(DOB_man) %>% 
  mutate(id = dplyr::row_number()) %>% 
  filter(!is.na(DOB_man))

pl_brothers <- pl_brothers %>% 
  select(-c(ends_with("_woman"),idWife)) %>% 
  dplyr::left_join(select(pl_brothers,idWife_b = idWife,DOB_man_b = DOB_man,id_b = id,idFather_man,idMother_man), 
                   join_by(idFather_man == idFather_man,
                             idMother_man == idMother_man,
                             closest(id > id_b))) %>% 
  dplyr::left_join(select(pl_brothers,idWife_a = idWife,DOB_man_a = DOB_man,id_a = id,idFather_man,idMother_man), 
            join_by(idFather_man == idFather_man,
                    idMother_man == idMother_man,
                    closest(id < id_a))) %>% 
  mutate(gap_b = DOB_man-DOB_man_b,
         gap_a = DOB_man_a-DOB_man,
         idWife_pl = if_else(is.na(gap_a) | gap_b < gap_a, idWife_b, idWife_a,
                              missing = idWife_a)) %>% 
  left_join(select(unions_int,c(ends_with("_woman"),idWife_pl=idWife))) %>% 
  filter(!is.na(idWife_pl)) %>% 
  write_csv("data_modif/pl_brothers.csv")

pl_sisters <- unions_int %>% 
  select(-c(ends_with("_man"),idHusband)) %>% 
  arrange(DOB_woman) %>% 
  dplyr::left_join(select(unions_int,idHusband_b = idHusband,DOB_woman_b = DOB_woman,idFather_woman,idMother_woman), 
            join_by(idFather_woman == idFather_woman,
                    idMother_woman == idMother_woman,
                    closest(DOB_woman > DOB_woman_b))) %>% 
  dplyr::left_join(select(unions_int,idHusband_a = idHusband,DOB_woman_a = DOB_woman,idFather_woman,idMother_woman), 
            join_by(idFather_woman == idFather_woman,
                    idMother_woman == idMother_woman,
                    closest(DOB_woman < DOB_woman_a))) %>% 
  mutate(gap_b = DOB_woman-DOB_woman_b,
         gap_a = DOB_woman_a-DOB_woman,
         idHusband_pl = if_else(is.na(gap_a) | gap_b < gap_a, idHusband_b, idHusband_a,
                              missing = idHusband_a)) %>% 
  left_join(select(unions_int,c(ends_with("_man"),idHusband_pl=idHusband))) %>% 
  filter(!is.na(idHusband_pl)) %>% 
  write_csv("data_modif/pl_sisters.csv")

set.seed(12)

pl_local <- unions_int %>% 
  select(-c(ends_with("_woman"))) %>% 
  slice_sample(prop = 1, by = c(CodeLocation,year_mar)) %>%
  group_by(CodeLocation,year_mar) %>% 
  mutate(idWife_pl = lead(idWife)) %>% 
  filter(!is.na(idWife_pl)) %>% 
  left_join(select(unions_int,c(ends_with("_woman"),idWife_pl=idWife))) %>% 
  write_csv("data_modif/pl_local.csv")

pl_local_men <- unions_int %>% 
  select(-c(ends_with("_man"))) %>% 
  group_by(CodeLocation,year_mar) %>% 
  slice(sample(1:n())) %>% 
  mutate(idHusband_pl = lead(idHusband)) %>% 
  filter(!is.na(idHusband_pl)) %>% 
  left_join(select(unions_int,c(ends_with("_man"),idHusband_pl=idHusband))) %>% 
  write_csv("data_modif/pl_local_men.csv")

# Redefine migration by only comparing the first and last births of each family

mentions <- read_csv("data_modif/mentions_mod.csv")

b <- mentions %>% 
  filter(!(CodeLocation %in% c(0,22222)), Provenance %in% c("b","c","p")) %>% 
  select(-Role) %>% 
  rename(Role = Role_revised) %>% 
  filter(Type == "b", Role %in% c(1,3,4)) %>% 
  dplyr::add_count(idAct,Role) %>% 
  filter(nn < 2) %>% 
  select(idAct,CodeLocation,idInd,Role,Date_mod_precise) %>% 
  pivot_wider(id_cols = c(idAct,CodeLocation,Date_mod_precise), names_prefix = "Role_", names_from = Role, values_from = idInd) %>% 
  left_join(select(inds,Role_1=idInd,idFather,idMother)) %>% 
  mutate(Role_3 = if_else(is.na(Role_3) | Role_3 == -1, idFather, Role_3),
         Role_4 = if_else(is.na(Role_4) | Role_4 == -1, idMother, Role_4)) %>% 
  filter(is.finite(Role_3) & is.finite(Role_4),
         Role_3 != -1 & Role_4 != -1) %>% 
  mutate(Date_mod_precise = ymd(Date_mod_precise)) %>% 
  arrange(Date_mod_precise)

remove(mentions)

# same restrictions as in the main analytic sample
dist_thresh <- 10
survival <- 10

unions_alt <- u %>% 
  dplyr::inner_join(select(b, Role_3, Role_4, Date_fb = Date_mod_precise, CodeLocation_fb = CodeLocation),
             join_by(idHusband == Role_3, idWife == Role_4, DateU <= Date_fb),
             multiple = "first") %>% 
  dplyr::inner_join(select(b, Role_3, Role_4, Date_lb = Date_mod_precise, CodeLocation_lb = CodeLocation),
             join_by(idHusband == Role_3, idWife == Role_4, DateU <= Date_lb),
             multiple = "last") %>% 
  filter(Date_fb != Date_lb) %>% 
  dplyr::left_join(dist_matrix, join_by(CodeLocation_fb == Par_A, CodeLocation_lb == Par_B)) %>% 
  mutate(mig_u = if_else(distance >= dist_thresh, 1, 0)) %>% 
  filter(!is.na(mig_u))

unions_short <- unions_alt %>% 
  select(idHusband,idWife,mig_u)

unions_alt <- unions_alt %>% 
  dplyr::inner_join(inds_mig %>% filter(Sex == "m") %>% 
               select(idInd,idFather,idMother,idPGF = idFather_f, idPGM = idMother_f, idMGF = idFather_m, idMGM = idMother_m,
                      DOB,brank_em_sex,nsibs_em_sex) %>% 
               rename_with(~ paste0(.x,"_man")),join_by(idHusband == idInd_man)) %>% 
  dplyr::inner_join(inds_mig %>% filter(Sex == "f") %>% 
               select(idInd,idFather,idMother,idPGF = idFather_f, idPGM = idMother_f, idMGF = idFather_m, idMGM = idMother_m,
                      DOB,brank_em_sex,nsibs_em_sex) %>% 
               rename_with(~ paste0(.x,"_woman")),join_by(idWife == idInd_woman)) %>% 
  dplyr::inner_join(select(unions_short,idHusband,idWife,mig_p_man = mig_u), join_by(idFather_man == idHusband, idMother_man == idWife)) %>% 
  dplyr::inner_join(select(unions_short,idHusband,idWife,mig_p_woman = mig_u), join_by(idFather_woman == idHusband, idMother_woman == idWife)) %>% 
  dplyr::inner_join(select(unions_short,idHusband,idWife,mig_pgp_man = mig_u), join_by(idPGF_man == idHusband, idPGM_man == idWife)) %>% 
  dplyr::inner_join(select(unions_short,idHusband,idWife,mig_pgp_woman = mig_u), join_by(idPGF_woman == idHusband, idPGM_woman == idWife)) %>% 
  dplyr::inner_join(select(unions_short,idHusband,idWife,mig_mgp_man = mig_u), join_by(idMGF_man == idHusband, idMGM_man == idWife)) %>% 
  dplyr::inner_join(select(unions_short,idHusband,idWife,mig_mgp_woman = mig_u), join_by(idMGF_woman == idHusband, idMGM_woman == idWife)) %>% 
  dplyr::inner_join(select(parishes,CodeLocation,year_parish=year_par,lng,lat,closest_Mtl), join_by(CodeLocation_fb == CodeLocation)) %>% 
  mutate(year_mar = year(DateU),
         year_mar2 = year_mar**2, 
         year_parish2 = year_parish**2) %>% 
  filter(year_mar <= 1861-survival) %>% 
  mutate(n_migp = mig_p_man + mig_p_woman,
         n_miggp = mig_pgp_man + mig_mgp_man + mig_pgp_woman + mig_mgp_woman) %>% 
  write_csv(paste0("data_modif/unions_alt_",dist_thresh,".csv"))

rm(list = ls())
