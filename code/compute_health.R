# Côté-Gendreau 2026
# This file estimates family-level health by the levels of child and infant mortality

inds <- read_csv("data_modif/inds_DOB_est.csv", guess_max = 5000)

unions <- read_csv("data_modif/unions_bounds_mod.csv") %>% 
  select(idHusband,idWife,DateU,CodeLocation)

real_b <- read_csv("data_modif/real_b.csv")
real_s <- read_csv("data_modif/real_s.csv")
stillborn <- read_csv("data_modif/stillborn.csv")
mentions <- read_csv("data_modif/mentions_mod.csv") 

# Keep deaths under the age of 15 (either verified or reported)
real_s <- real_s %>% 
  left_join(select(inds,idInd,DOB,DOB_min)) %>% 
  mutate(age_true = ymd(Date_mod_precise) - DOB,
         age_max = ymd(Date_mod_precise) - DOB_min,
         Age_tf = ddays(Age_tf),
         under_15 = case_when(!is.na(age_max) & age_max < dyears(15) ~ 1,
                              !is.na(age_true) & age_true < dyears(15) ~ 1, 
                              !is.na(Age_tf) & Age_tf < dyears(15) ~ 1,
                              T ~ 0)) %>% 
  filter(under_15 > 0)

# Double stillborn deaths so they count for a birth and a death
acts <- mentions %>% 
  filter(idAct %in% c(real_s$idAct,real_b$idAct)) %>% 
  bind_rows(mentions %>% filter(idAct %in% stillborn) %>% mutate(Type = "b")) %>% 
  select(-Role) %>% 
  rename(Role = Role_revised) %>% 
  filter(Role %in% c(1,3,4)) %>% 
  group_by(idAct, Role, Type) %>% 
  mutate(n = dplyr::n()) %>% 
  ungroup() %>% 
  filter(n < 2)

filial_links <- acts %>% 
  pivot_wider(id_cols = idAct, names_from = Role, values_from = idInd, names_prefix = "Role") 

# Get the ratio of deaths to births by couple
tallies <- acts %>%
  filter(Role == 1) %>% 
  left_join(rename(filial_links, idInd = Role1)) %>% 
  left_join(select(inds,idInd,idFather,idMother)) %>% 
  mutate(idFather = if_else(is.na(Role3) | Role3 == -1, idFather, Role3),
         idMother = if_else(is.na(Role4) | Role4 == -1, idMother,Role4)) %>% 
  filter(!is.na(idFather) & idFather != -1 & !is.na(idMother) | idMother != -1) %>%
  group_by(idFather,idMother) %>% 
  dplyr::summarize(n_s = sum(Type == "s"), n_b = sum(Type == "b")) %>% 
  inner_join(select(unions, idFather = idHusband, idMother = idWife, DateU, CodeLocation)) %>% 
  mutate(decade_mar = floor(year(DateU)/10)*10,
         ratio = n_s/n_b)

# Get means and standard deviations of the ratio by decade and parish of marriage
decadal_values <- tallies %>% 
  filter(is.finite(ratio),!(CodeLocation %in% c(0,22222))) %>% 
  group_by(decade_mar,CodeLocation) %>% 
  dplyr::summarize(mean = mean(ratio), sd = sd(ratio)) 

# Get the normalized ratios
tallies <- tallies %>% 
  left_join(decadal_values) %>% 
  mutate(norm_ratio = (ratio - mean)/sd) %>% 
  filter(is.finite(norm_ratio)) %>% 
  write_csv("data_modif/surv_ratios.csv")
