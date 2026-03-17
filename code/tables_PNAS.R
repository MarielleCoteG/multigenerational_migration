# Côté-Gendreau 2026
# This file produces all the tables (both main paper and appendix)

# Packages we need
list_packages <- c("tidyverse","tidytable","kableExtra","stargazer")

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

unions <- read_csv("data_modif/unions_analysis_10km_20_10.csv") %>% 
  mutate(year_mar=year_mar/100,
         year_mar2=year_mar2/10000,
         year_parish=year_parish/100,
         year_parish2=year_parish2/10000)

inds <- read_csv("data_modif/inds_DOB_est.csv", guess_max = 5000) %>% 
  select(idInd,DOB,DOB_min,DOB_max,DOB_mean,flag_DOB_est,flag_DOD_est)

parishes <- read_csv("data_modif/parishes.csv")

dist_matrix <- read_csv("data_modif/dist_matrix.csv") %>% 
  pivot_longer(cols = -Par_A, names_to = "Par_B", values_to = "distance") %>% 
  mutate_all(~as.numeric(.x))

### Compute numbers

# Check how close parishes are to each other typically
parishes_1861 <- parishes %>% 
  filter(year_par <= 1861, province_state == "Québec")

dist_matrix_1861 <- dist_matrix %>% 
  filter(Par_A %in% parishes_1861$CodeLocation, Par_B %in% parishes_1861$CodeLocation) %>% 
  filter(Par_A != Par_B, Par_A != 31291) %>% 
  group_by(Par_A) %>% 
  summarize(min_dist = min(distance))

print("Median distance between a locality and its closest neighbor:")
print(median(dist_matrix_1861$min_dist))

print("Mean distance between a locality and its closest neighbor:")
print(mean(dist_matrix_1861$min_dist))

# Check for missingness of dates

# Check proportion with estimated dates
print("Proportions of husbands with estimated DOBs and DODs:")
print(unions %>% left_join(select(inds,idHusband=idInd,flag_DOB_est,flag_DOD_est)) %>% 
        summarize(DOB_is_estimated = mean(flag_DOB_est),
                  DOD_is_estimated = mean(flag_DOD_est)))

print("Proportions of wives with estimated DOBs and DODs:")
print(unions %>% left_join(select(inds,idWife=idInd,flag_DOB_est,flag_DOD_est)) %>% 
        summarize(DOB_is_estimated = mean(flag_DOB_est),
                  DOD_is_estimated = mean(flag_DOD_est)))

print("Average gap between husbands' min and max DOBs when DOB was interpolated:")
print(unions %>% left_join(select(inds,idHusband=idInd,flag_DOB_est,flag_DOD_est,DOB_mean,DOB_max,DOB_min)) %>% 
        filter(flag_DOB_est == 1 & !is.na(DOB_max) & !is.na(DOB_min)) %>% 
        filter(DOB_man == DOB_mean) %>% 
        mutate(gap_men = DOB_max - DOB_min) %>% 
        summarize(gap_men = mean(gap_men),
                  number = n()))

print("Average gap between wives' min and max DOBs when DOB was interpolated:")
print(unions %>% left_join(select(inds,idWife=idInd,flag_DOB_est,flag_DOD_est,DOB_max,DOB_min,DOB_mean)) %>% 
        filter(flag_DOB_est == 1 & !is.na(DOB_max) & !is.na(DOB_min)) %>% 
        filter(DOB_woman == DOB_mean) %>% 
        mutate(gap_women = DOB_max - DOB_min) %>% 
        summarize(gap_women = mean(gap_women),
                  number = n()))

# Check for how often brothers marry relatives

return_prop <- movers_statuses %>% 
  filter(adulthood_move == 1, Date_move - DateU_first >= ddays(2), Date_move - DateU_first < dyears(window)) %>% 
  filter(idI %in% unions$idHomme | idI %in% unions$idFemme)

pl_brothers <- read_csv("data_modif/pl_brothers.csv")

pl_brothers <- pl_brothers %>% 
  left_join(select(unions, idHusband, idFather_woman_real = idFather_woman, idMother_woman_real = idMother_woman,
                   idPGF_woman_real = idPGF_woman, idPGM_woman_real = idPGM_woman, 
                   idMGF_woman_real = idMGF_woman, idMGM_woman_real = idMGM_woman)) %>% 
  mutate(sisters = case_when(idFather_woman_real == idFather_woman & idMother_woman_real == idMother_woman ~ 2,
                             idFather_woman_real == idFather_woman | idMother_woman_real == idMother_woman ~ 1,
                             T ~ 0),
         cousins = case_when(sisters != 0 ~ 0,
                             (idPGF_woman_real == idPGF_woman | idPGF_woman_real == idMGF_woman) &
                               (idPGM_woman_real == idPGM_woman | idPGM_woman_real == idMGM_woman) ~ 2,
                             (idPGF_woman_real == idPGF_woman | idPGF_woman_real == idMGF_woman) |
                               (idPGM_woman_real == idPGM_woman | idPGM_woman_real == idMGM_woman) ~ 1,
                             T ~ 0))

print("Distribution of couples by whether the placebo wife is a full sister (2), a half-sister (1) or not a sister to the real wife:")
table(pl_brothers$sisters)

print("Distribution of couples by whether the placebo wife shares 2+ grandparents (2), 1 grandparent (1) or none with the real wife:")
table(pl_brothers$cousins)

# Check for return migration
print("Proportion of migratory couples who have migrated at least once within the first twenty years of their marriage once to a place where they, their parents and their grandparents have never resided:")
print(proportions(table(unions$mig_u_rest,unions$mig_u)[,2]))

# Make Table 1

# Urban parishes: only Montreal, Quebec City and TR proper (and protestant, hospitals, ethnic churches)
mtl <- c(3901,3906,3902,31877,31859,3915)
qc <- c(4501,4504,31543,4505,4503,4502,4507,4506)
tr <- c(6001,6002)
sherbrooke <- c(5501)

urban_parishes <- c(mtl,qc,tr,sherbrooke)

types_moves <- unions %>% 
  filter(mig_u == 1) %>% 
  dplyr::left_join(select(parishes,codeP,year_parish_A=annee), join_by(Par_A == codeP)) %>% 
  dplyr::left_join(select(parishes,codeP,year_parish_B=annee), join_by(Par_B == codeP)) %>% 
  mutate(type_A = case_when(Par_A %in% urban_parishes ~ "Urban",
                            year(Date_move_u) - year_parish_A >= 15 ~ "Rural (established)",
                            T ~ "Rural (frontier)"),
         type_B = case_when(Par_B %in% urban_parishes ~ "Urban",
                            year(Date_move_u) - year_parish_B >= 15 ~ "Rural (established)",
                            T ~ "Rural (frontier)"),
         across(type_A:type_B, ~factor(.x,levels = c("Urban","Rural (established)","Rural (frontier)"), ordered = TRUE)))

perc <- round(table(types_moves$type_A,types_moves$type_B)/nrow(types_moves)*100,digits=1)
perc <- cbind(perc,rowSums(perc))
perc <- rbind(perc,colSums(perc))
rownames(perc) <- c("Urban","Rural (established)","Rural (frontier)","Total")
colnames(perc) <- c("Urban","Rural (established)","Rural (frontier)","Total")

perc_df <- as.data.frame(perc)
perc_df <- tibble::rownames_to_column(perc_df, var = "Origin")

# TABLE 1
sink(file = "tables/matrix_DO.tex")
kable(perc_df, format = "latex", booktabs = TRUE, caption = "Origins and destinations of first postmarital moves", align = "lcccr",
      label = "OD") %>%
  kable_styling(latex_options = c("hold_position","scale_down")) %>%
  row_spec(nrow(perc_df), bold = TRUE) %>% 
  column_spec(ncol(perc_df), bold = TRUE) %>% 
  add_header_above(c(" ", "Destination" = 3))
sink(file = NULL)

# TABLE S1
unions_summ <- unions %>%
  mutate(mig_cat2_no = if_else(mig_cat2_pb_max == "none",1,0),
         mig_cat2_pb = if_else(mig_cat2_pb_max == "prebirth",1,0),
         mig_cat2_0 = if_else(mig_cat2_pb_max == "0-9",1,0),
         mig_cat2_10 = if_else(mig_cat2_pb_max == "10-19",1,0)) %>% 
  mutate(dummy_value = 1) %>% 
  pivot_wider(names_from = n_migp, 
    values_from = dummy_value, 
    names_prefix = "n_migp_", 
    values_fill = 0) %>% 
  mutate(dummy_value = 1) %>%
  pivot_wider(names_from = n_miggp, 
    values_from = dummy_value, 
    names_prefix = "n_miggp_",
    values_fill = 0) %>% 
  select(mig_u,
         n_migp_0,n_migp_1,n_migp_2,
         n_miggp_0,n_miggp_1,n_miggp_2,n_miggp_3,n_miggp_4,
         mig_p_man,mig_p_woman,
         mig_pgp_man,mig_pgp_woman,mig_mgp_man,mig_mgp_woman,
         mig_cat2_no,mig_cat2_pb,mig_cat2_0,mig_cat2_10,
         nsibs_em_sex_man,nsibs_em_sex_woman,
         brank_em_sex_man,brank_em_sex_woman,
         year_mar,year_parish,lat,lng) %>% 
  drop_na() %>% 
  mutate_all(as.double) %>% 
  summarise(across(everything(), list(
    mean = ~mean(.),
    median = ~median(.),
    sd = ~sd(.)
  ), .names = "{.col}__{.fn}$")) %>%
  pivot_longer(cols = everything(), 
               names_to = c("variable", "statistic"), 
               names_pattern = "(.*)__(.*)", 
               values_to = "value") %>%
  pivot_wider(names_from = "statistic", values_from = "value") %>% 
  mutate_if(is.numeric,~round(.x,digits=2)) %>% 
  select(-variable)

titles <- c("Variable","Mean","Median","Standard deviation")

Variable <- c("Migration, focal couple",
              "0 migratory parental couples","1 migratory parental couple","2 migratory parental couples",
              "0 migratory grandparental couples","1 migratory grandparental couple","2 migratory grandparental couples","3 migratory grandparental couples","4 migratory grandparental couples",
              "Migration, husband's parents",
              "Migration, wife's parents",
              "Migration, husband's paternal grandparents","Migration, wife's paternal grandparents",
              "Migration, husband's maternal grandparents","Migration, wife's maternal grandparents",
              "Spouses' parents have not migrated either before or during spouses' childhood",
              "Spouse(s)' parents migrated before spouse(s)' own birth","... between the ages of 0 and 9",
              "... between the ages of 10 and 19",
              "Number of boys in husband's family","Number of girls in wife's family",
              "Husband's birth rank among boys","Wife's birth rank among girls",
              "Year of marriage",
              "Year of parish foundation",
              "Latitude","Longitude")

summ <- bind_cols(Variable,unions_summ)

summ <- setNames(summ, titles)

linesep<-function(x,y=character()){
  if(!length(x))
    return(y)
  linesep(x[-length(x)], c(rep('',x[length(x)]-1),'\\addlinespace',y))  
}

sink(file = "tables/summ_Aug2025.tex")
kbl(summ, align=c('l', rep('c', 2)), caption = "Descriptive statistics",
    label = "summ",
    format = "latex",booktabs=T,
    linesep = linesep(c(1,3,5,6,4,4,4)),
    escape = FALSE)
sink(file = NULL)


# Make regression tables

reg_num <- glm(mig_u ~ n_migp*n_miggp +
                 brank_em_sex_man*nsibs_em_sex_man +
                 brank_em_sex_woman*nsibs_em_sex_woman +
                 year_parish + year_parish2 + year_mar+ year_mar2 +
                 lat*lng,
               data = unions, family = "binomial")

reg_gen <- glm(mig_u ~ mig_p_man*mig_p_woman +
                 mig_p_man*mig_pgp_man + mig_p_man*mig_mgp_man + 
                 mig_p_man*mig_pgp_woman + mig_p_man*mig_mgp_woman + 
                 mig_p_woman*mig_pgp_man + mig_p_woman*mig_mgp_man +
                 mig_p_woman*mig_pgp_woman + mig_p_woman*mig_mgp_woman +
                 mig_pgp_man*mig_mgp_man + 
                 mig_pgp_man*mig_pgp_woman + mig_pgp_man*mig_mgp_woman +
                 mig_mgp_man*mig_pgp_woman + mig_mgp_man*mig_mgp_woman +
                 mig_pgp_woman*mig_mgp_woman +
                 brank_em_sex_man*nsibs_em_sex_man +
                 brank_em_sex_woman*nsibs_em_sex_woman +
                 year_parish + year_parish2 + year_mar+ year_mar2 +
                 lat*lng,
               data = unions, family = "binomial")

reg_child <- glm(mig_u ~ mig_cat2_pb_max +
                   brank_em_sex_man*nsibs_em_sex_man +
                   brank_em_sex_woman*nsibs_em_sex_woman +
                   year_parish + year_parish2 + year_mar + year_mar2 +
                   lat*lng,
                 data = unions, family = "binomial")

all <- intersect(names(reg_num$coefficients),intersect(names(reg_gen$coefficients),names(reg_child$coefficients)))

first <- setdiff(names(reg_num$coefficients),all)
second <- setdiff(names(reg_gen$coefficients),all)
third <- setdiff(names(reg_child$coefficients),all)

all <- setdiff(all,"(Intercept)")

vars.order <- c(first,second,third,all)

# TABLE S2
sink(file = "tables/regression_results.tex")
reg_table <- stargazer(reg_num,reg_gen,reg_child, title="Regression results", align=F,
                       dep.var.labels=c("Couple migrated"),
                       covariate.labels=c("$I(n_p=1)$","$I(n_p=2)$",
                                          "$I(n_{gp}=1)$","$I(n_{gp}=2)$","$I(n_{gp}=3)$","$I(n_{gp}=4)$",
                                          "$I(n_p=1)*I(n_{gp=1})$","$I(n_p=1)*I(n_{gp=2})$","$I(n_p=1)*I(n_{gp=3})$","$I(n_p=1)*I(n_{gp=4})$",
                                          "$I(n_p=2)*I(n_{gp=1})$","$I(n_p=2)*I(n_{gp=2})$","$I(n_p=2)*I(n_{gp=3})$","$I(n_p=2)*I(n_{gp=4})$",
                                          "$M_{p,H}$","$M_{p,W}$","$M_{pgp,H}$","$M_{mgp,H}$",
                                          "$M_{pgp,W}$","$M_{mgp,W}$",
                                          "$M_{p,H}*M_{p,W}$",
                                          "$M_{p,H}*M_{pgp,H}$",
                                          "$M_{p,H}*M_{mgp,H}$",
                                          "$M_{p,H}*M_{pgp,W}$",
                                          "$M_{p,H}*M_{mgp,W}$",
                                          "$M_{p,W}*M_{pgp,H}$",
                                          "$M_{p,W}*M_{mgp,H}$",
                                          "$M_{p,W}*M_{pgp,W}$",
                                          "$M_{p,W}*M_{mgp,W}$",
                                          "$M_{pgp,H}*M_{mgp,H}$",
                                          "$M_{pgp,H}*M_{pgp,W}$",
                                          "$M_{pgp,W}*M_{mgp,W}$",
                                          "$M_{mgp,H}*M_{pgp,W}$",
                                          "$M_{mgp,H}*M_{mgp,W}$",
                                          "$M_{pgp,W}*M_{mgp,W}$",
                                          "$CM_{before~ birth}$",
                                          "$CM_{0-9}$","$CM_{10-19}$",
                                          "Birth rank among boys, H","Number of boys, H",
                                          "Birth rank among girls, W","Number of girls, W",
                                          "Year of parish foundation$/100$","Year of parish foundation${}^2/10,000$",
                                          "Year of marriage$/100$","Year of marriage${}^2/10,000$",
                                          "Latitude","Longitude",
                                          "Birth rank among boys*N boys",
                                          "Birth rank among girls*N girls",
                                          "Latitude*Longitude"
                       ),
                       column.sep.width = "1pt",
                       keep.stat=c("n","LL"), single.row=T,
                       initial.zero=F, no.space=T,
                       order=paste0("^", vars.order , "$"),
                       header=FALSE,
                       font.size="small",
                       label = "tab:regression",
                       digits = 3,
                       digits.extra = 5,
                       star.cutoffs = c(0.05,0.001,0.0001))
sink(file = NULL)

rm(list = ls())
