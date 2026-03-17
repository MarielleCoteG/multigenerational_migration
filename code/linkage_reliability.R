# This script uses various criteria to grossly estimate the "reliability" of the linkage:
## whether the names in the record and those of the individuals match
## whether the age and timing work (e.g. a couple married in 1750 cannot have a child born in 1850)
## whether there are other individuals mentioned to support the link (e.g., a death record with only a subject is less reliable)

mentions <- read_csv("data_modif/mentions_mod.csv")
inds <- read_csv("data_modif/inds_mod.csv",guess_max=10000)
unions <- read_csv("data_modif/unions_mod.csv")

unions_long <- unions %>% 
  pivot_longer(c(idHusband,idWife),names_to = "Spouse", values_to = "idInd") %>% 
  arrange(DateU) %>% 
  mutate(yearU = year(DateU)) %>% 
  arrange(yearU,desc(QualityDate))

inds <- inds %>% 
  arrange(DOB) %>% 
  left_join(select(unions,QualityDateMP = QualityDate,idFather = idHusband,idMother = idWife,DateMariageP = DateU)) %>% 
  dplyr::left_join(select(unions,QualityDateM1_mother = QualityDate,idMother = idWife,DateMariage1_mother = DateU), by = join_by(idMother), multiple="first") %>% 
  dplyr::left_join(select(unions_long,QualityDateM1 = QualityDate,idInd,DateM1 = DateU), by = join_by(idInd), multiple="first") %>% 
  dplyr::left_join(select(filter(.,QualityDateBirth <= 5,idMother!=-1),QualityDateChild1_mother = QualityDateBirth,idMother,DateChild1_mother = DOB), by = join_by(idMother), multiple="first") %>% 
  dplyr::left_join(select(filter(.,QualityDateBirth <= 5,idMother!=-1),QualityDateChildn = QualityDateBirth,idMother,DateChildn = DOB), by = join_by(idInd == idMother), multiple="last") %>% 
  dplyr::left_join(select(.,DOB,DOD,QualityDateBirth,QualityDateDeath,idInd),join_by(idFather==idInd),suffix=c("","_father")) %>% 
  dplyr::left_join(select(.,DOB,DOD,QualityDateBirth,QualityDateDeath,idInd),join_by(idMother==idInd),suffix=c("","_mother"))


mentions_short <- mentions %>% 
  filter(Type %in% c("b","s")) %>% 
  select(idAct,idMention,idInd,Type,Role,Rank,Age_tf,fname,fname_std,lname,lname_std,DateEvent_precise,Date_mod_precise) %>% 
  dplyr::left_join(select(inds,idInd,fname_std,lname_std,DOB,DateMariageP,QualityDateBirth,QualityDateMP,
                   DOB_mother,DOB_father,QualityDateBirth_mother,QualityDateBirth_father,
                   DateMariage1_mother,DateChild1_mother), join_by(idInd), suffix=c("","_ind")) %>% 
  filter(Role %in% c(1,2,3,4)) %>% 
  arrange(desc(Rank)) %>% 
  group_by(idAct,Role) %>% 
  slice(1) %>% 
  ungroup %>% 
  arrange(Role) %>% 
  pivot_wider(id_cols = idAct, names_from = Role, values_from = c(idMention,idInd,fname,fname_std,lname,lname_std,fname_std_ind,lname_std_ind), unused_fn = first)

write_csv(mentions_short,"mentions_short.csv")

linkage_reliablility <- function(idA) {
  
  d <- mentions_short[mentions_short$idAct == idA,]
  
  has_spouse <- !is.na(d$idMention_2)
  has_father <- !is.na(d$idMention_3)
  has_mother <-!is.na(d$idMention_4)
  reliability <- c(5,if_else(has_father | has_mother, 5,NA))
  
  type <- d$Type
  
  Age_tf <- ddays(round(d$Age_tf))
  
  if (type %in% c("b","s") & has_mother == T) {
    
    if (type == "b") {
      
      DateN <- case_when(d$DateEvent_precise != 99999999 ~ ymd(d$DateEvent_precise),
                         is.na(d$Age_tf) ~ ymd(d$Date_mod_precise),
                         T ~ as.Date(ymd(d$Date_mod_precise) - 1.5*Age_tf))
    } else {
      
      DateD <- if_else(d$DateEvent_precise == 99999999, ymd(d$Date_mod_precise), ymd(d$DateEvent_precise))
      
      DateN <- if_else(is.na(Age_tf),NA,as.Date(DateD - 1.5*Age_tf))
    }
    
    reliability[2] = case_when(d$QualityDateBirth_mother <= 5 & DateN - ymd(d$DOB_mother) > dyears(55) ~ -10,
                               DateN - ymd(d$DateMariage1_mother) > dyears(37) ~ -10,
                               DateN - ymd(d$DateChild1_mother) > dyears(37) ~ -10,
                               T ~ reliability[2])
  }
  
  if (type == "s") {
    
    if (is.na(Age_tf)) {
      reliability[1] =  reliability[1]-1
    } else {
      
      DateD <- if_else(d$DateEvent_precise == 99999999, ymd(d$Date_mod_precise), ymd(d$DateEvent_precise))
      
      Age_true <- DateD - ymd(d$DOB)
      Age_max <- DateD - ymd(d$DateMariageP)
      
      Age_true <- if_else(is.na(Age_true), Age_max, Age_true)
      if (!is.na(Age_true)) {
        if (Age_true > 1.5*Age_tf | Age_true < 0.7*Age_tf) {
          reliability[1] =  reliability[1]-1
        }   
      }
    }
  }
  
  fname_act <- str_split(d$fname_std_1," ")[[1]]
  lname_act <- str_split(d$lname_std_1," ")[[1]]
  fname_ind <- str_split(d$fname_std_ind_1," ")[[1]]
  lname_ind <- str_split(d$lname_std_ind_1," ")[[1]]
  
  if (!is.na(fname_act[1]) & !is.na(fname_ind[1]) & length(intersect(fname_act,fname_ind)) < 1) {reliability[1] = reliability[1]-1}
  if (!is.na(lname_act[1]) & !is.na(lname_ind[1]) & length(intersect(lname_act,lname_ind)) < 1) {reliability[1] = reliability[1]-1}
  
  
  if (has_spouse == 0 & has_father == 0 & has_mother == 0) {
    reliability[1] = reliability[1]-1
  }
  
  if (has_spouse == 1) {
    
    fname_act_sp <- str_split(d$fname_std_2," ")[[1]]
    lname_act_sp <- str_split(d$lname_std_2," ")[[1]]
    fname_ind_sp <- str_split(d$fname_std_ind_2," ")[[1]]
    lname_ind_sp <- str_split(d$lname_std_ind_2," ")[[1]]
    
    if (!is.na(fname_act_sp[1]) & !is.na(fname_ind_sp[1]) & length(intersect(fname_act_sp,fname_ind_sp)) < 1) {reliability[1] = reliability[1]-1}
    if (!is.na(lname_act_sp[1]) & !is.na(lname_ind_sp[1]) & length(intersect(lname_act_sp,lname_ind_sp)) < 1) {reliability[1] = reliability[1]-1}
  }
  
  if (has_mother == 1) {
    
    fname_act_m <- str_split(d$fname_std_4," ")[[1]]
    lname_act_m <- str_split(d$lname_std_4," ")[[1]]
    fname_ind_m <- str_split(d$fname_std_ind_4," ")[[1]]
    lname_ind_m <- str_split(d$lname_std_ind_4," ")[[1]]
    
    if (!is.na(fname_act_m[1]) & !is.na(fname_ind_m[1]) & length(intersect(fname_act_m,fname_ind_m)) < 1) {reliability[2] = reliability[2]-1}
    if (!is.na(lname_act_m[1]) & !is.na(lname_ind_m[1]) & length(intersect(lname_act_m,lname_ind_m)) < 1) {reliability[2] = reliability[2]-1}
  }
  
  if (has_father == 1) {
    
    fname_act_f <- str_split(d$fname_std_3," ")[[1]]
    lname_act_f <- str_split(d$lname_std_3," ")[[1]]
    fname_ind_f <- str_split(d$fname_std_ind_3," ")[[1]]
    lname_ind_f <- str_split(d$lname_std_ind_3," ")[[1]]
    
    if (!is.na(fname_act_f[1]) & !is.na(fname_ind_f[1]) & length(intersect(fname_act_f,fname_ind_f)) < 1) {reliability[2] = reliability[2]-1}
    if (!is.na(lname_act_f[1]) & !is.na(lname_ind_f[1]) & length(intersect(lname_act_f,lname_ind_f)) < 1) {reliability[2] = reliability[2]-1}
  }
  
  return(reliability)
}

ids <- mentions_short %>% 
  filter(idInd_1 != -1) %>% 
  pull(idAct) %>% 
  unique()

assess <- matrix(data=NA,ncol=2,nrow=0)

for (i in 1:length(ids)) {
  assess <- rbind(assess,linkage_reliablility(ids[i]))
  if(i%%10000 == 0) {print(i)}
}

assessments <- cbind(ids,assess) %>% 
  as.data.frame()

write_csv(assessments,"data_modif/linkage_reliability.csv")
