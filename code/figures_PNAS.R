# Côté-Gendreau 2026
# This file makes all the figures (both main paper and appendix)

# Packages we need
list_packages <- c("dplyr","readr","stringr","tibble","tidytable","lubridate","sjPlot","ggeffects","RColorBrewer","ggpubr","scales","sf","ggplot2","ggrepel","ggspatial","extrafont","survival","ggsurvfit","survminer")

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

# Formatting
# I use the font LM Roman 10 - if you don't have that font installed, you can remove references to LM Roman 10 throughout
options(scipen = 999)
theme_set(theme_linedraw(base_family = 'LM Roman 10', base_size = 14) + 
            theme(text = element_text(color = "black"),
                  axis.text.x = element_text(size = 10),
                  axis.ticks.x=element_blank(),
                  plot.margin = unit(c(0,0,0,0), "lines"),
                  panel.grid = element_blank(),
                  axis.title.y = element_text(margin = margin(t = 0, r = 3.5, b = 0, l = 1))))

# This function plots predicted probabilities by number of migratory parents and grandparents
main_plot <- function(data = unions, outcome = "mig_u", version = 1, save = F, limits_y = NULL, title = NULL,
                      small = F, model = "logit") {
  
  outcome_sym <- sym(outcome)
  
  # To make sure that interactions are not causing results to be unstable
  if (model == "no_inter") {
    formula <- expr(!!outcome_sym ~ 
                      n_migp + n_miggp +
                      brank_em_sex_man + nsibs_em_sex_man +
                      brank_em_sex_woman + nsibs_em_sex_woman +
                      year_parish + year_parish2 + year_mar + year_mar2 +
                      lat + lng)
  } else {
    formula <- expr(!!outcome_sym ~ 
                      n_migp * n_miggp +
                      brank_em_sex_man * nsibs_em_sex_man +
                      brank_em_sex_woman * nsibs_em_sex_woman +
                      year_parish + year_parish2 + year_mar + year_mar2 +
                      lat * lng)
  }
  
  # Linear probability model instead of logistic regression model
  if (model == "lpm") {
    reg_num <- lm(formula, data = data)
  } else {
    reg_num <- glm(formula, data = data, family = "binomial")
  }
  
  n_mig <- ggemmeans(reg_num,terms=c("n_migp","n_miggp")) %>% 
    as.data.frame() %>% 
    mutate(n_migp = as.factor(x),
           n_miggp = as.factor(group)) 
  
  text_labels <- expand.grid(
    n_migp = as.factor(0:2),
    n_miggp = as.factor(0:4)
  )
  
  text_labels <- text_labels %>%
    arrange(n_migp, n_miggp) %>%
    mutate(
      x_numeric = as.numeric(n_migp),
      dodge_index = as.numeric(n_miggp) - 1,
      x_pos = x_numeric + (dodge_index - (5 - 1)/2) * (0.75 / (5)),
      label = as.character(n_miggp)
    )
  
  if (!is.null(limits_y) & length(limits_y) == 2) {
    n_mig <- n_mig %>% 
      mutate(predicted = if_else(!between(predicted,limits_y[1],limits_y[2]),NA,predicted))
  }
  
  preds_plot <- ggplot(n_mig, aes(n_migp,predicted, group = n_miggp)) +
    geom_point(aes(color = n_miggp, shape = n_migp),
               position = position_dodge(width = 0.75),
               alpha = 1,
               size = if_else(small == T, 2/5, 2),
               stroke = 1) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high, color = n_miggp),
                   alpha = 1, position = position_dodge(width = 0.75)) +
    labs(title = title, x = "", y = "") +
    theme(plot.margin = unit(c(2,0,0,0), "lines"),
          axis.text.x = element_text(size = 12),
          plot.title = element_text(face = "bold",
                                    size = 12,
                                    hjust = 0.5,
                                    margin = unit(c(-2,0,2,0), "lines"))) +
    scale_color_manual(values = brewer.pal(9, "Greys")[c(9,7,6,5,4)]) +
    scale_x_discrete(expand = c(0, 0.5)) +
    geom_vline(xintercept = c(1.5,2.5), linetype = "dotted") +
    geom_text(data = text_labels,
              aes(x = x_pos, y = I(1) + 0.02, vjust=0, label = label),
              inherit.aes = FALSE,
              size = 3.5,
              family = "LM Roman 10") +
    coord_cartesian(clip = 'off') +
    guides(color = "none", shape = "none") +
    scale_size_continuous(range = c(0,1), breaks = c(1, 5, 10, 15)) +
    scale_shape_manual(values = c(19,17,4))
  
  if (NA %in% n_mig$predicted) {
    
    preds_plot <- preds_plot +
      scale_y_continuous(limits = limits_y, oob = scales::oob_squish,
                         expand = c(0, 0))
  } else {
    preds_plot <- preds_plot +
      scale_y_continuous(limits = limits_y)
  }
  
  if (small == F ) {
    
    preds_plot <- preds_plot +
      annotate(
        "text",
        x = 2,
        y = I(1) + 0.08,
        label = "Number of migratory grandparental couples",
        size = 5,
        vjust = 0,
        family = "LM Roman 10"
      ) +
      labs(y = "Predicted migration probability",
           x = "Number of migratory parental couples") +
      theme(plot.margin = unit(c(2.5,0,0,0), "lines"))
  }
  
  if (save == T) {
    
    name_data <- deparse(substitute(data))
    suffix <- sub(".*_", "", name_data)
    
    if (outcome != "mig_u") {
      suffix <- paste(suffix,sub(".*_(.*)", "\\1", outcome),sep="_")
    } 
    
    if (model != "logit") {
      suffix <- paste(suffix,model,sep="_")
    }
    
    ggsave(paste0("figures/preds_gen_",suffix,".pdf"),
           plot=preds_plot,width=7,height=5,
           device = cairo_pdf)
  }
  
  print(preds_plot)
  return(preds_plot)
}

# FIGURE 2
unions_main <- read_csv("data_modif/unions_analysis_10km_20_10.csv") %>% 
  mutate(mig_cat2_pb_max = factor(mig_cat2_pb_max,levels=c("none","prebirth","0-9","10-19"), ordered=F),
         across(c(n_migp,n_miggp), ~factor(.x)))
main <- main_plot(data = unions_main, save = T)

# FIGURE S9: With a 5km threshold
unions_5km <- read_csv("data_modif/unions_analysis_5km_20_10.csv") %>% 
  mutate(across(c(n_migp,n_miggp), ~factor(.x)))
main_5 <- main_plot(data = unions_5km, save = T)

# FIGURE S10: With a 25km threshold
unions_25km <- read_csv("data_modif/unions_analysis_25km_20_10.csv") %>% 
  mutate(across(c(n_migp,n_miggp), ~factor(.x)))
main_25 <- main_plot(data = unions_25km, save = T, limits_y = c(0.09,0.32))

# FIGURE S11: Restricting the observation period to 10 years
unions_10y <- read_csv("data_modif/unions_analysis_10km_10_10.csv") %>% 
  mutate(across(c(n_migp,n_miggp), ~factor(.x)))
main_10y <- main_plot(data = unions_10y, save = T)

# FIGURE S12: Requiring observation over 20 years
unions_20y <- read_csv("data_modif/unions_analysis_10km_20_20.csv") %>% 
  mutate(across(c(n_migp,n_miggp), ~factor(.x)))
main_20y <- main_plot(data = unions_20y, save = T)

# FIGURE S13: Measure migration by comparing first and last births
unions_alt <- read_csv("data_modif/unions_alt_10.csv") %>% 
  mutate(across(c(n_migp,n_miggp), ~factor(.x)))
alt <- main_plot(data = unions_alt, save = T)

# FIGURE S14: Restrict to onwards migration
restr <- main_plot(data = unions_main, outcome = "mig_u_restr", save = T)

# FIGURE S15: Restrict to fully native-born trees
unions_no_imm <- filter(unions_main,imm_gen_man == 4, imm_gen_woman == 4)
main_no_imm <- main_plot(data = unions_no_imm, save = T)

# FIGURE S16: Try with linear probability model instead
lpm <- main_plot(data = unions_main, model = "lpm", save = T)

# Results by geographic/temporal subcontext
east <- main_plot(data = unions_main %>% filter(closest_Mtl == F),limits_y = c(0.22,0.53), title = "East",
                  small = T, save = F)
west <- main_plot(data = unions_main %>% filter(closest_Mtl == T),limits_y = c(0.22,0.53), title = "West",
                  small = T, save = F)

old <- main_plot(data = unions_main %>% filter(year_parish < 1777),limits_y = c(0.22,0.53), title = "Parishes founded before 1777", small = T, save = F)
new <- main_plot(data = unions_main %>% filter(year_parish >= 1777),limits_y = c(0.22,0.53), title = "Parishes founded in or after 1777", small = T, save = F)

per1 <- main_plot(data = unions_main %>% filter(year_mar <= 1825),limits_y = c(0.22,0.53), title = "Couples married before 1826", small = T, save = F)
per2 <- main_plot(data = unions_main %>% filter(year_mar > 1825),limits_y = c(0.22,0.53), title = "Couples married in or after 1826", small = T, save = F)

# FIGURE S6
subcontext <- ggarrange(east,west,old,new,per1,per2,
                        ncol=2, nrow=3, common.legend = TRUE, legend="bottom") %>%  
  annotate_figure(., left = text_grob("Predicted migration probability",
                                      rot = 90, vjust = 1, size = 15, family = "LM Roman 10"),
                  bottom = text_grob("Number of migratory parental couples",
                                   rot = 0, vjust = 0, size = 15, family = "LM Roman 10"),
                  top = text_grob("Number of migratory grandparental couples",
                                  rot = 0, vjust = 1, size = 15, family = "LM Roman 10")) %>%
  ggsave("figures/PNAS_subcontext.pdf",plot=.,width=7.5,height=9,device=cairo_pdf)


# FIGURE 3
gender_plot <- function(data = unions, quantity = "OR", save = F, title = NULL, limits_y = c(1,1.55), small = F, model = "all_inter") {
  
  # To see how ORs look without interactions
  if (model == "no_inter") {
    reg_gen <- glm(mig_u ~ mig_p_man + mig_p_woman +
                     mig_pgp_man + mig_mgp_man + 
                     mig_pgp_woman + mig_mgp_woman + 
                     brank_em_sex_man + nsibs_em_sex_man +
                     brank_em_sex_woman + nsibs_em_sex_woman +
                     year_parish + year_parish2 + year_mar+ year_mar2 +
                     lat + lng,
                   data = data, family = "binomial")
    
  # To see how ORs look if we only keep a reduced set of interactions
  } else if (model == "part_inter") {
    
    reg_gen <- glm(mig_u ~ mig_p_man*mig_p_woman +
                     mig_p_man*mig_pgp_man + mig_p_man*mig_mgp_man + 
                     mig_p_woman*mig_pgp_woman + mig_p_woman*mig_mgp_woman +
                     mig_pgp_man*mig_mgp_man + 
                     mig_pgp_woman*mig_mgp_woman +
                     brank_em_sex_man*nsibs_em_sex_man +
                     brank_em_sex_woman*nsibs_em_sex_woman +
                     year_parish + year_parish2 + year_mar+ year_mar2 +
                     lat*lng,
                   data = data, family = "binomial")
  } else {
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
                   data = data, family = "binomial")
  }
  
  odds_ratios <- plot_model(reg_gen,terms = c("mig_p_woman","mig_p_man","mig_pgp_woman","mig_pgp_man","mig_mgp_woman","mig_mgp_man"))$data
  
  odds_ratios <- odds_ratios %>% 
    mutate(cat = if_else(term %in% c("mig_p_man","mig_p_woman"), "p","gp")) %>% 
    mutate(term = factor(term, levels=c("mig_pgp_man","mig_mgp_man","mig_pgp_woman","mig_mgp_woman","mig_p_man","mig_p_woman")),
           gender = if_else(str_detect(term,"woman"),"wife","husband"))
  
  plot <- ggplot(odds_ratios,aes(x=term,y=estimate)) +
    geom_linerange(aes(ymin = conf.low, ymax=conf.high)) +
    geom_point(aes(y = estimate, fill = cat, shape = gender), size = 4, stroke = 1) +
    labs(y = "Odds ratios of migrating", x = "Ascendant couples", title = title) +
    scale_y_continuous(limits = limits_y) +
    scale_x_discrete(labels=c("Husband's\npaternal\ngrandparents","Husband's\nmaternal\ngrandparents",
                              "Wife's\npaternal\ngrandparents","Wife's\nmaternal\ngrandparents",
                              "Husband's\nparents","Wife's\nparents")) +
    scale_fill_manual(values = c("white","grey")) +
    scale_shape_manual(values = c(24,21)) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x = element_text(margin = margin(b = 2.8, t = 2.8)),
          plot.margin = unit(c(0,0,0,0), "lines"),
          plot.title = element_text(hjust=0.5, face="bold", size = 12),
          panel.grid = element_blank()) +
    geom_vline(xintercept = c(4.5), linetype = "dotted") +
    geom_hline(yintercept = 1) +
    guides(fill = "none", shape = "none")
  
  if (small == T) {
    
    plot <- plot +
      labs(x = "") +
      scale_x_discrete(labels=c("H's\npaternal\ngps","H's\nmaternal\ngps",
                                "W's\npaternal\ngps","W's\nmaternal\ngps",
                                "H's\nparents","W's\nparents")) +
      labs(y = "")
    
  }
  
  if (save == T) {
    
    suffix <- if_else(model == "all_inter", "", paste0("_",model))
    
    if (quantity == "OR") {
      ggsave(paste0("figures/OR",suffix,".pdf"),
             plot=plot,width=6,height=4,
             device = cairo_pdf)
    } else {
      ggsave(paste0("figures/preds_p_gp",suffix,".pdf"),
             plot=plot,width=8,height=6,
             device = cairo_pdf)
    }
  }
  
  print(plot)
  return(plot)
}

# FIGURE 3
gender_plot(data = unions_main, quantity = "OR", save = T)

# Odds ratios by geographic/temporal subcontext
east_OR <- gender_plot(data = unions_main %>% filter(closest_Mtl == F),limits_y = c(0.98,1.68), title = "East", small = T)
west_OR <- gender_plot(data = unions_main %>% filter(closest_Mtl == T),limits_y = c(0.98,1.68), title = "West", small = T)

old_OR <- gender_plot(data = unions_main %>% filter(year_parish < 1777),limits_y = c(0.98,1.68), title = "Parishes founded before 1777",small = T)
new_OR <- gender_plot(data = unions_main %>% filter(year_parish >= 1777),limits_y = c(0.98,1.68), title = "Parishes founded in or after 1777",small = T)

per1_OR <- gender_plot(data = unions_main %>% filter(year_mar < 1826),limits_y = c(0.98,1.68), title = "Couples married before 1826", small = T)
per2_OR <- gender_plot(data = unions_main %>% filter(year_mar >= 1826),limits_y = c(0.98,1.68), title = "Couples married in or after 1826", small = T)

# FIGURE S7
subcontext_OR <- ggarrange(east_OR,west_OR,old_OR,new_OR,per1_OR,per2_OR,
                        ncol=2, nrow=3, common.legend = TRUE, legend="bottom") %>%  
  annotate_figure(., left = text_grob("Odds ratios of migrating",
                                      rot = 90, vjust = 1, hjust = 0.35, size = 15, family = "LM Roman 10"),
                  bottom = text_grob("Ascendant couples",
                                     just = "bottom", hjust = 0.5, y = 0.5, size = 15, family = "LM Roman 10")) %>%
  ggsave("figures/PNAS_subcontext_OR.pdf",plot=.,width=8.5,height=9,device=cairo_pdf)

# To replicate Figure 3 but using a linear probability model instead
gender_plot_LPM <- function(data = unions, save = F, title = NULL, limits_y = c(0,0.5), small = F, model = "all_inter") {
  
  reg_gen <- lm(mig_u ~ mig_p_man*mig_p_woman +
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
                data = data)
  
  coefficients <- summary(reg_gen)$coefficients[c("mig_p_woman","mig_p_man","mig_pgp_woman","mig_pgp_man","mig_mgp_woman","mig_mgp_man"),c("Estimate","Std. Error")]
  
  colnames(coefficients) <- c("estimate","se")
  
  coefficients <- coefficients %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "term") %>% 
    mutate(cat = if_else(term %in% c("mig_p_man","mig_p_woman"), "p","gp")) %>% 
    mutate(term = factor(term, levels=c("mig_pgp_man","mig_mgp_man","mig_pgp_woman","mig_mgp_woman","mig_p_man","mig_p_woman")),
           gender = if_else(str_detect(term,"woman"),"wife","husband")) %>% 
    mutate(conf.low = estimate - 1.96*se, conf.high = estimate + 1.96*se)
  
  plot <- ggplot(coefficients,aes(x=term,y=estimate)) +
    geom_linerange(aes(ymin = conf.low, ymax=conf.high)) +
    geom_point(aes(y = estimate, fill = cat, shape = gender), size = 4, stroke = 1) +
    labs(y = "Coefficients", x = "Ascendant couples", title = title) +
    scale_y_continuous(limits = limits_y) +
    scale_x_discrete(labels=c("Husband's\npaternal\ngrandparents","Husband's\nmaternal\ngrandparents",
                              "Wife's\npaternal\ngrandparents","Wife's\nmaternal\ngrandparents",
                              "Husband's\nparents","Wife's\nparents")) +
    scale_fill_manual(values = c("white","grey")) +
    scale_shape_manual(values = c(24,21)) +
    theme(axis.ticks.x=element_blank(),
          plot.margin = unit(c(0,0,0,0), "lines"),
          axis.text.x = element_text(margin = margin(b = 2.8, t = 2.8)),
          plot.title = element_text(hjust=0.5, face="bold", size = 12),
          panel.grid = element_blank()) +
    geom_vline(xintercept = c(4.5), linetype = "dotted") +
    geom_hline(yintercept = 0) +
    guides(fill = "none", shape = "none")
  
  if (small == T) {
    plot <- plot +
      scale_x_discrete(labels=c("H's\npaternal\ngps","H's\nmaternal\ngps",
                                "W's\npaternal\ngps","W's\nmaternal\ngps",
                                "H's\nparents","W's\nparents")) +
      labs(y = "")
  }
  
  if (save == T) {
    
    suffix <- if_else(model == "all_inter", "", paste0("_",model))
    
    ggsave(paste0("figures/coef_lpm",suffix,".pdf"),
           plot=plot,width=6,height=4,
           device = cairo_pdf)
  }
  print(plot)
  return(plot)
}

# FIGURE S17
gender_plot_LPM(data = unions_main, save = T, limits_y = NULL)

# To construct Figure 4 (Figure 3 but with placebo spouses)
plot_placebo <- function(data = NULL, quantity = "odds", limits_y = NULL, small = F, title = NULL, y_lab = TRUE, placebo = "f") {
  
  reg <- glm(mig_u ~ mig_p_man*mig_p_woman +
               mig_p_man*mig_pgp_man + mig_p_man*mig_mgp_man + 
               mig_p_woman*mig_pgp_woman + mig_p_woman*mig_mgp_woman +
               mig_pgp_man*mig_mgp_man + mig_pgp_woman*mig_mgp_woman +
               mig_pgp_man*mig_pgp_woman + mig_pgp_man*mig_mgp_woman +
               mig_mgp_man*mig_pgp_woman + mig_mgp_man*mig_mgp_woman +
               brank_em_sex_man*nsibs_em_sex_man +
               brank_em_sex_woman*nsibs_em_sex_woman +
               year_parish + year_parish2 + year_mar+ year_mar2 +
               lat*lng,
             data = data, family = "binomial")
  
  odds_ratios <- plot_model(reg,terms = c("mig_p_woman","mig_p_man","mig_pgp_woman","mig_pgp_man","mig_mgp_woman","mig_mgp_man"))$data
  
  odds_ratios <- odds_ratios %>% 
    mutate(cat = if_else(term %in% c("mig_p_man","mig_p_woman"), "p","gp")) %>% 
    mutate(term = factor(term, levels=c("mig_pgp_man","mig_mgp_man","mig_pgp_woman","mig_mgp_woman","mig_p_man","mig_p_woman")),
           gender = if_else(str_detect(term,"woman"),"wife","husband"))
  
  if (placebo == "m") {
    color_values <- c("red", "black")
    linetype_values <- c("21", "solid")
  } else {
    color_values <- c("black", "red")
    linetype_values <- c("solid", "21")
  }
  
  plot <- ggplot(odds_ratios,aes(x=term,y=estimate)) +
    geom_linerange(aes(ymin = conf.low, ymax=conf.high, color = gender, linetype = gender), linewidth = 0.8) +
    geom_point(aes(y = estimate, fill = cat, shape = gender, color = gender), size = 4, stroke = 1.5) +
    labs(y = "Odds ratios", x = "Ascendant couples", title = title) +
    scale_y_continuous(limits = c(floor(min(odds_ratios$conf.low)*10)/10,max(ceiling(max(odds_ratios$conf.high)*10)/10))) +
    scale_x_discrete(labels=c("Husband's\npaternal\ngrandparents","Husband's\nmaternal\ngrandparents",
                              "Wife's\npaternal\ngrandparents","Wife's\nmaternal\ngrandparents",
                              "Husband's\nparents","Wife's\nparents")) +
    scale_fill_manual(values = c("white","grey")) +
    scale_shape_manual(values = c(24,21)) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = linetype_values) +
    theme(axis.ticks.x=element_blank(),
          plot.margin = unit(c(4,2,4,2), "lines"),
          axis.text.x = element_text(margin = margin(b = 2.8, t = 2.8)),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5,size=12)) +
    geom_vline(xintercept = c(4.5), linetype = "dotted") +
    geom_hline(yintercept = 1) +
    guides(fill = "none", shape = "none", color = "none", linetype = "none")
  
  if (small == T) {
    plot <- plot +
      labs(x = "") +
      scale_x_discrete(labels=c("H's\npaternal\ngps","H's\nmaternal\ngps",
                                "W's\npaternal\ngps","W's\nmaternal\ngps",
                                "H's\nparents","W's\nparents"),
                       limits = c("mig_pgp_man","mig_mgp_man","mig_pgp_woman","mig_mgp_woman","mig_p_man","mig_p_woman")) +
      theme(plot.margin = unit(c(10,1,10,1),"pt"),
            plot.title = element_text(size = 15),
            axis.text.x = element_text(size = 10))
  }
  
  if (!is.null(limits_y)) {
    plot <- plot +
      scale_y_continuous(limits = limits_y)
  }
  
  if (y_lab == F) {
    plot <- plot +
      rremove("ylab")
  }
  return(plot)
}

# Panels of Figure 4
pl_brothers <- read_csv("data_modif/pl_brothers.csv")
brothers <- plot_placebo(data = pl_brothers,title="Replacing wife with\nwife of brother closest in age",small=T,y_lab=F,limits_y=c(0.97,1.6))

pl_local <- read_csv("data_modif/pl_local.csv")
local <- plot_placebo(data = pl_local,title="Replacing wife with random woman\nwho married in same place and year",small=T,y_lab=F,limits_y=c(0.97,1.6))

# FIGURE 4
placebo <- ggarrange(brothers,local,
                     ncol=2, nrow=1) %>%  
  annotate_figure(., left = text_grob("Odds ratios of migrating",
                                      rot = 90, vjust = 0.2, hjust = 0.415, size = 17, family = "LM Roman 10"),
                  bottom = text_grob("Ascendant couples",
                                     just = "bottom", hjust = 0.5, y = 1.5, size = 17, family = "LM Roman 10"))

placebo <- placebo + 
  theme(plot.margin = unit(c(-5, 0, -25, 1), "pt"))

ggsave("figures/OR_placebo.pdf",plot=placebo,width=8,height=4,device=cairo_pdf)

# Subplots of Figure S8
pl_sisters <- read_csv("data_modif/pl_sisters.csv")
sisters <- plot_placebo(data = pl_sisters,title="Replacing husband with\nhusband of sister closest in age",small=T,placebo="m",y_lab=F,limits_y=c(0.97,1.6))

pl_local_men <- read_csv("data_modif/pl_local_men.csv")
local_men <- plot_placebo(data = pl_local_men,title="Replacing husband with random man\nwho married in same place and year",placebo="m",small=T,y_lab=F,limits_y=c(0.97,1.6))

# FIGURE S8
placebo_men <- ggarrange(sisters,local_men,
                     ncol=2, nrow=1) %>%  
  annotate_figure(., left = text_grob("Odds ratios of migrating",
                                      rot = 90, vjust = 0.2, hjust = 0.415, size = 17, family = "LM Roman 10"),
                  bottom = text_grob("Ascendant couples",
                                     just = "bottom", hjust = 0.5, y = 1.5, size = 17, family = "LM Roman 10"))

placebo_men <- placebo_men + 
  theme(plot.margin = unit(c(-5, 0, -25, 1), "pt"))

ggsave("figures/OR_placebo_men.pdf",plot=placebo_men,width=8,height=4,device=cairo_pdf)

# FIGURE 5
reg_child <- glm(mig_u ~ mig_cat2_pb_max +
                   brank_em_sex_man*nsibs_em_sex_man +
                   brank_em_sex_woman*nsibs_em_sex_woman +
                   year_parish + year_parish2 + year_mar+ year_mar2 +
                   lat*lng,
                 data = unions_main, family = "binomial")

pred_child <- ggpredict(reg_child,terms=c("mig_cat2_pb_max"),
                        typical = c(numeric = "median", factor = "mode")) %>% 
  as.data.frame() %>% 
  mutate(cats = as.factor(case_when(x == "none" ~ 1,
                                    x == "prebirth" ~ 2,
                                    T ~ 3)))


preds_child_plot <- ggplot(pred_child, aes(x, predicted)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high,
                      shape = cats),
                  size = 1) +
  labs(y = "Predicted migration probability",
       x = "Timing of parental migration") +
  scale_x_discrete(labels = c("None",
                              "Before own birth",
                              "0-9 years old",
                              "10-19 years old"),
                   expand = c(0, 0.5)) +
  scale_shape_manual(values = c(4,1,19)) +
  guides(color = "none", shape = "none") +
  geom_vline(xintercept = c(1.5,2.5), linetype = "dotted")

ggsave(paste0("figures/preds_child.pdf"),
       plot=preds_child_plot,width=6,height=4,
       device = cairo_pdf)

# Construct map (Figure S1)
parishes <- read_csv("data_modif/parishes.csv")

target_moves <- read_csv("data_modif/target_moves_u_10km.csv") %>% 
  filter(idU %in% unions_main$idUnion)

# Quebec shapefile
# SOURCE: STATISTICS CANADA 
## https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
QC <- read_sf(dsn = "data_aux/shapefiles/lpr_000b21a_e", layer ="lpr_000b21a_e") %>% 
  st_simplify(., preserveTopology = FALSE, dTolerance = 1000) %>% 
  st_transform(., "+proj=longlat +datum=NAD83")

# To add the Saguenay River to the map layout
# SOURCE: STATISTICS CANADA 
## https://www12.statcan.gc.ca/census-recensement/alternative_alternatif.cfm?l=eng&dispext=zip&t=lhy_000c16a_e.zip&k=%20%20%20143102&loc=http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lhy_000c16a_e.zip
water <- read_sf(dsn = "data_aux/shapefiles/lhy_000c16a_e", layer ="lhy_000c16a_e") %>% 
  dplyr::filter(HYDROUID == 8105863 | NAME == "Rivière Saguenay / Saguenay River") %>% 
  st_transform(., "+proj=longlat +datum=NAD83")

parishes_indata <- data.frame(CodeLocation = c(target_moves$Par_A,target_moves$Par_B)) %>% 
  group_by(CodeLocation) %>% 
  mutate(n = dplyr::n()) %>% 
  ungroup() %>% 
  distinct() %>% 
  arrange(n) %>% 
  left_join(select(parishes,year_par,CodeLocation,lng,lat)) %>% 
  filter(year_par <= 1861)

coordinate_cities <- data.frame(
  city = c("Montreal","Quebec City"),
  lat = c(45.50466, 46.81363),
  long = c(-73.55681, -71.2057))

spectral_colors <- brewer.pal(11, "Spectral")
spectral_colors[6] <- "#FFFD7F" 

# FIGURE S1
map_parishes <- ggplot() + 
  geom_sf(data=QC, fill = "#f8f8f8") +
  geom_sf(data=water, fill = "white") +
  coord_sf(xlim = c(-77, -61.5), ylim = c(44.95, 50.7)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_point(data = parishes_indata, aes(x = lng, y = lat, fill = year_par),
             col = "black", stroke = 0.05,
             shape = 21, alpha = 0.9, size = 1.2) +
  scale_fill_gradientn(colors = spectral_colors) +
  geom_text_repel(data = coordinate_cities, aes(x = long, y = lat,label = city), size = 4, vjust = -1, 
                  family = "LM Roman 10", fontface="bold", nudge_x = c(-0.8), nudge_y = c(0.8), segment.size = 0.3) +
  labs(fill = "Parish registers started") +
  theme_void(base_family = "LM Roman 10", base_size = 12) +
  theme(legend.box="horizontal",
        legend.direction="horizontal",
        legend.position=c(0.75,0.25), 
        legend.text = element_text(size = 12,
                                   margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
        legend.title = element_text(size = 12,
                                    margin = margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),
        plot.margin = unit(c(0,0,0,0),units="mm"),
        legend.key.width  = unit(2.5, "lines"),
        legend.key.height = unit(0.6, "lines")) +
  guides(fill = guide_colorbar(title.position = "top",order=0)) +
  annotation_scale(location = "br",
                   pad_x = unit(0.6, "cm"),
                   text_family = "LM Roman 10",
                   text_cex = 0.9)

ggsave("figures/map_parishes_PNAS.pdf", device = cairo_pdf, plot=map_parishes, width = 6,height = 3)


# FIGURE S2: Distribution of distance

distr_distance <- unions_main %>% 
  select(distance) %>% 
  mutate(dist_cat = floor(distance*10)/10) %>% 
  dplyr::group_by(dist_cat) %>% 
  dplyr::summarize(n = dplyr::n())

distr_distance_plot <- ggplot(data=unions_main %>% filter(distance != 0),aes(x=distance)) +
  labs(x="Distance (log scale)", y="Density") +
  geom_density(adjust=1/2) +
  scale_x_log10(limits = c(0.0999,1500),breaks = c(0.1, 1, 5, 10, 25, 100, 1000), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1.59), expand = c(0,0), breaks = seq(0,1.5,by=0.5)) +
  geom_vline(xintercept = c(5,10,25,233,1.31), linetype = c("dotdash","longdash","dotdash","dotted","dotted")) +
  annotate(
    "text",
    x = 600, y = 0.5, hjust=0.5, label = "Montreal -\nQuebec City",
    family = "LM Roman 10") +
  annotate("curve", 
           x = 600, y = 0.4, xend = 250, yend = 0.2, 
           arrow = arrow(length = unit(0.3, "cm")), 
           curvature = -0.1, 
           color = "black") +
  annotate(
    "text",
    x = 0.55, y = 0.5, hjust=0.5, label = "Quebec City -\nSaint-Roch",
    family = "LM Roman 10") +
  annotate("curve", 
           x = 0.55, y = 0.4, xend = 1.2, yend = 0.25, 
           arrow = arrow(length = unit(0.3, "cm")), 
           curvature = 0.1, 
           color = "black") +
  theme_linedraw(base_family = 'LM Roman 10', base_size = 14) + 
  theme(text = element_text(color = "black"),
        axis.text.x = element_text(size = 10),
        plot.margin = unit(c(0,0,0,0), "lines"),
        panel.grid = element_blank())

ggsave("figures/distance_PNAS.pdf",distr_distance_plot,width=6,height=4,device=cairo_pdf)


# FIGURE S3: Histogram of number of moves

unions_main <- unions_main %>% 
  mutate(num_moves_mod = if_else(num_moves >= 6, "6+", paste(num_moves)))

hist <- ggplot(unions_main,aes(x=num_moves_mod)) +
  geom_bar(color = "black", 
                 fill = "white") +
  scale_y_continuous(breaks = seq(0, 60000, by = 20000),
                     limits = c(0,75000),expand = c(0, 0)) +
  labs(x = "Number of moves", y = "Number of couples")

hist_data <- ggplot_build(hist)$data[[1]]

hist <- hist +  
  geom_text(data = hist_data, 
                          aes(x = (xmin + xmax) / 2, y = count, label = count), 
                          vjust = -0.5,  
                          size = 3,
            family = "LM Roman 10") 

ggsave("figures/hist_number_moves.pdf", device=cairo_pdf, plot=hist, width = 8,height = 6)


# Survival plots (Figures S4 and S5)

if (file.exists("data_modif/surv_ratios.csv")) {
  health_scores <- read_csv("data_modif/surv_ratios.csv")
} else {
  source("code/compute_health.R")
  health_scores <- read_csv("data_modif/surv_ratios.csv")
}

unions_all <- read_csv("data_modif/unions_analysis_10km_20_all.csv")%>% 
  mutate(cens = pmin(Date_end,end_obs_u,na.rm=T),
         move_ever = if_else(is.na(Date_move_ever),0,1),
         surv = if_else(move_ever == 0, cens-start_obs_u, Date_move_ever-start_obs_u),
         surv = time_length(surv,"years")) %>% 
  left_join(select(health_scores,idHusband = idFather, idWife = idMother, norm_ratio)) %>% 
  mutate(health_ter = ntile(norm_ratio, 3))

unions_survival <- unions_main %>% 
  left_join(select(health_scores,idHusband = idFather, idWife = idMother, norm_ratio)) %>% 
  mutate(health_ter = ntile(norm_ratio, 3)) %>% 
  mutate(cens = pmin(Date_end,end_obs_u,na.rm=T),
         move_ever = if_else(is.na(Date_move_ever),0,1),
         surv = if_else(move_ever == 0, cens-start_obs_u, Date_move_ever-start_obs_u),
         surv = time_length(surv,"years")) %>% 
  select(surv,cens,move_ever,mig_u,health_ter)

unions_nosurv <- unions_all %>% 
  filter(!(idUnion %in% unions_main$idUnion)) 

unions_died <- unions_nosurv %>% 
  filter(died_before_surv == 1)

unions_after1851 <- unions_nosurv %>% 
  filter(!(idUnion %in% unions_died$idUnion))  %>% 
  filter(year_mar > 1851)

unions_unknown <- unions_nosurv %>% 
    filter(!(idUnion %in% unions_died$idUnion))  %>% 
    filter(!(idUnion %in% unions_after1851$idUnion)) 

unions_top <- unions_all %>% filter(health_ter == 1)
unions_mid <- unions_all %>% filter(health_ter == 2)
unions_bottom <- unions_all %>% filter(health_ter == 3)

surv_fit_main <- survfit(Surv(surv, move_ever) ~ 1, data=unions_survival, type = "kaplan-meier") 
surv_fit_all <- survfit(Surv(surv, move_ever) ~ 1, data=unions_all, type = "kaplan-meier") 
surv_fit_nosurv <- survfit(Surv(surv, move_ever) ~ 1, data=unions_nosurv, type = "kaplan-meier") 
surv_died <- survfit(Surv(surv, move_ever) ~ 1, data=unions_died, type = "kaplan-meier") 
surv_after1851 <- survfit(Surv(surv, move_ever) ~ 1, data=unions_after1851, type = "kaplan-meier") 
surv_unknown <- survfit(Surv(surv, move_ever) ~ 1, data=unions_unknown, type = "kaplan-meier") 

surv_top <- survfit(Surv(surv, move_ever) ~ 1, data=unions_top, type = "kaplan-meier") 
surv_mid <- survfit(Surv(surv, move_ever) ~ 1, data=unions_mid, type = "kaplan-meier") 
surv_bottom <- survfit(Surv(surv, move_ever) ~ 1, data=unions_bottom, type = "kaplan-meier") 

surv_plot_detailed <- ggsurvplot_combine(list(surv_fit_all,surv_fit_main,surv_fit_nosurv,surv_died,surv_after1851,surv_unknown),
                         censor = FALSE, linetype = 1,
                         conf.int = TRUE, conf.int.style = "ribbon", conf.int.alpha = 0.5,
                         ggtheme = theme_bw())

surv_plot_health <- ggsurvplot_combine(list(surv_fit_main,surv_top,surv_mid,surv_bottom),
                                         censor = FALSE, linetype = 1,
                                         conf.int = TRUE, conf.int.style = "ribbon", conf.int.alpha = 0.5,
                                         ggtheme = theme_bw())

surv_data_detailed <- surv_plot_detailed$plot$data
surv_data_health <- surv_plot_health$plot$data

surv_plot_time_detailed <- ggplot(surv_data_detailed,aes(x=time,y=surv,linetype=strata, col=strata)) +
  geom_line(linewidth = 0.7) +
  scale_linetype_manual(values = c(1,1,1,2,2,2), labels = c("All couples","Analytic sample","Couples observed for fewer than 10 years","Couples who experienced death within 10 years","Couples married after 1851","Couples otherwise lost before 10 years")) +
  scale_color_manual(values = c("black","darkgreen","darkgrey","coral","darkmagenta","deeppink"),  labels = c("All couples","Analytic sample","Couples observed for fewer than 10 years","Couples who experienced death within 10 years","Couples married after 1851","Couples otherwise lost before 10 years")) +
  scale_y_continuous(limits=c(0,1), expand = c(0,0), labels = function(x) as.character(x)) +
  scale_x_continuous(limits=c(0,65), breaks = seq(0,65,by=10), expand = c(0,0)) +
  labs(x = "Time since marriage", y = "Probability of not having migrated", color = NULL, linetype = NULL) +
  theme_bw(base_family = "LM Roman 10", base_size = 14) +
  theme(axis.title = element_text(size = 14),
        legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(color = "black", fill = "white")) +
  geom_vline(xintercept = c(10,20), linetype = c("dotted","dashed")) +
  annotate(
    "text",
    x = 20.5, y = 0.45, hjust=0, label = "Only moves in the 20 years\nafter marriage are considered",
    family = "LM Roman 10", size = 12/.pt) +
  annotate(
    "text",
    x = 10.5, y = 0.85, hjust=0, label = "Couples need to\nbe followed for\nat least 10 years",
    family = "LM Roman 10", size = 12/.pt)

surv_plot_time_health <- ggplot(surv_data_health,aes(x=time,y=surv,linetype=strata, col=strata)) +
  geom_line(linewidth = 0.7) +
  scale_linetype_manual(values = c(1,3,3,3), labels = c("Analytic sample","1st tertile (healthiest)","2nd tertile","3rd tertile (least healthy)")) +
  scale_color_manual(values = c("black","darkgreen","orange","purple"),  labels = c("Analytic sample","1st tertile (healthiest)","2nd tertile","3rd tertile (least healthy)")) +
  scale_y_continuous(limits=c(0.25,1), expand = c(0,0), labels = function(x) as.character(x)) +
  scale_x_continuous(limits=c(0,65), breaks = seq(0,65,by=10), expand = c(0,0)) +
  labs(x = "Time since marriage", y = "Probability of not having migrated", color = NULL, linetype = NULL) +
  theme_bw(base_family = "LM Roman 10", base_size = 14) +
  theme(axis.title = element_text(size = 14),
        legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(color = "black", fill = "white"))

# FIGURE S4
ggsave("figures/surv_PNAS_detailed.pdf",surv_plot_time_detailed,width=9,height=6,device=cairo_pdf)

# FIGURE S5
ggsave("figures/surv_PNAS_health.pdf",surv_plot_time_health,width=9,height=6,device=cairo_pdf)

rm(list = ls())
