rm(list = ls())
library(tidyverse)
library(survey)
library(srvyr)
library(coefplot)
library(ggthemes)
library(lemon)
library(ggforce)
library(scales)
library(grid)
library(cowplot)
library(haven)
library(xtable)
library(scales)

## Helper functions to make tables
add_parens <- function(x, digits = 2) {
  x <- as.numeric(x)
  return(paste0("(", sprintf(paste0("%.", digits, "f"), x), ")"))
}

format_num <- function(x, digits = 2) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

make_entry <- function(est, se, p, digits = 2) {
  entry <- paste0(format_num(est, digits = digits), " ", 
                  add_parens(se, digits = digits))
  entry[p < 0.05] <- paste0(entry[p < 0.05], "*")
  return(entry)
}

table_entry <- function(est, se, digits = 2) {
  entry <- paste0(format_num(est, digits = digits), " ", 
                  add_parens(se, digits = digits))
  return(entry)
}


## Note: we need the options below else the variances cannot be estimated for
## weighted calculations involving RPH's sample of police due to lonely PSUs
## see: https://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options(survey.lonely.psu = "adjust")
options(na.action = "na.pass")


### ---- Load GSS data and occupational codes, code up variables of interest -----------
## Load GSS series downloaded from here: http://gss.norc.org/get-the-data/stata 
gss <-
  read_dta("GSS7218_R3.dta")

## Crosswalk for occupational codes 
occ_codes <- read_csv("occ10-to-egp-class-crosswalk.csv")

## Recode outcomes using authors' coding scheme, and generate indicator for 
## police officers and their supervisors 
## See p.50: http://gss.norc.org/Documents/reports/methodological-reports/MR125.pdf
gss_dat <-
  gss %>%
  mutate(
    ## NB: 3850 police officers; 3710 First-Line Supervisors Of Police And
    ## Detectives; 3820 is Detectives And Criminal Investigators
    police = ifelse(occ10 %in% c(3850, 3710, 3820), "Yes", "No"),
    military = ifelse(occ10 %in% c(9800, 9810, 9820, 9830), "Yes", "No"),
    teacher = ifelse(occ10 %in% c(2310, 2320, 2330, 2300), "Yes", "No"),
    scientist = ifelse(
      occ10 %in% c(1600, 1710, 1610, 1720, 1005, 1640, 1740, 1650, 1760, 1860,
        1815,1830), "Yes", "No"
    ),
    lazy_black = case_when(
      workblks == 98 ~ tagged_na("d"),
      workblks == 99 ~ tagged_na("n"),
      TRUE ~ as.numeric(workblks)
    ),
    lazy_white = case_when(
      workwhts == 98 ~ tagged_na("d"),
      workwhts == 99 ~ tagged_na("n"),
      TRUE ~ as.numeric(workwhts)
    ),
    unint_black = case_when(
      intlblks == 98 ~ tagged_na("d"),
      intlblks == 99 ~ tagged_na("n"),
      TRUE ~ 8 - as.numeric(intlblks)
    ),
    unint_white = case_when(
      intlwhts == 98 ~ tagged_na("d"),
      intlwhts == 99 ~ tagged_na("n"),
      TRUE ~ 8 - as.numeric(intlwhts)
    ),
    ## Blacks are lazier than Whites
    bw_lazygap = as.numeric(lazy_black - lazy_white),
    ## Blacks less intelligent than Whites
    bw_unintgap = as.numeric(unint_black - unint_white),
    ## Feel closer to Whites than Blacks
    bw_close = as.numeric(closeblk - closewht),
    racdif_discrim = case_when(
      racdif1 == 1 ~ "Yes",
      racdif1 == 2 ~ "No",
      is_tagged_na(racdif1, "d") ~ "DK/NR",
      is_tagged_na(racdif1, "n") ~ "DK/NR"
    ),
    racdif_ability = case_when(
      racdif2 == 1 ~ "Yes",
      racdif2 == 2 ~ "No",
      is_tagged_na(racdif2, "d") ~ "DK/NR",
      is_tagged_na(racdif2, "n") ~ "DK/NR"
    ),
    racdif_education = case_when(
      racdif3 == 1 ~ "Yes",
      racdif3 == 2 ~ "No",
      is_tagged_na(racdif3, "d") ~ "DK/NR",
      is_tagged_na(racdif3, "n") ~ "DK/NR"
    ),
    racdif_motivation = case_when(
      racdif4 == 1 ~ "Yes",
      racdif4 == 2 ~ "No",
      is_tagged_na(racdif4, "d") ~ "DK/NR",
      is_tagged_na(racdif4, "n") ~ "DK/NR"
    ),
    ## Assistance to Blacks
    blkassist_spend = case_when(
      natracey == 1 ~ "Too little",
      natracey == 2 ~ "About right",
      natracey == 3 ~ "Too much",
      is_tagged_na(natracey, "d") ~ "DK/NR",
      is_tagged_na(natracey, "n") ~ "DK/NR"
    ),
    ## Improving the conditions of Blacks
    blkimprove_spend = case_when(
      natrace == 1 ~ "Too little",
      natrace == 2 ~ "About right",
      natrace == 3 ~ "Too much",
      is_tagged_na(natrace, "d") ~ "DK/NR",
      is_tagged_na(natrace, "n") ~ "DK/NR"
    ),
    police_spend = case_when(
      natcrimy == 1 ~ "Too little",
      natcrimy == 2 ~ "About right",
      natcrimy == 3 ~ "Too much",
      is_tagged_na(natcrimy, "d") ~ "DK/NR",
      is_tagged_na(natcrimy, "n") ~ "DK/NR"
    ),
    police_strike = case_when(
      polhitok == 1 ~ "Yes",
      polhitok == 2 ~ "No",
      is_tagged_na(polhitok, "d") ~ "DK/NR",
      is_tagged_na(polhitok, "n") ~ "DK/NR"
    ),
    dem_female = as.numeric(sex == 2),
    dem_race = case_when(race == 1 ~ "White",
                         race == 2 ~ "Black",
                         race == 3 ~ "Other"),
    dem_race = factor(dem_race, levels = c("White", "Black", "Other")),
    dem_edu_cat = case_when(
      degree == 0 ~ "No high school diploma",
      degree == 1 ~ "High school diploma",
      degree == 2 ~ "Associate's degree",
      degree == 3 ~ "Bachelor's degree",
      degree == 4 ~ "Graduate degree"
    ),
    dem_edu_yrs = as.numeric(educ),
    dem_income = as.numeric(conrinc),
    dem_income_cat = case_when(
      dem_income < 20000 ~ "$19,999 or less",
      dem_income >= 20000 & dem_income < 35000 ~ "$20,000 - $34,999",
      dem_income >= 35000 & dem_income < 50000 ~ "$35,000 - $49,999",
      dem_income >= 50000 & dem_income < 65000 ~ "$50,000 - $64,999",
      dem_income >= 65000 & dem_income < 80000 ~ "$65,000 - $79,999",
      dem_income >= 80000 & dem_income < 100000 ~ "$80,000 - $99,999",
      dem_income >= 100000 & dem_income < 125000 ~"$100,000 - $124,999",
      dem_income >= 125000 & dem_income < 200000 ~ "$125,000 - $199,999",
      dem_income >= 200000  ~ "$200,000 and above"
    ),
    dem_income_cat = factor(
      dem_income_cat,
      levels = c(
        "$19,999 or less",
        "$20,000 - $34,999",
        "$35,000 - $49,999",
        "$50,000 - $64,999",
        "$65,000 - $79,999",
        "$80,000 - $99,999",
        "$100,000 - $124,999",
        "$125,000 - $199,999",
        "$200,000 and above"
      )
    ),
    dem_region = case_when(
      region %in% 1:2 ~ "Northeast",
      region %in% 3:4 ~ "Midwest",
      region %in% 5:7 ~ "South",
      region %in% 8:9 ~ "West"
    ),
    dem_conserv = as.numeric(polviews),
    dem_partyid = na_if(partyid, 7),
    dem_partyid =  dem_partyid  + 1,
    republican = case_when(dem_partyid > 4 ~ "Republican",
                           dem_partyid < 4 ~ "Democrat"),
    dem_union = case_when(union %in% c(1, 3) | memunion == 1 ~ 1,
                          TRUE ~ 0),
    dem_emp = case_when(
      wrkstat == 1 ~ "Full-time",
      wrkstat == 2 ~ "Part-time",
      wrkstat == 5 ~ "Retired",
      TRUE ~ "Not employed"
    ),
    dem_emp = factor(
      dem_emp,
      levels = c("Not employed", "Retired", "Part-time",
                 "Full-time")
    ),
    dem_size_group = case_when(
      size >= 1000 ~ "1000+",
      size >= 500 & size < 1000  ~ "[500,1000)",
      size >= 250 & size < 500   ~ "[250,500)",
      size >= 100 & size < 250   ~ "[100,250)",
      size >= 50 & size < 100    ~ "[50,100)",
      size >= 25 & size < 50     ~ "[25,50)",
      size >= 10 & size < 25     ~ "[10,25)",
      size < 10                  ~ "Under 10"
    ),
    dem_size_group = factor(
      dem_size_group,
      levels = c(
        "1000+",
        "[500,1000)",
        "[250,500)",
        "[100,250)",
        "[50,100)",
        "[25,50)",
        "[10,25)",
        "Under 10"
      )
    ),
    ## Age categories from ACS
    dem_age = as.numeric(age),
    dem_age_cat = case_when(
      age %in% c(18:23) ~ '18-23',
      age %in% c(24:29) ~ '24-29',
      age %in% c(30:39) ~ '30-39',
      age %in% c(40:49) ~ '40-49',
      age %in% c(50:59) ~ '50-59',
      age >= 60 ~ '60+'
    ),
    dem_age_cat = factor(dem_age_cat, levels = c('18-23',
                                                 '24-29',
                                                 '30-39',
                                                 '40-49',
                                                 '50-59',
                                                 '60+')),
  ) %>%
  left_join(occ_codes, by = "occ10")


### ---- Analyses for Figure 1 and 2 -----
ungroup.rowwise_df <- function(x) {
  class(x) <- c( "tbl_df", "data.frame")
  x
}

gss_svy_natcrimy <-
  gss_dat %>%
  filter(year >= 1984, police == "No") %>%
  select(natcrimy, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

est_natcrimy_police <- 
  gss_dat %>%
  filter(year >= 1984, police == "Yes") %>%
  select(year, natcrimy) %>%
  group_by(year) %>%
  summarise(x = sum(natcrimy == 3, na.rm = TRUE),
            n = sum(!is.na(natcrimy))) %>%
  rowwise() %>%
  mutate(
    prop = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[4]),
    prop_low = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[5]),
    prop_upp = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[6])
  ) %>% 
  ungroup.rowwise_df %>%  
  mutate(police = "Yes",
         prop_low = ifelse(is.na(prop), NA, prop_low),
         prop_upp = ifelse(is.na(prop), NA, prop_upp)) %>% 
  select(-x)

est_natcrimy <-
  gss_svy_natcrimy %>%
  group_by(year) %>%
  summarize(n = n(),
            prop = survey_mean(as.numeric(natcrimy == 3), vartype = "ci", 
                               proportion = TRUE, na.rm = TRUE)) %>% 
  mutate(police = "No") %>% 
  bind_rows(est_natcrimy_police)

est_polhitok_police <- 
  gss_dat %>%
  filter(year >= 1984, police == "Yes") %>%
  select(year, polhitok) %>%
  group_by(year) %>%
  summarise(x = sum(polhitok == 1, na.rm = TRUE),
            n = sum(!is.na(polhitok))) %>%
  rowwise() %>%
  mutate(
    prop = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[4]),
    prop_low = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[5]),
    prop_upp = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[6])
  ) %>% 
  ungroup.rowwise_df %>%  
  mutate(police = "Yes",
         prop_low = ifelse(is.na(prop), NA, prop_low),
         prop_upp = ifelse(is.na(prop), NA, prop_upp)) %>% 
  select(-x) 

gss_svy_polhitok <-
  gss_dat %>%
  filter(year >= 1984, police == "No") %>%
  select(polhitok, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

est_polhitok <-
  gss_svy_polhitok %>%
  group_by(year) %>%
  summarize(n = n(),
            prop = survey_mean(as.numeric(polhitok == 1), proportion = TRUE, 
                               vartype = "ci", na.rm = TRUE)) %>% 
  mutate(police = "No") %>% 
  bind_rows(data.frame(year = 1985, n = NA, prop = NA, prop_low = NA, prop_upp = NA,
                       police = "No")) %>% 
  bind_rows(est_polhitok_police)


est_natracey_police <- 
  gss_dat %>%
  filter(year >= 1984, police == "Yes") %>%
  select(year, natracey) %>%
  group_by(year) %>%
  summarise(x = sum(natracey == 3, na.rm = TRUE),
            n = sum(!is.na(natracey))) %>%
  rowwise() %>%
  mutate(
    prop = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[4]),
    prop_low = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[5]),
    prop_upp = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[6])
  ) %>% 
  ungroup.rowwise_df %>%  
  mutate(police = "Yes",
         prop_low = ifelse(is.na(prop), NA, prop_low),
         prop_upp = ifelse(is.na(prop), NA, prop_upp)) %>% 
  select(-x) 


gss_svy_natracey <-
  gss_dat %>%
  filter(year >= 1984, police == "No") %>%
  select(natracey, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

est_natracey <-
  gss_svy_natracey %>%
  group_by(year) %>%
  summarize(n = n(),
            prop = survey_mean(as.numeric(natracey == 3), proportion = TRUE, 
                               vartype = "ci", na.rm = TRUE)) %>% 
  mutate(police = "No") %>% 
  bind_rows(est_natracey_police)



gss_svy_racdif1 <-
  gss_dat %>%
  filter(year >= 1984, police == "No") %>%
  select(racdif1, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

est_racdif1_police <- 
  gss_dat %>%
  filter(year >= 1984, police == "Yes") %>%
  select(year, racdif1) %>%
  group_by(year) %>%
  summarise(x = sum(racdif1 == 1, na.rm = TRUE),
            n = sum(!is.na(racdif1))) %>%
  rowwise() %>%
  mutate(
    prop = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[4]),
    prop_low = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[5]),
    prop_upp = as.numeric(binom.confint(
      x = x,
      n = n,
      conf.level = 0.95,
      methods = "exact"
    )[6])
  ) %>% 
  ungroup.rowwise_df %>%  
  mutate(police = "Yes",
         prop_low = ifelse(is.na(prop), NA, prop_low),
         prop_upp = ifelse(is.na(prop), NA, prop_upp)) %>% 
  select(-x) 

est_racdif1 <-
  gss_svy_racdif1 %>%
  group_by(year) %>%
  summarize(n = n(),
            prop = survey_mean(as.numeric(racdif1 == 1), proportion = TRUE, 
                               vartype = "ci", na.rm = TRUE)) %>% 
  mutate(police = "No") %>% 
  bind_rows(est_racdif1_police)


## Combine for plotting:
gg_natcrime <-
  est_natcrimy %>%
  mutate(outcome = "Too Much Spending on Law Enforcement (natcrimy)",
         response = "natcrimy")

gg_polhitok <-
  est_polhitok %>%
  mutate(outcome = "Can Imagine Approving of Policeman Striking Adult Male (polhitok)",
         response = "polhitok")

gg_natracey <-
  est_natracey %>%
  mutate(outcome = "Too Much Spending on Assistance to Blacks (natracey)",
         response = "natracey")

gg_racdif1 <-
  est_racdif1 %>%
  mutate(outcome = "Black-White Inequality Mainly Due to Discrimination (racdif1)",
         response = "racdif1")


gg_df <-
  bind_rows(gg_natcrime, gg_polhitok, gg_natracey, gg_racdif1) %>%
  mutate(outcome = factor(outcome,
                          levels = c("Too Much Spending on Law Enforcement (natcrimy)",
                                     "Can Imagine Approving of Policeman Striking Adult Male (polhitok)",
                                     "Too Much Spending on Assistance to Blacks (natracey)",
                                     "Black-White Inequality Mainly Due to Discrimination (racdif1)"))
  ) %>% 
  mutate_if(is.numeric, replace_na, NA) %>% 
  mutate(n = ifelse(is.na(prop), NA, n))


## Average width of CIs
gg_df %>% 
  filter(police == "Yes") %>% 
  group_by(response) %>% 
  summarise(avg_width = mean(prop_upp - prop_low, na.rm = TRUE))

## Simple F-tests for changes over time
# lm(as.numeric(natcrimy == 3) ~ as.factor(year), na.action = na.omit,
#           data = gss_dat %>% filter(year >= 1984)) %>% 
#   summary() 
# 
# lm(as.numeric(polhitok == 1) ~ as.factor(year), na.action = na.omit,
#    data = gss_dat %>% filter(year >= 1984)) %>% 
#   summary() 
# 
# lm(as.numeric(natracey == 3) ~ as.factor(year), na.action = na.omit,
#    data = gss_dat %>% filter(year >= 1984)) %>% 
#   summary() 
# 
# lm(as.numeric(racdif1 == 1) ~ as.factor(year), na.action = na.omit,
#    data = gss_dat %>% filter(year >= 1984)) %>%
#   summary() 


### ---- Generate Figure 1 -------
g_police_top <-
  ggplot(
    gg_df %>% 
      filter(response %in% c("natcrimy", "polhitok")) %>% na.omit(),
    aes(
      x = as.factor(year),
      y = prop,
      ymin = prop_low,
      ymax = prop_upp,
      color = police,
      group = police,
      fill = police
    )
  ) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.5, color = NA) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c("#E69F00", "#0072B2"),
    labels = c("Non-police", "Police"),
    guide = guide_legend(title = NULL)
  ) +
  scale_fill_manual(
    values = c("#E69F00", "#0072B2"),
    labels = c("Non-police", "Police"),
    guide = guide_legend(title = NULL)
  ) +
  theme_fivethirtyeight() +
  facet_col(~ outcome, scales = "free") + 
  labs(
    x = "",
    title = "A. Estimated proportion agreeing with statements",
    y = ""
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0, face = "plain",
                              size = 10),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.key.height = unit(0, "cm"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    strip.background = element_blank(),
    legend.margin = margin(
      t = -0.75,
      r = 0,
      b = 0.5,
      l = 0,
      unit = "lines"
    ),
    plot.margin = unit(c(0.1, 0.2, 0, 0.2), "lines")
  )


g_police_btm <-
  ggplot(
    gg_df %>% filter(response %in% c("natcrimy", "polhitok"), police == "Yes"), 
    aes(
      x = as.factor(year),
      y = n,
      label = n
    )
  ) +
  geom_point(stat = 'identity', fill = "black", size = 4)  +
  geom_segment(aes(
    y = 0,
    x = as.factor(year),
    yend = n,
    xend = as.factor(year)
  ),
  color = "black") +
  geom_text(color = "white", size = 2) +
  scale_y_continuous(limits = c(0, 17), breaks = seq(0, 15, 5), expand = c(0,0)) +
  theme_fivethirtyeight() +
  facet_col(~ outcome) + 
  labs(
    x = "",
    y = "",
    title = "B. Number of police responding to questions",
    caption = ""
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0, face = "plain",
                              size = 10),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.key.height = unit(0, "cm"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    strip.background = element_blank(),
    legend.margin = margin(
      t = -0.75,
      r = 0,
      b = 0.5,
      l = 0,
      unit = "lines"
    ),
    plot.margin = unit(c(0, 0.2, 0, 0.2), "lines")
  )

g <-
  plot_grid(
    g_police_top,
    g_police_btm,
    ncol = 1,
    align = "v",
    rel_heights = c(0.9, 0.7)
  )


g %>%
  ggsave(
    filename = "police_series_plot.pdf",
    .,
    width = 6.5,
    height = 6
  )


### ---- Generate FIgure 2 -------
g_race_top <-
  ggplot(
    gg_df %>% 
      filter(response %in% c("natracey", "racdif1")) %>% na.omit(),
    aes(
      x = as.factor(year),
      y = prop,
      ymin = prop_low,
      ymax = prop_upp,
      color = police,
      group = police,
      fill = police
    )
  ) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.5, color = NA) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c("#E69F00", "#0072B2"),
    labels = c("Non-police", "Police"),
    guide = guide_legend(title = NULL)
  ) +
  scale_fill_manual(
    values = c("#E69F00", "#0072B2"),
    labels = c("Non-police", "Police"),
    guide = guide_legend(title = NULL)
  ) +
  theme_fivethirtyeight() +
  facet_col(~ outcome, scales = "free") + 
  labs(
    x = "",
    title = "A. Estimated proportion agreeing with statements",
    y = ""
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0, face = "plain",
                              size = 10),
    strip.text = element_text(size = 8),
    legend.key.height = unit(0, "cm"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    strip.background = element_blank(),
    legend.margin = margin(
      t = -0.75,
      r = 0,
      b = 0.5,
      l = 0,
      unit = "lines"
    ),
    plot.margin = unit(c(0.1, 0.2, 0, 0.2), "lines")
  )


g_race_btm <-
  ggplot(
    gg_df %>% filter(response %in% c("natracey", "racdif1"), police == "Yes"), 
    aes(
      x = as.factor(year),
      y = n,
      label = n
    )
  ) +
  geom_point(stat = 'identity', fill = "black", size = 4)  +
  geom_segment(aes(
    y = 0,
    x = as.factor(year),
    yend = n,
    xend = as.factor(year)
  ),
  color = "black") +
  geom_text(color = "white", size = 2) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 15, 5), expand = c(0,0)) +
  theme_fivethirtyeight() +
  facet_col(~ outcome) + 
  labs(
    x = "",
    y = "",
    title = "B. Number of police responding to questions",
    caption = ""
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0, face = "plain",
                              size = 10),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.key.height = unit(0, "cm"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    strip.background = element_blank(),
    legend.margin = margin(
      t = -0.75,
      r = 0,
      b = 0.5,
      l = 0,
      unit = "lines"
    ),
    plot.margin = unit(c(0, 0.2, 0, 0.2), "lines")
  )

g <-
  plot_grid(
    g_race_top,
    g_race_btm,
    ncol = 1,
    align = "v",
    rel_heights = c(0.9, 0.7)
  )


g %>%
  ggsave(
    filename = "race_series_plot.pdf",
    .,
    width = 6.5,
    height = 6
  )

### ---- Analyses for Table 1 and export -----

## Note: this throws warning messages when fitting RPH's logistic regressions. 
## I'm just replicating their model specifications here. A simple diff-in-means
## on the non-truncated DVs or OLS w/ covariate adjustment would be a more
## sensible approach for looking at these correlations

## Replicate estimates reported in Table 2 for spending on assistance
## to blacks. This is natracey (see p. 277 in GSS codebook)
gss_svy_natracey <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, natracey, dem_race, dem_female, dem_age, 
         vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

## What does OLS say? 
svyglm(
  as.numeric(natracey == 3) ~ police , design = gss_svy_natracey
) %>% summary()


logit_fit_natracey <- 
  svyglm(
    as.numeric(natracey == 3) ~ police + dem_race + dem_female + 
      dem_age + year, design = gss_svy_natracey, family = binomial
  )

gg_est_natracey <- 
  tidy(logit_fit_natracey) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "natracey")

## extract predicted probabilities 
predict(
  logit_fit_natracey,
  data.frame(
    police = c("Yes", "No", "Yes", "No"),
    dem_race = c("White", "White", "Black", "Black"),
    dem_female = 0,
    dem_age = 45,
    year = 2000
  ),
  type = "response"
)

## Do these differences hold for everyone? Make simple comparison for Black
## Cops and female cops 
gss_svy_natracey <-
  gss_dat %>%
  filter(year >= 1984, dem_race != "White") %>%
  select(police, natracey, dem_female, dem_age, dem_race,
         vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

svyglm(
  as.numeric(natracey == 3) ~ police + dem_female + 
    dem_age + year, design = gss_svy_natracey, family = binomial
) %>% summary()


## Apply the same model to natrace ("Improving the conditions of Blacks"), which
## has fewer missing values for both police and non-police in the GSS series
gss_svy_natrace <-
  gss_dat %>%
  #filter(year >= 1984) %>%
  select(police, natrace, dem_race, dem_female, dem_age, 
         vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_natrace <- 
  svyglm(
    as.numeric(natrace == 3) ~ police + dem_race + dem_female + 
      dem_age + year, design = gss_svy_natrace, family = binomial
  )

gg_est_natrace <- 
  tidy(logit_fit_natrace) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "natrace")

gss_svy_natrace_subset <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, natrace, dem_race, dem_female, dem_age, 
         vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_natrace_subset <- 
  svyglm(
    as.numeric(natrace == 3) ~ police + dem_race + dem_female + 
      dem_age + year, design = gss_svy_natrace_subset, family = binomial
  )

gg_est_natrace_subset <- 
  tidy(logit_fit_natrace_subset) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "natrace_subset")

## Combine results and export table
gg_natrace_df <- 
  bind_rows(gg_est_natracey, gg_est_natrace_subset, gg_est_natrace) %>% 
  mutate(term = factor(term, levels = c("policeYes",
                                        "dem_raceBlack", 
                                        "dem_raceOther",
                                        "dem_female",
                                        "dem_age",
                                        "year",
                                        "(Intercept)"),
                       labels = c("Police = Yes",
                                  "Race = Black",
                                  "Race = Other",
                                  "Sex = Female",
                                  "Age",
                                  "Year",
                                  "Constant")),
         outcome = factor(outcome, levels = c("natracey", "natrace_subset",
                                              "natrace")))


gg_natrace_df %>% 
  mutate(entry = make_entry(est = estimate, se = std.error, p = p.value)) %>% 
  select(term, entry, outcome) %>%
  spread(outcome, entry) %>%
  xtable() %>%
  print(include.rownames = FALSE, 
        include.colnames = FALSE,
        hline.after = c(),
        only.contents = TRUE,
        type = "latex",
        file = "table2_natrace.tex")


### ---- Analyses for Table 2 and export -----

## Note: this throws warning messages when fitting RPH's logistic regressions. 
## I'm just replicating their model specifications here. A simple diff-in-means
## on the non-truncated DVs or OLS w/ covariate adjustment would be a more
## sensible approach for looking at these correlations

## Replicate estimates reported in RPH Table 2 for racdif1
gss_svy_racdif1 <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, racdif1, dem_race, dem_female, dem_age, 
         vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_racdif1 <- 
  svyglm(
    as.numeric(racdif1 == 1) ~ police*dem_race + police*dem_female + 
      dem_age + year, design = gss_svy_racdif1, family = binomial
  )

gg_est_racdif1 <- 
  tidy(logit_fit_racdif1) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "racdif1")

## What's going on here? 
predict(
  logit_fit_racdif1,
  data.frame(
    police = c("Yes", "No"),
    dem_race = "White",
    dem_female = 0,
    dem_age = 45,
    year = 2000
  ),
  type = "link"
)

## What if we fit the same model as for natracey?
svyglm(
  as.numeric(racdif1 == 1) ~ police + dem_race + dem_female + 
    dem_age + year, design = gss_svy_racdif1, family = binomial
) %>% summary() 

## As before, what if the same model (w/o interactions) were fit to nonwhite
## respondents
gss_svy_racdif1 <-
  gss_dat %>%
  filter(year >= 1984, dem_race != "White") %>%
  select(police, racdif1, dem_female, dem_age, dem_race,
         vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

svyglm(
  as.numeric(racdif1 == 1) ~ police + dem_female + 
    dem_age + year, design = gss_svy_racdif1,
  family = binomial
) %>% summary()


## Replicate same specification on the other 3 items 

## Reason 2: Because most have less in-born ability to learn?
gss_svy_racdif2 <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, racdif2, dem_race, dem_female, dem_age, 
         vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_racdif2 <- 
  svyglm(
    as.numeric(racdif2 == 1) ~ police*dem_race + police*dem_female + 
      dem_age + year, design = gss_svy_racdif2, family = binomial
  )

gg_est_racdif2 <- 
  tidy(logit_fit_racdif2) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "racdif2")

## Reason 3: Because most don't have the chance for education that it takes to 
## rise out of poverty?
gss_svy_racdif3 <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, racdif3, dem_race, dem_female, dem_age, 
         vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_racdif3 <- 
  svyglm(
    as.numeric(racdif3 == 1) ~ police*dem_race + police*dem_female + 
      dem_age + year, design = gss_svy_racdif3, family = binomial
  )

gg_est_racdif3 <- 
  tidy(logit_fit_racdif3) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "racdif3")

## Reason 4: Because most just don't have the motivation or will power to pull 
## themselves up out of poverty?
gss_svy_racdif4 <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, racdif4, dem_race, dem_female, dem_age, 
         vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_racdif4 <- 
  svyglm(
    as.numeric(racdif4 == 1) ~ police*dem_race + police*dem_female + 
      dem_age + year, design = gss_svy_racdif4, family = binomial
  )

gg_est_racdif4 <- 
  tidy(logit_fit_racdif4) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "racdif4")


## Combine results and export table
gg_racdif_df <- 
  bind_rows(gg_est_racdif1, gg_est_racdif2, gg_est_racdif3, gg_est_racdif4) %>% 
  mutate(term = factor(term, levels = c("policeYes",
                                        "dem_raceBlack", 
                                        "policeYes:dem_raceBlack",
                                        "dem_raceOther",
                                        "policeYes:dem_raceOther",
                                        "dem_female",
                                        "policeYes:dem_female",
                                        "dem_age",
                                        "year",
                                        "(Intercept)"),
                       labels = c("Police = Yes",
                                  "Race = Black",
                                  "Black x Police",
                                  "Race = Other",
                                  "Other x Police",
                                  "Sex = Female",
                                  "Female x Police",
                                  "Age",
                                  "Year",
                                  "Constant")),
         outcome = factor(outcome, levels = c("racdif1", "racdif2", "racdif3",
                                              "racdif4")))


gg_racdif_df %>% 
  mutate(entry = make_entry(est = estimate, se = std.error, p = p.value)) %>% 
  select(term, entry, outcome) %>%
  spread(outcome, entry) %>%
  xtable() %>%
  print(include.rownames = FALSE, 
        include.colnames = FALSE,
        hline.after = c(),
        only.contents = TRUE,
        type = "latex",
        file = "table2_racdif.tex")

### ---- Analyses for Table 3 and export -----
### Polhitok
gss_svy_polhitok <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, polhitok, dem_race, dem_female, dem_age, vpsu, wtssall, year, 
         vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_polhitok <- 
  svyglm(
    as.numeric(polhitok == 1) ~ police + dem_race + dem_female + 
      police*dem_age + year, design = gss_svy_polhitok, family = binomial
  )

gg_est_polhitok <- 
  tidy(logit_fit_polhitok) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "polhitok")

## extract predicted probabilities 
predict(
  logit_fit_polhitok,
  data.frame(
    police = c("Yes", "No"),
    dem_race = c("White"),
    dem_female = 0,
    dem_age = 0,
    year = 2000
  ),
  type = "link"
)

## extract predicted probabilities 
predict(
  logit_fit_polhitok,
  data.frame(
    police = c("Yes", "No"),
    dem_race = c("White"),
    dem_female = 0,
    dem_age = 45,
    year = 2000
  ),
  type = "link"
)

### Polabuse
gss_svy_polabuse <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, polabuse, dem_race, dem_female, dem_age, vpsu, wtssall, year, 
         vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_polabuse <- 
  svyglm(
    as.numeric(polabuse == 1) ~ police + dem_race + dem_female + 
      police*dem_age + year, design = gss_svy_polabuse, family = binomial
  )

gg_est_polabuse <- 
  tidy(logit_fit_polabuse) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "polabuse")


### polmurdr
gss_svy_polmurdr <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, polmurdr, dem_race, dem_female, dem_age, vpsu, wtssall, year, 
         vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_polmurdr <- 
  svyglm(
    as.numeric(polmurdr == 1) ~ police + dem_race + dem_female + 
      police*dem_age + year, design = gss_svy_polmurdr, family = binomial
  )

gg_est_polmurdr <- 
  tidy(logit_fit_polmurdr) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "polmurdr")

### polescape
gss_svy_polescap <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, polescap, dem_race, dem_female, dem_age, vpsu, wtssall, year, 
         vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_polescap <- 
  svyglm(
    as.numeric(polescap == 1) ~ police + dem_race + dem_female + 
      police*dem_age + year, design = gss_svy_polescap, family = binomial
  )

gg_est_polescap <- 
  tidy(logit_fit_polescap) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "polescap")

### polattak
gss_svy_polattak <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, polattak, dem_race, dem_female, dem_age, vpsu, wtssall, year, 
         vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

logit_fit_polattak <- 
  svyglm(
    as.numeric(polattak == 1) ~ police + dem_race + dem_female + 
      police*dem_age + year, design = gss_svy_polattak, family = binomial
  )

gg_est_polattak <- 
  tidy(logit_fit_polattak) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "polattak")


## Combine results and export table
gg_polhit_df <- 
  bind_rows(gg_est_polhitok, gg_est_polabuse, gg_est_polmurdr,gg_est_polescap,
            gg_est_polattak) %>% 
  mutate(term = factor(term, levels = c("policeYes",
                                        "dem_raceBlack", 
                                        "dem_raceOther",
                                        "dem_female",
                                        "dem_age",
                                        "policeYes:dem_age",
                                        "year",
                                        "(Intercept)"),
                       labels = c("Police = Yes",
                                  "Race = Black",
                                  "Race = Other",
                                  "Sex = Female",
                                  "Age",
                                  "Age x Police",
                                  "Year",
                                  "Constant")),
         outcome = factor(outcome, levels = c("polhitok", "polabuse", "polmurdr",
                                              "polescap", "polattak")))


gg_polhit_df %>% 
  mutate(entry = make_entry(est = estimate, se = std.error, p = p.value)) %>% 
  select(term, entry, outcome) %>%
  spread(outcome, entry) %>%
  xtable() %>%
  print(include.rownames = FALSE, 
        include.colnames = FALSE,
        hline.after = c(),
        only.contents = TRUE,
        type = "latex",
        file = "table1_polhit.tex")

### ---- Analyses for Figures 3, 4, 7, 8 ----

## NOTE: THIS TAKES A LONG TIME TO RUN! YOU CAN SIMPLY IMPORT THE ESTIMATES 
## FROM THE PLOTTING SECTION BELOW. 

## Loop through all other occupations w/ at least 100 units
these <- as.numeric(names(which(table(gss_dat$occ10) > 100)))
these <- these[!(these %in% c(3850, 3710, 3820))]


### Discrimination 
gss_svy_discrim <-
  gss_dat %>%
  filter(year >= 1984, !(occ10 %in% c(3850, 3710, 3820))) %>%
  select(occ10, racdif1, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

glm_fits_discrim <- list()
for(i in 1:length(these)) { 
  glm_fits_discrim[[i]] <- 
    coef(summary(
      svyglm(
        as.formula(
          paste0("as.numeric(racdif1 == 1) ~ as.numeric(occ10 ==", these[i],")")
        ),
        design = gss_svy_discrim
      )
    ))[2,]
}

gss_svy_discrim <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, racdif1, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

police_fit_discrim <- coef(summary(
  svyglm(
    as.numeric(racdif1 == 1) ~ police, 
    design = gss_svy_discrim
  )
))[2,]


### Assistance to blacks
gss_svy_blkassist <-
  gss_dat %>%
  filter(year >= 1984, !(occ10 %in% c(3850, 3710, 3820))) %>%
  select(occ10, natracey, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

glm_fits_blkassist <- list()
for(i in 1:length(these)) { 
  glm_fits_blkassist[[i]] <- 
    coef(summary(
      svyglm(
        as.formula(
          paste0("as.numeric(natracey == 3) ~ as.numeric(occ10 ==", these[i],")")
        ),
        design = gss_svy_blkassist
      )
    ))[2,]
}

gss_svy_blkassist <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, natracey, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

police_fit_blkassist<- coef(summary(
  svyglm(
    as.numeric(natracey == 3) ~ police, 
    design = gss_svy_blkassist
  )
))[2,]

### Spending on police
gss_svy_polspend <-
  gss_dat %>%
  filter(year >= 1984, !(occ10 %in% c(3850, 3710, 3820))) %>%
  select(occ10, natcrimy, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

glm_fits_polspend <- list()
for(i in 1:length(these)) { 
  glm_fits_polspend[[i]] <- 
    coef(summary(
      svyglm(
        as.formula(
          paste0("as.numeric(natcrimy == 3) ~ as.numeric(occ10 ==", these[i],")")
        ),
        design = gss_svy_polspend
      )
    ))[2,]
}

gss_svy_polspend <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, natcrimy, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

police_fit_polspend <- coef(summary(
  svyglm(
    as.numeric(natcrimy == 3) ~ police, 
    design = gss_svy_polspend 
  )
))[2,]


## Imagining police striking 
gss_svy_polstrike <-
  gss_dat %>%
  filter(year >= 1984, !(occ10 %in% c(3850, 3710, 3820))) %>%
  select(occ10, polhitok, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

glm_fits_polstrike <- list()
for(i in 1:length(these)) { 
  glm_fits_polstrike[[i]] <- 
    coef(summary(
      svyglm(
        as.formula(
          paste0("as.numeric(polhitok == 1) ~ as.numeric(occ10 ==", these[i],")")
        ),
        design = gss_svy_polstrike
      )
    ))[2,]
}

gss_svy_polstrike <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, polhitok, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

police_fit_polstrike <- coef(summary(
  svyglm(
    as.numeric(polhitok == 1) ~ police, 
    design = gss_svy_polstrike
  )
))[2,]

## Combine estimates 
est_df <-
  bind_rows(
    bind_rows(glm_fits_discrim, police_fit_discrim) %>%
      mutate(outcome = "racdif_discrim"),
    bind_rows(glm_fits_blkassist, police_fit_blkassist) %>%
      mutate(outcome = "blkassist_spend"),
    bind_rows(glm_fits_polspend, police_fit_polspend) %>%
      mutate(outcome = "police_spend"),
    bind_rows(glm_fits_polstrike, police_fit_polstrike) %>%
      mutate(outcome = "police_strike")
  ) %>%
  mutate(
    occ10 = rep(c(these, 3850), 4),
    sig_diff = ifelse(`Pr(>|t|)` < 0.05, "Significant", "Not significant"),
    conf_lwr = Estimate - 1.96 * `Std. Error`,
    conf_upr = Estimate + 1.96 * `Std. Error`,
    racist = ifelse(Estimate < 0, "Yes", "No")
  ) %>%
  left_join(occ_codes) %>%
  mutate(
    title = ifelse(title == "Police and sheriff's patrol officers",
                   "Police", title),
    title = ifelse(is.na(title), "Refused", title),
    police = ifelse(title == "Police", "Police", "Non-police"),
  ) 

## Export all the estimates 
#write_rds(est_df, "occupation_estimates.Rds")


### ---- Generate Figures 3, 4, 7, 8   ------
est_df <- read_rds("occupation_estimates.Rds")

## How many significant diffs in same direction as police?
est_df %>% 
  filter(outcome %in% c("blkassist_spend", "police_strike"),
         sig_diff == "Significant") %>% 
  group_by(outcome) %>% 
  summarise(sig_diffs = sum(Estimate > 0))

est_df %>% 
  filter(!(outcome %in% c("blkassist_spend", "police_strike")),
         sig_diff == "Significant") %>% 
  group_by(outcome) %>% 
  summarise(sig_diffs = sum(Estimate < 0))


## How extreme are the diffs for police relative to the others? 
est_df %>% 
  group_by(outcome) %>% 
  summarise(as.extreme = mean(abs(Estimate[police != "Police"]) >= abs(Estimate[police == "Police"])))

gg_df <- 
  est_df %>% 
  filter(outcome == "blkassist_spend" & Estimate > 0 | 
           outcome == "police_spend" & Estimate < 0 |
           outcome == "police_strike" & Estimate > 0 |
           outcome == "racdif_discrim" & Estimate < 0)

g_police_spend <- 
  ggplot(gg_df %>% 
           filter(sig_diff == "Significant",
                  outcome == "police_spend"), 
         aes(x = Estimate, 
             y = reorder(title, abs(Estimate), na.rm = TRUE),
             color = police)) + 
  geom_vline(xintercept = 0, lty = 1) + 
  geom_errorbarh(
    aes(xmin = conf_lwr, xmax = conf_upr),
    height = 0, size = 1
  ) +
  geom_point(pch = 15, size = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#E69F00", "#0072B2"),
                     guide = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#E69F00", "#0072B2"),
                    guide = guide_legend(title = NULL)) +
  theme_fivethirtyeight() + 
  labs(
    y = "",
    x = "Average difference between occupational sub-group\nand other GSS respondents"
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10))

g_police_strike <- 
  ggplot(gg_df %>% 
           filter(sig_diff == "Significant",
                  outcome == "police_strike"), 
         aes(x = Estimate, 
             y = reorder(title, abs(Estimate), na.rm = TRUE),
             color = police)) + 
  geom_vline(xintercept = 0, lty = 1) + 
  geom_errorbarh(
    aes(xmin = conf_lwr, xmax = conf_upr),
    height = 0, size = 1
  ) +
  geom_point(pch = 15, size = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#E69F00", "#0072B2"),
                     guide = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#E69F00", "#0072B2"),
                    guide = guide_legend(title = NULL)) +
  theme_fivethirtyeight() + 
  labs(
    y = "",
    x = "Average difference between occupational sub-group\nand other GSS respondents"
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10))

g_blkassist_spend <- 
  ggplot(gg_df %>% 
           filter(sig_diff == "Significant",
                  outcome == "blkassist_spend"), 
         aes(x = Estimate, 
             y = reorder(title, abs(Estimate), na.rm = TRUE),
             color = police)) + 
  geom_vline(xintercept = 0, lty = 1) + 
  geom_errorbarh(
    aes(xmin = conf_lwr, xmax = conf_upr),
    height = 0, size = 1
  ) +
  geom_point(pch = 15, size = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#E69F00", "#0072B2"),
                     guide = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#E69F00", "#0072B2"),
                    guide = guide_legend(title = NULL)) +
  theme_fivethirtyeight() + 
  labs(
    y = "",
    x = "Average difference between occupational sub-group\nand other GSS respondents"
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10))

g_racdif_discrim <- 
  ggplot(gg_df %>% 
           filter(sig_diff == "Significant",
                  outcome == "racdif_discrim"), 
         aes(x = Estimate, 
             y = reorder(title, abs(Estimate), na.rm = TRUE),
             color = police)) + 
  geom_vline(xintercept = 0, lty = 1) + 
  geom_errorbarh(
    aes(xmin = conf_lwr, xmax = conf_upr),
    height = 0, size = 1
  ) +
  geom_point(pch = 15, size = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#E69F00", "#0072B2"),
                     guide = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#E69F00", "#0072B2"),
                    guide = guide_legend(title = NULL)) +
  theme_fivethirtyeight() + 
  labs(
    y = "",
    x = "Average difference between occupational sub-group\nand other GSS respondents"
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10))


## Export plots
fixed <- 1
spacing <- .285

g1 <-
  g_blkassist_spend + theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank())

g1 %>%
  ggsave(
    filename = "occupation_blkassist_spend.pdf",
    .,
    width = 8,
    height = fixed + spacing * (nrow(g1$data))
  )

g2 <-
  g_racdif_discrim + theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank())

g2 %>%
  ggsave(
    filename = "occupation_racdif_discrim.pdf",
    .,
    width = 8,
    height = fixed + spacing * (nrow(g2$data))
  )

g3 <-
  g_police_spend + theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank())

g3 %>%
  ggsave(
    filename = "occupation_police_spend.pdf",
    .,
    width = 8,
    height = fixed + spacing * (nrow(g3$data))
  )

g4 <-
  g_police_strike + theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank())

g4 %>%
  ggsave(
    filename = "occupation_police_strike.pdf",
    .,
    width = 8,
    height = fixed + spacing * (nrow(g4$data))*.8
  )


### ---- Analyses for Figures 5-6 and export -----

## How many obs do we have for cops on these Qs?  
gss_dat %>% 
  group_by(police) %>% 
  summarise(n_lazygap = sum(!is.na(bw_lazygap)),
            n_unintgap = sum(!is.na(bw_unintgap)),
            n_closegap = sum(!is.na(bw_close)))


### Blacks lazier than whites (positive estimates mean cop subgroup
## is more prejudiced than non-cop population)
gss_svy_lazy <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, bw_lazygap, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

lazy_fit <- 
  svyglm(
    bw_lazygap ~ police, design = gss_svy_lazy
  )

gg_est_lazy <- 
  tidy(lazy_fit ) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "bw_lazygap")

## Blacks less intelligent than whites (positive estimates mean cop subgroup
## is more prejudiced than non-cop population)
gss_svy_unint <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, bw_unintgap, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

unint_fit <- 
  svyglm(
    bw_unintgap ~ police, design = gss_svy_unint
  )

gg_est_unint <- 
  tidy(unint_fit) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "bw_unintgap")

## Interracial closeness (positive estimates mean subgroup feels closer to 
## blacks than whites)
gss_svy_close <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, bw_close, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

close_fit <- 
  svyglm(
    bw_close ~ police, design = gss_svy_close
  )

gg_est_close <- 
  tidy(close_fit) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "bw_close")


## Benchmark to diffs for Republicans
gss_svy_lazy <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(republican, bw_lazygap, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

lazy_fit_republican <- 
  svyglm(
    bw_lazygap ~ republican, design = gss_svy_lazy
  )

gg_est_lazy_republican <- 
  tidy(lazy_fit_republican) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "bw_lazygap")


gss_svy_unint <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(republican, bw_unintgap, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

unint_fit_republican <- 
  svyglm(
    bw_unintgap ~ republican, design = gss_svy_unint
  )

gg_est_unint_republican <- 
  tidy(unint_fit_republican) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "bw_unintgap")


gss_svy_close <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(republican, bw_close, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

close_fit_republican <- 
  svyglm(
    bw_close ~ republican, design = gss_svy_close
  )

gg_est_close_republican <- 
  tidy(close_fit_republican) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "bw_close")

## Benchmark to diffs for whites v. non-whites
gss_svy_lazy <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(dem_race, bw_lazygap, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

lazy_fit_whites <- 
  svyglm(
    bw_lazygap ~ as.numeric(dem_race == "White"), design = gss_svy_lazy
  )

gg_est_lazy_whites <- 
  tidy(lazy_fit_whites) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "bw_lazygap")

gss_svy_unint <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(dem_race, bw_unintgap, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

unint_fit_whites <- 
  svyglm(
    bw_unintgap ~ as.numeric(dem_race == "White"), design = gss_svy_unint
  )

gg_est_unint_whites <- 
  tidy(unint_fit_whites) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "bw_unintgap")

gss_svy_close <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(dem_race, bw_close, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

close_fit_whites <- 
  svyglm(
    bw_close ~ as.numeric(dem_race == "White"), design = gss_svy_close
  )

gg_est_close_whites <- 
  tidy(close_fit_whites) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "bw_close")


## Combine results & export plots
gg_intergroup_df <- 
  bind_rows(gg_est_close, gg_est_lazy, gg_est_unint, gg_est_close_republican, 
            gg_est_lazy_republican, gg_est_unint_republican, gg_est_close_whites,
            gg_est_lazy_whites, gg_est_unint_whites) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(conf_lwr = estimate - 1.96*std.error,
         conf_upr = estimate + 1.96*std.error,
         term = factor(term, levels = rev(c("policeYes",
                                            "republicanRepublican",
                                            "as.numeric(dem_race == \"White\")")),
                       labels = rev(c("Police (reference: Non-police)",
                                      "Republicans (reference: Democrats)",
                                      "Whites (reference: non-Whites)"))),
         outcome = factor(outcome, levels = c("bw_close",
                                              "bw_lazygap",
                                              "bw_unintgap"),
                          labels = c("Interracial closeness: Feel closer to Blacks than Whites",
                                     "Explicit prejudice: Blacks lazier than Whites",
                                     "Explicit prejudice: Blacks less intelligent than Whites")),
         police = ifelse(str_detect(term, "Police"), "Police", "Non-Police")) 

g1 <- 
  ggplot(gg_intergroup_df %>% filter(str_detect(outcome, "prejudice")), 
         aes(y = term,
             x = estimate,
             color = police)) +
  geom_vline(xintercept = 0, lty = 1) + 
  geom_errorbarh(
    aes(xmin = conf_lwr, xmax = conf_upr),
    height = 0,
    position = position_dodgev(height = 0.5),
    size = 1.5
  ) +
  geom_point(pch = 19, size = 2.5) + 
  facet_col( ~ outcome) +
  scale_x_continuous(limits = c(-0.5, 0.5)) + 
  theme_fivethirtyeight() +
  scale_color_manual(values = c("#E69F00", "#0072B2"),
                     guide = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#E69F00", "#0072B2"),
                    guide = guide_legend(title = NULL)) +
  xlab("Average difference between sub-group and reference group") + 
  theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10))

g2 <- 
  ggplot(gg_intergroup_df %>% filter(!str_detect(outcome, "prejudice")), 
         aes(y = term,
             x = estimate,
             color = police)) +
  geom_vline(xintercept = 0, lty = 1) + 
  geom_errorbarh(
    aes(xmin = conf_lwr, xmax = conf_upr),
    height = 0,
    position = position_dodgev(height = 0.5),
    size = 1.5
  ) +
  geom_point(pch = 19, size = 2.5) + 
  facet_col( ~ outcome) +
  scale_x_continuous(limits = c(-2.5, 2.5)) + 
  theme_fivethirtyeight() +
  scale_color_manual(values = c("#E69F00", "#0072B2"),
                     guide = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#E69F00", "#0072B2"),
                    guide = guide_legend(title = NULL)) +
  xlab("Average difference between sub-group and reference group") + 
  theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10))


g1 <- g1 + theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) 

g1 %>%
  ggsave(
    filename = "explicit_prejudice.pdf",
    .,
    width = 7,
    height = fixed + spacing * (nrow(g1$data))
  )

g2 <- g2 + theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) 

g2 %>%
  ggsave(
    filename = "interracial_closeness.pdf",
    .,
    width = 7,
    height = fixed + spacing * (nrow(g2$data))
  )




### ---- Analyses for Figure 9 and export ------

## Police 
table(gss_dat$police[gss_dat$year >= 1984 & !is.na(gss_dat$natcrimy)])

## Teacher
table(gss_dat$teacher[gss_dat$year >= 1984 & !is.na(gss_dat$nateducy)])

## Scientist 
table(gss_dat$scientist[gss_dat$year >= 1984 & !is.na(gss_dat$natsci)])

## Military
table(gss_dat$military[gss_dat$year >= 1984 & !is.na(gss_dat$natarmsy)])

## Police like spending on law enforcement 
gss_svy_police <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, natcrimy, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

police_fit <- 
  svyglm(
    as.numeric(natcrimy == 3) ~ police, design = gss_svy_police
  )

gg_est_police <- 
  tidy(police_fit) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "police_spend")

## Teachers like spending on education 
gss_svy_teacher <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(teacher, nateducy, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

teacher_fit <- 
  svyglm(
    as.numeric(nateducy == 3) ~ teacher, design = gss_svy_teacher
  )

gg_est_teacher <- 
  tidy(teacher_fit) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "education_spend")

## Scientists like spending on science 
gss_svy_scientist <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(scientist, natsci, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

scientist_fit <- 
  svyglm(
    as.numeric(natsci == 3) ~ scientist, design = gss_svy_scientist
  )

gg_est_scientist <- 
  tidy(scientist_fit) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "science_spend")

## People in the military like spending on national defense
gss_svy_military <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(military, natarmsy, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

military_fit <- 
  svyglm(
    as.numeric(natarmsy == 3) ~ military, design = gss_svy_military
  )

gg_est_military <- 
  tidy(military_fit) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(outcome = "military_spend")


gg_spending_df <- 
  bind_rows(gg_est_police, gg_est_teacher, gg_est_scientist, gg_est_military) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = factor(term, levels = c("policeYes", "teacherYes", "scientistYes",
                                        "militaryYes"),
                       labels = c("Police", "Teachers", "Scientists", "Military")),
         outcome = factor(outcome, levels = c("education_spend", "police_spend",
                                              "science_spend", "military_spend"),
                          labels = c("Too Much Spending on Education",
                                     "Too Much Spending on Law Enforcement",
                                     "Too Much Spending on Scientific Research",
                                     "Too Much Spending on National Defense")),
         conf_lwr = estimate - 1.96*std.error,
         conf_upr = estimate + 1.96*std.error,
         police = ifelse(str_detect(term, "Police"), "Police", "Non-Police"))

g <- 
  gg_spending_df %>% 
  ggplot(., aes(x = estimate, y = term, color = police)) + 
  geom_vline(xintercept = 0, lty = 1) + 
  geom_errorbarh(
    aes(xmin = conf_lwr, xmax = conf_upr),
    height = 0,
    position = position_dodgev(height = 0.5),
    size = 1.5
  ) +
  geom_point(pch = 19, size = 2.5) + 
  facet_col( ~ outcome, scales = "free_y") +
  scale_x_continuous(limits = c(-0.25, 0.25)) + 
  scale_color_manual(values = c("#E69F00", "#0072B2"),
                     guide = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#E69F00", "#0072B2"),
                    guide = guide_legend(title = NULL)) +
  theme_fivethirtyeight() +
  xlab("Average difference between sub-group and all other GSS respondents") + 
  theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10))

g <- g + theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) 

g %>%
  ggsave(
    filename = "occupation_spendings.pdf",
    .,
    width = 7,
    height = 2 + spacing * (nrow(g$data))
  )

### ---- Analyses for Table 4 and export  ----------

## Note that this doesn't export a clean latex table of results. Most of the 
## LEMAS estimates are pulled directly from the 2016 report. 

## Estimates for GSS sample. Ignore nonresponse on RPH's DVs and just compute
## proportion for the entire 277 cops. Assume missing responses are MCAR
gss_design <- 
  gss_dat %>% 
  filter(year >= 1984, police == "Yes") %>% 
  mutate(dem_sex = ifelse(dem_female == 1, "Female", "Male"),
         race_sex = paste(dem_race, dem_sex)) %>%
  select(race_sex, dem_race, dem_size_group, dem_female, dem_race, dem_union, 
         dem_emp, dem_region, vpsu, vstrat, wtssall, dem_age_cat, dem_edu_cat,
         dem_age) %>% 
  as_survey_design(
    id = vpsu,
    strata = vstrat,
    weights = wtssall,
    nest = TRUE
  )

## Region proportions
region_props <- 
  gss_design %>% 
  group_by(dem_region) %>% 
  summarise(prop_w = survey_mean(),
            prop_uw = n() / nrow(.)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(variable = "region") %>% 
  rename(level = dem_region)

## Sex props
sex_props <- 
  gss_design %>% 
  group_by(dem_female) %>% 
  summarise(prop_w = survey_mean(),
            prop_uw = n() / nrow(.)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(variable = "sex",
         dem_female = ifelse(dem_female == 1, "Female", "Male")) %>% 
  rename(level = dem_female)

## Employment props
employ_props <- 
  gss_design %>% 
  group_by(dem_emp) %>% 
  summarise(prop_w = survey_mean(),
            prop_uw = n() / nrow(.)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(variable = "employment") %>% 
  rename(level = dem_emp)

## Race proportions
race_props <- 
  gss_design %>% 
  group_by(dem_race) %>% 
  summarise(prop_w = survey_mean(),
            prop_uw = n() / nrow(.)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(variable = "race") %>% 
  rename(level = dem_race)

## Size of area
size_props <- 
  gss_design %>% 
  group_by(dem_size_group) %>% 
  summarise(prop_w = survey_mean(),
            prop_uw = n() / nrow(.)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(variable = "areasize") %>% 
  rename(level = dem_size_group)

## Age props
age_props <- 
  gss_design %>% 
  group_by(dem_age_cat) %>% 
  summarise(prop_w = survey_mean(),
            prop_uw = n() / nrow(.)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(variable = "age") %>% 
  rename(level = dem_age_cat)

## Education props
edu_props <- 
  gss_design %>% 
  group_by(dem_edu_cat) %>% 
  summarise(prop_w = survey_mean(),
            prop_uw = n() / nrow(.)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(variable = "education") %>% 
  rename(level = dem_edu_cat)

## Union props
union_props <- 
  gss_design %>% 
  group_by(dem_union) %>% 
  summarise(prop_w = survey_mean(),
            prop_uw = n() / nrow(.)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(variable = "union",
         dem_union = ifelse(dem_union == 0, "Non-union", "Union")) %>% 
  rename(level = dem_union)


## Joint dist of race/female
race_sex_props <- 
  gss_design %>% 
  group_by(race_sex) %>% 
  summarise(prop_w = survey_mean(),
            prop_uw = n()/nrow(.)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(variable = "race_sex") %>% 
  rename(level = race_sex)

sample_props <- 
  bind_rows(region_props, race_props, size_props, employ_props, union_props, 
            edu_props, age_props, sex_props, race_sex_props) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(prop_w = sprintf("%1.2f", prop_w),
         prop_uw = sprintf("%1.2f", prop_uw)) %>% 
  arrange(variable) 

sample_props %>% 
  write_csv(., file = "police_descriptives.csv")

##--- comparison to LEMAS estimates 
state_dict <- 
  data.frame(state = state.abb, region = state.region) %>% 
  mutate(region = case_when(
    region == "North Central" ~ "Midwest",
    region == "South" ~ "South",
    region == "Northeast" ~ "Northeast",
    region == "West" ~ "West"))

lemas_dat <- 
  read_dta("lemas16.dta") %>% 
  set_names(tolower) %>% 
  left_join(state_dict) %>% 
  mutate(union = case_when(pers_colbar_swn == 3 ~ "No", 
                           is.na(pers_colbar_swn) ~ "UNK",
                           TRUE ~ "Yes"))

lemas_svy <- 
  svydesign(id = ~ lear_id, data = lemas_dat, weight = ~finalwgt, 
            strata = ~strata)

## Total officers
svytotal(~ftsworn, lemas_svy)

## Region proportions: 
svytable(ftsworn ~ region, lemas_svy)/701274

## Unionization: 
svytable(ftsworn ~ union, lemas_svy)/701274

### ---- Analyses for Table 5 and export -----

## Attempt to replicate reported sample sizes
vars <- c("police_spend", "police_strike", "blkassist_spend", "racdif_discrim")

desc_dat <- 
  gss_dat %>% 
  filter(year >= 1984, police == "Yes") %>% 
  select(vars, ballot, form, version) %>% 
  pivot_longer(cols = police_spend:racdif_discrim,
               names_to = "outcome", 
               values_to = "response") %>% 
  mutate(response = ifelse(is.na(response), "NAP", response),
         response = factor(response, levels = c("Too little",
                                                "About right",
                                                "Too much",
                                                "Yes", 
                                                "No",
                                                "DK/NR",
                                                "NAP")),
         ballot = ifelse(is.na(ballot), 5, ballot),
         ballot = factor(ballot, levels = c(1:3, 5), labels = c("A", "B", "C", "UNK")),
         form = factor(form, levels = 1:3, labels = c("X", "Y", "Z"))) 

## Spending items
desc_dat %>% 
  filter(outcome %in% c("blkassist_spend", "police_spend")) %>% 
  count(response, outcome) %>% 
  #  group_by(outcome) %>% 
  #  mutate(prop = n/sum(n)) %>% 
  pivot_wider(names_from = c("response"),
              values_from = c("n")) %>% 
  select(response, police_spend, blkassist_spend)

## Others
desc_dat %>% 
  filter(!(outcome %in% c("blkassist_spend", "police_spend"))) %>% 
  count(response, outcome) %>% 
  #  group_by(outcome) %>% 
  #  mutate(prop = n/sum(n)) %>% 
  pivot_wider(names_from = c("response"),
              values_from = c("n")) %>% 
  select(response, police_strike, racdif_discrim)

## NB: bc of split ballot Qs different police are answering each Q, so that
## each Q applies to a different little sample of police. 
## See: http://gss.norc.org/Lists/gssFAQs/DispForm.aspx?ID=9
tmp <- 
  gss_dat %>%
  filter(police == "Yes", year >= 1984) %>% 
  select(vars, year, ballot, version, form) %>% 
  rowwise() %>% 
  mutate(n = sum(!is.na(c_across(police_spend:racdif_discrim))))

## Where are the cops that answered all 4 outcome questions? 
table(tmp$year[tmp$n == 4])
sum(table(tmp$year[tmp$n == 4]))

## 16 / 277 cops didn't get any of these
sum(table(tmp$year[tmp$n == 0]))
table(tmp$year[tmp$n == 0])

## How many cops *could have answered* 1 or more of these questions, by year
sum(table(tmp$year[tmp$n >= 1]))
table(tmp$year[tmp$n >= 1])

## Ballots a,b,c, or d (2006 only) denote the rotation 
## Form denotes the experimental form. A split ballot was used in the 1973, 
## 1974, 1976, 1978, 1980, and 1982+. The variant or "Y" wordings appear on 
## Form 2; the "Z" wordings appear on Form 3; and the standard GSS wordings 
## appear on Form 1. See Appendix P for what variables were on what form.
## Version: since 1994 the GSS employs a biennial dual sample design w/ 
## two versions. This is why observations increased since 1994. 
table(gss_dat$ballot[!is.na(gss_dat$police_spend)])
table(gss_dat$form[!is.na(gss_dat$police_spend)])

table(gss_dat$ballot[!is.na(gss_dat$polhitok)])
table(gss_dat$form[!is.na(gss_dat$polhitok)])

table(gss_dat$ballot[!is.na(gss_dat$natcrimy)])
table(gss_dat$form[!is.na(gss_dat$natcrimy)])

table(gss_dat$ballot[!is.na(gss_dat$natracey)])
table(gss_dat$form[!is.na(gss_dat$natracey)])

### ---- Analyses for Figure 10 and export ----------

blues <- RColorBrewer::brewer.pal(5, "Blues")
gg_df <- 
  gss_dat %>%
  filter(police == "Yes", year >= 1984) %>% 
  select(vars, year) %>% 
  mutate_if(is.character, list(~replace_na(., "NAP"))) %>% 
  pivot_longer(cols = police_spend:racdif_discrim) %>% 
  group_by(year, name) %>% 
  summarise(n_asked = sum(!(value %in% c("NAP"))),
            n_asked = na_if(n_asked, 0),
            n_answered = sum(!(value %in% c("NAP", "DK/NR"))),
            n_answered = na_if(n_answered, 0)) %>% 
  mutate(cuts = case_when(n_answered  <= 4 ~ "1-4",
                          n_answered  %in% c(5:8) ~ "5-8",
                          n_answered  %in% c(9:12) ~ "9-12",
                          n_answered  > 12 ~ "13-16"),
         cuts = factor(cuts, levels = c("1-4", "5-8", "9-12", "13-16")),
         name = factor(name, levels = c("police_spend",
                                        "blkassist_spend",
                                        "police_strike",
                                        "racdif_discrim"),
                       labels = c("Too Much Spending on Law Enforcement (natcrimy)",
                                  "Too Much Spending on Assistance to Blacks (natracey)",
                                  "Can Imagine Approving of Policeman Striking Adult Male (polhitok)",
                                  "Black-White Inequality Mainly Due to Discrimination (racdif1)")),
         n = paste0(n_answered, "/", n_asked),
         n = ifelse(is.na(n_asked), NA, n)) 


g <-
  ggplot(gg_df, aes(
    x = factor(year),
    y = name,
    fill = cuts,
    label = n
  )) +
  geom_tile(color = "black", size = 0.5) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual("Observations", na.value = "grey50",
                    values  = blues[1:4]) +
  theme_fivethirtyeight() +
  geom_text(
    size = 2.5,
    na.rm = TRUE,
    parse = TRUE
  ) +
  xlab("") + ylab("") +
  facet_col( ~ name, scales = "free_y") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 10),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.key.height = unit(0, "cm"),
    legend.title = element_text(size = 10),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    strip.background = element_blank(),
    legend.margin = margin(
      t = -0.75,
      r = 0,
      b = 0.5,
      l = 0,
      unit = "lines"
    ),
    plot.margin = unit(c(0, 0.2, 0, 0.2), "lines")
  )

g %>%
  ggsave(
    filename = "series_n_plot.pdf",
    .,
    width = 8,
    height = 5
  )
