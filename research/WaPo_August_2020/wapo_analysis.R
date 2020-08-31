rm(list = ls())
library(tidyverse)
library(estimatr)
library(ggthemes)
library(scales)
library(ggrepel)
library(grid)
library(RCurl)

## Note: June 15 survey is between-subjects survey experiment, July 13 survey
## is within subjects survey experiment plus additional Qs on policy attitudes
## & polce spending. Treatment variable is Z_protest_frame in June 15 survey &
## Z_defund_order in July 13 survey

## Questions re: protest support [Strongly oppose (1) … Strongly support (7)]: 
## Do you support or oppose protests to defund the police?
## Do you support or oppose protests by Black Lives Matter? 
## Do you support or oppose protests against police brutality? 

## Questions re: gov. spending (July 13 only). Excepting "police services", 
##  question wording & labels all from GSS. All presented in random order 
##  before "police services".  
##  Are we spending too much, too little, or about the right amount on ...
##  [Too little, About right, Too much, Don’t know]

## Questions re: the role of police and other social services (July 13 only). 
##   For the following list of activities, please say whether you think each 
##   should be performed by police only, by social services only, or by both 
##   police and social services. [Police only; Social services only; Both police 
##   and social services]. Activities presented in random order: 
##  - Responding to and investigating violent crimes (e.g. homicide, domestic 
##    violence)
##  - Responding to and investigating property crimes (e.g. burglaries, car 
##    thefts)
##  - Responding to and investigating vice crimes (e.g. prostitution, drug use)
##  - Conducting motor vehicle stops (e.g. traffic violations, sobriety 
##    checkpoints)
##  - Maintaining civil order (e.g. regulating the use of public spaces, 
##    dealing with loitering and panhandling)
##  - Crime prevention (e.g. patrolling high crime areas, monitoring 
##    surveillance cameras)
##  - Social services (e.g. assisting with dependent persons such as juvenile 
##    runaways and people with mental illnesses)
##  - Community engagement (e.g. meeting with residents about local issues, 
##    coordinating youth outreach programs)

##---- Read in raw data from Kyle's Github ------
june_15 <-
  read_csv(getURL(
    "http://kyle-peyton.com/research/WaPo_August_2020/june_15_2020.csv"
  ))

july_13 <-
  read_csv(getURL(
    "http://kyle-peyton.com/research/WaPo_August_2020/july_13_2020.csv"
  ))

##------ Fig. 1 from article ------

## Reshape data from July 13 survey for combining w/ June 15 survey
july_13_long <-
  july_13 %>%
  select(
    protest_support_blm_n,
    protest_support_dfnd_n,
    protest_support_brut_n,
    Z_defund_order,
    race,
    party_id_3,
    survey
  ) %>%
  pivot_longer(
    cols = protest_support_blm_n:protest_support_brut_n,
    names_to = "Z_protest_frame",
    values_to = "protest_support_n"
  ) %>%
  mutate(
    Z_protest_frame = factor(
      Z_protest_frame,
      levels = c(
        "protest_support_brut_n",
        "protest_support_blm_n",
        "protest_support_dfnd_n"
      ),
      labels = c("Police Brutality",
                 "Black Lives Matter",
                 "Defund the Police")
    ),
    protest_support_binary = as.numeric(protest_support_n > 4)
  )

## Stack June 15 data w/ July 13 for plotting. 
long_dat <-
  june_15 %>%
  select(
    protest_support_n,
    protest_support_binary,
    Z_protest_frame,
    race,
    party_id_3,
    survey
  ) %>%
  bind_rows(., july_13_long) %>%
  mutate(
    survey = factor(
      survey,
      levels = c("YCLS June 15", "YCLS July 13"),
      labels = c("June 15", "July 13")
    ),
    Z_protest_frame = factor(
      Z_protest_frame,
      levels = c(
        "Police Brutality",
        "Black Lives Matter",
        "Defund the Police"
      ),
      labels = c(
        "against police brutality",
        "by Black Lives Matter",
        "to defund the police"
      )
    )) 

## NB: check for Q ordering effects in July 13 survey (Z_protest_frame is not
## randomly assigned here)
long_dat %>%
  filter(survey == "July 13",
         !is.na(Z_protest_frame),
         !is.na(Z_defund_order)) %>%
  group_by(Z_protest_frame)  %>%
  do(tidy(lm_robust(
    protest_support_binary ~ Z_defund_order, data = .
  ))) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = "Defund last (v. first)",
    Z_protest_frame = factor(
      Z_protest_frame,
      levels = c(
        "against police brutality",
        "by Black Lives Matter",
        "to defund the police"
      ),
      labels = c("Police Brutality",
                 "Black Lives Matter",
                 "Defund the Police")
    )
  ) %>%
  select(Z_protest_frame,
         term,
         estimate,
         std.error,
         p.value,
         conf.low,
         conf.high)


## Estimate avg differences for each survey and plot results 
gg_df <- 
  long_dat %>%
  group_by(Z_protest_frame, survey) %>% 
  do(tidy(lm_robust(
    protest_support_binary ~ 1, data = .
  )))
  
avg_diffs_plot <-
  ggplot(
    gg_df,
    mapping = aes(
      x = estimate,
      y = reorder(Z_protest_frame, estimate),
      colour = survey,
      group = survey,
      label = scales::percent(estimate, accuracy = 2L,
                              suffix = "")
    )
  ) +
  geom_linerange(
    aes(xmin = 0.2, xmax = estimate, y = Z_protest_frame),
    position = coefplot::position_dodgev(0.7),
    show.legend = FALSE
  ) +
  geom_point(size = 10, position = coefplot::position_dodgev(0.7)) +
  geom_text(
    aes(
      x = estimate,
      label = scales::percent(estimate, accuracy = 2L,
                              suffix = "")
    ),
    color = "white",
    size = 3,
    position = coefplot::position_dodgev(0.7)
  ) +
  scale_x_continuous(
    limits = c(0.20, 0.70),
    breaks = c(0.25, 0.5, 0.75),
    labels = percent
  ) +
  scale_color_manual("", values = c("darkgrey", "black")) +
  scale_shape_manual("", values = c(16, 16)) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(
    legend.key.size = unit(2, "line"),
    legend.position = c(0.35,-0.1),
    legend.text = element_text(size = 10, family = "sans"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.2),
    axis.line = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_text(size = 12, family = "sans"),
    plot.caption = element_text(hjust = 0, vjust = -5),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))

## Add labels for WaPo
avg_diffs_plot <- 
  avg_diffs_plot + 
  labs(
    title = "Most support the protests - unless they're\nassociated with police defunding",
    subtitle = "Do you support or oppose protests ... (%)?",
    caption = "Note: Respondents were randomly assigned to answer one question in June 15 survey; and all \nthree questions were answered, in randomized order, in July 13 survey.\nSource: Yale Cooperative Lucid Surveys, June 15 (N = 1,057); July 13 2020 (N = 1,087).\nSample: Nationally representative of U.S. adults on age, gender, race/ethnicity, and region.")

avg_diffs_plot

ggsave(filename = "fig1.png",
       avg_diffs_plot,
       height = 5,
       width = 7)

##----- Partisan differences (cut from article) ------
bpr_colors <- c("#1F3A93", "#7C2C55", "#D91E18")

gg_df <- 
  long_dat %>%
  group_by(Z_protest_frame, party_id_3) %>%
  do(tidy(lm_robust(
    protest_support_binary ~ 1 + survey, data = .,
  ))) %>% 
  filter(term == "(Intercept)") 

party_diffs_plot <- 
  ggplot(
    gg_df,
    mapping = aes(
      x = estimate,
      y = Z_protest_frame,
      colour = party_id_3,
      group = party_id_3,
      label = scales::percent(estimate, accuracy = 2L,
                              suffix = "")
    )
  ) +
  geom_linerange(
    aes(xmin = 0.2, xmax = estimate, y = Z_protest_frame),
    position = coefplot::position_dodgev(0.7),
    show.legend = FALSE
  ) +
  geom_point(size = 10, position = coefplot::position_dodgev(0.7)) +
  geom_text(
    aes(
      x = estimate,
      label = scales::percent(estimate, accuracy = 2L,
                              suffix = "")
    ),
    color = "white",
    size = 3,
    position = coefplot::position_dodgev(0.7)
  ) +
  scale_x_continuous(
    labels = percent
  ) +
  scale_color_manual("", values = bpr_colors) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(
    legend.key.size = unit(2, "line"),
    legend.position = c(0.35,-0.1),
    legend.text = element_text(size = 10, family = "sans"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.2),
    axis.line = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_text(size = 12, family = "sans"),
    plot.caption = element_text(hjust = 0, vjust = -5),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))

party_diffs_plot <- 
  party_diffs_plot + 
  labs(
    title = "Support for protests is strongest among Democrats, \nregardless of frame",
    subtitle = "Do you support or oppose protests ... (%)?",
    caption = "Note: Respondents randomly assigned to answer one question in June 15 survey; and all \nthree questions were answered, in randomized order, in July 13 survey. Pooled estimates plotted.\nSource: Yale Cooperative Lucid Surveys, June 15 (N = 1,057); July 13 2020 (N = 1,087).\nSample: Nationally representative of U.S. adults on age, gender, race/ethnicity, and region.")

party_diffs_plot
ggsave(filename = "party_diffs_support.png",
       party_diffs_plot,
       height = 5,
       width = 7)

##----- Race/ethnicity differences (cut from article) ------
gg_df <- 
  long_dat %>%
  group_by(Z_protest_frame, race) %>%
  do(tidy(lm_robust(
    protest_support_binary ~ 1 + survey, data = .,
  ))) %>% 
  filter(term == "(Intercept)") 

race_diffs_plot <- 
  ggplot(
    gg_df,
    mapping = aes(
      x = estimate,
      y = Z_protest_frame,
      colour = race,
      group = race,
      label = scales::percent(estimate, accuracy = 2L,
                              suffix = "")
    )
  ) +
  geom_linerange(
    aes(xmin = 0.2, xmax = estimate, y = Z_protest_frame),
    position = coefplot::position_dodgev(0.7),
    show.legend = FALSE
  ) +
  geom_point(size = 10, position = coefplot::position_dodgev(0.7)) +
  geom_text(
    aes(
      x = estimate,
      label = scales::percent(estimate, accuracy = 2L,
                              suffix = "")
    ),
    color = "white",
    size = 3,
    position = coefplot::position_dodgev(0.7)
  ) +
  scale_x_continuous(
    labels = percent
  ) +
  scale_color_manual("", values = c("#1b9e77", "#d95f02", "#7570b3")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(
    legend.key.size = unit(2, "line"),
    legend.position = c(0.35,-0.1),
    legend.text = element_text(size = 10, family = "sans"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.2),
    axis.line = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_text(size = 12, family = "sans"),
    plot.caption = element_text(hjust = 0, vjust = -5),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))

race_diffs_plot <- 
  race_diffs_plot + 
  labs(
    title = "Black and White respondents are sharply divided in their \nsupport for protests, regardless of frame",
    subtitle = "Do you support or oppose protests ... (%)?",
    caption = "Note: Respondents randomly assigned to answer one question in June 15 survey; and all \nthree questions were answered, in randomized order, in July 13 survey. Pooled estimates plotted.\nSource: Yale Cooperative Lucid Surveys, June 15 (N = 1,057); July 13 2020 (N = 1,087).\nSample: Nationally representative of U.S. adults on age, gender, race/ethnicity, and region.")

race_diffs_plot

ggsave(filename = "race_diffs_support.png",
       race_diffs_plot,
       height = 5,
       width = 7)

## ----- Fig 2 from article  ------

## Reshape data on spending questions from July 13 survey for plotting
policy_long <- 
  july_13 %>% 
  select(starts_with("gss_") & -ends_with("_n"), 
         police_spend, race, party_id_3) %>% 
  pivot_longer(cols = gss_welfare:police_spend,
               names_to = "outcome", values_to = "group") %>% 
  mutate(outcome = factor(outcome, levels = c("gss_welfare",
                                              "police_spend", 
                                              "gss_science", 
                                              "gss_childcare", 
                                              "gss_atp",
                                              "gss_enviro",
                                              "gss_ss",        
                                              "gss_health",
                                              "gss_education"),
                          labels = c("Welfare",
                                     "Police",
                                     "Science",
                                     "Childcare",
                                     "Aid to poor",
                                     "Environment",
                                     "Social Security",
                                     "Healthcare",
                                     "Education"))) %>% 
  filter(!is.na(outcome), !is.na(group)) 

## Estimate avg. differences in group proportions across all policy areas 
gg_df <- 
  policy_long %>%
  mutate(group = factor(
    group,
    levels = c("Too much", "Too little",
               "About right", "Don't know")
  )) %>%
  group_by(outcome, group) %>%
  summarise(cases = n()) %>%
  mutate(pop = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(
    cases, pop, conf.level = 0.95
  )))) %>%
  tidyr::unnest(tst) 

## Estimate diffs on police spending by race/ethnicity groupings
police_spend_race <- 
  policy_long %>%
  filter(outcome == "Police") %>% 
  mutate(group = factor(
    group,
    levels = c("Too much", "Too little",
               "About right", "Don't know")
  )) %>% 
  group_by(race, group) %>%
  summarise(cases = n()) %>%
  mutate(pop = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(
    cases, pop, conf.level = 0.95
  )))) %>%
  tidyr::unnest(tst) 

## Proportion saying "too much" spending by race/ethnicity: 
police_spend_race %>% 
  filter(group == "Too much") %>% 
  select(race, cases, pop, estimate)

## Plot results for avg. diffs across all policy areas
spend_diffs_plot <- 
  gg_df %>% 
  mutate(bar_center = estimate / 2) %>%  # For position of labels
  filter(group != "Don't know") %>% # remove dont knows from plot
  ggplot(., mapping = aes(x = estimate, y = outcome, fill = group,
                          group = outcome)) +
  geom_bar(stat= 'identity', width = 0.8) + 
  geom_text_repel(aes(x = bar_center, y = outcome, 
                      label = scales::percent(estimate, accuracy = 2L,
                                              suffix = "")),
                  color = "whitesmoke", force = 0, direction = 'y', 
                  vjust = 0, nudge_y = -0.1) + 
  facet_wrap(~ group, nrow = 1) + 
  scale_x_continuous(labels = percent) +
  scale_fill_manual("", values = rev(c("#000000", "#E69F00", "#56B4E9"))) + 
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.text = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.caption.position = "plot"
  ) 

## Add labels for WaPo
spend_diffs_plot <- 
  spend_diffs_plot + 
  labs(title = "Across a variety of policy areas, including policing, few\nsay there is 'too much' government spending",
       subtitle = "Are we spending too much, too little, or about the right amount on ... (%)?",
       caption = "Note: Don't know responses not shown.\nSource: Yale Cooperative Lucid Surveys, July 13 2020 (N = 1,087).\nSample: Nationally representative of U.S. adults on age, gender, race/ethnicity, and region.")

spend_diffs_plot

ggsave(filename = "fig2.png",
       spend_diffs_plot,
       height = 5,
       width = 8)

## ----- Fig 3 from article  ------

## Reshape data on spending questions from July 13 survey for plotting
police_long <- 
  july_13 %>% 
  select(starts_with("police_matrix"), race, party_id_3) %>% 
  pivot_longer(cols = starts_with("police_matrix"),
               values_to = "group", names_to = "outcome") %>% 
  mutate(outcome = str_replace(outcome, "police_matrix_", ""),
         outcome = factor(outcome, levels = rev(c("property_crime",
                                                  "vehicle_stops",
                                                  "violent_crime", 
                                                  "prevent_crime",
                                                  "vice_crime",
                                                  "civil_order",
                                                  "community_engage",
                                                  "social_service")),
                          labels = rev(c("Respond to property crimes",
                                         "Conduct vehicle stops",
                                         "Respond to violent crimes",
                                         "Crime prevention",
                                         "Respond to vice crimes",
                                         "Maintain civil order",
                                         "Community engagement",
                                         "Provide social services"))),
         group = factor(group, levels = c("Police", "Social Services", "Both"),
                        labels = c("Police only", "Social services only", 
                                   "Both")))

## Estimate avg. differences in group proportions across all policy areas
gg_df <- 
  police_long %>% 
  filter(!is.na(group), !is.na(outcome)) %>% 
  group_by(outcome, group) %>% 
  summarise(cases = n()) %>% 
  mutate(pop = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(cases, pop, conf.level=0.95)))) %>%
  tidyr::unnest(tst) 

## Estimate diffs on police spending by race/ethnicity groupings
police_roles_race <- 
  police_long %>% 
  filter(!is.na(group), !is.na(outcome)) %>% 
  group_by(race, group) %>%
  summarise(cases = n()) %>%
  mutate(pop = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(
    cases, pop, conf.level = 0.95
  )))) %>%
  tidyr::unnest(tst) 

## Proportion saying "Police only" by race/ethnicity: 
police_roles_race %>% 
  filter(group == "Police only") %>% 
  select(race, cases, pop, estimate)


## Plot results for avg. diffs across all police roles 
role_diffs_plot <- 
  gg_df %>% 
  mutate(bar_center = estimate/2) %>%  # for centering labels
  ggplot(., mapping = aes(x = estimate, y = outcome, fill = group,
                              group = group)) +
  geom_bar(stat= 'identity', width = 0.8) + 
  geom_text_repel(aes(x = bar_center, y = outcome, 
                      label = scales::percent(estimate, accuracy = 2L,
                                              suffix = "")),
                  color = "white", force = 0, direction = 'y', 
                  vjust = 0, nudge_y = -0.1) + 
  facet_wrap(~ group, nrow = 1) + 
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  scale_fill_manual("", values = rev(c("#000000", "#E69F00", "#56B4E9"))) + 
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.text = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.caption.position = "plot"
  ) 
  
role_diffs_plot <- 
  role_diffs_plot + 
  labs(title = "Most believe the police should still play a role in the\nservices they commonly provide",
       subtitle = "For the following list of activities commonly performed by police departments, \nplease say whether you think each should be performed by ... (%)",
       caption = "Note: Lists of activities were presented to respondents in random order.\nSource: Yale Cooperative Lucid Surveys, July 13 2020 (N = 1,087).\nSample: Nationally representative of U.S. adults on age, gender, race/ethnicity, and region.")

role_diffs_plot
ggsave(filename = "fig3.png",
       role_diffs_plot,
       height = 5,
       width = 8)








