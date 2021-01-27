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


## Load GSS series downloaded from here: http://gss.norc.org/get-the-data/stata 
gss <-
  read_dta("GSS7218_R3.dta")

## Recode outcomes using authors' coding scheme, and generate indicator for 
## police officers and their supervisors 
## See p.50: http://gss.norc.org/Documents/reports/methodological-reports/MR125.pdf
gss_dat <-
  gss %>%
  mutate(
    police = ifelse(occ10 %in% c(3850, 3710), "Yes", "No"),
    racdif_discrim = ifelse(racdif1 == 1, "Yes", "No"),
    assist_toomuch = ifelse(natracey == 3, "Yes", "No")
  )

## NB: we need this options else the variance cannot be estimated. 
## see: https://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options(survey.lonely.psu = "adjust")
options(na.action = "na.pass")

## Now generate estimates for each outcome measure / group combination
gss_svy_discrim <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, racdif_discrim, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

gss_svy_assist <-
  gss_dat %>%
  filter(year >= 1984) %>%
  select(police, assist_toomuch, vpsu, wtssall, year, vstrat) %>%
  na.omit() %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(
    id = vpsu,
    strata = stratvar,
    weights = wtssall,
    nest = TRUE
  )

est_discrim <-
  gss_svy_discrim %>%
  group_by(year, police, racdif_discrim) %>%
  summarize(n = n(),
            prop = survey_prop(na.rm = TRUE, vartype = "ci"))

est_assist <-
  gss_svy_assist %>%
  group_by(year, police,  assist_toomuch) %>%
  summarize(n = n(),
            prop = survey_prop(na.rm = TRUE, vartype = "ci"))

## Combine for plotting:
gg_discrim <-
  est_discrim %>%
  rename(response = racdif_discrim) %>%
  mutate(outcome = "Black-White inequality is 'Mainly due to discrimination'")

gg_assist <-
  est_assist %>%
  rename(response = assist_toomuch) %>%
  mutate(outcome = "We are spending too much on 'Assistance to Blacks'")

est_out <- bind_rows(gg_discrim, gg_assist)


## Make the plots
g1 <-
  ggplot(
    subset(est_out, response == "Yes"),
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
  geom_hline(yintercept = 1, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.5, color = NA) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = scales::percent_format(accuracy = 1)) +
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
  facet_col( ~ outcome) +
  theme_fivethirtyeight() +
  labs(
    x = "Year",
    y = "Percent Agreeing",
    title = "Estimated proportion agreeing with statements from 1984 to 2018",
    caption = ""
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12))

g1

g2 <-
  est_out %>%
  group_by(police, year, outcome) %>%
  summarise(n = sum(n)) %>%
  filter(police == "Yes") %>%
  ggplot(.,
         aes(x = as.factor(year),
             y = n,
             label = n)) +
  geom_point(stat = 'identity', fill = "black", size = 6)  +
  geom_segment(aes(
    y = 0,
    x = as.factor(year),
    yend = n,
    xend = as.factor(year)
  ),
  color = "black") +
  geom_text(color = "white", size = 3) +
  facet_col( ~ outcome) +
  scale_y_continuous(limits = c(0, 16)) +
  theme_fivethirtyeight() +
  labs(
    x = "Year",
    y = "",
    title = "Number of cops responding to questions from 1984 to 2018",
    caption = "Data source: General Social Survey"
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12))
g2

g1 <- g1 + theme(plot.margin = grid::unit(c(1,-2, 1, 1), "mm"))
g1

g2 <- g2 + theme(plot.margin = grid::unit(c(-5, 1, 1, 1), "mm"))
g2

plot_grid(g1, g2, ncol = 1, rel_heights = c(1, 0.65))


## Sanity check
# gss_dat %>%
#   filter(police == "Yes", year == 1986) %>%
#   summarise(n_racedif1 = sum(!is.na(racdif1)),
#             n_natracey = sum(!is.na(natracey)))
