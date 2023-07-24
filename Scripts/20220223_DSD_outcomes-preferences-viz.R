## PROJECT: COP_UGA
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: DSD Analysis - outcomes/preferences analysis viz
## LICENSE: MIT
## DATE:    2022-02-23


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(glue)
library(janitor)
library(ggtext)
library(extrafont)
library(patchwork)
library(waffle)

#IMPORT ------------------------------------------------------------------

#Run tidying script and save to /Dataout folder

df_clean <- read_csv("Dataout/20220223-DSD-preference-clean.csv")


#PREFERENCE VIZ ----------------------------------------------------------

#  How many people were in preferred ARV Disp? ---------

df_preference <- df_clean %>% 
  count(is_current_mode_preferred) %>% 
  pivot_wider(names_from = is_current_mode_preferred, values_from = n) %>% 
  mutate(total = No + Yes,
         preferred = Yes / total,
         not_preferred = No/total) %>% 
  pivot_longer(cols = c(preferred, not_preferred), names_to = "preference") %>% 
  mutate(value = round(value * 100))


df_preference <- df_preference %>% 
  mutate(fill_color = ifelse(preference == "not_preferred", trolley_grey_light, golden_sand),
         val_lab = glue("Current ARV Method = Preferred ARV Method\n(n) - {(Yes)}\nTotal -{(total)}"))

df_preference %>%
  #filter(fundingagency == "USAID") %>% 
  ggplot(aes(fill = fill_color, values = value)) +
  geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE, make_proportional = TRUE, na.rm = TRUE) +
  geom_text(aes(x = 5, y  = 12, label = val_lab, color = golden_sand),
            family = "Source Sans Pro SemiBold", size = 14/.pt) +
  #  facet_wrap(~region, nrow = 1, strip.position = "bottom") +
  facet_wrap(~fct_reorder(val_lab, value,min), nrow = 1, strip.position = "bottom") +
  expand_limits(y = 14) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal() +
  labs(x= NULL, y = NULL,
       title = "75% of participants in the sample are currently in their preferred model of ARV dispensing") +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = .5),
        panel.spacing = unit(1, "pt")) +
  guides(fill = guide_legend(reverse = TRUE))

si_save("Graphics/dsd-preference-waffle.svg")

# How about by sex?
#  How many people were in preferred ARV Disp? ---------

df_preference <- df_clean %>% 
  count(is_current_mode_preferred, gender) %>% drop_na() %>% 
  pivot_wider(names_from = is_current_mode_preferred, values_from = n) %>% 
  mutate(total = No + Yes,
         preferred = Yes / total,
         not_preferred = No/total) %>% 
  pivot_longer(cols = c(preferred, not_preferred), names_to = "preference") %>% 
  mutate(value = round(value * 100))


df_preference <- df_preference %>% 
  mutate(sex_color = ifelse(gender == "Female", moody_blue, genoa),
         fill_color = ifelse(preference == "not_preferred", trolley_grey_light, sex_color),
         val_lab = glue("Current ARV Method = Preferred ARV Method\n(n) - {(Yes)}\nTotal -{(total)}"),
         num_lab = ifelse(preference == "preferred", glue("{value}%"), NA))



df_preference %>%
  #filter(fundingagency == "USAID") %>% 
  ggplot(aes(fill = fill_color, values = value)) +
  geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE, make_proportional = TRUE, na.rm = TRUE) +
  geom_text(aes(x = 5, y  = 12, label = val_lab, color = sex_color),
            family = "Source Sans Pro SemiBold", size = 14/.pt) +
  geom_text(aes(x = 5, y  = 11, label = num_lab, color = sex_color),
            family = "Source Sans Pro SemiBold", size = 20/.pt) +
  #  facet_wrap(~region, nrow = 1, strip.position = "bottom") +
  facet_wrap(~fct_reorder(gender, value,min), nrow = 1, strip.position = "bottom") +
  expand_limits(y = 14) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal() +
  labs(x= NULL, y = NULL,
       title = "Across both male and female participants, 75% of participants in the sample are currently in their preferred model of ARV dispensing") +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = .5),
        panel.spacing = unit(1, "pt")) +
  guides(fill = guide_legend(reverse = TRUE))

si_save("Graphics/dsd-preference-waffle-by-sex.svg")

# Of those currently on preferred, what is current ARV?

nudge_space <- 0.125 #set nudge for offset bars


df_dsd_curr <- df_clean %>% 
  filter(is_current_mode_preferred == "Yes") %>% 
  count(current_arv_mode) %>%drop_na() 

df_dsd_curr$total <- sum(df_dsd_curr$n) 

df_dsd_curr <- df_dsd_curr %>% 
  mutate(share = n/total) %>% 
  rename(dsd = current_arv_mode, current_n = n, total = total, current_share = share)

df_dsd_curr %>% 
  # filter(current_share != "Other") %>% 
  mutate(fill_color = ifelse(dsd %in% c("Facility-based individual management (FBIM)",
                                        "Facility-based group (FBG)",
                                        "Fast track refill (FTDR)"), scooter, genoa)) %>% 
  ggplot(aes(x = fct_reorder(dsd, current_n, .desc = TRUE))) +
  # geom_col(aes(y = current_n),fill = trolley_grey_light, alpha = 1, position = position_nudge(x = -nudge_space)) +
  geom_col(aes(y = current_n, fill = fill_color), position = "dodge") +
  # geom_label(aes(y = 0, label = percent(share, 1), fill = share, 
  #                color = ifelse(share > 0.3, "white", grey90k)), 
  #            vjust = 1.3, 
  #            size = 10/.pt, 
  #            label.size = NA, family = "Source Sans Pro") +
  geom_text(aes(y = current_n, label = percent(current_share, 1)), family = "Source Sans Pro",
            hjust = 1.5, vjust = -1, na.rm = TRUE) +
  # geom_text(aes(y = preferred_n, label = percent(preferred_share, 1)), family = "Source Sans Pro",
  #           hjust = 0, vjust = -1, na.rm = TRUE) +
  #scale_y_continuous(labels = percent) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL, title = "Of participants currently on their preferred ARV dispensing method, most preferred facility-based DSD models to community models, especially the fast track refill model",
       subtitle = "Uganda COP22 Patient Preference Analysis | USAID",
       caption = "Source: DSD Patient Reference Export, 16 Feb 2022") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

si_save("COP22_UGA_current-dsd.svg", path = "Graphics")

# What about those not on preferred DSD?

df_dsd <- df_clean %>% 
  filter(is_current_mode_preferred == "No") %>% 
  count(current_arv_mode) %>%drop_na()
#pivot_wider(names_from = current_arv_mode, values_from = n) %>%

df_dsd$total <- sum(df_dsd$n)

df_dsd <- df_dsd %>% 
  group_by(current_arv_mode) %>% 
  mutate(share = n/total) %>% 
  rename(dsd = current_arv_mode, current_n = n, total = total, current_share = share) 

df_dsd_join <- df_clean %>% 
  filter(is_current_mode_preferred == "No") %>% 
  count(preferred_DSD) %>%drop_na()

df_dsd_join$total <- sum(df_dsd_join$n)

df_dsd_join <- df_dsd_join %>% 
  mutate(preferred_share = n/total) %>% 
  rename(preferred_n = n, total = total, dsd  = preferred_DSD) %>% 
  left_join(df_dsd, by = c("dsd"))

df_dsd_join %>% 
  filter(current_share != "Other") %>% 
  mutate(fill_color = ifelse(dsd %in% c("Facility-based individual management (FBIM)",
                                        "Facility-based group (FBG)"), scooter, genoa)) %>% 
  ggplot(aes(x = fct_reorder(dsd, preferred_n, .desc = TRUE))) +
  geom_col(aes(y = current_share),fill = trolley_grey_light, alpha = 1, position = position_nudge(x = -nudge_space)) +
  geom_col(aes(y = preferred_share, fill = fill_color), position = position_nudge(x = nudge_space)) +
  # geom_label(aes(y = 0, label = percent(share, 1), fill = share, 
  #                color = ifelse(share > 0.3, "white", grey90k)), 
  #            vjust = 1.3, 
  #            size = 10/.pt, 
  #            label.size = NA, family = "Source Sans Pro") +
  geom_text(aes(y = current_share, label = percent(current_share, 1)), family = "Source Sans Pro",
            hjust = 1.5, vjust = -1, na.rm = TRUE) +
  geom_text(aes(y = preferred_share, label = percent(preferred_share, 1)), family = "Source Sans Pro",
            hjust = 0, vjust = -1, na.rm = TRUE) +
  scale_y_continuous(labels = percent, limits = c(0, .6)) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL, title = "Of participants who reported that current ARV dispensing method was not preferred, most preferred community-based DSD models to facility models",
       subtitle = "Uganda COP22 Patient Preference Analysis | USAID",
       caption = "Source: DSD Patient Reference Export, 16 Feb 2022") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

si_save("COP22_UGA_preferred-dsd.svg", path = "Graphics")


# -----------------

# #Alluvial Attempt
# 
# df_clean %>%
#   filter(is_current_mode_preferred == "No") %>% 
#   count(is_current_mode_preferred, current_arv_mode, preferred_DSD) %>%
#   # view()
#   ggplot(aes(y = n, axis2 = current_arv_mode, axis1 = preferred_DSD)) +
#   geom_alluvium(aes(fill = preferred_DSD), width = 0, reverse = F) + 
#   geom_stratum(width = 1/10, reverse = F,
#                fill = trolley_grey_light, 
#                color = "white") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
#   scale_x_continuous(breaks = 1:2, 
#                      labels = c("Current", "Preferred"),
#                      position = "top") +
#   coord_cartesian(expand = T, clip = "off") +
#   si_style() +
#   # coord_flip() + 
#   labs(y = NULL) +
#   theme(axis.text.y = element_blank(), 
#         legend.position = "none") +
#   scale_fill_manual(values = c(genoa_light, moody_blue_light, golden_sand_light)) +
#   theme_void()
# 
# 
# ggsave(file.path(images, "ZMB_alluvial_opu_target_shift.svg"),
#        width = 10, height = 5.625)



#  How do clinical outcomes differ by current method of ARV dispensation?

#IIT
df_DSD_iit <- df_clean %>% 
  count(current_arv_mode, missed_appt) %>% drop_na() %>% 
  pivot_wider(names_from = missed_appt, values_from = n) %>% 
  mutate(total = No + Yes,
         IIT = Yes / total,
         fill_color = ifelse(current_arv_mode %in% c("Facility-based individual management (FBIM)",
                                                     "Facility-based group (FBG)", "Fast track refill (FTDR)"), scooter, genoa))

df_DSD_iit_age_sex <- df_clean %>% 
  count(gender, agecoarse, current_arv_mode, missed_appt) %>% drop_na() %>% 
  pivot_wider(names_from = missed_appt, values_from = n) %>% 
  mutate(total = No + Yes,
         IIT = Yes / total,
         fill_color = ifelse(current_arv_mode %in% c("Facility-based individual management (FBIM)",
                                                     "Facility-based group (FBG)", "Fast track refill (FTDR)"), scooter_light, genoa_light))

v2_iit <- df_DSD_iit_age_sex %>% 
  ggplot(aes(agecoarse, fct_relevel(current_arv_mode, "Community client-led ART delivery (CCLAD)",
                                    "Community Drug Distribution Point (CDDP)",
                                    "Fast track refill (FTDR)",
                                    "Community Pharmacy",
                                    "Facility-based group (FBG)",
                                    "Facility-based individual management (FBIM)"))) +
  geom_tile(aes(fill = fill_color),color = "white", size = 1, alpha = .8) +
  geom_text(aes(label = percent(IIT, 1)), na.rm = TRUE,
            family = "Source Sans Pro", hjust = -.25, color = "#505050") + 
  facet_grid(~gender) +
  scale_x_discrete(position = "top") +
  scale_fill_identity() +
  labs(x = NULL, y = NULL) +
  si_style_nolines() +
  theme(strip.placement = "outside",
        strip.text.x = element_text(hjust = .5),
        panel.spacing.x = unit(0, "points"),
        plot.background = element_rect(fill = "white", color = NA))

fct_reorder(df_DSD_iit$current_arv_mode, IIT, min)


v1_iit <- df_DSD_iit %>% 
  ggplot(aes(IIT, fct_reorder(current_arv_mode, IIT, min), fill = fill_color)) +
  geom_col() +
  geom_vline(xintercept = seq(0, 1, by = .25), 
             color = "white", alpha = .6) +
  geom_text(aes(label = percent(IIT, 1)), na.rm = TRUE,
            family = "Source Sans Pro", hjust = -.25, color = "#505050") +
  labs(x = NULL, y = NULL,
       # title = glue("AS OF {month}, {shr} OF {ptnr} TOTAL REPORTED TREATMENT PATIENTS WERE ON <span style = 'color:{genoa};'>+3 MONTHS OF MMD</span>"),
       # caption = glue("Source: {hfr_source}")
  ) +
  scale_x_continuous(label = percent, expand = c(.005, .005), position = "top") +
  scale_fill_identity() +
  scale_alpha_identity() +
  si_style_nolines() +
  theme(legend.position = "none",
        # axis.text.x = element_blank(),
        plot.title = element_markdown(),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = NA))


v1_iit + v2_iit + plot_annotation(
  title = glue("Share of patients of patients with missed appointments by current ARV dispensing method"),
  subtitle = glue("Uganda COP22 Patient Preference Analysis | USAID"),
  caption = "Source: DSD Patient Reference Export, 16 Feb 2022"
)  & si_style_nolines() & theme(
  # axis.text.x = element_blank(),
  #legend.position = "none",
  strip.placement = "outside",
  strip.text.x = element_text(hjust = .5),
  panel.spacing.x = unit(.01, "points"),
  plot.title = element_markdown(),
  plot.title.position = "plot",
  plot.background = element_rect(fill = "white", color = NA)
)

si_save("cop-22-iit-by-current-mode.svg", path = "Graphics")

#VLS --------------

df_DSD_vls <-  df_clean %>% 
  count(current_arv_mode, vls) %>% drop_na() %>% 
  pivot_wider(names_from = vls, values_from = n) %>% 
  mutate(total = No + Yes,
         VLS = Yes / total,
         fill_color = ifelse(current_arv_mode %in% c("Facility-based individual management (FBIM)",
                                                     "Facility-based group (FBG)",  "Fast track refill (FTDR)"), scooter, genoa))

df_DSD_vls_age_sex <- df_clean %>% 
  count(gender, agecoarse, current_arv_mode, vls) %>% drop_na() %>% 
  pivot_wider(names_from = vls, values_from = n) %>% 
  mutate(total = No + Yes,
         VLS = Yes / total,
         fill_color = ifelse(current_arv_mode %in% c("Facility-based individual management (FBIM)",
                                                     "Facility-based group (FBG)", "Fast track refill (FTDR)"), scooter_light, genoa_light))

v2_vls <- df_DSD_vls_age_sex %>% 
  filter(!is.na(VLS)) %>% 
  ggplot(aes(agecoarse, fct_relevel(current_arv_mode, "Facility-based individual management (FBIM)",
                                    "Facility-based group (FBG)",
                                    "Community Drug Distribution Point (CDDP)",
                                    "Fast track refill (FTDR)",
                                    "Community client-led ART delivery (CCLAD)"))) +
  geom_tile(aes(fill = fill_color),color = "white", size = 1, alpha = .8) +
  geom_text(aes(label = percent(VLS, 1)), na.rm = TRUE,
            family = "Source Sans Pro", hjust = -.25, color = "#505050") + 
  facet_grid(~gender) +
  scale_x_discrete(position = "top") +
  scale_fill_identity() +
  labs(x = NULL, y = NULL) +
  si_style_nolines() +
  theme(strip.placement = "outside",
        strip.text.x = element_text(hjust = .5),
        panel.spacing.x = unit(0, "points"),
        plot.background = element_rect(fill = "white", color = NA))

v1_vls <- df_DSD_vls %>% 
  filter(!is.na(VLS)) %>% 
  ggplot(aes(VLS, fct_reorder(current_arv_mode, VLS, min), fill = fill_color)) +
  geom_col() +
  geom_vline(xintercept = seq(0, 1, by = .25), 
             color = "white", alpha = .6) +
  geom_text(aes(label = percent(VLS, 1)), na.rm = TRUE,
            family = "Source Sans Pro", hjust = -.25, color = "#505050") +
  labs(x = NULL, y = NULL,
       # title = glue("AS OF {month}, {shr} OF {ptnr} TOTAL REPORTED TREATMENT PATIENTS WERE ON <span style = 'color:{genoa};'>+3 MONTHS OF MMD</span>"),
       # caption = glue("Source: {hfr_source}")
  ) +
  scale_x_continuous(label = percent, expand = c(.005, .005), position = "top") +
  scale_fill_identity() +
  scale_alpha_identity() +
  si_style_nolines() +
  theme(legend.position = "none",
        # axis.text.x = element_blank(),
        plot.title = element_markdown(),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = NA))


v1_vls + v2_vls + plot_annotation(
  title = glue("Share of patients with suppressed viral loads by current ARV dispensing method"),
  subtitle = glue("Uganda COP22 Patient Preference Analysis | USAID"),
  caption = "Source: DSD Patient Reference Export, 16 Feb 2022"
)  & si_style_nolines() & theme(
  # axis.text.x = element_blank(),
  #legend.position = "none",
  strip.placement = "outside",
  strip.text.x = element_text(hjust = .5),
  panel.spacing.x = unit(.01, "points"),
  plot.title = element_markdown(),
  plot.title.position = "plot",
  plot.background = element_rect(fill = "white", color = NA)
)

si_save("cop-22-vls-by-current-mode.svg", path = "Graphics")







