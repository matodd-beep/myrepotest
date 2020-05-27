
#File: script_HHS_example_code
#Author: Meagan Pharis (Meagan.Pharis@Phila.gov)
#Date: December 2019
#Purpose: Example 1: Hypertension in adults. Overtime, by sex, and by age category. With ggplots.

library(tidyverse); library(srvyr); library(survey)
library(questionr)


# 1: Read in data, create variables, and create a survey object.
# 2: Look at the weighted survey mean for hypertension (high blood pressure) overtime and graph it.
# 3: Look at hypertension by sex and age category. Determine any significant differences.
# 3b: Create a categorical ggplot of hypertension by age categories.



#######
## 1 ##
#######

hhs <- read.csv("V:/PHMC Household Survey/hhs_2000to2018_Phila.csv", header = T, na.strings = c("NA", ""))


lookfor(hhs, "bmi", "obe")

# create variables for obesity, sex, and age

hhs <- hhs %>%
  mutate(age_cat = case_when(RESPAGE < 35 ~ "18-34 years",
                              RESPAGE %in% c(35:49) ~ "35-49 years",
                              RESPAGE %in% c(50:64) ~ "50-64 years",
                              RESPAGE > 64 ~ "65+ years",
                              RESPAGE_4CAT == 1 ~ "18-34 years",
                              RESPAGE_4CAT == 2 ~ "35-49 years",
                              RESPAGE_4CAT == 3 ~ "50-64 years",
                              RESPAGE_4CAT == 4 ~ "65+ years",
                              TRUE ~ NA_character_),
         sex = case_when(SEX01 == 1 ~ "Male",
                         SEX01 == 2 ~ "Female",
                         TRUE ~ NA_character_),
         female = case_when(SEX01 == 2 ~ 1,
                            SEX01 == 1 ~ 0,
                            TRUE ~ NA_real_),
         overweight = case_when(BMI >= 25 ~ 1,
                                BMI < 25 ~ 0,
                                TRUE ~ NA_real_),
         obese = case_when(BMI >= 30 ~ 1,
                           BMI < 30 ~ 0,
                           TRUE ~ NA_real_),
         sev_obese = case_when(BMI >= 40 ~ 1,
                           BMI < 40 ~ 0,
                           TRUE ~ NA_real_))
         



# create survey object using the srvyr package

w_hhs <- hhs %>%
  as_survey_design(weights = ADBALWT)




# now have a ready to use survey object (w_hhs) and can move on to analysis


#######
## 2 ##
#######


# Get a weighted surveymean of obese and severe obese by year

sum_obese <- w_hhs %>%
  group_by(year) %>%
  summarise(obese = survey_mean(obese, vartype = "ci", na.rm = T),
            sev_obese = survey_mean(sev_obese, vartype = "ci", na.rm = T)) %>%
  mutate_at(2:7, ~round(.*100))
  # specify vartype "ci" to get a 95% confidence interval. By default a standard error is provided.
  # If you have missings in the hypertension variable but don't specific na.rm = T, then you'll get an error



# Create an overtime ggplot with error bar area

g_obese <- ggplot(sum_obese, aes(x = year)) +
  geom_ribbon(aes(ymin = obese_low, ymax = obese_upp),
              alpha = 0.2, color = NA, fill = "blue", show.legend = F) +
  geom_line(aes(y = obese), color = "blue") +
  geom_point(aes(y = obese), color = "blue") +
  geom_text(aes(y = obese, label = paste0(obese, "%")), vjust = -.5, hjust = -.1, color = "blue", show.legend = F) +
 
  geom_ribbon(aes(ymin = sev_obese_low, ymax = sev_obese_upp),
              alpha = 0.2, color = NA, fill = "red", show.legend = F) +
  geom_line(aes(y = sev_obese), color = "red") +
  geom_point(aes(y = sev_obese), color = "red") +
  geom_text(aes(y = sev_obese, label = paste0(sev_obese, "%")), vjust = -.5, hjust = -.1, color = "red", show.legend = F) +
  
  
   scale_x_continuous(breaks = sum_obese$year) +
  scale_y_continuous(limits = c(0,45), expand = c(0,0), labels = function(x) paste0(x, "%")) +
  labs(title = "Obesity and severe obesity among adults \n in Philadelphia, 2000-2018",
       caption = "Source: PHMC Household Health Survey") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#eaedee"),
        axis.title =  element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        plot.caption = element_text(colour = "#7a8489"),
        plot.title = element_text(hjust = 0.5))

# run to see ggplot in plot panel
g_obese

ggsave("H:\\rlearn\\Raw Output\\obesity and severe obesity PHMC.pdf")
