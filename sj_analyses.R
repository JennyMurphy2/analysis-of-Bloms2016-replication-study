# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)
library(janitor)

set.seed(21)

# Load data

data <- read_csv("rep_data.csv") %>%
  clean_names() 

# Squat jump data

paired_data <- data %>%
  select(participant, sj_height_m_placebo_number_1:sj_height_m_caffeine_number_3) %>%
  rowwise() %>%
  mutate(sj_pla = mean(c(sj_height_m_placebo_number_1, sj_height_m_placebo_number_2,
                           sj_height_m_placebo_number_3))*100,
         sj_caff = mean(c(sj_height_m_caffeine_number_1, sj_height_m_caffeine_number_2,
                          sj_height_m_caffeine_number_3))*100)  %>% 
  mutate(difference = sj_caff - sj_pla) %>%
  as.data.frame()

# Prepare data ----------------

long_data <- paired_data %>% 
  select(participant, sj_pla,  sj_caff) %>%
  pivot_longer(cols = c("sj_pla", "sj_caff"),
               names_to = "group",
               values_to = "sj_height") 

## Descriptives -------------------------------------

summary_rep <- long_data %>%
  group_by(group) %>%
  summarise(count = n(),
            mean = mean(sj_height),
            sd = sd(sj_height)) %>%
  mutate(mean_diff = mean(paired_data$difference), 
         sd_diff = sd(paired_data$difference)
  )
summary_rep

## Resolving assumptions  ------------------------------------
### Distribution ---------------------------------------

ggplot(long_data, aes(sj_height)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10) +
  facet_wrap(~ group,
          labeller = label_both)

ggplot(long_data, aes(group, sj_height, color = group)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

  ggplot(long_data, aes(group, sj_height, color = group)) +  
  geom_violin(fill = "light gray") +
  geom_boxplot(width = .07,
               fill = "white") +
  geom_jitter(position = position_jitter(0.21)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 12,
               color = "black",
               size = 5) +
  theme_bw()

### Outliers on difference score -----------------------------------

outliers <- paired_data %>%
  identify_outliers(difference)

### Normality ----------------------------------------------------------
  
paired_data %>% 
    shapiro_test(difference) 
  
# data is not normal therefore remove participant id = 54
  
# Normal data -----------  
normal_long_data <- long_data %>%
  filter(participant !=54)

normal_paired_data <- paired_data %>%
  filter(participant !=54)

## Outlier test ------

normal_paired_data %>%
  identify_outliers(difference)

## Normality test ------

normal_paired_data %>% 
  shapiro_test(difference) 
  
# Normal paired t-test ---------------------------------------------------

normal_long_data$group <- as.factor(normal_long_data$group)

# R compares conditions alphabetically, I am reordering here to match the original study

normal_long_data$group <- forcats::fct_relevel(normal_long_data$group, "sj_caff", "sj_pla")

#normal_results <- t.test(sj_height ~ group, normal_long_data, 
#                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
#  tidy()
#normal_results

normal_results <- t.test(normal_paired_data$sj_caff, normal_paired_data$sj_pla, paired = TRUE)
normal_results

normal_summary_rep <- normal_long_data %>%
  group_by(group) %>%
  summarise(count = n(),
            mean = mean(sj_height),
            sd = sd(sj_height)) %>%
  mutate(mean_diff = mean(normal_paired_data$difference), 
         sd_diff = sd(normal_paired_data$difference)
  )
normal_summary_rep

# Analyse the replication ------

## Calculate replication ES ------

rep_dz <- d.dep.t.diff(mdiff = normal_results$estimate, 
                       sddiff = normal_summary_rep$sd_diff[1], 
                       n = normal_summary_rep$count[1], a = 0.05)
rep_dz

## Original values ------

orig_values <- data.frame(
  ori_pval = 0.001,
  N = 25,
  caff_mean = 34.5,
  caff_sd = 6.7,
  pla_mean = 32.8,
  plac_sd = 6.2
)

## Calculate Original ES  ------

quantile = 1 - (orig_values$ori_pval/2)# for two-tailed

orig_tval <- qt(quantile, df = 24)

ori_dz <- d.dep.t.diff.t(t = orig_tval, n = 25, a = 0.05)
ori_dz

## Z-test  --------

rep_test <- compare_smd(
  smd1 = ori_dz$d,
  n1 = orig_values$N,
  smd2 = rep_dz$d,
  n2 = summary_rep$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test

