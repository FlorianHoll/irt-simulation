library(tidyverse)

source("utils.R")

RESULT_FOLDER = "../results/"

# read in data
results <- list.files(RESULT_FOLDER, "*.Rds")
results_as_list <- lapply(results[1:62], function(x) {
  r <- readRDS(paste0(RESULT_FOLDER, x))
  get_all_deviations(r)
})

all_results <- do.call(rbind, results_as_list)

# plot deviation of discrimination parameters
all_results %>%
  group_by(nr_persons, nr_items, nr_dimensions) %>%
  summarise(
    a_m= mean(discrimination_deviations),
    a_se = se(discrimination_deviations),
    d_m = mean(difficulty_deviations),
    d_se = se(difficulty_deviations)
  ) %>%
  ggplot(aes(x=as.factor(nr_items), y=a_m, fill=as.factor(nr_dimensions))) +
    geom_bar(stat="identity", position = position_dodge()) +
    facet_wrap(~as.factor(nr_persons), scales="free") +
    geom_errorbar(aes(x=as.factor(nr_items), ymin=a_m-1.96*a_se, ymax=a_m+1.96*a_se), width=.3, position=position_dodge(.9))

# plot deviation of mean parameters
all_results %>%
  group_by(nr_persons, nr_items, nr_dimensions) %>%
  summarise(
    a_m= mean(discrimination_deviations),
    a_se = se(discrimination_deviations),
    d_m = mean(difficulty_deviations),
    d_se = se(difficulty_deviations)
  ) %>%
  ggplot(aes(x=as.factor(nr_items), y=d_m, fill=as.factor(nr_dimensions))) +
  geom_bar(stat="identity", position = position_dodge()) +
  facet_wrap(~as.factor(nr_persons)) +
  geom_errorbar(aes(x=as.factor(nr_items), ymin=d_m-1.96*d_se, ymax=d_m+1.96*d_se), width=.3, position=position_dodge(.9))

# plot deviation of discriminations as a function of the number of persons
all_results %>%
  filter(nr_dimensions == 3) %>%
  group_by(nr_persons, nr_items) %>%
  summarise(
    a_m = mean(discrimination_deviations),
    a_se = se(discrimination_deviations),
    ci_lower = a_m - 1.96*a_se,
    ci_upper = a_m + 1.96*a_se
  ) %>%
  ggplot(aes(x=nr_persons, y=a_m, col=as.factor(nr_items))) +
    geom_hline(yintercept=0, linetype="dashed", size=1) +  
    geom_line(size=1) +
    geom_ribbon(aes(x=nr_persons, ymin=ci_lower, ymax=ci_upper, fill=as.factor(nr_items)), alpha=.2) +
    # geom_smooth(se=FALSE) +
    geom_errorbar(aes(x=nr_persons, ymin=ci_lower, ymax=ci_upper), size=1)

# plot deviation of discriminations as a function of the number of persons
all_results %>%
  group_by(nr_persons, nr_dimensions, nr_items) %>%
  summarise(
    a_m = mean(discrimination_deviations),
    a_se = se(discrimination_deviations),
    ci_lower = a_m - 1.96*a_se,
    ci_upper = a_m + 1.96*a_se
  ) %>%
  ggplot(aes(x=nr_persons, y=a_m, col=as.factor(nr_dimensions))) +
  geom_hline(yintercept=0, linetype="dashed", size=1) +  
  geom_line(size=1) +
  geom_ribbon(aes(x=nr_persons, ymin=ci_lower, ymax=ci_upper, fill=as.factor(nr_dimensions)), alpha=.2) +
  # geom_smooth(se=FALSE) +
  geom_errorbar(aes(x=nr_persons, ymin=ci_lower, ymax=ci_upper), size=1) +
  facet_wrap(~nr_items)
 

