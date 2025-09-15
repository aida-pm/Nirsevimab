# Install packages if not already installed
required_packages <- c("readxl", "tidyverse", "broom", "glmnet", "sandwich")

new_packages <- required_packages[!(required_packages %in% 
                                      installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, quietly = TRUE)

# Suppress warnings while loading libraries
suppressWarnings({
  library(readxl)
  library(tidyverse)
  library(broom)
  library(glmnet)
  library(sandwich)
})

#data <- read_excel("admissionsED.xlsx")
data <- read_excel("attendancesED.xlsx")

# Transform the data of total admissions to fit the Poisson model condition
data <- data %>%
  mutate(
    log_total_admissions = log(all_diag), 
    place_fac = as.factor(Place),
    season_fac = as.factor(Season),
    age_fac = as.factor(age_group)
  )

data <- data %>%
  filter(age_fac %in% c("<6m", "6-11m", "12m-23m"))

data <- data %>%
  filter(Season != "2020-2021") %>%  # Exclude 2020-2021
  filter(Season != "2023-2024") %>% # Exclude 2023-2024
  mutate(season_fac = case_when(
    Season == "2024-2025" ~ "2024-2025",
    TRUE ~ "pre-intervention"
  ),
    season_fac = factor(season_fac, 
                        levels = c("pre-intervention", "2024-2025")))

# Model and results of season per age
model_results3 <- list()

for (place in unique(data$Place)) {
  model_results3[[place]] <- list()  
  for (age in unique(data$age_fac)) {
    model2 <- glm(bronquis ~ season_fac + offset(log_total_admissions),
                 family = poisson(link = "log"),
                 data = data[data$Place == place & data$age_fac == age, ])
    model_results3[[place]][[age]] <- summary(model2)  
  }
}
model_results3

# e^beta (RR) with CI and pval for the final table
final_results <- function(model) {
  coef_est <- coef(model)[ , "Estimate"]
  std_err <- coef(model)[ , "Std. Error"]
  pval <- coef(model)[, "Pr(>|z|)"]

  ci_lower <- coef_est - 1.96 * std_err
  ci_upper <- coef_est + 1.96 * std_err
  
  rr <- exp(coef_est)
  ci_lower_exp <- exp(ci_lower)
  ci_upper_exp <- exp(ci_upper)

  result_df <- data.frame(
    Term = names(coef_est),
    RR = rr,
    "CI Lower 95%" = ci_lower_exp,
    "CI Upper 95%" = ci_upper_exp,
    "P-val" = pval
  )
  
  return(result_df)
}


model_rr_results3 <- list()
for (place in names(model_results3)) {
  model_rr_results3[[place]] <- list()  
  for (age in names(model_results3[[place]])) {
    model_rr_results3[[place]][[age]] <- final_results(model_results3[[place]][[age]])
  }
}

model_rr_results3

# plot
rr_data <- map_dfr(names(model_rr_results3), function(place) {
  map_dfr(names(model_rr_results3[[place]]), function(age) {
    df <- model_rr_results3[[place]][[age]]
    df$Place <- place
    df$Age <- age
    df
  })
})

rr_data_filtered <- rr_data %>%
  filter(grepl("season_fac", Term))

rr_data_filtered$Age <- factor(rr_data_filtered$Age, levels = c("<6m", "6-11m", "12m-23m"))
rr_data_filtered$Place <- factor(rr_data_filtered$Place, 
                                 levels = c("Catalonia", "Rome", "Bristol",
                                            "Leicester", "Glasgow", "Edinburgh",
                                            "Iceland", "Romania"))

rr_data_filtered$Term <- recode(rr_data_filtered$Term,
                                "season_fac2024-2025" = "")
ggplot(rr_data_filtered, aes(
  x = RR, 
  y = Place, 
  color = Age,
  shape = Age  
)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = `CI.Lower.95.`, xmax = `CI.Upper.95.`), 
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  facet_wrap(~ Term, ncol = 1) +
  scale_x_log10() +
  scale_color_manual(values = c(
    "<6m" = "#4292c6",
    "6-11m" = "#74c476",
    "12m-23m" = "#e34a33"
  )) +
  scale_shape_manual(values = c(
    "<6m" = 16,    # solid circle
    "6-11m" = 17,  # solid triangle
    "12m-23m" = 15 # solid square
  )) +
  theme_minimal(base_size = 13) +
  labs(x = "RR (log scale)",
       title = "Attendances at ED",
       color = "Age group",
       shape = "Age group") +  
  theme(legend.position = "bottom")

