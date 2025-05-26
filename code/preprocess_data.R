
# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data
data_raw <- read.csv("./data/data_raw/actions_taken_1-3_state_DC.csv") # 11531 observations and 99 variables

# Information on all variables and their possible values
# https://ffiec.cfpb.gov/documentation/publications/loan-level-datasets/lar-data-fields


# Select interesting variables for analysis.
# All other variables are either not needed or cannot be used properly (e.g. due to number of NAs)
data <- data_raw %>% 
  select(
    action_taken,
    applicant_credit_score_type,
    co.applicant_credit_score_type,
    income,
    debt_to_income_ratio,
    applicant_age,
    applicant_ethnicity.1,
    applicant_race.1,
    applicant_sex,
    loan_amount,
    loan_to_value_ratio,
    interest_rate,
    rate_spread,
    loan_type,
    loan_purpose,
    loan_term,
    origination_charges,
    total_loan_costs,
    discount_points,
    property_value,
    total_units,
    occupancy_type,
    conforming_loan_limit,
    tract_population,
    tract_minority_population_percent,
    tract_to_msa_income_percentage,
    tract_owner_occupied_units,
    tract_median_age_of_housing_units,
    submission_of_application,
    business_or_commercial_purpose,
    preapproval,
    lien_status,
    open.end_line_of_credit
  )

# We create a new variable "has_co.applicant"
data <- data %>% 
  mutate(
    has_co.applicant = if_else(co.applicant_credit_score_type == 10, "No", # 10 means "No co-applicant"
                               "Yes"),
    co.applicant_credit_score_type = NULL # Not needed anymore
  )
dim(data) # 11531 observations and 33 variables


# Handling missing values and other special values
sum(is.na(data$income)) # 792 NAs -> impute values with median grouped by "tract_to_msa_income_percentage"
data <- data %>% 
  group_by(tract_to_msa_income_percentage) %>% 
  mutate(income = if_else(is.na(income), median(income, na.rm = TRUE), income)) %>%
  ungroup()

sum(is.na(data$debt_to_income_ratio)) # 855 NAs -> new category "unknown"
data$debt_to_income_ratio <- replace_na(data$debt_to_income_ratio, "unknown")
sum(data$debt_to_income_ratio == "Exempt") # 144 obs. -> also unknown
data <- data %>%  mutate(
  debt_to_income_ratio = if_else(debt_to_income_ratio == "Exempt", "unknown", debt_to_income_ratio))

sum(data$loan_term == "Exempt", na.rm = TRUE) # 144 obs. -> remove
data <- data %>% 
  filter(loan_term != "Exempt")
sum(is.na(data$loan_term))
data$loan_term <- as.numeric(data$loan_term)
data$loan_term <- replace_na(data$loan_term, median(data$loan_term, na.rm = TRUE))

sum(is.na(data$property_value)) # 419 NAs -> impute values with median groubed by "total_units"
data$property_value <- as.numeric(data$property_value)
data <- data %>% 
  group_by(total_units) %>% 
  mutate(property_value = if_else(is.na(property_value), median(property_value, na.rm = TRUE), property_value)) %>%
  ungroup()

sum(is.na(data$applicant_ethnicity.1)) # 5 NAs -> remove
data <- drop_na(data, applicant_ethnicity.1)

sum(is.na(data$applicant_race.1)) # 2 NAs -> remove
data <- drop_na(data, applicant_race.1)

sum(is.na(data$loan_to_value_ratio)) # 512 NAs -> calculate values with loan_amount/property_value
data$loan_to_value_ratio <- as.numeric(data$loan_to_value_ratio)
data <- data %>% 
  mutate(loan_to_value_ratio = if_else(is.na(loan_to_value_ratio), (loan_amount/property_value)*100, loan_to_value_ratio))

sum(is.na(data$interest_rate)) # 2807 NAs -> variable not really usable -> remove column
data$interest_rate <- NULL

sum(is.na(data$rate_spread)) # 3576 NAs -> same with interest_rate
data$rate_spread <- NULL

sum(is.na(data$origination_charges)) # 4666 NAs -> same with interest_rate
data$origination_charges <- NULL

sum(is.na(data$total_loan_costs)) # 4699 NAs -> same with interest_rate
data$total_loan_costs <- NULL

sum(is.na(data$discount_points)) # 4683 NAs -> same with interest_rate
data$discount_points <- NULL

sum(is.na(data$conforming_loan_limit)) # 138 NAs -> new category "unknown"
data$conforming_loan_limit <- replace_na(data$conforming_loan_limit, "unknown")

dim(data) # 11244 observations and 29 variables


# Categorical variables to factors and numerical features to numerics
data <- data %>% 
  mutate(action_taken = if_else(action_taken == 1, "Loan approved", "Loan denied"),
         preapproval = if_else(preapproval == 1, "Preapproval requested", "Preapproval not requested"),
         loan_type = if_else(loan_type == 1, "Conventional",
                             if_else(loan_type == 2, "FHA", "VA")),
         loan_purpose = if_else(loan_purpose == 1, "Home purchase",
                                if_else(loan_purpose == 2, "Home improvement",
                                        if_else(loan_purpose == 31, "Refinancing",
                                                if_else(loan_purpose == 32, "Cash-out refinancing", "Other purpose")))),
         lien_status = if_else(lien_status == 1, "Secured by a first lien", "Secured by a subordinate lien"),
         open.end_line_of_credit = if_else(open.end_line_of_credit == 1, "Open-end line of credit", "Not open-end line of credit"),
         business_or_commercial_purpose = if_else(business_or_commercial_purpose == 1,
                                                  "Primarily for a business or commercial purpose",
                                                  "Not primarily for a business or commercial purpose"),
         occupancy_type = if_else(occupancy_type == 1, "Principal residence",
                                  if_else(occupancy_type == 2, "Second residence", "Investment property")),
         debt_to_income_ratio = if_else(debt_to_income_ratio %in% c("36", "37", "38", "39", "40", "41", "42"), "36%-42%",
                                        if_else(debt_to_income_ratio %in% c("43", "44", "45", "46", "47", "48", "49"), "43%-49%",
                                        debt_to_income_ratio)),
         debt_to_income_ratio = if_else(debt_to_income_ratio == "20%-<30%", "20%-29%",
                                        if_else(debt_to_income_ratio == "30%-<36%", "30%-35%", debt_to_income_ratio)),
         applicant_ethnicity.1 = if_else(applicant_ethnicity.1 %in% c(1, 11, 12, 13, 14), "Hispanic or Latino",
                                         if_else(applicant_ethnicity.1 == 2, "Not Hispanic or Latino", "unknwon")),
         applicant_race.1 = if_else(applicant_race.1 %in% c(2, 21, 22, 23, 24, 25, 26, 27), "Asian",
                                    if_else(applicant_race.1 == 3, "Black or African American",
                                            if_else(applicant_race.1 == 5, "White", "other"))),
         applicant_sex = if_else(applicant_sex %in% c(3, 4, 6), "unknown",
                                 if_else(applicant_sex == 1, "Male", "Female")),
         submission_of_application = if_else(submission_of_application == 1, "Submitted directly", "Not submitted directly")
  ) %>% 
  rename(
    applicant_ethnicity = applicant_ethnicity.1,
    applicant_race = applicant_race.1,
  )

data <- data %>% 
  mutate(
    applicant_ethnicity = as.factor(applicant_ethnicity),
    applicant_race = as.factor(applicant_race),
    applicant_sex = as.factor(applicant_sex),
    action_taken = as.factor(action_taken),
    preapproval = as.factor(preapproval),
    loan_type = as.factor(loan_type),
    loan_purpose = as.factor(loan_purpose),
    lien_status = as.factor(lien_status),
    open.end_line_of_credit = as.factor(open.end_line_of_credit),
    business_or_commercial_purpose = as.factor(business_or_commercial_purpose),
    occupancy_type = as.factor(occupancy_type),
    total_units = as.factor(total_units),
    debt_to_income_ratio = factor(debt_to_income_ratio,
                                  levels = c("<20%", "20%-29%", "30%-35%", "36%-42%", "43%-49%", "50%-60%", ">60%", "unknown")),
    applicant_credit_score_type = as.factor(applicant_credit_score_type),
    applicant_age = as.factor(applicant_age),
    income = as.numeric(income),
    loan_amount = as.numeric(loan_amount),
    property_value = as.numeric(property_value),
    has_co.applicant = as.factor(has_co.applicant),
    conforming_loan_limit = as.factor(conforming_loan_limit),
    submission_of_application = as.factor(submission_of_application)
  )


# Transformation of various variables
data %>% 
  ggplot(aes(x = income)) +
  geom_density() # heavy skewed -> log transformation
table(data$income < 0) # only 15 values < 0 -> set them to 0 for log transformation

data %>% 
  ggplot(aes(x = property_value)) +
  geom_density() # heavy skewed -> log transformation
table(data$property_value <= 0) # No value <= 0

data %>% 
  ggplot(aes(x = loan_amount)) +
  geom_density() # heavy skewed -> log transformation
table(data$loan_amount <= 0) # No value <= 0

data <- data %>% 
  mutate(income_log = if_else(income < 0, 0, income),
         income_log = log1p(income_log),
         property_value_log = log(property_value),
         loan_amount_log = log(loan_amount)
  )

# We dont need to original variables any longer
data$income <- NULL
data$property_value <- NULL
data$loan_amount <- NULL


# Save preprocessed data
write.csv(data, file = "./data/credit_approval_data.csv")
saveRDS(data, file = "./data/credit_approval_data.rds")

