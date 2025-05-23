
# Load packages
library(dplyr)
library(tidyr)

# Read data
data_raw <- read.csv("./data/data_raw/actions_taken_1-3_state_DC.csv") # 11531 observations and 99 variables

# Information on all variables and their possible values
# https://ffiec.cfpb.gov/documentation/publications/loan-level-datasets/lar-data-fields


# We only look at data with only one applicant
# So we remove all data with one or more co-applicants
data <- filter(data_raw, co.applicant_credit_score_type == 10) # 10 means "No co-applicant"

# Rough removal of unnecessary columns
data <- select(data, -c(84:92)) # Not needed for analysis
data <- select(data, -c(79:81)) # Not needed for analysis
data <- select(data, -c(75:77)) # Not needed for analysis
data <- select(data, -c(49:73)) # Not needed for analysis
dim(data) # 7557 observations and 59 variables

# Now more precise removal of unnecessary columns
data <- select(data, !lei) # Not needed for analysis
data <- select(data, !activity_year) # We only look at year 2023 -> unnecessary
data <- select(data, !state_code) # We only look at District of Columbia (DC) -> unnecessary
data <- select(data, !county_code) # DC only has county 11001 -> unnecessary
data <- select(data, !derived_msa.md) # Almost only takes the value 47894 -> unnecessary
data <- select(data, !conforming_loan_limit) # Not needed for analysis
data <- select(data, !derived_loan_product_type) # Same information in loan_type and lien_status
data <- select(data, !derived_dwelling_category) # Not needed for analysis
data <- select(data, !purchaser_type) # Not needed for analysis
data <- select(data, !reverse_mortgage) # Almost only takes value 2 -> unnecessary
data <- select(data, !interest_rate) # Only accepted credits have an interest_rate -> Not really usable for analysis
data <- select(data, !rate_spread) # Same as with interest_rate
data <- select(data, !total_loan_costs) # Many NAs (~47%) -> Not really usable for analysis
data <- select(data, !total_points_and_fees) # Many NAs (~100%) -> Not usable for analysis
data <- select(data, !origination_charges) # Many NAs (~46%) -> Not really usable for analysis
data <- select(data, !discount_points) # Many NAs (~47%) -> Not really usable for analysis
data <- select(data, !lender_credits) # Many NAs (~47%) -> Not really usable for analysis
data <- select(data, !prepayment_penalty_term) # Many NAs (~95%) -> Not really usable for analysis
data <- select(data, !intro_rate_period) # Many NAs (~71%) -> Not really usable for analysis
data <- select(data, !negative_amortization) # Only takes the vlaue 2 -> unnecessary
data <- select(data, !interest_only_payment) # Not needed for analysis
data <- select(data, !balloon_payment) # Almost only takes value 2 -> unnecessary
data <- select(data, !other_nonamortizing_features) # Almost only takes value 2 -> unnecessary
data <- select(data, !construction_method) # Almost only takes value 1 -> unnecessary
data <- select(data, !manufactured_home_secured_property_type) # Almost only takes value 3 -> unnecessary
data <- select(data, !manufactured_home_land_property_interest) # Almost only takes value 5 -> unnecessary
data <- select(data, !multifamily_affordable_units) # Many NAs (~99%) -> Not really usable for analysis
data <- select(data, !applicant_sex) # Same information as in derived_sex -> unnecessary
data <- select(data, !submission_of_application) # Not needed for analysis
data <- select(data, !initially_payable_to_institution) # Almost only takes value 1 -> unnecessary
data <- select(data, !tract_population) # Not needed for analysis
data <- select(data, !ffiec_msa_md_median_family_income) # Almost only takes value 150100 -> unnecessary
data <- select(data, !tract_owner_occupied_units) # Not needed for analysis
data <- select(data, !tract_one_to_four_family_homes) # Not needed for analysis
data <- select(data, !tract_median_age_of_housing_units) # Not needed for analysis
data <- select(data, !hoepa_status) # Not needed for analysis
data <- select(data, !loan_to_value_ratio) # Not needed for analysis
dim(data) # 7557 observations and 22 variables


# Handling missing values
sum(is.na(data$census_tract)) # 19 NAs -> no sensible replacement value -> removing
data <- drop_na(data, census_tract)
data <- select(data, !census_tract) # Not needed anymore
sum(is.na(data$property_value)) # 322 NAs -> property_value is crucial for mortgage credit applications -> removing
data <- drop_na(data, property_value)
sum(is.na(data$income)) # 325 NAs -> no sensible replacement value -> removing
data <- drop_na(data, income)
sum(is.na(data$debt_to_income_ratio)) # 90 NAs -> new category "unkonwn"
data$debt_to_income_ratio <- replace_na(data$debt_to_income_ratio, "unknown")
sum(is.na(data$loan_term)) # 95 NAs -> replace with median
data$loan_term <- as.numeric(data$loan_term)
data$loan_term <- replace_na(data$loan_term, median(data$loan_term, na.rm = TRUE))
dim(data) # 6891 observations and 21 variables


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
         open.end_line_of_credit = if_else(open.end_line_of_credit == 1, "Open-end line of credit", "Not an open-end line of credit"),
         business_or_commercial_purpose = if_else(business_or_commercial_purpose == 1,
                                                  "Primarily for a business or commercial purpose",
                                                  "Not primarily for a business or commercial purpose"),
         occupancy_type = if_else(occupancy_type == 1, "Principal residence",
                                  if_else(occupancy_type == 2, "Second residence", "Investment property"))
         )

data <- data %>% 
  mutate(
    derived_ethnicity = as.factor(derived_ethnicity),
    derived_race = as.factor(derived_race),
    derived_sex = as.factor(derived_sex),
    action_taken = as.factor(action_taken),
    preapproval = as.factor(preapproval),
    loan_type = as.factor(loan_type),
    loan_purpose = as.factor(loan_purpose),
    lien_status = as.factor(lien_status),
    open.end_line_of_credit = as.factor(open.end_line_of_credit),
    business_or_commercial_purpose = as.factor(business_or_commercial_purpose),
    occupancy_type = as.factor(occupancy_type),
    total_units = as.factor(total_units),
    debt_to_income_ratio = as.factor(debt_to_income_ratio),
    applicant_credit_score_type = as.factor(applicant_credit_score_type),
    applicant_age = as.factor(applicant_age),
    income = as.numeric(income),
    loan_amount = as.numeric(loan_amount),
    property_value = as.numeric(property_value)
  )


# Save preprocessed data
write.csv(data, file = "./data/credit_approval_data.csv")
saveRDS(data, file = "./data/credit_approval_data.rds")

