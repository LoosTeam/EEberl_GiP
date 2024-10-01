# Analysis of change of nutritional status parameters and initiation of geophagia

## Backward selection

This code prepares the dataset for backward selection:

```{r}
GLMAnalysisBackward <-GLMAnalysis %>%
    #Filter the GLMAnalysis ubset for visits belonging to participants listed in Change_IDs and visits where a dietary recall was completed in the third trimester
    filter(
        visit_trimester==3,
        Done_24h==1,
        ID%in% Change_IDs) %>%
    # Select variables for the backward selection analysis based on variables that had a p-value <0.25 in the univariate analyses
    select(
        ID,                     # Participant ID
        soileating,             # Whether or not the participant ate soil
        soileating_t3,          # Soil-eating behavior in third trimester
        ga_inclusion,           # Gestational age at enrollment
        # weight_W,             # Removed due to 123 missing values
        # bmi_W,                # Removed due to 123 missing values
        # Prepreg_FeDeficient,  # Removed due to 112 missing values
        parity,                 # Number of previous births
        # FolicSupp_Inc,        # Removed due to duplication with 'ironfol_supp_inc'
        # IronSupp_Inc,         # Removed due to duplication with 'ironfol_supp_inc'
        ironfol_supp_inc,       # Iron and folic acid supplementation at enrollment
        enrollment_year,        # Year of enrollment into the study
        age,                    # Age of the participant
        hx_miscarriage,         # History of miscarriage
        marital_status,         # Marital status of the participant
        maternal_occupation,    # Occupation of the mother
        malaria_anytime,        # Whether the participant had malaria anytime during pregnancy (excluding delvery)
        antihelminths_anytime,  # Use of antihelminthics anytime during pregnancy
        B12_supp_anytime,       # Vitamin B12 supplementation anytime during pregnancy
        GA_visit,               # Gestational age at the time of the visit
        count_scheduled_ANV,    # Number of scheduled antenatal visits
        preeclampsia            # Whether the mother had preeclampsia during pregnancy
    ) %>%
    # Arrange the data in descending order based on soil-eating behavior
    arrange(desc(soileating)) %>%
    # Ensure that each participant (ID) appears only once in the dataset, keeping all other columns
    distinct(ID, .keep_all = TRUE) %>%
    # Remove rows with any missing values in the selected columns
    na.omit()
```

This code fits a full logistic regression model predicting `soileating` (binary outcome) based on multiple predictors:

```{r}
Fullmodel <- glm(soileating ~
                     ga_inclusion +
                     GA_visit +
                     parity +
                     ironfol_supp_inc +
                     enrollment_year +
                     age +
                     hx_miscarriage +
                     marital_status +
                     maternal_occupation +
                     malaria_anytime +
                     antihelminths_anytime	+
                     B12_supp_anytime +
                     count_scheduled_ANV +
                     preeclampsia, 
                 data = GLMAnalysisBackward, 
                 family = binomial) # Logistic regression (binary outcome)

# Perform backward stepwise selection to find the best-fitting model by removing non-significant predictors
backward_selection <- step(Fullmodel)

# Display the formula of the selected model after backward stepwise selection
formula(backward_selection)

# Print the summary of the final model, including coefficients, p-values, and model fit statistics
summary(backward_selection)
```

## Multivariable logistic regression analysis

This code fits a crude and adjusted logistic regression model predicting geophagia in the third trimester (`soileating_t3`) based on changes in nutritional status parameters across pregnancy. The change in MUAC from first trimester is included as an example.

```{r}
#Fit a crude model
Crudemodel <- glm(soileating_t3 ~ ChangeT1_MUAC, 
                  data = ChangeAnalysis, 
                  family = binomial)

#Fit an adjusted model with gestational age at enrollment, marital status, and vitamin B12 supplementation during pregnancy as the co-variates
Adjmodel <- glm(soileating_t3 ~ ChangeT1_MUAC
                + ga_inclusion
                + marital_status 
                + B12_supp_anytime 
                , data = ChangeAnalysis, family = binomial,
                na.action = na.omit) # Omit any rows with missing data

# Publish the results of the crude model (formatted for presentation)
publish(Crudemodel)

# Publish the results of the adjusted model (formatted for presentation)
publish(Adjmodel)

# AIC and BIC can be used to compare model fit between different models (with and without B12_supp_anytime)

# AIC(Adjmodel, Adjmodel2)  
# BIC(Adjmodel, Adjmodel2) 
```

This code computes the mean difference and confidence intervals:

```{r}
subset <- ChangeAnalysis %>%
    filter(
        # Include only participants with geophagia in the third trimester
        soileating_t3==1, 
        # Exclude participants with missing change data for the variable and from the trimester of interest
        !is.na(ChangeT1_MUAC))

t.test(subset$ChangeT1_MUAC) # t-test to compute the 95% confidence intervals of the change variable
```
