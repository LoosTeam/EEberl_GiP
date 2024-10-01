# Univariate analyses
Univariate analyses with either geophagia at any timepoint during pregnancy or geophagia in the third trimester as the dependent variable were used to identify which co-variates to adjust for in the logistic regression model investigating the association between changes in nutritional status parameters and initiation og GiP in third trimester. 

The following code creates a subset of the data for participants who completed a dietary recall (Done_24h == 1) in the third trimester (visit_trimester == 3) and a subset for participants who completed at least one dietary recall during pregnancy, with a preference for entries where geophagia was reported:

```{r}
T3_UnivAnalysis <- ponadb %>%
    # Include only participants with a completed dietary recall in the third trimester
    filter(Done_24h == 1,
           visit_trimester == 3) %>% 
    # Arrange the data in descending order based on the soil-eating behavior
    arrange(desc(soileating)) %>%
    # Retain only distinct IDs, ensuring only one entry per individual while keeping all other columns
    distinct(ID, .keep_all = TRUE)


SoilEver_UnivAnalysis <- ponadb %>%
    # Include only those participants who have dietary recall data
    filter(ID %in% dietrecall_IDs) %>%  
    # Arrange the data in descending order based on whether they ever ate soil (soileating_ever)
    arrange(desc(soileating_ever)) %>%
    # Retain only distinct IDs, ensuring only one entry per individual while keeping all other columns
    distinct(ID, .keep_all = TRUE)

```

This code selects the columns of interest for the univariate analysis. For the univariate analysis with geophagia at any time as the dependent variable, select `soileating_ever` . For the univariate analysis withgeophagia in the third trimester as the dependent variable, select `soileating` and 15 extra co-variates.

```{r}
Univariate <- T3_UnivAnalysis  %>%
    select(
        # "soileating_ever",
        "soileating",
        "age",
        "ga_inclusion",
        "inclusion_weight",
        "BMI_I",
        "MUAC_I",
        "height",
        "weight_W",
        "bmi_W",
        "alcohol",
        "hx_miscarriage",
        "hx_stillbirth",
        "parity",
        "marital_status",
        "ses",
        "ethnicity",
        "religion",
        "maternal_education",
        "maternal_occupation",
        "paternal_education",
        "paternal_occupation",
        "inclusion_malaria",
        "HIVstatus",
        "inclusion_anaemia",
        "recent_hx_anemia",
        "Prepreg_B12Deficient",
        "Prepreg_FolDeficient",
        "Prepreg_FeDeficient",
        "Prepreg_Anaemia",
        "chronic_disease",
        "medication_inc",
        'folic_supp_inc',
        'iron_supp_inc',
        "ironfol_supp_inc",
        "enrollment_season",
        "enrollment_year",
        "TWIN.x",
        "sex_of_offspring",
        # Include the following variables in the third trimester geophagia univariate analysis:
        "malaria_anytime",
        "antihelminths_anytime",
        "any_supp_anytime",
        "B12_supp_anytime",
        "fol_supp_anytime",
        "iron_supp_anytime",
        "GA_visit",
        "count_scheduled_ANV",
        "count_all_ANV",
        "had_emr_visits",
        "anaemia_t1",
        "IronDeficient_T1",
        "FolDeficient_T1",
        "B12Deficient_T1",
        "preeclampsia"
    )
```

This code creates a summary table from the 'Univariate' dataset, grouping by the `soileating` or `soileating_ever` variable

```{r}
Univariate %>%
    tbl_summary(by = soileating, 
                # Treat all dichotomous variables as categorical
                type = all_dichotomous() ~ "categorical", 
                # Exclude missing values from the summary
                missing = "no",  
                # Calculate percentages by column
                percent = "column",
                # Define summary statistics for continuous and categorical variables
                statistic = list(                    
                    all_continuous() ~ "{mean}",    
                    all_categorical() ~ "{n} ({p}%)"),  
                digits = list(                       
                    # Specify the number of decimal places for the summary statistics
                    all_continuous() ~ 1,           
                    all_categorical() ~ c(0, 1)))
# Add statistical tests to the summary table and define tests to be used for the different variable types
add_p(
        test = list(                            
            all_continuous() ~ "t.test",       
            all_categorical() ~ "chisq.test")) %>%  
# Format the p-values to three decimal places 
modify_fmt_fun(p.value ~ function(x) style_pvalue(x, digits = 3)) %>%
# Modify the header of the table to include a bold "Variable" label
modify_header(label ~ "**Variable**") %>%
# Add a footnote to the table explaining the summary statistics
modify_footnote(
        all_stat_cols() ~ "Mean or Frequency (%)"  
) %>%
# Add a caption to the table
modify_caption("**Univariate analysis**") %>%
# Bold the labels in the summary table for emphasis
bold_labels() %>%
# Add an overall row to the summary table for overall statistics
add_overall() %>%
# Convert the summary table to a GT table format 
as_gt()
```

