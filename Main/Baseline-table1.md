## Demographics and baseline characteristics

### Table of demographic and baseline characteristics among women with and without GiP

This code selects variables to include in table 1:

```{r}
tbl1_analysis <- tbl1_subset %>%
    select("soileating_ever",
           "age",
           "ga_inclusion_wk",
           "enrollment_season",
           "inclusion_weight",
           "BMI_I",
           "MUAC_I",
           "alcohol",
           "parity",
           "hx_miscarriage",
           "hx_stillbirth",
           "marital_status",
           "ethnicity",
           "religion",
           "maternal_education",
           "maternal_occupation",
           "ses",
           "inclusion_malaria",
           "HIVstatus",
           "inclusion_anaemia",
           "recent_hx_anemia",
           "chronic_disease",
           "medication_inc",
           "ironfol_supp_inc")
```

This code create a summary table grouped by whether or not participants reported eating soil at any time-point during pregnancy (soileating_ever):

```{r}
# Begin with the 'tbl1_analysis' data
tbl1_analysis %>%                                  
    tbl_summary(by = soileating_ever,             
                # Treat all dichotomous variables as categorical
                type = all_dichotomous() ~ "categorical",  
                # Exclude missing values from the summary
                missing = "no",                   
                # Display percentages based on column totals
                percent = "column",               
                # Define summary statistics
                statistic = list(                 
                    # Show mean for continuous variables
                    all_continuous() ~ "{mean}",  
                    # Show counts and percentages for categorical variables
                    all_categorical() ~ "{n} ({p}%)"),  
                # Specify number of digits to display
                digits = list(                    
                    # Show 1 decimal place for continuous variables
                    all_continuous() ~ 1,         
                    # Show 0 digits for counts, 1 decimal place for percentages
                    all_categorical() ~ c(0, 1))) %>%  
    # Add p-values to the table
    add_p(                                        
        # Use t-test for continuous variables
        test = list(all_continuous() ~ "t.test",  
                    # Use chi-square test for categorical variables
                    all_categorical() ~ "chisq.test"),  
        # Format p-values to 3 decimal places
        pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>%  
    # Format p-values, avoiding scientific notation
    modify_fmt_fun(p.value ~ function(x) ifelse(is.na(x),  
                                                NA,       
                                                format(x, scientific = FALSE))) %>%
    # Customize the header label to show 'Variable' in bold
    modify_header(label ~ "**Variable**") %>%     
    # Add a footnote to all statistic columns
    modify_footnote(                              
        all_stat_cols() ~ "Mean or Frequency (%)") %>%
    # Add confidence intervals for specified variables
    add_ci(                                       
        # Format CI for 'age' with 1 decimal place
        style_fun = list("age" ~ style_number_1digits,           
                         # Format CI for 'ga_inclusion_wk' with 1 decimal
                         "ga_inclusion_wk" ~ style_number_1digits,  
                         # Format CI for 'MUAC_I' with 1 decimal place
                         "MUAC_I" ~ style_number_1digits)) %>%      
    # Add a caption in bold
    modify_caption("**Table 1. Baseline and demographic characteristics between participants who never ate soil and participants who ate soil at any time-point during pregnancy**") %>%  
    # Bold the labels in the table
    bold_labels() %>%                              
    # Add an overall summary column
    add_overall() %>%                              
    # Convert the table to a 'gt' table format for rendering
    as_gt()                                        
```

### Confidence intervals for continuous variables

To resolve the issue with the code for table 1 not showing confidence intervals for continuous variables, this code recreates the confidence intervals:

```{r}
# Select continuous variables from table 1 subset
Tbl1Cont_DietRecallparticipants <- tbl1_subset  %>%
    select("soileating_ever",
           "age",
           "ga_inclusion_wk",
           "inclusion_weight",
           "BMI_I",
           "weight_W",
           "bmi_W",
           "MUAC_I")

tbl1CI <- 
    Tbl1Cont_DietRecallparticipants %>%
    # Generate a summary table, calculating the mean for all continuous variables
    tbl_summary(statistic = all_continuous() ~ "{mean}") %>%
    # Modify the header to include the overall sample size (N)
    modify_header(stat_0 = "**Overall** N = {N}") %>%
    # Add confidence intervals for continuous variables
    add_ci(
        # Apply custom number formatting to 1 decimal place for continuous variables
        style_fun = list(all_continuous() ~ style_number_1digits)
    )
```

This code creates another summary table for continuous variables, grouped by whether or not participants ate soil at any time-point during pregnancy (soileating_ever):

```{r}
tbl2CI <-
    Tbl1Cont_DietRecallparticipants %>%
    # Calculate the mean within each group of participants who either ate or not did not eat soil at any time-point during pregnancy
    tbl_summary(by = soileating_ever,
                statistic = all_continuous() ~ "{mean}") |>
    # Add confidence intervals with custom number formatting to 1 decimal place
    add_ci(style_fun = list(all_continuous() ~ style_number_1digits))

```

This code merges the tables, without spanning columns:

```{r}
tbl_merge(list(tbl1CI, tbl2CI), tab_spanner = FALSE) %>%
    as_kable() # Convert the merged table to a kable format for better presentation
```
