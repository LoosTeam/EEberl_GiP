# Pre-pregnancy nutritional status parameter analysis

This code creates a subset of the data that only includes participants who completed a dietary recall during pregnancy and also had measurements of MUAC, BMI, vitamin B12, ferritin, folate, or haemoglobin before pregnancy:

```{r}
subset_prepreg_participants <- ponadb %>%
    filter((
        #Value for HB measured at pre-pregnancy
        !is.na(HB_prepreg) | 
            #value for mid-upper arm circumference measured at pre-pregnancy
            !is.na(muac_W) |
            #value for CRP measured at pre-pregnancy
            !is.na(CRP_new_W) |
            #value for BMI measured and calculated at pre-pregnancy
            !is.na(bmi_W)|
            #value for serum B12 measured at pre-pregnancy
            !is.na(B12_W) |
            #value for serum ferritin measured at pre-pregnancy
            !is.na(Ferritin_W) |
            #value for serum folate measured at pre-pregnancy
            !is.na(Folin_W)) &
            ID %in% dietrecall_IDs) %>%
    distinct(ID, .keep_all = TRUE)
```

## Table of nutritional status parameters at pre-pregnancy among participants with or without GiP

This code creates a summary table comparing pre-pregnancy nutritional status parameters between participants who ate or never ate soil during pregnancy, including means, confidence intervals and p-values:

```{r}
tbl3_prepreg <- subset_prepreg_participants  %>%
    # Select the relevant columns from the dataset for the table
    select(
        # Whether the participant ever ate soil during pregnancy
        "soileating_ever", 
        "bmi_W", 
        "muac_W",
        "B12_W",  
        # Pre-pregnancy Vitamin B12 deficiency status based on 'B12_W'and WHO definition
        "Prepreg_B12Deficient",    
        "Folin_W",    
        # Pre-pregnancy Folate deficiency status based on 'Folin_W' and WHO definition
        "Prepreg_FolDeficient",  
        # Corrected ferritin concentration at pre-pregnancy - based on 'Ferritin_W' and 'CRP_new_W'
        "CorrFer_W", 
        # Pre-pregnancy Iron deficiency status based on 'CorrFer_W' and WHO definition 
        "Prepreg_FeDeficient",
        # Log-transformed corrected serum ferritin concentration
        "logCorrFer_W",            
        "HB_prepreg",  
        # Pre-pregnancy anemia status based on HB_prepreg and WHO definition
        "Prepreg_Anaemia"          
    ) %>%
    # Create a summary table, grouping by the 'soileating_ever' variable
    tbl_summary(by = soileating_ever,
                # Do not include missing data 
                missing = "no",    
                statistic = list(
                    # Use mean for continuous variables
                    all_continuous() ~ "{mean}", 
                    # Use count and percentage for categorical variables
                    all_categorical() ~ "{n} ({p}%)"  
                ),
                # Set decimal places for continuous variables
                digits = list(all_continuous() ~ c(1, 1))  
    ) %>%
    # Add p-values for testing differences
    add_p(
        test = list(
            # Use t-test for continuous variables
            all_continuous() ~ "t.test", 
            # Use chi-square test for categorical variables
            all_categorical() ~ "chisq.test"  
        )
    ) %>%
    # Modify p-value formatting to 3 decimal places
    modify_fmt_fun(p.value ~ function(x) style_pvalue(x, digits = 3)) %>%
    # Add confidence intervals for continuous variables and format CI to 1 decimal place
    add_ci(
        style_fun = all_continuous() ~ style_number_1digits,  
        pattern = "{stat} [{ci}]"  # Display mean and CI as "mean [CI]"
    ) %>%
    # Add the number of observations (n) for each group
    add_n() %>%
    # Modify the header for the variable column
    modify_header(label ~ "**Variable**") %>%
    # Add footnotes explaining the statistics displayed in the table
    modify_footnote(
        all_stat_cols() ~ "Mean [95%CI] for measurements of nutritional status parameters; n (%) for nutritional diagnoses"
    ) %>%
    # Add a caption to describe the table
    modify_caption("**Table 3. Pre-pregnancy characteristics between participants who never ate soil and participants who ate soil at any time-point during pregnancy**") %>%
    # Bold the labels (variable names)
    bold_labels() %>%
    # Convert the table to a gt (grammar of tables) object for display
    as_gt()
```
