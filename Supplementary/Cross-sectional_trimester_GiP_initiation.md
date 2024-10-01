# Table comparing nutritional status in the first trimester between initiated and not initated GiP groups

This code filters the data for participants in the subset used for the logistic regression change analysis (Change_IDs) and creates a summary table comparing nutritional status parameters in the first trimester between participants who did anddid not intiate geophagia in the third trimester. The table includes statistics, p-values, and confidence intervals:

```{r}
# Select relevant columns for analysis
Tbl_T1Analysis <- ChangeAnalysis  %>%
    select(
        ID, 
        visittype,           # Type of ANC visit 
        visitnb,             # Visit number
        visit_trimester,     # Trimester during which the visit occurred
        Done_24h,            # Completion of dietary recall
        soileating,          # Reporting of geophagia at that visit (binary)
        soileating_t3,       # Reporting of geophagia in the third trimester (T3)
        T1_MUAC,             # Mid-upper arm circumference in first trimester (T1)
        T1_B12,              # Vitamin B12 levels during T1
        B12Deficient_T1,     # Vitamin B12 deficiency during T1 (binary)
        T1_Fol,              # Folate levels during T1 
        FolDeficient_T1,     # Folate deficiency during T1 (binary)
        T1_CorrFer,          # Corrected ferritin (iron) levels during T1
        IronDeficient_T1,    # Iron deficiency during T1 (binary)
        T1_HB,               # Hemoglobin levels during T1 
        anaemia_t1,          # Indicator for anemia during T1 (binary)
        ) %>%
    # Arrange the data in descending order based on soil-eating
    arrange(desc(soileating)) %>%
    # Remove duplicates of ID, retaining all other information
    distinct(ID, .keep_all = TRUE) %>%
    # Filter the data to include only IDs that are present in Change_IDs
    filter(ID %in% Change_IDs) %>%
    # Select variables to include in analysis 
    select(soileating,
           T1_MUAC,
           T1_B12,
           B12Deficient_T1,
           T1_Fol,
           FolDeficient_T1,
           T1_CorrFer,
           IronDeficient_T1,
           T1_HB,
           anaemia_t1
    ) %>%
    # Create a new variable by taking the log of corrected ferritin levels
        mutate(
        logT1_CorrFer = log(T1_CorrFer)
    ) %>%
    # Generate a summary table grouped by soil-eating behavior
    tbl_summary(
        by = soileating,      
        statistic = list(
            all_categorical() ~ "{n} ({p}%)",  
            all_continuous() ~ "{mean}"        
        ),
        missing = "no",       
        digits = list(all_continuous() ~ c(1, 1))  
    ) %>%
    # Add p-values to the summary using the chi-squared test for categorical and t-test for continuous variables
    add_p(test = list(
        all_categorical() ~ "chisq.test",  
        all_continuous() ~ "t.test"        
    )) %>%
    # Format the p-values in a non-scientific notation
    modify_fmt_fun(p.value ~ function(x)
        ifelse(is.na(x), NA, format(x, scientific = FALSE))
    ) %>%
    # Add the total number of observations (N) to the summary table
    add_n() %>%
    # Add CI for continuous variables, formatted to 1 decimal place
    add_ci(style_fun = all_continuous() ~ style_number_1digits,
           pattern = "{stat} [{ci}]") %>%
    # Modify the header to rename the label column to "Variable"
    modify_header(label ~ "**Variable**") %>%
    # Add a footnote to explain the statistics
    modify_footnote(all_stat_cols() ~ "Mean [95%CI] for measurements of nutritional status parameters; n (%) for nutritional status diagnoses") %>%
    # Bold the labels in the table for better readability
    bold_labels() %>%
    # Convert the summary table to a GT (graphic table) format for rendering in reports or markdowns
    as_gt()

```
