## Cross-sectional nutritional status parameter analyses

For each trimester, create a summary table comparing nutritional status parameters ibetween participants who ate or did not ate soil in that specific trimester, including means, confidence intervals and p-values.

### First trimester analysis

This code filters and processes data for participants in the first trimester, creates a summary table comparing nutritional status parameters based on geophagia in first trimester, and adds statistics, p-values, and confidence intervals:

```{r}
tbl_t1_analysis <- ponadb  %>%
    # Filter the dataset to include only records from the first trimester (visit_trimester == 1) and where a dietary recall (Done_24h == 1) was completed
    filter(visit_trimester == 1,
           Done_24h == 1) %>%
    # Arrange the rows in descending order based on the 'soileating' variable (those who ate soil will appear first)
    arrange(desc(soileating)) %>%
    # Remove duplicate rows based on 'ID', keeping only the first occurrence of each ID so that there is a preference for keeping dietary recalls where geophagia was reported.
    distinct(ID, .keep_all = TRUE) %>%
    # Select the relevant columns for the analysis table
    select(
        # Whether the individual ate soil or not at the particular ANC visit
        soileating, 
         # Mid-upper arm circumference measurement
        New_MUAC,
        # Vitamin B12 serum concentration at the visit or in the same trimester
        New_B12, 
        # Vitamin B12 deficiency status (binary) based on 'New_B12'
        B12Deficient, 
        # Serum folate concentration at the visit or in the same trimester
        New_Fol, 
         # Folate deficiency status (binary)
        FolDeficient,
        # Corrected ferritin concentration at the visit or in the same trimester
        CorrNew_Fer, 
        # Log-transformed corrected ferritin concentration
        CorrNew_logFer, 
        # Iron deficiency status (binary) based on 'CorrNew_Fer'
        IronDeficient, 
        # Haemoglobin concentration at the visit or in the same trimester
        New_HB, 
        # Anemia status (binary) based on 'New_HB' 
        Anaemia 
    ) %>%
    # Create a summary table, grouping by the 'soileating' variable
    tbl_summary(
        by = soileating,  # Grouping variable is 'soileating'
        statistic = list(
            # For categorical variables, show count and percentage
            all_categorical() ~ "{n} ({p}%)", 
            # For continuous variables, show the mean
            all_continuous() ~ "{mean}"        
        ),
        missing = "no",   # Do not display missing data
        digits = list(all_continuous() ~ c(1, 1))  # Set decimal places for continuous variables
    ) %>%
    # Add p-values to test for differences between groups
    add_p(
        test = list(
            # Use chi-square test for categorical variables
            all_categorical() ~ "chisq.test",  
            # Use t-test for continuous variables
            all_continuous() ~ "t.test"        
        )
    ) %>%
    # Modify p-value formatting to show 3 decimal places
    modify_fmt_fun(p.value ~ function(x) style_pvalue(x, digits = 3)) %>%
    # Add confidence intervals for continuous variables and format to 1 decimal place
    add_ci(
        style_fun = all_continuous() ~ style_number_1digits,  
        pattern = "{stat} [{ci}]"  # Show the mean and confidence interval as "mean [CI]"
    ) %>%
    # Add the number of observations (n) for each group
    add_n() %>%
    # Modify the header for the 'label' column, labeling it as "Variable"
    modify_header(label ~ "**Variable**") %>%
    # Add footnotes explaining the statistics displayed in the table
    modify_footnote(
        all_stat_cols() ~ "Mean [95%CI] for measurements of nutritional status parameters; n (%) for nutritional status diagnoses"
    ) %>%
    # Add a caption to the table describing its content
    modify_caption("**Association between GiP and nutritional status in first trimester**") %>%
    # Bold the labels (i.e., variable names) in the table
    bold_labels() %>%
    # Convert the table to a 'gt' (grammar of tables) object for display
    as_gt()


```

### Second trimester analysis

This code filters and processes data for participants in the second trimester, creates a summary table comparing nutritional status parameters based on geophagia in the second trimester, and adds statistics, p-values, and confidence intervals. Refer to 'First trimester analysis' code chunk for variable definitions and explanations of commands:

```{r}
tbl_t2_analysis <- ponadb  %>%
    # Filter the dataset to include only records from the second trimester (visit_trimester == 2) and where a dietary recall (Done_24h == 1) was completed
    filter(visit_trimester == 2,
           Done_24h == 1) %>%
    arrange(desc(soileating)) %>%
    distinct(ID, .keep_all = TRUE) %>%
    select(soileating,
           New_MUAC,
           New_B12,
           B12Deficient,
           New_Fol,
           FolDeficient,
           CorrNew_Fer,
           CorrNew_logFer,
           IronDeficient,
           New_HB,
           Anaemia) %>%
    tbl_summary(
        by = soileating,
        statistic = list(
            all_categorical() ~ "{n} ({p}%)",
            all_continuous() ~ "{mean}"),
        missing = "no",
        digits = list(all_continuous() ~ c(1, 1))) %>%
    add_p(test = list(all_categorical() ~ "chisq.test",
                      all_continuous() ~ "t.test")) %>%
    modify_fmt_fun(p.value ~ function(x) style_pvalue(x, digits = 3)) %>%
    add_ci(style_fun = all_continuous() ~ style_number_1digits,
           pattern = "{stat} [{ci}]") %>%
    add_n() %>%
    modify_header(label ~ "**Variable**") %>%
    modify_footnote(all_stat_cols() ~ "Mean [95%CI] for measurements of nutritional status parameters; n (%) for nutritional status diagnoses") %>%
    modify_caption("**Association between GiP and nutritional status in second trimester**") %>%
    bold_labels() %>%
    as_gt()
```

### Third trimester analysis

This code filters and processes data for participants in the third trimester, creates a summary table comparing nutritional status parameters based on geophagia in the third trimester, and adds statistics, p-values, and confidence intervals. Refer to the 'First trimester analysis' code chunk for variable definitions and explanations of commands:

```{r}
tbl_t3_analysis <- ponadb  %>%
     # Filter the dataset to include only records from the third trimester (visit_trimester == 3) and where a dietary recall (Done_24h == 1) was completed
    filter(visit_trimester == 3,
           Done_24h == 1) %>%
    arrange(desc(soileating)) %>%
    distinct(ID, .keep_all = TRUE) %>%
    select(soileating,
           New_MUAC,
           New_B12,
           B12Deficient,
           New_Fol,
           FolDeficient,
           CorrNew_Fer,
           CorrNew_logFer,
           IronDeficient,
           New_HB,
           Anaemia) %>%
    tbl_summary(
        by = soileating,
        statistic = list(
            all_categorical() ~ "{n} ({p}%)",
            all_continuous() ~ "{mean}"),
        missing = "no",
        digits = list(all_continuous() ~ c(1, 1))) %>%
    add_p(test = list(all_categorical() ~ "chisq.test",
                      all_continuous() ~ "t.test")) %>%
    modify_fmt_fun(p.value ~ function(x) style_pvalue(x, digits = 3)) %>%
    add_ci(style_fun = all_continuous() ~ style_number_1digits,
           pattern = "{stat} [{ci}]") %>%
    add_n() %>%
    modify_header(label ~ "**Variable**") %>%
    modify_footnote(all_stat_cols() ~ "Mean [95%CI] for measurements of nutritional status parameters; n (%) for nutritional status diagnoses") %>%
    modify_caption("**Association between GiP and nutritional status in third trimester**") %>%
    bold_labels() %>%
    as_gt()
```
