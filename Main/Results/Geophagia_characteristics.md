# Geophagia characteristics and temporality

## Table of geophagia characteristics

This code creates a data frame to hold the characteristics summary grouped by trimester from the subset of participants with geophagia at any time during pregnancy:

```{r}
tbl2_gip_characteristics <- data.frame(
    table1(
        # Specify geophagia characteric variables to summarize and group by trimester. 
        ~ first_soiltype_trim 
        + new_soil_dailyportion 
        + new_soil_dailyfreq 
        + new_soil_dailyintake
        | visit_trimester,
        # Use data from subset of participants who reported eating soil at any timepoint during pregnancy 
        data = gip_df,
        # Do not include an overall summary (i.e. do not aggregate across groups)
        overall = F, 
        # Render continuous variables as means in the summary table
        render.continuous = c(. = "Mean")
    )
)
```

This code reproduces confidence intervals by creating a trimester-specific subset (for instance third trimester) and performing a one-sample t-test on the continuous variable of interest:

```{r}
subset_df <- gip_df %>%
    # E.g. filter to include only records from visit_trimester 3
    filter(visit_trimester == 3)

# Perform a t-test on the new soil daily intake in the subset
t.test(subset_df$new_soil_dailyintake)
```

## Overall GiP characteristics

This code creates a data frame to hold the averages for geophagia characteristics for each individual across all trimesters:

```{r}
overall_giP_ID_df <- ponadb %>%
    # Filter to include only individuals who reported eating soil (soileating == 1)
    filter(soileating == 1) %>%
    # Select relevant columns for analysis
    select(ID, 
           soil_volume_per_sitting, 
           AvgDailyFreq, 
           AvgDailySoilAmount) %>%
    # Group data by participant ID
    group_by(ID) %>%
    # Calculate the mean for all selected variables, ignoring NA values
    summarise_all(mean, na.rm = TRUE)
```

This code determines the overall averages and summary statistics:

```{r}
summary_gip_ID <- overall_giP_ID_df %>%
    # Calculate the mean for specified columns
    summarise(
        mean_soil_volume_per_sitting = mean(soil_volume_per_sitting, na.rm = TRUE),
        mean_AvgDailyFreq = mean(AvgDailyFreq, na.rm = TRUE),
        mean_AvgDailySoilAmount = mean(AvgDailySoilAmount, na.rm = TRUE))

#Determine min and max of average daily soil intake. 
summary(overall_giP_ID_df$AvgDailySoilAmount)
```

## Plot (UpSet) of GiP occurence and initiation

This code creates a subset of the data to only include participants that reported eating soil at anytime during pregnancy and completed a dietary recall in all three trimesters:

```{r}
subset_GiP_trim <- ponadb %>%
    filter(soileating_ever == 1,
           dietrecall_t1==1,
           dietrecall_t2==1,
           dietrecall_t3==1) %>%
    # Remove repetitions such that there is only one row per participant. 
    distinct(ID, .keep_all = TRUE) %>%
    #Edit the variable names of which trimester geophagia was reported for the subsequent UpSet plot
    mutate(
        First = as.numeric(as.character(soileating_t1)),
        Second = as.numeric(as.character(soileating_t2)),
        Third = as.numeric(as.character(soileating_t3))) %>%
    as.data.frame()
```

This code creates the UpSet plot showing which trimester(s) geophagia was reported among women who completed a dietary recall in all three trimesters:

```{r}
upset(subset_GiP_trim,
      sets = c("First" ,"Second", "Third"),
      mb.ratio = c(0.7, 0.3) ,
      text.scale = c(1.5, 1.5, 1.3, 1.3, 1.9, 2.1),
      order.by = "degree")
```
