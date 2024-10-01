# About the dataset

In the dataset from the FOETALforNCD pregnancy study (PONA), each row is an antenatal care (ANC) visit with multiple ANC visits for each participant. Each visit is categorised as either a study enrollment visit, scheduled ANC visit, unscheduled ANC visit, or delivery visit. Each participant is assigned with a unique ID number and data collected at each visit are stored as separate variables.

# Data preparation

## Data subsets

### IDs of dietary recall participants

This code creates a list of IDs of the participants that completed at least one dietary recall during pregnancy:

```{r}
dietrecall_IDs <- ponadb %>%    # Start with the 'ponadb' data frame
    filter(Done_24h == 1) %>%   # Filter for visits with completed dietary recalls
    distinct(ID) %>%            # Remove repetititons of 'ID' values
    pull(ID)                    # Extract 'ID' as a vector
```

### Subset for baseline and demographic analysis

The following code creates a subset of the data for analysis comparing those with and without geophagia at any time-point during pregnancy. This subset includes participants that completed at least one dietary recall during pregnancy:

```{r}
tbl1_subset <- ponadb %>%          
    filter(ID %in% dietrecall_IDs) %>%          
    distinct(ID, .keep_all = TRUE)
```

### Subsets for cross-sectional analyses

The following code chunks create trimester-specific subsets of the data for the cross-sectional analyses investigating the association between geophagia and nutritional status parameters in each trimester.

#### First trimester

```{r}
# Filter for ANC visits that occured in the first trimester and where the participant completed a dietary recall.
T1Analysis <- ponadb %>%               
    filter(visit_trimester == 1,       
           Done_24h == 1) %>%  
    # Arrange rows with geophagia "yes" occuring first
    arrange(desc(soileating)) %>% 
    # Remove subsequent rows of the same ID, retaining all columns
    distinct(ID, .keep_all = TRUE)     


```

#### Second trimester

```{r}
# Filter for ANC visits that occured in the second trimester and where the participant completed a dietary recall.
T2Analysis <- ponadb %>%
    filter(visit_trimester == 2,
           Done_24h == 1) %>%
    # Arrange rows with geophagia "yes" occuring first
    arrange(desc(soileating)) %>% 
    # Remove subsequent rows of the same ID, retaining all columns
    distinct(ID, .keep_all = TRUE)
```

#### Third trimester

```{r}
# Filter for ANC visits that occured in the third trimester and where the participant completed a dietary recall. 
T3Analysis <- ponadb %>%
    filter(visit_trimester == 3,
           Done_24h == 1) %>%
    # Arrange rows with geophagia "yes" occuring first
    arrange(desc(soileating)) %>% 
    # Remove subsequent rows of the same ID, retaining all columns
    distinct(ID, .keep_all = TRUE)
```

### Subset of participants who ate soil at any time-point (GiP)

This code creates a subset of the data including only the first visit where geophagia was reported in a specific trimester among participants that reported eating soil during pregnancy:

```{r}
gip_df <- ponadb %>%
    # Filter for visits where geophagia was reported
    filter(soileating == 1) %>%
    # Convert 'visit_trimester' to a factor variable
    mutate(visit_trimester = as.factor(visit_trimester)) %>% 
    # Remove repetitions such that there is only one visit where geophagia was reported per trimester, per participant 
    distinct(ID, visit_trimester, .keep_all = TRUE)  

```

### Subset for cross-sectional plots

This code creates a subset of the data for the cross-sectional plots:

```{r}
Xanalysis <- ponadb %>%
    # Convert visit_trimester to a factor variable and change the labels of the levels to 1 = First, 2 = Second, 3 = Third. 
    mutate(visit_trimester = factor(
        visit_trimester,
        levels = c(1, 2, 3),
        labels = c("First", "Second", "Third"))) %>%
    # Filter for visits with completed dietary recalls 
    filter(Done_24h == 1) %>%
    # Remove repetitions such that there is only one dietary recall visit per trimester, per participant, with a preference for including dietary recall visits where geophagia was reported. 
    arrange(desc(soileating)) %>%
    distinct(ID,
             visit_trimester,
             .keep_all = TRUE)
```

### IDs of participants for logistic regression analyses

The following code creates a list of participant IDs to include in the logistic regression analyses investigating the association between changes in nutritional status parameters and the initiation of geophagia in third trimester:

```{r}
Change_IDs <- ponadb %>%
    # Replace NA values with 0 ("No") for the reporting of geophagia in first and second trimester
    mutate(
        soileating_t1 = coalesce(soileating_t1, 0),
        soileating_t2 = coalesce(soileating_t2, 0)) %>%
    #Filter for participants that completed a dietary recall in first, second and third trimester and did not report geophagia not reported in the first and second trimester 
    filter(
        soileating_t2==0,
        soileating_t1==0,
        dietrecall_t3==1,
        dietrecall_t2==1,
        dietrecall_t1==1) %>%
    #Remove repetitions of IDs and extract the 'ID' column as a vector
    distinct(ID) %>%
    pull(ID)
```

### Subset for multivariable logistic regression analyses and creation of change variables

This code creates variables for the differences in nutritional status parameters from first to second trimester, first to third trimester, and second to third trimester:

```{r}
GLMAnalysis <- ponadb %>%
    # Arrange in ascending order of gestational age at visit with each participant's group of visits
    arrange(ID, GA_visit) %>%
    # Create variables for the change from 1st to 2nd trimester in serum concentrations and MUAC 
    mutate(
        ChangeT1toT2_B12Serum = T2_B12-T1_B12,
        ChangeT1toT2_CorrFer = T2_CorrFer - T1_CorrFer,
        ChangeT1toT2_Fol = T2_Fol-T1_Fol,
        ChangeT1toT2_HB = T2_HB-T1_HB,
        ChangeT1toT2_MUAC = T2_MUAC - T1_MUAC) %>%
    # Create variables for the change from 1st to 3rd trimester in serum concentrations and MUAC 
    mutate(
        ChangeT1_B12Serum = T3_B12-T1_B12,
        ChangeT1_FerSerum = T3_Fer-T1_Fer,
        ChangeT1_CorrFer = T3_CorrFer - T1_CorrFer,
        ChangeT1_FolSerum = T3_Fol-T1_Fol,
        ChangeT1_HB = T3_HB-T1_HB,
        ChangeT1_MUAC = T3_MUAC-T1_MUAC) %>%
    # Create variables for the change from 2nd to 3rd trimester in serum concentrations and MUAC  
    mutate(
        ChangeT2_B12Serum = T3_B12-T2_B12,
        ChangeT2_FerSerum = T3_Fer-T2_Fer,
        ChangeT2_CorrFer = T3_CorrFer - T2_CorrFer,
        ChangeT2_FolSerum = T3_Fol-T2_Fol,
        ChangeT2_HB = T3_HB-T2_HB,
        ChangeT2_MUAC = T3_MUAC - T2_MUAC) %>%
    # Scale 1st to 2nd and 1st to 3rd trimester change variables for logistic regression analysis 
    mutate(
        ChangeT1_50B12 = ChangeT1_B12Serum/50,
        ChangeT1_10CorrFer = ChangeT1_CorrFer/10,
        ChangeT1toT2_50B12 = ChangeT1toT2_B12Serum/50,
        ChangeT1toT2_10CorrFer = ChangeT1toT2_CorrFer/10)
```

This code creates a subset of the data for logistic regression analysis by including only participants with IDs in Change_IDs and third trimester visits where a dietary recall was completed.

```{r}
ChangeAnalysis <- GLMAnalysis %>%
    filter(
        visit_trimester == 3,         
        Done_24h == 1,               
        ID %in% Change_IDs           
    ) %>%
    arrange(desc(soileating))         
    distinct(ID, .keep_all = TRUE)   
```

