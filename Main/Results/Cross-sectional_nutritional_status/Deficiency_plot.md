### Preparation first trimester deficiencies

This code reshapes the dataset from wide to long format, so that the number of deficiency types (B12, folate, iron, anemia) for each `soileating` group can be analyzed. It also counts the number of deficient individuals in each group and converts `soileating` to a factor for further analysis:

```{r}
t1analysis_long <- T1Analysis %>%
    # Select relevant columns from the dataset for analysis
    select(
        soileating,        # Soil eating behavior
        B12Deficient,      # Vitamin B12 deficiency status
        FolDeficient,      # Folate deficiency status
        IronDeficient,     # Iron deficiency status
        Anaemia            # Anemia status
    ) %>%
    # Reshape the data from wide to long format
    pivot_longer(
         # Columns to be reshaped
        cols = c(B12Deficient, FolDeficient, IronDeficient, Anaemia),
        # New column for the deficiency type (e.g., B12, Folate, Iron, Anemia)
        names_to = "deficiency_type", 
        # New column for deficiency status (binary: deficient or not)
        values_to = "deficient"        
    ) %>%
    # Remove rows with missing values (NA)
    na.omit() %>%
    # Group the data by 'soileating' and 'deficiency_type' to perform grouped operations
    group_by(soileating, deficiency_type) %>%
    # Add a count column ('number_deficient') for the number of deficient individuals within each 'soileating' group. Weight the count by the 'deficient' variable to only count cases marked as deficient.
    add_count(soileating, wt = deficient, name = "number_deficient") %>%
    # Remove the grouping
    ungroup() %>%
    # Convert 'soileating' into a factor variable for categorical analysis
    mutate(
        soileating = as.factor(soileating)
    )
```

This code filters the data to separate those who ate soil (`soileating == 1`) and those who did not (`soileating == 0`). It then creates separate data frames with the counts of each deficiency type for both groups, adding a new column (`soileating`) to label the groups in the resulting data frames:

```{r}
# Filter the long-format dataset to include only rows where 'soileating' is equal to 1 (those who ate soil)
t1analysis_deficiency_soil <- t1analysis_long %>%
    filter(soileating == 1)

# Create a data frame of counts for each 'deficiency_type' among those who ate soil
df_t1_deficient_soil <- as.data.frame(
    table(t1analysis_deficiency_soil$deficiency_type))

# Add a new column to label this data as belonging to the 'soileating' group (value = "1")
df_t1_deficient_soil$soileating <- "1"

```

```{r}
# Filter the long-format dataset to include only rows where 'soileating' is equal to 0 (those who did not eat soil)
t1analysis_deficiency_no <- t1analysis_long %>%
    filter(soileating == 0)

# Create a data frame of counts for each 'deficiency_type' among those who did not eat soil
df_t1_deficient_no <- as.data.frame(
    table(t1analysis_deficiency_no$deficiency_type))

# Add a new column to label this data as belonging to the 'soileating' group (value = "0")
df_t1_deficient_no$soileating <- "0"
```

This code first combines the total counts of deficiency types for both soil-eating and non-soil-eating groups into a single data frame. Then, it merges this with the long-format dataset to calculate the proportion of deficient individuals in each group. Lastly, it adds a `trimester` column labeled "First" to indicate that this data pertains to the first trimester:

```{r}
# Combine the two data frames for soil-eating (df_t1_deficient_soil) and non-soil-eating (df_t1_deficient_no) groups into a single data frame (t1_totals_deficient)
t1_totals_deficient <- rbind(df_t1_deficient_soil, 
                             df_t1_deficient_no)

# Rename the columns of the combined data frame to 'deficiency_type' (type of deficiency), 'total_type' (total count per deficiency type), and 'soileating' (soil-eating group)
colnames(t1_totals_deficient) <- c("deficiency_type", 
                                   "total_type", "soileating")

# Join the long-format dataset (t1analysis_long) with the combined totals data (t1_totals_deficient). This merges the total count for each deficiency type and soil-eating group into the long-format dataset
t1_proportion_deficient_df <- left_join(
    t1analysis_long, 
    t1_totals_deficient, 
    by = c("soileating", "deficiency_type")) %>%
    # Calculate the proportion of deficient individuals for each group (number_deficient / total_type) and multiply by 100
    mutate(proportion = (number_deficient / total_type) * 100,
           # Add a new column 'trimester' and label it as "First" for the first trimester
           trimester = "First")
```

### Preparation second trimester deficiencies

This code reshapes the dataset from wide to long format, so that the number of deficiency types (B12, folate, iron, anemia) for each `soileating` group can be analyzed. It also counts the number of deficient individuals in each group and converts `soileating` to a factor for further analysis. The commands are the same as for 'Preparation first trimester deficiencies':

```{r}
t2analysis_long <- T2Analysis %>%
    select(soileating,
           B12Deficient,
           FolDeficient,
           IronDeficient,
           Anaemia) %>%
    pivot_longer(
        cols = c(B12Deficient, FolDeficient, IronDeficient, Anaemia),
        names_to = "deficiency_type",
        values_to = "deficient"
    ) %>%
    na.omit() %>%
    group_by(soileating, deficiency_type) %>%
    add_count(soileating, wt = deficient, name = "number_deficient") %>%
    ungroup() %>%
    mutate(
        soileating = as.factor(soileating)
    )
```

This code filters the data to separate those who ate soil (`soileating == 1`) and those who did not (`soileating == 0`). It then creates separate data frames with the counts of each deficiency type for both groups, adding a new column (`soileating`) to label the groups in the resulting data frames. The commands are the same as for 'Preparation first trimester deficiencies':

```{r}
t2_deficiency_soil <- t2analysis_long %>%
    filter(soileating==1)

df_t2_deficient_soil <- as.data.frame(
    table(t2_deficiency_soil$deficiency_type))

df_t2_deficient_soil$soileating <- "1"
```

```{r}
t2_deficiency_no <- t2analysis_long %>%
    filter(soileating==0)

df_t2_deficient_no <- as.data.frame(
    table(t2_deficiency_no$deficiency_type))

df_t2_deficient_no$soileating <- "0"

```

This code first combines the total counts of deficiency types for both soil-eating and non-soil-eating groups into a single data frame. Then, it merges this with the long-format dataset to calculate the proportion of deficient individuals in each group. Lastly, it adds a `trimester` column labeled "Second" to indicate that this data pertains to the second trimester. The commands are the same as for 'Preparation first trimester deficiencies':

```{r}
t2_totals_deficient <- rbind(df_t2_deficient_soil,
                             df_t2_deficient_no )

colnames(t2_totals_deficient) <- c("deficiency_type", 
                                   "total_type", "soileating")

t2_proportion_deficient_df <-left_join(
    t2analysis_long,
    t2_totals_deficient, 
    by = c("soileating", "deficiency_type")) %>%
    mutate(proportion = (number_deficient / total_type) * 100,
           trimester = "Second")
```

### Preparation third trimester deficiencies

This code reshapes the dataset from wide to long format, so that the number of deficiency types (B12, folate, iron, anemia) for each `soileating` group can be analyzed. It also counts the number of deficient individuals in each group and converts `soileating` to a factor for further analysis. The commands are the same as for 'Preparation first trimester deficiencies':

```{r}
t3analysis_long <- T3Analysis %>%
    select(soileating,
           B12Deficient,
           FolDeficient,
           IronDeficient,
           Anaemia) %>%
    pivot_longer(
        cols = c(B12Deficient, FolDeficient, IronDeficient, Anaemia),
        names_to = "deficiency_type",
        values_to = "deficient"
    ) %>%
    na.omit() %>%
    group_by(soileating, deficiency_type) %>%
    add_count(soileating, wt = deficient, name = "number_deficient") %>%
    ungroup() %>%
    mutate(
        soileating = as.factor(soileating)
    )
```

This code filters the data to separate those who ate soil (`soileating == 1`) and those who did not (`soileating == 0`). It then creates separate data frames with the counts of each deficiency type for both groups, adding a new column (`soileating`) to label the groups in the resulting data frames. The commands are the same as for 'Preparation first trimester deficiencies':

```{r}
t3_deficiency_soil <- t3analysis_long %>%
    filter(soileating==1)

df_t3_deficient_soil <- as.data.frame(
    table(t3_deficiency_soil$deficiency_type))

df_t3_deficient_soil$soileating <- "1"
```

```{r}
t3_deficiency_no <- t3analysis_long %>%
    filter(soileating==0)

df_t3_deficient_no <- as.data.frame(
    table(t3_deficiency_no$deficiency_type))

df_t3_deficient_no$soileating <- "0"
```

This code first combines the total counts of deficiency types for both soil-eating and non-soil-eating groups into a single data frame. Then, it merges this with the long-format dataset to calculate the proportion of deficient individuals in each group. Lastly, it adds a `trimester` column labeled "Third" to indicate that this data pertains to the third trimester. The commands are the same as for 'Preparation first trimester deficiencies':

```{r}
t3_totals_deficient <- rbind(df_t3_deficient_soil,
                             df_t3_deficient_no )

colnames(t3_totals_deficient) <- c("deficiency_type", 
                                   "total_type", "soileating")

t3_proportion_deficient_df <-left_join(
    t3analysis_long,
    t3_totals_deficient, 
    by = c("soileating", "deficiency_type") ) %>%
    mutate(proportion = (number_deficient / total_type) * 100,
           trimester = "Third")

```

### Combined plot of deficiencies

This code combines the proportion data frames for the first, second, and third trimesters into one data frame and then creates a bar plot to show the proportion of deficiencies by soil-eating behavior, grouped by trimester:

```{r}
comb_trim_deficiency <- rbind(
    t1_proportion_deficient_df, # First trimester data
    t2_proportion_deficient_df, # Second trimester data
    t3_proportion_deficient_df)  # Third trimester data

# Create bar plot 
deficiency_plot <- ggplot(data = comb_trim_deficiency,  
                         aes(# Deficiency type on the x-axis and proportion of deficiencies on the y-axis. Fill the bars based on 'soileating' group.
                             x = deficiency_type,            
                             y = proportion,                 
                             fill = as.factor(soileating)
                             )) +
    # Add bars to the plot, with 'stat = "identity"' to represent the raw values of the 'proportion'. 'position_dodge()' is used to place bars for different groups side by side
    geom_bar(stat = "identity", position = position_dodge()) +
    # Apply a classic theme with minimal grid lines and background
    theme_classic() +
    # Customize the y-axis scale to range from 0 to 100, with ticks every 10 units, and remove space between the axis and the plot elements
    scale_y_continuous(
        limits = c(0, 100),  
        breaks = seq(0, 100, by = 10),  
        expand = c(0, 0)  
    ) +
    # Customize the x-axis labels by first putting them in the correct order and then shortening them for better readability.
    scale_x_discrete(
        limits = c("B12Deficient", 
                   "FolDeficient", 
                   "IronDeficient", 
                   "Anaemia"),  
        labels = c("B12", "Folate", "Iron", "Anaemia")) +
    # Manually set the fill colors for the bars: purple for non-soil eaters ('0'), orange for soil eaters ('1')
    scale_fill_manual(values = c("0" = "purple", "1" = "#FF8000")) +
    # Create separate panels (facets) for each trimester, placing them in different columns
    facet_grid(cols = vars(trimester)) +
    # Customize the plot's appearance
    theme(
        # Remove the legend from the plot
        legend.position = "none",  
        # Remove the x-axis title
        axis.title.x = element_blank(), 
        # Set y-axis label font size
        axis.text.y = element_text(size = 11),
        # Set x-axis label font size
        axis.text.x = element_text(size = 11),  
        # Set y-axis title font size
        axis.title.y = element_text(size = 12),  
        # Set axis label text color to black
        axis.text = element_text(color = "black"),  
        # Set the font size of facet labels (trimester labels)
        strip.text = element_text(size = 12),  
        # Add a grey border around facet labels
        strip.background = element_rect(color="grey"),  
        # Add a grey border around the panels
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5)  
    ) +
    # Add a y-axis label for the plot
    labs(y = "Proportion (%)")
```
