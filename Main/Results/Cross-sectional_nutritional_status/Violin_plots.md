### MUAC plot

To plot the mean and confidence intervals, this code filters out missing values for `New_MUAC` and then calculates summary statistics (mean, standard error, etc.), grouping the data by both `soileating` and `visit_trimester`:

```{r}
MUACsummary <- Xanalysis %>%
    # Filter the dataset to include only rows where 'New_MUAC' is not missing (NA)
    filter(!is.na(New_MUAC)) %>%
    # Calculate summary statistics (mean, standard error, confidence intervals, etc.), for the 'New_MUAC' variable, grouped by 'soileating' and 'visit_trimester'
    summarySE(
        # The variable for which summary statistics are calculated (New MUAC)
        measurevar = "New_MUAC",  
        # Grouping by 'soileating' (soil eating behavior) and 'visit_trimester' (trimester of visit)
        groupvars = c("soileating", "visit_trimester")  
    )

```

This code creates a violin plot of the distribution of mid-upper arm circumference (MUAC), showing the differences between those who ate soil and those who did not. It also overlays mean values with confidence intervals, and facets the plot by the trimester of the visit:

```{r}
MUAC_plot <- MUACsummary %>%
    # Create a ggplot object with 'soileating' on the x-axis and 'New_MUAC' on the y-axis. The 'fill' aesthetic is also based on 'soileating' to differentiate the groups by color
    ggplot(aes(
        # Convert 'soileating' to a factor for categorical grouping
        x = as.factor(soileating),  
        y = New_MUAC,               
        fill = as.factor(soileating))) + 
    # Add a violin plot to show the distribution of 'New_MUAC' across 'soileating' groups
    geom_violin(
        # Use the cross-sectional analysis subset for the violin plot
        data = Xanalysis, 
        mapping = aes(x = as.factor(soileating), y = New_MUAC),
        # Set the transparency level of the violin plot
        alpha = 0.25, 
        # No border color for the violin plot
        colour = NA) +  
    # Add a point range plot to show the mean and confidence interval for 'New_MUAC'
    geom_pointrange(
        aes(
            # Lower bound of the CI
            ymin = New_MUAC - ci,
            # Upper bound of the CI
            ymax = New_MUAC + ci,  
            # Colour points based on 'soileating'
            colour = factor(soileating)),  
        # Use a diamond shape for the point 
        shape = 18,  
        # Set the size of the point and error bars
        size = 0.7) +  
    # Use a minimalistic black-and-white theme for the plot
    theme_bw() +
    # Label the y-axis as 'MUAC (cm)'
    labs(y = "MUAC (cm)") +
    # Manually set the fill colors for the 'soileating' categories
    scale_fill_manual(values = c("purple", "#FF8000")) +
    # Customize the y-axis scale: set limits, breaks, and remove extra space at the edges
    scale_y_continuous(
        # Set the limits for the y-axis from 15 to 47
        limits = c(15, 47), 
        # Set breaks on the y-axis every 5 units
        breaks = seq(15, 45, by = 5),  
        # Remove padding around the y-axis limits
        expand = c(0, 0)) +  
    # Manually set the colors for the points based on 'soileating'
    scale_colour_manual(values = c("purple", "#FF8000")) +
    # Customize the plot theme for text, axes, and panel appearance
    theme(
        # Set the size of facet labels
        strip.text = element_text(size = 10),
        # Set the size of the y-axis title
        axis.title.y = element_text(size = 12), 
        # Set the size of the y-axis labels
        axis.text.y = element_text(size = 10),  
        # Remove x-axis text labels
        axis.text.x = element_blank(), 
        # Remove x-axis ticks
        axis.ticks.x = element_blank(),
        # Remove the x-axis title
        axis.title.x = element_blank(),  
        # Set the color of axis text to black
        axis.text = element_text(color = "black"), 
        # Remove the legend from the plot
        legend.position = "none",  
        # Remove the grid lines from the plot background
        panel.grid = element_blank()) +  
    # Create separate panels (facets) for each trimester
    facet_grid(~ visit_trimester)  
```

### Vitamin B12 plot

To plot the mean and confidence intervals, this code filters out missing values for `New_B12` and then calculates summary statistics (mean, standard error, etc.), grouping the data by both `soileating` and `visit_trimester`. Refer to the 'MUAC plot' code chunk for explanations of the different commands:

```{r}
B12summary <- Xanalysis %>%
    filter(!is.na(New_B12)) %>%
    summarySE(
        measurevar = "New_B12",
        groupvars = c("soileating", "visit_trimester"))
```

This code creates a violin plot of the distribution of serum vitamin B12, showing the differences between those who ate soil and those who did not. It also overlays mean values with confidence intervals, and facets the plot by the trimester of the visit. Refer to the 'MUAC plot' code chunk for explanations of the different commands:

```{r}
B12_plot <- B12summary %>%
    # Create a ggplot object with 'soileating' on the x-axis and 'New_B12' on the y-axis. The 'fill' aesthetic is also based on 'soileating' to differentiate the groups by color.
    ggplot(aes(
        x = as.factor(soileating),
        y = New_B12,
        fill = as.factor(soileating))) +
    geom_violin(
        data = Xanalysis,
        mapping = aes(x = as.factor(soileating),
                      y = New_B12),
        alpha = 0.25,
        colour = NA) +
    geom_pointrange(
        aes(
            ymin = New_B12 - ci,
            ymax = New_B12 + ci,
            colour = factor(soileating)),
        shape = 18,
        size = 0.7) +
    theme_bw() +
    labs(y = "Vitamin B12 (pmol/L)") +
    scale_fill_manual(values = c("purple", "#FF8000")) +
    scale_y_continuous(
        limits = c(0, 1500),
        breaks = seq(0, 1600, by = 200),
        expand = c(0, 0)) +
    scale_colour_manual(values = c("purple", "#FF8000")) +
    theme(
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none",
        panel.grid = element_blank()) +
    facet_grid(~ visit_trimester)

```

### Folate plot

To plot the mean and confidence intervals, this code filters out missing values for `New_Fol` and then calculates summary statistics (mean, standard error, etc.), grouping the data by both `soileating` and `visit_trimester`. Refer to the 'MUAC plot' code chunk for explanations of the different commands:

```{r}
Folsummary <- Xanalysis %>%
    filter(!is.na(New_Fol)) %>%
    summarySE(
        measurevar = "New_Fol",
        groupvars = c("soileating", "visit_trimester")
    )
```

This code creates a violin plot of the distribution of serum folate, showing the differences between those who ate soil and those who did not. It also overlays mean values with confidence intervals, and facets the plot by the trimester of the visit. Refer to the 'MUAC plot' code chunk for explanations of the different commands:

```{r}
Folate_plot <- Folsummary %>%
    # Create a ggplot object with 'soileating' on the x-axis and 'New_Fol' on the y-axis. The 'fill' aesthetic is also based on 'soileating' to differentiate the groups by color.
    ggplot(aes(
        x = as.factor(soileating),
        y = New_Fol,
        fill = as.factor(soileating)
    )) +
    geom_violin(
        data = Xanalysis,
        mapping = aes(x = as.factor(soileating),
                      y = New_Fol),
        alpha = 0.25,
        colour = NA
    ) +
    geom_pointrange(
        aes(
            ymin = New_Fol - ci,
            ymax = New_Fol + ci,
            colour = factor(soileating)
        ),
        shape = 18,
        size = 0.7
    ) +
    theme_bw() +
    labs(y = "Folate (nmol/L)") +
    scale_fill_manual(values = c("purple", "#FF8000")) +
    scale_y_continuous(
        limits = c(0, 160),
        breaks = seq(0, 150, by = 25),
        expand = c(0, 0)
    ) +
    scale_colour_manual(values = c("purple", "#FF8000")) +
    theme(
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none",
        panel.grid = element_blank()
    ) +
    facet_grid(~ visit_trimester)
```

### Log(Ferritin) plot

To plot the mean and confidence intervals, this code filters out missing values for `CorrNew_logFer` and then calculates summary statistics (mean, standard error, etc.), grouping the data by both `soileating` and `visit_trimester`. Refer to the 'MUAC plot' code chunk for explanations of the different commands:

```{r}
CorrlogFersummary <- Xanalysis %>%
    filter(!is.na(CorrNew_logFer)) %>%
    summarySE(
        measurevar = "CorrNew_logFer",
        groupvars = c("soileating", "visit_trimester")
    )
```

This code creates a violin plot of the distribution of log-transformed, corrected ferritin concentrations, showing the differences between those who ate soil and those who did not. It also overlays mean values with confidence intervals, and facets the plot by the trimester of the visit. Refer to the 'MUAC plot' code chunk for explanations of the different commands:

```{r}
CorrlogFer_plot <- CorrlogFersummary %>%
    ggplot(aes(
        x = as.factor(soileating),
        y = CorrNew_logFer,
        fill = as.factor(soileating)
    )) +
    geom_violin(
        data = Xanalysis,
        mapping = aes(x = as.factor(soileating),
                      y = CorrNew_logFer),
        alpha = 0.25,
        colour = NA
    ) +
    geom_pointrange(
        aes(
            ymin = CorrNew_logFer - ci,
            ymax = CorrNew_logFer + ci,
            colour = factor(soileating)
        ),
        shape = 18,
        size = 0.7
    ) +
    theme_bw() +
    labs(y = "log[Ferritin(ug/L)]") +
    scale_fill_manual(values = c("purple", "#FF8000"),
                      labels = c("Geophagia", "No Geophagia")) +
    scale_colour_manual(values = c("purple", "#FF8000"),
                        labels = c("Geophagia", "No Geophagia")) +
    scale_y_continuous(
        limits = c(-1, 7.5),
        breaks = seq(-1, 7, by = 1),
        expand = c(0, 0)
    ) +
    theme(
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none",
        panel.grid = element_blank()
    ) +
    facet_grid(~ visit_trimester)
```

### Haemoglobin plot

To plot the mean and confidence intervals, this code filters out missing values for `New_HB` and then calculates summary statistics (mean, standard error, etc.), grouping the data by both `soileating` and `visit_trimester`. Refer to the 'MUAC plot' code chunk for explanations of the different commands:

```{r}
HBsummary <- Xanalysis %>%
    filter(!is.na(New_HB)) %>%
    summarySE(
        measurevar = "New_HB",
        groupvars = c("soileating", "visit_trimester")
    )
```

This code creates a violin plot of the distribution of haemoglobin concentrations, showing the differences between those who ate soil and those who did not. It also overlays mean values with confidence intervals, and facets the plot by the trimester of the visit. Refer to the 'MUAC plot' code chunk for explanations of the different commands:

```{r}
HB_plot <- HBsummary %>%
    ggplot(aes(
        x = as.factor(soileating),
        y = New_HB,
        fill = as.factor(soileating)
    )) +
    geom_violin(
        data = Xanalysis,
        mapping = aes(x = as.factor(soileating),
                      y = New_HB),
        alpha = 0.25,
        colour = NA
    ) +
    geom_pointrange(
        aes(
            ymin = New_HB - ci,
            ymax = New_HB + ci,
            colour = factor(soileating)
        ),
        shape = 18,
        size = 0.7
    ) +
    theme_bw() +
    labs(y = "Haemoglobin (g/dL)") +
    scale_fill_manual(values = c("purple", "#FF8000")) +
    scale_y_continuous(
        limits = c(4, 17),
        breaks = seq(4, 20, by = 2),
        expand = c(0, 0)
    ) +
    scale_colour_manual(values = c("purple", "#FF8000")) +
    theme(
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none",
        panel.grid = element_blank()
    ) +
    facet_grid(~ visit_trimester)
```

### Combined plot of nutritional status parameters

This code combines all the nutritional status parameter plots into a grid with two columns:

```{r}
grid <- grid.arrange(MUAC_plot,
                     B12_plot,
                     Folate_plot,
                     CorrlogFer_plot,
                     HB_plot,
                     ncol = 2)
```
