# Time difference between dietary recall visits and measurements of nutritional status parameters

This code creates a vector in order to rename the 'deviation' variables to their corresponding nutritional status parameter so that these new labels can be used in the plot:

```{r}
new_names <- c(
    "DevGA_MUAC" = "MUAC",
    "DevGA_B12" = "B12",
    "DevGA_Fol" = "Folate",
    "DevGA_Fer" = "Ferritin",
    "DevGA_HB" = "Haemoglobin"
)
```

This code selects and renames the 'deviation' columns in the first trmester cross-sectional analysis subset (`T1Analysis`) then reshapes the data from wide to long format by grouping the nutritional status parameters into a single column:

```{r}
t1analysis_long <- T1Analysis %>% 
    select(ID,          # Participant ID
           soileating,  # Reporting of geophagia (binary)
            # Deviation/ difference in days between ANC visit where MUAC,B12, folate, ferritin, or haemoglobin was measured and the corresponding dietary recall visit in the same trimester.
           DevGA_MUAC, 
           DevGA_B12, 
           DevGA_Fol, 
           DevGA_Fer, 
           DevGA_HB) %>%
    # Rename columns that start with "DevGA" using the `new_names` mapping vector
    rename_with(~ new_names[.x], starts_with("DevGA")) %>%
    # Reshape the dataset from wide to long format. The selected columns (MUAC, B12, Folate, Ferritin, Haemoglobin) will be combined into a single "Parameter" column and their values will be moved into a "Deviation" column
    pivot_longer(
        cols = c("MUAC", "B12", "Folate", "Ferritin", "Haemoglobin"), 
        names_to = "Parameter", 
        values_to = "Deviation"
    ) %>%
    # Convert the "Parameter" column into a factor and set the order of levels
    mutate(Parameter = factor(
        Parameter, 
        levels = c("Haemoglobin", "Ferritin", "Folate", "B12", "MUAC")
    ),
    # Add a new column called 'Trimester' with a fixed value of 'First'
    Trimester = 'First')

```

This code selects and renames the 'deviation' columns in the second trmester cross-sectional analysis subset (`T2Analysis`) then reshapes the data from wide to long format by grouping the nutritional status parameters into a single column. The commands are the same as in the previous code chunk:

```{r}
T2Analysis_long <- T2Analysis %>%
    select(ID,
           soileating,
           DevGA_MUAC,
           DevGA_B12,
           DevGA_Fol,
           DevGA_Fer,
           DevGA_HB) %>%
    rename_with(~ new_names[.x], starts_with("DevGA")) %>%
    pivot_longer(
        cols = c("MUAC", "B12", "Folate", "Ferritin", "Haemoglobin"),
        names_to = "Parameter",
        values_to = "Deviation"
    ) %>%
    mutate(Parameter = factor(
        Parameter,
        levels = c("Haemoglobin", "Ferritin", "Folate", "B12", "MUAC")
    ),
    Trimester = "Second")
```

This code selects and renames the 'deviation' columns in the third trmester cross-sectional analysis subset (`T3Analysis`) then reshapes the data from wide to long format by grouping the nutritional status parameters into a single column. The commands are the same as in the previous code chunk:

```{r}
T3Analysis_long <- T3Analysis %>%
    select(ID,
           soileating,
           DevGA_MUAC,
           DevGA_B12,
           DevGA_Fol,
           DevGA_Fer,
           DevGA_HB) %>%
    rename_with(~ new_names[.x], starts_with("DevGA")) %>%
    pivot_longer(
        cols = c("MUAC", "B12", "Folate", "Ferritin", "Haemoglobin"),
        names_to = "Parameter",
        values_to = "Deviation"
    ) %>%
    mutate(Parameter = factor(
        Parameter,
        levels = c("Haemoglobin", "Ferritin", "Folate", "B12", "MUAC")
    ),
    Trimester = "Third")
```

## Jitter plot
This code creates the combined dataset to plot and colours to use in the plot:

```{r}
# Combine the three long-formatted trimester datasets by stacking them row-wise
CombinedDev <- rbind(t1analysis_long, T2Analysis_long, T3Analysis_long)

# Create a vector to define a colour-blind friendly colour palette
palette <- c("#D55E00", "#0072B2", "#E69F00","purple", "#009E73")
```

```{r}
CombinedDevPlot <- 
    # Create a ggplot object with the CombinedDev dataset
    ggplot(CombinedDev, aes(
        y = Parameter,  # Map the 'Parameter' variable to the y-axis
        x = Deviation,  # Map the 'Deviation' variable to the x-axis
        text = paste(  # Add interactive text with details for tooltips (to be used in interactive plot)
            "ID:", ID,
            "\nGeophagia:", soileating,
            "\nParameter:", Parameter,
            "\nDeviation:", Deviation)
    )) +
    # Add a jitter plot to reduce overplotting and fill and colour jitter points by nutritional status parameter
    geom_jitter(aes(
        fill = Parameter,
        color = Parameter), 
        size = 1) +
    # Use a black and white theme for the plot and label the x-axis
    theme_bw() +
    labs(x = "Days from dietary recall visit") +
    # Customize the y-axis scale 
    scale_x_continuous(
        limits = c(-63, 91),
        breaks = seq(-63, 91, by = 7),
        expand = c(0, 0)
    ) +
    # Customize various theme elements
    theme(
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(size = 11)
    ) +
    # Manually set the colour scale using the pre-defined palette
    scale_color_manual(values = palette)+
    # Create separate facets for each trimester in the data 
    facet_grid(rows = vars(Trimester))
```

This code creates an interactive version of the plot using plotly with the ability to see the participant's ID, whether or not they reported geophagia in the corresponding dietary recall, the exact deviation value and nutritional status parameter when you hover over a specific data point.

```{r}
ggplotly(CombinedDevPlot,
         # Specify that tooltips should display the "text" aesthetic from ggplot (ID, Geophagia, Parameter, Deviation)
         tooltip= c("text")) %>%
    # Customize the appearance of hover labels
    layout(hoverlabel=list(bgcolor="white"))
```
