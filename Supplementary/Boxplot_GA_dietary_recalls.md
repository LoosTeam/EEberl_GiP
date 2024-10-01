# Gestational age at dietary recall visit for each trimester

This code prepares the data to plot by creating a new data frame that consists of one entry per participant and trimester of dietary recall completion, with a preference for dietary recall visits that included a geophagia report:

```{r}
GA_24hr_Analysis_Sample <- ponadb %>% 
    # Filter the data to only include rows where the "Done_24h" column equals 1
    filter(Done_24h == 1) %>%
    # Create new variables to plot based on pre-existing variables.
    mutate(
        Trimester = factor(visit_trimester, 
                           labels = c("First", "Second", "Third")),
        GestWk = GA_visit / 7
    ) %>%
    # Arrange the data by "ID", "visit_trimester" and then in descending order by the "soileating" column.
    arrange(ID, visit_trimester, desc(soileating)) %>%
    # Keep only distinct rows based on "ID" and "visit_trimester", retaining all columns for the first occurrence.
    distinct(ID, visit_trimester, .keep_all = TRUE) %>%
    # Select only the columns "Trimester", "GestWk", and "Done_24h" for the final dataset to plot. 
    select(Trimester, GestWk, Done_24h)

```

## Boxplot
This code generates a summary plot visualising the distribution of gestational weeks where dietary recalls were completed across different trimesters in the `GA_24hr_Analysis_Sample` dataset:

```{r}
GADiet_summaryPlot <- ggplot(
    GA_24hr_Analysis_Sample,
    # Define the aesthetics: x-axis is trimester, y-axis is gestational week
    aes(x = Trimester,
        y = GestWk)
) +
    # Add a boxplot with a light grey fill color
    geom_boxplot(fill = "lightgrey") +
    # Set the y-axis label to "Gestational week"
    labs(y = "Gestational week") +
    # Apply a minimal theme
    theme_minimal() +
    # Customize the y-axis: limits from 0 to 42, breaks every 2 weeks, and no expansion (no padding around limits)
    scale_y_continuous(limits = c(0, 42),
                       breaks = seq(0, 42, by = 2),
                       expand = c(0, 0)) +
    # Customize various theme elements including omitting grid lines, colour of axis lines, and text size of axis titles and labels.
    theme(
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.position = "none"
    )

```
