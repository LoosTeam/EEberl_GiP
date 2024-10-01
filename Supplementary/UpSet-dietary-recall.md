# UpSet plot of diet recall completion in cohort

This code prepares the data to plot, such that only participants that completed at least one dietary recall during pregnancy are included and that there is only one entry per participant.

```{r}
DietRecalls <- ponadb %>%
    filter(Done_24h==1)%>%
    mutate(
        First = as.numeric(as.character(dietrecall_t1)),
        Second = as.numeric(as.character(dietrecall_t2)),
        Third = as.numeric(as.character(dietrecall_t3))
    ) %>%
    distinct(ID, .keep_all = TRUE) %>%
    as.data.frame()
```

This code creates the UpSet plot to visualize the overlap between participants who completed dietary recalls in the first, second, and third trimesters.

```{r}
# 
upset(DietRecalls, 
      sets = c("Third", "Second", "First"), 
      # Set the ratio between the main bar plot and the matrix plot
      mb.ratio = c(0.7, 0.3),  
      # Customize the size of various text elements in the plot
      text.scale = c(1.5, 1.5, 1.3, 1.3, 1.9, 2.1), 
      # Keep the order of sets as specified (First, Second, Third)
      keep.order = TRUE)                      
```

