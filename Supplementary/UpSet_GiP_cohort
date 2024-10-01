# UpSet plot of GiP occurrence in the whole cohort

This code prepares the data to plot, such that only participants that ate soil at any time during pregnancy are included and that there is only one entry per participant.

```{r}
GiP_Upset_Trimester <- ponadb %>%
    filter(soileating == 1) %>%
    mutate(
        soileating_t1 = coalesce(soileating_t1, 0),
        soileating_t2 = coalesce(soileating_t2, 0),
        soileating_t3 = coalesce(soileating_t3, 0)
    ) %>%
    distinct(ID, .keep_all = TRUE) %>%
    mutate(
        First = as.numeric(as.character(soileating_t1)),
        Second = as.numeric(as.character(soileating_t2)),
        Third = as.numeric(as.character(soileating_t3))
    ) %>%
    as.data.frame()
```

## UpSet plot
This code creates the UpSet plot to visualize the overlap between participants who ate soil in the first, second, and third trimesters.

```{r}
upset(GiP_Upset_Trimester, 
      sets = c("First" ,"Second", "Third")
      ,mb.ratio = c(0.7, 0.3) ,
      text.scale = c(1.5, 1.5, 1.3, 1.3, 1.9, 2.1), 
      # Order the intersections in the plot by the number of sets each overlaps (degree)
      order.by = "degree")
```
