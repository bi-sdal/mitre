
# Maps

```{r}
arlFit <- readRDS(here('./data/mitre/final/smart_maps/fitted_prob_data.RDS'))
```

```{r}
ggplot(arlFit) + 
  geom_polygon(aes(x=long,y=lat,group=group,fill=fitted.values),
               #geom_polygon(aes(x=long,y=lat,group=group,fill=prob),
               alpha=1,color="grey70",lwd=.5) +
  scale_fill_gradientn(colors=c('lightblue','red','yellow'),
                       values = scales::rescale(c(0, .09, .18, 0.25, 0.4)),
                       #labels=percent,name=expression("Pr(DOME)"),
                       limits=c(0,max(fit0$fitted,fit1$fitted))) +
  coord_quickmap() + #coord_equal(ratio=1) +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
        plot.caption = element_text(hjust=0)) + #labels
  labs(title="Fitted Probability of Domestic Abuse Call", x="", y="")
```

```{r}
ggplot(arlFit) + 
  geom_polygon(aes(x=long,y=lat,group=group,fill=fitted.values0),
               #geom_polygon(aes(x=long,y=lat,group=group,fill=prob),
               alpha=1,color="grey70",lwd=.5) +
  scale_fill_gradientn(colors=c('lightblue','red','yellow'),
                       values = scales::rescale(c(0, .09, .18, 0.25, 0.4)),
                       #labels=percent,name=expression("Pr(DOME)"),
                       limits=c(0,max(fit0$fitted,fit1$fitted))) +
  coord_quickmap() + #coord_equal(ratio=1) +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
        plot.caption = element_text(hjust=0)) + #labels
  labs(title="Fitted Probability of Domestic Abuse Call", x="", y="")
```

## differences

```{r}
max_diff <- max(arlFit$fitted.values - arlFit$fitted.values0, na.rm = TRUE)
print(max_diff)

ggplot(arlFit) + 
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group,
                   fill = fitted.values - fitted.values0),
               alpha = 1,
               color = "grey70",
               lwd = .5) +
  scale_fill_viridis_c() +
  coord_quickmap() + #coord_equal(ratio=1) +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(lineheight = .8,
                                  face = "bold",
                                  vjust = 1,
                                  hjust = .5),
        plot.caption = element_text(hjust = 0)) + #labels
  labs(title = "Fitted Probability of Domestic Abuse Call",
       x = "",
       y = "")
```

GEOIDs of the "highest" differences

```{r}
arlFit %>%
  dplyr::filter(fitted.values - fitted.values0 > .002) %>%
  dplyr::select(GEOID) %>%
  base::unique()
```

```{r}
arlFit %>%
  dplyr::filter(fitted.values - fitted.values0 < -.002) %>%
  dplyr::select(GEOID) %>%
  base::unique()
```

# Fitted vs Feature Plots

```{r}
feature = 'medInc'
featureFitPlotData = data.table(getElement(fit1$data, feature), fit1$fitted.values)
colnames(featureFitPlotData) = c(feature, "Pr(DOME)")
ggplot(data = featureFitPlotData) + 
  geom_point(aes(x = get(feature), y = `Pr(DOME)`)) + 
  labs(x = feature)

```

```{r}
for(i in names(fit1$coefficients)[-1]){
  feature = i
  featureFitPlotData = data.table(getElement(fit1$data, feature), fit1$fitted.values)
  colnames(featureFitPlotData) = c(feature, "Pr(DOME)")
  p = ggplot(data = featureFitPlotData) + 
    geom_point(aes(x = get(feature), y = `Pr(DOME)`)) + 
    labs(x = feature)
  plot(p)
}
