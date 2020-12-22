
```{.plotly_r}
library(plotly)
a <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 0,
  dtick = 0.25,
  ticklen = 5,
  tickwidth = 2,
  tickcolor = toRGB("blue")
)
s <- seq(1, 4, by = 0.25)
fig <- plot_ly(x = ~s, y = ~s)
fig <- fig %>% layout(xaxis = a, yaxis = a)
```