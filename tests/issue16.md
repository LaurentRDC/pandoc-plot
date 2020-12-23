---
plot-configuration: tests/fixtures/.verbose-config.yml
---

# Verifying that unicode symbols work with the PlotJL toolkit

```{.plotsjl}
using Plots
plot(sin, (x->begin
            sin(2x)
        end), 0, 2π, line = 4, leg = false, fill = (0, :orange))
```