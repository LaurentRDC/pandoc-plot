
This document shows two figures. The first figure is from the matplotlib gallery item [Line bars and markers](https://matplotlib.org/examples/lines_bars_and_markers/fill_demo.html):

```{.matplotlib caption="This figure has no preamble." source=true}
# Example modified from
# https://matplotlib.org/examples/lines_bars_and_markers/fill_demo.html
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(0, 1, 500)
y = np.sin(4 * np.pi * x) * np.exp(-5 * x)

fig, ax = plt.subplots()

ax.fill(x, y, zorder=10)
ax.grid(True, zorder=5)
```

The second figure has the same content, prepended by the script `include.py`:

```{.matplotlib preamble=include.py caption="This figure has a preamble. You can take a look at the source code for this figure to convince yourself." source=true}
# Example modified from
# https://matplotlib.org/examples/lines_bars_and_markers/fill_demo.html
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(0, 1, 500)
y = np.sin(4 * np.pi * x) * np.exp(-5 * x)

fig, ax = plt.subplots()

ax.fill(x, y, zorder=10)
ax.grid(True, zorder=5)
```