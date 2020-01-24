---
title: Example of pandoc-plot usage with Markdown
---

The plots below showcase the use of `pandoc-plot`. To compile this file:

```bash
pandoc --filter pandoc-plot -i markdown.md -o example.html
```

The most basic plot is composed of the toolkit name (e.g. `matplotlib`):

```{.matplotlib}
# This example ahs been modified from the following Matplotlib gallery item:
# https://matplotlib.org/examples/images_contours_and_fields/streamplot_demo_features.html
import numpy as np
import matplotlib.pyplot as plt

Y, X = np.mgrid[-3:3:100j, -3:3:100j]
U = -1 - X**2 + Y
V = 1 + X - Y**2
speed = np.sqrt(U*U + V*V)

fig0, ax0 = plt.subplots()
strm = ax0.streamplot(X, Y, U, V, color=U, linewidth=2, cmap=plt.cm.autumn)
fig0.colorbar(strm.lines)
```

For documents which can show links (e.g. HTML), you can link the source code for the figure in the caption:

```{.matplotlib source=True}
# This example has been modified from the following Matplotlib gallery item:
# https://matplotlib.org/examples/statistics/histogram_demo_multihist.html
import numpy as np
import matplotlib.pyplot as plt

np.random.seed(0)

n_bins = 10
x = np.random.randn(1000, 3)

fig, (ax0, ax1) = plt.subplots(nrows=1, ncols=2)

colors = ['red', 'tan', 'lime']
ax0.hist(x, n_bins, density=1, histtype='bar', color=colors, label=colors)
ax0.legend(prop={'size': 10})
ax0.set_title('bars with legend')

ax1.hist(x, n_bins, density=1, histtype='bar', stacked=True)
ax1.set_title('stacked bar')

fig.tight_layout()
```

You can add a caption to your figure like this:

```{.matplotlib}

```