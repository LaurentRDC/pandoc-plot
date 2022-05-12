---
plot-configuration: tests/fixtures/.verbose-config.yml
---

```{.matplotlib executable="./issue46/bin/python"}
import sys
print(f"sys.executable={sys.executable}", file=sys.stderr)
import npstreams
```