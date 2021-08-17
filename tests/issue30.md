---
plot-configuration: tests/fixtures/.issue30.yml
---

# heading

https://github.com/LaurentRDC/pandoc-plot/issues/30

## subheading

this is text

```{.plantuml caption="test"}
@startgantt
[Prototype design] lasts 15 days
[Test prototype] lasts 10 days
-- All example --
[Task 1 (1 day)] lasts 1 day
[T2 (5 days)] lasts 5 days
[T3 (1 week)] lasts 1 week
[T4 (1 week and 4 days)] lasts 1 week and 4 days
[T5 (2 weeks)] lasts 2 weeks
@endgantt
```