# Custom configuration file

This example shows how to use a custom configuration file.

To compile this example:

```bash
pandoc --filter pandoc-plot -M plot-configuration=plot-config.yml -i markdown.md -o example.html
```

or, equivalently:

```bash
pandoc --filter pandoc-plot --metadata plot-configuration:plot-config.yml -i markdown.md -o example.html
```