# Report example

This example shows you how you can integrate pandoc-plot with your scientific writing, which is commonly done in La/TeX. You will need Python 3+, and the `matplotlib` package.

In this example, we have a LaTex template `template.tex` that mimics a very simple article. The template contains a title, author name, and abstract, but the content is placed in a separate Markdown file, `article.md`.

The content of `article.md` will be manipulated by pandoc-plot first, and then place in the template where `$body$` is placed.

To compile:

```bash
pandoc --filter pandoc-plot --template=template.tex --output=article.pdf article.md
```