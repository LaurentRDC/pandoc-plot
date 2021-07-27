"""
Pandoc filter to replace code block content with file contents. This will replace:

```{.bash include=wtv.txt}
```

with the content of the file `wtv.txt`.
"""

from pandocfilters import toJSONFilter, CodeBlock, get_value

def include_code(key, value, format, _):
    if key == 'CodeBlock':
        [[ident, classes, keyvals], code] = value

        fname, keyvals = get_value(keyvals, "include", None)
        if fname is not None:
            filecontent = open(fname, mode='r').read()
            return CodeBlock([ident, classes, keyvals], filecontent)

if __name__ == "__main__":
    toJSONFilter(include_code)