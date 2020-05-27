stack install

# Set the output to UTF8 to be included in the README
# See here:
#   https://stackoverflow.com/questions/40098771/changing-powershells-default-output-encoding-to-utf-8
$PSDefaultParameterValues['Out-File:Encoding'] = 'utf8'
pandoc-plot --help > help.txt

pandoc --standalone --toc --include-before-body=readme/header.md --filter=pandoc-include-code readme/README.template -t gfm -o README.md
rm .\help.txt
