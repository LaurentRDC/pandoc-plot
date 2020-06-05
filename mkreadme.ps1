stack install

# Set the output to UTF8 to be included in the README
# See here:
#   https://stackoverflow.com/questions/40098771/changing-powershells-default-output-encoding-to-utf-8
$PSDefaultParameterValues['Out-File:Encoding'] = 'utf8'

pandoc-plot --help > help.txt
pandoc-plot clean --help > help-clean.txt
pandoc-plot toolkits --help > help-toolkits.txt
pandoc-plot write-example-config --help > help-config.txt

pandoc --standalone --toc --include-before-body=readme/header.md --filter=pandoc-include-code readme/README.template -t gfm -o README.md

rm .\help.txt
rm .\help-clean.txt
rm .\help-toolkits.txt
rm .\help-config.txt