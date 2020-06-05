stack install

# Using ASCII encoding so that the Byte Order Mark is not included at the beginning of the file
pandoc-plot --help | Out-File -FilePath help.txt -Encoding ASCII
pandoc-plot clean --help | Out-File -FilePath  help-clean.txt -Encoding ASCII
pandoc-plot toolkits --help | Out-File -FilePath  help-toolkits.txt -Encoding ASCII
pandoc-plot write-example-config --help | Out-File -FilePath  help-config.txt -Encoding ASCII

pandoc --standalone --toc --include-before-body=readme/header.md --filter=pandoc-include-code readme/README.template -t gfm -o README.md

rm .\help.txt
rm .\help-clean.txt
rm .\help-toolkits.txt
rm .\help-config.txt