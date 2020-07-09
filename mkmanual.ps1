stack install

# Using ASCII encoding so that the Byte Order Mark is not included at the beginning of the file
stack exec -- pandoc-plot --help | Out-File -FilePath help.txt -Encoding ASCII
stack exec -- pandoc-plot clean --help | Out-File -FilePath  help-clean.txt -Encoding ASCII
stack exec -- pandoc-plot toolkits --help | Out-File -FilePath  help-toolkits.txt -Encoding ASCII
stack exec -- pandoc-plot write-example-config --help | Out-File -FilePath  help-config.txt -Encoding ASCII

stack exec -- pandoc --standalone --toc --include-before-body=docs/header.md --filter=pandoc-include-code docs/MANUAL.template -t gfm -o MANUAL.md

rm .\help.txt
rm .\help-clean.txt
rm .\help-toolkits.txt
rm .\help-config.txt