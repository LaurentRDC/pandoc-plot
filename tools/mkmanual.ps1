stack install

$version = stack exec -- pandoc-plot --version

# Using ASCII encoding so that the Byte Order Mark is not included at the beginning of the file
stack exec -- pandoc-plot --help | Out-File -FilePath help.txt -Encoding ASCII
stack exec -- pandoc-plot clean --help | Out-File -FilePath  help-clean.txt -Encoding ASCII
stack exec -- pandoc-plot toolkits --help | Out-File -FilePath  help-toolkits.txt -Encoding ASCII
stack exec -- pandoc-plot write-example-config --help | Out-File -FilePath  help-config.txt -Encoding ASCII

# For GitHub
stack exec -- pandoc `
                --standalone `
                --toc `
                -M version=$version `
                --wrap=preserve `
                --filter=pandoc-include-code `
                --template=docs/manual-template.md `
                docs/manual-content.md `
                -t gfm -o MANUAL.md
                
# To be embedded
stack exec -- pandoc `
                --standalone `
                --self-contained `
                --wrap=preserve `
                --css=docs/theme.css `
                MANUAL.md -o docs/MANUAL.html

rm .\help.txt
rm .\help-clean.txt
rm .\help-toolkits.txt
rm .\help-config.txt

# Manual may have changed - update binary
stack install