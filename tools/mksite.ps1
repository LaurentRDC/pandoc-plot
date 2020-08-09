.\tools\mkmanual

cd docs/

stack exec -- pandoc-plot clean index.md

$VERSION = stack exec -- pandoc-plot --version

stack exec -- pandoc `
                --standalone `
                --css=theme.css `
                --filter pandoc-plot `
                --metadata title="pandoc-plot $VERSION" `
                -i index.md `
                -o index.html

cd ..