.\mkmanual

cd docs/

stack exec -- pandoc-plot clean index.md

stack exec -- pandoc `
                --standalone `
                --css=theme.css `
                --filter pandoc-plot `
                -i index.md `
                -o index.html

cd ..