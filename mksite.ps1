stack exec -- pandoc `
                --standalone `
                --css=docs/theme.css `
                --filter pandoc-plot `
                -M "plot-configuration=docs/plot-config.yml" `
                -i docs/index.md `
                -o docs/index.html