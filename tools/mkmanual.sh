echo "Making the pandoc-plot manual"

cabal install exe:pandoc-plot --overwrite-policy=always
cabal install pandoc-cli --overwrite-policy=always
declare version="$(pandoc-plot --version)"

pandoc-plot --help > help.txt
pandoc-plot clean --help > help-clean.txt
pandoc-plot toolkits --help > help-toolkits.txt
pandoc-plot write-example-config --help > help-config.txt

# For GitHub
echo "Creating Markdown manual..."
pandoc \
    --standalone \
    --embed-resources \
    --toc \
    --toc-depth=4 \
    --metadata version=$version \
    --metadata title="pandoc-plot $version manual" \
    --wrap=preserve \
    --filter=tools/include-code.py \
    --template=docs/manual-template.md \
    docs/manual-content.md \
    -t gfm -o MANUAL.md
                
# To be embedded
echo "Creating HTML manual..."
pandoc \
    --standalone \
    --embed-resources \
    --wrap=preserve \
    --metadata title="pandoc-plot $version manual" \
    --css=docs/theme.css \
    MANUAL.md -o docs/MANUAL.html

rm help.txt
rm help-clean.txt
rm help-toolkits.txt
rm help-config.txt

# Manual may have changed - update binary
cabal install --overwrite-policy=always