# Release checklist

[ ] Render the README with `mkreadme.ps1`
[ ] Update CHANGELOG.md if necessary.
[ ] Run Azure pipelines to make sure tests pass.
[ ] Tag release in git.
    `git tag -a "version" && git push origin "version"`
[ ] Download artifacts from Azure pipelines and upload to new GitHub release.
[ ] Run `build-setup.ps1` to create Windows installer
[ ] Upload to Hackage: `stack upload .`

## Conda-forge package
[ ] Create branch on `pandoc-plot-feedstock`.
[ ] Update feedstock with new filehashes. 
[ ] Rerender feedstock: `conda smithy rerender -c auto` 
[ ] Pull request to upstream: https://github.com/conda-forge/pandoc-plot-feedstock
[ ] Once tests pass, merge.