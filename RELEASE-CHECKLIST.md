# Release checklist

[ ] Update CHANGELOG.md if necessary.
[ ] Run Github Actions to make sure tests pass.
[ ] Tag release in git.
    `git tag -a "version" && git push origin "version"`
[ ] Download artifacts from Github Actions and upload to new GitHub release.
[ ] Upload to Hackage: `stack upload .`

## Conda-forge package
[ ] Create branch on `pandoc-plot-feedstock`.
[ ] Update feedstock with new filehashes. 
[ ] Rerender feedstock: `conda smithy rerender -c auto` 
[ ] Pull request to upstream: https://github.com/conda-forge/pandoc-plot-feedstock
[ ] Once tests pass, merge.

## winget package
[ ] Create branch on `winget-pkgs`
[ ] Create new manifest in `manifests/LaurentPRenedeCotret/pandoc-plot`
[ ] Pull request to upstream: https://github.com/microsoft/winget-pkgs