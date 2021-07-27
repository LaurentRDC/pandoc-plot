# Release checklist

[ ] Update CHANGELOG.md if necessary.
[ ] Tag release in git.
    `git tag -a "version" && git push origin "version"`
    The executables/installers will be created, new GitHub release will
    automatically be generated, and the new version will be uploaded to Hackage.
[ ] Release to Hackage: `cabal upload .`

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