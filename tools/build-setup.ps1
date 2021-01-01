# Build windows installer

Write-Host "Cleaning .stack-work directory"
stack clean --full

Write-Host "Building executable"
stack install pandoc-plot --local-bin-path ".\installer"

$version = .\installer\pandoc-plot.exe --version
Write-Host "Version: " $version

Write-Host "Building setup using Inno Setup Compiler"
iscc /dAppVersion=$version ".\installer\pandoc-plot-setup.iss"