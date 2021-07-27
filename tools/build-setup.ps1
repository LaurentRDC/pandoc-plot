# Build windows installer

Write-Host "Building executable"
cabal install ".\installer"

$version = .\installer\pandoc-plot.exe --version
Write-Host "Version: " $version

Write-Host "Building setup using Inno Setup Compiler"
iscc /dAppVersion=$version ".\installer\pandoc-plot-setup.iss"