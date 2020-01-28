# Build windows installer

Write-Host "Cleaning .stack-work directory"
stack clean --full

Write-Host "Building executable"
stack install pandoc-plot --local-bin-path ".\installer"

# Extract version from cabal file
# This is so we can dynamically name the installer using the exe version
$regex = "^version:\s+\d+.\d+.\d+.\d+"
$versionString = Select-String -Path ".\pandoc-plot.cabal" -Pattern $regex -AllMatches
$version = ($versionString.Matches.Value -split "\s+")[1]
Write-Host "Version: " $version

Write-Host "Building setup using Inno Setup Compiler"
if ($ENV:PROCESSOR_ARCHITECTURE -eq "AMD64"){
    $iscc = get-item "C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
}
else {
    $iscc = get-item "C:\Program Files\Inno Setup 5\ISCC.exe"
}
& $iscc /dAppVersion=$version ".\installer\pandoc-plot-setup.iss"