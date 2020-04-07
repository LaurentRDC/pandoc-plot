# Build windows installer

Write-Host "Cleaning .stack-work directory"
stack clean --full

Write-Host "Building executable"
stack install pandoc-plot --local-bin-path ".\installer"

$version = .\installer\pandoc-plot.exe --version
Write-Host "Version: " $version

Write-Host "Building setup using Inno Setup Compiler"
if ($ENV:PROCESSOR_ARCHITECTURE -eq "AMD64"){
    $iscc = get-item "C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
}
else {
    $iscc = get-item "C:\Program Files\Inno Setup 5\ISCC.exe"
}
& $iscc /dAppVersion=$version ".\installer\pandoc-plot-setup.iss"