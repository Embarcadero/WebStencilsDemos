# Script to tag and push Docker image to Docker Hub
# Usage: .\push_docker_image.ps1 [version]
# Example: .\push_docker_image.ps1 13.0

param(
    [string]$version = "13.0"
)

# Docker Hub repository
$dockerHubRepo = "thony/webstencils-demo"
$localImageName = "webstencils-demo"
$localImageTag = "latest"

Write-Host "=== Docker Hub Push Script ===" -ForegroundColor Cyan
Write-Host "Repository: $dockerHubRepo" -ForegroundColor Yellow
Write-Host "Version: $version" -ForegroundColor Yellow
Write-Host ""

# Check if local image exists
Write-Host "Checking if local image exists..." -ForegroundColor Cyan
$imageExists = wsl docker images "$localImageName`:$localImageTag" -q
if (-not $imageExists) {
    Write-Error "Local image $localImageName`:$localImageTag not found. Please build it first."
    exit 1
}

Write-Host "Local image found: $localImageName`:$localImageTag" -ForegroundColor Green
Write-Host ""

# Tag the image with Docker Hub repository name and version
Write-Host "Tagging image as $dockerHubRepo`:$version..." -ForegroundColor Cyan
wsl docker tag "$localImageName`:$localImageTag" "$dockerHubRepo`:$version"
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to tag image as $dockerHubRepo`:$version"
    exit 1
}
Write-Host "Tagged as $dockerHubRepo`:$version" -ForegroundColor Green

# Tag the image as latest
Write-Host "Tagging image as $dockerHubRepo`:latest..." -ForegroundColor Cyan
wsl docker tag "$localImageName`:$localImageTag" "$dockerHubRepo`:latest"
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to tag image as $dockerHubRepo`:latest"
    exit 1
}
Write-Host "Tagged as $dockerHubRepo`:latest" -ForegroundColor Green
Write-Host ""

# Check if user is logged in to Docker Hub
Write-Host "Checking Docker Hub login status..." -ForegroundColor Cyan
$loginCheck = wsl docker info 2>&1 | Select-String "Username"
if (-not $loginCheck) {
    Write-Warning "You may not be logged in to Docker Hub."
    Write-Host "Please run: wsl docker login" -ForegroundColor Yellow
    $continue = Read-Host "Continue anyway? (y/n)"
    if ($continue -ne "y") {
        exit 0
    }
}
Write-Host ""

# Push both tags
Write-Host "Pushing $dockerHubRepo`:$version to Docker Hub..." -ForegroundColor Cyan
wsl docker push "$dockerHubRepo`:$version"
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to push $dockerHubRepo`:$version"
    exit 1
}
Write-Host "Successfully pushed $dockerHubRepo`:$version" -ForegroundColor Green
Write-Host ""

Write-Host "Pushing $dockerHubRepo`:latest to Docker Hub..." -ForegroundColor Cyan
wsl docker push "$dockerHubRepo`:latest"
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to push $dockerHubRepo`:latest"
    exit 1
}
Write-Host "Successfully pushed $dockerHubRepo`:latest" -ForegroundColor Green
Write-Host ""

Write-Host "=== Push Complete ===" -ForegroundColor Green
Write-Host "Image available at: https://hub.docker.com/r/$dockerHubRepo" -ForegroundColor Cyan
Write-Host "Tags pushed: latest, $version" -ForegroundColor Cyan

