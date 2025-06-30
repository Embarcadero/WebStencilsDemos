# This script is executed as a post-build event to build and prepare a Docker image for the Delphi application.
# It handles cleanup of old Docker resources, sets up permissions, and builds a new Docker image.

# Define the image name and tag
$imageName = "webstencils-demo"
$imageTag = "latest"

# Clean up old containers, images, and volumes
wsl docker ps -a --filter "ancestor=$imageName`:$imageTag" -q | ForEach-Object { wsl docker stop $_; wsl docker rm $_ }
wsl docker images "$imageName`:$imageTag" -q | ForEach-Object { wsl docker rmi -f $_ }
wsl docker volume prune -f

# Get the directory where the script is located
$scriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

# Verify Linux64 directory exists
$linux64Path = Join-Path $scriptPath "Linux64"
if (-not (Test-Path $linux64Path)) {
    Write-Error "Linux64 directory not found"
    exit 1
}

# Verify the executable exists
$executablePath = Join-Path $linux64Path "Docker\WebStencilsDemo"
if (-not (Test-Path $executablePath)) {
    Write-Error "WebStencilsDemo executable not found in Linux64/Docker"
    exit 1
}

# Verify resources directory exists
$resourcesPath = Join-Path $scriptPath "..\..\resources"
if (-not (Test-Path $resourcesPath)) {
    Write-Error "resources directory not found"
    exit 1
}

# Verify initial database exists
$initialDbPath = Join-Path $resourcesPath "data\database.sqlite3"
if (-not (Test-Path $initialDbPath)) {
    Write-Error "Initial database not found in resources/data"
    exit 1
}

# Create a temporary build context directory
$buildContextPath = Join-Path $scriptPath "docker_build"
if (Test-Path $buildContextPath) {
    Remove-Item -Path $buildContextPath -Recurse -Force
}
New-Item -ItemType Directory -Path $buildContextPath | Out-Null

# Create the directory structure for the executable
$executableDestDir = Join-Path $buildContextPath "Linux64\Docker"
New-Item -ItemType Directory -Path $executableDestDir -Force | Out-Null

# Copy required files to build context
Copy-Item -Path $executablePath -Destination (Join-Path $buildContextPath "Linux64\Docker\WebStencilsDemo") -Force
Copy-Item -Path $resourcesPath -Destination (Join-Path $buildContextPath "resources") -Recurse -Force
Copy-Item -Path (Join-Path $scriptPath "Dockerfile") -Destination (Join-Path $buildContextPath "Dockerfile") -Force

# Set executable permissions using WSL
$wslExecutablePath = wsl wslpath -u ((Join-Path $buildContextPath "Linux64\Docker\WebStencilsDemo") -replace '\\', '/')
wsl chmod +x $wslExecutablePath

# Convert Windows path to WSL path
$wslPathInput = $buildContextPath -replace '\\', '/'
$wslPath = wsl wslpath -u $wslPathInput

# Build command using WSL with --no-cache to force a clean build
$buildCommand = "docker build --no-cache -t $($imageName):$($imageTag) $($wslPath)"
Invoke-Expression -Command "wsl $buildCommand"

# Clean up build context
Remove-Item -Path $buildContextPath -Recurse -Force

if ($LASTEXITCODE -eq 0) {
    Write-Host "Docker image built successfully"
} else {
    Write-Error "Failed to build Docker image"
    exit 1
} 