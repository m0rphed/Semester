#!/usr/bin/env bash

set -eu
set -o pipefail

# Set enviroment variales for paket and fake-cli
PAKET_SKIP_RESTORE_TARGETS=true
FAKE_DETAILED_ERRORS=true 

dotnet tool install --global Paket

echo "Adding token for calculator..."
echo "Restoring dotnet tools..."
dotnet tool restore
paket config add-token "https://nuget.pkg.github.com/kirillgarbar/index.json" "$MY_COOL_TOKEN"
paket config add-token 'https://www.nuget.org' "$MY_COOL_TOKEN"
paket config add-token "https://www.myget.org/F/avalonia-ci/api/v2" "$MY_COOL_TOKEN"

# Start fake-cli and build repository with targets
dotnet fake build -t "$@"
