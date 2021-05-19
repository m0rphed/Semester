#!/usr/bin/env bash

set -eu
set -o pipefail
dotnet new tool-manifest --force
dotnet tool install Paket

echo "Adding token for calculator..."
echo $MY_COOL_TOKEN
dotnet paket config add-token "https://nuget.pkg.github.com/kirillgarbar/index.json" "$MY_COOL_TOKEN"

echo "Adding Arithm package..."
cd ./src/AvaloniaEditDemo
echo "[INFO] Current path is: " ; pwd
dotnet add package Arithm --version 1.0.2
cd ../../
echo "[INFO] Current path is: " ; pwd

echo "Restoring dotnet tools..."
dotnet tool restore

PAKET_SKIP_RESTORE_TARGETS=true FAKE_DETAILED_ERRORS=true dotnet fake build -t "$@"
