#!/usr/bin/env bash

# Reset
Color_Off='\033[0m'       # Text Reset

# Regular Colors
Black='\033[0;30m'        # Black
Red='\033[0;31m'          # Red
Green='\033[0;32m'        # Green
Yellow='\033[0;33m'       # Yellow
Blue='\033[0;34m'         # Blue
Purple='\033[0;35m'       # Purple
Cyan='\033[0;36m'         # Cyan
White='\033[0;37m'        # White

set -eu
set -o pipefail

printf "${Red}[info] ${Cyan}Running 'dotnet tool restore'...${Color_Off}\n"
dotnet tool restore

printf "${Red}[info] ${Cyan}Adding token for calculator...${Color_Off}\n"
printf "${Yellow}token: ${Yellow} => { $MY_COOL_TOKEN }${Color_Off}\n"
dotnet paket config add-token "https://nuget.pkg.github.com/kirillgarbar/index.json" "$MY_COOL_TOKEN"
# dotnet nuget add source --name "GitHub_Arithm" --username AndreiZaycev --password "$MY_COOL_TOKEN" "https://nuget.pkg.github.com/kirillgarbar/index.json" --store-password-in-clear-text

printf "${Red}[info] ${Cyan}Running 'dotnet paket restore'...${Color_Off}\n"
dotnet paket restore

# Set enviroment variales for paket and fake-cli
PAKET_SKIP_RESTORE_TARGETS=true
FAKE_DETAILED_ERRORS=true

printf "${Red}[info] ${Cyan}Running 'dotnet fake build -t \"\$\@\"'...${Color_Off}\n"
# Start fake-cli and build repository with targets
dotnet fake build -t "$@"
