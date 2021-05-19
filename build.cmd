echo "Adding token for calculator..."
paket config add-token "https://nuget.pkg.github.com/kirillgarbar/index.json" %MY_COOL_TOKEN%

echo "Adding Arithm package..."
cd src\AvaloniaEditDemo
dotnet add package Arithm --version 1.0.2
cd ..\..\
echo "Restoring dotnet tools..."
dotnet tool restore

dotnet fake build -t %*