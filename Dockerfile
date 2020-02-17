FROM mcr.microsoft.com/dotnet/core/sdk:3.1.101-alpine3.10

WORKDIR /app
COPY . /app

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false

# FROM mcr.microsoft.com/dotnet/core/runtime:3.1.1-alpine3.10
FROM mcr.microsoft.com/dotnet/core/runtime:2.1.15-alpine3.10

WORKDIR /app
COPY --from=0 /app/bin/Release/netcoreapp2.1/linux-x64/publish .

ENTRYPOINT ["dotnet", "telegram-rest.dll"]
