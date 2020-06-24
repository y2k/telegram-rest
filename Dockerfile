FROM mcr.microsoft.com/dotnet/core/sdk:3.1.301-alpine3.12

WORKDIR /app
COPY . /app

RUN dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/core/runtime:3.1.5-alpine3.12

WORKDIR /app
COPY --from=0 /app/bin/Release/netcoreapp3.1/linux-x64/publish .

ENTRYPOINT ["dotnet", "telegram-rest.dll"]
