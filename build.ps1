[xml]$doc = Get-Content .\src\Directory.Build.props
$version = $doc.Project.PropertyGroup.VersionPrefix # the version under development, update after a release
$versionSuffix = '-build.0' # manually incremented for local builds

function isVersionTag($tag){
    $v = New-Object Version
    [Version]::TryParse($tag, [ref]$v)
}

if ($env:appveyor){
    $versionSuffix = '-build.' + $env:appveyor_build_number
    if ($env:appveyor_repo_tag -eq 'true' -and (isVersionTag($env:appveyor_repo_tag_name))){
        $version = $env:appveyor_repo_tag_name
        $versionSuffix = ''
    }
    Update-AppveyorBuild -Version "$version$versionSuffix"
}

dotnet build -c Release freya-types.sln /p:Version=$version$versionSuffix
dotnet test --no-build -c Release tests/Freya.Types.Http.Tests/Freya.Types.Http.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Types.Http.Cors.Tests/Freya.Types.Http.Cors.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Types.Http.State.Tests/Freya.Types.Http.State.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Types.Language.Tests/Freya.Types.Language.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Types.Uri.Tests/Freya.Types.Uri.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Types.Uri.Template.Tests/Freya.Types.Uri.Template.Tests.fsproj
dotnet pack --no-build -c Release src/Freya.Types /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Types.Http /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Types.Http.Cors /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Types.Http.Patch /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Types.Http.State /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Types.Language /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Types.Uri /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Types.Uri.Template /p:Version=$version$versionSuffix -o $psscriptroot/bin
