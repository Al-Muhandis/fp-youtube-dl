#!/usr/bin/env pwsh
##############################################################################################################

Function Show-Usage {
    Return "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
"
}

Function Request-File {
    ForEach ($REPLY in $args) {
        $params = @{
            Uri = $REPLY
            OutFile = (Split-Path -Path $REPLY -Leaf).Split('?')[0]
        }
        Invoke-WebRequest @params | Out-Null
        Return $params.OutFile
    }
}

Function Install-Program {
    While ($Input.MoveNext()) {
        Switch ((Split-Path -Path $Input.Current -Leaf).Split('.')[-1]) {
            'msi' {
                & msiexec /passive /package $Input.Current | Out-Host
            }
            'exe' {
                & ".\$($Input.Current)" /SP- /VERYSILENT /SUPPRESSMSGBOXES /NORESTART | Out-Host
            }
        }
        Remove-Item $Input.Current
    }
}

Function Build-Project {
    $VAR = @{
        Use = 'use'
        Cmd = 'lazbuild'
        Url = 'https://fossies.org/windows/misc/lazarus-3.6-fpc-3.2.2-win64.exe'
        Path = "C:\Lazarus"
    }
    Try {
        Get-Command $VAR.Cmd
    } Catch {
        "Install $($VAR.Path)" | Out-Host
        Request-File $VAR.Url | Install-Program
        $env:PATH+=";$($VAR.Path)"
        Get-Command $VAR.Cmd
    }
    If (Test-Path -Path $($VAR.Use)) {
        & git submodule update --init --recursive --force --remote | Out-Host
        $COMPONENTS = "$($VAR.Use)\components.txt"
        If (Test-Path -Path $COMPONENTS) {
            'Download packages:' | Out-Host
            Get-Content -Path $COMPONENTS | ForEach-Object {
                If ((! (& $VAR.Cmd --verbose-pkgsearch $_ )) &&
                    (! (& $VAR.Cmd --add-package $_)) &&
                    (! (Test-Path -Path "$($VAR.Use)\$($_)"))) {
                        "    download package $($_)" | Out-Host
                        $OutFile = Request-File "https://packages.lazarus-ide.org/$($_).zip"
                        Expand-Archive -Path $OutFile -DestinationPath "$($VAR.Use)\$($_)" -Force
                        Remove-Item $OutFile
                    }
            }
        }
        'Add dependencies:' | Out-Host
        Get-ChildItem -Filter '*.lpk' -Recurse -File –Path 'use'| Sort-Object | ForEach-Object {
            If (& $VAR.Cmd --add-package-link $_) {
                "    [SUCCESS] add dependence $($_)" | Out-Host
            } Else {
                "    [FAILED!] add dependence $($_)" | Out-Host
            }
        }
    }
    'Build projects:' | Out-Host
    Get-ChildItem -Filter '*.lpi' -Recurse -File –Path 'src'| Sort-Object | ForEach-Object {
        If (& $VAR.Cmd --no-write-project --recursive $_) {
            "    [SUCCESS] build project $($_)" | Out-Host
        } Else {
            "    [FAILED!] build project $($_)" | Out-Host
            & $VAR.Cmd --no-write-project --recursive $_ | Out-Host
            $exitCode = $LastExitCode
            Throw $exitCode
        }
    }
    "Done!" | Out-Host
}

Function Switch-Action {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict # -Trace 1
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        Switch ($args[0]) {
            'build' {
                Build-Project
            }
            Default {
                Show-Usage
            }
        }
    } Else {
        Show-Usage
    }
}

##############################################################################################################
Switch-Action @args | Out-Null
