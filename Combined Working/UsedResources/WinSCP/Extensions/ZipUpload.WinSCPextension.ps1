# @name         &ZIP and Upload...
# @command      powershell.exe -ExecutionPolicy Bypass -File "%EXTENSION_PATH%" -sessionUrl "!S" -remotePath "!/" -archiveName "%ArchiveName%" -pause -sessionLogPath "%SessionLogPath%" %Use7zip% -path7zip "%Path7zip%" -archive7zip %Archive7zip% !&
# @description  Packs the selected files to a ZIP archive and uploads it
# @flag         ApplyToDirectories
# @version      4
# @homepage     https://winscp.net/eng/docs/library_example_zip_and_upload
# @require      WinSCP 5.8.4
# @require      .NET 4.5
# @option       ArchiveName -run textbox "&Archive name:" "archive"
# @option       - -config -run group "7-zip"
# @option         Use7zip -config -run checkbox "Use &7-zip" "" -use7zip
# @option         Archive7zip -config -run dropdownlist "&Archive type (with 7-zip):" zip zip 7z xz gzip bzip2 tar
# @option         Path7zip -config file "7-zip &path (7z.exe/7za.exe):" "C:\Program Files\7-Zip\7z.exe"
# @option       - -config group "Logging"
# @option         SessionLogPath -config sessionlogfile
# @optionspage  https://winscp.net/eng/docs/library_example_zip_and_upload#options

param (
    # Use Generate URL function to obtain a value for -sessionUrl parameter.
    $sessionUrl = "sftp://user:mypassword;fingerprint=ssh-rsa-xx-xx-xx@example.com/",
    [Parameter(Mandatory = $True)]
    $remotePath,
    [Switch]
    $pause,
    [Switch]
    $use7Zip,
    # The 7z.exe can be replaced with portable 7za.exe
    $path7zip = "C:\Program Files\7-Zip\7z.exe",
    $archive7zip = "zip",
    [Parameter(Mandatory = $True)]
    $archiveName,
    $sessionLogPath = $Null,
    [Parameter(Mandatory = $True, ValueFromRemainingArguments = $True, Position = 0)]
    $localPaths
)

try
{
    if ($use7Zip)
    {
        $archiveName += "." + $archive7zip
    }
    else
    {
        $archiveName += ".zip"
    }

    Write-Host ("Archiving {0} files to archive {1}..." -f $localPaths.Count, $archiveName)
    
    $archivePath = Join-Path ([System.IO.Path]::GetTempPath()) $archiveName

    if (Test-Path $archivePath)
    {
        Remove-Item $archivePath
    }

    # Using 7-Zip one can create also other archive formats, not just ZIP
    if ($use7Zip)
    {
        # Create archive
        & "$path7zip" a "-t$archive7zip" $archivePath $localPaths

        if ($LASTEXITCODE -gt 0)
        {
            throw "Archiving failed."
        }
    }
    else
    {
        if ($PSVersionTable.PSVersion.Major -lt 3)
        {
            throw "PowerShell 3.0 and newer is required. Please, upgrade PowerShell. Or try using the 7-zip mode instead."
        }

        Add-Type -AssemblyName "System.IO.Compression"
        Add-Type -AssemblyName "System.IO.Compression.FileSystem"

        $zip = [System.IO.Compression.ZipFile]::Open($archivePath, [System.IO.Compression.ZipArchiveMode]::Create)

        # Replace with Compress-Archive once PowerShell 5.0 is widespread

        foreach ($localPath in $localPaths)
        {
            $parentPath = Split-Path -Parent (Resolve-Path $localPath)

            if (Test-Path $localPath -PathType Leaf)
            {
                $files = $localPath
            }
            else
            {
                $files = Get-ChildItem $localPath -Recurse -File | Select-Object -ExpandProperty FullName
            }

            foreach ($file in $files)
            {
                $entryName = $file.Replace(($parentPath + "\"), "")
                Write-Host ("Adding {0}..." -f $entryName)
                [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $file, $entryName) | Out-Null
            }
        }

        $zip.Dispose()
    }

    Write-Host ("Archive {0} created, uploading..." -f $archiveName)

    # Load WinSCP .NET assembly
    $assemblyPath = if ($env:WINSCP_PATH) { $env:WINSCP_PATH } else { $PSScriptRoot }
    Add-Type -Path (Join-Path $assemblyPath "WinSCPnet.dll")

    # Setup session options
    $sessionOptions = New-Object WinSCP.SessionOptions
    $sessionOptions.ParseUrl($sessionUrl)

    $session = New-Object WinSCP.Session

    try
    {
        $session.SessionLogPath = $sessionLogPath

        # Connect
        $session.Open($sessionOptions)

        $session.PutFiles($session.EscapeFileMask($archivePath), $remotePath).Check()

        Write-Host ("Archive {0} uploaded." -f $archiveName)

        & "$env:WINSCP_PATH\WinSCP.exe" "$sessionUrl" /refresh "$remotePath"
    }
    finally
    {
        # Disconnect, clean up
        $session.Dispose()
    }

    Remove-Item $archivePath
    $result = 0
}
catch [Exception]
{
    Write-Host ("Error: {0}" -f $_.Exception.Message)
    $result = 1
}

# Pause if -pause switch was used
if ($pause)
{
    Write-Host "Press any key to exit..."
    [System.Console]::ReadKey() | Out-Null
}

exit $result
