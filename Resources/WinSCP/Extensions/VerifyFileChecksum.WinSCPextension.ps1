# @name         Verify &Checksum
# @command      powershell.exe -ExecutionPolicy Bypass -File "%EXTENSION_PATH%" -sessionUrl "!S" -localPath "!^!" -remotePath "!/!" -pause -sessionLogPath "%SessionLogPath%"
# @description  Compares checksums of the selected local and remote file
# @flag         RemoteFiles
# @version      3
# @homepage     https://winscp.net/eng/docs/library_example_verify_file_checksum
# @require      WinSCP 5.8.4
# @option       SessionLogPath -config sessionlogfile
# @optionspage  https://winscp.net/eng/docs/library_example_verify_file_checksum#options
 
param (
    # Use Generate URL function to obtain a value for -sessionUrl parameter.
    $sessionUrl = "sftp://user:mypassword;fingerprint=ssh-rsa-xx-xx-xx@example.com/",
    [Parameter(Mandatory = $True)]
    $localPath,
    [Parameter(Mandatory = $True)]
    $remotePath,
    $sessionLogPath = $Null,
    [Switch]
    $pause
)
 
try
{
    Write-Host $localPath
 
    # Calculate local file checksum
    $sha1 = [System.Security.Cryptography.SHA1]::Create()
    $localStream = [System.IO.File]::OpenRead($localPath)
    $localChecksum = [System.BitConverter]::ToString($sha1.ComputeHash($localStream))
 
    Write-Host $localChecksum
    
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
        
        Write-Host $remotePath
 
        # Calculate remote file checksum
        $remoteChecksum = [System.BitConverter]::ToString($session.CalculateFileChecksum("sha-1", $remotePath))
        Write-Host $remoteChecksum
    }
    finally
    {
        # Disconnect, clean up
        $session.Dispose()
    }
 
    # Compare cheksums
    if ($localChecksum -eq $remoteChecksum)
    {
        Write-Host "Match"
        $result = 0
    }
    else
    {
        Write-Host "Does NOT match"
        $result = 1
    }
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
