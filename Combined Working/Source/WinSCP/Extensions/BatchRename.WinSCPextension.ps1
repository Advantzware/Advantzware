# @name         Batch &Rename...
# @command      powershell.exe -ExecutionPolicy Bypass -File "%EXTENSION_PATH%" -sessionUrl "!S" -remotePath "!/" -pattern "%Pattern%" -replacement "%Replacement%" -pause -sessionLogPath "%SessionLogPath%" %PreviewMode% !& 
# @description  Renames remote file using regular expression
# @flag         RemoteFiles
# @version      1
# @homepage     https://winscp.net/eng/docs/library_example_advanced_rename
# @require      WinSCP 5.8.4
# @option       - -run group "Rename"
# @option         Pattern -run textbox "Replace file name part matching this pattern:"
# @option         Replacement -run textbox "with:"
# @option       - -run -config group "Options"
# @option         PreviewMode -run -config checkbox "&Preview changes" "-previewMode" "-previewMode"
# @option       - -config group "Logging"
# @option         SessionLogPath -config sessionlogfile
# @optionspage  https://winscp.net/eng/docs/library_example_advanced_rename#options

param (
    # Use Generate URL function to obtain a value for -sessionUrl parameter.
    $sessionUrl = "sftp://user:mypassword;fingerprint=ssh-rsa-xx-xx-xx@example.com/",
    [Parameter(Mandatory = $True)]
    $remotePath,
    [Parameter(Mandatory = $True)]
    $pattern,
    $replacement,
    [Switch]
    $pause,
    $sessionLogPath = $Null,
    [Switch]
    $previewMode,
    [Parameter(Mandatory = $True, ValueFromRemainingArguments = $True, Position = 0)]
    $files
)

try
{
    if ($previewMode)
    {
        $anyChange = $False
        foreach ($file in $files)
        {
            $newName = $file -replace $pattern, $replacement
            Write-Host ("{0} => {1}" -f $file, $newName)
            if ($newName -ne $file)
            {
                $anyChange = $True
            }
        }

        Write-Host
        
        if (!$anyChange)
        {
            Write-Host "No change to be made"
            $continue = $False
        }
        else
        {
            Write-Host -NoNewline "Continue? y/N "
            $key = [System.Console]::ReadKey()
            Write-Host
            Write-Host
            $continue = ($key.KeyChar -eq "y")
            if (!$continue)
            {
                $pause = $False
            }
        }
    }
    else
    {
        $continue = $True
    }
    

    if (!$continue)
    {
        $result = 1
    }
    else
    {
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

            foreach ($file in $files)
            {
                $newName = $file -replace $pattern, $replacement
                Write-Host ("{0} => {1}" -f $file, $newName)

                $fullName = $session.CombinePaths($remotePath, $file)
                $fullNewName = $session.CombinePaths($remotePath, $newName)
                $session.MoveFile($fullName, $fullNewName)
            }
        }
        finally
        {
            # Disconnect, clean up
            $session.Dispose()
        }

        & "$env:WINSCP_PATH\WinSCP.exe" "$sessionUrl" /refresh "$remotePath"
    }

    $result = 0
}
catch [Exception]
{
    Write-Host $_.Exception.Message
    $result = 1
}

if ($pause)
{
    Write-Host "Press any key to exit..."
    [System.Console]::ReadKey() | Out-Null
}

exit $result
