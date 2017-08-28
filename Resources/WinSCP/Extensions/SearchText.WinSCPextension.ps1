# @name         &Search for Text...
# @command      powershell.exe -ExecutionPolicy Bypass -File "%EXTENSION_PATH%" -sessionUrl "!S" -path "!/" -text "%Text%" -wildcard "%Wildcard%" -pause -sessionLogPath "%SessionLogPath%"
# @description  Searches recursively for a text in the current remote directory
# @version      3
# @homepage     https://winscp.net/eng/docs/library_example_recursive_search_text
# @require      WinSCP 5.8.4
# @option       Text -run textbox "Text:"
# @option       Wildcard -run textbox "File mask:" "*.*"
# @option       SessionLogPath -config sessionlogfile
# @optionspage  https://winscp.net/eng/docs/library_example_recursive_search_text#options

param (
    # Use Generate URL function to obtain a value for -sessionUrl parameter.
    $sessionUrl = "sftp://user:mypassword;fingerprint=ssh-rsa-xx-xx-xx@example.com/",
    [Parameter(Mandatory = $True)]
    $path,
    [Parameter(Mandatory = $True)]
    $text,
    $wildcard = "*.*",
    $sessionLogPath = $Null,
    [Switch]
    $pause
)

try
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

        # Recursivelly enumerate files to grep
        $fileInfos =
            $session.EnumerateRemoteFiles(
                $path, $wildcard, [WinSCP.EnumerationOptions]::AllDirectories)

        foreach ($fileInfo in $fileInfos)
        {
            # Action on match

            # Modify the code below if you want to do another task with
            # matching files, instead of grepping their contents

            Write-Host ("File {0} matches mask, searching contents..." -f $fileInfo.FullName)
            $tempPath = (Join-Path $env:temp $fileInfo.Name)
            # Download file to temporary directory
            $transferResult = $session.GetFiles($session.EscapeFileMask($fileInfo.FullName), $tempPath)
            # Did the download succeeded?
            if (!$transferResult.IsSuccess)
            {
                # Print error (but continue with other files)
                Write-Host $transferResult.Failures[0].Message
            }
            else
            {
                # Search and print lines containing "text".
                # Use -Pattern instead of -SimpleMatch for regex search
                $matchInfo = Select-String -Path $tempPath -SimpleMatch $text
                # Print the results
                foreach ($match in $matchInfo)
                {
                    Write-Host ($fileInfo.FullName + ":" + $match.LineNumber + ":" + $match.Line)
                }
                # Delete temporary local copy
                Remove-Item $tempPath
            }
        }
    }
    finally
    {
        # Disconnect, clean up
        $session.Dispose()
    }

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
