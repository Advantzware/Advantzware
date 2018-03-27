# @name         &Keep Local Directory up to Date...
# @command      powershell.exe -ExecutionPolicy Bypass -File "%EXTENSION_PATH%" -sessionUrl "!S" -localPath "%LocalPath%" -remotePath "%RemotePath%" %Delete% %Beep% %ContinueOnError% -interval "%Interval%" -pause -sessionLogPath "%SessionLogPath%"
# @description  Periodically scans for changes in a remote directory and reflects them on a local directory
# @version      3
# @homepage     https://winscp.net/eng/docs/library_example_keep_local_directory_up_to_date
# @require      WinSCP 5.9.2
# @option       - -run group "Directories"
# @option         RemotePath -run textbox "&Watch for changes in the remote directory:" "!/"
# @option         LocalPath -run textbox "... &and automatically reflect them on the local directory:" "!\"
# @option       - -config -run group "Options"
# @option         Delete -config -run checkbox "&Delete files" "" -delete 
# @option         Beep -config -run checkbox "&Beep on change" "" -beep
# @option         ContinueOnError -config -run checkbox "&Continue on &error" "" -continueOnError
# @option         Interval -config -run textbox "&Interval (in seconds):" "30"
# @option       - -config group "Logging"
# @option         SessionLogPath -config sessionlogfile
# @optionspage  https://winscp.net/eng/docs/library_example_keep_local_directory_up_to_date#options

param (
    # Use Generate URL function to obtain a value for -sessionUrl parameter.
    $sessionUrl = "sftp://user:mypassword;fingerprint=ssh-rsa-xx-xx-xx@example.com/",
    [Parameter(Mandatory = $True)]
    $localPath,
    [Parameter(Mandatory = $True)]
    $remotePath,
    [Switch]
    $delete,
    [Switch]
    $beep,
    [Switch]
    $continueOnError,
    $sessionLogPath = $Null,
    $interval = 30,
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
    
    # Optimization
    # (do not waste time enumerating files, if you do not need to scan for deleted files)
    if ($delete) 
    {
        $localFiles = Get-ChildItem -Recurse -Path $localPath
    }

    try
    {
        $session.SessionLogPath = $sessionLogPath

        Write-Host "Connecting..."
        $session.Open($sessionOptions)

        while ($True)
        {
            Write-Host "Synchronizing changes..."
            $result = $session.SynchronizeDirectories([WinSCP.SynchronizationMode]::Local, $localPath, $remotePath, $delete)

            $changed = $False

            if (!$result.IsSuccess)
            {
              if ($continueOnError)
              {
                Write-Host ("Error: {0}" -f $result.Failures[0].Message)
                $changed = $True
              }
              else
              {
                $result.Check()
              }
            }

            # Print updated files
            foreach ($download in $result.Downloads)
            {
                Write-Host ("{0} <= {1}" -f $download.Destination, $download.FileName)
                $changed = $True
            }

            if ($delete)
            {
                # scan for removed local files (the $result does not include them)
                $localFiles2 = Get-ChildItem -Recurse -Path $localPath

                if ($localFiles)
                {
                    $changes = Compare-Object -DifferenceObject $localFiles2 -ReferenceObject $localFiles
                
                    $removedFiles =
                        $changes |
                        Where-Object -FilterScript { $_.SideIndicator -eq "<=" } |
                        Select-Object -ExpandProperty InputObject

                    # Print removed local files
                    foreach ($removedFile in $removedFiles)
                    {
                        Write-Host ("{0} deleted" -f $removedFile)
                        $changed = $True
                    }
                }

                $localFiles = $localFiles2
            }

            if ($changed)
            {
                if ($beep)
                {
                    [System.Console]::Beep()
                }
            }
            else
            {
                Write-Host "No change."
            }
            
            Write-Host "Waiting for $interval seconds, press Ctrl+C to abort..."
            $wait = [int]$interval
            # Wait for 1 second in a loop, to make the waiting breakable
            while ($wait -gt 0)
            {
                Start-Sleep -Seconds 1
                $wait--
            }

            Write-Host
        }
    }
    finally
    {
        Write-Host "Disconnecting..."
        # Disconnect, clean up
        $session.Dispose()
    }
}
catch [Exception]
{
    Write-Host ("Error: {0}" -f $_.Exception.Message)
}

# Pause if -pause switch was used
if ($pause)
{
    Write-Host "Press any key to exit..."
    [System.Console]::ReadKey() | Out-Null
}

# Never exits cleanly
exit 1
