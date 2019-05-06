# @name         &Compare Files
# @command      powershell.exe -ExecutionPolicy Bypass -File "%EXTENSION_PATH%" ^
#                   -localPath "!^!" -remotePath "!" -tool "%Tool%"
# @description  Compares the selected local and remote path using ^
#                   an external file comparison tool
# @flag         ShowResultsInMsgBox 
# @flag         ApplyToDirectories
# @version      1
# @shortcut     Shift+Ctrl+Alt+C
# @homepage     https://winscp.net/eng/docs/extension_compare_files
# @require      WinSCP 5.13.4
# @option       - group "Options"
# @option         Tool dropdownlist "Select &file comparison tool:" "" ^
#                     "=Automatic" "ExamDiff Pro" "Beyond Compare" "KDiff3" "WinMerge" "fc"
# @optionspage  https://winscp.net/eng/docs/extension_compare_files#options
 
param (
    [Parameter(Mandatory = $True)]
    $localPath,
    [Parameter(Mandatory = $True)]
    $remotePath,
    $tool
)
 
try
{
    $pf = "%PF%"
    $tools = (
        ("ExamDiff Pro", "$pf\ExamDiff Pro\ExamDiff.exe"),
        ("Beyond Compare", "$pf\Beyond Compare 4\BCompare.exe"),
        ("KDiff3", "$pf\KDiff3\kdiff3.exe"),
        ("WinMerge", "$pf\WinMerge\WinMergeU.exe"),
        ("fc", "fc.exe")
    )

    $path = $Null

    foreach ($t in $tools)
    {
        $tname = $t[0]
        if ((-not $tool) -or
            ($tname -eq $tool))
        {
            $path = $Null
            $tpath = $t[1]
            if ($tpath.Contains($pf))
            {
                $path64 = $tpath.Replace($pf, $env:ProgramW6432)
                # Only true as long as WinSCP is 32-bit
                $path32 = $tpath.Replace($pf, $env:ProgramFiles)
                if (Test-Path $path64)
                {
                    $path = $path64
                }
                elseif (Test-Path $path32)
                {
                    $path = $path32
                }
            }
            else
            {
                $path = $tpath
            }

            if ($path)
            {
                $tool = $tname
                break
            }
            elseif ($tool)
            {
                throw "Cannot find $tool"
            }
        }
    }

    if (-not $path)
    {
        throw "Unknown tool $tool"
    }

    if ($tool -eq "fc")
    {
        Start-Process $env:ComSpec `
            -ArgumentList "/c", $path, $localPath, $remotePath, "&", "pause" -Wait
    }
    else
    {
        Start-Process $path -ArgumentList $localPath, $remotePath -Wait
    }

    $result = 0
}
catch
{
    Write-Host "Error: $($_.Exception.Message)"
    $result = 1
}
 
exit $result
