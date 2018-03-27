# @name         Generate &HTTP URL
# @command      powershell.exe -ExecutionPolicy Bypass -STA -NoProfile -File "%EXTENSION_PATH%" -webRoot "%WebRoot%" -rootPath "%RootPath%" -hostName "%HostName%" -serverName "!@" -path "!/!" %Https% %Pause% %Clipboard% %Open%
# @description  Generates HTTP URL of the selected file
# @flag         RemoteFiles
# @version      1
# @homepage     https://winscp.net/eng/docs/extension_generate_http_url
# @require      WinSCP 5.9.3
# @option       - -site group "URL"
# @option         - -site label "These options are site-specific."
# @option         WebRoot -site textbox "&Web root path"
# @option         Https -site checkbox "Use HTTP&S" "" "-https"
# @option         RootPath -site textbox "&URL root path (optional)"
# @option         HostName -site textbox "&Web server hostname override (optional)"
# @option       - group "Options"
# @option         Pause checkbox "Display URL in console" "-pause" "-pause"
# @option         Clipboard checkbox "Copy URL to clipboard" "-clipboard" "-clipboard"
# @option         Open checkbox "Open URL in web browser" "" "-open"
# @optionspage  https://winscp.net/eng/docs/extension_generate_http_url#options

param (
    [Parameter(Mandatory = $True)]
    $webRoot,
    $rootPath,
    $hostName,
    $serverName,
    [Parameter(Mandatory = $True)]
    $path,
    [Switch] 
    $https,
    [Switch] 
    $pause,
    [Switch] 
    $clipboard,
    [Switch] 
    $open
)

try
{
    if (!$webRoot -or ($webRoot.SubString($webRoot.Length - 1, 1) -ne "/"))
    {
        $webRoot += "/"
    }

    if (($path.Length -lt $webRoot.length) -or
        ($path.SubString(0, $webRoot.Length) -ne $webRoot))
    {
        throw "The path $path is not under web root $webRoot."
    }
    
    if ($rootPath)
    {
        if ($rootPath.SubString($rootPath.Length - 1) -ne "/")
        {
            $rootPath += "/"
        }
    }
    else
    {
        $rootPath = "/"
    }

    $urlPath = $path.SubString($webRoot.Length)
 
    if ($https)
    { 
        $protocol = "https://"
    }
    else
    {
        $protocol = "http://"
    }

    if (!$hostName)
    {
        $hostName = $serverName
    }
    
    $url = "$protocol$hostName$rootPath$urlPath"

    Write-Host $url

    if ($clipboard)
    {
        Add-Type -Assembly PresentationCore
        [Windows.Clipboard]::SetText($url) 
    }

    if ($open)
    {
        Start-Process $url
    }

    $result = 0
}
catch [Exception]
{
    Write-Host $_.Exception.Message
    $result = 1
    $pause = $true
}

if ($pause)
{
    Write-Host "Press any key to exit..."
    [System.Console]::ReadKey() | Out-Null
}

exit $result
