DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.

ASSIGN
    cEmail = SEARCH("cmail.exe")
    cEmail = cEmail + " -host:"
    cEmail = cEmail + "ron.stark@advantzware.com:Chester1!@"
    cEmail = cEmail + "outlook.office365.com:25"
    cEmail = cEmail + " -to:ron.stark@advantzware.com"
    cEmail = cEmail + " -from:ron@thestarkgroup.com"
    cEmail = cEmail + " ~"-subject:Test cemail.exe~""
    cEmail = cEmail + " ~"-body:Body Text~""
    .

MESSAGE
    "cEmail:" cEmail
VIEW-AS ALERT-BOX.

OS-COMMAND NO-WAIT start VALUE(cEmail).
