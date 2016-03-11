/* aoaConnect.p */

DEFINE INPUT PARAMETER ipcUserID   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcPassword AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcAppInfo  AS CHARACTER NO-UNDO.

/* If you do want to check the username and password */
/* you should do it here, and return error to prevent the connection */

SESSION:SERVER-CONNECTION-CONTEXT = ipcAppInfo.

OUTPUT TO 'testparam.txt' APPEND.
PUT UNFORMATTED
    '[' STRING(TODAY,'99.99.9999') ','
    STRING(TIME,'hh:mm:ss') '] aoaConnect.p'
    SKIP
    '[' STRING(TODAY,'99.99.9999') ','
    STRING(TIME,'hh:mm:ss') '] '
    'SERVER-CONNECTION-ID: ' SESSION:SERVER-CONNECTION-ID
    SKIP
    '[' STRING(TODAY,'99.99.9999') ','
    STRING(TIME,'hh:mm:ss') '] '
    'SERVER-CONNECTION-CONTEXT: ' SESSION:SERVER-CONNECTION-CONTEXT
    SKIP(1)
    .
OUTPUT CLOSE.
