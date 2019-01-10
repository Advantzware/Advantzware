/* dynQuery.p */

DEFINE INPUT PARAMETER iphQuery AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER ipcType  AS CHARACTER NO-UNDO.

DEFINE VARIABLE iRecordCount AS INTEGER NO-UNDO.

iphQuery:QUERY-OPEN().
iphQuery:GET-FIRST().
DO WHILE NOT iphQuery:QUERY-OFF-END:
    iRecordCount = iRecordCount + 1.
    iphQuery:GET-NEXT().
END. /* do while */

MESSAGE
    "iRecordCount:" iRecordCount
    "for" ipcType
VIEW-AS ALERT-BOX.
