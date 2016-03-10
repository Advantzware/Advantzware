/* aoaBin.p */

/* ** temp-table definitions **************************************** */

/* ** function declarations ***************************************** */
FUNCTION fGetServerConnectionContext RETURNS CHARACTER (ipcType AS CHARACTER):
    DEFINE VARIABLE cContext AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE idx AS INTEGER     NO-UNDO.

    ASSIGN
        cContext = SESSION:SERVER-CONNECTION-CONTEXT
        idx = LOOKUP(ipcType,cContext,"|")
        .

    OUTPUT TO 'testparam.txt' APPEND.
    PUT UNFORMATTED '[' STRING(TIME.'hh:mm:ss am') '] ' cContact SKIP.
    OUTPUT CLOSE.

    IF NUM-ENTRIES(cContext,"|") GT 1 AND idx NE 0 THEN
    cContext = ENTRY(idx + 1,cContext,"|").

    RETURN cContext.
END FUNCTION.

FUNCTION fGetCompanySCC RETURNS CHARACTER:
    OUTPUT TO 'testparam.txt' APPEND.
    PUT UNFORMATTED '[' STRING(TIME.'hh:mm:ss am') '] Company' SKIP.
    OUTPUT CLOSE.
    RETURN fGetServerConnectionContext ("Company").
END FUNCTION.

FUNCTION fGetUserIDSCC RETURNS CHARACTER:
    RETURN fGetServerConnectionContext ("UserID").
END FUNCTION.

FUNCTION fGetNameSCC RETURNS CHARACTER:
    RETURN fGetServerConnectionContext ("Name").
END FUNCTION.

/* ** procedure declarations **************************************** */
