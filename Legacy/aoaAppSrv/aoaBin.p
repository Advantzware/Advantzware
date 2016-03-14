/* aoaBin.p */

/* ** temp-table definitions **************************************** */

/* ** function declarations ***************************************** */

/* ** procedure declarations **************************************** */


















/* saving these for future reference

FUNCTION fGetServerConnectionContext RETURNS CHARACTER (ipcType AS CHARACTER):
    DEFINE VARIABLE cContext AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE idx AS INTEGER     NO-UNDO.

    ASSIGN
        cContext = SESSION:SERVER-CONNECTION-CONTEXT
        idx = LOOKUP(ipcType,cContext,"|")
        .

    IF NUM-ENTRIES(cContext,"|") GT 1 AND idx NE 0 THEN
    cContext = ENTRY(idx + 1,cContext,"|").

    RETURN cContext.
END FUNCTION.

FUNCTION fGetCompanySCC RETURNS CHARACTER:
    RETURN fGetServerConnectionContext ("Company").
END FUNCTION.

FUNCTION fGetNameSCC RETURNS CHARACTER:
    RETURN fGetServerConnectionContext ("Name").
END FUNCTION.

FUNCTION fGetParamValue RETURNS CHARACTER (ipcField AS CHARACTER):
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.

    IF AVAILABLE user-print THEN
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF TRIM(user-print.field-name[idx]) EQ ipcField THEN DO:
            cReturnValue = user-print.field-value[idx].
            LEAVE.
        END. /* found screen object */
    END. /* do idx */

    RETURN cReturnValue.
END FUNCTION.

FUNCTION fGetUserIDSCC RETURNS CHARACTER:
    RETURN fGetServerConnectionContext ("UserID").
END FUNCTION.
*/
/*
PROCEDURE getParamValues:
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserID  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName    AS CHARACTER NO-UNDO.

    ASSIGN
        cCompany = fGetCompanySCC()
        cUserID  = fGetUserIDSCC()
        cName    = fGetNameSCC()
        .
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ cCompany
           AND user-print.program-id EQ cName
           AND user-print.user-id    EQ cUserID
           AND user-print.batch      EQ ""
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN RETURN.

END PROCEDURE.
*/
