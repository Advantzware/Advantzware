/* aoaParamDefs.i */

DEFINE VARIABLE aoaID           AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaProgramID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaTitle        AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaColumns      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE aoaCustListForm AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaHost         AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaPort         AS INTEGER   NO-UNDO.
DEFINE VARIABLE aoaParam        AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaURL          AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaLocation     AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaUserID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaExcelOnly    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cProgramID      AS CHARACTER NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ ipcCompany
       AND sys-ctrl.name    EQ "asAOA"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = ipcCompany
        sys-ctrl.name     = "asAOA"
        sys-ctrl.char-fld = "localhost"
        sys-ctrl.int-fld  = 5162
        sys-ctrl.descrip  = "Advantzware Open Analytics AppServer"
        .
END.
FIND CURRENT sys-ctrl NO-LOCK.

ASSIGN
    aoaID           = ENTRY(1,ipcParamStr)
    aoaProgramID    = ENTRY(2,ipcParamStr)
    aoaTitle        = ENTRY(3,ipcParamStr)
    aoaType         = ENTRY(4,ipcParamStr)
    aoaColumns      = ENTRY(5,ipcParamStr) EQ "YES"
    aoaCustListForm = ENTRY(6,ipcParamStr)
    aoaExcelOnly    = ENTRY(7,ipcParamStr) EQ "aoaExcelOnly"
    aoaHost         = IF OS-GETENV("COMPUTERNAME") NE sys-ctrl.char-fld THEN sys-ctrl.char-fld
                      ELSE OS-GETENV("COMPUTERNAME")
    aoaPort         = sys-ctrl.int-fld
    aoaParam        = REPLACE(PROGRAM-NAME(2),aoaProgramID,"param/" + aoaProgramID)
    aoaParam        = REPLACE(aoaParam,".p",".w")
    aoaCompany      = ipcCompany
    aoaLocation     = ipcLocation
    aoaUserID       = USERID("ASI")
    .

/* used for testing in AppBuilder */
IF NUM-ENTRIES(ipcParamStr) EQ 8  THEN
aoaParam = ENTRY(8,ipcParamStr).

cProgramID = aoaTitle + ".rpa".

SESSION:SET-WAIT-STATE('').

