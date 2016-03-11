/* aoaParamDefs.i */

DEFINE VARIABLE aoaID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaTitle   AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaHost    AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaPort    AS INTEGER   NO-UNDO.
DEFINE VARIABLE aoaParam   AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaURL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaUserID  AS CHARACTER NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ ipcCompany
       AND sys-ctrl.name    EQ "aoaHost"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = ipcCompany
        sys-ctrl.name     = "aoaHost"
        sys-ctrl.char-fld = "localhost"
        sys-ctrl.int-fld  = 5162
        sys-ctrl.descrip  = "Advantzware Open Analytics Host IP Address"
        .
END.
FIND CURRENT sys-ctrl NO-LOCK.

ASSIGN
    aoaID      = ENTRY(1,ipcParamStr)
    aoaName    = ENTRY(2,ipcParamStr)
    aoaTitle   = ENTRY(3,ipcParamStr)
    aoaType    = ENTRY(4,ipcParamStr)
    aoaHost    = sys-ctrl.char-fld
    aoaPort    = sys-ctrl.int-fld
    aoaParam   = REPLACE(PROGRAM-NAME(2),aoaName,"Param/" + aoaName)
    aoaParam   = REPLACE(aoaParam,".p",".w")
    aoaCompany = ipcCompany
    aoaUserID  = USERID("NoSweat")
    aoaURL     = "http://" + aoaHost + ":80/AdvantzwareOA/"
               + aoaType + ".aspx?ID=" + aoaID
    .

IF aoaType EQ "Document" THEN
ASSIGN aoaURL = aoaURL + "^&refresh=true^&connection=AdvantzwareOA".

SESSION:SET-WAIT-STATE('').
