/* ctrl-f.i - rstark - 4.24.2019 */

DEFINE VARIABLE cCtrlFErrorMsg   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFObjectName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFUserID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCtrlFResponse   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCtrlFRunAudit   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rCtrlFRowID      AS ROWID     NO-UNDO.
DEFINE VARIABLE hSession         AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.


IF FRAME-DB EQ "" AND FRAME-FILE EQ "" THEN
    MESSAGE
        "Widget Object: ~"" + FRAME-FIELD + "~""
        VIEW-AS ALERT-BOX TITLE "CTRL-F Object View".
ELSE DO:
    IF NOT VALID-HANDLE(hSession) THEN 
        RUN system/session.p PERSISTENT SET hSession.
    RUN spGetCompany IN hSession (OUTPUT cCompany).
        
    RUN spDynAuditField (
        cCompany,
        FRAME-DB,
        FRAME-FILE,
        FRAME-FIELD,
        OUTPUT rCtrlFRowID,
        OUTPUT cCtrlFErrorMsg,
        OUTPUT lCtrlFRunAudit
        ).
    IF lCtrlFRunAudit THEN DO:
        FIND FIRST dynParamValue NO-LOCK
             WHERE ROWID(dynParamValue) EQ rCtrlFRowID
             NO-ERROR.
        IF AVAILABLE dynParamValue THEN
        RUN AOA/Jasper.p (
            dynParamValue.subjectID,
            dynParamValue.user-id,
            dynParamValue.prgmName,
            dynParamValue.paramValueID,
            NO /* suppress parameters */
            ).
    END. /* if run audit */
    ELSE DO:
        ASSIGN
            cCtrlFErrorMsg   = CHR(10) + CHR(10) + cCtrlFErrorMsg
            cCtrlFObjectName = "Database: " + FRAME-DB + CHR(10) 
                        + "Table: " + FRAME-FILE + CHR(10)
                        + "Field: " + FRAME-FIELD
                        + (IF FRAME-INDEX NE 0 THEN "["
                        + STRING(FRAME-INDEX) + "]" ELSE "")
                        .
        MESSAGE
            cCtrlFObjectName
            cCtrlFErrorMsg
            VIEW-AS ALERT-BOX TITLE "CTRL-F Field View".
    END. /* else */
END. /* else */
