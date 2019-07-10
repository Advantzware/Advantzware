/* ctrl-f.i - rstark - 4.24.2019 */

DEFINE VARIABLE cCtrlFAuditKey   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFCompany    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFErrorMsg   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFIdxFields  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFObjectName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFUserID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hCtrlFTable      AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCtrlFResponse   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCtrlFRunAudit   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rCtrlFRowID      AS ROWID     NO-UNDO.

IF FRAME-DB EQ "" AND FRAME-FILE EQ "" THEN
    MESSAGE
        "Widget Object: ~"" + FRAME-FIELD + "~""
    VIEW-AS ALERT-BOX TITLE "CTRL-F Object View".
ELSE DO:
    RUN spGetSessionParam ("Company", OUTPUT cCtrlFCompany).
/*    &IF "{&FIRST-EXTERNAL-TABLE}" NE "" &THEN                   */
/*    RUN nosweat/primFlds.p (FRAME-FILE, OUTPUT cCtrlFIdxFields).*/
/*    hCtrlFTable = {&FIRST-EXTERNAL-TABLE}:HANDLE.               */
/*    &ENDIF                                                      */
    RUN spDynAuditField (
        cCtrlFCompany,
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
            NO /* show parameters */
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
