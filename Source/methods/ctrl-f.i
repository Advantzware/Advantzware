/* ctrl-f.i - rstark - 4.24.2019 */

DEFINE VARIABLE cCtrlFAuditKey     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFErrorMsg     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFObjectName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFPrgmName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFUserID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFSessionParam AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFSessionValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlFieldLabel    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCtrlTableLabel    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hCtrlFTable        AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCtrlFResponse     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCtrlFRunAudit     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rCtrlFRowID        AS ROWID     NO-UNDO.

ASSIGN
    cCtrlFPrgmName     = ENTRY(1,ENTRY(NUM-ENTRIES(PROGRAM-NAME(1)," "),PROGRAM-NAME(1)," "),".")
    cCtrlFSessionParam = cCtrlFPrgmName + "|" + FRAME-FILE
    .
IF FRAME-DB EQ "" AND FRAME-FILE EQ "" THEN
    MESSAGE
        "Widget Object: ~"" + FRAME-FIELD + "~""
    VIEW-AS ALERT-BOX TITLE "CTRL-F Object View (" + cCtrlFPrgmName + ")".
ELSE DO:
    RUN spGetSessionParam (cCtrlFSessionParam, OUTPUT cCtrlFSessionValue).
    IF cCtrlFSessionValue NE "" THEN DO:
        RUN spGetSessionParam ("Company", OUTPUT cCtrlFCompany).
        RUN spDynAuditField (
            cCtrlFCompany,
            FRAME-DB,
            FRAME-FILE,
            FRAME-FIELD,
            cCtrlFSessionValue,
            OUTPUT rCtrlFRowID,
            OUTPUT cCtrlFErrorMsg,
            OUTPUT lCtrlFRunAudit
            ).
    END. /* if session has a value */
    ELSE
    cCtrlFErrorMsg = "Audit Field History NOT Enabled for Module: " + cCtrlFPrgmName.
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
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name EQ FRAME-FILE
             NO-ERROR.
        FIND FIRST ASI._field OF ASI._file NO-LOCK
             WHERE ASI._field._field-name EQ FRAME-FIELD
             NO-ERROR. 
        ASSIGN
            cCtrlFieldLabel  = IF AVAILABLE ASI._field AND ASI._field._Label      NE ? THEN ASI._field._Label      ELSE "" 
            cCtrlTableLabel  = IF AVAILABLE ASI._file  AND ASI._file._file-label  NE ? THEN ASI._file._file-label  ELSE ""
            cCtrlFErrorMsg   = CHR(10) + CHR(10) + cCtrlFErrorMsg
            cCtrlFObjectName = "Database: " + FRAME-DB + CHR(10) 
                             + "Table: " + FRAME-FILE + " - "
                             + cCtrlTableLabel + CHR(10)
                             + "Field: " + FRAME-FIELD
                             + (IF FRAME-INDEX NE 0 THEN "["
                             + STRING(FRAME-INDEX) + "]" ELSE "") + " - "
                             + cCtrlFieldLabel
                             + CHR(10) + CHR(10)
                             + "Audit Key: " + cCtrlFSessionValue
                             .
        MESSAGE
            cCtrlFObjectName
            cCtrlFErrorMsg
        VIEW-AS ALERT-BOX TITLE "CTRL-F Field View (" + cCtrlFPrgmName + ")".
    END. /* else */
END. /* else */
