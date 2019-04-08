/* applhelp.p - rstark - rewritten 4.6.2019 */

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE cFrameDB      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFrameField   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFrameFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLookupField  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLookupPrgm   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cObjectName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturnValues AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
DEFINE VARIABLE lDeveloper    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lResponse     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rRecID        AS RECID     NO-UNDO.

IF KEYLABEL(LASTKEY) EQ "CTRL-F" THEN
    IF FRAME-DB EQ "" AND FRAME-FILE EQ "" THEN
        MESSAGE
            "Object: ~"" + FRAME-FIELD + "~""
        VIEW-AS ALERT-BOX TITLE "CTRL-F Object View".
    ELSE DO:
        cObjectName = FRAME-DB + "." + FRAME-FILE + "." + FRAME-FIELD +
            (IF FRAME-INDEX NE 0 THEN "[" + STRING(FRAME-INDEX) + "]" ELSE "").
        MESSAGE
            "Field: ~"" + cObjectName + "~""
        VIEW-AS ALERT-BOX TITLE "CTRL-F Field View".
    END. /* else */
ELSE
IF KEYLABEL(LASTKEY) EQ "CTRL-P" THEN
RUN Get_Procedure IN Persistent-Handle ("popups.",OUTPUT run-proc,YES).
ELSE
IF KEYLABEL(LASTKEY) EQ "F1" THEN DO: /* F1 function key */
    ASSIGN
        cFrameDB    = FRAME-DB
        cFrameFile  = FRAME-FILE
        cFrameField = FRAME-FIELD
        cUserID     = USERID("ASI")
        cPrgmName   = PROGRAM-NAME(2)
        cPrgmName   = SUBSTRING(cPrgmName,1,INDEX(cPrgmName,"."))
        cPrgmName   = ENTRY(NUM-ENTRIES(cPrgmName," "),cPrgmName," ")
        lDeveloper  = cUserID NE "" AND CAN-DO(g_developer,cUserID)
        .
    RUN pGetDynLookup.
    IF AVAILABLE dynLookup THEN DO:
        IF lDeveloper THEN DO:
            MESSAGE
                "Update DYNAMIC Lookup?" SKIP(1)
                "~"Cancel~" to Remove Lookup from this Field."
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
            UPDATE lResponse.
            CASE lResponse:
                WHEN YES THEN
                RUN pRunDynLookup (0, "", 0).
                WHEN NO THEN
                RUN pRunDynLookup (
                    dynLookup.subjectID,
                    dynLookup.user-id,
                    dynLookup.paramValueID
                    ). 
                OTHERWISE DO:
                    MESSAGE
                        "Remove DYNAMIC Lookup from this Field?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lResponse.
                    IF lResponse THEN
                    DO TRANSACTION:
                        FIND CURRENT dynLookup EXCLUSIVE-LOCK.
                        DELETE dynLookup.
                    END. /* transaction */
                END. /* otherwise */
            END CASE.
        END. /* if developer */
        ELSE
        RUN pRunDynLookup (
            dynLookup.subjectID,
            dynLookup.user-id,
            dynLookup.paramValueID
            ). 
    END. /* if avail dynlookup */
    ELSE DO:
        RUN pGetLookups.
        IF AVAILABLE lookups THEN DO:
            IF lDeveloper THEN DO:
                MESSAGE
                    "Update FRAMEWORK Lookup ~"" + lookups.prgmname + "~"?" SKIP(1)
                    "~"Cancel~" to Remove Lookup from this Field."
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                UPDATE lResponse.
                CASE lResponse:
                    WHEN YES THEN
                    RUN pRunLookup (lookups.prgmname).
                    WHEN NO THEN
                    RUN VALUE("lookups/" + lookups.prgmname + "p").
                    OTHERWISE DO:
                        MESSAGE
                            "Remove FRAMEWORK Lookup from this Field?"
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                        UPDATE lResponse.
                        IF lResponse THEN
                        DO TRANSACTION:
                            FIND CURRENT lookups EXCLUSIVE-LOCK.
                            DELETE lookups.
                        END. /* transaction */
                    END. /* otherwise */
                END CASE.
            END. /* if developer */
            ELSE
            RUN VALUE("lookups/" + lookups.prgmname + "p").
        END. /* if avail lookups */
        ELSE IF lDeveloper THEN DO:
            MESSAGE
                "Create DYNAMIC Lookup for :" SKIP(1)
                "Database :" cFrameDB SKIP
                "Table :" cFrameFile SKIP
                "Field :" cFrameField SKIP(1)
                "Called From:" cPrgmName
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE lResponse.
            CASE lResponse:
                WHEN YES THEN
                RUN pRunDynLookup (0, "", 0).
                WHEN NO THEN DO:
                    MESSAGE
                        "Create FRAMEWORK Lookup for :" SKIP(1)
                        "Database :" cFrameDB SKIP
                        "Table :" cFrameFile SKIP
                        "Field :" cFrameField SKIP(1)
                        "Called From:" cPrgmName
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lResponse.
                    IF lResponse THEN DO:
                        RUN Get_Procedure IN Persistent-Handle ("lkupdate.",OUTPUT run-proc,NO).
                        IF run-proc NE "" THEN DO:
                            RUN VALUE(run-proc) (OUTPUT cLookupPrgm).
                            RUN pRunLookup (cLookupPrgm).
                        END. /* if run-proc */
                    END. /* if lresponse */
                END. /* no */
            END CASE.
        END. /* if ldeveloper */
        ELSE
        MESSAGE
            "No Lookup for this Field Exists"
        VIEW-AS ALERT-BOX INFORMATION.
    END. /* else */
END. /* if f1 */ 

PROCEDURE pGetDynLookup:
    FIND FIRST dynLookup NO-LOCK
         WHERE dynLookup.prgmName  EQ cPrgmName
           AND dynLookup.tableDB   EQ cFrameDB
           AND dynLookup.tableName EQ cFrameFile
           AND dynLookup.fieldName EQ cFrameField
         NO-ERROR.
END PROCEDURE.

PROCEDURE pGetLookups:
    FIND FIRST lookups NO-LOCK
         WHERE lookups.frame_db    EQ cFrameDB
           AND lookups.frame_file  EQ cFrameFile
           AND lookups.frame_field EQ cFrameField 
         NO-ERROR.
END PROCEDURE.

PROCEDURE pRunDynLookup:
    DEFINE INPUT PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiParamValueID AS INTEGER   NO-UNDO.

    RUN AOA/dynLookup.p (
        g_company,
        ipiSubjectID,
        ipcUserID,
        ipiParamValueID,
        OUTPUT cReturnValues,
        OUTPUT cLookupField,
        OUTPUT rRecID
        ).
    IF rRecID NE ? THEN DO:
        IF ipiSubjectID NE 0 THEN
        FOCUS:SCREEN-VALUE = cLookupField.
        ELSE
        DO TRANSACTION:
            IF NOT AVAILABLE dynLookup THEN DO:
                CREATE dynLookup.
                ASSIGN
                    dynLookup.prgmName  = cPrgmName
                    dynLookup.tableDB   = cFrameDB
                    dynLookup.tableName = cFrameFile
                    dynLookup.fieldName = cFrameField
                    .
            END. /* if avail */
            ELSE
            FIND CURRENT dynLookup EXCLUSIVE-LOCK.
            ASSIGN
                idx                    = LOOKUP("subjectID",cReturnValues,"|")
                dynLookup.subjectID    = INTEGER(ENTRY(idx + 1,cReturnValues,"|"))
                idx                    = LOOKUP("user-id",cReturnValues,"|")
                dynLookup.user-id      = ENTRY(idx + 1,cReturnValues,"|")
                idx                    = LOOKUP("paramValueID",cReturnValues,"|")
                dynLookup.paramValueID = INTEGER(ENTRY(idx + 1,cReturnValues,"|"))
                .
            FIND CURRENT dynLookup NO-LOCK.
            RUN pRunDynLookup (
                dynLookup.subjectID,
                dynLookup.user-id,
                dynLookup.paramValueID
                ).
        END. /* trans */
    END. /* if rrecid */
END PROCEDURE.

PROCEDURE pRunLookup:
    DEFINE INPUT PARAMETER ipcLookupPrgm AS CHARACTER NO-UNDO.
                                                                                 
    RUN Get_Procedure IN Persistent-Handle ("lookups.",OUTPUT run-proc,NO).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (ipcLookupPrgm).
    IF NOT AVAILABLE lookups THEN
    DO TRANSACTION:
        CREATE lookups.
        ASSIGN
            lookups.frame_db    = cFrameDB
            lookups.frame_file  = cFrameFile
            lookups.frame_field = cFrameField
            lookups.prgmname    = ipcLookupPrgm
            .
    END. 
    RUN VALUE("lookups/" + lookups.prgmname + "p").
END PROCEDURE.
