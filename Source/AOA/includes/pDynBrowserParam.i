/* pDynBrowserParam.i */

{AOA/includes/fGetDynParamValue.i}

{AOA/includes/pGetDynParamValue.i}
{AOA/includes/pSetDynParamValue.i "dyn"}

PROCEDURE pDynBrowserParam:
    DEFINE INPUT PARAMETER ipcSubjectAltID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cPrgmName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iParamValueID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSubjectID    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lShowParam    AS LOGICAL   NO-UNDO.
    
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectAltID EQ ipcSubjectAltID
         NO-ERROR.
    IF AVAILABLE dynSubject THEN DO:
        ASSIGN
            iSubjectID = dynSubject.subjectID
            cUserID    = USERID("ASI")
            cPrgmName  = "[" + ipcSubjectAltID + "]"
            .
        RUN pGetDynParamValue (
            iSubjectID,
            cUserID,
            cPrgmName,
            iParamValueID
            ).
        IF AVAILABLE dynParamValue THEN DO:
            lShowParam = dynParamValue.showParameters OR
                         DYNAMIC-FUNCTION("sfIsUserAdmin")
                         .
            RUN AOA/dynBrowserParam.w (
                cPrgmName,
                ROWID(dynParamValue),
                lShowParam
                ).
        END. /* if avail dynparamvalue */
    END. /* if avail dynsubject */

END PROCEDURE.
