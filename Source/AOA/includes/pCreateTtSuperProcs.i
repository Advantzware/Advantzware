/* pCreateTtSuperProcs.i - rstark - 11.28.2019 */

DEFINE VARIABLE cInternalProcs AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSignature     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSuperProcs    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hSuperProc     AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
DEFINE VARIABLE jdx            AS INTEGER   NO-UNDO.
DEFINE VARIABLE kdx            AS INTEGER   NO-UNDO.

cSuperProcs = "AOA/spDynCalcField.p,"
            + "AOA/spDynDescriptionProc.p,"
            + "AOA/spDynInitializeProc.p,"
            + "AOA/spDynValidateProc.p,"
            + "AOA/spJasper.p,"
            + "nosweat/persist.p,"
            + "system/CommonProcs.p,"
            + "system/CreditProcs.p,"
            + "system/PurgeProcs.p,"
            + "system/Session.p,"
            + "system/TagProcs.p"
            .
DO idx = 1 TO NUM-ENTRIES(cSuperProcs):
    RUN VALUE(ENTRY(idx,cSuperProcs)) PERSISTENT SET hSuperProc.
    cInternalProcs = hSuperProc:INTERNAL-ENTRIES.
    DO jdx = 1 TO NUM-ENTRIES(cInternalProcs):
        CREATE ttSuperProc.
        ASSIGN
            ttSuperProc.procName     = hSuperProc:NAME
            ttSuperProc.internalProc = ENTRY(jdx,cInternalProcs)
            cSignature               = LC(hSuperProc:GET-SIGNATURE(ttSuperProc.internalProc))
            ttSuperProc.procType     = CAPS(ENTRY(1,cSignature))
            ttSuperProc.returnType   = CAPS(ENTRY(2,cSignature))
            ENTRY(1,cSignature)      = ""
            ENTRY(2,cSignature)      = ""
            cSignature               = LEFT-TRIM(cSignature,",,")
            cSignature               = REPLACE(cSignature,"input ","")
            cSignature               = REPLACE(cSignature,"output","OUTPUT")
            ttSuperProc.procParams   = cSignature
            .
    END. /* do jdx */
    DELETE PROCEDURE hSuperProc.
END. /* do idx */
