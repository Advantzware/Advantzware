/* pSuperProcs.i - rstark - 11.28.2019 */

DEFINE VARIABLE cSuperProcs AS CHARACTER NO-UNDO.

/* util/ImportProcs.p */
{util/ttImport.i NEW SHARED}

PROCEDURE pSuperProcs:
    DEFINE VARIABLE cInternalProcs AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSignature     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hSuperProc     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx            AS INTEGER   NO-UNDO.
    
    cSuperProcs = "api/InboundProcs.p,"
                + "api/JSONProcs.p,"
                + "AOA/spDynCalcField.p,"
                + "AOA/spDynDescriptionProc.p,"
                + "AOA/spDynInitializeProc.p,"
                + "AOA/spDynValidateProc.p,"
                + "AOA/spJasper.p,"
                + "custom/schedule.p,"
                + "est/ArtiosProcs.p,"
                + "est/CadImgProcs.p,"
                + "est/EstimateCalcProcs.p,"
                + "fg/FgBinBuild.p,"
                + "fg/FGItemIDProcs.p,"
                + "fg/ReasonCode.p,"
                + "inventory/CycleCountCompare.p,"
                + "inventory/CycleCountCompareRM.p,"
                + "Inventory/InventoryProcs.p,"
                + "jc/JobProcs.p,"
                + "nosweat/persist.p,"
                + "oe/OrderProcs.p,"
                + "oe/PriceProcs.p,"
                + "oerep/LoadtagProcs.p,"
                + "sys/NotesProcs.p,"            
                + "system/CommonProcs.p,"
                + "system/CostProcs.p,"
                + "system/CreditProcs.p,"
                + "system/FreightProcs.p,"
                + "system/ftpProcs.p,"
                + "system/MessageProcs.p,"
                + "system/oeValidate.p,"
                + "system/OutputProcs.p,"
                + "system/PgmMstrSecur.p,"
                + "system/ProgramMasterSecurity.p,"
                + "system/PurgeProcs.p,"
                + "system/Session.p,"
                + "system/TagProcs.p,"
                + "system/TaxProcs.p,"
                + "system/VendorCostProcs.p,"
                + "util/ImportProcs.p,"
                + "util/Validate.p"
                .
    DO idx = 1 TO NUM-ENTRIES(cSuperProcs):
        RUN VALUE(ENTRY(idx,cSuperProcs)) PERSISTENT SET hSuperProc NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            DO jdx = 1 TO ERROR-STATUS:NUM-MESSAGES:
                CREATE ttSuperProc.
                ASSIGN
                    ttSuperProc.procName     = ENTRY(idx,cSuperProcs)
                    ttSuperProc.internalProc = "Message: " + STRING(jdx)
                    ttSuperProc.procType     = "ERROR"
                    ttSuperProc.procParams   = ERROR-STATUS:GET-MESSAGE(jdx)
                    .
            END. /* do jdx */
            NEXT.
        END.
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
END PROCEDURE.
