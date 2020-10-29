/* pSuperProcs.i - rstark - 11.28.2019 */

DEFINE VARIABLE cSuperProcs AS CHARACTER NO-UNDO.

/* ********************************************************* */
/* shared tables and or variables reside here                */

{sys/ref/CustList.i NEW}
/* fg/FgBinBuild.p */
{fg/ttFGBins.i "NEW SHARED"}
/* Inventory/InventoryProcs.p */
{Inventory/ttInventory.i "NEW SHARED"}
/* oe/OrderProcs.p */
{sys/inc/var.i NEW SHARED}
/* oerep/LoadtagProcs.p */
{oerep/r-loadtg.i NEW}
DEFINE NEW SHARED TEMP-TABLE tt-word-print LIKE w-ord 
    FIELD tag-no AS CHARACTER.
/* util/ImportProcs.p */
{util/ttImport.i NEW SHARED}

/* ********************************************************* */

PROCEDURE pSuperProcs:
    DEFINE VARIABLE cInternalProcs AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProcFile      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSignature     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hSuperProc     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx            AS INTEGER   NO-UNDO.
    
    /* vv alphabetical list of persistent procedures vv */
    cSuperProcs = "AOA/spDynCalcField.p,"
                + "AOA/spDynDescriptionProc.p,"
                + "AOA/spDynInitializeProc.p,"
                + "AOA/spDynValidateProc.p,"
                + "AOA/spJasper.p,"
                + "api/AdvantzwareMonitorProcs.p,"
                + "api/inbound/InventoryReceiptProcs.p,"
                + "api/InboundProcs.p,"
                + "api/JSONProcs.p,"
                + "api/OutboundProcs.p,"
                + "custom/schedule.p,"
                + "est/ArtiosProcs.p,"
                + "est/CadImgProcs.p,"
                + "est/EstimateCalcProcs.p,"
                + "est/EstimateProcs.p,"
                + "fg/FgBinBuild.p,"
                + "fg/FGItemIDProcs.p,"
                + "fg/ReasonCode.p,"
                + "inventory/CycleCountCompare.p,"
                + "inventory/CycleCountCompareRM.p,"
                + "Inventory/invBin.p,"
                + "Inventory/InventoryProcs.p,"
                + "Inventory/invIM.p,"
                + "Inventory/invMTG.p,"
                + "Inventory/invMTM.p,"
                + "Inventory/invRG.p,"
                + "Inventory/invRM.p,"
                + "Inventory/invSG.p,"
                + "jc/JobProcs.p,"
                + "lstlogic/persist.p,"
                + "methods/excelrep.p,"
                + "nosweat/persist.p,"
                + "oe/OrderProcs.p,"
                + "oe/PriceProcs.p,"
                + "oerep/LoadtagProcs.p,"
                + "oerep/r-invprtARSuper.p,"
                + "oerep/r-invprtOESuper.p,"
                + "sys/NotesProcs.p,"
                + "system/CommonProcs.p,"
                + "system/ConversionProcs.p,"
                + "system/CostProcs.p,"
                + "system/CreditProcs.p,"
                + "system/FileSysProcs.p,"
                + "system/FormatProcs.p,"
                + "system/FreightProcs.p,"
                + "system/ftpProcs.p,"
                + "system/GLProcs.p,"     
                + "system/oeValidate.p,"
                + "system/OSProcs.p,"
                + "system/OutputProcs.p,"
                + "system/PgmMstrSecur.p,"
                + "system/ProgramMasterSecurity.p,"
                + "system/PurgeProcs.p,"
                + "system/Session.p,"
                + "system/TagProcs.p,"
                + "system/TaxProcs.p,"
                + "system/VendorCostProcs.p,"
                + "UDF/mfPersist.p,"
                + "util/dev/spRefTable.p,"
                + "util/dev/VendorCostConvProcs.p,"
                + "util/ImportProcs.p,"
                + "util/updQuoteProcs.p,"
                + "util/Validate.p,"
                .
    /* ^^ alphabetical list of persistent procedures ^^ */
    cSuperProcs = TRIM(cSuperProcs,",").
    DO idx = 1 TO NUM-ENTRIES(cSuperProcs):
        cProcFile = ENTRY(idx,cSuperProcs).
        IF SEARCH(cProcFile) EQ ? THEN
        cProcFile = REPLACE(cProcFile,".p",".r").
        IF SEARCH(cProcFile) EQ ? THEN DO:
            CREATE ttSuperProc.
            ASSIGN
                ttSuperProc.procName     = ENTRY(idx,cSuperProcs)
                ttSuperProc.internalProc = "Error Message"
                ttSuperProc.procType     = "ERROR"
                ttSuperProc.procParams   = "Procedure File Not Found"
                .
            NEXT.
        END. /* if search */
        RUN VALUE(ENTRY(idx,cSuperProcs)) PERSISTENT SET hSuperProc NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            DO jdx = 1 TO ERROR-STATUS:NUM-MESSAGES:
                CREATE ttSuperProc.
                ASSIGN
                    ttSuperProc.procName     = ENTRY(idx,cSuperProcs)
                    ttSuperProc.internalProc = "Erro Message"
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
                cSignature               = REPLACE(cSignature,"input-","INPUT-")
                cSignature               = REPLACE(cSignature,"output","OUTPUT")
                cSignature               = REPLACE(cSignature," table "," TABLE ")
                ttSuperProc.procParams   = cSignature
                .
        END. /* do jdx */
        DELETE PROCEDURE hSuperProc.
    END. /* do idx */
END PROCEDURE.
