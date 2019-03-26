
/*------------------------------------------------------------------------
    File        : EstimateProcs.p
    Purpose     : Start moving some repetitive code into common procedures

    Syntax      :

    Description : Houses common procedures for calculating estimates and jobs

    Author(s)   : BV
    Created     : Thu Jun 14 18:19:14 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est\EstimateProcs.i}
{est\CostTempTables.i}
DEFINE STREAM sOutput.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetPromptForVendor RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION fGetCERun RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER,
    ipcIndustry AS CHARACTER) FORWARD.

FUNCTION fGetRecKey RETURNS CHARACTER PRIVATE
    (  ) FORWARD.

FUNCTION fGetUserID RETURNS CHARACTER PRIVATE
    (  ) FORWARD.

/* ***************************  Main Block  *************************** */
FIND FIRST est NO-LOCK 
    WHERE est.company EQ '001'
    AND est.est-no EQ '   13610'
    NO-ERROR.
RUN pBuildCalculationTables(ROWID(est),NO).
RUN pExportXML(TEMP-TABLE ttEstMaster:HANDLE, "C:\temp\ttEstMaster.xml").
RUN pExportXML(TEMP-TABLE ttEstQty:HANDLE, "C:\temp\ttEstQty.xml").
RUN pExportXML(TEMP-TABLE ttCalculationErrors:HANDLE, "C:\temp\ttCalcErrors.xml").
RUN pExportXML(TEMP-TABLE ttEstOperation:HANDLE, "C:\temp\ttEstOperations.xml").
MESSAGE "Done"
    VIEW-AS ALERT-BOX.

/* **********************  Internal Procedures  *********************** */


PROCEDURE GetEstimateDir:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBaseDir AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSubDir AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CEBrowse",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT opcBaseDir,
        OUTPUT lFound).
    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CEBrowse",
        INPUT "D",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT opcSubDir,
        OUTPUT lFound).

    IF opcBaseDir EQ "" THEN
        opcBaseDir = "users\".

    ASSIGN 
        opcBaseDir = REPLACE(opcBaseDir,"/","\")  /*replace slashes in wrong direction*/
        opcBaseDir = TRIM(opcBaseDir,"\") + "\"  /*ensure there is a slash on the end*/
        .

    IF DEC(opcSubDir) GT 0 THEN 
    DO:
        opcSubDir = opcBaseDir + opcSubDir + "\".
        FILE-INFO:FILE-NAME = opcSubDir.
        IF FILE-INFO:FULL-PATHNAME = ? THEN
            OS-CREATE-DIR VALUE(opcSubDir).        
    END.
    ELSE 
        opcSubDir = opcBaseDir.


END PROCEDURE.

PROCEDURE GetMaterialCost:
    /*------------------------------------------------------------------------------
     Purpose:  Given an item
     Notes:  replaces est/matcost.i
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSetup AS DECIMAL NO-UNDO.
   
    DEFINE VARIABLE iIndex     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lCostFound AS LOGICAL NO-UNDO.

    ASSIGN
        lCostFound = NO
        opdCost    = 0
        opdSetup   = 0.

    FIND FIRST ITEM NO-LOCK
        WHERE ROWID(item) EQ ipriItem
        NO-ERROR.
    IF NOT AVAILABLE ITEM THEN LEAVE.
    FIND FIRST e-item OF item NO-LOCK NO-ERROR.
    
    IF AVAILABLE e-item THEN 
    DO:
        RELEASE e-item-vend.
        IF ipcVendNo NE "" THEN 
            FIND FIRST e-item-vend OF e-item NO-LOCK 
                WHERE e-item-vend.item-type EQ YES
                AND e-item-vend.vend-no EQ ipcVendNo
                NO-ERROR.
        IF NOT AVAILABLE e-item-vend THEN 
            FOR EACH e-item-vend OF e-item NO-LOCK
                WHERE e-item-vend.item-type EQ YES
                BY e-item-vend.vend-no:
                LEAVE.
            END.
 
        CREATE ttCostTable.
 
        IF AVAILABLE e-item-vend THEN
        DO:
            ASSIGN 
                ttCostTable.cRunCostUom = e-item-vend.std-uom.
            DO iIndex = 1 TO 10:
                ASSIGN
                    ttCostTable.dRunQty[iIndex]  = e-item-vend.run-qty[iIndex]
                    ttCostTable.dRunCost[iIndex] = e-item-vend.run-cost[iIndex]
                    ttCostTable.dSetups[iIndex]  = e-item-vend.setups[iIndex]
                    .
            END.

            DO iIndex = 1 TO 10:
                ASSIGN
                    ttCostTable.dRunQty[iIndex + 10]  = e-item-vend.runQtyXtra[iIndex]
                    ttCostTable.dRunCost[iIndex + 10] = e-item-vend.runCostXtra[iIndex]
                    ttCostTable.dSetups[iIndex + 10]  = e-item-vend.setupsXtra[iIndex].
            END.
        END.
        DO iIndex = 1 TO 20:
            IF ttCostTable.dRunQty[iIndex] NE 0   AND
                ttCostTable.dRunQty[iIndex] GE ipdQty THEN 
            DO:
                ASSIGN
                    lCostFound = YES
                    opdCost    = ttCostTable.dRunCost[iIndex]
                    opdSetup   = ttCostTable.dSetups[iIndex]
                    opcCostUom = ttCostTable.cRunCostUom
                    .
                LEAVE.
            END.
        END.
  
        DELETE ttCostTable.
    END.

    IF item.i-code EQ "R" AND NOT lCostFound THEN
        opdCost= IF ce-ctrl.r-cost THEN item.avg-cost ELSE item.last-cost.


END PROCEDURE.

PROCEDURE pAddError PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Adds an error to the error temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcError AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiForm AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlank AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplCritical AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    CREATE ttCalculationErrors.
    ASSIGN 
        ttCalculationErrors.cError    = ipcError
        ttCalculationErrors.iFormNo   = ipiForm
        ttCalculationErrors.iBlankNo  = ipiBlank
        ttCalculationErrors.iPassNo   = ipiPass
        ttCalculationErrors.lCritical = iplCritical
        .
    IF iplCritical THEN 
        cMessage = "Critical Error: ".
    cMessage = cMessage + ipcError.
    IF ipiForm GE 0 THEN 
        cMessage = cMessage + " Form: " + STRING(ipiForm).
    IF ipiBlank GT 0 THEN 
        cMessage = cMessage + " Blank: " + STRING(ipiBlank).
    IF ipiPass GT 0 THEN 
        cMessage = cMessage + " Pass: " + STRING(ipiPass).
    
    ttCalculationErrors.cMessage = cMessage.

END PROCEDURE.

PROCEDURE pAddEstimateMaster PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an estimate buffer, creates the Estimate Master with all primary settings and 
     control variables
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est FOR est.
    DEFINE INPUT PARAMETER iplUI AS LOGICAL NO-UNDO.

    CREATE ttEstMaster.
    ASSIGN 
        ttEstMaster.rec_key        = fGetRecKey()
        ttEstMaster.riSource       = ROWID(ipbf-est)
        ttEstMaster.cCompany       = ipbf-est.company
        ttEstMaster.cLoc           = ipbf-est.loc
        ttEstMaster.cEstNo         = ipbf-est.est-no
        ttEstMaster.cIndustry      = IF ipbf-est.est-type LT 5 THEN "F" ELSE "C"
        ttEstMaster.cUserID        = fGetUserID()
        ttEstMaster.iEstType       = ipbf-est.est-type
        ttEstMaster.dtCalcDateTime = NOW 
        .
    CASE ipbf-est.est-type:
        WHEN 1 OR 
        WHEN 5 THEN 
            ttEstMaster.cEstTypeDesc = "Single".
        WHEN 2 OR 
        WHEN 6 THEN 
            ttEstMaster.cEstTypeDesc = "Set".
        OTHERWISE 
        ttEstMaster.cEstTypeDesc = "Combo/Tandem".
    END CASE.
    FIND FIRST ce-ctrl NO-LOCK 
        WHERE ce-ctrl.company EQ ipbf-est.company
        AND ce-ctrl.loc EQ ipbf-est.loc
        NO-ERROR.
    IF NOT AVAILABLE ce-ctrl THEN 
    DO: 
        RUN pAddError("Estimate Control File Not Found", -1, 0, 0, YES).
    END. 
    ELSE 
    DO:
        ASSIGN /*Initialize the quantity specific CostMaster with values from ce-ctrl*/
            ttEstMaster.cMarginOn                    = ce-ctrl.sell-by
            ttEstMaster.dMarginPct                   = ce-ctrl.prof-mrkup
            ttEstMaster.dWarehouseMarkupPct          = ce-ctrl.whse-mrkup / 100 /*ctrl[1]*/
            ttEstMaster.dHandlingChargePct           = ce-ctrl.hand-pct / 100 /*ctrl[2]*/
            ttEstMaster.dHandlingRatePerCWTRMPct     = ce-ctrl.rm-rate / 100 /*ctrl[3]*/ /*NOTE CHANGED to be /100 */
            ttEstMaster.dSpecial1MarkupPct           = ce-ctrl.spec-%[1] / 100 /*ctrl[4]*/ /*NOTE CHANGED to be /100 */
            ttEstMaster.dSpecial2MarkupPct           = ce-ctrl.spec-%[2] / 100 /*ctrl[11]*/ /*NOTE CHANGED to be /100 */
            ttEstMaster.dSpecial3MarkupPct           = ce-ctrl.spec-%[3] / 100 /*ctrl[12]*/ /*NOTE CHANGED to be /100 */
            ttEstMaster.lShowCommissoins             = ce-ctrl.comm-add /*ctrl[5]*/
            ttEstMaster.lAddToFactCostFreight        = ce-ctrl.shp-add /*ctrl[6]*/
            ttEstMaster.lShowLaborRates              = ce-ctrl.sho-labor /*ctrl[7]*/
            ttEstMaster.lAddToFactCostSpecial1       = ce-ctrl.spec-add[1] /*ctrl[13]*/
            ttEstMaster.lAddToFactCostSpecial2       = ce-ctrl.spec-add[2] /*ctrl[14]*/
            ttEstMaster.lAddToFactCostSpecial3       = ce-ctrl.spec-add[3] /*ctrl[15]*/
            ttEstMaster.lAddToFactCostGSA            = ce-ctrl.spec-add[6] /*ctrl[16]*/
            ttEstMaster.lAddToFactCostRoyalty        = ce-ctrl.spec-add[8] /*ctrl[18]*/
            ttEstMaster.dFoldPct                     = ce-ctrl.fold-pct / 100 /*ctrl[19]*/ /*NOTE CHANGED to be /100 */    
            ttEstMaster.lAddToFactCostComm           = ce-ctrl.spec-add[7] /*ctrl[17]*/
            ttEstMaster.dHandlingRatePerCWTFGPct     = ce-ctrl.fg-rate / 100
            ttEstMaster.dHandlingRatePerCWTRMFarmPct = ce-ctrl.rm-rate-farm / 100
            ttEstMaster.dHandlingRatePerCWTFGFarmPct = ce-ctrl.fg-rate-farm / 100
            ttEstMaster.dHandlingChargeFarmPct       = ce-ctrl.hand-pct-farm / 100
            .
    END.
    RUN pGetAllCalculationConfigs(BUFFER ttEstMaster).

END PROCEDURE.

PROCEDURE pAddEstimateOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Adds an EstimateOperation for calculation given an qty master, est-op and mach buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstQty FOR ttEstQty.
    DEFINE PARAMETER BUFFER ipbf-est-op   FOR est-op.
    DEFINE PARAMETER BUFFER ipbf-mach     FOR mach.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    CREATE ttEstOperation.
    ASSIGN 
        ttEstOperation.rec_key                     = fGetRecKey()
        ttEstOperation.rec_keyEstQty               = ipbf-ttEstQty.rec_key
        ttEstOperation.rec_keyEstimate             = ipbf-ttEstQty.rec_keyEstimate
        ttEstOperation.dOperationQuantity          = ipbf-ttEstQty.dOperationQuantity
        ttEstOperation.cOperationID                = ipbf-est-op.m-code
        ttEstOperation.cOperationDesc              = ipbf-est-op.m-dscr
        ttEstOperation.cDepartmentPrimary          = ipbf-est-op.dept
        ttEstOperation.iForm                       = ipbf-est-op.s-num
        ttEstOperation.iBlank                      = ipbf-est-op.b-num
        ttEstOperation.dHoursSetup                 = ipbf-est-op.op-mr
        ttEstOperation.dFeedsWastedInSetup         = ipbf-est-op.op-waste 
        ttEstOperation.dFeedsPerHour               = ipbf-est-op.op-speed
        ttEstOperation.dFeedsWastedRate            = ipbf-est-op.op-spoil
        ttEstOperation.dCrewCountSetup             = ipbf-est-op.op-crew[1]
        ttEstOperation.dCrewCountRun               = ipbf-est-op.op-crew[2]
        ttEstOperation.dCostTotalSetup             = ipbf-est-op.op-rate[1]
        ttEstOperation.dCostTotalRun               = ipbf-est-op.op-rate[2]
        ttEstOperation.iSequenceEstimate           = ipbf-est-op.line
        ttEstOperation.iSequenceDepartment         = ipbf-est-op.d-seq
        ttEstOperation.iRunQtyOut                  = MINIMUM(ipbf-est-op.n-out,1)
        ttEstOperation.iRunQtyDivisor              = ipbf-est-op.n_out_div
        ttEstOperation.lIsLocked                   = ipbf-est-op.isLocked
        ttEstOperation.iCountSheets                = ipbf-est-op.num-sh
        ttEstOperation.iCountColors                = ipbf-est-op.num-col
        ttEstOperation.iCountCoatings              = ipbf-est-op.num-coat
        ttEstOperation.iCountPlateChanges          = ipbf-est-op.plates
        ttEstOperation.iCountFountainChanges       = ipbf-est-op.fountains
        
        ttEstOperation.cTypeFeed                   = ipbf-mach.p-type
        ttEstOperation.iSequenceWithinDepartment   = ipbf-mach.m-seq
        ttEstOperation.dCostPerHourDLPerCrewSetup  = ipbf-mach.lab-rate[1]
        ttEstOperation.dCostPerHourDLPerCrewRun    = ipbf-mach.lab-rate[1]
        ttEstOperation.dCostPerHourFOHSetup        = ipbf-mach.mr-fixoh 
        ttEstOperation.dCostPerHourFOHRun          = ipbf-mach.run-fixoh
        ttEstOperation.dCostPerHourVOHSetup        = ipbf-mach.mr-varoh
        ttEstOperation.dCostPerHourVOHRun          = ipbf-mach.run-varoh
        ttEstOperation.dCostMinimum                = ipbf-mach.mrk-rate
        ttEstOperation.dHoursSetupWashup           = ipbf-mach.washup
        ttEstOperation.cHoursSetupWashupPer        = ipbf-mach.col-pass
        ttEstOperation.dFeedsWastedInSetupPerColor = ipbf-mach.col-wastesh
        ttEstOperation.dFeedsWastedInSetupBase     = ipbf-mach.mr-waste
        ttEstOperation.dInkWastedPerMR             = ipbf-mach.ink-waste
        ttEstOperation.dInkWastedPerColor          = ipbf-mach.col-wastelb
        ttEstOperation.cInkWastedUOM               = "LB"
        .
    DO iIndex = 1 TO EXTENT(ipbf-mach.dept):
        ttEstOperation.cDepartmentList = ttEstOperation.cDepartmentList + ipbf-mach.dept[iIndex] + ",".
    END.
    ttEstOperation.cDepartmentList = TRIM(ttEstOperation.cDepartmentList,",").

END PROCEDURE.

PROCEDURE pAddEstimateQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given Estimate Master, Add Estimate Quantity Record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstMaster FOR ttEstMaster.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiReleases AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplRunShip AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-est-qty FOR est-qty.
    
    CREATE ttEstQty.
    BUFFER-COPY ipbf-ttEstMaster EXCEPT rec_key TO ttEstQty.
    ASSIGN 
        ttEstQty.rec_key         = fGetRecKey()
        ttEstQty.rec_keyEstimate = ipbf-ttEstMaster.rec_key
        ttEstQty.dMasterQuantity = ipdQuantity
        ttEstQty.iReleases       = ipiReleases
        ttEstQty.lRunAndShip     = iplRunShip
        .
    /*Get the target operation quantity*/
    FOR EACH bf-est-qty NO-LOCK 
        WHERE bf-est-qty.company EQ ipbf-ttEstMaster.cCompany
        AND bf-est-qty.est-no EQ ipbf-ttEstMaster.cEstNo
        BY bf-est-qty.eqty DESCENDING:
        IF bf-est-qty.eqty LE ipdQuantity THEN 
        DO:
            ttEstQty.dOperationQuantity = bf-est-qty.eqty.
            LEAVE.
        END.
    END.

END PROCEDURE.

PROCEDURE pAddCostHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Encapsulates the Creation of the Header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est FOR est.
    DEFINE PARAMETER BUFFER ipbf-ef  FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb  FOR eb.
    DEFINE INPUT PARAMETER cDescription AS CHARACTER.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL.
    DEFINE INPUT PARAMETER iplIsItem AS LOGICAL.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER.
    DEFINE INPUT PARAMETER ipiForm AS INTEGER. 
    DEFINE INPUT PARAMETER ipiBlank AS INTEGER.
    DEFINE INPUT PARAMETER ipdtTimeStamp AS DATETIME.
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER.
    DEFINE INPUT PARAMETER ipcFormRK AS CHARACTER.
    DEFINE INPUT PARAMETER ipcSetRK AS CHARACTER.
    DEFINE INPUT-OUTPUT PARAMETER iopcRK AS CHARACTER. 
    
    DEFINE BUFFER bf-eb FOR eb.
    DEFINE VARIABLE dFactor   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dFormSqIn AS DECIMAL NO-UNDO.

    CREATE ttCostHeader.
    iopcRK = fGetRecKey().
    ASSIGN 
        ttCostHeader.company           = ipbf-est.company
        ttCostHeader.headerDescription = cDescription
        ttCostHeader.isItem            = iplIsItem
        ttCostHeader.rec_key           = iopcRK
        ttCostHeader.rec_keyParentForm = IF ipcFormRK NE "" THEN ipcFormRK ELSE ttCostHeader.rec_key
        ttCostHeader.rec_keyParentSet  = IF ipcSetRK NE "" THEN ipcSetRK ELSE ttCostHeader.rec_key
        ttCostHeader.calculationTime   = ipdtTimeStamp 
        ttCostHeader.calculatedBy      = ipcUserID 
        ttCostHeader.quantityMaster    = ipdQty
        ttCostHeader.jobNo             = ipcJobNo
        ttCostHeader.jobNo2            = ipiJobNo2
        ttCostHeader.formNo            = ipiForm
        ttCostHeader.blankNo           = ipiBlank
        ttCostHeader.estimateNo        = ipbf-est.est-no
        ttCostHeader.industry          = IF ipbf-est.est-type < 5 THEN 1 ELSE 2
        ttCostHeader.estimateType      = ipbf-est.est-type
        ttCostHeader.factorSet         = 1
        .
    
    IF AVAILABLE ipbf-ef THEN 
    DO:
        ASSIGN              
            ttCostHeader.lengthGross = ipbf-ef.gsh-len 
            ttCostHeader.widthGross  = ipbf-ef.gsh-wid 
            ttCostHeader.depthGross  = ipbf-ef.gsh-dep 
            ttCostHeader.lengthNet   = ipbf-ef.nsh-len             
            ttCostHeader.widthNet    = ipbf-ef.nsh-wid 
            ttCostHeader.depthNet    = ipbf-ef.nsh-dep
            .
        IF CAN-FIND(FIRST bf-eb OF ipbf-ef WHERE bf-eb.blank-no GT 1) THEN 
        DO:
            dFormSqIn = 0.
            FOR EACH bf-eb OF ipbf-ef NO-LOCK:
                dFormSqIn = dFormSqIn + bf-eb.t-sqin * bf-eb.num-up.
            END.       
        END.
            
    END.
    IF AVAILABLE ipbf-eb THEN 
    DO:
        ASSIGN      
            ttCostHeader.quantityPerSet  = IF ipbf-eb.quantityPerSet NE 0 THEN ipbf-eb.quantityPerSet ELSE ipbf-eb.yld-qty
            ttCostHeader.quantityYield   = ipbf-eb.yld-qty
            ttCostHeader.quantityRequest = ipbf-eb.bl-qty 
            ttCostHeader.lengthBlank     = ipbf-eb.t-len
            ttCostHeader.widthBlank      = ipbf-eb.t-wid
            ttCostHeader.blankSquareFeet = ipbf-eb.t-sqin / 144
            ttCostHeader.customerPartID  = ipbf-eb.part-no
            ttCostHeader.fgItemID        = ipbf-eb.stock-no
            
            .
        IF dFormSqIn GT 0 THEN 
            ttCostHeader.factorForm = (ipbf-eb.t-sqin * ipbf-eb.num-up ) / dFormSqin.
        IF ttCostHeader.factorForm EQ 0 THEN ttCostHeader.factorForm = 1. 
    END.
     

END PROCEDURE.

PROCEDURE pBuildCalculationTables PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given Row ID of estimate, 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEstJob AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplUI AS LOGICAL NO-UNDO.

    FIND FIRST job NO-LOCK 
        WHERE ROWID(job) EQ ipriEstJob
        NO-ERROR.
    IF NOT AVAILABLE job THEN 
        FIND FIRST est NO-LOCK 
            WHERE ROWID(est) EQ ipriEstJob
            NO-ERROR.
    ELSE 
        FIND FIRST est NO-LOCK
            WHERE est.company EQ job.company
            AND est.est-no EQ job.est-no
            NO-ERROR. 
    
    IF NOT AVAILABLE est THEN 
        RUN pAddError("Invalid Estimate or Job", -1, 0, 0, YES).
    ELSE 
    DO:
        RUN pResetCalculations(BUFFER est).
        RUN pAddEstimateMaster(BUFFER est, iplUi).
    END.
    FIND FIRST ttEstMaster NO-ERROR.
    IF AVAILABLE ttEstMaster THEN 
    DO:
        RUN pBuildQuantities(BUFFER ttEstMaster).
    END.
    FOR EACH ttEstQty:
        RUN pBuildCostHeaders(BUFFER est, BUFFER job, ttEstQty.dMasterQuantity).
        RUN pBuildOperations(BUFFER ttEstQty).
    END.
END PROCEDURE.

PROCEDURE pBuildCostHeaders PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the ttCostHeader TempTable based on the structure of the estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est FOR est.
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL.

    DEFINE VARIABLE dtCalcTime   AS DATETIME.
    DEFINE VARIABLE cUserID      AS CHARACTER.
    DEFINE VARIABLE cRK          AS CHARACTER.
    DEFINE VARIABLE cFormRK      AS CHARACTER.
    DEFINE VARIABLE cSetRK       AS CHARACTER.
    DEFINE VARIABLE iFormCreated AS INTEGER.
    DEFINE VARIABLE cJobNo       AS CHARACTER.
    DEFINE VARIABLE iJobNo2      AS INTEGER.
    DEFINE VARIABLE lIsItem      AS LOGICAL.

    IF AVAILABLE job THEN 
        ASSIGN 
            cJobNo  = job.job-no
            iJobNo2 = job.job-no2
            .
    dtCalcTime = NOW.
    cUserID = fGetUserID().
    ASSIGN 
        cRK     = ""
        cFormRK = ""
        cSetRK  = ""
        .
    IF ipbf-est.est-type EQ 8 THEN 
    DO:
        /*create a separate header record for the combo/tandem estimate as a whole*/
        RUN pAddCostHeader(BUFFER ipbf-est, BUFFER ef, BUFFER eb, 
            "Combo/Tandem Master Header",
            ipdQty, 
            NO,  /*This is not an item, thus no job-hdr link*/
            cJobNo,
            iJobNo2,
            0, 
            0,  
            dtCalcTime, 
            cUserID, 
            cFormRK, 
            cSetRK,
            INPUT-OUTPUT cRK).
        ASSIGN  
            cSetRK = cRK  /*Set the "Set" RK for a summary total header*/
            .
    END.
    FOR EACH eb OF ipbf-est NO-LOCK
        BREAK BY eb.form-no  
        BY eb.blank-no DESCENDING:
        lIsItem = YES.
        FIND FIRST ef OF eb NO-LOCK NO-ERROR. /*Not available for Set Header Blank*/
        IF FIRST-OF(eb.form-no) AND eb.blank-no GT 1 AND  iFormCreated NE eb.form-no THEN /*Last Blank of Form*/
        DO:
            /*create a separate header record for the form as a whole - "Form Master"*/
            RUN pAddCostHeader(BUFFER ipbf-est, BUFFER ef, BUFFER eb, 
                "Combo Form #" + STRING(eb.form-no) + " Master",
                ipdQty, 
                NO,  /*This is not an item, thus no job-hdr link*/
                cJobNo,
                iJobNo2,
                eb.form-no, 
                0,  
                dtCalcTime, 
                cUserID, 
                cFormRK, 
                cSetRK,
                INPUT-OUTPUT cRK).
            ASSIGN  
                cFormRK      = cRK  /*Set the form RK for the next blanks of the form to the newly created form header rec_key*/
                iFormCreated = eb.form-no /*register it so as not to create another form header*/
                . 
        END.    
        IF cFormRK NE "" THEN 
        DO:
            /*create a separate header record for the blank item as a whole*/
            RUN pAddCostHeader(BUFFER ipbf-est, BUFFER ef, BUFFER eb, 
                "Form #" + STRING(eb.form-no) + " Blank #" + STRING(eb.blank-no) + " Item Header",
                ipdQty, 
                YES,  /*This will be an item header record to match job-hdr for item*/
                cJobNo,
                iJobNo2,
                eb.form-no, 
                eb.blank-no,
                dtCalcTime, 
                cUserID, 
                cFormRK, 
                cSetRK,
                INPUT-OUTPUT cRK).
            ASSIGN  
                lIsItem = NO.
        END.
        /*Create Rec_key for blank*/
        IF FIRST-OF(eb.blank-no) THEN 
            RUN pAddCostHeader(BUFFER ipbf-est, BUFFER ef, BUFFER eb, 
                "Form #" + STRING(eb.form-no) + " Blank #" + STRING(eb.blank-no) + " Blank",
                ipdQty, 
                lIsItem,  /*Item by default, unless it is an factored combo item*/
                cJobNo,
                iJobNo2,
                eb.form-no, 
                eb.blank-no, 
                dtCalcTime, 
                cUserID, 
                cFormRK, 
                cSetRK,
                INPUT-OUTPUT cRK). 
        IF NOT AVAILABLE ef AND eb.form-no EQ 0 AND eb.blank-no EQ 0 THEN  /*Set Header Blank*/ 
            cSetRK = cRK.
        IF LAST-OF(eb.form-no) THEN
            ASSIGN 
                cFormRK      = ""
                iFormCreated = 0
                .
    END.     

END PROCEDURE.

PROCEDURE pBuildOperations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an est buffer and quantity master, builds the Operations Temp Table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstQty FOR ttEstQty.

    DEFINE           BUFFER bf-est-op     FOR est-op.
    DEFINE           BUFFER bf-est-qty    FOR est-qty.


    FOR EACH bf-est-op EXCLUSIVE-LOCK 
        WHERE bf-est-op.company EQ ipbf-ttEstQty.cCompany
        AND bf-est-op.est-no EQ ipbf-ttEstQty.cEstNo
        AND bf-est-op.qty EQ ipbf-ttEstQty.dOperationQuantity:
        FIND FIRST mach NO-LOCK 
            WHERE mach.company EQ bf-est-op.company
            AND mach.m-code EQ bf-est-op.m-code
            NO-ERROR.
        IF AVAILABLE mach THEN 
        DO:
            IF mach.obsolete THEN  
                RUN pAddError("Machine Code " + bf-est-op.m-code + " is inactive", bf-est-op.s-num, bf-est-op.b-num, bf-est-op.op-pass, NO).
            RUN pAddEstimateOperation(BUFFER ipbf-ttEstQty, BUFFER bf-est-op, BUFFER mach). 
        END.
        ELSE 
        DO: 
            RUN pAddError("Machine Code " + bf-est-op.m-code + " is not valid", bf-est-op.s-num, bf-est-op.b-num, bf-est-op.op-pass, YES).
            LEAVE.
        END.

    END.        
    
 

END PROCEDURE.

PROCEDURE pBuildQuantities PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given Estimate Master, builds the QUantities Temp Table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstMaster FOR ttEstMaster.

    DEFINE           BUFFER bf-est           FOR est.
    DEFINE           BUFFER bf-est-qty       FOR est-qty.

    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

    FIND FIRST bf-est NO-LOCK 
        WHERE ROWID(bf-est) EQ ipbf-ttEstMaster.riSource
        NO-ERROR.
    IF AVAILABLE bf-est THEN 
    DO:
        FIND FIRST bf-est-qty NO-LOCK 
            WHERE bf-est-qty.company EQ bf-est.company
            AND bf-est-qty.est-no EQ bf-est.est-no
            NO-ERROR.
        IF NOT AVAILABLE bf-est-qty THEN 
            RUN pAddError("Estimate quantity does not exist for estimate " + bf-est.est-no, -1, 0, 0, YES).
        ELSE 
        DO iCount = 1 TO 20:
            IF bf-est-qty.qty[iCount] GT 0 THEN 
                RUN pAddEstimateQuantity(BUFFER ipbf-ttEstMaster, bf-est-qty.qty[iCount], INTEGER(bf-est-qty.qty[iCount + 20]), bf-est-qty.whsed[iCount]).
        END.
    END.
END PROCEDURE.

PROCEDURE pGetAllCalculationConfigs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Assigns all master-level NK1 settings to master fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstMaster FOR ttEstMaster.

    ASSIGN 
        ipbf-ttEstMaster.cDisplayFormat = fGetCERun(ipbf-ttEstMaster.cCompany, ipbf-ttEstMaster.cIndustry)
        .

END PROCEDURE.

PROCEDURE pGetCEVendor PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the options stored in CEVendor Config
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER oplPrompt AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplUserInGroup AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserGroups AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCategory AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendor AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cParseString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER   NO-UNDO.

    oplPrompt = fGetPromptForVendor(ipcCompany).
    IF oplPrompt THEN 
    DO:
    
        RUN sys/ref/nk1Look.p(INPUT ipcCompany,
            INPUT "CEVendor",
            INPUT "C",
            INPUT NO,
            INPUT NO,
            INPUT "",
            INPUT "",
            OUTPUT cParseString,
            OUTPUT lFound).
        IF lFound AND cParseString NE "" THEN 
        DO:
            ASSIGN 
                opcUserGroups = SUBSTRING(cParseString,1,INDEX(TRIM(cParseString)," ") - 1)
                cParseString  = SUBSTRING(cParseString,INDEX(TRIM(cParseString),"(") + 1)
                cParseString  = TRIM(cParseString,")")
                opcCategory   = SUBSTRING(cParseString,1, INDEX(cParseString,"=") - 1)
                opcVendor     = SUBSTRING(cParseString,INDEX(cParseString,"=") + 1)
                .
        END.
        IF opcCategory NE "" THEN 
            FIND FIRST procat NO-LOCK 
                WHERE procat.company EQ ipcCompany
                AND procat.procat EQ opcCategory
                NO-ERROR.
        IF NOT AVAILABLE procat THEN opcCategory = "".
        IF opcVendor NE "" THEN 
            FIND FIRST vend NO-LOCK 
                WHERE vend.company EQ ipcCompany
                AND vend.vend-no EQ opcVendor
                NO-ERROR.
        IF NOT AVAILABLE vend THEN opcVendor = "".
        REPEAT iCount = 1 TO NUM-ENTRIES(opcUserGroups):
            IF TRIM(ENTRY(iCount,opcUserGroups)) NE "" THEN
            DO:
                FIND FIRST usergrps WHERE
                    usergrps.usergrps = ENTRY(iCount,opcUserGroups)
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usergrps AND
                    (CAN-DO(usergrps.users,USERID("ASI")) OR
                    TRIM(usergrps.users) EQ "*") THEN
                DO:
                    oplUserInGroup = YES.
                    LEAVE.
                END.
            END.
        END.
    END.    
END PROCEDURE.

PROCEDURE pGetVendor PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: replaces cec/getVend.i
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplUI AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendor AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lPromptForVendor AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lUserInGroup     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cExcludeCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcludeVendor   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserGroups      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lError           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValid           AS LOGICAL   NO-UNDO INIT YES.

    RUN pGetCEVendor(ipcCompany, OUTPUT lPromptForVendor, OUTPUT lUserInGroup, OUTPUT cUserGroups, OUTPUT cExcludeCategory, OUTPUT cExcludeVendor).

    IF lPromptForVendor AND iplUI THEN
    DO:
        lValid = lUserInGroup OR cUserGroups EQ "".
        IF lValid EQ NO AND cExcludeCategory NE "" THEN
            /*            RUN cec/get-exclude-vend.p(INPUT ROWID(xest),*/
            /*                INPUT cExcludeCategory,                  */
            /*                INPUT cExcludeVendor,                    */
            /*                OUTPUT opcVendor).                       */

            IF lValid THEN
    /*            RUN cec/est-vend.w (RECID(xest), OUTPUT opcVendor, OUTPUT lError) NO-ERROR.*/
    END.
    IF lError THEN 
    DO:
        RETURN ERROR.
    END.


END PROCEDURE.

PROCEDURE pResetCalculations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Clears temp-tables for calculations
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est FOR est.
    
    /*Clean up old est-op that were used for calculation only - Now replaced by ttEstOperation*/    
    FOR EACH est-op EXCLUSIVE-LOCK 
        WHERE est-op.company EQ ipbf-est.company 
        AND est-op.est-no  EQ ipbf-est.est-no 
        AND est-op.line    GE 500:
        DELETE est-op.
    END.

    EMPTY TEMP-TABLE ttEstMaster.
    EMPTY TEMP-TABLE ttEstQty.
    EMPTY TEMP-TABLE ttEstOperation.
    
END PROCEDURE.

PROCEDURE pValidateEstimateInputs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an estimate, validates all critical values and builds 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER bf-est FOR est.

    FOR EACH ef NO-LOCK 
        WHERE ef.company EQ bf-est.company
        AND ef.est-no EQ bf-est.est-no:
    /*Add form validations here*/
    END.
    FOR EACH eb NO-LOCK 
        WHERE eb.company EQ bf-est.company
        AND eb.est-no EQ bf-est.est-no:
    /*Add Blank validations here*/
    END.
    FOR EACH est-op NO-LOCK
        WHERE est-op.company EQ bf-est.company
        AND est-op.est-no  EQ bf-est.est-no
        AND est-op.line    LT 500,
        FIRST mach NO-LOCK
        WHERE mach.company EQ bf-est.company
        AND mach.loc EQ bf-est.loc
        AND mach.m-code EQ est-op.m-code:
    
        IF mach.obsolete THEN 
            RUN pAddError ("Machine: " + TRIM(mach.m-code) + " is obsolete", est-op.s-num, est-op.b-num, est-op.op-pass,YES).
    
    /*Other per operation validations*/
    END.

END PROCEDURE.
PROCEDURE pExportTempTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Exports the contents of any temp-table into CSV   
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTTname AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hQuery AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    OUTPUT STREAM sOutput to VALUE(ipcFileName).

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS (iphTT:DEFAULT-BUFFER-HANDLE).

    hQuery:QUERY-PREPARE("FOR EACH " + ipcTTName).
    hQuery:QUERY-OPEN().

    DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
        IF iphTT:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):DATA-TYPE NE "ROWID" THEN
            PUT STREAM sOutput UNFORMATTED iphTT:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):NAME + ",".
    END.
    PUT STREAM sOutput UNFORMATTED SKIP.

    REPEAT:  
        hQuery:GET-NEXT().  
        IF hQuery:QUERY-OFF-END THEN LEAVE.  

        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
            IF iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):DATA-TYPE NE "ROWID" THEN 
                PUT STREAM sOutput UNFORMATTED 
                    '"' iphTT:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):BUFFER-VALUE '",'.
        END.
        PUT STREAM sOutput UNFORMATTED SKIP.
    END.
    OUTPUT STREAM sOutput CLOSE.

END PROCEDURE.
PROCEDURE pExportXML:
    /*------------------------------------------------------------------------------
     Purpose: Exports the contents of the temp-table to XML
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO. 
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRetOK          AS LOGICAL   NO-UNDO.

    /* Code to populate the temp-table  */
    ASSIGN
        cTargetType     = "file"
        lFormatted      = TRUE
        cEncoding       = ?
        cSchemaLocation = ?
        lWriteSchema    = FALSE
        lMinSchema      = FALSE.

    lRetOK = iphTT:WRITE-XML(cTargetType, ipcFile,lFormatted, cEncoding,
        cSchemaLocation, lWriteSchema, lMinSchema).

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetPromptForVendor RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the log value of CEVendor
     Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE VARIABLE cPromptForVendor AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound           AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CEVendor",
        INPUT "L",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cPromptForVendor,
        OUTPUT lFound).
    
    RETURN lFound AND cPromptForVendor EQ "YES".
		
END FUNCTION.

FUNCTION fGetCERun RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcIndustry AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the NK1 char value of CERUNF
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
	
    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CERun" + ipcIndustry,
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cFormat,
        OUTPUT lFound).
    IF NOT lFound THEN cFormat = "ASI".
	
    RETURN cFormat.
		
END FUNCTION.

FUNCTION fGetRecKey RETURNS CHARACTER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next rec key for assignment into new table
     Notes:
    ------------------------------------------------------------------------------*/	

    /*    RETURN DYNAMIC-FUNCTION("sfGetNextRecKey").*/
    RETURN STRING(YEAR(TODAY),"9999")
        + STRING(MONTH(TODAY),"99")
        + STRING(DAY(TODAY),"99")
        + STRING(TIME,"99999")
        + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
        .
		
END FUNCTION.

FUNCTION fGetUserID RETURNS CHARACTER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    /*    RETURN DYNAMIC-FUNCTION("sfGetUserID").*/
    RETURN USERID("asi").

		
END FUNCTION.

