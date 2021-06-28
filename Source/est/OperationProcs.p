
/*------------------------------------------------------------------------
    File        : OperationProcs.p
    Purpose     : 

    Syntax      :

    Description : Procedure for calculating machine standards, routing, etc.			

    Author(s)   : BV
    Created     : Wed Sep 16 14:09:32 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est\ttOperationAttribute.i}

DEFINE TEMP-TABLE ttOperation NO-UNDO
    LIKE estCostOperation
    FIELD linealFeetPerFeed AS DECIMAL
    FIELD estType           AS CHARACTER
    FIELD quantityMaster    AS DECIMAL 
    .
DEFINE TEMP-TABLE ttEstBlank NO-UNDO 
    FIELD estCostBlankID      AS INT64
    FIELD estCostFormID       AS INT64
    FIELD iOut                AS INTEGER
    FIELD dQtyInOut           AS DECIMAL
    FIELD dQtyInOutRunWaste   AS DECIMAL
    FIELD dQtyInOutSetupWaste AS DECIMAL
    FIELD lOutputInitialized  AS LOGICAL /*Truly a temp-table field and not a db field*/
    .
DEFINE TEMP-TABLE ttAxis NO-UNDO
    FIELD axisType       AS CHARACTER 
    FIELD axisCoordinate AS INTEGER 
    FIELD axisValue      AS DECIMAL
    FIELD axisPage       AS INTEGER
    .

DEFINE TEMP-TABLE ttJobMch NO-UNDO LIKE job-mch 
    FIELD riJobMch    AS ROWID
    FIELD isExtraCopy AS LOGICAL
    .


/* Define Globals - Machine Attributes */

DEFINE VARIABLE giAttributeIDNoOfColors       AS INTEGER NO-UNDO INITIAL 1.    //# of Colors
DEFINE VARIABLE giAttributeIDBoxLen           AS INTEGER NO-UNDO INITIAL 2.    //Box Length
DEFINE VARIABLE giAttributeIDBoxWid           AS INTEGER NO-UNDO INITIAL 3.    //Box Width
DEFINE VARIABLE giAttributeIDBlankLen         AS INTEGER NO-UNDO INITIAL 4.    //Blank Length
DEFINE VARIABLE giAttributeIDBlankWid         AS INTEGER NO-UNDO INITIAL 5.    //Blank Width
DEFINE VARIABLE giAttributeIDGlueLapLen       AS INTEGER NO-UNDO INITIAL 6.    //Glue Lap Length
DEFINE VARIABLE giAttributeIDBlankSqFt        AS INTEGER NO-UNDO INITIAL 7.    //Blank Sq In/Sq Ft
DEFINE VARIABLE giAttributeIDCaliper          AS INTEGER NO-UNDO INITIAL 8.    //Caliper Thickness
DEFINE VARIABLE giAttributeIDWeightperMSF     AS INTEGER NO-UNDO INITIAL 9.    //Weight per MSF
DEFINE VARIABLE giAttributeIDRollWid          AS INTEGER NO-UNDO INITIAL 10.    //Roll Width
DEFINE VARIABLE giAttributeIDNShtWid          AS INTEGER NO-UNDO INITIAL 11.    //Net Sheet Width
DEFINE VARIABLE giAttributeIDNShtLen          AS INTEGER NO-UNDO INITIAL 12.    //Net Sheet Length
DEFINE VARIABLE giAttributeIDDieNumberUp      AS INTEGER NO-UNDO INITIAL 13.    //Die Number Up
DEFINE VARIABLE giAttributeIDFilmLen          AS INTEGER NO-UNDO INITIAL 14.    //Film Length
DEFINE VARIABLE giAttributeIDFilmWid          AS INTEGER NO-UNDO INITIAL 15.    //Film Width
DEFINE VARIABLE giAttributeIDGShtLen          AS INTEGER NO-UNDO INITIAL 16.    //Gross Sht Length
DEFINE VARIABLE giAttributeIDGShtWid          AS INTEGER NO-UNDO INITIAL 17.    //Gross Sht Width
DEFINE VARIABLE giAttributeIDEstQty           AS INTEGER NO-UNDO INITIAL 18.    //Estimate Qty
DEFINE VARIABLE giAttributeIDDieLIIn          AS INTEGER NO-UNDO INITIAL 19.    //Die Lineal Inches
DEFINE VARIABLE giAttributeIDEstSheets        AS INTEGER NO-UNDO INITIAL 20.    //Estimated Sheets
DEFINE VARIABLE giAttributeIDSheetSqIn        AS INTEGER NO-UNDO INITIAL 21.    //Sheet Square Inches
DEFINE VARIABLE giAttributeIDNoOfPlates       AS INTEGER NO-UNDO INITIAL 22.    //Number of Plates
DEFINE VARIABLE giAttributeIDNoOfCuts         AS INTEGER NO-UNDO INITIAL 23.    //Number of Cuts
DEFINE VARIABLE giAttributeIDNoOfItems        AS INTEGER NO-UNDO INITIAL 24.    //Number of Items
DEFINE VARIABLE giAttributeIDNoOutGShtWid     AS INTEGER NO-UNDO INITIAL 25.    //# Out GrsSht Width
DEFINE VARIABLE giAttributeIDNoOutGShtLen     AS INTEGER NO-UNDO INITIAL 26.    //# Out Grs Sht Length
DEFINE VARIABLE giAttributeIDSetPartsperForm  AS INTEGER NO-UNDO INITIAL 27.    //Set Parts per Form
DEFINE VARIABLE giAttributeIDInkCoverage      AS INTEGER NO-UNDO INITIAL 28.    //Ink Coverage
DEFINE VARIABLE giAttributeIDPartsperSet      AS INTEGER NO-UNDO INITIAL 29.    //Parts per Set
DEFINE VARIABLE giAttributeIDRoutingNoOut     AS INTEGER NO-UNDO INITIAL 30.    //Routing # Out
DEFINE VARIABLE giAttributeIDQtySetLongs      AS INTEGER NO-UNDO INITIAL 31.    //Qty/Set Longs
DEFINE VARIABLE giAttributeIDQtySetShorts     AS INTEGER NO-UNDO INITIAL 32.    //Qty/Set Shorts
DEFINE VARIABLE giAttributeIDNoOnDieLen       AS INTEGER NO-UNDO INITIAL 33.    //# On Die Length
DEFINE VARIABLE giAttributeIDNoOnDieWid       AS INTEGER NO-UNDO INITIAL 34.    //# On Die Width
DEFINE VARIABLE giAttributeIDBlankSqIn        AS INTEGER NO-UNDO INITIAL 35.    //Blank Sq In
DEFINE VARIABLE giAttributeIDUnitizingFormula AS INTEGER NO-UNDO INITIAL 98.    //Unitizing Formula
DEFINE VARIABLE giAttributeIDDieHoursFormula  AS INTEGER NO-UNDO INITIAL 99.    //Die Hours Formula
DEFINE VARIABLE giAttributeIDStyle            AS INTEGER NO-UNDO INITIAL 101.   //Style
DEFINE VARIABLE giAttributeIDBoardItemID      AS INTEGER NO-UNDO INITIAL 102.   //Board ItemID
DEFINE VARIABLE giAttributeIDBoxDepth         AS INTEGER NO-UNDO INITIAL 104.   //Box Depth
 

DEFINE VARIABLE gcDeptsForPrinters       AS CHARACTER NO-UNDO INITIAL "PR".
DEFINE VARIABLE gcDeptsForGluers         AS CHARACTER NO-UNDO INITIAL "GL,QS".
DEFINE VARIABLE gcDeptsForLeafers        AS CHARACTER NO-UNDO INITIAL "WN,WS,FB,FS".
DEFINE VARIABLE gcDeptsForSheeters       AS CHARACTER NO-UNDO INITIAL "RC,RS,CR".
DEFINE VARIABLE gcDeptsForCoaters        AS CHARACTER NO-UNDO INITIAL "PR,CT".
DEFINE VARIABLE glOpRatesSeparate        AS LOGICAL   NO-UNDO INITIAL YES.    /*CEOpRates - log val*/   


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */
FUNCTION fGetAttributeName RETURNS CHARACTER PRIVATE
    (ipiAttributeID AS INTEGER) FORWARD.

FUNCTION fGetAttributeValue RETURNS DECIMAL PRIVATE
    (ipiAttributeID AS INTEGER) FORWARD.

FUNCTION fGetColorsForBlankPass RETURNS INTEGER PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipiPass AS INTEGER) FORWARD.

FUNCTION fGetFeet RETURNS DECIMAL PRIVATE
    (ipdDim AS DECIMAL,
    ipcUOM AS CHARACTER) FORWARD.

FUNCTION fHasDataCollected RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-job-mch FOR job-mch) FORWARD.

FUNCTION fIsAssemblyFeed RETURNS LOGICAL PRIVATE
    (ipcFeedType AS CHARACTER) FORWARD.

FUNCTION fIsAssemblyPartFeed RETURNS LOGICAL PRIVATE
    (ipcFeedType AS CHARACTER) FORWARD.

FUNCTION fIsDepartment RETURNS LOGICAL PRIVATE
    (ipcDepartment AS CHARACTER,
    ipcDepartmentList AS CHARACTER EXTENT 4) FORWARD.

FUNCTION fIsInk RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER,
    ipcItemID AS CHARACTER) FORWARD.

FUNCTION fIsOperationFound RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER,
    ipcOperationID AS CHARACTER,
    ipiJob AS INTEGER,
    ipiFormNo AS INTEGER,
    ipiBlankNo AS INTEGER,
    ipiPass AS INTEGER,
    ipcDepartmentID AS CHARACTER) FORWARD.

FUNCTION fGetNetSheetOut RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER, ipcEstimateID AS CHARACTER, ipiFormNo AS INTEGER, ipiPass AS INTEGER, ipiDefaultOut AS INTEGER) FORWARD.
    
FUNCTION fGetOperationsColor RETURNS INTEGER PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipiPass AS INTEGER) FORWARD.  
     
FUNCTION fGetOperationsCalThickness RETURNS INTEGER PRIVATE
    (BUFFER ipbf-ef FOR ef) FORWARD.
    
FUNCTION fGetOperationsQty RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipcLocationID AS CHARACTER,
    ipiPass AS INTEGER) FORWARD.   
     
FUNCTION fGetDieNumberUp RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER) FORWARD. 
        
FUNCTION fGetOperationsEstSheet RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipiPass AS INTEGER) FORWARD. 
     
FUNCTION fGetOperationsGrsShtWid RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipiPass AS INTEGER) FORWARD.      
     
FUNCTION fGetOperationsPartPerSet RETURNS INTEGER PRIVATE
    (BUFFER ipbf-eb FOR eb,     
    ipiPartPerSet AS INTEGER,
    ipcSetCount AS CHARACTER) FORWARD.  
     
FUNCTION fGetOperationsInkCoverage RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb) FORWARD.   

FUNCTION fGetPartCount RETURNS DECIMAL PRIVATE
    (ipcCompany AS CHARACTER, ipcEstimateID AS CHARACTER) FORWARD.    

FUNCTION fGetJobMachRunQty RETURNS DECIMAL PRIVATE
    (ipcCompany AS CHARACTER, ipiJob AS INTEGER, ipiFormNo AS INTEGER) FORWARD.      

FUNCTION fIsSetType RETURNS LOGICAL PRIVATE
    (ipcType AS CHARACTER) FORWARD.

FUNCTION fRoundUp RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL) FORWARD.

FUNCTION fGetBlankSqFtArea RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb) FORWARD. 

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE BuildRouting:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE ClearAttributes:
    /*------------------------------------------------------------------------------
     Purpose:  Clears all attributes
     Notes:
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAttribute.

END PROCEDURE.


PROCEDURE GetAttributes:
    /*------------------------------------------------------------------------------
     Purpose: Returns the temp-table to caller
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttAttribute.

END PROCEDURE.

PROCEDURE GetOperationRates:
    /*------------------------------------------------------------------------------
     Purpose:  Returns Rates for given machine
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateMRLabor AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateMRFixedOverhead AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateMRVariableOverhead AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateRunLabor AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateRunFixedOverhead AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateRunVariableOverhead AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mach FOR mach.
    
    RUN pGetMachineBuffer(ipcCompany, ipcLocationID, ipcOperationID, BUFFER bf-mach, OUTPUT oplError, OUTPUT opcMessage).
    IF AVAILABLE bf-mach THEN 
    DO:
        ASSIGN 
            opdRateMRFixedOverhead     = bf-mach.mr-fixoh
            opdRateMRVariableOverhead  = bf-mach.mr-varoh
            opdRateRunFixedOverhead    = bf-mach.run-fixoh
            opdRateRunVariableOverhead = bf-mach.run-varoh
            opdRateMRLabor             = bf-mach.mr-rate  // Refactor:  OpRatesSeparate & variable crew size
            opdRateRunLabor            = bf-mach.run-rate //Refactor: OpRatesSeparate & variable crew size
            .
            
    END.
END PROCEDURE.

PROCEDURE GetOperationStandards:
    /*------------------------------------------------------------------------------
     Purpose: Given a company and machine code, return standards for the machine
        based on the current context of attributes
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOpMRWaste AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOpMRHours AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOpRunSpeed AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOpRunSpoil AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mach FOR mach.
    DEFINE VARIABLE cMessageMRWaste  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessageRunSpeed AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessageMRHours  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessageRunSpoil AS CHARACTER NO-UNDO.
            
    RUN pGetMachineBuffer(ipcCompany, ipcLocationID, ipcOperationID, BUFFER bf-mach, OUTPUT oplError, OUTPUT opcMessage).

    IF AVAILABLE bf-mach THEN 
    DO:           
        RUN pGetMRWaste(BUFFER bf-mach, OUTPUT opdOpMRWaste, OUTPUT oplError, OUTPUT cMessageMRWaste).
        RUN pGetRunSpeed(BUFFER bf-mach, OUTPUT opdOpRunSpeed, OUTPUT oplError, OUTPUT cMessageRunSpeed).
        RUN pGetMRHours(BUFFER bf-mach, OUTPUT opdOpMRHours, OUTPUT oplError, OUTPUT cMessageMRHours).
        RUN pGetRunSpoil(BUFFER bf-mach, OUTPUT opdOpRunSpoil, OUTPUT oplError, OUTPUT cMessageRunSpoil).
    END.
    RUN pBuildMessage(cMessageMRWaste, INPUT-OUTPUT opcMessage).
    RUN pBuildMessage(cMessageRunSpeed, INPUT-OUTPUT opcMessage).
    RUN pBuildMessage(cMessageMRHours, INPUT-OUTPUT opcMessage).
    RUN pBuildMessage(cMessageRunSpoil, INPUT-OUTPUT opcMessage).

END PROCEDURE.


PROCEDURE GetOperationStandardsForJobMch:
    /*------------------------------------------------------------------------------
     Purpose: given job-mch rowid, get updated standards
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJobMch AS ROWID.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-eb      FOR eb.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    FIND bf-job-mch NO-LOCK 
        WHERE ROWID(bf-job-mch) EQ ipriJobMch
        NO-ERROR.
    IF AVAILABLE bf-job-mch THEN 
    DO:
        FIND FIRST bf-job NO-LOCK 
            WHERE bf-job.company EQ bf-job-mch.company
            AND bf-job.job EQ bf-job-mch.job
            NO-ERROR.
        IF NOT AVAILABLE bf-job THEN RETURN.
        FIND FIRST bf-eb NO-LOCK 
            WHERE bf-eb.company EQ bf-job-mch.company
            AND bf-eb.est-no EQ bf-job.est-no
            AND bf-eb.form-no EQ bf-job-mch.frm
            AND bf-eb.blank-no EQ MAX(bf-job-mch.blank-no,1)
            NO-ERROR.
            
        RUN SetAttributesFromJobMch (ROWID(bf-job-mch), bf-job-mch.m-code, bf-job-mch.pass, OUTPUT lError, OUTPUT cMessage).
        IF NOT lError THEN 
        DO:
            FIND CURRENT bf-job-mch EXCLUSIVE-LOCK.
            RUN GetOperationRates(bf-job-mch.company, bf-job.loc, bf-job-mch.m-code, 
                OUTPUT bf-job-mch.mr-rate, 
                OUTPUT bf-job-mch.mr-fixoh, 
                OUTPUT bf-job-mch.mr-varoh,
                OUTPUT bf-job-mch.run-rate,
                OUTPUT bf-job-mch.run-fixoh,
                OUTPUT bf-job-mch.run-varoh,
                OUTPUT lError, OUTPUT cMessage).
            RUN GetOperationStandards(bf-job-mch.company, bf-job.loc, bf-job-mch.m-code,
                OUTPUT bf-job-mch.mr-waste, 
                OUTPUT bf-job-mch.mr-hr, 
                OUTPUT bf-job-mch.speed, 
                OUTPUT bf-job-mch.wst-prct, 
                OUTPUT lError, OUTPUT cMessage).
            bf-job-mch.run-qty =  fGetJobMachRunQty(bf-job-mch.company, bf-job-mch.job, bf-job-mch.frm).
        END.
    END.

END PROCEDURE.

PROCEDURE pAddAxis PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcAxisType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPage AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeaderValues AS DECIMAL NO-UNDO EXTENT.
    
    DEFINE VARIABLE iIndex  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.
    
    iExtent = EXTENT(ipdHeaderValues).
    DO iIndex = 1 TO iExtent:
        IF ipdHeaderValues[iIndex] NE 0 THEN 
        DO:
            CREATE ttAxis.
            ASSIGN 
                ttAxis.axisType       = ipcAxisType
                ttAxis.axisValue      = ipdHeaderValues[iIndex]
                ttAxis.axisCoordinate = iIndex
                ttAxis.axisPage       = ipiPage 
                .
        END.
    END.

END PROCEDURE.

PROCEDURE pAddEstOperationFromEstOp PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an estCostOperation based on est-op and form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-op        FOR est-op.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE PARAMETER BUFFER opbf-ttOperation   FOR ttOperation.

    DEFINE           BUFFER bf-mach            FOR mach.
    DEFINE           BUFFER bf-est-op          FOR est-op.

    FIND FIRST bf-mach NO-LOCK 
        WHERE bf-mach.company EQ ipbf-est-op.company
        AND bf-mach.m-code EQ ipbf-est-op.m-code
        NO-ERROR.
    IF AVAILABLE bf-mach THEN 
    DO:
        CREATE opbf-ttOperation.
        ASSIGN 
            opbf-ttOperation.estCostFormID                = ipbf-estCostForm.estCostFormID
            opbf-ttOperation.estCostHeaderID              = ipbf-estCostForm.estCostHeaderID
            opbf-ttOperation.estimateNo                   = ipbf-estCostForm.estimateNo
            opbf-ttOperation.estType                      = ipbf-estCostHeader.estType
            opbf-ttOperation.quantityMaster               = ipbf-estCostHeader.quantityMaster
            opbf-ttOperation.formNo                       = ipbf-est-op.s-num
            opbf-ttOperation.blankNo                      = ipbf-est-op.b-num
            //opbf-ttOperation.estCostBlankID               = fGetEstBlankID(opbf-ttOperation.estCostBlankID, opbf-ttOperation.estCostFormID, opbf-ttOperation.blankNo)
            opbf-ttOperation.company                      = ipbf-est-op.company
            opbf-ttOperation.operationID                  = ipbf-est-op.m-code
            opbf-ttOperation.pass                         = MAX(ipbf-est-op.op-pass, 1)
            opbf-ttOperation.sequenceOfOperation          = ipbf-est-op.line
            opbf-ttOperation.numOutDivisor                = ipbf-est-op.n_out_div
                       
            opbf-ttOperation.quantityInSetupWaste         = ipbf-est-op.op-waste
            opbf-ttOperation.hoursSetup                   = ipbf-est-op.op-mr
            opbf-ttOperation.speed                        = ipbf-est-op.op-speed
            opbf-ttOperation.quantityInRunWastePercent    = ipbf-est-op.op-spoil
            opbf-ttOperation.isLocked                     = ipbf-est-op.isLocked
            opbf-ttOperation.crewSizeSetup                = ipbf-est-op.op-crew[1]
            opbf-ttOperation.crewSizeRun                  = ipbf-est-op.op-crew[2]
            opbf-ttOperation.countInks                    = ipbf-est-op.num-col
            opbf-ttOperation.countCoats                   = ipbf-est-op.num-coat
            opbf-ttOperation.countFountainChanges         = ipbf-est-op.fountains
            opbf-ttOperation.countPlateChanges            = ipbf-est-op.plates
            
            opbf-ttOperation.isSpeedInLF                  = bf-mach.therm
            opbf-ttOperation.operationName                = bf-mach.m-dscr
            opbf-ttOperation.feedType                     = bf-mach.p-type
            opbf-ttOperation.outputType                   = opbf-ttOperation.feedType
            opbf-ttOperation.departmentIDPrimary          = bf-mach.dept[1]
            opbf-ttOperation.departmentID                 = bf-mach.dept
            opbf-ttOperation.quantityInSetupWastePerColor = bf-mach.col-wastesh
            opbf-ttOperation.costPerHourFOSetup           = bf-mach.mr-fixoh
            opbf-ttOperation.costPerHourFORun             = bf-mach.run-fixoh
            opbf-ttOperation.costPerHourVOSetup           = bf-mach.mr-varoh
            opbf-ttOperation.costPerHourVORun             = bf-mach.run-varoh
            opbf-ttOperation.quantityInkLbsWastedPerSetup = bf-mach.ink-waste
            opbf-ttOperation.quantityInkLbsWastedPerColor = bf-mach.col-wastelb
            opbf-ttOperation.hoursRunMinimum              = bf-mach.minRunHours
            opbf-ttOperation.costMinimum                  = bf-mach.mrk-rate
            .

        IF glOpRatesSeparate THEN 
            ASSIGN 
                opbf-ttOperation.costPerManHourDLSetup = bf-mach.lab-rate[1]
                opbf-ttOperation.costPerManHourDLRun   = bf-mach.lab-rate[2]
                .
        ELSE 
            ASSIGN 
                opbf-ttOperation.costPerManHourDLSetup = bf-mach.lab-rate[bf-mach.lab-drate]
                opbf-ttOperation.costPerManHourDLRun   = bf-mach.lab-rate[bf-mach.lab-drate]
                .
            
       
        IF fIsDepartment(gcDeptsForPrinters, opbf-ttOperation.departmentID) THEN  
            opbf-ttOperation.isPrinter = YES.
        IF fIsDepartment(gcDeptsForCoaters, opbf-ttOperation.departmentID) THEN  
            opbf-ttOperation.isCoater = YES.
        IF fIsDepartment(gcDeptsForSheeters, opbf-ttOperation.departmentID) THEN 
        DO: 
            IF NOT CAN-FIND(FIRST estCostOperation 
                WHERE estCostOperation.estCostHeaderID EQ opbf-ttOperation.estCostHeaderID
                AND estCostOperation.estCostFormID EQ ipbf-estCostForm.estCostFormID
                AND estCostOperation.isNetSheetMaker
                AND estCostOperation.estCostOperationID NE opbf-ttOperation.estCostOperationID) THEN   
                opbf-ttOperation.isNetSheetMaker = YES.
            
            opbf-ttOperation.outputType      = "S".
        END.
        IF fIsDepartment(gcDeptsForGluers, opbf-ttOperation.departmentID)  THEN 
            opbf-ttOperation.isGluer = YES.
        IF fIsDepartment(gcDeptsForLeafers, opbf-ttOperation.departmentID)  THEN 
            opbf-ttOperation.isLeafer = YES.
        
        IF CAN-DO("R,S,A,P",opbf-ttOperation.feedType) THEN 
        DO:
            FOR EACH bf-est-op NO-LOCK 
                WHERE bf-est-op.company EQ ipbf-est-op.company
                AND bf-est-op.est-no EQ ipbf-est-op.est-no
                AND bf-est-op.s-num EQ ipbf-est-op.s-num
                AND bf-est-op.qty EQ ipbf-est-op.qty
                AND bf-est-op.line GT ipbf-est-op.line
                AND bf-est-op.line LT 500,
                FIRST bf-mach NO-LOCK 
                WHERE bf-mach.company EQ bf-est-op.company
                AND bf-mach.m-code EQ bf-est-op.m-code 
                BY bf-est-op.line:
                IF bf-mach.p-type EQ "B" THEN  /*Last machine before a blank fed*/
                    ASSIGN 
                        opbf-ttOperation.isBlankMaker = YES
                        opbf-ttOperation.outputType   = "B"
                        .
                LEAVE.
            END.
            IF NOT AVAILABLE bf-est-op THEN /*Last Machine*/  
                ASSIGN 
                    opbf-ttOperation.isBlankMaker = YES
                    opbf-ttOperation.outputType   = "B"
                    .
        END.
        
        CASE opbf-ttOperation.feedType:
            WHEN "R" THEN 
                opbf-ttOperation.linealFeetPerFeed = fGetFeet(ipbf-estCostForm.grossLength, ipbf-estCostForm.dimUOM).
            WHEN "S" THEN 
                DO:
                    IF opbf-ttOperation.isNetSheetMaker THEN 
                        opbf-ttOperation.linealFeetPerFeed = fGetFeet(ipbf-estCostForm.grossLength, ipbf-estCostForm.dimUOM).
                    ELSE
                        opbf-ttOperation.linealFeetPerFeed = fGetFeet(ipbf-estCostForm.netLength, ipbf-estCostForm.dimUOM).
                END.
        END CASE.
        
        opbf-ttOperation.numOutForOperation = 1.
        IF opbf-ttOperation.isNetSheetMaker THEN 
            ASSIGN 
                opbf-ttOperation.numOutForOperation = fGetNetSheetOut(opbf-ttOperation.company, opbf-ttOperation.estimateNo, opbf-ttOperation.formNo, opbf-ttOperation.pass, ipbf-estCostForm.numOutNet)
                .
        IF opbf-ttOperation.isBlankMaker THEN 
            ASSIGN 
                opbf-ttOperation.numOutForOperation = opbf-ttOperation.numOutForOperation * ipbf-estCostForm.numOutBlanksOnNet.
                
        IF opbf-ttOperation.blankNo NE 0 THEN 
        DO:
            FIND FIRST estCostBlank NO-LOCK 
                WHERE estCostBlank.estCostHeaderID EQ opbf-ttOperation.estCostHeaderID
                AND estCostBlank.estCostFormID EQ opbf-ttOperation.estCostFormID
                AND estCostBlank.blankNo EQ opbf-ttOperation.blankNo
                NO-ERROR.
            IF AVAILABLE estCostBlank THEN 
                opbf-ttOperation.estCostBlankID = estCostBlank.estCostBlankID. 
        END.
    END.
    
END PROCEDURE.

PROCEDURE pBuildMessage PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Builds a message string based on new message and existing message
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMessageAdd AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.
    
    IF ipcMessageAdd NE "" THEN 
        IF iopcMessage NE "" THEN
            iopcMessage = iopcMessage + CHR(13) + ipcMessageAdd.
        ELSE 
            iopcMessage = ipcMessageAdd.

END PROCEDURE.

PROCEDURE pGetEffectiveEstOpQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  For a given estimate (company, estimate) and master quantity,
        find the appropriate est-op (routing definition) record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityMaster AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityTarget AS DECIMAL NO-UNDO.
  
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipcCompany
        AND est-op.est-no  EQ ipcEstimateID 
        AND est-op.line    LT 500
        BREAK BY est-op.qty:
    
        IF FIRST-OF(est-op.qty) THEN 
        DO:
            /*Refactor - Qty in Ops must match one of the est-qty?*/
            IF FIRST(est-op.qty) OR
                CAN-FIND(FIRST est-qty
                WHERE est-qty.company EQ est-op.company
                AND est-qty.est-no  EQ est-op.est-no
                AND est-qty.eqty    EQ est-op.qty)
                THEN opdQuantityTarget = est-op.qty.
            IF est-op.qty GE ipdQuantityMaster THEN LEAVE.
        END.
    END.

END PROCEDURE.

PROCEDURE pGetMachineBuffer PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Return the machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-mach FOR mach.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    FIND FIRST opbf-mach NO-LOCK 
        WHERE opbf-mach.company EQ ipcCompany
        AND opbf-mach.loc EQ ipcLocationID
        AND opbf-mach.m-code EQ ipcOperationID
        NO-ERROR.
    IF NOT AVAILABLE opbf-mach THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Machine Code"
            .
        RETURN. 
    END. 
    
    

END PROCEDURE.

PROCEDURE pGetMatrixSpeedReduction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mstd FOR mstd.
    DEFINE OUTPUT PARAMETER opdReduction AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dCaliper          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCaliper          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDepth            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dDepth            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDepthReduction   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCaliperReduction AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cDepthMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCaliperMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName             AS CHARACTER NO-UNDO.
     
    RUN pGetAttribute(giAttributeIDCaliper, OUTPUT cCaliper, OUTPUT cName, OUTPUT oplError, OUTPUT cMessage).
    dCaliper = DECIMAL(cCaliper).
    RUN pGetAttribute(giAttributeIDBoxDepth, OUTPUT cDepth, OUTPUT cName, OUTPUT oplError, OUTPUT cMessage).
    dDepth = DECIMAL(cDepth).
    IF AVAILABLE ipbf-mstd THEN 
    DO:
        DO iIndex = 1 TO EXTENT(ipbf-mstd.board-cal):
            IF ipbf-mstd.board-cal[iIndex] GE dCaliper AND ipbf-mstd.board-cal[iIndex] NE 0 THEN
            DO:
                ASSIGN 
                    dCaliperReduction = ipbf-mstd.spd-reduc[iIndex] / 100 
                    cCaliperMessage   = "Caliper reduction of " + STRING(dCaliperReduction) + " for caliper of " + cCaliper + " index level " + STRING(iIndex).
                LEAVE.
            END. 
        END.
        DO iIndex = 1 TO EXTENT(ipbf-mstd.board-depth):
            IF ipbf-mstd.board-depth[iIndex] GE dDepth AND ipbf-mstd.board-depth[iIndex] NE 0 THEN 
            DO:
                ASSIGN 
                    dDepthReduction = ipbf-mstd.depth-reduc[iIndex] / 100
                    cDepthMessage   = "Depth reduction of " + STRING(opdReduction) + " for depth of " + cDepth + " index level " + STRING(iIndex).
                .
                LEAVE.
            END.
        END.    
        opdReduction = dDepthReduction + dCaliperReduction.
        RUN pBuildMessage(cCaliperMessage, INPUT-OUTPUT opcMessage).
        RUN pBuildMessage(cDepthMessage, INPUT-OUTPUT opcMessage).
                 
    END.
END PROCEDURE.

PROCEDURE pGetValue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a standards buffer and type, return the value based on attribute context
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mstd FOR mstd.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValue AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mmtx FOR mmtx.
    DEFINE BUFFER bf-mmty FOR mmty.
    
    DEFINE VARIABLE dXAttributeValue AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cXAttribute      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dYAttributeValue AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cYAttribute      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iX               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iY               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPageX           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPageY           AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttAxis.
    CASE ipcType:
        WHEN "RunSpeed" THEN 
            DO:
                ASSIGN 
                    dXAttributeValue = fGetAttributeValue(ipbf-mstd.rs-x)
                    dYAttributeValue = fGetAttributeValue(ipbf-mstd.rs-y)
                    cXAttribute      = fGetAttributeName(ipbf-mstd.rs-x)
                    cYAttribute      = fGetAttributeName(ipbf-mstd.rs-y)
                    .
                FOR EACH bf-mmtx NO-LOCK OF ipbf-mstd
                    WHERE bf-mmtx.mr-run EQ NO
                    AND bf-mmtx.page-no EQ 0
                    BY bf-mmtx.across-no:
                    RUN pAddAxis("X", bf-mmtx.across-no, bf-mmtx.col-value).
                END.
                FOR EACH bf-mmtx NO-LOCK OF ipbf-mstd
                    WHERE bf-mmtx.mr-run EQ NO
                    AND bf-mmtx.across-no EQ 0
                    BY bf-mmtx.page-no:
                    RUN pAddAxis("Y", bf-mmtx.page-no, bf-mmtx.row-value).
                END. 
                RUN pGetCoordinates(dXAttributeValue, "X", OUTPUT iX, OUTPUT iPageX).
                RUN pGetCoordinates(dYAttributeValue, "Y", OUTPUT iY, OUTPUT iPageY).
                IF iX NE 0 AND iY NE 0 THEN 
                    FIND FIRST bf-mmtx NO-LOCK OF ipbf-mstd
                        WHERE bf-mmtx.mr-run EQ NO
                        AND bf-mmtx.page-no EQ iPageY
                        AND bf-mmtx.across-no EQ iPageX
                        NO-ERROR.
                IF AVAILABLE bf-mmtx THEN 
                    opdValue = bf-mmtx.vals[10 * iY + iX].
                
            END.
        WHEN "RunSpoil" THEN 
            DO:
                ASSIGN 
                    dXAttributeValue = fGetAttributeValue(ipbf-mstd.sp-x)
                    dYAttributeValue = fGetAttributeValue(ipbf-mstd.sp-y)
                    cXAttribute      = fGetAttributeName(ipbf-mstd.sp-x)
                    cYAttribute      = fGetAttributeName(ipbf-mstd.sp-y)
                    .
                FOR EACH bf-mmtx NO-LOCK OF ipbf-mstd
                    WHERE bf-mmtx.mr-run EQ YES
                    AND bf-mmtx.page-no EQ 0
                    BY bf-mmtx.across-no:
                    RUN pAddAxis("X", bf-mmtx.across-no, bf-mmtx.col-value).
                END.
                FOR EACH bf-mmtx NO-LOCK OF ipbf-mstd
                    WHERE bf-mmtx.mr-run EQ YES
                    AND bf-mmtx.across-no EQ 0
                    BY bf-mmtx.page-no:
                    RUN pAddAxis("Y",bf-mmtx.page-no, bf-mmtx.row-value).
                END.
                RUN pGetCoordinates(dXAttributeValue, "X", OUTPUT iX, OUTPUT iPageX).
                RUN pGetCoordinates(dYAttributeValue, "Y", OUTPUT iY, OUTPUT iPageY).
                IF iX NE 0 AND iY NE 0 THEN 
                    FIND FIRST bf-mmtx NO-LOCK OF ipbf-mstd
                        WHERE bf-mmtx.mr-run EQ YES
                        AND bf-mmtx.page-no EQ iPageY
                        AND bf-mmtx.across-no EQ iPageX
                        NO-ERROR.
                IF AVAILABLE bf-mmtx THEN 
                    opdValue = bf-mmtx.vals[10 * iY + iX].
                
            END.        
        WHEN "MRHours" THEN 
            DO:
                ASSIGN 
                    dXAttributeValue = fGetAttributeValue(ipbf-mstd.mr-x)
                    dYAttributeValue = fGetAttributeValue(ipbf-mstd.mr-y)
                    cXAttribute      = fGetAttributeName(ipbf-mstd.mr-x)
                    cYAttribute      = fGetAttributeName(ipbf-mstd.mr-y)
                    .
                FOR EACH bf-mmty NO-LOCK OF ipbf-mstd
                    WHERE bf-mmty.page-no EQ 0
                    BY bf-mmty.across-no:
                    RUN pAddAxis("X", bf-mmty.across-no, bf-mmty.col-value).
                END.
                FOR EACH bf-mmty NO-LOCK OF ipbf-mstd
                    WHERE bf-mmty.across-no EQ 0
                    BY bf-mmty.page-no:
                    RUN pAddAxis("Y",bf-mmty.page-no, bf-mmty.row-value).
                END.
                RUN pGetCoordinates(dXAttributeValue, "X", OUTPUT iX, OUTPUT iPageX).
                RUN pGetCoordinates(dYAttributeValue, "Y", OUTPUT iY, OUTPUT iPageY).
                IF iX NE 0 AND iY NE 0 THEN 
                    FIND FIRST bf-mmty NO-LOCK OF ipbf-mstd
                        WHERE bf-mmty.page-no EQ iPageY
                        AND bf-mmty.across-no EQ iPageX
                        NO-ERROR.
                IF AVAILABLE bf-mmty THEN 
                DO: 
                    ASSIGN 
                        opdValue = bf-mmty.vals[10 * iY + iX].
                END.
            END.
            
    END CASE.
    RUN pBuildMessage(ipcType + ": " + STRING(opdValue) + " Matrix Style: " + (IF ipbf-mstd.style EQ "" THEN "[Blank]" ELSE ipbf-mstd.style), INPUT-OUTPUT opcMessage).
    RUN pBuildMessage("X Axis: Attribute: " + cXAttribute + " Attribute Value: " + STRING(dXAttributeValue) + " Page: " + STRING(iPageX) + " Index: " + STRING(iX), INPUT-OUTPUT opcMessage).
    RUN pBuildMessage("Y Axis: Attribute: " + cYAttribute + " Attribute Value: " + STRING(dYAttributeValue) + " Page: " + STRING(iPageY) + " Index: " + STRING(iY), INPUT-OUTPUT opcMessage).
    RUN pBuildMessage(CHR(13), INPUT-OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE pGetAttribute PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an attribute type, get the current value from context
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiAttributeID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAttributeValue AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAttributeName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    FIND FIRST ttAttribute NO-LOCK 
        WHERE ttAttribute.attributeID EQ ipiAttributeID
        NO-ERROR.
    IF AVAILABLE ttAttribute THEN 
        ASSIGN 
            opcAttributeName  = ttAttribute.attributeName
            opcAttributeValue = ttAttribute.attributeValue
            .
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Attribute: " + STRING(ipiAttributeID) + " not available"
            .
            
END PROCEDURE.

PROCEDURE pGetItemSpeedReduction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Get Speed Reduction for the Board item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDepartmentID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdReduction AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-item FOR item.
    
    DEFINE VARIABLE cDept      AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE cBoard     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dReduction AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cName      AS CHARACTER NO-UNDO.
      
    RUN pGetAttribute(giAttributeIDBoardItemID, OUTPUT cBoard, OUTPUT cName, OUTPUT oplError, OUTPUT opcMessage).
    
    IF oplError THEN RETURN.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipcCompany
        AND bf-item.i-no EQ cBoard
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Board Attribute: " + cBoard
            .
    ELSE 
    DO:
        DO iIndex = 1 TO 10:
            IF bf-item.dept-name[iIndex] EQ ipcDepartmentID AND bf-item.speed%[iIndex] NE 0 THEN  
            DO:
                ASSIGN 
                    opdReduction = bf-item.speed%[iIndex] / 100
                    opcMessage   = "Item specific reduction of " + STRING(opdReduction) + " for item " + TRIM(cBoard) + " for department " + ipcDepartmentID
                    .
                LEAVE.
            END.
        END.
             
    END.       
    
END PROCEDURE.

PROCEDURE pGetAttributeStyle PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an attribute type, get the current value from context
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcStyle AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-style FOR style.
    DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
   
    RUN pGetAttribute(giAttributeIDStyle, OUTPUT opcStyle, OUTPUT cName, OUTPUT oplError, OUTPUT opcMessage).
    
    IF oplError THEN RETURN.
    
    FIND FIRST bf-style NO-LOCK 
        WHERE bf-style.company EQ ipcCompany
        AND bf-style.style EQ opcStyle
        NO-ERROR.
    IF NOT AVAILABLE bf-style THEN 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Style Attribute: " + opcStyle
            .
                    
END PROCEDURE.

PROCEDURE pGetCoordinates PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given lookup values for X
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdLookup AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcAxisType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCoordinate AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiPage AS INTEGER NO-UNDO.
    
    FOR EACH ttAxis NO-LOCK
        WHERE ttAxis.axisType EQ ipcAxisType
        BY ttAxis.axisPage
        BY ttAxis.axisCoordinate:
        IF ipdLookup LE ttAxis.axisValue THEN LEAVE.
    END.
    IF AVAILABLE ttAxis THEN 
        ASSIGN 
            opiCoordinate = ttAxis.axisCoordinate
            opiPage       = ttAxis.axisPage
            . 
 
END PROCEDURE.

PROCEDURE pGetMRHours PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given company and machine code, return the MR Hours based on the 
        current attribute context.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mach FOR mach.
    DEFINE OUTPUT PARAMETER opdMRHours AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mstd FOR mstd.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN pGetStandardBuffer(ipbf-mach.company, ipbf-mach.loc, ipbf-mach.m-code, BUFFER bf-mstd, OUTPUT oplError, OUTPUT cMessage).
    IF AVAILABLE bf-mstd THEN 
    DO:
        RUN pGetValue(BUFFER bf-mstd, "MRHours", OUTPUT opdMRHours, OUTPUT oplError, OUTPUT opcMessage).
        
    END.
    
END PROCEDURE.


PROCEDURE pGetRunSpeed PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given company and machine code, return the Run Speed based on the 
        current attribute context.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mach FOR mach.
    DEFINE OUTPUT PARAMETER opdRunSpeed AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mstd FOR mstd.
    
    DEFINE VARIABLE dReductionItem          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dReductionMatrix        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dReductionTotal         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cMessageMatrix          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessageItemReduction   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessageMatrixReduction AS CHARACTER NO-UNDO.
    
    RUN pGetStandardBuffer(ipbf-mach.company, ipbf-mach.loc, ipbf-mach.m-code, BUFFER bf-mstd, OUTPUT oplError, OUTPUT opcMessage).
    IF AVAILABLE bf-mstd THEN 
    DO:
        RUN pGetValue(BUFFER bf-mstd, "RunSpeed", OUTPUT opdRunSpeed, OUTPUT oplError, OUTPUT cMessageMatrix).
        RUN pGetItemSpeedReduction(ipbf-mach.company, ipbf-mach.dept[1], OUTPUT dReductionItem, OUTPUT oplError, OUTPUT cMessageItemReduction).
        RUN pGetMatrixSpeedReduction(BUFFER bf-mstd, OUTPUT dReductionMatrix, OUTPUT oplError, OUTPUT cMessageMatrixReduction).
        ASSIGN 
            dReductionTotal = dReductionItem + dReductionMatrix
            opdRunSpeed     = opdRunSpeed * (1 - dReductionTotal).
        .
        RUN pBuildMessage(cMessageMatrix, INPUT-OUTPUT opcMessage).
        RUN pBuildMessage(cMessageItemReduction, INPUT-OUTPUT opcMessage).
        RUN pBuildMessage(cMessageMatrixReduction, INPUT-OUTPUT opcMessage).
    END.
    
END PROCEDURE.

PROCEDURE pGetRunSpoil PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given company and machine code, return the Run Spoil % based on the 
        current attribute context.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mach FOR mach.
    DEFINE OUTPUT PARAMETER opdRunSpoil AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mstd FOR mstd.
    
    RUN pGetStandardBuffer(ipbf-mach.company, ipbf-mach.loc, ipbf-mach.m-code, BUFFER bf-mstd, OUTPUT oplError, OUTPUT opcMessage).
    IF AVAILABLE bf-mstd THEN 
    DO:
        RUN pGetValue(BUFFER bf-mstd, "RunSpoil", OUTPUT opdRunSpoil, OUTPUT oplError, OUTPUT opcMessage).
        
    END.
    
END PROCEDURE.

PROCEDURE pGetMRWaste PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given company and machine code, return the Run Spoil % based on the 
        current attribute context.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mach FOR mach.
    DEFINE OUTPUT PARAMETER opdMRWaste AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cColors          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iColors          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dMRWasteFromMach AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dMRWasteFromInks AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAttrName        AS CHARACTER NO-UNDO.
            
    dMRWasteFromMach = ipbf-mach.mr-waste.
        
    IF fIsDepartment(gcDeptsForPrinters, ipbf-mach.dept) OR fIsDepartment(gcDeptsForCoaters, ipbf-mach.dept) THEN   
    DO:      
        RUN pGetAttribute(giAttributeIDNoOfColors, OUTPUT cColors, OUTPUT cAttrName, OUTPUT oplError, OUTPUT opcMessage). //Get colors attribute
        IF NOT oplError THEN 
            iColors = INTEGER(cColors).
        IF iColors GT 0 THEN
            dMRWasteFromInks = ipbf-mach.col-wastesh * iColors.                       
    END.          
    opdMRWaste = dMRWasteFromMach + dMRWasteFromInks. 
    RUN pBuildMessage("MRWaste: " + STRING(dMRWasteFromMach) + " from machine file", INPUT-OUTPUT opcMessage).
    RUN pBuildMessage("Waste for Inks: " + STRING(dMRWasteFromInks) + " from " + cColors + " colors", INPUT-OUTPUT opcMessage).
    RUN pBuildMessage(CHR(13), INPUT-OUTPUT opcMessage).
        
END PROCEDURE.        

PROCEDURE pGetStandardBuffer PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOpID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-mstd FOR mstd.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cStyle AS CHARACTER NO-UNDO.
    
    RUN pGetAttributeStyle(ipcCompany, OUTPUT cStyle, OUTPUT oplError, OUTPUT opcMessage).
    
    IF oplError THEN RETURN.
    
    FIND FIRST opbf-mstd NO-LOCK
        WHERE opbf-mstd.company EQ ipcCompany
        AND opbf-mstd.loc EQ ipcLocationID
        AND opbf-mstd.m-code EQ ipcOpID
        AND opbf-mstd.style EQ cStyle
        NO-ERROR.
    IF NOT AVAILABLE opbf-mstd THEN       /* maybe there's only a blank style mstd */
        FIND FIRST opbf-mstd NO-LOCK
            WHERE opbf-mstd.company EQ ipcCompany
            AND opbf-mstd.loc EQ ipcLocationID
            AND opbf-mstd.m-code EQ ipcOpID
            AND opbf-mstd.style EQ ""
            NO-ERROR.    
    IF NOT AVAILABLE opbf-mstd THEN       /* get any mstd */
        FIND FIRST opbf-mstd NO-LOCK
            WHERE opbf-mstd.company EQ ipcCompany
            AND opbf-mstd.loc EQ ipcLocationID
            AND opbf-mstd.m-code EQ ipcOpID
            NO-ERROR  .      

END PROCEDURE.

PROCEDURE pOperationChangeAddDepartment PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given all inputs, add a operation to the job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob AS INTEGER NO-UNDO. 
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDepartmentID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.

    DEFINE BUFFER bf-job     FOR job.
    
    FIND bf-job NO-LOCK 
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job EQ ipiJob
        NO-ERROR.
    IF NOT AVAILABLE bf-job THEN RETURN.
    
    CREATE bf-job-mch.
    ASSIGN
        bf-job-mch.company        = ipcCompany
        bf-job-mch.job            = bf-job.job
        bf-job-mch.job-no         = bf-job.job-no
        bf-job-mch.job-no2        = bf-job.job-no2
        bf-job-mch.frm            = ipiFormNo
        bf-job-mch.blank-no       = ipiBlankNo
        bf-job-mch.pass           = ipiPass
        bf-job-mch.m-code         = ipcOperationID
        bf-job-mch.dept           = ipcDepartmentID
        
        /* this let's SB know touch screen data collection made changes */
        bf-job-mch.est-op_rec_key = 'TS ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS')
        .
       
    RUN GetOperationStandardsForJobMch(ROWID(bf-job-mch)).
    
END PROCEDURE.

PROCEDURE pOperationChangeAddMachine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds a new machine to the job for the new operation ID.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-mch FOR job-mch.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    
    CREATE bf-job-mch.
    BUFFER-COPY ipbf-job-mch EXCEPT m-code job-mchID TO bf-job-mch.
    ASSIGN
        bf-job-mch.m-code = ipcOperationID
        .
    RUN GetOperationStandardsForJobMch(ROWID(bf-job-mch)).
        
END PROCEDURE.

PROCEDURE pOperationChangeDetermineAction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given job-mch buffer and machine code, determine through prompts or settings
        which action to take
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-mch FOR job-mch.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAction AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cMessage1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lChoice   AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE ipbf-job-mch THEN 
    DO:
        cMessage1 = "Machine " + TRIM(ipcOperationID) + " is not defined in job standards for this job/form/blank/pass.".
        IF fHasDataCollected(BUFFER ipbf-job-mch) THEN              
            ASSIGN
                cMessage2 = "Data has already been collected for " + TRIM(ipbf-job-mch.m-code) + ".  Would you like to add machine " + TRIM(ipcOperationID) + "?"
                opcAction = "Add"
                .
        ELSE
            ASSIGN
                cMessage2 = "Would you like to replace machine " + TRIM(ipbf-job-mch.m-code) + " with machine " + TRIM(ipcOperationID) + "?"
                opcAction = "Replace"
                .
    END.
    ELSE
        ASSIGN
            cMessage1 = "Machine " + TRIM(ipcOperationID) + " does not have a department that is valid for this job/form/blank/pass."
            cMessage2 = "Would you like to add the department to job standards?"
            opcAction = "AddDept"
            .

    MESSAGE cMessage1 SKIP cMessage2
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lChoice.        
    
    IF NOT lChoice THEN 
        opcAction = "Cancel".

END PROCEDURE.

PROCEDURE pOperationChangeReplaceMachine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an existing job-mch, replace the machine code
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-mch FOR job-mch.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    
    FIND CURRENT ipbf-job-mch EXCLUSIVE-LOCK.
    ipbf-job-mch.m-code = ipcOperationID.
    FIND CURRENT ipbf-job-mch NO-LOCK.
    RUN GetOperationStandardsForJobMch(ROWID(ipbf-job-mch)).    

END PROCEDURE.

PROCEDURE pProcessOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: should replace ce/prokalk.i and ce/pr4-mch.p
    ------------------------------------------------------------------------------*/
    //DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    //DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttOperation FOR ttOperation.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOut AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutSetupWaste AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutRunWaste AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-mach FOR mach.

    DEFINE VARIABLE iInkCoatCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQty          AS DECIMAL NO-UNDO. 
    DEFINE VARIABLE dLFPerFeed    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPartCount    AS DECIMAL NO-UNDO.
    
    ASSIGN 
        ipbf-ttOperation.quantityOut       = iopdQtyInOut  /*This machines out is last machines in*/  
        ipbf-ttOperation.quantityInNoWaste = ipbf-ttOperation.quantityOut / ipbf-ttOperation.numOutForOperation  /*Get QtyIn in Feed units*/
        iopdQtyInOutSetupWaste             = iopdQtyInOutSetupWaste / ipbf-ttOperation.numOutForOperation
        iopdQtyInOutRunWaste               = iopdQtyInOutRunWaste / ipbf-ttOperation.numOutForOperation
        ipbf-ttOperation.quantityIn        = fRoundUp(ipbf-ttOperation.quantityIn)
        iopdQtyInOutSetupWaste             = fRoundUp(iopdQtyInOutSetupWaste)
        iopdQtyInOutRunWaste               = fRoundUp(iopdQtyInOutRunWaste)
        .
    
    /*Recalc from standards off right now*/
    IF NOT ipbf-ttOperation.isLocked THEN 
    DO:
        FIND FIRST bf-mach NO-LOCK 
            WHERE bf-mach.company EQ ipbf-ttOperation.company
            AND bf-mach.m-code EQ ipbf-ttOperation.operationID
            NO-ERROR.
        //RUN pRecalcEstOperationFromStandardsSetupWaste(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER ipbf-ttOperation, BUFFER bf-mach).
       
    END.
    
    ASSIGN 
        ipbf-ttOperation.quantityInRunWaste        = (ipbf-ttOperation.quantityInNoWaste / 
                                                    (1 - (ipbf-ttOperation.quantityInRunWastePercent / 100))) 
                                                    - ipbf-ttOperation.quantityInNoWaste
        ipbf-ttOperation.quantityInRunWaste        = fRoundUp(ipbf-ttOperation.quantityInRunWaste)
        ipbf-ttOperation.quantityInAfterSetupWaste = ipbf-ttOperation.quantityInNoWaste + ipbf-ttOperation.quantityInRunWaste
        ipbf-ttOperation.quantityIn                = ipbf-ttOperation.quantityInAfterSetupWaste + ipbf-ttOperation.quantityInSetupWaste
        iopdQtyInOutRunWaste                       = iopdQtyInOutRunWaste + ipbf-ttOperation.quantityInRunWaste
        iopdQtyInOutSetupWaste                     = iopdQtyInOutSetupWaste + ipbf-ttOperation.quantityInSetupWaste
        ipbf-ttOperation.quantityIn                = fRoundUp(ipbf-ttOperation.quantityIn)
        iopdQtyInOut                               = ipbf-ttOperation.quantityIn
        .
    IF ipbf-ttOperation.isSpeedInLF THEN 
        ipbf-ttOperation.quantityInAfterSetupWasteLF = ipbf-ttOperation.quantityInAfterSetupWaste * ipbf-ttOperation.linealFeetPerFeed.
    
    //Apply feed types A and P after base in-out calculation performed.  These will only affect the run hrs
    IF fIsSetType(ipbf-ttOperation.estType) AND (ipbf-ttOperation.feedType EQ "A" OR  ipbf-ttOperation.feedType EQ "P") THEN 
    DO: 
        IF ipbf-ttOperation.feedType EQ "P" THEN 
            dPartCount = fGetPartCount(ipbf-ttOperation.company, ipbf-ttOperation.estimateNo).
        ELSE 
            dPartCount = 1.
        ASSIGN 
            ipbf-ttOperation.quantityInNoWaste         = ipbf-ttOperation.quantityMaster * dPartCount
            ipbf-ttOperation.quantityOut               = ipbf-ttOperation.quantityMaster
            ipbf-ttOperation.quantityInRunWaste        = (ipbf-ttOperation.quantityInNoWaste / 
                                                    (1 - (ipbf-ttOperation.quantityInRunWastePercent / 100))) 
                                                    - ipbf-ttOperation.quantityInNoWaste
            ipbf-ttOperation.quantityInRunWaste        = fRoundUp(ipbf-ttOperation.quantityInRunWaste)
            ipbf-ttOperation.quantityInAfterSetupWaste = ipbf-ttOperation.quantityInNoWaste + ipbf-ttOperation.quantityInRunWaste
            ipbf-ttOperation.quantityIn                = ipbf-ttOperation.quantityInAfterSetupWaste + ipbf-ttOperation.quantityInSetupWaste
            ipbf-ttOperation.quantityIn                = fRoundUp(ipbf-ttOperation.quantityIn)            
            .
    END.
    
END PROCEDURE.

PROCEDURE ProcessOperationChange:
    /*------------------------------------------------------------------------------
     Purpose:  Given an operationID (mach code) and job, confirm that this machine
     exists on job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob AS INTEGER NO-UNDO. 
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDepartmentID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAction AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    
    DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.
    
    IF fIsOperationFound(ipcCompany, 
        ipcOperationID,
        ipiJob,
        ipiFormNo,
        ipiBlankNo,
        ipiPass,
        ipcDepartmentID) THEN

        RETURN.  /*If a match is found, return with no action*/
                             
    /*Find machine from same department*/
    FIND FIRST bf-job-mch NO-LOCK
        WHERE bf-job-mch.company EQ ipcCompany
        AND bf-job-mch.job EQ ipiJob
        AND bf-job-mch.frm     EQ ipiFormNo
        AND bf-job-mch.blank-no EQ ipiBlankNo
        AND bf-job-mch.dept    EQ ipcDepartmentID
        AND bf-job-mch.pass    EQ ipiPass
        NO-ERROR.

    RUN pOperationChangeDetermineAction(BUFFER bf-job-mch, ipcOperationID, OUTPUT cAction).
    opcAction = cAction.
   
    CASE cAction:
        WHEN "Cancel" THEN 
            RETURN.
        WHEN "Add" THEN 
        RUN pOperationChangeAddMachine(BUFFER bf-job-mch, ipcOperationID).
        WHEN "Replace" THEN 
        RUN pOperationChangeReplaceMachine(BUFFER bf-job-mch, ipcOperationID).
        WHEN "AddDept" THEN 
        RUN pOperationChangeAddDepartment(ipcCompany, ipcOperationID, ipiJob, ipiFormNo, ipiBlankNo, ipiPass, ipcDepartmentID).
    END CASE.        
                          
END PROCEDURE.

PROCEDURE pSetAttributeForColors PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an estimate blank buffer and pass, set the 
        # of colors attribute.
     Notes: If Blank NO specific then return colors for that Blank otherwsie for whole Form
            If DEpartment is Press then count for specific Pass number Otherwise All
    ------------------------------------------------------------------------------*/
    
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT  PARAMETER ipcLocationID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOperationID  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDeptt        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPass         AS INTEGER NO-UNDO.

    DEFINE VARIABLE iColors   AS INTEGER.
    DEFINE VARIABLE cFeedType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.        
            
    DEFINE BUFFER bf-eb   FOR eb.
    DEFINE BUFFER bf-ef   FOR ef.
    DEFINE BUFFER bf-mach FOR mach.
    
    RUN pGetMachineBuffer(ipbf-eb.company, ipcLocationID, ipcOperationID, BUFFER bf-mach, OUTPUT cError, OUTPUT cMessage).
    
    IF AVAILABLE bf-mach THEN
        cFeedType = bf-mach.p-type. 
    
    /* For Printers; get colors for specific Blank and Specific Pass */
    IF ipcDeptt = "PR" THEN
        iColors = fGetColorsForBlankPass(BUFFER ipbf-eb, ipiPass).
    
    /* For a Blank Fed machine; get Colors for a specific BlankNo, Passes-All */
    ELSE IF cFeedType EQ "B" THEN
            iColors = fGetColorsForBlankPass(BUFFER ipbf-eb, 0).
        
    /* For Other cases; get Colors for a Form, Blanks-All, Passes-All */       
    ELSE
    DO:
        FOR FIRST bf-ef NO-LOCK
            WHERE bf-ef.company EQ ipbf-eb.company
            AND bf-ef.est-no EQ ipbf-eb.est-no
            AND bf-ef.form-no EQ ipbf-eb.form-no,
            EACH bf-eb NO-LOCK 
            WHERE bf-eb.company EQ bf-ef.company
            AND bf-eb.est-no EQ bf-ef.est-no
            AND bf-eb.form-no EQ bf-ef.form-no:
                
            iColors = iColors + fGetColorsForBlankPass(BUFFER bf-eb, 0).
        END.
    END.
        
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDNoOfColors, iColors).  //Maxco

END PROCEDURE.

PROCEDURE pSetAttributeFromStandard PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets an attribute value (Creates if doesn't exist)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiAttributeID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeValue AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-std-code FOR std-code.
    
    DEFINE VARIABLE cAttributeName AS CHARACTER NO-UNDO.
    
    FIND FIRST bf-std-code NO-LOCK 
        WHERE bf-std-code.code EQ STRING(ipiAttributeID,"99")
        NO-ERROR.
    IF AVAILABLE bf-std-code THEN 
        cAttributeName = bf-std-code.dscr.
    
    RUN pSetAttribute(ipiAttributeID, cAttributeName, ipcAttributeValue).
    
END PROCEDURE.

PROCEDURE pSetAttribute PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets an attribute value (Creates if doesn't exist)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiAttributeID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeValue AS CHARACTER NO-UNDO.
    
    FIND FIRST ttAttribute
        WHERE ttAttribute.attributeID EQ ipiAttributeID
        NO-ERROR.
    IF NOT AVAILABLE ttAttribute THEN 
    DO:
        CREATE ttAttribute.
        ASSIGN 
            ttAttribute.attributeID = ipiAttributeID.
    END.
    ASSIGN 
        ttAttribute.attributeName  = ipcAttributeName
        ttAttribute.attributeValue = ipcAttributeValue
        .

END PROCEDURE.


PROCEDURE pSetAttributesBlank PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a form, sets attributes that are form dependent
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    RUN pSetAttribute(giAttributeIDStyle, "Style", ipbf-eb.style).
    RUN pSetAttribute(giAttributeIDBoxDepth, "Box Depth", STRING(ipbf-eb.dep)).
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDBoxLen, STRING(ipbf-eb.len)).
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDBoxWid, STRING(ipbf-eb.wid)).
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDBlankLen, STRING(ipbf-eb.t-len)). //refactor dBlankLen
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDBlankWid, STRING(ipbf-eb.t-wid)). //refactor dBlankWid
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDGlueLapLen, STRING(ipbf-eb.lin-in)).
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDBlankSqFt, STRING(fGetBlankSqFtArea(BUFFER ipbf-eb))).  //eb.t-sqFt
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDRoutingNoOut, "0"). //none?
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDNoOnDieLen, STRING(ipbf-eb.num-wid)).
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDNoOnDieWid, STRING(ipbf-eb.num-len)).
    RUN pSetAttribute(giAttributeIDBlankSqIn, "Blank Sq In", STRING(ipbf-eb.t-sqin)).  //eb.t-sqin
    
END PROCEDURE.

PROCEDURE pSetAttributesEfNoBlank PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: If operation is for a sheet fed (blank = 0) process attributes for 
    total sheet
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE pSetAttributesForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a form, sets attributes that are form dependent
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    
    RUN pSetAttribute(giAttributeIDBoardItemID, "Board ItemID", ipbf-ef.board).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDCaliper, STRING(fGetOperationsCalThickness(BUFFER ipbf-ef))).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDWeightperMSF, STRING(ipbf-ef.weight)). 
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDRollWid, STRING(ipbf-ef.roll-wid)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDNShtWid, STRING(ipbf-ef.nsh-wid)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDNShtLen, STRING(ipbf-ef.nsh-len)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDFilmLen, STRING(ipbf-ef.leaf-l[1])).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDFilmWid, STRING(ipbf-ef.leaf-w[1])).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDGShtLen, STRING(ipbf-ef.gsh-len)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDGShtWid, STRING(ipbf-ef.gsh-wid)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDDieLIIn, STRING(ipbf-ef.die-in)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDSheetSqIn, STRING(ipbf-ef.nsh-len * ipbf-ef.nsh-wid / 144)). //v-ssqft
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDNoOfPlates, STRING(ipbf-ef.f-col + ipbf-ef.f-coat)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDNoOfCuts, STRING(ipbf-ef.n-cuts)). //v-cut
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDNoOfItems, STRING(ipbf-ef.blank-qty)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  giAttributeIDNoOutGShtLen, STRING(ipbf-ef.n-out-l)).
    

END PROCEDURE.

PROCEDURE pSetGlobalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets the NK1 setting global variables that are pertinent to th
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany,"CEOpRates","C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glOpRatesSeparate = lFound AND cReturn EQ "MR/Run Separate".
    
END PROCEDURE.



PROCEDURE GetRecalculatedRouting:
    /*------------------------------------------------------------------------------
    Purpose: given a estCostHeader, build all estCostOperations
    Notes:
   ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    
    DEFINE           BUFFER bf-ttOperation     FOR ttOperation.
    
    DEFINE VARIABLE dQtyFormsRequiredForBlanks    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyFormsRequiredForBlanksMax AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOut                     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutRunWaste             AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutSetupWaste           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyTarget                    AS DECIMAL NO-UNDO.
        
    RUN pGetEffectiveEstOpQuantity (ipbf-estCostHeader.company, ipbf-estCostHeader.estimateNo, ipbf-estCostHeader.quantityMaster, OUTPUT dQtyTarget).
    EMPTY TEMP-TABLE ttEstBlank.
    /*Process each est-op for the right quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-estCostHeader.company
        AND est-op.est-no EQ ipbf-estCostHeader.estimateNo
        AND est-op.s-num EQ ipbf-estCostForm.formNo
        AND est-op.line LT 500
        AND est-op.qty EQ dQtyTarget
        GROUP BY est-op.line DESCENDING:

    RUN pAddEstOperationFromEstOp(BUFFER est-op, BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER bf-ttOperation).                    
    IF AVAILABLE bf-ttOperation THEN 
    DO:
        /*REFACTOR to calculate quantities for combos*/        
        IF est-op.b-num NE 0 AND bf-ttOperation.feedType EQ "B" THEN
        DO:  /*Calculate for Combo*/
            FIND FIRST estCostBlank NO-LOCK 
                WHERE estCostBlank.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
                AND estCostBlank.estCostFormID EQ ipbf-estCostForm.estCostFormID
                AND estCostBlank.blankNo EQ est-op.b-num
                NO-ERROR.
            IF AVAILABLE estCostBlank THEN 
            DO:
                FIND FIRST ttEstBlank
                    WHERE ttEstBlank.estCostBlankID EQ estCostBlank.estCostBlankID
                    NO-ERROR.
                IF NOT AVAILABLE ttEstBlank THEN 
                DO:
                    CREATE ttEstBlank.
                    ASSIGN 
                        ttEstBlank.estCostBlankID = estCostBlank.estCostBlankID
                        ttEstBlank.estCostFormID  = estCostBlank.estCostFormID
                        ttEstBlank.iOut           = estCostBlank.numOut.
                END.
                IF NOT ttEstBlank.lOutputInitialized THEN 
                    ASSIGN 
                        ttEstBlank.dQtyInOut          = estCostBlank.quantityRequired
                        ttEstBlank.lOutputInitialized = YES
                        .
            END. /*estCostBlank Avail*/
            RUN pProcessOperation(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER bf-ttOperation, INPUT-OUTPUT ttEstBlank.dQtyInOut, 
                INPUT-OUTPUT ttEstBlank.dQtyInOutSetupWaste, INPUT-OUTPUT ttEstBlank.dQtyInOutRunWaste).
        END. /*BlankNo not 0*/
        ELSE 
        DO:                  
            IF bf-ttOperation.isBlankMaker THEN 
            DO:
                /*Find the most forms required to support each blank operations*/
                FOR EACH ttEstBlank NO-LOCK 
                    WHERE ttEstBlank.estCostFormID EQ ipbf-estCostForm.estCostFormID:
                    dQtyFormsRequiredForBlanks = fRoundUp(ttEstBlank.dQtyInOut / MAX(ttEstBlank.iOut,1)).
                    IF dQtyFormsRequiredForBlanksMax LT dQtyFormsRequiredForBlanks THEN 
                        ASSIGN 
                            dQtyFormsRequiredForBlanksMax = dQtyFormsRequiredForBlanks
                            dQtyInOutSetupWaste           = fRoundUp(ttEstBlank.dQtyInOutSetupWaste / MAX(ttEstBlank.iOut,1))
                            dQtyInOutRunWaste             = fRoundUp(ttEstBlank.dQtyInOutRunWaste / MAX(ttEstBlank.iOut,1))
                            .
                END.
                
                /*Convert the forms for the most wasteful blank into what is required out of the blank maker as a total for all blanks*/
                ASSIGN 
                    dQtyInOut           = dQtyFormsRequiredForBlanksMax * bf-ttOperation.numOutForOperation
                    dQtyInOutSetupWaste = dQtyInOutSetupWaste * bf-ttOperation.numOutForOperation
                    dQtyInOutRunWaste   = dQtyInOutRunWaste * bf-ttOperation.numOutForOperation.
            END.
            IF dQtyInOut EQ 0 THEN 
                dQtyInOut = ipbf-estCostForm.quantityFGOnFormYielded.
            RUN pProcessOperation(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER bf-ttOperation, INPUT-OUTPUT dQtyInOut, 
                INPUT-OUTPUT dQtyInOutSetupWaste, INPUT-OUTPUT dQtyInOutRunWaste).
                
        END.
        RUN pCalcEstOperation(BUFFER ipbf-estCostHeader, BUFFER bf-ttOperation, BUFFER ipbf-estCostForm).                    
    END.
                    
END. /*Each est-op*/
ASSIGN 
    ipbf-estCostForm.grossQtyRequiredSetupWaste = dQtyInOutSetupWaste
    ipbf-estCostForm.grossQtyRequiredRunWaste   = dQtyInOutRunWaste
    ipbf-estCostForm.grossQtyRequiredNoWaste    = dQtyInOut - (dQtyInOutSetupWaste + dQtyInOutRunWaste)
    ipbf-estCostForm.grossQtyRequiredTotal      = dQtyInOut
    .

END PROCEDURE.

PROCEDURE SetAttribute:
    /*------------------------------------------------------------------------------
     Purpose: Public Wrapper to Set Attribute
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiAttributeID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeValue AS CHARACTER NO-UNDO.
    
    RUN pSetAttribute(ipiAttributeID, ipcAttributeName, ipcAttributeValue).

END PROCEDURE.

PROCEDURE SetAttributes:
    /*------------------------------------------------------------------------------
     Purpose:  Sets Attributes for ttAttribute Table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttAttribute.
    
END PROCEDURE.

PROCEDURE SetAttributesForEstimate:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiForm AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlank AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID2 AS INTEGER NO-UNDO.

END PROCEDURE.

PROCEDURE SetAttributesFromJobMch:
    /*------------------------------------------------------------------------------
     Purpose: Given a rowid (for eb), build out the attributes required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMchCode AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipipass AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iForm  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBlank AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPass  AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-eb      FOR eb. 
    DEFINE BUFFER bf-ef      FOR ef.
    
    FIND FIRST bf-job-mch NO-LOCK
        WHERE ROWID(bf-job-mch) EQ ipriRowID
        NO-ERROR.
    IF NOT AVAILABLE bf-job-mch THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid job-mch rowid"
            .
        RETURN.
    END.
    ASSIGN 
        iForm  = bf-job-mch.frm
        iBlank = MAX(bf-job-mch.blank-no, 1)
        iPass  = MAX(bf-job-mch.pass, 1)
        .
    FIND FIRST bf-job NO-LOCK OF bf-job-mch NO-ERROR.
    IF NOT AVAILABLE bf-job THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid job for job-mch: " + STRING(bf-job-mch.job)
            .
        RETURN.
    END.
            
    FIND FIRST bf-eb NO-LOCK 
        WHERE bf-eb.company EQ bf-job.company
        AND bf-eb.est-no EQ bf-job.est-no
        AND bf-eb.form-no EQ iForm
        AND bf-eb.blank-no EQ iBlank
        NO-ERROR.
    
    IF AVAILABLE bf-eb THEN 
    DO:
        RUN pSetAttributesBlank(BUFFER bf-eb).
        RUN pSetAttributeForColors(BUFFER bf-eb, bf-job.loc, bf-job-mch.m-code, bf-job-mch.dept ,bf-job-mch.pass).  //Maxco
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDDieNumberUp, STRING(fGetDieNumberUp(BUFFER bf-eb,bf-job-mch.m-code))). //v-up  
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDEstQty, STRING(fGetOperationsQty(BUFFER bf-eb, bf-job-mch.m-code, bf-job.loc, iPass))).  //qty
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDEstSheets, STRING(fGetOperationsEstSheet(BUFFER bf-eb, bf-job-mch.m-code, iPass))). //(qty * v-yld / xeb.num-up / v-n-out)   not found
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDNoOutGShtWid, STRING(fGetOperationsGrsShtWid(BUFFER bf-eb, bf-job-mch.m-code, iPass))). //v-out
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDSetPartsperForm, STRING(fGetOperationsPartPerSet(BUFFER bf-eb,2,""))). //ld-parts[2]
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDInkCoverage, STRING(fGetOperationsInkCoverage(BUFFER bf-eb))). //ld-ink-frm
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDPartsperSet, STRING(fGetOperationsPartPerSet(BUFFER bf-eb,1,""))). //ld-parts[1]
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDQtySetLongs, STRING(fGetOperationsPartPerSet(BUFFER bf-eb,1,"long"))). //v-long-qty-set
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDQtySetShorts, STRING(fGetOperationsPartPerSet(BUFFER bf-eb,1,"short"))). //v-short-qty-set
        
        FIND FIRST bf-ef OF bf-eb NO-LOCK.
        IF AVAILABLE bf-ef THEN 
            RUN pSetAttributesForm(BUFFER bf-ef).
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Blank for estimate " + bf-job.est-no + " Form/Blank " + STRING(iForm) + "/" + STRING(iBlank)
            .
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fBuildMessageRETURNS CHARACTER PRIVATE
    ( ipcMessage AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  If message is not blank return it with append string first
     Notes:
    ------------------------------------------------------------------------------*/	
    
    IF ipcMessage NE "" THEN 
        RETURN CHR(13) + ipcMessage.
    		
END FUNCTION.


FUNCTION fGetAttributeName RETURNS CHARACTER PRIVATE
    (ipiAttributeID AS INTEGER):
        
    DEFINE VARIABLE cAttributeValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName           AS CHARACTER NO-UNDO.
    
    RUN pGetAttribute(ipiAttributeID, OUTPUT cAttributeValue, OUTPUT cName, OUTPUT lError, OUTPUT cMessage).
    RETURN cName.        
    
END FUNCTION.        

FUNCTION fGetAttributeValue RETURNS DECIMAL PRIVATE
    (ipiAttributeID AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Given an attribute ID, return the value in decimal
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cAttributeValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dAttributeValue AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName           AS CHARACTER NO-UNDO.
    
    RUN pGetAttribute(ipiAttributeID, OUTPUT cAttributeValue, OUTPUT cName, OUTPUT lError, OUTPUT cMessage).
    IF lError THEN 
        dAttributeValue = 0.
    ELSE 
        dAttributeValue = DECIMAL(cAttributeValue) NO-ERROR.
    
    RETURN dAttributeValue.
        		
END FUNCTION.

FUNCTION fGetColorsForBlankPass RETURNS INTEGER PRIVATE
    (BUFFER ipbf-eb FOR eb, ipiPass AS INTEGER):
    /*------------------------------------------------------------------------------
         Purpose: Given an estimate blank buffer and pass, set the 
            # of colors attribute.
         Notes: PassNo is optional Parameter
        ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE iIndex  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iColors AS INTEGER NO-UNDO.
    
    IF ipbf-eb.est-type GE 5 THEN
    DO:    
        DO iIndex = 1 TO 10:
            IF ipiPass NE 0 AND ipbf-eb.i-ps[iIndex] NE ipiPass THEN NEXT.
            iColors = iColors + fIsInk(ipbf-eb.company, ipbf-eb.i-code[iIndex]).   
        END.
    END.  
    ELSE 
    DO:  
        DO iIndex = 1 TO 17:
            IF ipiPass NE 0 AND ipbf-eb.i-ps2[iIndex] NE ipiPass THEN NEXT.
            iColors = iColors + fIsInk(ipbf-eb.company, ipbf-eb.i-code2[iIndex]).
        END.
    END.    

    RETURN iColors.
END FUNCTION.

FUNCTION fGetFeet RETURNS DECIMAL PRIVATE
    (ipdDim AS DECIMAL, ipcUOM AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Given a dimension and its uom, return the dimension in feet 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dDimInFeet AS DECIMAL NO-UNDO.
    
    IF ipcUOM NE "" THEN 
        dDimInFeet = DYNAMIC-FUNCTION("fConv_GetFeet", ipdDim, ipcUOM).
    ELSE 
        dDimInFeet = ipdDim / 12.  //assume inches
    
    RETURN dDimInFeet.		
END FUNCTION.

FUNCTION fHasDataCollected RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-job-mch FOR job-mch):
    /*------------------------------------------------------------------------------
    Purpose: Given job-mch buffer, determine if there is data collected for it
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lHasData AS LOGICAL NO-UNDO.
    
    lHasData = CAN-FIND(FIRST mch-act
        WHERE mch-act.company EQ ipbf-job-mch.company
        AND mch-act.job EQ ipbf-job-mch.job
        AND mch-act.m-code EQ ipbf-job-mch.m-code
        AND mch-act.frm EQ ipbf-job-mch.frm
        AND mch-act.blank-no EQ ipbf-job-mch.blank-no).

    RETURN lHasData.
		
END FUNCTION.

FUNCTION fIsAssemblyFeed RETURNS LOGICAL PRIVATE
    (ipcFeedType AS CHARACTER):
    /*------------------------------------------------------------------------------
    Purpose:  Given feed type, determine if assembly feed (P or A)
    Notes:
    ------------------------------------------------------------------------------*/

    RETURN ipcFeedType EQ "P" OR ipcFeedType EQ "A".
		
END FUNCTION.

FUNCTION fIsAssemblyPartFeed RETURNS LOGICAL PRIVATE
    ( ipcFeedType AS CHARACTER ):
    /*------------------------------------------------------------------------------
    Purpose:  Given feed type, determine if partition specific Feed
    Notes:
    ------------------------------------------------------------------------------*/	
    RETURN ipcFeedType EQ "P".
		
END FUNCTION.

FUNCTION fIsDepartment RETURNS LOGICAL PRIVATE
    (ipcDepartment AS CHARACTER, ipcDepartmentList AS CHARACTER EXTENT 4):
    /*------------------------------------------------------------------------------
     Purpose: determine if provided department is in department list
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsDepartment", ipcDepartment, ipcDepartmentList).
        
END FUNCTION.

FUNCTION fIsInk RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Given Item Buffer, determine if it is an ink
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE BUFFER bf-item FOR ITEM.
    DEFINE VARIABLE iIsInk AS INTEGER NO-UNDO.
    
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipcCompany
        AND bf-item.i-no EQ ipcItemID
        NO-ERROR.
    IF AVAIL bf-item AND DYNAMIC-FUNCTION("fEstimate_IsInk", bf-item.mat-type, bf-item.ink-type)
        THEN iIsInk = 1.   
     
    RETURN iIsInk.
        		
END FUNCTION.

FUNCTION fIsOperationFound RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER,
    ipcOperationID AS CHARACTER,
    ipiJob AS INTEGER, 
    ipiFormNo AS INTEGER,
    ipiBlankNo AS INTEGER,
    ipiPass AS INTEGER,
    ipcDepartmentID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Given a job and operation, determine return if found or not
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    lFound = CAN-FIND(
        FIRST job-mch 
        WHERE job-mch.company EQ ipcCompany
        AND job-mch.job  EQ ipiJob
        AND job-mch.m-code  EQ ipcOperationID
        AND job-mch.frm     EQ ipiFormNo
        AND job-mch.blank-no EQ ipiBlankNo
        AND job-mch.dept    EQ ipcDepartmentID
        AND job-mch.pass    EQ ipiPass).

    IF NOT lFound THEN
        /* search without using dept */
        lFound = CAN-FIND(
            FIRST job-mch
            WHERE job-mch.company EQ ipcCompany
            AND job-mch.job EQ ipiJob
            AND job-mch.m-code  EQ ipcOperationID
            AND job-mch.frm     EQ ipiFormNo
            AND job-mch.blank-no EQ ipiBlankNo
            AND job-mch.pass    EQ ipiPass
            ).
    
    RETURN lFound.


		
END FUNCTION.


FUNCTION fGetOperationsColor RETURNS INTEGER PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipiPass AS INTEGER):
    /*------------------------------------------------------------------------------
    Purpose: Given eb buffer, determine if there is data collected for it
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE iReturnPass AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER NO-UNDO.
    
    FIND FIRST est-op NO-LOCK
        WHERE est-op.company EQ ipbf-eb.company
        AND est-op.m-code EQ ipcMachine
        AND est-op.est-no EQ ipbf-eb.est-no
        AND est-op.s-num EQ ipbf-eb.form-no
        AND (est-op.b-num EQ ipbf-eb.blank-no OR est-op.b-num EQ 0 )
        AND est-op.op-pass EQ ipiPass 
        and est-op.line    LT 500 NO-ERROR.
    
    IF AVAIL est-op AND est-op.dept EQ "PR" THEN
    DO iCount = 1 TO 10:
        IF ipbf-eb.i-ps[iCount] NE est-op.op-pass THEN NEXT.
        FIND FIRST item NO-LOCK
            where (item.company EQ ipbf-eb.company) 
            AND item.i-no EQ ipbf-eb.i-code[iCount]
            AND INDEX("IV",item.mat-type) GT 0
            AND item.ink-type NE "A"
            NO-ERROR.
        IF AVAIL item THEN iReturnPass = iReturnPass + 1. /* iReturnPass now = # colors/coating this machine/this pass */
    END.        
    RETURN iReturnPass.
		
END FUNCTION.


FUNCTION fGetOperationsCalThickness RETURNS INTEGER PRIVATE
    (BUFFER ipbf-ef FOR ef):
    /*------------------------------------------------------------------------------
    Purpose: Returns caliper Thickness in Integer
    Notes: Converting the Caliper Thickness into Integer number multilpying by 1000, 
           Reason being Axis values are configured as integer than decimal. 
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE iReturnValue AS INTEGER NO-UNDO.
    
    IF AVAILABLE ipbf-ef AND ipbf-ef.cal NE 0 THEN
        iReturnValue = ipbf-ef.cal  * 1000.  
    
    
    RETURN iReturnValue.
		
END FUNCTION.

FUNCTION fGetOperationsQty RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipcLocationID AS CHARACTER,
    ipiPass AS INTEGER):
    /*------------------------------------------------------------------------------
    Purpose: 
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dReturnValue AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumUp       as INTEGER   NO-UNDO.
    DEFINE VARIABLE dNumOutCal   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cError       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQtyCalc     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpMRWaste   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpRunSpoil  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSpoilDeduct AS DECIMAL   NO-UNDO INITIAL 1.   
         
    DEFINE BUFFER bf-mach    for mach.
    
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ ipbf-eb.company
        AND ef.est-no EQ ipbf-eb.est-no
        AND ef.form-no EQ ipbf-eb.form-no NO-ERROR.
          
    run sys/inc/numup.p (ef.company, ef.est-no, ef.form-no, output iNumUp).     
    dNumOutCal = ef.n-out * ef.n-out-l .
         
    FIND FIRST est-op NO-LOCK
        WHERE est-op.company EQ ipbf-eb.company
        AND est-op.m-code EQ ipcMachine
        AND est-op.est-no EQ ipbf-eb.est-no
        AND est-op.s-num EQ ipbf-eb.form-no
        AND (est-op.b-num EQ ipbf-eb.blank-no OR est-op.b-num EQ 0 )
        AND est-op.op-pass EQ ipiPass 
        and est-op.line    lt 500 NO-ERROR.      
    IF AVAIL est-op THEN
        dReturnValue    = est-op.num-sh * (iNumUp * dNumOutCal). 
    
    /* Est-op record is not available that means its a new machine that is not on Estimate.
       Now, We have calculate the field on-fly */
    ELSE
    DO:
        /* Use Original Estimate Quantity*/
        dQtyCalc = ef.eqty.
               
        RUN pGetMachineBuffer(ipbf-eb.company, ipcLocationID, ipcMachine, BUFFER bf-mach, OUTPUT cError, OUTPUT cMessage).
        
        IF AVAILABLE bf-mach THEN 
        DO:           
            RUN pGetMRWaste(BUFFER bf-mach, OUTPUT dOpMRWaste, OUTPUT cError, OUTPUT cMessage).
            RUN pGetRunSpoil(BUFFER bf-mach, OUTPUT dOpRunSpoil, OUTPUT cError, OUTPUT cMessage).
            
            IF dOpRunSpoil NE 0 THEN
                dSpoilDeduct = (1 - (dOpRunSpoil / 100)).
            
            ASSIGN
                dQtyCalc = dQtyCalc / dSpoilDeduct
                dQtyCalc = dQtyCalc + dOpMRWaste.
        END.
        dReturnValue = dQtyCalc. 
    END.
    
    RETURN dReturnValue.
		
END FUNCTION.

FUNCTION fGetDieNumberUp RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER ):
    /*------------------------------------------------------------------------------
    Purpose: 
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dReturnValue AS DECIMAL NO-UNDO.         
    
    FIND FIRST mach NO-LOCK
        WHERE mach.company EQ ipbf-eb.company
        AND mach.m-code EQ ipcMachine NO-ERROR.
          
    IF AVAIL mach THEN
    DO:
        IF INDEX("AP",mach.p-type) GT 0 THEN 
        DO:
            ASSIGN
                dReturnValue = 1.
        END.
        ELSE 
        DO:
            IF mach.p-type EQ "B" THEN dReturnValue = ipbf-eb.num-up.
            ELSE
                RUN sys/inc/numup.p (ipbf-eb.company, ipbf-eb.est-no, ipbf-eb.form-no, OUTPUT dReturnValue).
        END.    
    END.  
    RETURN dReturnValue.
		
END FUNCTION.


FUNCTION fGetOperationsEstSheet RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipiPass AS INTEGER):
    /*------------------------------------------------------------------------------
    Purpose: 
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dReturnValue  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOperationQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iYldQty       as INTEGER NO-UNDO.
    DEFINE VARIABLE iNOut         AS INTEGER NO-UNDO.
    DEFINE VARIABLE cQtyValue     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cAttrName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    
    RUN pGetAttribute(giAttributeIDEstQty, OUTPUT cQtyValue, OUTPUT cAttrName, OUTPUT cError, OUTPUT cMessage). //Get colors attribute
    ASSIGN dOperationQty = DECIMAL(cQtyValue) NO-ERROR.
    
    FIND FIRST mach NO-LOCK
        WHERE mach.company EQ ipbf-eb.company
        AND mach.m-code EQ ipcMachine NO-ERROR.
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ ipbf-eb.company
        AND ef.est-no EQ ipbf-eb.est-no
        AND ef.form-no EQ ipbf-eb.form-no NO-ERROR.     
       
    IF AVAIL mach THEN
    DO:
        IF INDEX("AP",mach.p-type) GT 0 THEN 
        DO:
            ASSIGN           
                iNOut = 1.           
            IF mach.p-type EQ "A" THEN 
                iYldQty = 1.
            ELSE
                FOR EACH eb FIELDS(quantityPerSet)
                    WHERE eb.company EQ ipbf-eb.company
                    AND eb.est-no  EQ ipbf-eb.est-no
                    AND eb.form-no NE 0
                    NO-LOCK:
                    iYldQty = iYldQty +
                        (IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet) ELSE eb.quantityPerSet).
                END.
        END.
        ELSE 
        DO:
            RUN est/ef-#out.p (ROWID(ef), OUTPUT iNOut).
            IF ipbf-eb.est-type EQ 8 THEN
                iYldQty =  1.
            ELSE
                iYldQty = IF ipbf-eb.quantityPerSet GE 0 THEN ipbf-eb.quantityPerSet ELSE (-1 / ipbf-eb.quantityPerSet) .
                
        END.        
    END.
    
    dReturnValue = (dOperationQty * iYldQty / ipbf-eb.num-up / iNOut) .         
    
    RETURN dReturnValue.
		
END FUNCTION.


FUNCTION fGetOperationsGrsShtWid RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipiPass AS INTEGER):
    /*------------------------------------------------------------------------------
    Purpose: 
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dReturnValue AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iOut         AS INTEGER NO-UNDO.            
    
    FIND FIRST est-op NO-LOCK
        WHERE est-op.company EQ ipbf-eb.company
        AND est-op.m-code EQ ipcMachine
        AND est-op.est-no EQ ipbf-eb.est-no
        AND est-op.s-num EQ ipbf-eb.form-no
        AND (est-op.b-num EQ ipbf-eb.blank-no OR est-op.b-num EQ 0 )
        AND est-op.op-pass EQ ipiPass 
        and est-op.line    lt 500 NO-ERROR.
    
    IF AVAIL est-op AND est-op.n-out NE 0 THEN
        iOut = est-op.n-out.
    ELSE
    Do:
        FIND FIRST ef NO-LOCK
            WHERE ef.company EQ ipbf-eb.company
            AND ef.est-no EQ ipbf-eb.est-no
            AND ef.form-no EQ ipbf-eb.form-no NO-ERROR.
        
        IF avail ef THEN
            iOut = ef.n-out.
            
    END.          
        
    dReturnValue = iOut .         
    
    RETURN dReturnValue.
		
END FUNCTION.


FUNCTION fGetOperationsPartPerSet RETURNS INTEGER PRIVATE
    (BUFFER ipbf-eb FOR eb,     
    ipiPartPerSet AS INTEGER,
    ipcSetCount AS CHARACTER):
    /*------------------------------------------------------------------------------
    Purpose: 
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE iReturnValue AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPartPerForm AS INTEGER NO-UNDO.            
    DEFINE VARIABLE iPartPerSet  AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iYldQty      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLongCount   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iShortCount  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER NO-UNDO.
    
    IF ipbf-eb.est-type EQ 6 THEN
    DO:
        FOR EACH eb NO-LOCK
            WHERE eb.company EQ ipbf-eb.company
            AND eb.est-no  EQ ipbf-eb.est-no
            AND eb.form-no NE 0
            USE-INDEX est-qty:
            
            ASSIGN
                iYldQty     = IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE (-1 / eb.quantityPerSet)
                iPartPerSet = iPartPerSet + iYldQty 
                iCount      = iCount + 1.  
                
            IF iCount EQ 1 THEN
                iLongCount = iYldQty.
            ELSE IF iCount EQ 2 THEN
                    iShortCount = iYldQty.
                        
            IF eb.form-no EQ ipbf-eb.form-no THEN
                iPartPerForm = iPartPerForm + iYldQty.   
        END. /* FOR EACH eb NO-LOCK */
        
        
        IF ipcSetCount EQ "long" THEN
            iReturnValue = iLongCount.
        ELSE IF ipcSetCount EQ "short" THEN
            iReturnValue = iShortCount.
        ELSE
            iReturnValue = IF ipiPartPerSet EQ 2 THEN iPartPerForm ELSE iPartPerSet.
    END.
        
    RETURN iReturnValue.
		
END FUNCTION.


FUNCTION fGetOperationsInkCoverage RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb):
    /*------------------------------------------------------------------------------
    Purpose: 
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE iReturnValue AS INTEGER NO-UNDO.
    DEFINE VARIABLE dInkBlk      AS DECIMAL NO-UNDO.            
    DEFINE VARIABLE li           AS INTEGER NO-UNDO. 
        
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ ipbf-eb.company
        AND ef.est-no EQ ipbf-eb.est-no
        AND ef.form-no EQ ipbf-eb.form-no NO-ERROR.
         
    dInkBlk = 0.
    DO li = 1 TO EXTENT(ipbf-eb.i-code):
        IF ipbf-eb.i-code[li] NE "" AND ipbf-eb.i-%[li] NE 0 THEN 
        DO:
            FIND FIRST item
                WHERE item.company   EQ ipbf-eb.company
                AND item.i-no      EQ ipbf-eb.i-code[li]
                AND INDEX("IV",item.mat-type) GT 0
                AND item.ink-type  NE "A" 
                NO-LOCK NO-ERROR. 
            IF AVAIL item THEN
                dInkBlk = dInkBlk + ((ipbf-eb.t-sqin - ipbf-eb.t-win) *
                    ipbf-eb.num-up * (ipbf-eb.i-%[li] / 100)).
        END.
    
        IF dInkBlk GT (ipbf-eb.t-sqin - ipbf-eb.t-win) * ipbf-eb.num-up THEN 
        DO:
            dInkBlk = (ipbf-eb.t-sqin - ipbf-eb.t-win) * ipbf-eb.num-up.
            LEAVE.
        END.
    END.
    dInkBlk = dInkBlk / (ef.nsh-wid * ef.nsh-len) * 100.
    IF dInkBlk GT 100 THEN dInkBlk = 100.
    
    iReturnValue = dInkBlk.     
    
    RETURN iReturnValue.
		
END FUNCTION.

FUNCTION fGetNetSheetOut RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER, ipcEstimateID AS CHARACTER, ipiFormNo AS INTEGER, ipiPass AS INTEGER, ipiDefaultOut AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose:  Given an operation buffer, return the # out based on the 
     specific net sheet pass of the operation
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE BUFFER bf-ef-nsh FOR ef-nsh.
    DEFINE VARIABLE iOut AS INTEGER.
    
    
    FIND FIRST bf-ef-nsh NO-LOCK    
        WHERE bf-ef-nsh.company EQ ipcCompany
        AND bf-ef-nsh.est-no EQ ipcEstimateID
        AND bf-ef-nsh.form-no EQ ipiFormNo
        AND bf-ef-nsh.pass EQ ipiPass
        NO-ERROR.
    IF AVAILABLE bf-ef-nsh THEN 
        iOut = bf-ef-nsh.n-out-d * bf-ef-nsh.n-out-l * bf-ef-nsh.n-out-w.
    
    IF iOut LE 0 THEN 
        iOut = ipiDefaultOut.
    
    RETURN iOut.
        
END FUNCTION.

FUNCTION fGetPartCount RETURNS DECIMAL PRIVATE
    (ipcCompany AS CHARACTER, ipcEstimateID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Gets the part count for a set for partition feed type calculation
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE BUFFER bf-eb FOR eb.
    
    DEFINE VARIABLE dParts AS DECIMAL NO-UNDO.
    
    FOR EACH bf-eb NO-LOCK 
        WHERE bf-eb.company EQ ipcCompany
        AND bf-eb.est-no EQ ipcEstimateID
        AND bf-eb.form-no NE 0:
        
        dParts = dParts + DYNAMIC-FUNCTION("fEstimate_GetQuantityPerSet", BUFFER bf-eb).
    END.
    
    RETURN dParts.

       
END FUNCTION.

FUNCTION fGetJobMachRunQty RETURNS DECIMAL PRIVATE
    (ipcCompany AS CHARACTER, ipiJob AS INTEGER, ipiFormNo AS INTEGER):
    /*------------------------------------------------------------------------------
    Purpose: 
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dReturnValue  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyEach      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
           
    FOR EACH job-mat NO-LOCK
        WHERE job-mat.company EQ ipcCompany
        AND job-mat.job       EQ ipiJob
        AND job-mat.frm       EQ ipiFormNo,        
        FIRST ITEM FIELDS(s-dep s-len basis-w r-wid)
        WHERE item.company  EQ ipcCompany
        AND item.i-no     EQ job-mat.i-no
        AND item.mat-type EQ "B"
        NO-LOCK:
                              
        IF job-mat.qty-uom NE "EA" THEN
            RUN Conv_QuantityFromUOMtoUOM( INPUT ipcCompany,
                INPUT job-mat.i-no, 
                INPUT "RM",
                INPUT job-mat.qty,
                INPUT job-mat.qty-uom,
                INPUT "EA",
                INPUT item.basis-w,
                INPUT IF item.r-wid EQ 0 THEN item.s-len ELSE 12,
                INPUT IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid, 
                INPUT item.s-dep,
                INPUT 0, 
                OUTPUT dQtyEach,
                OUTPUT lError,
                OUTPUT cErrorMessage).
                    
        dReturnValue = dReturnValue + ( IF job-mat.qty-uom EQ "EA" THEN job-mat.qty ELSE dQtyEach) .
    END.
    
    RETURN dReturnValue.
		
END FUNCTION.

FUNCTION fIsSetType RETURNS LOGICAL PRIVATE
    (ipcType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns if estimate type is a set
     Notes:
    ------------------------------------------------------------------------------*/	

    RETURN DYNAMIC-FUNCTION("fEstimate_IsSetType", ipcType).
		
END FUNCTION.

FUNCTION fRoundUp RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL):
    /*------------------------------------------------------------------------------
     Purpose: Given a value, rounds up to next integer
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("sfCommon_RoundUp", ipdValue).
    
END FUNCTION.


FUNCTION fGetBlankSqFtArea RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb):
    /*------------------------------------------------------------------------------
     Purpose: It returns the Blank Sq.Ft. of a Blank record
     Notes: It takes Blank's t-sqin field and converts that into SqFt.
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE dReturnValue AS DECIMAL NO-UNDO.
    
    IF AVAILABLE ipbf-eb and ipbf-eb.t-sqin NE 0 THEN
        dReturnValue = DYNAMIC-FUNCTION("fConv_GetAreaSqFeet", ipbf-eb.t-sqin,"SQIN").
        
    IF dReturnValue = ? THEN
        dReturnValue = 0.
       
    RETURN dReturnValue.
    
END FUNCTION.
