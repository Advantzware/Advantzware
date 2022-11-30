
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
{est\ttMachineRoutings.i}
{est\OperationProcsTT.i}
{system/ttTag.i &Table-Name=ttOpsTag}


/* Define Globals - Machine Attributes */

DEFINE VARIABLE giAttributeIDNoOfColors       AS INTEGER NO-UNDO INITIAL 1.    //# of Colors
DEFINE VARIABLE giAttributeIDBoxLen           AS INTEGER NO-UNDO INITIAL 2.    //Box Length
DEFINE VARIABLE giAttributeIDBoxWid           AS INTEGER NO-UNDO INITIAL 3.    //Box Width
DEFINE VARIABLE giAttributeIDBlankLen         AS INTEGER NO-UNDO INITIAL 4.    //Blank Length
DEFINE VARIABLE giAttributeIDBlankWid         AS INTEGER NO-UNDO INITIAL 5.    //Blank Width
DEFINE VARIABLE giAttributeIDGlueLapLen       AS INTEGER NO-UNDO INITIAL 6.    //Glue Lap Length
DEFINE VARIABLE giAttributeIDBlankSFSI        AS INTEGER NO-UNDO INITIAL 7.    //Blank Sq In/Sq Ft
DEFINE VARIABLE giAttributeIDCaliper          AS INTEGER NO-UNDO INITIAL 8.    //Caliper Thickness (integer)
DEFINE VARIABLE giAttributeIDWeightperMSF     AS INTEGER NO-UNDO INITIAL 9.    //Weight per MSF
DEFINE VARIABLE giAttributeIDRollWid          AS INTEGER NO-UNDO INITIAL 10.    //Roll Width
DEFINE VARIABLE giAttributeIDNShtWid          AS INTEGER NO-UNDO INITIAL 11.    //Net Sheet Width
DEFINE VARIABLE giAttributeIDNShtLen          AS INTEGER NO-UNDO INITIAL 12.    //Net Sheet Length
DEFINE VARIABLE giAttributeIDDieNumberUp      AS INTEGER NO-UNDO INITIAL 13.    //Die Number Up
DEFINE VARIABLE giAttributeIDFilmLen          AS INTEGER NO-UNDO INITIAL 14.    //Film Length
DEFINE VARIABLE giAttributeIDFilmWid          AS INTEGER NO-UNDO INITIAL 15.    //Film Width
DEFINE VARIABLE giAttributeIDGShtLen          AS INTEGER NO-UNDO INITIAL 16.    //Gross Sht Length
DEFINE VARIABLE giAttributeIDGShtWid          AS INTEGER NO-UNDO INITIAL 17.    //Gross Sht Width
DEFINE VARIABLE giAttributeIDEstQtyPerFG      AS INTEGER NO-UNDO INITIAL 18.    //Estimate Qty - Its Estimate qty in terms of Finished Goods 
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
DEFINE VARIABLE giAttributeIDEstQtyPerFeed    AS INTEGER NO-UNDO INITIAL 36.    //Estimate Qty - Its Estimate qty in terms of Feeds 
DEFINE VARIABLE giAttributeIDUnitizingFormula AS INTEGER NO-UNDO INITIAL 98.    //Unitizing Formula
DEFINE VARIABLE giAttributeIDDieHoursFormula  AS INTEGER NO-UNDO INITIAL 99.    //Die Hours Formula
DEFINE VARIABLE giAttributeIDStyle            AS INTEGER NO-UNDO INITIAL 101.   //Style
DEFINE VARIABLE giAttributeIDBoardItemID      AS INTEGER NO-UNDO INITIAL 102.   //Board ItemID
DEFINE VARIABLE giAttributeIDBoxDepth         AS INTEGER NO-UNDO INITIAL 104.   //Box Depth
DEFINE VARIABLE giAttributeIDCaliperDecimal   AS INTEGER NO-UNDO INITIAL 105.   //Caliper as decimal

DEFINE VARIABLE gcAttributeNameEstQtyPerFeed  AS CHARACTER NO-UNDO INITIAL "Estimate Quantity (Feeds)". 

DEFINE VARIABLE gcDeptsForPrinters                   AS CHARACTER NO-UNDO INITIAL "PR".
DEFINE VARIABLE gcDeptsForGluers                     AS CHARACTER NO-UNDO INITIAL "GL,QS".
DEFINE VARIABLE gcDeptsForLeafers                    AS CHARACTER NO-UNDO INITIAL "WN,WS,FB,FS".
DEFINE VARIABLE gcDeptsForSheeters                   AS CHARACTER NO-UNDO INITIAL "RC,RS,CR,GU".
DEFINE VARIABLE gcDeptsForCoaters                    AS CHARACTER NO-UNDO INITIAL "PR,CT".
DEFINE VARIABLE gcIndustryFolding                    AS CHARACTER NO-UNDO INITIAL "Folding".
DEFINE VARIABLE gcIndustryCorrugated                 AS CHARACTER NO-UNDO INITIAL "Corrugated".

DEFINE VARIABLE glOpRatesSeparate                    AS LOGICAL   NO-UNDO INITIAL YES.    /*CEOpRates - log val*/
DEFINE VARIABLE glApplyOperationMinimumCharge        AS LOGICAL   NO-UNDO. /*CEPRICE Logical*/
DEFINE VARIABLE glApplyOperationMinimumChargeRunOnly AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcBlankMakerOutput                   AS CHARACTER NO-UNDO INITIAL "B".
DEFINE VARIABLE gcSheetMakerOutput                   AS CHARACTER NO-UNDO INITIAL "S".
DEFINE VARIABLE gcDeptsForWidthSheeters              AS CHARACTER NO-UNDO INITIAL "GU".
 
DEFINE VARIABLE glTagDisabled AS LOGICAL NO-UNDO.
DEFINE VARIABLE giTotalOut    AS INTEGER NO-UNDO INIT 1.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fGetCumulNumOutForOperation RETURNS INTEGER PRIVATE
    (INPUT ipiOut AS INTEGER  ) FORWARD.

FUNCTION fGetAttributeName RETURNS CHARACTER PRIVATE
    (ipiAttributeID AS INTEGER) FORWARD.

FUNCTION fGetAttributeValue RETURNS DECIMAL PRIVATE
    (ipiAttributeID AS INTEGER) FORWARD.

FUNCTION fGetColorsForBlankPass RETURNS INTEGER PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipiPass AS INTEGER) FORWARD.

FUNCTION fGetEstIndustry RETURNS CHARACTER PRIVATE
	(INPUT ipcEstimateNo AS CHARACTER) FORWARD.

FUNCTION fGetFeet RETURNS DECIMAL PRIVATE
    (ipdDim AS DECIMAL,
    ipcUOM AS CHARACTER) FORWARD.

FUNCTION fHasDataCollected RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-job-mch FOR job-mch) FORWARD.

FUNCTION fIsAssemblyFeed RETURNS LOGICAL PRIVATE
    (ipcFeedType AS CHARACTER) FORWARD.

FUNCTION fIsAssemblyPartFeed RETURNS LOGICAL PRIVATE
    (ipcFeedType AS CHARACTER) FORWARD.

FUNCTION fIsCRDeptNeeded RETURNS LOGICAL PRIVATE
	(BUFFER ipbf-ef FOR ef) FORWARD.

FUNCTION fIsDepartment RETURNS LOGICAL PRIVATE
    (ipcDepartment AS CHARACTER,
    ipcDepartmentList AS CHARACTER EXTENT 4) FORWARD.

FUNCTION fIsInk RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER,
    ipcItemID AS CHARACTER) FORWARD.

FUNCTION fIsItemPurchasedFG RETURNS LOGICAL PRIVATE
	(BUFFER ipbf-eb FOR eb) FORWARD.

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
    ipcLocationID AS CHARACTER,
    ipcOperationID AS CHARACTER,
    ipcDeptt AS CHARACTER,
    ipiPass AS INTEGER) FORWARD.
         
FUNCTION fGetOperationsCalThickness RETURNS INTEGER PRIVATE
    (BUFFER ipbf-ef FOR ef) FORWARD.

FUNCTION fGetOriginalQtyfromEst RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    BUFFER ipbf-job FOR job) FORWARD.
        
FUNCTION fGetOperationsQty RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipcLocationID AS CHARACTER,
    ipiPass AS INTEGER, OUTPUT opiCumNumOut AS INTEGER) FORWARD.   
     
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

FUNCTION fGetRequiredQtyUsingEstOp RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb, INPUT ipdQty AS DECIMAL, INPUT ipcEstType AS CHARACTER, INPUT ipcQtyType AS CHARACTER) FORWARD.

FUNCTION fGetRequiredQtyUsingJob RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb, BUFFER ipbf-job FOR job, INPUT ipcEstType AS CHARACTER, INPUT ipcQtyType AS CHARACTER) FORWARD.

FUNCTION fGetJobMachRunQty RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb, ipcMachCode AS CHARACTER, ipiPass AS INTEGER,
     ipdRunQty AS DECIMAL) FORWARD.      

FUNCTION fIsSetType RETURNS LOGICAL PRIVATE
    (ipcType AS CHARACTER) FORWARD.

FUNCTION fRoundUp RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL) FORWARD.

FUNCTION fGetBlankSqFTorSqINArea RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb) FORWARD.
    
FUNCTION fGetQuantityPerSet RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb) FORWARD.
    
FUNCTION fIsComboType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER) FORWARD.
     
FUNCTION fGetEstimateType RETURNS CHARACTER PRIVATE
    (ipiEstType AS INTEGER, ipcEstimateTypeID AS CHARACTER) FORWARD.

FUNCTION fValidMachineLimits RETURNS LOGICAL PRIVATE
	(BUFFER ipbf-eb FOR eb, BUFFER ipbf-mach FOR mach, INPUT ipdQty AS DECIMAL, INPUT ipcDept AS CHARACTER) FORWARD.
	
FUNCTION fVerifyLimitsForPrinter RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-mach FOR mach, ipcDept AS CHARACTER) FORWARD.	


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */
PROCEDURE Operations_GetNumout:
    
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo          AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlanko          AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOperationId     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdEstQty          AS DECIMAL NO-UNDO. 
    DEFINE INPUT  PARAMETER ipiSeq             AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiNumOut          AS INTEGER NO-UNDO.
   
    DEFINE VARIABLE iNumOut     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNumOn AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cDept       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOutputType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFormNumOut AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-est    FOR est.
    DEFINE BUFFER bf-ef     FOR ef.
    DEFINE BUFFER bf-eb     FOR eb.
    DEFINE BUFFER bf-est-op FOR est-op.
    DEFINE BUFFER bf-mach   FOR mach.
    
    
    FIND FIRST bf-est NO-LOCK
        WHERE bf-est.company EQ ipcCompany
        AND bf-est.est-no  EQ ipcEstimateNo NO-ERROR. 
    
    IF NOT AVAILABLE bf-est THEN
        RETURN.
        
    FIND FIRST bf-ef NO-LOCK
        WHERE bf-ef.company EQ bf-est.company
        AND bf-ef.est-no  EQ bf-est.est-no
        AND bf-ef.form-no EQ MAX(ipiFormNo,1) NO-ERROR.
    
    IF NOT AVAILABLE bf-ef THEN
        RETURN.
    
    FIND FIRST bf-mach NO-LOCK
        WHERE bf-mach.company = ipcCompany 
        AND bf-mach.m-code = ipcOperationId NO-ERROR.
        
    IF NOT AVAILABLE bf-mach THEN
        RETURN.
    
    cDept = bf-mach.dept[1].
    
    RUN Operations_GetOutputType(INPUT bf-ef.company, 
                                            INPUT bf-ef.est-no, 
                                            INPUT bf-ef.form-no, 
                                            INPUT ipcOperationId,
                                            INPUT ipdEstQty,
                                            INPUT ipiseq,
                                            OUTPUT cOutputType).
     
    FOR EACH bf-eb NO-LOCK
        WHERE bf-eb.company EQ bf-est.company
        AND bf-eb.est-no    EQ bf-est.est-no
        AND bf-eb.form-no   EQ ipiFormNo:
               
        ASSIGN
            iBlankNumOn = MAX(bf-eb.num-wid, 1) * MAX(bf-eb.num-len, 1) * MAX(bf-eb.num-dep, 1) NO-ERROR.
            
        IF iBlankNumOn NE 0 THEN
            iFormNumOut = iFormNumOut + iBlankNumOn.
    END.
    
    IF cOutputType = gcBlankMakerOutput THEN
        iNumOut = iFormNumOut.
        
    ELSE IF cOutputType = gcSheetMakerOutput THEN
    DO:
        IF cDept EQ gcDeptsForWidthSheeters THEN
            iNumOut = bf-ef.n-out-l.
            
        ELSE IF cDept = "RC" OR cDept = "CR" THEN 
            iNumOut = bf-ef.n-out.
        
    END.   
    ELSE 
        iNumOut = 1.

    ASSIGN opiNumOut = iNumOut.
    
END PROCEDURE.

PROCEDURE Operations_GetOutputType:
    /*------------------------------------------------------------------------------
     Purpose: Check Output type for Operations
     Notes:
    ------------------------------------------------------------------------------*/ 
    
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcEstimateNo   AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipiFormNo       AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOperationId  AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipdQty          AS DECIMAL NO-UNDO. 
    DEFINE INPUT  PARAMETER ipiLine         AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcOutputType   AS CHARACTER NO-UNDO.
    
       
    DEFINE VARIABLE cOutputType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lLastMachine AS LOGICAL   NO-UNDO.
    
        
    DEFINE BUFFER bf-mach        FOR mach.
    DEFINE BUFFER bfCurrent-mach FOR mach.
    DEFINE BUFFER bf-est-op      FOR est-op.
    
    FIND FIRST bfCurrent-mach NO-LOCK
        WHERE bfCurrent-mach.company = ipcCompany 
        AND bfCurrent-mach.m-code = ipcOperationId NO-ERROR.
        
    IF NOT AVAILABLE bfCurrent-mach THEN
        RETURN "".
         
    IF CAN-DO("R,S,A,P", bfCurrent-mach.p-type) THEN 
    DO:
        /* Verify it's Last machine before a blank fed. Check the Subsequent machine is blank fed */
        FOR EACH bf-est-op NO-LOCK 
            WHERE bf-est-op.company EQ ipcCompany
            AND bf-est-op.est-no    EQ ipcEstimateNo
            AND bf-est-op.s-num     EQ ipiFormNo
            AND bf-est-op.qty       EQ ipdQty
            AND bf-est-op.line      GT ipiLine
            AND bf-est-op.line      LT 500,
            FIRST bf-mach NO-LOCK 
            WHERE bf-mach.company EQ bf-est-op.company
            AND bf-mach.m-code EQ bf-est-op.m-code 
            BY bf-est-op.line:
            
            IF bf-mach.p-type EQ "B" THEN  
                ASSIGN 
                    cOutputType   = gcBlankMakerOutput
                    .
            LEAVE.
        END.
        
        /* Check if this Est-OP is the Last Machine in the Routings*/  
        IF NOT AVAILABLE bf-est-op THEN 
            ASSIGN 
                lLastMachine = YES.
                
    END. /* IF CAN-DO("R,S,A,P", bf-mach.p-type) THEN */
        
    IF cOutputType EQ "" AND CAN-DO(gcDeptsForSheeters, bfCurrent-mach.dept[1]) THEN 
        ASSIGN
            cOutputType = gcSheetMakerOutput.
    
    IF cOutputType EQ "" AND lLastMachine = YES THEN 
        ASSIGN
            cOutputType = gcBlankMakerOutput.
            
            
    opcOutputType = cOutputType.
        
END PROCEDURE.


PROCEDURE Operations_BuildEstimateRouting:
/*------------------------------------------------------------------------------
    Purpose:  Given an Estimate No and Form No, calculate the routings
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo          AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdEstQty          AS DECIMAL NO-UNDO.
    
    RUN Operations_BuildEstimateRoutingTT (ipcCompany, ipcEstimateNo, ipiFormNo, ipdEstQty).
    RUN pAddEstOPFromRouting (ipcCompany, ipcEstimateNo, ipdEstQty).
    
END.

PROCEDURE Operations_BuildEstimateRoutingTT:
/*------------------------------------------------------------------------------
    Purpose:  procedure to calculate the machine routings
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo          AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdEstQty          AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lValidFarmOut    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iRouteX          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRouteY          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPageX           AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPageY           AS INTEGER NO-UNDO.
    DEFINE VARIABLE iIndex           AS INTEGER NO-UNDO.
    DEFINE VARIABLE dXAttributeValue AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dYAttributeValue AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMSF             AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lRoutingMatrixApplied AS LOGICAL NO-UNDO.
                
    
    DEFINE BUFFER bf-est         FOR est.
    DEFINE BUFFER bf-ef          FOR ef.
    DEFINE BUFFER bf-eb          FOR eb.
    DEFINE BUFFER bf-Style       FOR style.
    DEFINE BUFFER bf-routing-mtx FOR routing-mtx.
    DEFINE BUFFER bf-routing     FOR routing.
    
    
    FIND FIRST bf-est NO-LOCK
        WHERE bf-est.company = ipcCompany
          AND bf-est.est-no = ipcEstimateNo NO-ERROR.
    
    IF NOT AVAILABLE bf-est THEN
        RETURN.
        
    RUN pGetInkTT (ipcCompany, ipcEstimateNo, bf-est.est-type).

    FOR EACH bf-ef NO-LOCK
        WHERE bf-ef.company  EQ ipcCompany
        AND bf-ef.est-no   EQ ipcEstimateNo
        AND (bf-ef.form-no EQ ipiFormNo OR ipiFormNo EQ 0):
        
        FOR FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ bf-ef.company
            AND bf-eb.est-no  EQ bf-ef.est-no
            AND bf-eb.form-no EQ bf-ef.form-no:
                 
            /* Check and Add Farm Out Machines for Purchased FG */
            RUN pProcessFODept (BUFFER bf-ef, BUFFER bf-eb, ipdEstQty, OUTPUT lValidFarmOut).
            
            /* If Purchased good and Valid Farm out machines added then skip further deptt */
            IF lValidFarmOut THEN
                NEXT.
                
            /* Other Manufactured FG */
            ELSE
            DO:
                RUN pGetStyleBuffer(bf-eb.company, bf-eb.style, BUFFER bf-Style).
                
                dMSF = ((ipdEstQty * bf-eb.t-len * bf-eb.t-wid) / 144) / 1000.
                
                
                RUN pGetRoutingMatrix(bf-ef.company, bf-ef.loc, bf-eb.style, dMSF, BUFFER bf-ef, BUFFER bf-eb, BUFFER bf-routing-mtx).
               
                IF AVAILABLE bf-routing-mtx THEN
                DO:
                    lRoutingMatrixApplied = YES.
                    
                    IF bf-routing-mtx.dim-type EQ "BLANK"  THEN
                        ASSIGN
                            dXAttributeValue = bf-eb.t-len
                            dYAttributeValue = bf-eb.t-wid.
                    ELSE
                        ASSIGN
                            dXAttributeValue = bf-ef.nsh-len
                            dYAttributeValue = bf-ef.nsh-len.
                   
                    
                    RUN pGetCoordinates(dXAttributeValue, "RX", OUTPUT iRouteX, OUTPUT iPageX).
                    RUN pGetCoordinates(dYAttributeValue, "RY", OUTPUT iRouteY, OUTPUT iPageY).
                    
                    iIndex = ((iRouteY - 1) * 10) + iRouteX.
                     
                    FIND FIRST bf-routing NO-LOCK
                        WHERE bf-routing.company EQ bf-routing-mtx.company
                          AND bf-routing.loc     EQ bf-routing-mtx.loc
                          AND bf-routing.r-code  EQ bf-routing-mtx.r-code[iIndex] NO-ERROR.
                          
                    IF AVAILABLE bf-routing THEN
                       RUN pProcessDeptFromRouting (BUFFER bf-ef, BUFFER bf-eb, BUFFER bf-routing, ipdEstQty).
                        
                END.
                ELSE
                DO:
                    RUN pProcessCRDept (BUFFER bf-ef, BUFFER bf-eb, ipdEstQty).
                    RUN pProcessRCDept (BUFFER bf-ef, BUFFER bf-eb, ipdEstQty).
                    RUN pProcessInk (BUFFER bf-ef, BUFFER bf-eb, ipdEstQty).
                    RUN pProcessGUDept (BUFFER bf-ef, BUFFER bf-eb, ipdEstQty).
                    RUN pProcessLMDept (BUFFER bf-ef, BUFFER bf-eb, ipdEstQty).
                    RUN pProcessDCDept (BUFFER bf-ef, BUFFER bf-eb, ipdEstQty).
                
                    RUN pProcessGLDept (BUFFER bf-ef, BUFFER bf-eb, ipdEstQty).
                END.
                
            END. /* ELSE Manufactured FG  */
           
        END.  /* FOR FIRST bf-eb NO-LOCK */
        
    END. /* FOR EACH bf-ef NO-LOCK */

    IF NOT lRoutingMatrixApplied THEN
        RUN pRemoveRedundantDept(ipcCompany, ipcEstimateNo). 

END PROCEDURE.

PROCEDURE pAddEstOPFromRouting PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Save EstOP records from calculated Routings
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdEstQty          AS DECIMAL NO-UNDO.
    
    
    DEFINE VARIABLE iSeq       AS INTEGER NO-UNDO INIT 1.
    DEFINE VARIABLE riRowid    AS ROWID   NO-UNDO.
    DEFINE VARIABLE iNumSheets AS INTEGER NO-UNDO.
    DEFINE VARIABLE dMRWaste   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMRHrs     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSpeed     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSpoilPrct AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-est-op FOR est-op.
    DEFINE BUFFER bf-mach   FOR mach.
    DEFINE BUFFER bf-est    FOR est.
    DEFINE BUFFER bf-ef     FOR ef.
    DEFINE BUFFER bf-eb     FOR eb.
    DEFINE BUFFER bfExcl-est-op FOR est-op.
    
    
    FIND FIRST bf-est NO-LOCK
        WHERE bf-est.company = ipcCompany 
          AND bf-est.est-no  = ipcEstimateNo NO-ERROR.
          
    IF NOT AVAILABLE bf-est THEN
        RETURN.
    
    FOR EACH ttRouting
        WHERE ttRouting.Company = ipcCompany
          AND ttRouting.EstimateNo = ipcEstimateNo:
    
        FIND FIRST bf-mach NO-LOCK
            WHERE bf-mach.company  EQ ttRouting.Company
            AND bf-mach.m-code   EQ ttRouting.OperationId  NO-ERROR.
            
        IF NOT AVAILABLE bf-mach THEN
            NEXT.
            
        FIND FIRST bf-ef NO-LOCK
            WHERE bf-ef.company = bf-est.company
              AND bf-ef.est-no  = bf-est.est-no
              AND bf-ef.form-no = ttRouting.FormId NO-ERROR.
        
        IF NOT AVAILABLE bf-ef THEN
            NEXT.
        
        DO TRANSACTION:
            
            CREATE bf-est-op.
            ASSIGN
                bf-est-op.company    = bf-est.company
                bf-est-op.e-num      = bf-est.e-num
                bf-est-op.est-no     = bf-est.est-no
                bf-est-op.line       = iSeq
                bf-est-op.qty        = (IF bf-est.est-type EQ 8 THEN 0 ELSE ipdEstQty)
                bf-est-op.s-num      = ttRouting.FormId
                bf-est-op.b-num      = (IF bf-est.est-type EQ 5 THEN 1 ELSE ttRouting.BlankId)
                bf-est-op.op-pass    = (IF ttRouting.Pass NE 0 THEN ttRouting.Pass ELSE 1)
                bf-est-op.op-sb      = (IF bf-mach.p-type EQ "B" THEN NO ELSE YES)
                bf-est-op.m-code     = bf-mach.m-code
                bf-est-op.m-dscr     = bf-mach.m-dscr
                bf-est-op.dept       = ttRouting.departmentID
                bf-est-op.d-seq      = bf-mach.d-seq
                bf-est-op.n-out      = bf-ef.n-out
                bf-est-op.op-spoil   = bf-mach.run-spoil
                bf-est-op.op-crew[1] = bf-mach.mr-crusiz
                bf-est-op.op-crew[2] = bf-mach.run-crusiz
                iSeq                 = iSeq + 1
                riRowid              = ROWID(bf-est-op)
                .

            
            RUN est/getcrusz.p (ROWID(bf-mach), ROWID(bf-eb), bf-est-op.dept, "M R", INPUT-OUTPUT bf-est-op.op-crew[1]).
            RUN est/getcrusz.p (ROWID(bf-mach), ROWID(bf-eb), bf-est-op.dept, "RUN", INPUT-OUTPUT bf-est-op.op-crew[2]).
            
            ASSIGN
                bf-est-op.op-rate[1] = (bf-mach.lab-rate[bf-mach.lab-drate] * bf-est-op.op-crew[1]) + bf-mach.mr-varoh  + bf-mach.mr-fixoh
                bf-est-op.op-rate[2] = (bf-mach.lab-rate[bf-mach.lab-drate] * bf-est-op.op-crew[2]) + bf-mach.run-varoh + bf-mach.run-fixoh.
  
  
            RELEASE bf-est-op.
         
        END.
              
        RUN pGetOperationStandardsForEstOp(riRowid, 0, OUTPUT dSpeed, OUTPUT dMRHrs, OUTPUT dSpoilPrct, OUTPUT dMRWaste, OUTPUT iNumSheets).
         
        DO TRANSACTION:
            FIND FIRST bfExcl-est-op EXCLUSIVE-LOCK
                WHERE ROWID(bfExcl-est-op) = riRowid NO-ERROR.
            
            IF AVAILABLE bfExcl-est-op THEN
                ASSIGN 
                    bfExcl-est-op.op-speed = dSpeed
                    bfExcl-est-op.op-mr    = dMRHrs
                    bfExcl-est-op.op-spoil = dSpoilPrct
                    bfExcl-est-op.op-waste = dMRWaste
                    bfExcl-est-op.num-sh   = iNumSheets
                    .
        END. /* DO TRANSACTION */
        
         
    END. /* FOR EACH ttRouting */

END PROCEDURE.

PROCEDURE pAddMachine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Adds the machine in routing of the Estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb      FOR eb.
    DEFINE PARAMETER BUFFER ipbf-mach    FOR mach.
    DEFINE INPUT  PARAMETER ipcDept      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPass      AS INTEGER NO-UNDO.
    
    FIND FIRST ttRouting
        WHERE ttRouting.Company     = ipbf-eb.company
          AND ttRouting.EstimateNo  = ipbf-eb.est-no 
          AND ttRouting.FormId      = ipbf-eb.form-no
          AND ttRouting.BlankId     = ipbf-eb.blank-no
          AND ttRouting.Pass        = ipiPass
          AND ttRouting.OperationId = ipbf-mach.m-code NO-ERROR.
          
    IF NOT AVAILABLE ttRouting THEN 
    DO:
        CREATE ttRouting.
        ASSIGN
            ttRouting.Company      = ipbf-eb.company
            ttRouting.EstimateNo   = ipbf-eb.est-no
            ttRouting.FormId       = ipbf-eb.form-no
            ttRouting.BlankID      = ipbf-eb.blank-no
            ttRouting.Pass         = ipiPass
            ttRouting.DeptSeq      = 10 * ipbf-mach.d-seq
            ttRouting.DepartmentId = ipcDept
            ttRouting.OperationId  = ipbf-mach.m-code.
           
    END.

END PROCEDURE.

PROCEDURE Operations_ImportMachineStandards:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEstOp        AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipdProbeQty     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSpeed        AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMRHrs        AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMRWaste      AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSpoilPrct    AS DECIMAL NO-UNDO.

    DEFINE VARIABLE iNumSheets AS INTEGER NO-UNDO.

    RUN pGetOperationStandardsForEstOp(ipriEstOp, ipdProbeQty, OUTPUT opdSpeed, OUTPUT opdMRHrs, OUTPUT opdSpoilPrct, OUTPUT opdMRWaste, OUTPUT iNumSheets).
       
END PROCEDURE.


PROCEDURE pCheckAndAddMachine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Check if respective deptt Machine and add routing if valid machine 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb             FOR eb.
    DEFINE INPUT  PARAMETER ipcDeptToAdd        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplRunValidation    AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipdEstQty           AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPass             AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE iCnt       AS INTEGER NO-UNDO.
    DEFINE VARIABLE rMachRowId AS ROWID   NO-UNDO.
    DEFINE VARIABLE lValidMach AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-style FOR style.
    DEFINE BUFFER bf-mach  FOR mach.
    
    EMPTY TEMP-TABLE ttMachineList.
    
    FIND FIRST bf-style NO-LOCK 
        WHERE bf-style.company EQ ipbf-eb.company
          AND bf-style.style EQ ipbf-eb.style NO-ERROR.
          
    IF AVAILABLE bf-style THEN
    DO:
        Style-blk:
        DO iCnt = 1 TO 7:
            IF bf-style.m-code[iCnt] EQ "" THEN 
                NEXT.
                    
            FIND FIRST bf-mach NO-LOCK
                WHERE bf-mach.company  EQ ipbf-eb.company
                  AND bf-mach.obsolete EQ NO  
                  AND bf-mach.m-code   EQ bf-style.m-code[iCnt] NO-ERROR.
                
            IF AVAILABLE bf-mach 
                AND (bf-mach.dept[1] EQ ipcDeptToAdd
                 OR  bf-mach.dept[2] EQ ipcDeptToAdd
                 OR  bf-mach.dept[3] EQ ipcDeptToAdd
                 OR  bf-mach.dept[4] EQ ipcDeptToAdd) THEN
            DO:
                CREATE ttMachineList.
                ASSIGN 
                    ttMachineList.Company     = bf-mach.company
                    ttMachineList.OperationId = bf-mach.m-code.
                    
            END. /* IF AVAILABLE bf-mach */
            
        END. /* Style-blk */
    END.
    
    FOR EACH bf-mach NO-LOCK
        WHERE bf-mach.company  EQ ipbf-eb.company
        AND bf-mach.obsolete EQ NO
        AND bf-mach.dept[1]  EQ ipcDeptToAdd:
            
        CREATE ttMachineList.
        ASSIGN 
            ttMachineList.Company     = bf-mach.company
            ttMachineList.OperationId = bf-mach.m-code.              
    END. /* FOR EACH bf-mach NO-LOCK */
    
    
    FOR EACH ttMachineList,
        FIRST bf-mach NO-LOCK
        WHERE bf-mach.company = ttMachineList.Company
        AND bf-mach.m-code  = ttMachineList.OperationId:
        
        IF iplRunValidation THEN
        DO:
            IF fValidMachineLimits(BUFFER ipbf-eb, BUFFER bf-mach, ipdEstQty, ipcDeptToAdd) THEN
                lValidMach = YES.
                        
            IF lValidMach = YES AND LOOKUP(gcDeptsForCoaters, ipcDeptToAdd) GT 0 THEN
            DO:
                IF fVerifyLimitsForPrinter(BUFFER bf-mach, ipcDeptToAdd) = NO THEN
                   lValidMach = NO.
            END.
        END.
        ELSE
            lValidMach = YES.
            
        IF lValidMach THEN
        DO:
            RUN pAddMachine(BUFFER ipbf-eb, BUFFER bf-mach, ipcDeptToAdd, ipiPass).
            
            oplSuccess = YES.
            LEAVE.
        END.
    END. /* FOR EACH ttMachineList */
    
END PROCEDURE.
    
PROCEDURE pCheckAndAddMachineFromRouting PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Check if respective deptt Machine and add routing if valid machine 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef             FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb             FOR eb.
    DEFINE PARAMETER BUFFER ipbf-routing        FOR routing.
    DEFINE INPUT  PARAMETER iplRunValidation    AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipdEstQty           AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPass             AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE iCnt       AS INTEGER NO-UNDO.
    DEFINE VARIABLE rMachRowId AS ROWID   NO-UNDO.
    DEFINE VARIABLE lValidMach AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cDeptToAdd AS CHARACTER NO-UNDO.
        
    DEFINE BUFFER bf-mach  FOR mach.
    
    EMPTY TEMP-TABLE ttMachineList.
    
    Mach-blk:
    DO iCnt = 1 TO EXTENT(ipbf-routing.m-code):
        IF ipbf-routing.m-code[iCnt] EQ "" THEN 
            NEXT.
                    
        FIND FIRST bf-mach NO-LOCK
            WHERE bf-mach.company  EQ ipbf-eb.company
              AND bf-mach.obsolete EQ NO  
              AND bf-mach.m-code   EQ ipbf-routing.m-code[iCnt] NO-ERROR.
                
        IF AVAILABLE bf-mach THEN
        DO:
            CREATE ttMachineList.
            ASSIGN 
                ttMachineList.Company     = bf-mach.company
                ttMachineList.OperationId = bf-mach.m-code.
                    
        END. /* IF AVAILABLE bf-mach */
            
    END. /* Mach-blk */
    
    FOR EACH ttMachineList,
        FIRST bf-mach NO-LOCK
        WHERE bf-mach.company = ttMachineList.Company
          AND bf-mach.m-code  = ttMachineList.OperationId:
        
        cDeptToAdd = bf-mach.dept[1]. 
        
        /* Department for Printers*/
        IF LOOKUP(gcDeptsForCoaters, cDeptToAdd) GT 0 THEN
        DO:
            RUN pProcessInk (BUFFER ipbf-ef, BUFFER ipbf-eb, ipdEstQty).
            NEXT.
        END.
        
        /* Other Departments */
        IF iplRunValidation THEN
        DO:
            IF fValidMachineLimits(BUFFER ipbf-eb, BUFFER bf-mach, ipdEstQty, cDeptToAdd) THEN
                lValidMach = YES.
                        
            IF lValidMach = YES AND LOOKUP(gcDeptsForCoaters, cDeptToAdd) GT 0 THEN
            DO:
                IF fVerifyLimitsForPrinter(BUFFER bf-mach, cDeptToAdd) = NO THEN
                   lValidMach = NO.
            END.
        END.
        ELSE
            lValidMach = YES.
            
        IF lValidMach THEN
        DO:
            RUN pAddMachine(BUFFER ipbf-eb, BUFFER bf-mach, cDeptToAdd, ipiPass).
            
            oplSuccess = YES.
        END.
    END. /* FOR EACH ttMachineList */
    
END PROCEDURE.

PROCEDURE pCollectDataForEstimate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Calculate and prepare data for source Est-op/Estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE INPUT PARAMETER ipriEstop                AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateType         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTargetQty            AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdBlankQty             AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQtyFGOnFormYielded   AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER                         TABLE FOR ttEstOp.
    DEFINE OUTPUT PARAMETER oplError                AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQtyYield           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyFGOnFormYielded AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cQtyValue           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAttrName           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE opdProbQty          AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-est-op FOR est-op.
    DEFINE BUFFER bf-eb     FOR eb.
   
    FIND FIRST bf-est-op NO-LOCK
        WHERE ROWID(bf-est-op) = ipriEstop NO-ERROR.
            
    IF NOT AVAILABLE bf-est-op THEN
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Est-op Record not found"
            .
        RETURN.
    END.
        
    opdTargetQty = bf-est-op.qty.
        
    RUN pGetEstOPDataFromEstimate (BUFFER ipbf-ef, opdTargetQty,OUTPUT TABLE ttEstOp).
    
    /* Get Qty for which Probe is being processed */
    RUN pGetAttribute(giAttributeIDEstQtyPerFeed, OUTPUT cQtyValue, OUTPUT cAttrName, OUTPUT cError, OUTPUT cMessage). //Get colors attribute
    ASSIGN opdProbQty = DECIMAL(cQtyValue) NO-ERROR.
    
    IF opdProbQty EQ 0 THEN    
        opdProbQty = bf-est-op.qty.
    
    opdBlankQty = fGetRequiredQtyUsingEstOp (BUFFER ipbf-eb, opdProbQty, ipcEstimateType ,"").
        
    FOR EACH bf-eb NO-LOCK
        OF ipbf-ef:
            
        dQtyYield = fGetRequiredQtyUsingEstOp (BUFFER bf-eb, bf-est-op.qty, ipcEstimateType ,"YieldQty").
        dQtyFGOnFormYielded = dQtyFGOnFormYielded + dQtyYield.
    END.

END PROCEDURE.

PROCEDURE pCollectDataForJobMch PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Calculate and prepare data for source job-mch
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE INPUT PARAMETER ipriJobMch               AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateType         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAction               AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOrgMachine           AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTargetQty            AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdBlankQty             AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQtyFGOnFormYielded   AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER                         TABLE FOR ttEstOp.
    DEFINE OUTPUT PARAMETER oplError                AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQtyRequired        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyYield           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyFGOnFormYielded AS DECIMAL NO-UNDO.
    
    
    DEFINE BUFFER bf-job-mch FOR Job-mch.
    DEFINE BUFFER bf-job     FOR Job.
    DEFINE BUFFER bf-eb      FOR eb.
    
    FIND FIRST bf-job-mch NO-LOCK
        WHERE ROWID(bf-job-mch) = ipriJobMch NO-ERROR.
        
    IF NOT AVAILABLE bf-job-mch THEN
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Job-mch Record not found"
            .
        RETURN.
    END.
        
    FIND FIRST bf-job NO-LOCK 
        WHERE bf-job.company EQ bf-job-mch.company
        AND bf-job.job EQ bf-job-mch.job
        NO-ERROR.
    
    IF NOT AVAILABLE bf-job THEN
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Job Record not found"
            .
        RETURN.
    END.
        
    dQtyRequired = fGetRequiredQtyUsingJob (BUFFER ipbf-eb, BUFFER bf-job, INPUT ipcEstimateType, INPUT "").
        
    RUN pGetEffectiveEstOpQuantity (ipbf-eb.company, ipbf-eb.est-no, dQtyRequired, OUTPUT opdTargetQty).
        
    IF opdTargetQty EQ 0 THEN
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Operation's effective Quantity not found"
            .
        RETURN.
    END.
        
    RUN pGetEstOPDataFromJobMch (BUFFER ipbf-ef, BUFFER ipbf-eb, BUFFER bf-job-mch, bf-job.loc, ipcAction, ipcOrgMachine, opdTargetQty,OUTPUT TABLE ttEstOp).
        
    FOR EACH bf-eb NO-LOCK
        OF ipbf-ef:
            
        dQtyYield = fGetRequiredQtyUsingJob (BUFFER bf-eb, BUFFER bf-job, ipcEstimateType ,"YieldQty").
        dQtyFGOnFormYielded = dQtyFGOnFormYielded + dQtyYield.
    END.
    
    opdBlankQty = dQtyRequired.

END PROCEDURE.

PROCEDURE pCreateInkRec PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process the Ink setup on Estimate and creates Ink TT
     Notes: It works for Corrugated Industry and refers respective fields
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT  PARAMETER ipiPass     AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemCode AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-item FOR Item.
    
    
    FIND FIRST bf-Item NO-LOCK
        WHERE bf-Item.company EQ ipbf-eb.company
          AND bf-Item.i-no    EQ ipcItemCode
          AND INDEX("IV",bf-Item.mat-type) GT 0 NO-ERROR.  

    IF AVAILABLE bf-Item THEN 
    DO:
        FIND FIRST ttInk
            WHERE ttInk.FormId   EQ ipbf-eb.form-no
              AND ttInk.BlankID  EQ ipbf-eb.blank-no
              AND ttInk.Pass     EQ ipiPass
              AND ttInk.InkCode  EQ ipcItemCode NO-ERROR.
              
        IF NOT AVAILABLE ttInk THEN
        DO:
            CREATE ttInk.
            ASSIGN 
                ttInk.FormId    = ipbf-eb.form-no
                ttInk.BlankID   = ipbf-eb.blank-no
                ttInk.Pass      = ipiPass
                ttInk.InkCode   = bf-Item.i-no
                ttInk.PressType = bf-Item.press-type.
             
        END.
        
        IF bf-Item.mat-type EQ "I" THEN
            ttInk.InkCount = ttInk.InkCount + 1.
            
        ELSE IF bf-Item.ink-type EQ "A" THEN
            ttInk.IsCoating = YES.
        ELSE
            ttInk.VarnishCount = ttInk.VarnishCount + 1.
        
    END.
    
END PROCEDURE.

PROCEDURE pGetEfficiencyByQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Check Efficiency By Quantity and determine the increase in the Speed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mach FOR mach.
    DEFINE PARAMETER BUFFER ipbf-mstd FOR mstd.
    DEFINE OUTPUT PARAMETER opdIncrease AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError    AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iExt AS INTEGER NO-UNDO.
    DEFINE VARIABLE dOperationQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cQtyValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAttrName  AS CHARACTER NO-UNDO.
    
    RUN pGetAttribute(giAttributeIDEstQtyPerFeed, OUTPUT cQtyValue, OUTPUT cAttrName, OUTPUT oplError, OUTPUT opcMessage). //Get colors attribute
    ASSIGN dOperationQty = DECIMAL(cQtyValue) NO-ERROR.
    
    /* Extra Sheets Calc */
    DO iExt = 1 TO 9:
      IF ipbf-mstd.run-qty[iExt] GT dOperationQty THEN 
        LEAVE.
        
      IF ipbf-mstd.x-sheets[iExt] NE 0 THEN 
          opdIncrease = ipbf-mstd.x-sheets[iExt] / 100.
      
    END.
   

END PROCEDURE.

PROCEDURE pGetInkTT PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstimateType    AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE iExt AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-eb FOR eb.
    
    FOR EACH bf-eb NO-LOCK
        WHERE bf-eb.company EQ ipcCompany
        AND bf-eb.est-no  EQ ipcEstimateNo: 
            
        IF ipiEstimateType LE 4 THEN
        DO iExt = 1 TO EXTENT(bf-eb.i-code2):
            RUN pCreateInkRec (BUFFER bf-eb, bf-eb.i-ps2[iExt], bf-eb.i-code2[iExt]).
        END.
        ELSE
        DO iExt = 1 TO EXTENT(bf-eb.i-code):
            RUN pCreateInkRec (BUFFER bf-eb, bf-eb.i-ps[iExt], bf-eb.i-code[iExt]).
        END.
    END.
                
END PROCEDURE.

PROCEDURE pGetRoutingMatrix PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLoc     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStyle   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiMSF     AS INTEGER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb          FOR eb.
    DEFINE PARAMETER BUFFER opbf-routing-mtx FOR routing-mtx.

    FIND FIRST opbf-routing-mtx NO-LOCK
        WHERE opbf-routing-mtx.company EQ ipcCompany
          AND opbf-routing-mtx.loc     EQ ipcLoc
          AND opbf-routing-mtx.style   EQ ipcStyle
          AND opbf-routing-mtx.msf     GE ipiMSF NO-ERROR.
          
    IF AVAILABLE opbf-routing-mtx THEN
    DO:      
        RUN pAddAxis("RX", 0, opbf-routing-mtx.bl-len).
        RUN pAddAxis("RY", 0, opbf-routing-mtx.bl-wid).
    END.

END PROCEDURE.

PROCEDURE pProcessDeptFromRouting PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Process machines from Routing dept
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb          FOR eb.
    DEFINE PARAMETER BUFFER ipbf-routing     FOR routing.
    DEFINE INPUT  PARAMETER ipdEstQty        AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lMachineAdded AS LOGICAL NO-UNDO.
    
    RUN pCheckAndAddMachineFromRouting(BUFFER ipbf-ef, BUFFER ipbf-eb, BUFFER ipbf-routing,Yes, ipdEstQty,1,OUTPUT lMachineAdded).
        
END PROCEDURE.


PROCEDURE pProcessCRDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Check if corrugator needed
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef     FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb     FOR eb.
    DEFINE INPUT  PARAMETER ipdEstQty   AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lMachineAdded AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-item-bom FOR item-bom.
    
    /* Check if corrugator machine required */
    IF CAN-FIND(FIRST bf-item-bom
                WHERE bf-item-bom.company  EQ ipbf-ef.company
                  AND bf-item-bom.parent-i EQ ipbf-ef.board) THEN
        RUN pCheckAndAddMachine (BUFFER ipbf-eb, "CR", YES, ipdEstQty, 1, OUTPUT lMachineAdded).
        
END PROCEDURE.

PROCEDURE pProcessCTDept PRIVATE:
/*------------------------------------------------------------------------------
     Purpose:
     Notes: Check if Coating machine needed
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef     FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb     FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ttInk  FOR ttInk.
    DEFINE INPUT  PARAMETER ipdEstQty   AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lMachineAdded AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iColors      AS INTEGER NO-UNDO.
    
    iColors = ipbf-ttInk.InkCount + ipbf-ttInk.VarnishCount.
    
    IF ipbf-ttInk.IsCoating THEN
        RUN pCheckAndAddMachine (BUFFER ipbf-eb, "CT", YES, ipdEstQty, ipbf-ttInk.Pass, OUTPUT lMachineAdded).
        
END PROCEDURE.

PROCEDURE pProcessDCDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process and import die cutter machine
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef         FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb         FOR eb.
    DEFINE INPUT  PARAMETER ipdEstQty       AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lMachineAdded AS LOGICAL NO-UNDO.
    
    RUN pCheckAndAddMachine (BUFFER ipbf-eb, "DC",NO, ipdEstQty, 1, OUTPUT lMachineAdded).
    
END PROCEDURE.


PROCEDURE pProcessFODept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process and import Farm Out machine
     Notes: Check if Estimate Item is Purchased Finished Good
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef         FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb         FOR eb.
    DEFINE INPUT  PARAMETER ipdEstQty       AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMachineAdded AS LOGICAL NO-UNDO.
    
    IF ipbf-eb.pur-man THEN
        RUN pCheckAndAddMachine (BUFFER ipbf-eb, "FO",NO, ipdEstQty, 1, OUTPUT oplMachineAdded).
    
END PROCEDURE.


PROCEDURE pProcessRCDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Check if Ream cutter/Sliter/Saw needed
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef     FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb     FOR eb.
    DEFINE INPUT  PARAMETER ipdEstQty   AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lMachineAdded AS LOGICAL NO-UNDO.
    
    /* Check if corrugator machine required */
    IF ipbf-ef.n-cuts GT 0 OR ipbf-ef.roll THEN
        RUN pCheckAndAddMachine (BUFFER ipbf-eb, "RC", YES, ipdEstQty, 1, OUTPUT lMachineAdded).

END PROCEDURE.

PROCEDURE pProcessGUDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Check if guillotine machine needed
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef     FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb     FOR eb.
    DEFINE INPUT  PARAMETER ipdEstQty   AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lMachineAdded AS LOGICAL NO-UNDO.
    
    /* Check if gluer machine required */
    IF ipbf-ef.n-out-l GT 1 THEN
        RUN pCheckAndAddMachine (BUFFER ipbf-eb, "GU", YES, ipdEstQty, 1, OUTPUT lMachineAdded).

END PROCEDURE.

PROCEDURE pProcessLMDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Check if laminating machine needed
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef     FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb     FOR eb.
    DEFINE INPUT  PARAMETER ipdEstQty   AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lMachineAdded AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dCal          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cDeptCode    AS CHARACTER NO-UNDO INIT "LM".
    
    DEFINE BUFFER opbf-ttDeptLimit FOR ttDeptLimit.
    
    RUN pSetLimitsForDept (BUFFER ipbf-ef, BUFFER ipbf-eb,cDeptCode, BUFFER opbf-ttDeptLimit).
    
    /* Add-on logic for LM dept */    
    IF AVAILABLE opbf-ttDeptLimit THEN
    DO:    
        /* trim-pen stored as integer */
        IF ipbf-ef.trim-pen NE 0 THEN 
        DO:
            dCal = ipbf-ef.trim-pen / 1000. 
            IF ipbf-ef.cal NE 0 AND ipbf-ef.cal NE .014 THEN 
                dCal = dCal - .014 + ipbf-ef.cal.
        END.
        
        IF dCal NE 0 THEN
            opbf-ttDeptLimit.Caliper = dCal.
    END. 
    
    IF ipbf-ef.medium NE "" THEN
        RUN pCheckAndAddMachine (BUFFER ipbf-eb, cDeptCode, YES, ipdEstQty, 1, OUTPUT lMachineAdded).

END PROCEDURE.

PROCEDURE pProcessGLDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Check if gluer needed
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef     FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb     FOR eb.
    DEFINE INPUT  PARAMETER ipdEstQty   AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lMachineAdded AS LOGICAL NO-UNDO.
    
    /* Check if gluer machine required */
    IF ipbf-eb.adhesive NE "" AND ipbf-eb.gluelap  NE 0 THEN
        RUN pCheckAndAddMachine (BUFFER ipbf-eb, "GL", YES, ipdEstQty, 1, OUTPUT lMachineAdded).

END PROCEDURE.

PROCEDURE pProcessPRDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Check if Ream cutter/Sliter/Saw needed
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef    FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb    FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ttInk FOR ttInk.
    DEFINE INPUT  PARAMETER ipdEstQty   AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lMachineAdded AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iMaxColor     AS INTEGER NO-UNDO.
    DEFINE VARIABLE cDeptCode    AS CHARACTER NO-UNDO INIT "PR".
    
    DEFINE BUFFER opbf-ttDeptLimit FOR ttDeptLimit.
    
    iMaxColor = ipbf-ttInk.InkCount + ipbf-ttInk.VarnishCount.
    
    RUN pSetLimitsForDept (BUFFER ipbf-ef, BUFFER ipbf-eb, cDeptCode, BUFFER opbf-ttDeptLimit).
    
    IF AVAILABLE opbf-ttDeptLimit THEN
        ASSIGN opbf-ttDeptLimit.NumColors = iMaxColor
               opbf-ttDeptLimit.PressType = ipbf-ttInk.PressType.
    
    RUN pCheckAndAddMachine (BUFFER ipbf-eb, cDeptCode, YES, ipdEstQty, ipbf-ttInk.Pass, OUTPUT lMachineAdded).
    
END PROCEDURE.

PROCEDURE pProcessInk PRIVATE:
    /*------------------------------------------------------------------------------
         Purpose:
         Notes: Check if INK is defined and printing is needed
        ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT  PARAMETER ipdEstQty   AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-Item FOR item.
    
    FOR EACH ttInk
        WHERE ttInk.FormId  = ipbf-eb.form-no
          AND ttInk.BlankID = ipbf-eb.blank-no:
            
        RUN pProcessCTDept (BUFFER ipbf-ef, BUFFER ipbf-eb, BUFFER ttInk, ipdEstQty).
        
        RUN pProcessPRDept (BUFFER ipbf-ef, BUFFER ipbf-eb, BUFFER ttInk, ipdEstQty).
              
    END.
    
END PROCEDURE.

PROCEDURE pRemoveRedundantDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Cleanup any redundant department machines
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttRouting  FOR ttRouting.
    
    /* Check secondary departments for Priter */
    RUN pCleanupSubDeptForPrimaryDept (ipcCompany, ipcEstimateNo, "PR", "CT" ).
    RUN pCleanupSubDeptForPrimaryDept (ipcCompany, ipcEstimateNo, "PR", "RC" ).
    RUN pCleanupSubDeptForPrimaryDept (ipcCompany, ipcEstimateNo, "PR", "DC" ).
    RUN pCleanupSubDeptForPrimaryDept (ipcCompany, ipcEstimateNo, "PR", "HS" ).
    RUN pCleanupSubDeptForPrimaryDept (ipcCompany, ipcEstimateNo, "PR", "GL" ).
    
    /* Check secondary departments for Die Cutter */
    RUN pCleanupSubDeptForPrimaryDept (ipcCompany, ipcEstimateNo, "DC", "HS" ).
    
    
    /* If RS/window/film/label/leaf on any machine - delete others */
    RUN pCleanupSubDeptForAnyDept (ipcCompany, ipcEstimateNo, "RS" ).
    RUN pCleanupSubDeptForAnyDept (ipcCompany, ipcEstimateNo, "WS" ).
    RUN pCleanupSubDeptForAnyDept (ipcCompany, ipcEstimateNo, "WN" ).
    RUN pCleanupSubDeptForAnyDept (ipcCompany, ipcEstimateNo, "FS" ).
    RUN pCleanupSubDeptForAnyDept (ipcCompany, ipcEstimateNo, "FB" ).
    
    
END PROCEDURE.

PROCEDURE pCleanupSubDeptForPrimaryDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Cleanup any redundant department machines
     Notes: If primary Dept is having additional dept equal to given dept then we'll delete it
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDeptPrimary  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDeptSecond   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-ttRouting FOR ttRouting.
    DEFINE BUFFER bf-mach      FOR mach.
    
    FOR FIRST bf-ttRouting
        WHERE bf-ttRouting.Company      =  ipcCompany
          AND bf-ttRouting.EstimateNo   = ipcEstimateNo
          AND bf-ttRouting.departmentID = ipcDeptPrimary:
              
        FIND FIRST bf-mach NO-LOCK
            WHERE bf-mach.company  EQ bf-ttRouting.Company
            AND bf-mach.m-code     EQ bf-ttRouting.OperationId
            AND (bf-mach.dept[1]   EQ ipcDeptSecond
             OR  bf-mach.dept[2]   EQ ipcDeptSecond
             OR  bf-mach.dept[3]   EQ ipcDeptSecond
             OR  bf-mach.dept[4]   EQ ipcDeptSecond) NO-ERROR.
            
        IF AVAILABLE bf-mach THEN
            lFound = YES.
            
    END. /* FOR FIRST bf-ttRouting */
    
    IF lFound THEN
        FOR EACH ttRouting
            WHERE ttRouting.Company    EQ ipcCompany
            AND ttRouting.EstimateNo   EQ ipcEstimateNo
            AND ttRouting.departmentID EQ ipcDeptSecond:
    
            DELETE  ttRouting.  
        END. /* FOR EACH ttRouting */
    
END.

PROCEDURE pCleanupSubDeptForAnyDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Cleanup any redundant department machines
     Notes: If Any other Machine is having the given dept then we'll delete the given dept
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDeptSecond   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-ttRouting FOR ttRouting.
    DEFINE BUFFER bf-mach      FOR mach.
    
    
    FOR EACH bf-ttRouting
        WHERE bf-ttRouting.Company      EQ  ipcCompany
          AND bf-ttRouting.EstimateNo   EQ ipcEstimateNo
          AND bf-ttRouting.departmentID NE ipcDeptSecond:
          
        FIND FIRST bf-mach NO-LOCK
            WHERE bf-mach.company  EQ bf-ttRouting.Company
            AND bf-mach.m-code     EQ bf-ttRouting.OperationId
            AND (bf-mach.dept[1]   EQ ipcDeptSecond
             OR  bf-mach.dept[2]   EQ ipcDeptSecond
             OR  bf-mach.dept[3]   EQ ipcDeptSecond
             OR  bf-mach.dept[4]   EQ ipcDeptSecond) NO-ERROR.
            
        IF AVAILABLE bf-mach THEN
            lFound = YES.
            
    END. /* FOR EACH bf-ttRouting */
    
    IF lFound THEN
        FOR EACH ttRouting
            WHERE ttRouting.Company    EQ ipcCompany
            AND ttRouting.EstimateNo   EQ ipcEstimateNo
            AND ttRouting.departmentID EQ ipcDeptSecond:
    
            DELETE  ttRouting.  
        END. /* FOR EACH ttRouting */
    
END.


PROCEDURE pSetLimitsForDept PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Set Limits/Size criteria for a given department
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef         FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb         FOR eb.
    DEFINE INPUT  PARAMETER ipcDept         AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER bf-ttDeptLimit  FOR ttDeptLimit.
    
    DEFINE VARIABLE dCal AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-ttDeptLimit
        WHERE bf-ttDeptLimit.DeptCode = ipcDept NO-ERROR.
        
    IF NOT AVAILABLE bf-ttDeptLimit THEN
    DO:    
        CREATE bf-ttDeptLimit.
        ASSIGN 
            bf-ttDeptLimit.DeptCode  = ipcDept   
            bf-ttDeptLimit.ShtLength = IF ipbf-ef.roll THEN ipbf-ef.gsh-len ELSE ipbf-ef.nsh-len
            bf-ttDeptLimit.ShtWidth  = IF ipbf-ef.roll THEN ipbf-ef.gsh-wid ELSE ipbf-ef.nsh-wid
            bf-ttDeptLimit.Caliper   = ipbf-ef.cal
            .
    END.
           
END PROCEDURE.

PROCEDURE pResetObjects PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Delete all internal objects.
 Notes: Delete Handles, objects, temp-table etc
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcBeforeAftr AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttJobMch.
    EMPTY TEMP-TABLE ttEstBlank.
    EMPTY TEMP-TABLE ttAxis.
    EMPTY TEMP-TABLE  ttEstOp.
        
    /* If being run after the process don't Cleanup objets used by other calling routines*/
    IF ipcBeforeAftr EQ "Before" THEN
    DO:
        EMPTY TEMP-TABLE  ttOperation.
        EMPTY TEMP-TABLE  ttAttribute.
        EMPTY TEMP-TABLE  ttOpsTag.
        glTagDisabled = FALSE.
    END.

END PROCEDURE.

PROCEDURE Operations_ClearAttributes:
    /*------------------------------------------------------------------------------
     Purpose:  Clears all attributes
     Notes:
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAttribute.

END PROCEDURE.


PROCEDURE Operations_GetAttributes:
    /*------------------------------------------------------------------------------
     Purpose: Returns the temp-table to caller
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttAttribute.

END PROCEDURE.

PROCEDURE pGetOperationRates PRIVATE:
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

PROCEDURE Operations_GetOperationStandards:
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


PROCEDURE pGetOperationStandardsForJobMch PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given job-mch rowid, get updated standards
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJobMch AS ROWID.
    DEFINE INPUT  PARAMETER ipcAction      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcExistingOps AS CHARACTER NO-UNDO.
    
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
        IF NOT AVAILABLE bf-eb THEN RETURN.
           
                
        RUN pSetAttributesFromJobMch (ROWID(bf-job-mch), bf-job-mch.m-code, bf-job-mch.pass, ipcAction, ipcExistingOps, OUTPUT lError, OUTPUT cMessage).
        
        IF cMessage NE "" THEN
            RUN pBuildTagInfo ("Error","Error from pSetAttributesFromJobMch",cMessage).
            
        RUN pBuildMessage("", INPUT-OUTPUT cMessage).
        
        IF NOT lError THEN 
        DO:
            FIND CURRENT bf-job-mch EXCLUSIVE-LOCK.
            RUN pGetOperationRates(bf-job-mch.company, bf-job.loc, bf-job-mch.m-code, 
                OUTPUT bf-job-mch.mr-rate, 
                OUTPUT bf-job-mch.mr-fixoh, 
                OUTPUT bf-job-mch.mr-varoh,
                OUTPUT bf-job-mch.run-rate,
                OUTPUT bf-job-mch.run-fixoh,
                OUTPUT bf-job-mch.run-varoh,
                OUTPUT lError, OUTPUT cMessage).
                
            RUN pBuildMessage("", INPUT-OUTPUT cMessage).
        
            RUN GetOperationStandards(bf-job-mch.company, bf-job.loc, bf-job-mch.m-code,
                OUTPUT bf-job-mch.mr-waste, 
                OUTPUT bf-job-mch.mr-hr, 
                OUTPUT bf-job-mch.speed, 
                OUTPUT bf-job-mch.wst-prct, 
                OUTPUT lError, OUTPUT cMessage).
            
            RUN pBuildMessage("", INPUT-OUTPUT cMessage).
            
            bf-job-mch.run-qty = fGetJobMachRunQty(BUFFER bf-eb, bf-job-mch.m-code,bf-job-mch.pass,bf-job-mch.run-qty).
        END.
        
        FIND CURRENT bf-job-mch NO-LOCK.
        IF AVAILABLE bf-job-mch THEN
            RUN pAddTagInfo (bf-job-mch.rec_key).
    END.

END PROCEDURE.

PROCEDURE pGetOperationStandardsForEstOp PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given ESt-op rowid, gessign the Operations standards
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriRowid       AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipdProbeQty     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSpeed        AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMRHrs        AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSpoilPrct    AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMRWaste      AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiNumSheets    AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-est         FOR est.
    DEFINE BUFFER bf-ef          FOR ef.
    DEFINE BUFFER bf-eb          FOR eb.
    DEFINE BUFFER bf-est-op      FOR est-op.
    
    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cEstSheets AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAttrName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNumSheets AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dMRWaste   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMRHrs     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSpeed     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSpoilPrct AS DECIMAL   NO-UNDO.
    
     RUN pSetProcessScope(INPUT "MainProcessStart").
     
    FIND FIRST bf-est-op NO-LOCK
        WHERE ROWID(bf-est-op) = ipriRowid NO-ERROR.
    
    IF AVAILABLE bf-est-op THEN
    DO:         
        FIND FIRST bf-est NO-LOCK
            WHERE bf-est.company  EQ bf-est-op.company
            AND bf-est.est-no   EQ bf-est-op.est-no NO-ERROR.
        
        IF NOT AVAILABLE bf-est THEN
            RETURN.
        
        FIND FIRST bf-eb NO-LOCK 
            WHERE bf-eb.company  EQ bf-est-op.company
            AND bf-eb.est-no   EQ bf-est-op.est-no
            AND bf-eb.form-no  EQ MAX(bf-est-op.s-num,1)
            AND bf-eb.blank-no EQ MAX(bf-est-op.b-num,1) NO-ERROR.
            
        IF NOT AVAILABLE bf-eb THEN RETURN.
                
        RUN pSetAttributesFromEstOp (ipriRowid, bf-est.loc, ipdProbeQty, OUTPUT lError, OUTPUT cMessage).
            
        RUN pBuildMessage("", INPUT-OUTPUT cMessage).
        
        IF NOT lError THEN 
        DO:
            RUN Operations_GetOperationStandards(bf-est-op.company, bf-est.loc, bf-est-op.m-code,
                OUTPUT dMRWaste, 
                OUTPUT dMRHrs, 
                OUTPUT dSpeed, 
                OUTPUT dSpoilPrct, 
                OUTPUT lError,
                OUTPUT cMessage).
                
         
            RUN pBuildMessage("", INPUT-OUTPUT cMessage).
            
            RUN pGetAttribute(giAttributeIDEstSheets, OUTPUT cEstSheets, OUTPUT cAttrName, OUTPUT lError, OUTPUT cMessage). //Get colors attribute
            
            ASSIGN 
                iNumSheets = INTEGER(cEstSheets) NO-ERROR.
        END.
        
        ASSIGN
            opdMRHrs     = dMRHrs
            opdSpoilPrct = dSpoilPrct
            opdMRWaste   = dMRWaste
            opdSpeed     = dSpeed
            opiNumSheets = iNumSheets
            .
        
         
    END.
    RUN pSetProcessScope(INPUT "MainProcessEnd").
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

PROCEDURE pAddOperationFromEstOp PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an ttOperation (~estCostOperation) based on est-op and form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstOp       FOR ttEstOp.
    DEFINE PARAMETER BUFFER ipbf-est           FOR est.
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER opbf-ttOperation   FOR ttOperation.
    DEFINE INPUT  PARAMETER ipcEstType         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE inumOutBlanksOnNet AS INTEGER NO-UNDO.
    DEFINE VARIABLE inumOutNet         AS INTEGER NO-UNDO.

    DEFINE           BUFFER bf-mach            FOR mach.
    DEFINE           BUFFER bf-eb              FOR eb.
    DEFINE           BUFFER bf-ttEstOP          FOR ttEstOp.

    
    FOR EACH bf-eb NO-LOCK
        OF ipbf-ef:
        inumOutBlanksOnNet = inumOutBlanksOnNet + (MAX(bf-eb.num-wid, 1) * MAX(bf-eb.num-len, 1) * MAX(bf-eb.num-dep, 1)).
    END.
    
    FIND FIRST bf-mach NO-LOCK 
        WHERE bf-mach.company EQ ipbf-ttEstOp.company
        AND bf-mach.m-code EQ ipbf-ttEstOp.m-code
        NO-ERROR.
    IF AVAILABLE bf-mach THEN 
    DO:
        CREATE opbf-ttOperation.
        ASSIGN 
            opbf-ttOperation.estCostOperationID           = ipbf-ttEstOp.line
            opbf-ttOperation.estimateNo                   = ipbf-est.est-no
            opbf-ttOperation.estType                      = ipcEstType
            opbf-ttOperation.quantityMaster               = ipbf-ttEstOp.qty
            opbf-ttOperation.formNo                       = ipbf-ttEstOp.s-num
            opbf-ttOperation.blankNo                      = ipbf-ttEstOp.b-num
            opbf-ttOperation.company                      = ipbf-ttEstOp.company
            opbf-ttOperation.operationID                  = ipbf-ttEstOp.m-code
            opbf-ttOperation.pass                         = MAX(ipbf-ttEstOp.op-pass, 1)
            opbf-ttOperation.sequenceOfOperation          = ipbf-ttEstOp.line
            opbf-ttOperation.numOutDivisor                = ipbf-ttEstOp.n_out_div
            opbf-ttOperation.quantityInSetupWaste         = ipbf-ttEstOp.op-waste
            opbf-ttOperation.isLocked                     = ipbf-ttEstOp.isLocked
            opbf-ttOperation.crewSizeSetup                = ipbf-ttEstOp.op-crew[1]
            opbf-ttOperation.crewSizeRun                  = ipbf-ttEstOp.op-crew[2]
            opbf-ttOperation.countInks                    = ipbf-ttEstOp.num-col
            opbf-ttOperation.countCoats                   = ipbf-ttEstOp.num-coat
            opbf-ttOperation.countFountainChanges         = ipbf-ttEstOp.fountains
            opbf-ttOperation.countPlateChanges            = ipbf-ttEstOp.plates
            opbf-ttOperation.isSpeedInLF                  = bf-mach.therm
            opbf-ttOperation.operationName                = bf-mach.m-dscr
            opbf-ttOperation.feedType                     = bf-mach.p-type
            opbf-ttOperation.outputType                   = bf-mach.p-type
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
            IF NOT CAN-FIND(FIRST ttOperation 
                WHERE ttOperation.formNo EQ opbf-ttOperation.formNo 
                AND ttOperation.isNetSheetMaker
                AND ttOperation.estCostOperationID NE opbf-ttOperation.estCostOperationID) THEN   
                opbf-ttOperation.isNetSheetMaker = YES.
            
            opbf-ttOperation.outputType      = "S".
        END.
        IF fIsDepartment(gcDeptsForGluers, opbf-ttOperation.departmentID)  THEN 
            opbf-ttOperation.isGluer = YES.
        IF fIsDepartment(gcDeptsForLeafers, opbf-ttOperation.departmentID)  THEN 
            opbf-ttOperation.isLeafer = YES.
        
        IF CAN-DO("R,S,A,P",opbf-ttOperation.feedType) THEN 
        DO:
            FOR EACH bf-ttEstOP NO-LOCK 
                WHERE bf-ttEstOP.company EQ ipbf-ttEstOp.company
                AND bf-ttEstOP.est-no EQ ipbf-ttEstOp.est-no
                AND bf-ttEstOP.s-num EQ ipbf-ttEstOp.s-num
                AND bf-ttEstOP.qty EQ ipbf-ttEstOp.qty
                AND bf-ttEstOP.line GT ipbf-ttEstOp.line
                AND bf-ttEstOP.line LT 500,
                FIRST bf-mach NO-LOCK 
                WHERE bf-mach.company EQ bf-ttEstOP.company
                AND bf-mach.m-code EQ bf-ttEstOP.m-code 
                BY bf-ttEstOP.line:
                IF bf-mach.p-type EQ "B" THEN  /*Last machine before a blank fed*/
                    ASSIGN 
                        opbf-ttOperation.isBlankMaker = YES
                        opbf-ttOperation.outputType   = "B"
                        .
                LEAVE.
            END.
            IF NOT AVAILABLE bf-ttEstOP THEN /*Last Machine*/  
                ASSIGN 
                    opbf-ttOperation.isBlankMaker = YES
                    opbf-ttOperation.outputType   = "B"
                    .
        END.
        
        CASE opbf-ttOperation.feedType:
            WHEN "R" THEN 
                opbf-ttOperation.linealFeetPerFeed = fGetFeet(ipbf-ef.gsh-len, "IN").
            WHEN "S" THEN 
                DO:
                    IF opbf-ttOperation.isNetSheetMaker THEN 
                        opbf-ttOperation.linealFeetPerFeed = fGetFeet(ipbf-ef.gsh-len, "IN").
                    ELSE
                        opbf-ttOperation.linealFeetPerFeed = fGetFeet(ipbf-ef.nsh-len, "IN").
                END.
        END CASE.

        inumOutNet = MAX(ipbf-ef.n-out-l, 1) * MAX(ipbf-ef.n-out, 1) * MAX(ipbf-ef.n-out-d, 1).
        
        
        opbf-ttOperation.numOutForOperation = 1.
        IF opbf-ttOperation.isNetSheetMaker THEN 
            ASSIGN 
                opbf-ttOperation.numOutForOperation = fGetNetSheetOut(opbf-ttOperation.company, opbf-ttOperation.estimateNo, opbf-ttOperation.formNo, opbf-ttOperation.pass, inumOutNet)
                .
        IF opbf-ttOperation.isBlankMaker THEN 
            ASSIGN 
                opbf-ttOperation.numOutForOperation = opbf-ttOperation.numOutForOperation * inumOutBlanksOnNet.
    END.
    
END PROCEDURE.

PROCEDURE pAddTagInfo PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds process summary/error stream. Create tags on Job-mch.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iCnt  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTemp AS CHARACTER NO-UNDO.
            
    RUN ClearTagsForGroup(
            INPUT ipcRecKey,
            INPUT "OpsStandard"
            ).
            
    FOR EACH ttOpsTag:
                
        RUN AddTagInfoForGroup(
            INPUT ipcRecKey,
            INPUT "job-mch",
            INPUT ttOpsTag.Description,
            INPUT ttOpsTag.Note[1],
            INPUT "OpsStandard"
            ). /*From TagProcs Super Proc*/
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
    DO: 
        IF iopcMessage NE "" THEN
            iopcMessage = iopcMessage + CHR(13) + ipcMessageAdd.
        ELSE 
            iopcMessage = ipcMessageAdd.
            
    END.
    
END PROCEDURE.

PROCEDURE pBuildTagInfo PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the TT Tag for debug/information purpose
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcNotes AS CHARACTER NO-UNDO.
    
    IF glTagDisabled = TRUE THEN
        RETURN.
        
    CREATE ttOpsTag. 
    ASSIGN
        ttOpsTag.tagType     = ipcType
        ttOpsTag.description = ipcName
        ttOpsTag.Note[1]      = ipcNotes
        .

END PROCEDURE.

PROCEDURE pGetEffectiveEstOpQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  For a given estimate (company, estimate) and master quantity,
        find the appropriate est-op (routing definition) record and applicable Quantity
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityMaster  AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityTarget AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-est-op  FOR est-op.
    DEFINE BUFFER bf-est-qty FOR est-qty.
    
    
    FOR EACH bf-est-op NO-LOCK 
        WHERE bf-est-op.company EQ ipcCompany
        AND bf-est-op.est-no  EQ ipcEstimateID 
        AND bf-est-op.line    LT 500
        BREAK BY bf-est-op.qty:
    
        IF FIRST-OF(bf-est-op.qty) THEN 
        DO:
            /*Refactor - Qty in Ops must match one of the est-qty?*/
            IF FIRST(bf-est-op.qty) OR
                CAN-FIND(FIRST bf-est-qty
                WHERE bf-est-qty.company EQ bf-est-op.company
                AND bf-est-qty.est-no  EQ bf-est-op.est-no
                AND bf-est-qty.eqty    EQ bf-est-op.qty)
                THEN opdQuantityTarget = bf-est-op.qty.
            IF bf-est-op.qty GE ipdQuantityMaster THEN LEAVE.
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
     
    RUN pGetAttribute(giAttributeIDCaliperDecimal, OUTPUT cCaliper, OUTPUT cName, OUTPUT oplError, OUTPUT cMessage).
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
                    cDepthMessage   = "Depth reduction of " + STRING(dDepthReduction) + " for depth of " + cDepth + " index level " + STRING(iIndex).
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

    RUN pBuildTagInfo ("Info",ipcType + ": " + STRING(opdValue)+ "| X Axis [" + cXAttribute + " = " + STRING(dXAttributeValue) + "]" + ". Y Axis [" + cYAttribute + " = " + STRING(dYAttributeValue) + "]" ,"").
        
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
        IF ttAxis.axisValue GE ipdLookup THEN LEAVE.
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
    DEFINE VARIABLE dIncreaseInSpeed        AS DECIMAL NO-UNDO.
    
    RUN pGetStandardBuffer(ipbf-mach.company, ipbf-mach.loc, ipbf-mach.m-code, BUFFER bf-mstd, OUTPUT oplError, OUTPUT opcMessage).
    IF AVAILABLE bf-mstd THEN 
    DO:
        RUN pGetValue(BUFFER bf-mstd, "RunSpeed", OUTPUT opdRunSpeed, OUTPUT oplError, OUTPUT cMessageMatrix).
        RUN pGetEfficiencyByQty(BUFFER ipbf-mach, BUFFER bf-mstd,  OUTPUT dIncreaseInSpeed, OUTPUT oplError, OUTPUT cMessageItemReduction).
        opdRunSpeed     = opdRunSpeed * (1 + dIncreaseInSpeed).
        
        RUN pGetItemSpeedReduction(ipbf-mach.company, ipbf-mach.dept[1], OUTPUT dReductionItem, OUTPUT oplError, OUTPUT cMessageItemReduction).
        RUN pGetMatrixSpeedReduction(BUFFER bf-mstd, OUTPUT dReductionMatrix, OUTPUT oplError, OUTPUT cMessageMatrixReduction).
        ASSIGN 
            dReductionTotal = dReductionItem + dReductionMatrix
            opdRunSpeed     = opdRunSpeed * (1 - dReductionTotal).
        .
        RUN pBuildMessage(cMessageMatrix, INPUT-OUTPUT opcMessage).
        RUN pBuildMessage(cMessageItemReduction, INPUT-OUTPUT opcMessage).
        RUN pBuildMessage(cMessageMatrixReduction, INPUT-OUTPUT opcMessage).
        
        IF cMessageItemReduction NE "" THEN
            RUN pBuildTagInfo ("Info","RunSpeed" + ": Item Reduction- " + STRING(dReductionItem),  cMessageItemReduction).
        IF cMessageMatrixReduction NE "" THEN
            RUN pBuildTagInfo ("Info","RunSpeed" + ": Matrix Reduction- " + STRING(dReductionMatrix), cMessageMatrixReduction).
        
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
     
    opdRunSpoil = opdRunSpoil + ipbf-mach.run-spoil.
    
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
    
    
    RUN pBuildTagInfo ("Info","MRWaste: " + STRING(opdMRWaste) + "| File MR Waste- " + STRING(dMRWasteFromMach) + ". Ink Waste- " + STRING(dMRWasteFromInks),  "Colors- " + cColors + ". MR Waste per color- " + STRING(ipbf-mach.col-wastesh)).
        
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
       
    RUN pGetOperationStandardsForJobMch(ROWID(bf-job-mch), "AddDept","").
    
END PROCEDURE.

PROCEDURE pOperationChangeAddMachine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds a new machine to the job for the new operation ID.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-mch FOR job-mch.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cOldMach AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    
    CREATE bf-job-mch.
    BUFFER-COPY ipbf-job-mch EXCEPT m-code job-mchID TO bf-job-mch.
    ASSIGN
        bf-job-mch.m-code = ipcOperationID
        .
    cOldMach = ipbf-job-mch.m-code.
    RUN pGetOperationStandardsForJobMch(ROWID(bf-job-mch), "Add",cOldMach).
        
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
    
    DEFINE VARIABLE cExistingMach AS CHARACTER NO-UNDO.
    
    FIND CURRENT ipbf-job-mch EXCLUSIVE-LOCK.
    cExistingMach = ipbf-job-mch.m-code.
    ipbf-job-mch.m-code = ipcOperationID.
    FIND CURRENT ipbf-job-mch NO-LOCK.
    RUN pGetOperationStandardsForJobMch(ROWID(ipbf-job-mch), "Replace", cExistingMach).    

END PROCEDURE.

PROCEDURE pProcessOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: should replace ce/prokalk.i and ce/pr4-mch.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOperation FOR ttOperation.
    DEFINE PARAMETER BUFFER ipbf-eb          FOR eb.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOut AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutSetupWaste AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutRunWaste AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-mach FOR mach.

    DEFINE VARIABLE iInkCoatCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQty          AS DECIMAL NO-UNDO. 
    DEFINE VARIABLE dLFPerFeed    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPartCount    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lError        AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    
    FIND FIRST bf-mach NO-LOCK 
        WHERE bf-mach.company EQ ipbf-ttOperation.company
        AND bf-mach.m-code EQ ipbf-ttOperation.OperationId
        NO-ERROR.
    
    
    ASSIGN 
        ipbf-ttOperation.quantityOut       = iopdQtyInOut  /*This machines out is last machines in*/  
        ipbf-ttOperation.quantityInNoWaste = ipbf-ttOperation.quantityOut / ipbf-ttOperation.numOutForOperation  /*Get QtyIn in Feed units*/
        iopdQtyInOutSetupWaste             = iopdQtyInOutSetupWaste / ipbf-ttOperation.numOutForOperation
        iopdQtyInOutRunWaste               = iopdQtyInOutRunWaste / ipbf-ttOperation.numOutForOperation
        ipbf-ttOperation.quantityIn        = fRoundUp(ipbf-ttOperation.quantityIn)
        iopdQtyInOutSetupWaste             = fRoundUp(iopdQtyInOutSetupWaste)
        iopdQtyInOutRunWaste               = fRoundUp(iopdQtyInOutRunWaste)
        .

    /* Set Quantity and dependent attributes for each Line. These will be used to get Spoilage value */   
    ipbf-ttOperation.FGCumulativeNumOut = fGetCumulNumOutForOperation(INPUT ipbf-ttOperation.numOutForOperation).
    ipbf-ttOperation.FGCumulativeNumOut = MAX(1,ipbf-ttOperation.FGCumulativeNumOut).
     
    RUN pSetAttribute(giAttributeIDEstQtyPerFeed, gcAttributeNameEstQtyPerFeed, iopdQtyInOut).
    RUN pSetAttributeFromStandard(ipbf-ttOperation.company,  giAttributeIDEstQtyPerFG, iopdQtyInOut * ipbf-ttOperation.FGCumulativeNumOut ).
    RUN pSetAttributeFromStandard(ipbf-ttOperation.company,  giAttributeIDEstSheets, STRING(fGetOperationsEstSheet(BUFFER ipbf-eb, ipbf-ttOperation.OperationId, ipbf-ttOperation.pass))). 
    
    /* Now calculate spoilage for this pass based upon Qty set above */
    RUN pGetRunSpoil(BUFFER bf-mach, OUTPUT ipbf-ttOperation.quantityInRunWastePercent, OUTPUT lError, OUTPUT cMessage).
    RUN pGetMRWaste (BUFFER bf-mach, OUTPUT ipbf-ttOperation.quantityInSetupWaste, OUTPUT lError, OUTPUT cMessage).
    
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
    
    /* Set Quantity and dependent attributes for each Line. These will be used to get Spoilage value */   
    RUN pSetAttribute(giAttributeIDEstQtyPerFeed, gcAttributeNameEstQtyPerFeed, ipbf-ttOperation.quantityIn).  
    RUN pSetAttributeFromStandard(ipbf-ttOperation.company,  giAttributeIDEstQtyPerFG, ipbf-ttOperation.quantityIn * ipbf-ttOperation.FGCumulativeNumOut).  
    RUN pSetAttributeFromStandard(ipbf-ttOperation.company,  giAttributeIDEstSheets, STRING(fGetOperationsEstSheet(BUFFER ipbf-eb, ipbf-ttOperation.OperationId, ipbf-ttOperation.pass))). 
    
    /* Calculate Run parameters based upon QtyIn calculated for this machine */
    RUN pGetMRHours(BUFFER bf-mach, OUTPUT ipbf-ttOperation.hoursSetup, OUTPUT lError, OUTPUT cMessage).
    RUN pGetRunSpeed(BUFFER bf-mach, OUTPUT ipbf-ttOperation.speed, OUTPUT lError, OUTPUT cMessage).
    
END PROCEDURE.

PROCEDURE Operations_ProcessOperationChange:
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
    
    RUN pSetProcessScope(INPUT "MainProcessStart").
                             
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
    
    RUN pSetProcessScope(INPUT "MainProcessEnd").
                          
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
          
     iColors = fGetOperationsColor(BUFFER ipbf-eb,ipcLocationID,ipcOperationID, ipcDeptt, ipiPass).
            
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
    RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDBlankSFSI, STRING(fGetBlankSqFTorSqINArea(BUFFER ipbf-eb))).  //eb.t-sqFt
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
    RUN pSetAttribute(giAttributeIDCaliperDecimal, "Caliper Thickness (in Decimal)",STRING(ipbf-ef.cal)).
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

PROCEDURE pSetAttributesFromQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given the ttEstOP and ttOperations, set attributes that are Operations Qty dependent
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT  PARAMETER ipcOperationId AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationId AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER TABLE FOR ttOperation.
    
    DEFINE VARIABLE dCalcQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCumulNumOut AS INTEGER NO-UNDO.
    
    dCalcQty = fGetOperationsQty(BUFFER ipbf-eb, ipcOperationId, ipcLocationId, ipiPass, OUTPUT iCumulNumOut).
    
    IF dCalcQty NE 0 THEN
    DO:
        RUN pSetAttribute(giAttributeIDEstQtyPerFeed, gcAttributeNameEstQtyPerFeed, STRING(dCalcQty)).  
        RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDEstQtyPerFG, STRING(dCalcQty * iCumulNumOut)).  
        RUN pSetAttributeFromStandard(ipbf-eb.company,  giAttributeIDEstSheets, STRING(fGetOperationsEstSheet(BUFFER ipbf-eb, ipcOperationId, ipiPass))).
    END. 
        
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
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEPrice", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN glApplyOperationMinimumCharge = cReturn EQ "YES".
    
    IF lFound AND glApplyOperationMinimumCharge THEN 
    DO: 
        RUN sys/ref/nk1look.p (ipcCompany, "CEPrice", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
        IF lFound THEN glApplyOperationMinimumChargeRunOnly = cReturn EQ "RunOnly".
    END.
    
END PROCEDURE.

PROCEDURE pGetEstOPDataFromEstimate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds applicable Est-OP data into temp-table
     Notes: No need of recalc, just copy existing est-op into TT
    ------------------------------------------------------------------------------*/ 
    
    DEFINE PARAMETER BUFFER ipbf-ef      FOR ef.
    DEFINE INPUT  PARAMETER ipdTargetQty        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER                     TABLE FOR ttEstOp.
    
    DEFINE BUFFER bf-est-op FOR est-op.
    
    /* Create Data into TT */
    FOR EACH bf-est-op NO-LOCK 
        WHERE bf-est-op.company EQ ipbf-ef.company
        AND bf-est-op.est-no EQ ipbf-ef.est-no
        AND bf-est-op.s-num EQ ipbf-ef.form-no
        AND bf-est-op.qty   EQ ipdTargetQty :
    
        CREATE ttEstOp.
        BUFFER-COPY bf-est-op TO ttEstOp.
       
    END.  //FOR EACH bf-est-op
    
END PROCEDURE.    

PROCEDURE pGetEstOPDataFromJobMch PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds applicable Est-OP data into temp-table
     Notes: If action is Add/Replace Machine then we'll use Est-op for existing machine
    ------------------------------------------------------------------------------*/ 
    
    DEFINE PARAMETER BUFFER ipbf-ef      FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb      FOR eb.
    DEFINE PARAMETER BUFFER ipbf-job-mch FOR job-mch.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAction           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOriginalMachine  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTargetQty        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER                     TABLE FOR ttEstOp.
    
    DEFINE VARIABLE cError   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-est-op FOR est-op.
    DEFINE BUFFER bf-mach   FOR mach.
    DEFINE BUFFER bf-est    FOR est.
    
    
    /* Create Data into TT */
    FOR EACH bf-est-op NO-LOCK 
        WHERE bf-est-op.company EQ ipbf-ef.company
        AND bf-est-op.est-no EQ ipbf-ef.est-no
        AND bf-est-op.s-num EQ ipbf-ef.form-no
        AND bf-est-op.qty   EQ ipdTargetQty :
    
        CREATE ttEstOp.
        BUFFER-COPY bf-est-op TO ttEstOp.
        
        IF (ipcAction EQ "Add" OR ipcAction EQ "Replace") AND bf-est-op.m-code = ipcOriginalMachine THEN
        DO:
            RUN pGetMachineBuffer(ipbf-ef.company, ipcLocation, ipbf-job-mch.m-code, BUFFER bf-mach, OUTPUT cError, OUTPUT cMessage).
            ASSIGN
                ttEstOp.m-code     = bf-mach.m-code
                ttEstOp.m-dscr     = bf-mach.m-dscr
                ttEstOp.dept       = ipbf-job-mch.dept
                ttEstOp.machSeq    = bf-mach.m-seq
                ttEstOp.op-spoil   = bf-mach.run-spoil
                ttEstOp.op-crew[1] = bf-mach.mr-crusiz
                ttEstOp.op-crew[2] = bf-mach.run-crusiz
                ttEstOp.op-rate[1] = (bf-mach.lab-rate[bf-mach.lab-drate] * ttEstOp.op-crew[1]) + bf-mach.mr-varoh  + bf-mach.mr-fixoh
                ttEstOp.op-rate[2] = (bf-mach.lab-rate[bf-mach.lab-drate] * ttEstOp.op-crew[2]) + bf-mach.mr-varoh  + bf-mach.mr-fixoh
                .
                
             
            RUN pGetEstOpCalcFields (BUFFER bf-mach, BUFFER ttEstOp).
        END.
      
    END.  //FOR EACH bf-est-op
    
    IF ipcAction = "AddDept" THEN
    DO:
        RUN pGetMachineBuffer(ipbf-ef.company, ipcLocation, ipbf-job-mch.m-code, BUFFER bf-mach, OUTPUT cError, OUTPUT cMessage).
    
        FIND FIRST bf-est NO-LOCK
            WHERE bf-est.company = ipbf-ef.company
            AND bf-est.est-no = ipbf-ef.est-no NO-ERROR.
          
        IF AVAILABLE bf-est AND AVAILABLE bf-mach THEN 
        DO:
            CREATE ttEstOp.
            ASSIGN
                ttEstOp.company    = bf-est.company
                ttEstOp.e-num      = bf-est.e-num
                ttEstOp.est-no     = bf-est.est-no
                ttEstOp.qty        = IF bf-est.est-type EQ 8 THEN 0 ELSE ipdTargetQty
                ttEstOp.s-num      = ipbf-job-mch.frm
                ttEstOp.b-num      = IF bf-est.est-type EQ 5 THEN 1 ELSE ipbf-job-mch.blank-no
                ttEstOp.op-pass    = ipbf-job-mch.pass
                ttEstOp.op-sb      = YES
                ttEstOp.m-code     = bf-mach.m-code
                ttEstOp.m-dscr     = bf-mach.m-dscr
                ttEstOp.dept       = ipbf-job-mch.dept
                ttEstOp.d-seq      = bf-mach.d-seq
                ttEstOp.n-out      = ipbf-ef.n-out
                ttEstOp.op-spoil   = bf-mach.run-spoil
                ttEstOp.op-crew[1] = bf-mach.mr-crusiz
                ttEstOp.op-crew[2] = bf-mach.run-crusiz
                .
            RUN pGetEstOpCalcFields (BUFFER bf-mach, BUFFER ttEstOp).
        END.
    END.
    
    RUN pGetOpsSequencing( INPUT-OUTPUT TABLE ttEstOp BY-REFERENCE).
    
END PROCEDURE.    

PROCEDURE pGetOpsSequencing PRIVATE:
    DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR ttEstOp.
    
    DEFINE VARIABLE iSeqNo  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLineNo AS INTEGER NO-UNDO.
    
    FOR EACH ttEstOp
        BREAK BY ttEstOp.s-num
        BY ttEstOp.b-num
        BY ttEstOp.d-seq
        BY ttEstOp.machSeq:
            
        IF FIRST-OF(ttEstOp.d-seq) THEN 
            iSeqNo = 0.
    
        ASSIGN
            iSeqNo          = iSeqNo + 1
            ttEstOp.op-pass = iSeqNo
            iLineNo         = iLineNo + 1
            ttEstOp.line    = iLineNo.
    END.
END PROCEDURE.      

PROCEDURE pGetEstOpCalcFields PRIVATE:
     /*------------------------------------------------------------------------------
     Purpose: Return the calculated fields of EstOP
     Notes: 
    ------------------------------------------------------------------------------*/
    
    DEFINE PARAMETER BUFFER ipbf-Mach FOR mach.
    DEFINE PARAMETER BUFFER ipbf-ttEstOP FOR ttEstOp.
    
    DEFINE VARIABLE lError   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN pGetMRWaste(BUFFER ipbf-Mach, OUTPUT ipbf-ttEstOP.op-waste, OUTPUT lError, OUTPUT cMessage).
      
END.            
           

PROCEDURE pRecalcOperations PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: build all estimate Operations
    Notes: Firstly get applicable est-ops and then recalculate the Operations TT
   ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb      FOR eb.
    DEFINE INPUT PARAMETER ipriJobMch AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipriEstop  AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAction AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOrgMachine AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE   FOR ttOperation.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQtyFormsRequiredForBlanks    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyFormsRequiredForBlanksMax AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOut                     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutRunWaste             AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutSetupWaste           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyTarget                    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyFGOnFormYielded           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyRequired                  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cEstimateType                 AS CHARACTER NO-UNDO.
    
    
    DEFINE BUFFER bf-est         FOR est.
    DEFINE BUFFER bf-eb          FOR eb.
    DEFINE BUFFER bf-ef          FOR ef.
    DEFINE BUFFER bf-ttOperation FOR ttOperation.
    
    EMPTY TEMP-TABLE ttOperation.
    EMPTY TEMP-TABLE ttEstop.
    
    /* Set Recalc Scope */
    RUN pSetProcessScope(INPUT "RecalcStart").
    
    FIND FIRST bf-ef NO-LOCK
        OF ipbf-eb NO-ERROR.
    
    IF NOT AVAILABLE bf-ef THEN
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "EF Record not found"
            .
        RETURN.
    END.
    
    FIND FIRST bf-est NO-LOCK
        WHERE bf-est.company = bf-ef.company
        AND bf-est.est-no = bf-ef.est-no NO-ERROR.
    
    IF NOT AVAILABLE bf-est THEN
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Est Record not found"
            .
        RETURN.
    END.
    
    cEstimateType = fGetEstimateType (INPUT bf-est.est-type, INPUT bf-est.estimateTypeID).
    
    IF ipriJobMch <> ? THEN
        RUN pCollectDataForJobMch(BUFFER ipbf-eb, BUFFER bf-ef, ipriJobMch, cEstimateType, ipcAction,ipcOrgMachine, OUTPUT dQtyTarget, OUTPUT dQtyRequired, OUTPUT dQtyFGOnFormYielded, OUTPUT TABLE ttEstOp, OUTPUT oplError, OUTPUT opcMessage).
        
    ELSE
        RUN pCollectDataForEstimate(BUFFER ipbf-eb, BUFFER bf-ef, ipriEstop, cEstimateType, OUTPUT dQtyTarget, OUTPUT dQtyRequired, OUTPUT dQtyFGOnFormYielded, OUTPUT TABLE ttEstOp, OUTPUT oplError, OUTPUT opcMessage).
    
    
    EMPTY TEMP-TABLE ttEstBlank.
    
    FOR EACH bf-eb NO-LOCK
        WHERE bf-eb.company eq bf-ef.company
        AND bf-eb.est-no eq bf-ef.est-no
        AND bf-eb.form-no eq bf-ef.form-no:
            
            
        IF NOT CAN-FIND (FIRST ttEstBlank
            WHERE ttEstBlank.blankID EQ bf-eb.blank-no) THEN
        DO:
            CREATE ttEstBlank.
            ASSIGN 
                ttEstBlank.BlankID   = bf-eb.blank-no
                ttEstBlank.FormID    = bf-eb.form-no
                ttEstBlank.iOut      = MAX(bf-eb.num-wid, 1) * MAX(bf-eb.num-len, 1) * MAX(bf-eb.num-dep, 1)
                ttEstBlank.dQtyInOut = dQtyRequired
                .
        END.
        
    END.
    
    giTotalOut = 1.
    
    /*Process each est-op for the right quantity*/
    FOR EACH ttEstop 
        WHERE ttEstop.company EQ bf-ef.company
        AND ttEstop.est-no EQ bf-ef.est-no
        AND ttEstop.s-num EQ bf-ef.form-no
        AND ttEstop.line LT 500
        AND ttEstop.qty EQ dQtyTarget
        BY ttEstop.line DESCENDING:
            
        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company = bf-ef.company
            AND bf-eb.est-no = bf-ef.est-no
            AND bf-eb.form-no = bf-ef.form-no
            AND bf-eb.blank-no = MAX(ttEstop.b-num,1) NO-ERROR.
        
        IF NOT AVAILABLE bf-eb THEN
            RETURN.

        RUN pAddOperationFromEstOp(BUFFER ttEstop, BUFFER bf-est, BUFFER bf-ef, BUFFER bf-ttOperation, cEstimateType).
                        
        IF AVAILABLE bf-ttOperation THEN 
        DO:

            /*REFACTOR to calculate quantities for combos*/        
            IF ttEstop.b-num NE 0 AND bf-ttOperation.feedType EQ "B" THEN
            DO:  /*Calculate for Combo*/
            
                FIND FIRST ttEstBlank
                    WHERE ttEstBlank.FormID  EQ ttEstop.s-num
                    AND ttEstBlank.BlankID EQ ttEstop.b-num
                    NO-ERROR.
                IF AVAILABLE ttEstBlank and NOT ttEstBlank.lOutputInitialized THEN 
                    ASSIGN 
                        ttEstBlank.lOutputInitialized = YES
                        .
                RUN pProcessOperation(BUFFER bf-ttOperation, BUFFER bf-eb, INPUT-OUTPUT ttEstBlank.dQtyInOut, 
                    INPUT-OUTPUT ttEstBlank.dQtyInOutSetupWaste, INPUT-OUTPUT ttEstBlank.dQtyInOutRunWaste).
            END. /*BlankNo not 0*/
            ELSE 
            DO:  
                IF bf-ttOperation.isBlankMaker THEN 
                DO:
                    /*Find the most forms required to support each blank operations*/
                    FOR EACH ttEstBlank NO-LOCK 
                        WHERE ttEstBlank.FormID EQ ttEstop.s-num:
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
                    dQtyInOut = dQtyFGOnFormYielded.
            
                RUN pProcessOperation(BUFFER bf-ttOperation, BUFFER bf-eb, INPUT-OUTPUT dQtyInOut, 
                    INPUT-OUTPUT dQtyInOutSetupWaste, INPUT-OUTPUT dQtyInOutRunWaste).
                
            END.
        
            /* Copy calculated values on Est OP for any reference purpose */
            ASSIGN
                ttEstop.op-mr    = bf-ttOperation.hoursSetup                   
                ttEstop.op-speed = bf-ttOperation.speed
                ttEstop.op-spoil = bf-ttOperation.quantityInRunWastePercent
                .  
            
            RUN pCalcEstOperation(BUFFER bf-ttOperation).                    
        END.
                    
    END. /*Each ttEstop*/
    RUN pSetProcessScope(INPUT "RecalcEnd").

     
END PROCEDURE.


PROCEDURE pCalcEstOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a estCostOperation buffer, calculate simple calculated fields
     Notes: Should replace end if pr4-mch.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOperation FOR ttOperation.
    
    DEFINE VARIABLE dCostMinimumDiff       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostMinimumDiffFactor AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostMinimumDiffSetup  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostMinimumDiffRun    AS DECIMAL NO-UNDO.
    
        
    IF ipbf-ttOperation.speed NE 0 THEN
        IF ipbf-ttOperation.isSpeedInLF THEN
            ipbf-ttOperation.hoursRun = ipbf-ttOperation.quantityInAfterSetupWasteLF / ipbf-ttOperation.speed. 
        ELSE 
            ipbf-ttOperation.hoursRun = ipbf-ttOperation.quantityInAfterSetupWaste / ipbf-ttOperation.speed.
    ELSE 
        ipbf-ttOperation.hoursRun = 0.
    
    IF ipbf-ttOperation.numOutDivisor GT 0 THEN
        ipbf-ttOperation.hoursRun = ipbf-ttOperation.hoursRun / ipbf-ttOperation.numOutDivisor.
        
    IF ipbf-ttOperation.hoursRun LT ipbf-ttOperation.hoursRunMinimum THEN 
        ipbf-ttOperation.hoursRun = ipbf-ttOperation.hoursRunMinimum.
    
    ASSIGN    
        ipbf-ttOperation.costPerHourTotalRun   = ipbf-ttOperation.costPerManHourDLRun * ipbf-ttOperation.crewSizeRun + 
                                                     ipbf-ttOperation.costPerHourFORun + ipbf-ttOperation.costPerHourVORun
        ipbf-ttOperation.costPerHourTotalSetup = ipbf-ttOperation.costPerManHourDLSetup * ipbf-ttOperation.crewSizeSetup + 
                                                     ipbf-ttOperation.costPerHourFOSetup + ipbf-ttOperation.costPerHourVOSetup
        ipbf-ttOperation.costTotalDLSetup      = ipbf-ttOperation.hoursSetup * ipbf-ttOperation.crewSizeSetup * ipbf-ttOperation.costPerManHourDLSetup
        ipbf-ttOperation.costTotalVOSetup      = ipbf-ttOperation.hoursSetup * ipbf-ttOperation.costPerHourVOSetup
        ipbf-ttOperation.costTotalFOSetup      = ipbf-ttOperation.hoursSetup * ipbf-ttOperation.costPerHourFOSetup
        ipbf-ttOperation.costTotalDLRun        = ipbf-ttOperation.hoursRun * ipbf-ttOperation.crewSizeRun * ipbf-ttOperation.costPerManHourDLRun
        ipbf-ttOperation.costTotalVORun        = ipbf-ttOperation.hoursRun * ipbf-ttOperation.costPerHourVORun
        ipbf-ttOperation.costTotalFORun        = ipbf-ttOperation.hoursRun * ipbf-ttOperation.costPerHourFORun
        ipbf-ttOperation.costTotalSetup        = ipbf-ttOperation.costTotalDLSetup + ipbf-ttOperation.costTotalVOSetup + ipbf-ttOperation.costTotalFOSetup
        ipbf-ttOperation.costTotalRun          = ipbf-ttOperation.costTotalDLRun + ipbf-ttOperation.costTotalVORun + ipbf-ttOperation.costTotalFORun
        ipbf-ttOperation.costTotal             = ipbf-ttOperation.costTotalRun + ipbf-ttOperation.costTotalSetup
        .
    
    /*Apply minimum Charge*/
    IF glApplyOperationMinimumCharge AND ipbf-ttOperation.costTotal GT 0 THEN 
    DO:
        IF glApplyOperationMinimumChargeRunOnly THEN 
        DO:
            IF ipbf-ttOperation.costMinimum GT ipbf-ttOperation.costTotalRun THEN 
                ASSIGN 
                    dCostMinimumDiffRun = ipbf-ttOperation.costMinimum - ipbf-ttOperation.costTotalRun.
        END.
        ELSE 
        DO:
            IF ipbf-ttOperation.costMinimum GT ipbf-ttOperation.costTotal THEN 
                ASSIGN 
                    dCostMinimumDiff       = ipbf-ttOperation.costMinimum - ipbf-ttOperation.costTotal
                    dCostMinimumDiffFactor = ipbf-ttOperation.costTotalSetup / ipbf-ttOperation.costTotal
                    dCostMinimumDiffSetup  = dCostMinimumDiff * dCostMinimumDiffFactor
                    dCostMinimumDiffRun    = dCostMinimumDiff - dCostMinimumDiffSetup
                    .
        END.
        ASSIGN 
            ipbf-ttOperation.costTotalDlRun   = ipbf-ttOperation.costTotalDLRun + dCostMinimumDiffRun
            ipbf-ttOperation.costTotalDlSetup = ipbf-ttOperation.costTotalDLSetup + dCostMinimumDiffSetup
            ipbf-ttOperation.costTotalSetup   = ipbf-ttOperation.costTotalDLSetup + ipbf-ttOperation.costTotalVOSetup + ipbf-ttOperation.costTotalFOSetup
            ipbf-ttOperation.costTotalRun     = ipbf-ttOperation.costTotalDLRun + ipbf-ttOperation.costTotalVORun + ipbf-ttOperation.costTotalFORun
            ipbf-ttOperation.costTotal        = ipbf-ttOperation.costTotalRun + ipbf-ttOperation.costTotalSetup
            .
                    
    END. 
                        
END PROCEDURE.

PROCEDURE Operations_GetEstimateOperationTT:
   
   DEFINE OUTPUT PARAMETER TABLE FOR ttOperation.
   
END.

PROCEDURE Operations_GetEstimateRoutingTT:
   
   DEFINE OUTPUT PARAMETER TABLE FOR ttRouting.
   
END.

PROCEDURE pSetProcessScope PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcProc AS CHARACTER NO-UNDO.
    
     glTagDisabled = IF ipcProc EQ "RecalcStart" THEN TRUE ELSE FALSE.

    IF ipcProc EQ "MainProcessStart" THEN 
        RUN pResetObjects(INPUT "Before").
    ELSE IF ipcProc EQ "MainProcessEnd" THEN
         RUN pResetObjects(INPUT "After").  

END PROCEDURE.


PROCEDURE Operations_SetAttributes:
    /*------------------------------------------------------------------------------
     Purpose:  Sets Attributes for ttAttribute Table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttAttribute.
    
END PROCEDURE.

PROCEDURE Operations_SetAttributesFromEstOp:
    /*------------------------------------------------------------------------------
     Purpose: Given a rowid (for est-op), build out the attributes required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriRowid       AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdProbeQty     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError        AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.

    RUN pSetAttributesFromEstOp(ipriRowid, ipcLocation, ipdProbeQty, OUTPUT oplError, OUTPUT opcMessage).
    
END PROCEDURE.    

PROCEDURE pSetAttributesFromEstOp PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a rowid (for est-op), build out the attributes required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriRowid       AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdProbeQty     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError        AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cMsg   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-eb      FOR eb. 
    DEFINE BUFFER bf-ef      FOR ef.
    DEFINE BUFFER bf-est-op  FOR est-op.
    
    FIND FIRST bf-est-op NO-LOCK
        WHERE ROWID(bf-est-op) = ipriRowid NO-ERROR.
    
    IF NOT AVAILABLE bf-est-op THEN
        RETURN.
                
    FIND FIRST bf-eb NO-LOCK 
        WHERE bf-eb.company  EQ bf-est-op.company
        AND bf-eb.est-no   EQ bf-est-op.est-no
        AND bf-eb.form-no  EQ MAX(bf-est-op.s-num,1)
        AND bf-eb.blank-no EQ MAX(bf-est-op.b-num,1) NO-ERROR.
        
    IF AVAILABLE bf-eb THEN 
    DO:
        RUN pSetAttributesBlank(BUFFER bf-eb).
        RUN pSetAttributeForColors(BUFFER bf-eb, ipcLocation, bf-est-op.m-code, bf-est-op.dept ,bf-est-op.op-pass).  //Maxco
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDDieNumberUp, STRING(fGetDieNumberUp(BUFFER bf-eb,bf-est-op.m-code))). //v-up  
        
        IF ipdProbeQty NE 0 THEN
        DO:
            RUN pSetAttribute(giAttributeIDEstQtyPerFeed, gcAttributeNameEstQtyPerFeed, ipdProbeQty).  //qty
            RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDEstQtyPerFG, ipdProbeQty).  //qty
        END.
        ELSE
        DO:
            RUN pSetAttribute(giAttributeIDEstQtyPerFeed,gcAttributeNameEstQtyPerFeed, bf-est-op.qty).  //qty
            RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDEstQtyPerFG, bf-est-op.qty).  //qty
        END.
           
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDEstSheets, STRING(fGetOperationsEstSheet(BUFFER bf-eb, bf-est-op.m-code, bf-est-op.op-pass))). //(qty * v-yld / xeb.num-up / v-n-out)   not found
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDNoOutGShtWid, STRING(fGetOperationsGrsShtWid(BUFFER bf-eb, bf-est-op.m-code, bf-est-op.op-pass))). //v-out
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDSetPartsperForm, STRING(fGetOperationsPartPerSet(BUFFER bf-eb,2,""))). //ld-parts[2]
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDInkCoverage, STRING(fGetOperationsInkCoverage(BUFFER bf-eb))). //ld-ink-frm
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDPartsperSet, STRING(fGetOperationsPartPerSet(BUFFER bf-eb,1,""))). //ld-parts[1]
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDQtySetLongs, STRING(fGetOperationsPartPerSet(BUFFER bf-eb,1,"long"))). //v-long-qty-set
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDQtySetShorts, STRING(fGetOperationsPartPerSet(BUFFER bf-eb,1,"short"))). //v-short-qty-set                
                
        FIND FIRST bf-ef OF bf-eb NO-LOCK.
        IF AVAILABLE bf-ef THEN 
            RUN pSetAttributesForm(BUFFER bf-ef).

        /* Re-build the Est-op and Operations Data */ 
        RUN pRecalcOperations(BUFFER bf-eb,?,ipriRowid, "NoChange", "", OUTPUT TABLE ttOperation, OUTPUT oplError, OUTPUT cMsg).                
                
        /* Calculate Attribute from Operations */
        IF NOT oplError THEN
            RUN pSetAttributesFromQty (BUFFER bf-eb, bf-est-op.m-code, ipcLocation , bf-est-op.op-pass, INPUT TABLE ttOperation BY-REFERENCE).
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Blank for estimate " + bf-est-op.est-no + " Form/Blank " + STRING(bf-est-op.s-num) + "/" + STRING(bf-est-op.b-num)
            .
    
END PROCEDURE.


PROCEDURE pSetAttributesFromJobMch PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a rowid (for eb), build out the attributes required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMchCode AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipipass AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAction      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcExistingOps AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iForm  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBlank AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPass  AS INTEGER NO-UNDO.
    DEFINE VARIABLE cMsg   AS CHARACTER NO-UNDO.
    
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
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDEstQtyPerFeed, STRING(fGetOriginalQtyfromEst(BUFFER bf-eb, BUFFER bf-job))).  //qty
        RUN pSetAttributeFromStandard(bf-eb.company,  giAttributeIDEstQtyPerFG, STRING(fGetOriginalQtyfromEst(BUFFER bf-eb, BUFFER bf-job))).  //qty
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
        
        /* Re-build the Est-op and Operations Data */ 
        RUN pRecalcOperations(BUFFER bf-eb, ROWID(bf-job-mch), ?, ipcAction, ipcExistingOps, OUTPUT TABLE ttOperation, OUTPUT oplError, OUTPUT cMsg).
         
        /* Calculate Attribute from Operations */
        IF NOT oplError THEN
            RUN pSetAttributesFromQty (BUFFER bf-eb, bf-job-mch.m-code, bf-job.loc, iPass, INPUT TABLE ttOperation BY-REFERENCE).
         
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


FUNCTION fGetCumulNumOutForOperation RETURNS INTEGER PRIVATE
    (INPUT ipiOut AS INTEGER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE iTempOut AS INTEGER NO-UNDO.
        
    ASSIGN
        giTotalOut = giTotalOut * ipiOut
        iTempOut   = giTotalOut.
       
    RETURN iTempOut.
        
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

FUNCTION fGetEstIndustry RETURNS CHARACTER PRIVATE
	(INPUT ipcEstimateNo AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-est FOR est.
    
    FIND FIRST bf-est NO-LOCK
        WHERE bf-est.est-no = ipcEstimateNo No-ERROR.
        
    IF AVAILABLE bf-est THEN
        cReturn = IF bf-est.est-type LE 4 THEN gcIndustryFolding ELSE gcIndustryCorrugated.
    
    RETURN cReturn.
		
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
    IF AVAILABLE bf-item AND DYNAMIC-FUNCTION("fEstimate_IsInk", bf-item.mat-type, bf-item.ink-type)
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
    ipcLocationID AS CHARACTER,
    ipcOperationID AS CHARACTER,
    ipcDeptt AS CHARACTER,
    ipiPass AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Given an estimate blank buffer and pass, set the 
        # of colors attribute.
     Notes: If Blank NO specific then return colors for that Blank otherwsie for whole Form
            If Department is Press then count for specific Pass number Otherwise All
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iReturnColors AS INTEGER.
    DEFINE VARIABLE cFeedType     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.        
            
    DEFINE BUFFER bf-eb   FOR eb.
    DEFINE BUFFER bf-ef   FOR ef.
    DEFINE BUFFER bf-mach FOR mach.
    
    RUN pGetMachineBuffer(ipbf-eb.company, ipcLocationID, ipcOperationID, BUFFER bf-mach, OUTPUT cError, OUTPUT cMessage).
    
    IF AVAILABLE bf-mach THEN
        cFeedType = bf-mach.p-type. 
    
    /* For Printers; get colors for specific Blank and Specific Pass */
    IF ipcDeptt = "PR" THEN
        iReturnColors = fGetColorsForBlankPass(BUFFER ipbf-eb, ipiPass).
    
    /* For a Blank Fed machine; get Colors for a specific BlankNo, Passes-All */
    ELSE IF cFeedType EQ "B" THEN
            iReturnColors = fGetColorsForBlankPass(BUFFER ipbf-eb, 0).
        
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
                
            iReturnColors = iReturnColors + fGetColorsForBlankPass(BUFFER bf-eb, 0).
        END.
    END.
        
    RETURN iReturnColors.
END.


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

FUNCTION fGetOriginalQtyfromEst RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    BUFFER ipbf-job FOR job):
    /*------------------------------------------------------------------------------
    Purpose: Set Quantity from Estimate. 
    Notes: It don't consider Spoilage while calculating from Estimate
    ------------------------------------------------------------------------------*/    
    
    DEFINE VARIABLE dReturnValue  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyRequired  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyTarget    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cEstimateType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-est FOR est.
        
    FIND FIRST bf-est NO-LOCK
        WHERE bf-est.company = ipbf-eb.company
        AND bf-est.est-no = ipbf-eb.est-no NO-ERROR.
    
    IF AVAILABLE bf-est THEN
        cEstimateType = fGetEstimateType (INPUT bf-est.est-type, INPUT bf-est.estimateTypeID).
        
       dQtyRequired = fGetRequiredQtyUsingJob (BUFFER ipbf-eb, BUFFER ipbf-job, INPUT cEstimateType, INPUT "").
        
    RUN pGetEffectiveEstOpQuantity (ipbf-eb.company, ipbf-eb.est-no, dQtyRequired, OUTPUT dQtyTarget).
    
    IF dQtyTarget NE 0 THEN 
        dReturnValue = dQtyTarget.
    ELSE
        dReturnValue = dQtyRequired.
     
    RETURN dReturnValue.
END.

FUNCTION fGetOperationsQty RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb,
    ipcMachine AS CHARACTER,
    ipcLocationID AS CHARACTER,
    ipiPass AS INTEGER,
    OUTPUT opiCumNumOut AS INTEGER):
    /*------------------------------------------------------------------------------
    Purpose: 
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dReturnValue AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumUp       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dNumOutCal   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cError       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQtyCalc     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpMRWaste   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpRunSpoil  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSpoilDeduct AS DECIMAL   NO-UNDO INITIAL 1.   
         
    DEFINE BUFFER bf-mach    FOR mach.
    DEFINE BUFFER bf-ttEstOp        FOR ttEstOp.
    DEFINE BUFFER bf-ttOperation    FOR ttOperation.
    
    /* get Quantity from Operations Data*/
    FIND FIRST bf-ttOperation
        WHERE bf-ttOperation.company    EQ ipbf-eb.company
        AND bf-ttOperation.estimateNo   EQ ipbf-eb.est-no
        AND bf-ttOperation.operationID  EQ ipcMachine
        AND bf-ttOperation.formNo       EQ ipbf-eb.form-no
        AND bf-ttOperation.blankNo      EQ MAX(ipbf-eb.blank-no, 1)
        AND bf-ttOperation.pass         EQ MAX(ipiPass , 1) NO-ERROR.
            
    IF NOT AVAILABLE bf-ttOperation THEN
        FIND FIRST bf-ttOperation
            WHERE bf-ttOperation.company    EQ ipbf-eb.company
            AND bf-ttOperation.estimateNo   EQ ipbf-eb.est-no
            AND bf-ttOperation.operationID  EQ ipcMachine
            AND bf-ttOperation.formNo       EQ ipbf-eb.form-no
            AND bf-ttOperation.pass         EQ MAX(ipiPass , 1) NO-ERROR. 
            
    IF NOT AVAILABLE bf-ttOperation THEN
        FIND FIRST bf-ttOperation
            WHERE bf-ttOperation.company    EQ ipbf-eb.company
            AND bf-ttOperation.estimateNo   EQ ipbf-eb.est-no
            AND bf-ttOperation.operationID  EQ ipcMachine
            AND bf-ttOperation.formNo       EQ ipbf-eb.form-no NO-ERROR.
    
    IF AVAILABLE bf-ttOperation THEN     
        ASSIGN
            dReturnValue = bf-ttOperation.quantityIn
            opiCumNumOut = bf-ttOperation.FGCumulativeNumOut. 
        
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
          
    IF AVAILABLE mach THEN
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
    DEFINE VARIABLE iYldQty       AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNOut         AS INTEGER NO-UNDO.
    DEFINE VARIABLE cQtyValue     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cAttrName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantityPerSet AS DECIMAL NO-UNDO.
    
    RUN pGetAttribute(giAttributeIDEstQtyPerFeed, OUTPUT cQtyValue, OUTPUT cAttrName, OUTPUT cError, OUTPUT cMessage). //Get colors attribute
    ASSIGN dOperationQty = DECIMAL(cQtyValue) NO-ERROR.
    
    FIND FIRST mach NO-LOCK
        WHERE mach.company EQ ipbf-eb.company
        AND mach.m-code EQ ipcMachine NO-ERROR.
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ ipbf-eb.company
        AND ef.est-no EQ ipbf-eb.est-no
        AND ef.form-no EQ ipbf-eb.form-no NO-ERROR.     
       
    IF AVAILABLE mach THEN
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
            dQuantityPerSet = ipbf-eb.quantityPerSet.
            IF dQuantityPerSet EQ 0 THEN
                dQuantityPerSet = DYNAMIC-FUNCTION("fEstimate_GetQuantityPerSet", BUFFER ipbf-eb).
            
            RUN est/ef-#out.p (ROWID(ef), OUTPUT iNOut).
            IF ipbf-eb.est-type EQ 8 THEN
                iYldQty =  1.
            ELSE
                iYldQty = IF dQuantityPerSet GE 0 THEN dQuantityPerSet ELSE (-1 / dQuantityPerSet) .
                
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
    
    
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ ipbf-eb.company
        AND ef.est-no EQ ipbf-eb.est-no
        AND ef.form-no EQ ipbf-eb.form-no NO-ERROR.
        
    IF AVAILABLE ef THEN
        iOut = ef.n-out.
        
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
            IF AVAILABLE item THEN
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
        
        dParts = dParts + fGetQuantityPerSet(BUFFER bf-eb).
    END.
    
    RETURN dParts.

       
END FUNCTION.

FUNCTION fGetRequiredQtyUsingEstOp RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb, INPUT ipdQty AS DECIMAL, INPUT ipcEstType AS CHARACTER, INPUT ipcQtyType AS CHARACTER):
        
    DEFINE VARIABLE dReturnValue AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyPerSet   AS DECIMAL NO-UNDO.
    
    dQtyPerSet = fGetQuantityPerSet(BUFFER ipbf-eb).
    
    IF fIsComboType(ipcEstType) THEN
    DO: 
        IF ipcQtyType EQ "" THEN
            dReturnValue  = ipbf-eb.bl-qty * dQtyPerSet.
        ELSE
            dReturnValue  = ipbf-eb.yld-qt * dQtyPerSet.
    END.
    ELSE
        dReturnValue = ipdQty * dQtyPerSet.
    
    RETURN dReturnValue.
END.   

FUNCTION fGetRequiredQtyUsingJob RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb, BUFFER ipbf-job FOR job, INPUT ipcEstType AS CHARACTER, INPUT ipcQtyType AS CHARACTER):
        
    DEFINE VARIABLE dReturnValue AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyPerSet   AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    
    dQtyPerSet = fGetQuantityPerSet(BUFFER ipbf-eb).
    
    IF fIsComboType(ipcEstType) THEN
    DO: 
        IF ipcQtyType EQ "" THEN
            dReturnValue  = ipbf-eb.bl-qty * dQtyPerSet.
        ELSE
            dReturnValue  = ipbf-eb.yld-qt * dQtyPerSet.
    END.
    ELSE
    DO:
        FIND FIRST bf-job-hdr NO-LOCK 
            WHERE bf-job-hdr.company EQ ipbf-job.company
            AND bf-job-hdr.job EQ ipbf-job.job
            AND bf-job-hdr.job-no EQ ipbf-job.job-no
            AND bf-job-hdr.job-no2 EQ ipbf-job.job-no2
            NO-ERROR.
            
        IF AVAILABLE bf-job-hdr THEN
            dReturnValue = bf-job-hdr.qty * dQtyPerSet.
    END. 
    
    RETURN dReturnValue.
END.   

FUNCTION fGetJobMachRunQty RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb, ipcMachCode AS CHARACTER, ipiPass AS INTEGER, ipdRunQty AS DECIMAL):
    /*------------------------------------------------------------------------------
    Purpose: 
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dReturnValue AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyEach     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cMsgTyp      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessageStr  AS CHARACTER NO-UNDO.    
    
    DEFINE BUFFER bf-ttOperation FOR ttOperation.
    
    /* get Quantity from Operations Data*/
    FIND FIRST bf-ttOperation
        WHERE bf-ttOperation.company    EQ ipbf-eb.company
        AND bf-ttOperation.estimateNo   EQ ipbf-eb.est-no
        AND bf-ttOperation.operationID  EQ ipcMachCode
        AND bf-ttOperation.formNo       EQ ipbf-eb.form-no
        AND bf-ttOperation.blankNo      EQ MAX(ipbf-eb.blank-no, 1)
        AND bf-ttOperation.pass         EQ MAX(ipiPass , 1) NO-ERROR.
            
   IF AVAILABLE bf-ttOperation THEN
        ASSIGN
            dReturnValue = bf-ttOperation.quantityInAfterSetupWaste
            cMsgTyp      = "Info"
            cMessageStr  = "Run Qty-" + STRING(dReturnValue) +  "| Qty In- " + STRING(bf-ttOperation.quantityIn) + ". Qty Out- " + STRING(bf-ttOperation.quantityOut) + ". Qty MR Waste- " + STRING(bf-ttOperation.quantityInSetupWaste) + ". Qty Run Waste- " + STRING(bf-ttOperation.quantityInRunWaste)
            .
   ELSE
        ASSIGN
            cMsgTyp      = "Error"
            cMessageStr  = "ttOperation not found for Mach" + ipcMachCode + " F-" + STRING(ipbf-eb.form-no) + " B-" + STRING(MAX(ipbf-eb.blank-no, 1)) + " P-" + STRING(MAX(ipiPass , 1))
            dReturnValue = ipdRunQty
            .
       
    RUN pBuildTagInfo (cMsgTyp,cMessageStr, "").
        
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


FUNCTION fGetBlankSqFTorSqINArea RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb):
    /*------------------------------------------------------------------------------
     Purpose: It returns the Blank Sq.Ft. of a Blank record
     Notes: It takes Blank's t-sqin field and converts that into SqFt. 
     For Folding Industory we need to set an exception to return Sq IN and for Corrugated its SQ FT.
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE dReturnValue AS DECIMAL NO-UNDO.
    
    IF fGetEstIndustry (ipbf-eb.est-no) = gcIndustryFolding THEN
         dReturnValue = ipbf-eb.t-sqin.
    
    ELSE
    DO:
        IF AVAILABLE ipbf-eb AND ipbf-eb.t-sqin NE 0 THEN
            dReturnValue = DYNAMIC-FUNCTION("fConv_GetAreaSqFeet", ipbf-eb.t-sqin,"SQIN").
    END. 
       
    IF dReturnValue = ? THEN
        dReturnValue = 0.
       
    RETURN dReturnValue.
    
END FUNCTION.

FUNCTION fGetQuantityPerSet RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb):
    /*------------------------------------------------------------------------------
     Purpose: Returns the quantity per set in decimal form for an eb 
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_GetQuantityPerSet", BUFFER ipbf-eb).
        
END FUNCTION.

FUNCTION fIsComboType RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Combo Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN DYNAMIC-FUNCTION("fEstimate_IsComboType", ipcEstType).
    
END FUNCTION.

FUNCTION fGetEstimateType RETURNS CHARACTER PRIVATE
    (ipiEstType AS INTEGER, ipcEstimateTypeID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Given estimate qualifiers, return the Estimate Type Description
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN DYNAMIC-FUNCTION("fEstimate_GetEstimateType", ipiEstType, ipcEstimateTypeID).
    
END FUNCTION.


FUNCTION fValidMachineLimits RETURNS LOGICAL 
    (BUFFER ipbf-eb FOR eb, BUFFER ipbf-mach FOR mach, INPUT ipdQty AS DECIMAL, INPUT ipcDept AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE lResult    AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE dShtLength AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dShtWidth  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCaliper   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dChkQty    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cReturnErr AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ef    FOR ef.
    DEFINE BUFFER bf-style FOR style.
    
    
    FIND FIRST bf-ef NO-LOCK
        WHERE bf-ef.company = ipbf-eb.company
        AND bf-ef.est-no  = ipbf-eb.est-no
        AND bf-ef.form-no = ipbf-eb.form-no NO-ERROR.
    
    IF AVAILABLE bf-ef THEN
    DO:    
        FIND FIRST bf-style NO-LOCK 
            WHERE bf-style.company EQ ipbf-eb.company
            AND bf-style.style EQ ipbf-eb.style NO-ERROR.
    
        IF AVAILABLE bf-style THEN
        DO: 
            dChkQty    = ipdQty.
            
            FIND FIRST ttDeptLimit
                WHERE ttDeptLimit.DeptCode = ipcDept NO-ERROR.
                
            IF AVAILABLE ttDeptLimit THEN
                ASSIGN
                    dShtLength = ttDeptLimit.ShtLength
                    dShtWidth  = ttDeptLimit.ShtWIdth
                    dCaliper   = ttDeptLimit.Caliper.
            
            ELSE
                ASSIGN
                    dShtLength = IF bf-ef.roll THEN bf-ef.gsh-len ELSE bf-ef.nsh-len
                    dShtWidth  = IF bf-ef.roll THEN bf-ef.gsh-wid ELSE bf-ef.nsh-wid
                    dCaliper   = bf-ef.cal.
    
            RUN est/MachLimitsDims.p (ROWID(bf-style), ROWID(ipbf-mach), ROWID(ipbf-eb), dShtLength, dShtWidth, dCaliper, dChkQty, OUTPUT cReturnErr).
    
            IF cReturnErr EQ "" THEN
                RUN est/MachLimitsSlot.p (ROWID(bf-style), ROWID(ipbf-mach), ROWID(ipbf-eb), OUTPUT cReturnErr).
            
            IF cReturnErr EQ "" THEN
                RUN est/MachLimitsPanel.p (ROWID(bf-style), ROWID(ipbf-mach), ROWID(ipbf-eb), OUTPUT cReturnErr).
            
            IF cReturnErr EQ "" THEN
                lResult = YES.
        END.
    END.
    RETURN lResult.
        
END FUNCTION.

FUNCTION fVerifyLimitsForPrinter RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-mach FOR mach, ipcDept AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        
        FIND FIRST ttDeptLimit
            WHERE ttDeptLimit.DeptCode = ipcDept NO-ERROR.
                    
        IF AVAILABLE ttDeptLimit 
        AND ttDeptLimit.NumColors LE ipbf-mach.max-col 
        AND ipbf-mach.pr-type EQ ttDeptLimit.PressType THEN
          lResult = YES.
        
        RETURN lResult.
END FUNCTION.

PROCEDURE pGetStyleBuffer PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcStyle   AS CHARACTER NO-UNDO.
DEFINE PARAMETER BUFFER opbf-Style FOR style.

    FIND FIRST opbf-Style NO-LOCK 
        WHERE opbf-Style.company EQ ipcCompany
        AND opbf-Style.style EQ ipcStyle NO-ERROR.

END PROCEDURE.
