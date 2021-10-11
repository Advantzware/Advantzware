
/*------------------------------------------------------------------------
    File        : MRPReport.p
    Purpose     : Ticket 101951 - Replace ProductListXLS.p

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Mon Oct 04 10:18:00 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriEb AS ROWID  NO-UNDO.

DEFINE VARIABLE viWorkSheetCount   AS INTEGER          NO-UNDO.   
DEFINE VARIABLE LvOutputSelection  AS CHARACTER        INIT "on-Screen" NO-UNDO.
DEFINE VARIABLE CurActivePrinter   AS CHARACTER        NO-UNDO.
DEFINE VARIABLE AdobePrinter       AS CHARACTER        NO-UNDO.
DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE WshNetwork         AS COMPONENT-HANDLE.
DEFINE VARIABLE chFile             AS CHARACTER        NO-UNDO.
DEFINE VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttHeader
    FIELD company        AS CHARACTER 
    FIELD estimateNo     AS CHARACTER
    FIELD customerID     AS CHARACTER
    FIELD customerName   AS CHARACTER 
    FIELD customerPart   AS CHARACTER
    FIELD datePrinted    AS DATE
    FIELD dateCalculated AS DATE 
    FIELD plateID        AS CHARACTER 
    . 
    
DEFINE TEMP-TABLE ttQuantity
    FIELD quantity        AS INTEGER 
    FIELD estCostHeaderID AS INT64 
    FIELD indexID         AS INTEGER
    . 
    
DEFINE TEMP-TABLE ttMaterial
    FIELD itemID AS CHARACTER 
    FIELD itemName AS CHARACTER 
    FIELD sizeDesc AS CHARACTER 
    FIELD quantityOnHand AS DECIMAL 
    FIELD quantityAvailable AS DECIMAL 
    FIELD quantityRequired AS DECIMAL EXTENT 5
    FIELD quantityUOM AS CHARACTER
    FIELD quantityWasted AS DECIMAL EXTENT 5
    FIELD quantityWastePct AS DECIMAL EXTENT 5
    FIELD quantityToBuy AS DECIMAL EXTENT 5
    .

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fIsSet RETURNS LOGICAL PRIVATE
    (ipiEstType AS INTEGER) FORWARD.


/* ***************************  Main Block  *************************** */

RUN pBuildHeader(ipriEb).
RUN pBuildData.
RUN pBuildXlt.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the Quantity and Materials tables
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE dtLatestProbe AS DATE NO-UNDO. 
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    DEFINE VARIABLE cSizeDesc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQtyRequired AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyWasted AS DECIMAL NO-UNDO.
    
    FOR FIRST ttHeader,
        EACH estCostHeader NO-LOCK 
        WHERE estCostHeader.company EQ ttHeader.company
        AND estCostHeader.estimateNo EQ ttHeader.estimateNo
        BREAK BY estCostHeader.calcDateTime DESCENDING:
        
        IF FIRST-OF(estCostHeader.calcDateTime) THEN 
        DO: 
            ASSIGN 
                dtLatestProbe           = DATE(estCostHeader.calcDateTime)
                ttHeader.dateCalculated = dtLatestProbe
                ttHeader.datePrinted    = TODAY
                .                
        END.
        FIND FIRST ttQuantity NO-LOCK
            WHERE ttQuantity.quantity EQ estCostHeader.quantityMaster
            NO-ERROR.
        IF NOT AVAILABLE ttQuantity THEN 
        DO:
            CREATE ttQuantity.
            ASSIGN 
                ttQuantity.quantity = estCostHeader.quantityMaster
                ttQuantity.estCostHeaderID = estCostHeader.estCostHeaderID
                .
        END.  
    END.  //Each estCostHeader
    iIndex = 0.
    FOR EACH ttQuantity        
        BY ttQuantity.quantity:
        ASSIGN 
            iIndex = iIndex + 1
            ttQuantity.indexID = iIndex.
        FOR EACH estCostMaterial NO-LOCK 
            WHERE estCostMaterial.estCostHeaderID EQ ttQuantity.estCostHeaderID,
                FIRST item NO-LOCK 
                    WHERE item.company EQ estCostMaterial.company
                        AND item.i-no EQ estCostMaterial.itemID:
            
            IF estCostMaterial.isPrimarySubstrate THEN 
                cSizeDesc = TRIM(STRING(estCostMaterial.dimLength,">>>9.99")) + " x " + TRIM(STRING(estCostMaterial.dimWidth,">>>9.99")).
            ELSE 
                cSizeDesc = "".
                
            FIND FIRST ttMaterial
                WHERE ttMaterial.itemID EQ estCostMaterial.itemID
                AND ttMaterial.sizeDesc EQ cSizeDesc
                NO-ERROR.
            IF NOT AVAILABLE ttMaterial THEN DO: 
                CREATE ttMaterial.
                ASSIGN 
                    ttMaterial.itemID = estCostMaterial.itemID                    
                    ttMaterial.itemName = estCostMaterial.itemName
                    ttMaterial.quantityOnHand = item.q-onh
                    ttMaterial.quantityAvailable = item.q-avail
                    ttMaterial.sizeDesc = cSizeDesc
                    ttMaterial.quantityUOM = estCostMaterial.quantityUOM
                    .
            END.
            ASSIGN 
                dQtyRequired = estCostMaterial.quantityRequiredTotal
                dQtyWasted = estCostMaterial.quantityRequiredRunWaste + estCostMaterial.quantityRequiredSetupWaste
                
                .
            
            IF ttMaterial.quantityUOM NE estCostMaterial.quantityUOM THEN DO:
                //Convert 
            END.
                             
            ASSIGN 
                ttMaterial.quantityRequired[iIndex] = ttMaterial.quantityRequired[iIndex] + dQtyRequired
                ttMaterial.quantityWasted[iIndex] = ttMaterial.quantityWasted[iIndex] + dQtyWasted
                ttMaterial.quantityWastePct[iIndex] = ttMaterial.quantityWasted[iIndex] / (ttMaterial.quantityRequired[iIndex] - ttMaterial.quantityWasted[iIndex])  
                ttMaterial.quantityToBuy[iIndex] = MAX(ttMaterial.quantityRequired[iIndex] - ttMaterial.quantityAvailable, 0)
                .
                
        END.
    END.
    
END PROCEDURE.

PROCEDURE pBuildHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given ROWID for eb, builds all related header information
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEB AS ROWID NO-UNDO.

    DEFINE BUFFER bf-eb FOR eb.
    
    FIND FIRST eb NO-LOCK
        WHERE ROWID(eb) EQ ipriEB NO-ERROR.

    IF AVAILABLE eb THEN 
    DO: 
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ eb.company
            AND cust.cust-no EQ eb.cust-no
            NO-ERROR.
        
        IF fIsSet(eb.est-type) THEN 
        DO:
            FIND FIRST bf-eb NO-LOCK 
                WHERE bf-eb.company EQ eb.company
                AND bf-eb.est-no EQ eb.est-no
                AND bf-eb.form-no EQ 0
                NO-ERROR .
        END.
        IF NOT AVAILABLE bf-eb THEN  
            FIND bf-eb NO-LOCK 
                WHERE ROWID(bf-eb) EQ ROWID(eb).
        
        CREATE ttHeader.
        ASSIGN 
            ttHeader.company      = bf-eb.company
            ttHeader.estimateNo   = bf-eb.est-no
            ttHeader.customerID   = bf-eb.cust-no
            ttHeader.customerName = IF AVAILABLE cust THEN cust.name ELSE bf-eb.cust-no
            ttHeader.customerPart = bf-eb.part-no
            ttHeader.plateID      = eb.plate-no
            .
            
    END.
END PROCEDURE.

PROCEDURE pBuildXlt PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Outputs the data from the temptables to the report
 Notes:
------------------------------------------------------------------------------*/
        
    RUN pInitializeExcel.
    RUN pMainLoop.
    RUN pCleanup.    
    
END PROCEDURE.

PROCEDURE pInitializeExcel PRIVATE:

    /* Capture the current active printer. */
    IF LvOutputSelection = "email" THEN
        ASSIGN 
            CurActivePrinter = SESSION:PRINTER-NAME
            AdobePrinter     = "PDFcamp Printer".
  
    RUN sys/ref/getFileFullPathName.p ("Template\PurchasingList.xltx", OUTPUT chFile).
    IF chFile = ? THEN  
        APPLY 'close' TO THIS-PROCEDURE.

    /* Connect to the running Excel session. */
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.

    /* If Excel is running close it. */
    IF VALID-HANDLE (chExcelApplication) THEN
    DO:
        chExcelApplication:Quit()         NO-ERROR.
        RUN pCleanUp.
    END.


    /* Network connection checks. */
    CREATE "WScript.Network" WshNetwork NO-ERROR.
    IF NOT(VALID-HANDLE(WshNetwork)) THEN
    DO :
        MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.    
  
    /* Start a new session of Excel. */
    /*if not (valid-handle (chExcelApplication)) THEN*/
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
    /* Check if Excel got initialized. */
    IF NOT (VALID-HANDLE (chExcelApplication)) THEN
    DO :
        MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
  
    /* Make Excel visible. */
    ASSIGN
        chExcelApplication:VISIBLE = IF LvOutputSelection = "Email" OR 
                                     LvOutputSelection = "Printer" THEN  FALSE
                                  ELSE TRUE.      

END PROCEDURE.

PROCEDURE pMainLoop PRIVATE:
    
    /* Open our Excel Template. */  
    ASSIGN 
        chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
    /* Do not display Excel error messages. */
    chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

    /* Disable screen updating so it will go faster */
    chExcelApplication:ScreenUpdating = FALSE.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(1):Activate NO-ERROR.
    chWorkSheet      = chExcelApplication:Sheets:item(1).

    /*Fill in Data*/
    RUN pFillData.

    /* enable screen updating */
    chExcelApplication:ScreenUpdating = TRUE.
END PROCEDURE.

PROCEDURE pCleanUp PRIVATE:

    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.

    /* Reset the Active Printer to the Original Printer. */
    IF CurActivePrinter <> '' THEN
        WshNetwork:SetDefaultPrinter(CurActivePrinter).

    
    /* Release created objects. */
    RELEASE OBJECT WshNetwork         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.
END PROCEDURE.

PROCEDURE pFillData PRIVATE:   
    
    DEFINE VARIABLE iRowCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iColCount AS INTEGER NO-UNDO.
    
    FIND FIRST ttHeader NO-LOCK NO-ERROR.   
     
    ASSIGN
        viWorkSheetCount = 1.         
   
    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(viWorkSheetCount):Activate NO-ERROR.   
    
    ASSIGN
        chWorkSheet      = chExcelApplication:Sheets:item(viWorkSheetCount)
        chWorkSheet:name = "Purchasing List" .
      
     
    ASSIGN
        chWorkSheet:Range("B2"):value = ttHeader.customerName
        chWorkSheet:Range("B3"):value = ttHeader.customerPart
        chWorkSheet:Range("B4"):value = ttHeader.datePrinted
        chWorkSheet:Range("D3"):value = ttHeader.plateID
        chWorkSheet:Range("D4"):value = ttHeader.estimateNo
        .
    
    iColCount = 5.    
    FOR EACH ttQuantity NO-LOCK
        BY ttQuantity.quantity:
        chWorkSheet:Range("A6"):Offset(0,iColCount):value = ttQuantity.quantity.
        iColCount = iColCount + 3.
    END.
        
    iRowCount = 8.    
    FOR EACH ttMaterial NO-LOCK :           
       ASSIGN 
           chWorkSheet:Range("A" + STRING(iRowCount)):value = ttMaterial.itemID
           chWorkSheet:Range("B" + STRING(iRowCount)):value = ttMaterial.sizeDesc
           chWorkSheet:Range("C" + STRING(iRowCount)):value = ttMaterial.itemID 
           chWorkSheet:Range("D" + STRING(iRowCount)):value = ttMaterial.quantityUOM 
           chWorkSheet:Range("E" + STRING(iRowCount)):value = STRING(ttMaterial.quantityOnHand)
           chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(ttMaterial.quantityAvailable) 
           chWorkSheet:Range("G" + STRING(iRowCount)):value = STRING(ttMaterial.quantityRequired[1]) 
           chWorkSheet:Range("H" + STRING(iRowCount)):value = STRING(ttMaterial.quantityWastePct[1])
           chWorkSheet:Range("I" + STRING(iRowCount)):value = STRING(ttMaterial.quantityToBuy[1])
           chWorkSheet:Range("J" + STRING(iRowCount)):value = STRING(ttMaterial.quantityRequired[2]) 
           chWorkSheet:Range("K" + STRING(iRowCount)):value = STRING(ttMaterial.quantityWastePct[2])
           chWorkSheet:Range("L" + STRING(iRowCount)):value = STRING(ttMaterial.quantityToBuy[2])
           chWorkSheet:Range("M" + STRING(iRowCount)):value = STRING(ttMaterial.quantityRequired[3]) 
           chWorkSheet:Range("N" + STRING(iRowCount)):value = STRING(ttMaterial.quantityWastePct[3])
           chWorkSheet:Range("O" + STRING(iRowCount)):value = STRING(ttMaterial.quantityToBuy[3])
           chWorkSheet:Range("P" + STRING(iRowCount)):value = STRING(ttMaterial.quantityRequired[4]) 
           chWorkSheet:Range("Q" + STRING(iRowCount)):value = STRING(ttMaterial.quantityWastePct[4])
           chWorkSheet:Range("R" + STRING(iRowCount)):value = STRING(ttMaterial.quantityToBuy[4])
           chWorkSheet:Range("S" + STRING(iRowCount)):value = STRING(ttMaterial.quantityRequired[5]) 
           chWorkSheet:Range("T" + STRING(iRowCount)):value = STRING(ttMaterial.quantityWastePct[5])
           chWorkSheet:Range("U" + STRING(iRowCount)):value = STRING(ttMaterial.quantityToBuy[5])
           
           iRowCount = iRowCount + 1.     
    END.       
              
    chWorkbook:WorkSheets(1):Activate NO-ERROR.

END PROCEDURE. /* FillData*/


/* ************************  Function Implementations ***************** */

FUNCTION fIsSet RETURNS LOGICAL PRIVATE
    (ipiEstType AS INTEGER):
    /*------------------------------------------------------------------------------
    Purpose:  Given estimate type number, determine if set
    Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE VARIABLE cEstType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIsSet   AS LOGICAL   NO-UNDO.
    
    cEstType = DYNAMIC-FUNCTION("fEstimate_GetEstimateType", ipiEstType,"" ).

    IF cEstType NE "" THEN 
        lIsSet = DYNAMIC-FUNCTION("fEstimate_IsSetType", cEstType).
        
    RETURN lIsSet. 
		
END FUNCTION.

