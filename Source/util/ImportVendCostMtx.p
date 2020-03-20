
/*------------------------------------------------------------------------
    File        : ImportVendCostMtx.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Prep and Die	

    Author(s)   : Sewa Singh
    Created     : Wed Sept 11
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportVendCostMtx
    FIELD Company                 AS CHARACTER 
    FIELD Location                AS CHARACTER 
    FIELD itemType                AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Item Type" HELP "Required - FG or RM " 
    FIELD itemID                  AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Item#" HELP "Required - Size:15"
    FIELD vendorID                AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Vendor" HELP "Optional - - Size:8"
    FIELD customerID              AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Customer" HELP "Optional - Size:8"
    FIELD estimateNo              AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Estimate" HELP "Optional - Size:8"
    FIELD formNo                  AS INTEGER FORMAT ">>" INITIAL 0 COLUMN-LABEL "Form" HELP "Optional - Integer"
    FIELD blankNo                 AS INTEGER FORMAT ">>" INITIAL 0 COLUMN-LABEL "Blank" HELP "Optional - Integer"
    FIELD vendorItemID            AS CHARACTER FORMAT "x(16)" COLUMN-LABEL "Vendor Item" HELP "Optional - Size:16"
    FIELD effectiveDate           AS DATE COLUMN-LABEL "Effective" HELP "Optional - Date"
    FIELD expirationDate          AS DATE COLUMN-LABEL "Expires" HELP "Optional - Date"
    FIELD vendorUOM               AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Vendor Uom" HELP "Required - Size:3"
    FIELD dimWidthMinimum         AS DECIMAL FORMAT "->>,>>9.9999" INITIAL 0 COLUMN-LABEL "Width Minimum" HELP "Optional - Decimal"
    FIELD dimWidthMaximum         AS DECIMAL FORMAT "->>,>>9.9999" INITIAL 0 COLUMN-LABEL "Width Maximum" HELP "Optional - Decimal"
    FIELD dimLengthMinimum        AS DECIMAL FORMAT "->>,>>9.9999" INITIAL 0 COLUMN-LABEL "Length Minimum" HELP "Optional - Decimal"
    FIELD dimLengthMaximum        AS DECIMAL FORMAT "->>,>>9.9999" INITIAL 0 COLUMN-LABEL "Length Maximum" HELP "Optional - Decimal"
    FIELD dimLengthUnder          AS DECIMAL FORMAT "->>,>>9.9999" INITIAL 0 COLUMN-LABEL "Length Under" HELP "Optional - Decimal"
    FIELD dimWidthUnder           AS DECIMAL FORMAT "->>,>>9.9999" INITIAL 0 COLUMN-LABEL "Width Under" HELP "Optional - Decimal"
    FIELD dimWidthOver            AS DECIMAL FORMAT "->>,>>9.9999" INITIAL 0 COLUMN-LABEL "Width Upcharge" HELP "Optional - Decimal"
    FIELD dimLengthOver           AS DECIMAL FORMAT "->>,>>9.9999" INITIAL 0 COLUMN-LABEL "Length Upcharge" HELP "Optional - Decimal"
    FIELD quantityMinimumOrder    AS DECIMAL FORMAT "->>,>>>,>>9" INITIAL 0 COLUMN-LABEL "Min Order Qty" HELP "Optional - Decimal"
    FIELD quantityMaximumOrder    AS DECIMAL FORMAT "->>,>>>,>>9" INITIAL 0 COLUMN-LABEL "Max Order Qty" HELP "Optional - Decimal" 
    
    FIELD LevelQuantity01         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 1" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM01       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 1" HELP "Optional - decimal"
    FIELD LevelSetup01            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 1" HELP "Optional - decimal" 
    FIELD DeviationCost1          AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 1" HELP "Optional - Decimal"
    FIELD LeadTime1               AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 1" HELP "Optional - Integer"
    FIELD LevelQuantity02         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 2" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM02       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 2" HELP "Optional - decimal"
    FIELD LevelSetup02            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 2" HELP "Optional - decimal" 
    FIELD DeviationCost2          AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 2" HELP "Optional - Decimal"
    FIELD LeadTime2               AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 2" HELP "Optional - Integer"
    FIELD LevelQuantity03         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 3" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM03       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 3" HELP "Optional - decimal"
    FIELD LevelSetup03            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 3" HELP "Optional - decimal" 
    FIELD DeviationCost3          AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 3" HELP "Optional - Decimal"
    FIELD LeadTime3               AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 3" HELP "Optional - Integer"
    FIELD LevelQuantity04         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 4" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM04       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 4" HELP "Optional - decimal"
    FIELD LevelSetup04            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 4" HELP "Optional - decimal" 
    FIELD DeviationCost4          AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 4" HELP "Optional - Decimal"
    FIELD LeadTime4               AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 4" HELP "Optional - Integer"
    FIELD LevelQuantity05         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 5" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM05       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 5" HELP "Optional - decimal"
    FIELD LevelSetup05            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 5" HELP "Optional - decimal" 
    FIELD DeviationCost5          AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 5" HELP "Optional - Decimal"
    FIELD LeadTime5               AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 5" HELP "Optional - Integer"
    FIELD LevelQuantity06         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 6" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM06       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 6" HELP "Optional - decimal"
    FIELD LevelSetup06            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 6" HELP "Optional - decimal" 
    FIELD DeviationCost6          AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 6" HELP "Optional - Decimal"
    FIELD LeadTime6               AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 6" HELP "Optional - Integer"
    FIELD LevelQuantity07         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 7" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM07       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 7" HELP "Optional - decimal"
    FIELD LevelSetup07            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 7" HELP "Optional - decimal" 
    FIELD DeviationCost7          AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 7" HELP "Optional - Decimal"
    FIELD LeadTime7               AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 7" HELP "Optional - Integer"
    FIELD LevelQuantity08         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 8" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM08       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 8" HELP "Optional - decimal"
    FIELD LevelSetup08            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 8" HELP "Optional - decimal" 
    FIELD DeviationCost8          AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 8" HELP "Optional - Decimal"
    FIELD LeadTime8               AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 8" HELP "Optional - Integer"
    FIELD LevelQuantity09         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 9" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM09       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 9" HELP "Optional - decimal"
    FIELD LevelSetup09            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 9" HELP "Optional - decimal" 
    FIELD DeviationCost9          AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 9" HELP "Optional - Decimal"
    FIELD LeadTime9               AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 9" HELP "Optional - Integer"
    FIELD LevelQuantity10         AS DECIMAL   FORMAT ">,>>>,>>9.9<<" INITIAL 0 COLUMN-LABEL "Level Quantity 10" HELP "Optional - decimal" 
    FIELD LevelCostPerUOM10       AS DECIMAL   FORMAT ">>,>>9.9999" INITIAL 0 COLUMN-LABEL "Cost Per 10" HELP "Optional - decimal"
    FIELD LevelSetup10            AS DECIMAL   FORMAT "->>,>>9.99" INITIAL 0 COLUMN-LABEL "Setup 10" HELP "Optional - decimal"
    FIELD DeviationCost10         AS DECIMAL   FORMAT "->,>>>,>>9.99":U INITIAL 0 COLUMN-LABEL "Deviation Cost 10" HELP "Optional - Decimal"
    FIELD LeadTime10              AS INTEGER   FORMAT ">,>>>,>>9":U INITIAL 0 COLUMN-LABEL "Lead Time 10" HELP "Optional - Integer"
    
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportVendCostMtx"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportVendCostMtx FOR ttImportVendCostMtx.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO .
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO .
    DEFINE VARIABLE hVendorCostProcs AS HANDLE NO-UNDO.
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
     RUN system\VendorCostProcs.p PERSISTENT SET hVendorCostProcs.

    FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK 
        WHERE bf-vendItemCost.company EQ ipbf-ttImportVendCostMtx.Company
        AND bf-vendItemCost.itemType EQ ipbf-ttImportVendCostMtx.itemType
        AND bf-vendItemCost.itemID EQ ipbf-ttImportVendCostMtx.itemID
        AND bf-vendItemCost.vendorID EQ ipbf-ttImportVendCostMtx.vendorID
        AND bf-vendItemCost.customerID EQ ipbf-ttImportVendCostMtx.customerID
        AND bf-vendItemCost.estimateNo EQ ipbf-ttImportVendCostMtx.estimateNo
        AND bf-vendItemCost.formNo EQ ipbf-ttImportVendCostMtx.formNo
        AND bf-vendItemCost.blankNo EQ ipbf-ttImportVendCostMtx.blankNo
        AND bf-vendItemCost.expirationDate EQ date(ipbf-ttImportVendCostMtx.expirationDate)
        AND bf-vendItemCost.effectiveDate EQ date(ipbf-ttImportVendCostMtx.effectiveDate)
        NO-ERROR.

    IF NOT AVAILABLE bf-vendItemCost THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE bf-vendItemCost.
        ASSIGN 
            bf-vendItemCost.company   = ipbf-ttImportVendCostMtx.Company.
           
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                        
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.itemType, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.itemType).                                                   
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.itemID, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.itemID).                                                   
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.vendorID, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.vendorID).                               
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.customerID, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.customerID).                                                   
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.estimateNo, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.estimateNo).                                                   
    RUN pAssignValueI (ipbf-ttImportVendCostMtx.formNo, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.formNo).                                       
    RUN pAssignValueI (ipbf-ttImportVendCostMtx.blankNo, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.blankNo).                                         
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.vendorItemID, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.vendorItemID).                      
    RUN pAssignValueDate (ipbf-ttImportVendCostMtx.effectiveDate, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.effectiveDate).                                 
    RUN pAssignValueDate (ipbf-ttImportVendCostMtx.expirationDate, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.expirationDate).                                     
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.vendorUOM, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.vendorUOM).                                                 
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimWidthMinimum, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.dimWidthMinimum).                   
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimWidthMaximum, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.dimWidthMaximum).                                         
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimLengthMinimum, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.dimLengthMinimum).                                 
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimLengthMaximum, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.dimLengthMaximum).                                       
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimLengthUnder, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.dimLengthUnder).                                 
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimWidthUnder, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.dimWidthUnder).                                   
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimWidthOver, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.dimWidthOver).                             
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimLengthOver, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.dimLengthOver).                                 
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.quantityMinimumOrder, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.quantityMinimumOrder).                             
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.quantityMaximumOrder, iplIgnoreBlanks, INPUT-OUTPUT bf-vendItemCost.quantityMaximumOrder). 
    
    FOR EACH  bf-vendItemCostLevel EXCLUSIVE-LOCK
        WHERE bf-vendItemCostLevel.vendItemCostID EQ bf-vendItemCost.vendItemCostID :
        DELETE bf-vendItemCostLevel .
    END.
    
   DO iCount = 1 TO 10:
       IF iCount EQ 1 AND ipbf-ttImportVendCostMtx.LevelQuantity01 NE 0 THEN
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity01,ipbf-ttImportVendCostMtx.LevelCostPerUOM01,ipbf-ttImportVendCostMtx.LevelSetup01,ipbf-ttImportVendCostMtx.DeviationCost1,ipbf-ttImportVendCostMtx.LeadTime1) .
       ELSE IF iCount EQ 2 AND ipbf-ttImportVendCostMtx.LevelQuantity02 NE 0 THEN
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity02, ipbf-ttImportVendCostMtx.LevelCostPerUOM02,ipbf-ttImportVendCostMtx.LevelSetup02,ipbf-ttImportVendCostMtx.DeviationCost2,ipbf-ttImportVendCostMtx.LeadTime2) .
       ELSE IF iCount EQ 3 AND ipbf-ttImportVendCostMtx.LevelQuantity03 NE 0 THEN                                                                                                                                                                                           
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity03, ipbf-ttImportVendCostMtx.LevelCostPerUOM03,ipbf-ttImportVendCostMtx.LevelSetup03,ipbf-ttImportVendCostMtx.DeviationCost3,ipbf-ttImportVendCostMtx.LeadTime3) .
       ELSE IF iCount EQ 4 AND ipbf-ttImportVendCostMtx.LevelQuantity04 NE 0 THEN                                                                                                                                                                                           
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity04, ipbf-ttImportVendCostMtx.LevelCostPerUOM04,ipbf-ttImportVendCostMtx.LevelSetup04,ipbf-ttImportVendCostMtx.DeviationCost4,ipbf-ttImportVendCostMtx.LeadTime4) .
       ELSE IF iCount EQ 5 AND ipbf-ttImportVendCostMtx.LevelQuantity05 NE 0 THEN                                                                                                                                                                                           
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity05, ipbf-ttImportVendCostMtx.LevelCostPerUOM05,ipbf-ttImportVendCostMtx.LevelSetup05,ipbf-ttImportVendCostMtx.DeviationCost5,ipbf-ttImportVendCostMtx.LeadTime5) .
       ELSE IF iCount EQ 6 AND ipbf-ttImportVendCostMtx.LevelQuantity06 NE 0 THEN                                                                                                                                                                                           
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity06, ipbf-ttImportVendCostMtx.LevelCostPerUOM06,ipbf-ttImportVendCostMtx.LevelSetup06,ipbf-ttImportVendCostMtx.DeviationCost6,ipbf-ttImportVendCostMtx.LeadTime6) .
       ELSE IF iCount EQ 7 AND ipbf-ttImportVendCostMtx.LevelQuantity07 NE 0 THEN                                                                                                                                                                                           
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity07, ipbf-ttImportVendCostMtx.LevelCostPerUOM07,ipbf-ttImportVendCostMtx.LevelSetup07,ipbf-ttImportVendCostMtx.DeviationCost7,ipbf-ttImportVendCostMtx.LeadTime7) .
       ELSE IF iCount EQ 8 AND ipbf-ttImportVendCostMtx.LevelQuantity08 NE 0 THEN                                                                                                                                                                                           
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity08, ipbf-ttImportVendCostMtx.LevelCostPerUOM08,ipbf-ttImportVendCostMtx.LevelSetup08,ipbf-ttImportVendCostMtx.DeviationCost8,ipbf-ttImportVendCostMtx.LeadTime8) .
       ELSE IF iCount EQ 9 AND ipbf-ttImportVendCostMtx.LevelQuantity09 NE 0 THEN                                                                                                                                                                                           
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity09, ipbf-ttImportVendCostMtx.LevelCostPerUOM09,ipbf-ttImportVendCostMtx.LevelSetup09,ipbf-ttImportVendCostMtx.DeviationCost9,ipbf-ttImportVendCostMtx.LeadTime9) .
       ELSE IF iCount EQ 10 AND ipbf-ttImportVendCostMtx.LevelQuantity10 NE 0 THEN                                                                                                                                                                                           
           RUN pAssignVendCostValue(bf-vendItemCost.vendItemCostID ,ipbf-ttImportVendCostMtx.LevelQuantity10, ipbf-ttImportVendCostMtx.LevelCostPerUOM10,ipbf-ttImportVendCostMtx.LevelSetup10,ipbf-ttImportVendCostMtx.DeviationCost10,ipbf-ttImportVendCostMtx.LeadTime10) .
   END.
              
    FIND CURRENT bf-vendItemCost NO-LOCK NO-ERROR .
    
    RUN RecalculateFromAndTo IN hVendorCostProcs (bf-vendItemCost.vendItemCostID, OUTPUT lReturnError ,OUTPUT cReturnMessage ) .

    RELEASE bf-vendItemCostLevel.
    RELEASE bf-vendItemCost .
    DELETE OBJECT hVendorCostProcs.

END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportVendCostMtx FOR ttImportVendCostMtx.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE NO-UNDO.
    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE VARIABLE uom-list AS CHARACTER INIT "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.
    DEFINE BUFFER bf-ttImportVendCostMtx FOR ttImportVendCostMtx.

    RUN util/Validate.p PERSISTENT SET hdValidator.

    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVendCostMtx.itemType EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Item Type is Blank".
    END.

    IF oplValid THEN 
    DO:
        IF date(ipbf-ttImportVendCostMtx.expirationDate) EQ ? THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Expires Date is Blank".
    END.

    IF oplValid THEN 
    DO:
        IF date(ipbf-ttImportVendCostMtx.effectiveDate) EQ ? THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Effective Date is Blank".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVendCostMtx.itemID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Item is Blank".
    END. 

    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVendCostMtx.vendorUOM EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Vendor UOM is Blank".
    END.

    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVendCostMtx.itemType NE  "FG" OR ipbf-ttImportVendCostMtx.itemType EQ "RM" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Item Type must be FG or RM".
    END.
    
    ipbf-ttImportVendCostMtx.estimateNo =  FILL(" ",8 - LENGTH(TRIM(ipbf-ttImportVendCostMtx.estimateNo))) + TRIM(ipbf-ttImportVendCostMtx.estimateNo).
    
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST vendItemCost NO-LOCK 
            WHERE vendItemCost.company EQ ipbf-ttImportVendCostMtx.Company
            AND vendItemCost.itemType EQ ipbf-ttImportVendCostMtx.itemType
            AND vendItemCost.itemID EQ ipbf-ttImportVendCostMtx.itemID
            AND vendItemCost.vendorID EQ ipbf-ttImportVendCostMtx.vendorID
            AND vendItemCost.customerID EQ ipbf-ttImportVendCostMtx.customerID
            AND vendItemCost.estimateNo EQ ipbf-ttImportVendCostMtx.estimateNo
            AND vendItemCost.formNo EQ ipbf-ttImportVendCostMtx.formNo
            AND vendItemCost.blankNo EQ ipbf-ttImportVendCostMtx.blankNo
            AND vendItemCost.expirationDate EQ date(ipbf-ttImportVendCostMtx.expirationDate)
            AND (vendItemCost.effectiveDate EQ date(ipbf-ttImportVendCostMtx.effectiveDate)
            OR vendItemCost.effectiveDate LT 01/01/1900) NO-ERROR .

        IF AVAIL vendItemCost THEN
        DO: 
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate record exists"
                    .
            ELSE
                ASSIGN 
                    oplValid = YES
                    opcNote = "Update existing record"
                    .        
        END.
        ELSE 
            ASSIGN 
                oplValid = YES
                opcNote = "Add record"
                .
        
    END.
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:     
        IF oplValid AND ipbf-ttImportVendCostMtx.itemType EQ  "FG" THEN 
            RUN pIsValidFGITemID IN hdValidator (ipbf-ttImportVendCostMtx.itemID, NO, ipbf-ttImportVendCostMtx.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportVendCostMtx.itemType EQ  "RM" THEN 
            RUN pIsValidRMITemID IN hdValidator (ipbf-ttImportVendCostMtx.itemID, NO, ipbf-ttImportVendCostMtx.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportVendCostMtx.vendorID NE "" THEN 
            RUN pIsValidVendor IN hdValidator (ipbf-ttImportVendCostMtx.vendorID, NO, ipbf-ttImportVendCostMtx.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportVendCostMtx.customerID NE "" THEN 
            RUN pIsValidCustomerID IN hdValidator (ipbf-ttImportVendCostMtx.customerID, NO, ipbf-ttImportVendCostMtx.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportVendCostMtx.estimateNo NE "" THEN 
            RUN pIsValidEstID IN hdValidator (ipbf-ttImportVendCostMtx.estimateNo, NO, ipbf-ttImportVendCostMtx.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF ipbf-ttImportVendCostMtx.itemType EQ "RM" THEN DO:
            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.company EQ ipbf-ttImportVendCostMtx.Company
                AND ITEM.i-no EQ  ipbf-ttImportVendCostMtx.itemID NO-ERROR.
            IF AVAILABLE ITEM THEN
                RUN sys/ref/uom-rm.p  (ITEM.mat-type, OUTPUT uom-list).
            END.
            ELSE DO:
                RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).
            END.

        IF oplValid THEN 
            RUN pIsValidUOM IN hdValidator (ipbf-ttImportVendCostMtx.vendorUOM, YES, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid THEN 
            RUN pIsValidFromList IN hdValidator ("UOM",ipbf-ttImportVendCostMtx.vendorUOM,uom-list, OUTPUT oplValid, OUTPUT cValidNote).
    END.
    
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.

END PROCEDURE.


PROCEDURE pAssignVendCostValue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiSeq AS INTEGER NO-UNDO .
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostUom AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdSetup AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDevi AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLead AS INTEGER NO-UNDO.

    CREATE vendItemCostLevel .
    ASSIGN
        vendItemCostLevel.vendItemCostID = ipiSeq
        vendItemCostLevel.quantityBase   = ipdQty
        vendItemCostLevel.costPerUOM     = ipdCostUom
        vendItemCostLevel.costSetup      = ipdSetup
        vendItemCostLevel.costDeviation  = ipdDevi
        vendItemCostLevel.leadTimeDays   = ipiLead.
    
FIND CURRENT vendItemCostLevel NO-LOCK NO-ERROR .
RELEASE vendItemCostLevel .

END PROCEDURE.
