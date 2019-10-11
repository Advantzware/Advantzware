
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
    FIELD formNo                  AS INTEGER FORMAT ">>" COLUMN-LABEL "Form" HELP "Optional - Integer"
    FIELD blankNo                 AS INTEGER FORMAT ">>" COLUMN-LABEL "Blank" HELP "Optional - Integer"
    FIELD vendorItemID            AS CHARACTER FORMAT "x(16)" COLUMN-LABEL "Vendor Item" HELP "Optional - Size:16"
    FIELD effectiveDate           AS CHARACTER FORMAT "99/99/9999" COLUMN-LABEL "Effective" HELP "Optional - Date"
    FIELD expirationDate          AS CHARACTER FORMAT "99/99/9999" COLUMN-LABEL "Expires" HELP "Optional - Date"
    FIELD vendorUOM               AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Vendor Uom" HELP "Required - Size:3"
    FIELD dimWidthMinimum         AS DECIMAL FORMAT "->>,>>9.9999" COLUMN-LABEL "Width Minimum" HELP "Optional - Decimal"
    FIELD dimWidthMaximum         AS DECIMAL FORMAT "->>,>>9.9999" COLUMN-LABEL "Width Maximum" HELP "Optional - Decimal"
    FIELD dimLengthMinimum        AS DECIMAL FORMAT "->>,>>9.9999" COLUMN-LABEL "Length Minimum" HELP "Optional - Decimal"
    FIELD dimLengthMaximum        AS DECIMAL FORMAT "->>,>>9.9999" COLUMN-LABEL "Length Maximum" HELP "Optional - Decimal"
    FIELD dimLengthUnder          AS DECIMAL FORMAT "->>,>>9.9999" COLUMN-LABEL "Length Under" HELP "Optional - Decimal"
    FIELD dimWidthUnder           AS DECIMAL FORMAT "->>,>>9.9999" COLUMN-LABEL "Width Under" HELP "Optional - Decimal"
    FIELD dimWidthOver            AS DECIMAL FORMAT "->>,>>9.9999" COLUMN-LABEL "Width Upcharge" HELP "Optional - Decimal"
    FIELD dimLengthOver           AS DECIMAL FORMAT "->>,>>9.9999" COLUMN-LABEL "Length Upcharge" HELP "Optional - Decimal"
    FIELD quantityMinimumOrder    AS INTEGER FORMAT "->>,>>>,>>9" COLUMN-LABEL "Min Order Qty" HELP "Optional - Integer"
    FIELD quantityMaximumOrder    AS INTEGER FORMAT "->>,>>>,>>9" COLUMN-LABEL "Max Order Qty" HELP "Optional - Integer" 
    FIELD quantityFrom            AS DECIMAL FORMAT ">>,>>>,>>9.9999" COLUMN-LABEL "From Qty" HELP "Optional - Decimal"
    FIELD quantityTo              AS DECIMAL FORMAT ">>,>>>,>>9.9999" COLUMN-LABEL "To Qty" HELP "Optional - Decimal"
    FIELD costPerUOM              AS DECIMAL FORMAT ">>,>>>,>>9.9999" COLUMN-LABEL "Cost Per" HELP "Optional - Decimal"
    FIELD costSetup               AS DECIMAL FORMAT ">,>>>,>>9.9999" COLUMN-LABEL "Setup" HELP "Optional - Decimal"  
    FIELD costDeviation           AS DECIMAL FORMAT ">,>>>,>>9.9999" COLUMN-LABEL "Deviation" HELP "Optional - Decimal"
    
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
    DEFINE VARIABLE hVendorCostProcs AS HANDLE NO-UNDO.
     RUN system\VendorCostProcs.p PERSISTENT SET hVendorCostProcs.

    FIND FIRST vendItemCost EXCLUSIVE-LOCK 
        WHERE vendItemCost.company EQ ipbf-ttImportVendCostMtx.Company
        AND vendItemCost.itemType EQ ipbf-ttImportVendCostMtx.itemType
        AND vendItemCost.itemID EQ ipbf-ttImportVendCostMtx.itemID
        AND vendItemCost.vendorID EQ ipbf-ttImportVendCostMtx.vendorID
        AND vendItemCost.customerID EQ ipbf-ttImportVendCostMtx.customerID
        AND vendItemCost.estimateNo EQ ipbf-ttImportVendCostMtx.estimateNo
        AND vendItemCost.effectiveDate EQ date(ipbf-ttImportVendCostMtx.effectiveDate)
        NO-ERROR.

    IF NOT AVAILABLE vendItemCost THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE vendItemCost.
        ASSIGN 
            vendItemCost.company   = ipbf-ttImportVendCostMtx.Company.
           
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                        
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.itemType, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.itemType).                                                   
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.itemID, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.itemID).                                                   
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.vendorID, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.vendorID).                               
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.customerID, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.customerID).                                                   
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.estimateNo, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.estimateNo).                                                   
    RUN pAssignValueI (ipbf-ttImportVendCostMtx.formNo, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.formNo).                                       
    RUN pAssignValueI (ipbf-ttImportVendCostMtx.blankNo, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.blankNo).                                         
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.vendorItemID, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.vendorItemID).                      
    RUN pAssignValueCToDt (ipbf-ttImportVendCostMtx.effectiveDate, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.effectiveDate).                                 
    RUN pAssignValueCToDt (ipbf-ttImportVendCostMtx.expirationDate, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.expirationDate).                                     
    RUN pAssignValueC (ipbf-ttImportVendCostMtx.vendorUOM, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.vendorUOM).                                                 
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimWidthMinimum, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.dimWidthMinimum).                   
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimWidthMaximum, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.dimWidthMaximum).                                         
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimLengthMinimum, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.dimLengthMinimum).                                 
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimLengthMaximum, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.dimLengthMaximum).                                       
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimLengthUnder, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.dimLengthUnder).                                 
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimWidthUnder, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.dimWidthUnder).                                   
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimWidthOver, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.dimWidthOver).                             
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.dimLengthOver, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.dimLengthOver).                                 
    RUN pAssignValueI (ipbf-ttImportVendCostMtx.quantityMinimumOrder, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.quantityMinimumOrder).                             
    RUN pAssignValueI (ipbf-ttImportVendCostMtx.quantityMaximumOrder, iplIgnoreBlanks, INPUT-OUTPUT vendItemCost.quantityMaximumOrder).                                 
    
    
    FIND FIRST vendItemCostLevel EXCLUSIVE-LOCK
            WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID 
              AND vendItemCostLevel.quantityBase EQ ipbf-ttImportVendCostMtx.quantityTo
             NO-ERROR .
    IF NOT AVAIL vendItemCostLevel THEN DO:
        CREATE vendItemCostLevel .
        ASSIGN
            vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID
            vendItemCostLevel.quantityBase   = ipbf-ttImportVendCostMtx.quantityTo .
    END.

    RUN pAssignValueD (ipbf-ttImportVendCostMtx.costPerUOM, iplIgnoreBlanks, INPUT-OUTPUT vendItemCostLevel.costPerUOM). 
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.costSetup, iplIgnoreBlanks, INPUT-OUTPUT vendItemCostLevel.costSetup). 
    RUN pAssignValueD (ipbf-ttImportVendCostMtx.costDeviation, iplIgnoreBlanks, INPUT-OUTPUT vendItemCostLevel.costDeviation). 
              
    FIND CURRENT vendItemCost NO-LOCK NO-ERROR .
    FIND CURRENT vendItemCostLevel NO-LOCK NO-ERROR .
    RUN RecalculateFromAndTo IN hVendorCostProcs (vendItemCost.vendItemCostID, OUTPUT lReturnError ,OUTPUT cReturnMessage ) .

    RELEASE vendItemCostLevel.
    RELEASE vendItemCost .
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
            AND vendItemCost.effectiveDate EQ date(ipbf-ttImportVendCostMtx.effectiveDate) NO-ERROR .

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

