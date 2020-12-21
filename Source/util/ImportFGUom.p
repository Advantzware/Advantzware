
/*------------------------------------------------------------------------
    File        : ImportFGUom.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for FG Item UoM

    Author(s)   : Sewa Singh
    Created     : Fri Dec 18
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportFGUom
    FIELD Company                 AS CHARACTER 
    FIELD Location                AS CHARACTER
    FIELD itemID                  AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "FG Item No #" HELP "Required - Size:15"
    FIELD UOM                     AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM" HELP "Required - Size:3"
    FIELD descr                   AS CHARACTER FORMAT "x(24)" COLUMN-LABEL "UoM Description" HELP "Optional - Size:24"
    FIELD convFactor              AS DECIMAL   FORMAT ">>>,>>>,>>9.99<<<<" COLUMN-LABEL "Conv. Factor" HELP "Optional - Decimal"
    FIELD canPurchase             AS CHARACTER FORMAT "Yes/No" COLUMN-LABEL "Can Purchase" HELP "Optional - Logical  Initial Value: No"
    FIELD canSell                 AS CHARACTER FORMAT "Yes/No" COLUMN-LABEL "Can Sell" HELP "Optional - Logical  Initial Value: No"
    FIELD inactive                AS CHARACTER FORMAT "Yes/No" COLUMN-LABEL "Inactive" HELP "Optional - Logical Initial Value: No"
          .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportFGUom"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGUom FOR ttImportFGUom.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
   /* DEFINE VARIABLE riNote AS ROWID NO-UNDO.     */
    DEFINE BUFFER bf-itemUoM FOR itemUoM.

    FIND FIRST bf-itemUoM EXCLUSIVE-LOCK 
        WHERE bf-itemUoM.company EQ ipbf-ttImportFGUom.Company
        AND bf-itemUoM.itemID EQ ipbf-ttImportFGUom.itemID
        AND bf-itemUoM.UOM EQ ipbf-ttImportFGUom.UOM
        NO-ERROR.

    IF NOT AVAILABLE bf-itemUoM THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE bf-itemUoM.
        ASSIGN 
            bf-itemUoM.company      = ipbf-ttImportFGUom.Company
            bf-itemUoM.UOM          = ipbf-ttImportFGUom.UOM
            bf-itemUoM.itemID      = ipbf-ttImportFGUom.itemID
            bf-itemUoM.createdBy    = USERID(LDBNAME(1))
            bf-itemUoM.createdDtTm  = TODAY
            bf-itemUoM.updatedBy    = USERID(LDBNAME(1))
            bf-itemUoM.updatedDtTm  = TODAY
            bf-itemUoM.itemType    = "FG" .
            
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipbf-ttImportFGUom.Company
               AND itemfg.i-no    EQ ipbf-ttImportFGUom.itemID
             NO-ERROR.
        IF AVAILABLE itemfg THEN
        bf-itemUoM.procat = itemfg.procat.
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                                                                         
    RUN pAssignValueC (ipbf-ttImportFGUom.itemID, iplIgnoreBlanks, INPUT-OUTPUT bf-itemUoM.itemID).                                                   
    RUN pAssignValueC (ipbf-ttImportFGUom.UOM, iplIgnoreBlanks, INPUT-OUTPUT bf-itemUoM.UOM).                               
    RUN pAssignValueC (ipbf-ttImportFGUom.descr, iplIgnoreBlanks, INPUT-OUTPUT bf-itemUoM.descr).                                                   
    RUN pAssignValueD (ipbf-ttImportFGUom.convFactor, iplIgnoreBlanks, INPUT-OUTPUT bf-itemUoM.convFactor).                                                   
  
    ASSIGN
    bf-itemUoM.canPurchase = IF ipbf-ttImportFGUom.canPurchase EQ "Yes" THEN TRUE ELSE FALSE 
    bf-itemUoM.canSell     = IF ipbf-ttImportFGUom.canSell EQ "Yes" THEN TRUE ELSE FALSE
    bf-itemUoM.inactive    = IF ipbf-ttImportFGUom.inactive EQ "Yes" THEN TRUE ELSE FALSE.
  RELEASE bf-itemUoM .
                                                                                                                               
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGUom FOR ttImportFGUom.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-itemUoM FOR itemUoM.
    DEFINE BUFFER bf-ttImportFGUom FOR ttImportFGUom.


    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGUom.itemID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Please Enter F/G Item Number".
        IF ipbf-ttImportFGUom.UOM EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "FG UOM is Blank".
    END.
        
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportFGUom NO-LOCK 
            WHERE bf-ttImportFGUom.Company EQ ipbf-ttImportFGUom.Company
            AND bf-ttImportFGUom.itemID EQ ipbf-ttImportFGUom.itemID
            AND bf-ttImportFGUom.UOM EQ ipbf-ttImportFGUom.UOM
            AND ROWID(bf-ttImportFGUom) NE ROWID(ipbf-ttImportFGUom)
            NO-ERROR.
        IF AVAILABLE bf-ttImportFGUom THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST bf-itemUoM NO-LOCK 
            WHERE bf-itemUoM.Company EQ ipbf-ttImportFGUom.Company
            AND bf-itemUoM.itemID EQ ipbf-ttImportFGUom.itemID
            AND bf-itemUoM.UOM EQ ipbf-ttImportFGUom.UOM
            NO-ERROR .
        IF AVAIL bf-itemUoM THEN
        DO: 
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate record exists".
            ELSE
                ASSIGN 
                    oplValid = YES
                    opcNote = "Update existing record".        
        END.
        ELSE 
            ASSIGN 
                oplValid = YES
                opcNote = "Add record".
        
    END.
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
                   
        IF oplValid AND ipbf-ttImportFGUom.itemID NE "" THEN 
            RUN pIsValidFGITemID (ipbf-ttImportFGUom.itemID, NO, ipbf-ttImportFGUom.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
END PROCEDURE.

