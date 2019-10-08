
/*------------------------------------------------------------------------
    File        : ImportVendor.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Vendors	

    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportVend
    FIELD Company         AS CHARACTER FORMAT "x(3)"
    FIELD Location        AS CHARACTER 
    FIELD VendNo          AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Vendor" HELP "Required - Size:8" 
    FIELD VendName        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Vendor Name" HELP "Optional - Size:30"
    FIELD VendAdd1        AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Vendor Addr 1" HELP "Optional - Size:30"       
    FIELD VendAdd2        AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Vendor Addr 2" HELP "Optional - Size:30"      
    FIELD VendCity        AS CHARACTER FORMAT "X(16)" COLUMN-LABEL "City" HELP "Optional - Size:16"                     
    FIELD VendState       AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "State" HELP "Optional - Size:2"                     
    FIELD VendCountry     AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Country" HELP "Optional - Size:10" 
    FIELD VendPostal      AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "Postal Code" HELP "Optional - Size:11"
    FIELD VendZip         AS CHARACTER FORMAT "xxxxx-xxxx" COLUMN-LABEL "Zip Code" HELP "Optional - Size:9"
    FIELD VendRemit       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Remit To" HELP "Optional - Size:30"
    FIELD VendRAdd1       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Remit Addr 1" HELP "Optional - Size:30"
    FIELD VendRAdd2       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Remit Addr 2" HELP "Optional - Size:30"
    FIELD VendRCity       AS CHARACTER FORMAT "X(16)" COLUMN-LABEL "Remit City" HELP "Optional - Size:16"
    FIELD VendRState      AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Remit State" HELP "Optional - Size:2"
    FIELD VendRPostal     AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "Remit Postal Code" HELP "Optional - Size:11"
    FIELD VendRZip        AS CHARACTER FORMAT "xxxxx-xxxx" COLUMN-LABEL "Remit Zip Code" HELP "Optional - Size:9"
    FIELD Terms           AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Terms Code" HELP "Optional - Validated - Size:5"
    FIELD GL              AS CHARACTER FORMAT "X(25)" COLUMN-LABEL "Default G/L#" HELP "Optional - Validated - Size:25"
    FIELD VendAreaCode    AS CHARACTER FORMAT "(xxx)" COLUMN-LABEL "Area Code" HELP "Optional - Size:3"
    FIELD VendPhone       AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Phone" HELP "Optional - Size:7"
    FIELD VendFaxAreaCode AS CHARACTER FORMAT "(xxx)" COLUMN-LABEL "Fax Area Code" HELP "Optional - Size:3"
    FIELD VendFax         AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Fax" HELP "Optional - Size:7"
    FIELD Code1099        AS CHARACTER FORMAT "x" COLUMN-LABEL "1099 code" HELP "Optional - Y or N"
    FIELD FedID           AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Tax ID#" HELP "Optional - Size:15"
    FIELD VendType        AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Type" HELP "Optional - Validated - Size:8"
    FIELD TaxGroup        AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Tax" HELP "Optional - Validated - Size:3"
    FIELD Carrier         AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Carrier" HELP "Optional - Size:5"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportVend"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportVend FOR ttImportVend.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportVend FOR ttImportVend.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVend.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVend.VendNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Vendor".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST vend NO-LOCK 
            WHERE vend.company EQ ipbf-ttImportVend.Company
            AND vend.vend-no EQ ipbf-ttImportVend.VendNo
            NO-ERROR .
        IF AVAILABLE vend THEN 
        DO:
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate Exists:  Will be skipped"
                    .
            ELSE
                ASSIGN 
                    opcNote = "Update record - All fields to be overwritten"
                    .        
        END.
        ELSE 
            ASSIGN 
                opcNote = "Add record"
                .
        
    END.
    
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportVend.VendState NE "" THEN 
            RUN pIsValidState IN hdValidator (ipbf-ttImportVend.VendState, NO, OUTPUT oplValid, OUTPUT cValidNote). 
           
        IF oplValid AND ipbf-ttImportVend.VendRState NE "" THEN 
            RUN pIsValidState IN hdValidator (ipbf-ttImportVend.VendRState, NO, OUTPUT oplValid, OUTPUT cValidNote). 
           
        IF oplValid AND ipbf-ttImportVend.Terms NE "" THEN 
            RUN pIsValidTerms IN hdValidator (ipbf-ttImportVend.Terms, NO, ipbf-ttImportVend.Company, OUTPUT oplValid, OUTPUT cValidNote).
           
        IF oplValid AND ipbf-ttImportVend.GL NE "" THEN 
            RUN pIsValidGLAccount IN hdValidator (ipbf-ttImportVend.GL, NO, ipbf-ttImportVend.Company, OUTPUT oplValid, OUTPUT cValidNote).
          
        IF oplValid AND ipbf-ttImportVend.VendType NE "" THEN 
            RUN pIsValidVendorType IN hdValidator (ipbf-ttImportVend.VendType, NO, ipbf-ttImportVend.Company, OUTPUT oplValid, OUTPUT cValidNote).
           
        IF oplValid AND ipbf-ttImportVend.TaxGroup NE "" THEN 
            RUN pIsValidTaxGroup IN hdValidator (ipbf-ttImportVend.TaxGroup, NO, ipbf-ttImportVend.Company, OUTPUT oplValid, OUTPUT cValidNote).
       
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportVend FOR ttImportVend.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    
    FIND FIRST vend EXCLUSIVE-LOCK
        WHERE vend.company EQ ipbf-ttImportVend.Company
        AND vend.vend-no EQ ipbf-ttImportVend.VendNo
        NO-ERROR.
    IF NOT AVAILABLE vend THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE vend.
        ASSIGN 
            vend.company = ipbf-ttImportVend.Company
            vend.loc = ipbf-ttImportVend.Location
            vend.vend-no = ipbf-ttImportVend.VendNo
            vend.frt-pay      = "P"
            vend.payment-type = "Check"
            .
    END.
    RUN pAssignValueC (ipbf-ttImportVend.VendName, iplIgnoreBlanks, INPUT-OUTPUT vend.name).
    RUN pAssignValueC (ipbf-ttImportVend.VendRemit, iplIgnoreBlanks, INPUT-OUTPUT vend.remit).
    RUN pAssignValueC (ipbf-ttImportVend.VendAdd1, iplIgnoreBlanks, INPUT-OUTPUT vend.add1).
    RUN pAssignValueC (ipbf-ttImportVend.VendRAdd1, iplIgnoreBlanks, INPUT-OUTPUT vend.r-add1).
    RUN pAssignValueC (ipbf-ttImportVend.VendAdd2, iplIgnoreBlanks, INPUT-OUTPUT vend.add2).
    RUN pAssignValueC (ipbf-ttImportVend.VendRAdd2, iplIgnoreBlanks, INPUT-OUTPUT vend.r-add2).
    RUN pAssignValueC (ipbf-ttImportVend.VendCity, iplIgnoreBlanks, INPUT-OUTPUT vend.city).
    RUN pAssignValueC (ipbf-ttImportVend.VendRCity, iplIgnoreBlanks, INPUT-OUTPUT vend.r-city).
    RUN pAssignValueC (ipbf-ttImportVend.VendState, iplIgnoreBlanks, INPUT-OUTPUT vend.state).
    RUN pAssignValueC (ipbf-ttImportVend.VendRState, iplIgnoreBlanks, INPUT-OUTPUT vend.r-state).
    RUN pAssignValueC (ipbf-ttImportVend.VendCountry, iplIgnoreBlanks, INPUT-OUTPUT vend.country).
    RUN pAssignValueC (ipbf-ttImportVend.VendCountry, iplIgnoreBlanks, INPUT-OUTPUT vend.r-country).
    RUN pAssignValueC (ipbf-ttImportVend.VendPostal, iplIgnoreBlanks, INPUT-OUTPUT vend.postal).
    RUN pAssignValueC (ipbf-ttImportVend.VendRPostal, iplIgnoreBlanks, INPUT-OUTPUT vend.r-postal).
    RUN pAssignValueC (ipbf-ttImportVend.VendZip, iplIgnoreBlanks, INPUT-OUTPUT vend.zip).
    RUN pAssignValueC (ipbf-ttImportVend.VendRZip, iplIgnoreBlanks, INPUT-OUTPUT vend.r-zip).    
    RUN pAssignValueC (ipbf-ttImportVend.VendAreaCode, iplIgnoreBlanks, INPUT-OUTPUT vend.area-code).
    RUN pAssignValueC (REPLACE(ipbf-ttImportVend.VendPhone,"-",""), iplIgnoreBlanks, INPUT-OUTPUT vend.phone).
    RUN pAssignValueC (ipbf-ttImportVend.VendFaxAreaCode, iplIgnoreBlanks, INPUT-OUTPUT vend.fax-area).
    RUN pAssignValueC (REPLACE(ipbf-ttImportVend.VendFax,"-",""), iplIgnoreBlanks, INPUT-OUTPUT vend.fax).
    RUN pAssignValueC (ipbf-ttImportVend.TaxGroup, YES, INPUT-OUTPUT vend.tax-gr).
    RUN pAssignValueC (ipbf-ttImportVend.Terms, YES, INPUT-OUTPUT vend.terms).
    RUN pAssignValueC (ipbf-ttImportVend.Code1099, iplIgnoreBlanks, INPUT-OUTPUT vend.code-1099).
    RUN pAssignValueC (ipbf-ttImportVend.Carrier, YES, INPUT-OUTPUT vend.carrier).
    RUN pAssignValueC (ipbf-ttImportVend.GL, YES, INPUT-OUTPUT vend.actnum).
    RUN pAssignValueC (ipbf-ttImportVend.FedID, iplIgnoreBlanks, INPUT-OUTPUT vend.tax-id).
    
    
    IF vend.country = "Canada" THEN 
        vend.curr-code = "CAD".
    ELSE 
        vend.curr-code = "USD".
    

END PROCEDURE.

