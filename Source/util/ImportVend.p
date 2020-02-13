
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
    FIELD VendRCountry    AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "Remit Country" HELP "Optional - Size:11"
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
    FIELD BankAcct        AS CHARACTER FORMAT "x(18)" COLUMN-LABEL "Account#" HELP "Optional - Validated - Size:18"
    FIELD SwiftBIC        AS CHARACTER FORMAT "x(11)" COLUMN-LABEL "Swift Code" HELP "Optional - Validated - Size:11"
    FIELD BankRTN         AS INTEGER FORMAT "999999999" COLUMN-LABEL "Routing" HELP "Optional - Integer"
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
    DEFINE BUFFER bf-vend FOR vend.
    
    FIND FIRST bf-vend EXCLUSIVE-LOCK
        WHERE bf-vend.company EQ ipbf-ttImportVend.Company
        AND bf-vend.vend-no EQ ipbf-ttImportVend.VendNo
        NO-ERROR.
    IF NOT AVAILABLE bf-vend THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE bf-vend.
        ASSIGN 
            bf-vend.company = ipbf-ttImportVend.Company
            bf-vend.loc = ipbf-ttImportVend.Location
            bf-vend.vend-no = ipbf-ttImportVend.VendNo
            bf-vend.frt-pay      = "P"
            bf-vend.payment-type = "Check"
            .
    END.
    RUN pAssignValueC (ipbf-ttImportVend.VendName, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.name).
    RUN pAssignValueC (ipbf-ttImportVend.VendRemit, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.remit).
    RUN pAssignValueC (ipbf-ttImportVend.VendAdd1, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.add1).
    RUN pAssignValueC (ipbf-ttImportVend.VendRAdd1, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.r-add1).
    RUN pAssignValueC (ipbf-ttImportVend.VendAdd2, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.add2).
    RUN pAssignValueC (ipbf-ttImportVend.VendRAdd2, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.r-add2).
    RUN pAssignValueC (ipbf-ttImportVend.VendCity, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.city).
    RUN pAssignValueC (ipbf-ttImportVend.VendRCity, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.r-city).
    RUN pAssignValueC (ipbf-ttImportVend.VendState, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.state).
    RUN pAssignValueC (ipbf-ttImportVend.VendRState, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.r-state).
    RUN pAssignValueC (ipbf-ttImportVend.VendCountry, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.country).
    RUN pAssignValueC (ipbf-ttImportVend.VendRCountry, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.r-country).
    RUN pAssignValueC (ipbf-ttImportVend.VendPostal, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.postal).
    RUN pAssignValueC (ipbf-ttImportVend.VendRPostal, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.r-postal).
    RUN pAssignValueC (ipbf-ttImportVend.VendZip, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.zip).
    RUN pAssignValueC (ipbf-ttImportVend.VendRZip, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.r-zip).    
    RUN pAssignValueC (ipbf-ttImportVend.VendAreaCode, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.area-code).
    RUN pAssignValueC (REPLACE(ipbf-ttImportVend.VendPhone,"-",""), iplIgnoreBlanks, INPUT-OUTPUT bf-vend.phone).
    RUN pAssignValueC (ipbf-ttImportVend.VendFaxAreaCode, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.fax-area).
    RUN pAssignValueC (REPLACE(ipbf-ttImportVend.VendFax,"-",""), iplIgnoreBlanks, INPUT-OUTPUT bf-vend.fax).
    RUN pAssignValueC (ipbf-ttImportVend.TaxGroup, YES, INPUT-OUTPUT bf-vend.tax-gr).
    RUN pAssignValueC (ipbf-ttImportVend.Terms, YES, INPUT-OUTPUT bf-vend.terms).
    RUN pAssignValueC (ipbf-ttImportVend.Code1099, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.code-1099).
    RUN pAssignValueC (ipbf-ttImportVend.Carrier, YES, INPUT-OUTPUT bf-vend.carrier).
    RUN pAssignValueC (ipbf-ttImportVend.GL, YES, INPUT-OUTPUT bf-vend.actnum).
    RUN pAssignValueC (ipbf-ttImportVend.FedID, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.tax-id).
    RUN pAssignValueC (ipbf-ttImportVend.BankAcct, YES, INPUT-OUTPUT bf-vend.Bank-Acct).
    RUN pAssignValueC (ipbf-ttImportVend.SwiftBIC, YES, INPUT-OUTPUT bf-vend.SwiftBIC).
    RUN pAssignValueC (ipbf-ttImportVend.BankRTN, iplIgnoreBlanks, INPUT-OUTPUT bf-vend.Bank-RTN).
    
    IF bf-vend.country = "Canada" THEN 
        bf-vend.curr-code = "CAD".
    ELSE 
        bf-vend.curr-code = "USD".
    RELEASE bf-vend .

END PROCEDURE.

