
/*------------------------------------------------------------------------
    File        : ImportEstimate.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Estimates	

    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportVend
    FIELD Company          AS CHARACTER FORMAT "x(3)"
    FIELD Location        AS CHARACTER 
    FIELD cVendNo          AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Vendor"  
    FIELD cVendName        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Vendor Name"
    FIELD cVendAdd1        AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Vendor Addr 1"       
    FIELD cVendAdd2        AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Vendor Addr 2"       
    FIELD cVendCity        AS CHARACTER FORMAT "X(16)" COLUMN-LABEL "City"                     
    FIELD cVendState       AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "State"                     
    FIELD cVendCountry     AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Country"  
    FIELD cVendPostal      AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "Postal Code" 
    FIELD cVendZip         AS CHARACTER FORMAT "xxxxx-xxxx" COLUMN-LABEL "Zip Code" 
    FIELD cVendRemit       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Remit To" 
    FIELD cVendRAdd1       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Remit Addr 1" 
    FIELD cVendRAdd2       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Remit Addr 2" 
    FIELD cVendRCity       AS CHARACTER FORMAT "X(16)" COLUMN-LABEL "Remit City" 
    FIELD cVendRState      AS CHARACTER FORMAT "X(2)" COLUMN-LABEL  "Remit State" 
    FIELD cVendRPostal     AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "Remit Postal Code"  
    FIELD cVendRZip        AS CHARACTER FORMAT "xxxxx-xxxx" COLUMN-LABEL "Remit Zip Code"   
    FIELD cTerms           AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Terms Code"  
    FIELD cGL              AS CHARACTER FORMAT "X(25)" COLUMN-LABEL "Default G/L#" 
    FIELD cVendAreaCode    AS CHARACTER FORMAT "(xxx)" COLUMN-LABEL "Area Code" 
    FIELD cVendPhone       AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Phone" 
    FIELD cVendFaxAreaCode AS CHARACTER FORMAT "(xxx)" COLUMN-LABEL "Fax Area Code" 
    FIELD cVendFax         AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Fax" 
    FIELD c1099            AS CHARACTER FORMAT "x" COLUMN-LABEL "1099 code"
    FIELD cFedID           AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Tax ID#"
    FIELD cType            AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Type"
    FIELD cTaxGroup        AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Tax"
    FIELD cCarrier         AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Carrier"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER   NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/

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
        IF ipbf-ttImportVend.cVendNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Vendor".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportVend.cTaxGroup EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Tax".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST vend NO-LOCK 
            WHERE vend.company EQ ipbf-ttImportVend.Company
            AND vend.vend-no EQ ipbf-ttImportVend.cVendNo
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
        IF oplValid AND ipbf-ttImportVend.cVendState NE "" THEN 
            DO:
            FIND FIRST state NO-LOCK
                WHERE state.state = ipbf-ttImportVend.cVendState
                NO-ERROR.
            IF NOT AVAILABLE state THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid State"
                    .
        END.
        IF oplValid AND ipbf-ttImportVend.cVendRState NE "" THEN 
            DO:
            FIND FIRST state NO-LOCK
                WHERE state.state = ipbf-ttImportVend.cVendRState
                NO-ERROR.
            IF NOT AVAILABLE state THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Remit State"
                    .
        END.
        IF oplValid AND ipbf-ttImportVend.cTerms NE "" THEN 
            DO:
            FIND FIRST terms NO-LOCK 
                WHERE terms.company = ipbf-ttImportVend.Company
                AND terms.t-code EQ ipbf-ttImportVend.cTerms NO-ERROR.
            IF NOT AVAILABLE terms THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Terms"
                    .
        END.
        IF oplValid AND ipbf-ttImportVend.cGL NE "" THEN 
            DO:
            FIND FIRST account NO-LOCK
                WHERE account.company = ipbf-ttImportVend.Company
                AND account.actnum = ipbf-ttImportVend.cGL NO-ERROR.
            IF NOT AVAILABLE account THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid G/L#"
                    .
        END.
        IF oplValid AND ipbf-ttImportVend.c1099 NE "" THEN 
            DO:
            IF LOOKUP(TRIM(ipbf-ttImportVend.c1099),",Y,N") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "1099-code may be space, 'Y', or 'N'... "
                    .
        END.
        IF oplValid AND ipbf-ttImportVend.cType NE "" THEN 
            DO:
            FIND FIRST ventype NO-LOCK
                WHERE ventype.company = ipbf-ttImportVend.Company
                AND ventype.TYPE = ipbf-ttImportVend.cType
                NO-ERROR.
            IF NOT AVAILABLE ventype THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Type"
                    .
        END.
        IF oplValid AND ipbf-ttImportVend.cTaxGroup NE "" THEN 
            DO:
            FIND FIRST stax NO-LOCK
                WHERE stax.company = ipbf-ttImportVend.Company
                AND stax.tax-group = ipbf-ttImportVend.cTaxGroup
                NO-ERROR.
            IF NOT AVAILABLE stax THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Tax Group"
                    .
        END.



    END.
    
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportVend FOR ttImportVend.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    FOR EACH ipbf-ttImportVend NO-LOCK:
        FIND FIRST vend EXCLUSIVE-LOCK
            WHERE vend.company EQ ipbf-ttImportVend.Company
            AND vend.vend-no EQ ipbf-ttImportVend.cVendNo
            NO-ERROR.
        IF NOT AVAILABLE vend THEN 
        DO:
            iopiAdded = iopiAdded + 1.
            CREATE vend.
        END.
        ASSIGN
            vend.company      = ipbf-ttImportVend.Company
            vend.vend-no      = ipbf-ttImportVend.cVendNo
            vend.name         = ipbf-ttImportVend.cVendName
            vend.remit        = ipbf-ttImportVend.cVendName
            vend.add1         = ipbf-ttImportVend.cVendAdd1
            vend.r-add1       = ipbf-ttImportVend.cVendRAdd1
            vend.add2         = ipbf-ttImportVend.cVendAdd2
            vend.r-add2       = ipbf-ttImportVend.cVendRAdd2
            vend.city         = ipbf-ttImportVend.cVendCity
            vend.r-city       = ipbf-ttImportVend.cVendRCity
            vend.state        = ipbf-ttImportVend.cVendState
            vend.r-state      = ipbf-ttImportVend.cVendRState
            vend.country      = ipbf-ttImportVend.cVendCountry
            vend.r-country    = ipbf-ttImportVend.cVendCountry
            vend.postal       = ipbf-ttImportVend.cVendPostal
            vend.r-postal     = ipbf-ttImportVend.cVendRPostal
            vend.zip          = ipbf-ttImportVend.cVendZip
            vend.r-zip        = ipbf-ttImportVend.cVendRZip
            vend.area-code    = ipbf-ttImportVend.cVendAreaCode
            vend.phone        = REPLACE(ipbf-ttImportVend.cVendPhone,"-","")
            vend.fax-area     = ipbf-ttImportVend.cVendFaxAreaCode
            vend.fax          = REPLACE(ipbf-ttImportVend.cVendFax,"-","")
            vend.tax-gr       = ipbf-ttImportVend.cTaxGroup
            vend.terms        = ipbf-ttImportVend.cTerms
            vend.code-1099    = ipbf-ttImportVend.c1099
            vend.loc          = "MAIN"
            vend.frt-pay      = "P"
            vend.carrier      = ipbf-ttImportVend.cCarrier
            vend.actnum       = ipbf-ttImportVend.cGL
            vend.tax-id       = ipbf-ttImportVend.cFedID
            vend.payment-type = "Check"
            .
        IF vend.country = "Canada" THEN 
            vend.curr-code = "CAD".
        ELSE 
            vend.curr-code = "USD".
    END.
    

END PROCEDURE.

