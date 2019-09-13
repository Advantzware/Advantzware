/*------------------------------------------------------------------------
    File        : ImportLoadtag.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Loadtag	
    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportLoadtag    
    FIELD company      AS CHARACTER 
    FIELD item-type    AS LOGICAL     FORMAT "R/F" INITIAL TRUE LABEL "Item Type" HELP "Required - Logical, TRUE for RM, FALSE for FG"
    FIELD i-no         AS CHARACTER   FORMAT "x(15)" LABEL "Item#" HELP "Required - Size:15"
    FIELD i-name       AS CHARACTER   FORMAT "x(30)" LABEL "Name" HELP "Required - Size:30"
    FIELD tag-no       AS CHARACTER   FORMAT "X(20)" LABEL "Tag#" HELP "Required - Size:20"
    FIELD tag-date     AS DATE        FORMAT "99/99/9999" LABEL "Rcpt Date" HELP "Optional - Date"
    FIELD tag-time     AS INTEGER     LABEL "Rcpt Time" HELP "Optional - Integer"
    FIELD lot-no       AS CHARACTER   FORMAT "x(15)" LABEL "FG Lot#" HELP "Optional - Size:15"
    FIELD sts          AS CHARACTER   FORMAT "x" LABEL "Status" HELP "Required - Size:1, one of 'PROBICDT'"
    FIELD location     AS CHARACTER   FORMAT "x(5)" LABEL "Warehouse" HELP "Required - Size:5"
    FIELD loc-bin      AS CHARACTER   FORMAT "x(8)" LABEL "Bin" HELP "Required - Size:8"
    FIELD po-no        AS INTEGER     FORMAT ">>>>>9" LABEL "PO#" HELP "Optional Integer - Size:6"
    FIELD line         AS INTEGER     FORMAT "99" LABEL "PO Line" HELP "Optional Integer - Size:2"
    FIELD vend-tag     AS CHARACTER   FORMAT "x(20)" LABEL "Vendor Tag#" HELP "Optional - Size:20"
    FIELD job-no       AS CHARACTER   FORMAT "x(6)" LABEL "Job #" HELP "Optional - Size:6"
    FIELD job-no2      AS INTEGER     FORMAT ">9" LABEL "Run #" HELP "Optional - Size:2"
    FIELD shift        AS CHARACTER   FORMAT "x" LABEL "Shift" HELP "Optional - Size:1"
    FIELD crew         AS CHARACTER   FORMAT "x(5)" LABEL "Crew#" HELP "Optional - Size:5"
    FIELD ord-no       AS INTEGER     FORMAT ">>>>>9" LABEL "Order#" HELP "Optional Integer - Size:6"
    FIELD qty-case     AS INTEGER     LABEL "Qty/Case" HELP "Required Integer"
    FIELD case-bundle  AS INTEGER     LABEL "Case/Bundles-Pallet" HELP "Required Integer"
    FIELD pallet-count AS INTEGER     LABEL "Pallet Count" HELP "Required Integer"
    FIELD tot-cases    AS INTEGER     LABEL "Total Cases" HELP "Required Integer"
    FIELD partial      AS INTEGER     FORMAT ">>>,>>9" HELP "Required Integer"
    FIELD qty          AS DECIMAL     DECIMALS 6 FORMAT "->>>,>>>,>>9.9<<<<<" HELP "Required Decimal"
    FIELD std-cost     AS DECIMAL     DECIMALS 4 FORMAT ">>>,>>9.99<<" LABEL "Costs" HELP "Optional Decimal"
    FIELD cost-uom     AS CHARACTER   FORMAT "x(3)" LABEL "Cost UOM" HELP "Required - Size:3"
    FIELD pallet-no    AS CHARACTER   FORMAT "X(20)" LABEL "Pallet#" HELP "Optional - Size:20"
    FIELD unit-wt      AS DECIMAL     FORMAT ">>>,>>9.99<<" LABEL "Net Unit Wt" HELP "Optional - Decimal"
    FIELD tare-wt      AS DECIMAL     FORMAT ">>>,>>9.99<<" LABEL "Pallet Wt" HELP "Optional - Decimal"
    FIELD gross-wt     AS DECIMAL     FORMAT ">>>,>>9.99<<" LABEL "Tot. Pall. Wt." HELP "Optional - Decimal"
    FIELD completed    AS LOGICAL     LABEL "Completed" HELP "Optional - Logical"
    FIELD is-case-tag  AS LOGICAL     LABEL "Is Case Tag" HELP "Optional - Logical"
    FIELD cust-po-no   AS CHARACTER   FORMAT "x(15)" LABEL "Customer PO" HELP "Optional - Size:15"
    FIELD blank-no     AS INTEGER     FORMAT ">9" LABEL "Blank#" HELP "Optional - Integer"
    FIELD form-no      AS INTEGER     FORMAT ">>9" INITIAL 1 LABEL "Form#" HELP "Optional - Integer"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER   NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportLoadtag"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportLoadtag FOR ttImportLoadtag.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportLoadtag FOR ttImportLoadtag.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportLoadtag.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportLoadtag.Location EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Location".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportLoadtag.tag-no EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Tag".
    END.
    
    IF oplValid THEN DO:
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company EQ ipbf-ttImportLoadtag.company
            AND loadtag.item-type EQ ipbf-ttImportLoadtag.item-type
            AND loadtag.tag-no EQ ipbf-ttImportLoadtag.tag-no
            NO-ERROR.  
        IF AVAIL loadtag THEN DO:
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
                opcNote = "Add record".
    END.

    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid 
        AND ipbf-ttImportLoadtag.i-no NE "" THEN DO: 
            IF ipbf-ttImportLoadtag.item-type EQ FALSE THEN 
                RUN pIsValidFgItemID IN hdValidator (ipbf-ttImportLoadtag.i-no,YES,ipbf-ttImportLoadtag.company,output oplValid, OUTPUT cValidNote).
            ELSE IF ipbf-ttImportLoadtag.item-type EQ TRUE THEN 
                RUN pIsValidRmItemID IN hdValidator (ipbf-ttImportLoadtag.i-no,YES,ipbf-ttImportLoadtag.company,output oplValid, OUTPUT cValidNote).
        END.
        IF oplValid 
        AND ipbf-ttImportLoadtag.location NE "" THEN 
            RUN pIsValidWarehouse IN hdValidator (ipbf-ttImportLoadtag.location,YES,ipbf-ttImportLoadtag.company,output oplValid, OUTPUT cValidNote).
    END.
    
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportLoadtag FOR ttImportLoadtag.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
     
    IF AVAILABLE ipbf-ttImportLoadtag THEN DO: 
        FIND FIRST loadtag EXCLUSIVE-LOCK
            WHERE loadtag.company EQ ipbf-ttImportLoadtag.company
            AND loadtag.item-type EQ ipbf-ttImportLoadtag.item-type
            AND loadtag.tag-no EQ ipbf-ttImportLoadtag.tag-no
            NO-ERROR.  
        IF NOT AVAILABLE loadtag THEN 
        DO:
            iopiAdded = iopiAdded + 1.
            CREATE loadtag.
        END.
        
        ASSIGN
            loadtag.blank-no     = ipbf-ttImportLoadtag.blank-no
            loadtag.case-bundle  = ipbf-ttImportLoadtag.case-bundle
            loadtag.company      = ipbf-ttImportLoadtag.company
            loadtag.completed    = ipbf-ttImportLoadtag.completed
            loadtag.cost-uom     = ipbf-ttImportLoadtag.cost-uom
            loadtag.crew         = ipbf-ttImportLoadtag.crew
            loadtag.cust-po-no   = ipbf-ttImportLoadtag.cust-po-no
            loadtag.form-no      = ipbf-ttImportLoadtag.form-no
            loadtag.i-name       = ipbf-ttImportLoadtag.i-name
            loadtag.i-no         = ipbf-ttImportLoadtag.i-no
            loadtag.is-case-tag  = ipbf-ttImportLoadtag.is-case-tag
            loadtag.item-type    = ipbf-ttImportLoadtag.item-type
            loadtag.job-no       = ipbf-ttImportLoadtag.job-no
            loadtag.job-no2      = ipbf-ttImportLoadtag.job-no2
            loadtag.line         = ipbf-ttImportLoadtag.line
            loadtag.loc          = ipbf-ttImportLoadtag.location
            loadtag.loc-bin      = ipbf-ttImportLoadtag.loc-bin
            loadtag.misc-char[1] = ipbf-ttImportLoadtag.vend-tag
            loadtag.misc-char[2] = ipbf-ttImportLoadtag.lot-no
            loadtag.misc-dec[1]  = ipbf-ttImportLoadtag.unit-wt
            loadtag.misc-dec[2]  = ipbf-ttImportLoadtag.tare-wt
            loadtag.misc-dec[3]  = ipbf-ttImportLoadtag.gross-wt
            loadtag.ord-no       = ipbf-ttImportLoadtag.ord-no
            loadtag.pallet-count = ipbf-ttImportLoadtag.pallet-count
            loadtag.pallet-no    = ipbf-ttImportLoadtag.pallet-no
            loadtag.partial      = ipbf-ttImportLoadtag.partial
            loadtag.po-no        = ipbf-ttImportLoadtag.po-no
            loadtag.qty          = ipbf-ttImportLoadtag.qty
            loadtag.qty-case     = ipbf-ttImportLoadtag.qty-case
            loadtag.shift        = ipbf-ttImportLoadtag.shift
            loadtag.std-cost     = ipbf-ttImportLoadtag.std-cost
            loadtag.sts          = ipbf-ttImportLoadtag.sts
            loadtag.tag-date     = ipbf-ttImportLoadtag.tag-date
            loadtag.tag-no       = ipbf-ttImportLoadtag.tag-no
            loadtag.tag-time     = ipbf-ttImportLoadtag.tag-time
            loadtag.tot-cases    = ipbf-ttImportLoadtag.tot-cases
            loadtag.upd-date     = today
            loadtag.upd-time     = time
            .
            
        RELEASE loadtag.
    END.
    
END PROCEDURE.
