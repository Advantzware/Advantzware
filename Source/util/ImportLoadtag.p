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
{sys/inc/var.i NEW SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportLoadtag    
    FIELD company      AS CHARACTER 
    /* Required */
    FIELD item-type    AS CHAR        FORMAT "x(2)" LABEL "Item Type" HELP "Required - RM or FG - size:2"
    FIELD i-no         AS CHARACTER   FORMAT "x(15)" LABEL "Item#" HELP "Required - Size:15"
    FIELD i-name       AS CHARACTER   FORMAT "x(30)" LABEL "Name" HELP "Required - Size:30"
    FIELD tag-no       AS CHARACTER   FORMAT "X(20)" LABEL "Tag#" HELP "PREFIX numeric values with the # sign"
    /* Optional */
    FIELD sts          AS CHARACTER   FORMAT "x(12)" LABEL "Status" HELP "Optional - Size:12"
    FIELD tag-date     AS DATE        FORMAT "99/99/9999" LABEL "Rcpt Date" HELP "Optional - Date"
    FIELD lot-no       AS CHARACTER   FORMAT "x(15)" LABEL "FG Lot#" HELP "Optional - Size:15"
    FIELD location     AS CHARACTER   FORMAT "x(5)" LABEL "Warehouse" HELP "Optional - Size:5"
    FIELD loc-bin      AS CHARACTER   FORMAT "x(8)" LABEL "Bin" HELP "Optional - Size:8"
    FIELD po-no        AS INTEGER     FORMAT ">>>>>9" LABEL "PO#" HELP "Optional Integer - Size:6"
    FIELD line         AS INTEGER     FORMAT "99" LABEL "PO Line" HELP "Optional Integer - Size:2"
    FIELD vend-tag     AS CHARACTER   FORMAT "x(20)" LABEL "Vendor Tag#" HELP "Optional - Size:20"
    FIELD job-no       AS CHARACTER   FORMAT "x(9)" LABEL "Job #" HELP "Optional - SizeUpto:9"
    FIELD job-no2      AS INTEGER     FORMAT ">>9" LABEL "Run #" HELP "Optional - Size:3"
    FIELD shift        AS CHARACTER   FORMAT "x" LABEL "Shift" HELP "Optional - Size:1"
    FIELD crew         AS CHARACTER   FORMAT "x(5)" LABEL "Crew#" HELP "Optional - Size:5"
    FIELD ord-no       AS INTEGER     FORMAT ">>>>>9" LABEL "Order#" HELP "Optional Integer - Size:6"
    FIELD qty-case     AS INTEGER     LABEL "Qty/Case" HELP "Optional Integer"
    FIELD case-bundle  AS INTEGER     LABEL "Case/Bundles-Pallet" HELP "Optional Integer"
    FIELD pallet-count AS INTEGER     LABEL "Pallet Count" HELP "Optional Integer"
    FIELD tot-cases    AS INTEGER     LABEL "Total Cases" HELP "Optional Integer"
    FIELD partial      AS INTEGER     FORMAT ">>>,>>9" HELP "Optional Integer"
    FIELD qty          AS DECIMAL     DECIMALS 6 FORMAT "->>>,>>>,>>9.9<<<<<" HELP "Optional Decimal"
    FIELD std-cost     AS DECIMAL     DECIMALS 4 FORMAT ">>>,>>9.99<<" LABEL "Costs" HELP "Optional Decimal"
    FIELD cost-uom     AS CHARACTER   FORMAT "x(3)" LABEL "Cost UOM" HELP "Optional - Size:3"
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

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportLoadtag FOR ttImportLoadtag.

    
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
            AND loadtag.item-type EQ (IF ipbf-ttImportLoadtag.item-type EQ "RM" THEN TRUE ELSE FALSE)
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
            IF ipbf-ttImportLoadtag.item-type EQ "FG" THEN 
                RUN pIsValidFgItemID (ipbf-ttImportLoadtag.i-no,YES,ipbf-ttImportLoadtag.company,output oplValid, OUTPUT cValidNote).
            ELSE IF ipbf-ttImportLoadtag.item-type EQ "RM" THEN 
                RUN pIsValidRmItemID (ipbf-ttImportLoadtag.i-no,YES,ipbf-ttImportLoadtag.company,output oplValid, OUTPUT cValidNote).
        END.
        IF oplValid 
        AND ipbf-ttImportLoadtag.location NE "" THEN 
            RUN pIsValidWarehouse (ipbf-ttImportLoadtag.location,YES,ipbf-ttImportLoadtag.company,output oplValid, OUTPUT cValidNote).
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
    DEFINE BUFFER bf-loadtag FOR loadtag.
     
    ASSIGN ipbf-ttImportLoadtag.job-no = SUBSTRING(ipbf-ttImportLoadtag.job-no,1,iJobLen) NO-ERROR.
    
    IF AVAILABLE ipbf-ttImportLoadtag THEN DO: 
        FIND FIRST bf-loadtag EXCLUSIVE-LOCK
            WHERE bf-loadtag.company EQ ipbf-ttImportLoadtag.company
            AND bf-loadtag.item-type EQ (IF ipbf-ttImportLoadtag.item-type EQ "RM" THEN TRUE ELSE FALSE)
            AND bf-loadtag.tag-no EQ ipbf-ttImportLoadtag.tag-no
            NO-ERROR.  
        IF NOT AVAILABLE bf-loadtag THEN 
        DO:
            iopiAdded = iopiAdded + 1.
            CREATE bf-loadtag.
            
            ASSIGN 
                bf-loadtag.company      = ipbf-ttImportLoadtag.company
                bf-loadtag.item-type    = IF ipbf-ttImportLoadtag.item-type EQ "RM" THEN TRUE ELSE FALSE 
                bf-loadtag.tag-no       = IF SUBSTRING(ipbf-ttImportLoadtag.tag-no,1,1) EQ "#" THEN SUBSTRING(ipbf-ttImportLoadtag.tag-no,2) ELSE ipbf-ttImportLoadtag.tag-no 
                bf-loadtag.i-name       = ipbf-ttImportLoadtag.i-name
                bf-loadtag.i-no         = ipbf-ttImportLoadtag.i-no
                bf-loadtag.tag-time     = IF ipbf-ttImportLoadtag.tag-date NE ? THEN 0 ELSE ?
                .
        END.
        
        RUN pAssignValueC (ipbf-ttImportloadtag.cost-uom, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.cost-uom). 
        RUN pAssignValueC (ipbf-ttImportloadtag.crew, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.crew). 
        RUN pAssignValueC (ipbf-ttImportloadtag.cust-po-no, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.cust-po-no). 
        RUN pAssignValueC (ipbf-ttImportloadtag.job-no, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.job-no).
        RUN pAssignValueC (ipbf-ttImportloadtag.loc-bin, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.loc-bin).
        RUN pAssignValueC (ipbf-ttImportloadtag.location, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.loc).
        RUN pAssignValueC (ipbf-ttImportloadtag.lot-no, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.misc-char[2]). 
        RUN pAssignValueC (ipbf-ttImportloadtag.pallet-no, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.pallet-no). 
        RUN pAssignValueC (ipbf-ttImportloadtag.shift, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.shift). 
        RUN pAssignValueC (ipbf-ttImportloadtag.sts, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.sts). 
        RUN pAssignValueC (ipbf-ttImportloadtag.vend-tag, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.misc-char[1]). 
        RUN pAssignValueI (ipbf-ttImportloadtag.blank-no, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.blank-no). 
        RUN pAssignValueI (ipbf-ttImportloadtag.case-bundle, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.case-bundle). 
        RUN pAssignValueI (ipbf-ttImportloadtag.form-no, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.form-no). 
        RUN pAssignValueI (ipbf-ttImportloadtag.job-no2, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.job-no2). 
        RUN pAssignValueI (ipbf-ttImportloadtag.line, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.line). 
        RUN pAssignValueI (ipbf-ttImportloadtag.ord-no, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.ord-no). 
        RUN pAssignValueI (ipbf-ttImportloadtag.pallet-count, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.pallet-count). 
        RUN pAssignValueI (ipbf-ttImportloadtag.partial, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.partial). 
        RUN pAssignValueI (ipbf-ttImportloadtag.po-no, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.po-no). 
        RUN pAssignValueI (ipbf-ttImportloadtag.qty-case, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.qty-case). 
        RUN pAssignValueI (ipbf-ttImportloadtag.tot-cases, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.tot-cases). 
        RUN pAssignValueD (ipbf-ttImportloadtag.unit-wt, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.misc-dec[1]). 
        RUN pAssignValueD (ipbf-ttImportloadtag.tare-wt, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.misc-dec[2]). 
        RUN pAssignValueD (ipbf-ttImportloadtag.gross-wt, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.misc-dec[3]). 
        RUN pAssignValueD (ipbf-ttImportloadtag.qty, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.qty). 
        RUN pAssignValueD (ipbf-ttImportloadtag.std-cost, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.std-cost). 
        RUN pAssignValueCToL (ipbf-ttImportloadtag.completed, "Y", iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.completed). 
        RUN pAssignValueCToL (ipbf-ttImportloadtag.is-case-tag, "R", iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.is-case-tag). 
        RUN pAssignValueDate (ipbf-ttImportloadtag.tag-date, iplIgnoreBlanks, INPUT-OUTPUT bf-loadtag.tag-date). 

        ASSIGN
            bf-loadtag.upd-date     = today
            bf-loadtag.upd-time     = time
            .
            
        RELEASE bf-loadtag.
    END.
    
END PROCEDURE.
