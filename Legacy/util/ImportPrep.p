
/*------------------------------------------------------------------------
    File        : ImportPrep.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Prep and Die	

    Author(s)   : Sewa Singh
    Created     : Wed Sept 11
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportPrep
    FIELD Company                 AS CHARACTER 
    FIELD Location                AS CHARACTER 
    FIELD CODE                    AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Code" HELP "Required - Size:5" 
    FIELD dscr                    AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Description" HELP "Optional - Size:20"
    FIELD Mat-type                AS CHARACTER FORMAT "x" COLUMN-LABEL "Mat'l Type" HELP "Optional - - Size:1"
    FIELD mat_dscr                AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Mat'l Dscr" HELP "Optional - Size:30"
    FIELD mkup                    AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Markup" HELP "Optional - Decimal"
    FIELD cost                    AS DECIMAL FORMAT "->>,>>9.99" COLUMN-LABEL "Cost" HELP "Optional - Decimal"
    FIELD dfault                 AS CHARACTER FORMAT "x" COLUMN-LABEL "Always" HELP "Optional - Y or N (blank=N)"
    FIELD amtz                    AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Amtz" HELP "Optional - Size:15"
    FIELD ml                      AS CHARACTER FORMAT "X" COLUMN-LABEL "M/L" HELP "Optional - Y or N (blank=N)"
    FIELD uom                     AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM" HELP "Optional - Size:3"
    FIELD uom_dscr                AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "UOM Dscr" HELP "Optional - Size:20"
    FIELD simon                   AS CHARACTER FORMAT "x" COLUMN-LABEL "CSimon" HELP "Optional - Y or N (blank=N)"
    FIELD taxable                 AS CHARACTER FORMAT "x" COLUMN-LABEL "Taxable" HELP "Optional - Y or N (blank=N)"
    FIELD fgcat                   AS CHARACTER FORMAT "x(6)" COLUMN-LABEL "Category" HELP "Optional - Size:6"
    FIELD price                   AS DECIMAL   FORMAT "->>>>,>>9.99" COLUMN-LABEL "Price" HELP "Optional - Decimal"

    FIELD commissionable          AS CHARACTER   FORMAT "x" COLUMN-LABEL "Commission" HELP "Optional - Y or N (blank=N)"
    FIELD loc                     AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "loc" HELP "Optional - Size:5"
    FIELD loc-bin                 AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "loc-bin" HELP "Optional - Size:8"
    FIELD i-no                    AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "RM Item #" HELP "Optional - Size:15"
    FIELD vend-no                 AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Vendor #" HELP "Optional - Size:8" 
    FIELD actnum                  AS CHARACTER FORMAT "X(25)" COLUMN-LABEL "Sales G/L #" HELP "Optional - Size:25"
    FIELD cost-type               AS CHARACTER FORMAT "XXX" COLUMN-LABEL "Cost Type" HELP "Optional - Size:3"
    FIELD costtype_descr          AS CHARACTER   FORMAT "x(15)" COLUMN-LABEL "Cost Type Dscr" HELP "Optional - Size:15"
    FIELD cust-no                 AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Cust. #" HELP "Optional - Size:8"  
    FIELD cust-name               AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Cust Name" HELP "Optional - Size:20"
    FIELD Owner1                  AS CHARACTER FORMAT "x(25)" COLUMN-LABEL "Owner 1" HELP "Optional - Size:25"
    FIELD Owner1%                 AS INTEGER FORMAT ">>9" COLUMN-LABEL "Owner1 %" HELP "Optional - Integer"
    FIELD number-up               AS INTEGER FORMAT ">>9" COLUMN-LABEL "Number Up" HELP "Optional - Integer"
    FIELD no-of-impressions       AS INTEGER FORMAT ">>>,>>9" COLUMN-LABEL "Impressions" HELP "Optional - Integer"
    FIELD Owner2                  AS CHARACTER FORMAT "x(25)" COLUMN-LABEL "Owner 2" HELP "Optional - Size:25"
    FIELD Owner2%                 AS INTEGER FORMAT ">>9" COLUMN-LABEL "Owner2 %" HELP "Optional - Integer"
    FIELD cadNo                   AS CHARACTER   FORMAT "x(12)" COLUMN-LABEL "CAD No" HELP "Optional - Size:12"
    FIELD cad-image               AS CHARACTER   FORMAT "x(80)" COLUMN-LABEL "Image" HELP "Optional - Size:80"
    FIELD carton-w                AS DECIMAL FORMAT "->>,>>9.99<<<<" COLUMN-LABEL "Width" HELP "Optional - Decimal"   
    FIELD die-w                   AS DECIMAL   FORMAT "->>,>>9.99<<<<" COLUMN-LABEL "Die Width" HELP "Optional - Decimal"

    FIELD box-style               AS CHARACTER FORMAT "x(4)" COLUMN-LABEL "Box Style" HELP "Optional - Size:4"
    FIELD Prep-date               AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Prep Date" HELP "Optional - Date"
    FIELD prep-time               AS INTEGER FORMAT "->,>>>,>>9" COLUMN-LABEL "Time" HELP "Optional - Integer"
    FIELD carton-l                AS DECIMAL FORMAT "->>,>>9.99<<<<" COLUMN-LABEL "Length" HELP "Optional - Decimal"
    FIELD die-l                   AS DECIMAL FORMAT "->>,>>9.99<<<<" COLUMN-LABEL "Die Length" HELP "Optional - Decimal"
    FIELD wood-type               AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Wood Type" HELP "Optional - Size:10"
    FIELD received-date           AS CHARACTER   FORMAT "x(10)" COLUMN-LABEL "Last Rec'd" HELP "Optional - Date"
    FIELD carton-d                AS DECIMAL   FORMAT "->>,>>9.99<<<<" COLUMN-LABEL "Depth" HELP "Optional - Decimal"
    FIELD last-date               AS CHARACTER   FORMAT "x(10)" COLUMN-LABEL "Last Used" HELP "Optional - Date"
    FIELD disposal-date           AS CHARACTER   FORMAT "x(10)" COLUMN-LABEL "Disposal" HELP "Optional - Date"
    FIELD last-est-no             AS CHARACTER   FORMAT "x(5)" COLUMN-LABEL "Last Est." HELP "Optional - Size:5"
    FIELD last-order              AS INTEGER   FORMAT "->,>>>,>>9" COLUMN-LABEL "Last Order" HELP "Optional - integer"
    FIELD last-job-no             AS CHARACTER   FORMAT "x(6)" COLUMN-LABEL "Job#" HELP "Optional - Size:6"
    FIELD last-job-no2            AS INTEGER   FORMAT ">9" COLUMN-LABEL "Job2" HELP "Optional - Integer"
    
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportPrep"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportPrep FOR ttImportPrep.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.

    FIND FIRST prep EXCLUSIVE-LOCK 
        WHERE prep.company EQ ipbf-ttImportPrep.Company
        AND prep.CODE EQ ipbf-ttImportPrep.CODE
        NO-ERROR.

    IF NOT AVAILABLE prep THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE prep.
        ASSIGN 
            prep.company   = ipbf-ttImportPrep.Company
            prep.loc       = ipbf-ttImportPrep.Location
            prep.CODE      = ipbf-ttImportPrep.CODE .
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                        
    RUN pAssignValueC (ipbf-ttImportPrep.code, YES, INPUT-OUTPUT prep.code).                                                   
    RUN pAssignValueC (ipbf-ttImportPrep.dscr, YES, INPUT-OUTPUT prep.dscr).                                                   
    RUN pAssignValueC (ipbf-ttImportPrep.mat-type, iplIgnoreBlanks, INPUT-OUTPUT prep.mat-type).                               
    RUN pAssignValueD (ipbf-ttImportPrep.mkup, YES, INPUT-OUTPUT prep.mkup).                                                   
    RUN pAssignValueD (ipbf-ttImportPrep.cost, YES, INPUT-OUTPUT prep.cost).                                                   
    RUN pAssignValueC (ipbf-ttImportPrep.amtz, iplIgnoreBlanks, INPUT-OUTPUT prep.amtz).                                       
    RUN pAssignValueC (ipbf-ttImportPrep.uom, iplIgnoreBlanks, INPUT-OUTPUT prep.uom).                                         
    RUN pAssignValueC (substring(ipbf-ttImportPrep.simon,1,1), iplIgnoreBlanks, INPUT-OUTPUT prep.simon).                      
    RUN pAssignValueC (ipbf-ttImportPrep.taxable, iplIgnoreBlanks, INPUT-OUTPUT prep.taxable).                                 
    RUN pAssignValueC (ipbf-ttImportPrep.fgcat, iplIgnoreBlanks, INPUT-OUTPUT prep.fgcat).                                     
    RUN pAssignValueD (ipbf-ttImportPrep.price, YES, INPUT-OUTPUT prep.price).                                                 
    RUN pAssignValueC (ipbf-ttImportPrep.commissionable, iplIgnoreBlanks, INPUT-OUTPUT prep.commissionable).                   
    RUN pAssignValueC (ipbf-ttImportPrep.loc, iplIgnoreBlanks, INPUT-OUTPUT prep.loc).                                         
    RUN pAssignValueC (ipbf-ttImportPrep.loc-bin, iplIgnoreBlanks, INPUT-OUTPUT prep.loc-bin).                                 
    RUN pAssignValueC (ipbf-ttImportPrep.i-no, iplIgnoreBlanks, INPUT-OUTPUT prep.i-no).                                       
    RUN pAssignValueC (ipbf-ttImportPrep.vend-no, iplIgnoreBlanks, INPUT-OUTPUT prep.vend-no).                                 
    RUN pAssignValueC (ipbf-ttImportPrep.actnum, iplIgnoreBlanks, INPUT-OUTPUT prep.actnum).                                   
    RUN pAssignValueC (ipbf-ttImportPrep.cost-type, iplIgnoreBlanks, INPUT-OUTPUT prep.cost-type).                             
    RUN pAssignValueC (ipbf-ttImportPrep.cust-no, iplIgnoreBlanks, INPUT-OUTPUT prep.cust-no).                                 
    RUN pAssignValueC (ipbf-ttImportPrep.cust-name, iplIgnoreBlanks, INPUT-OUTPUT prep.cust-name).                             
    RUN pAssignValueC (ipbf-ttImportPrep.owner1, iplIgnoreBlanks, INPUT-OUTPUT prep.owner[1]).                                 
    RUN pAssignValueI (ipbf-ttImportPrep.owner1%, YES, INPUT-OUTPUT prep.owner-%[1]).                                          
    RUN pAssignValueI (ipbf-ttImportPrep.number-up, YES, INPUT-OUTPUT prep.number-up).                                         
    RUN pAssignValueI (ipbf-ttImportPrep.no-of-impressions, YES, INPUT-OUTPUT prep.no-of-impressions).                         
    RUN pAssignValueC (ipbf-ttImportPrep.owner2, iplIgnoreBlanks, INPUT-OUTPUT prep.owner[2]).                                 
    RUN pAssignValueI (ipbf-ttImportPrep.owner2%, YES, INPUT-OUTPUT prep.owner-%[2]).                                          
    RUN pAssignValueC (ipbf-ttImportPrep.cadNo, iplIgnoreBlanks, INPUT-OUTPUT prep.cadNo).                                     
    RUN pAssignValueC (ipbf-ttImportPrep.cad-image, iplIgnoreBlanks, INPUT-OUTPUT prep.cad-image).                             
    RUN pAssignValueD (ipbf-ttImportPrep.carton-w, YES, INPUT-OUTPUT prep.carton-w).                                           
    RUN pAssignValueD (ipbf-ttImportPrep.die-w, YES, INPUT-OUTPUT prep.die-w).                                                 
    RUN pAssignValueC (ipbf-ttImportPrep.box-style, iplIgnoreBlanks, INPUT-OUTPUT prep.box-style).                             
    RUN pAssignValueCToDt (ipbf-ttImportPrep.prep-date, iplIgnoreBlanks, INPUT-OUTPUT prep.prep-date).                             
    RUN pAssignValueI (ipbf-ttImportPrep.prep-time, iplIgnoreBlanks, INPUT-OUTPUT prep.prep-time).                             
    RUN pAssignValueD (ipbf-ttImportPrep.carton-l, YES, INPUT-OUTPUT prep.carton-l).                                           
    RUN pAssignValueD (ipbf-ttImportPrep.die-l, YES, INPUT-OUTPUT prep.die-l).                                                 
    RUN pAssignValueC (ipbf-ttImportPrep.wood-type, YES, INPUT-OUTPUT prep.wood-type).                                         
    RUN pAssignValueCToDt (ipbf-ttImportPrep.received-date, iplIgnoreBlanks, INPUT-OUTPUT prep.received-date).                     
    RUN pAssignValueD (ipbf-ttImportPrep.carton-d, YES, INPUT-OUTPUT prep.carton-d).                                           
    RUN pAssignValueCToDt (ipbf-ttImportPrep.last-date, iplIgnoreBlanks, INPUT-OUTPUT prep.last-date).                         
    RUN pAssignValueCToDt (ipbf-ttImportPrep.disposal-date, iplIgnoreBlanks, INPUT-OUTPUT prep.disposal-date).                 
    RUN pAssignValueC (ipbf-ttImportPrep.last-est-no, iplIgnoreBlanks, INPUT-OUTPUT prep.last-est-no).                         
    RUN pAssignValueI (ipbf-ttImportPrep.last-order, YES, INPUT-OUTPUT prep.last-order).                                       
    RUN pAssignValueC (ipbf-ttImportPrep.last-job-no, iplIgnoreBlanks, INPUT-OUTPUT prep.last-job-no).                         
    RUN pAssignValueI (ipbf-ttImportPrep.last-job-no2, YES, INPUT-OUTPUT prep.last-job-no2).                                 
    RUN pAssignValueC (ipbf-ttImportPrep.dfault, iplIgnoreBlanks, INPUT-OUTPUT prep.dfault).                                   
    IF ipbf-ttImportPrep.ml EQ "M" THEN
        ASSIGN prep.ml = YES.
    ELSE prep.ml = NO .



                                                                                                                               
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportPrep FOR ttImportPrep.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE NO-UNDO.
    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportPrep FOR ttImportPrep.

    RUN util/Validate.p PERSISTENT SET hdValidator.

    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportPrep.CODE EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Prep Code is Blank".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportPrep NO-LOCK 
            WHERE bf-ttImportPrep.Company EQ ipbf-ttImportPrep.Company
            AND bf-ttImportPrep.CODE EQ ipbf-ttImportPrep.CODE
            AND ROWID(bf-ttImportPrep) NE ROWID(ipbf-ttImportPrep)
            NO-ERROR.
        IF AVAILABLE bf-ttImportPrep THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST Prep NO-LOCK 
            WHERE Prep.company EQ ipbf-ttImportPrep.Company
            AND Prep.CODE EQ ipbf-ttImportPrep.CODE
            NO-ERROR .
        IF AVAIL prep THEN
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
        
        IF oplValid AND ipbf-ttImportPrep.actnum NE "" THEN 
            RUN pIsValidGLAccount IN hdValidator (ipbf-ttImportPrep.actnum, NO, ipbf-ttImportPrep.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportPrep.i-no NE "" THEN 
            RUN pIsValidRMITemID IN hdValidator (ipbf-ttImportPrep.i-no, NO, ipbf-ttImportPrep.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportPrep.fgcat NE "" THEN 
            RUN pIsValidFGCategory IN hdValidator (ipbf-ttImportPrep.fgcat, NO, ipbf-ttImportPrep.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportPrep.vend-no NE "" THEN 
            RUN pIsValidVendor IN hdValidator (ipbf-ttImportPrep.vend-no, NO, ipbf-ttImportPrep.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportPrep.mat-type NE "" THEN 
            RUN pIsValidMatType IN hdValidator (ipbf-ttImportPrep.mat-type, NO, ipbf-ttImportPrep.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportPrep.cost-type NE "" THEN 
            RUN pIsValidCostType IN hdValidator (ipbf-ttImportPrep.cost-type, NO, ipbf-ttImportPrep.Company, OUTPUT oplValid, OUTPUT cValidNote).
      
        IF oplValid AND ipbf-ttImportPrep.cust-no NE "" THEN 
            RUN pIsValidCustomerID IN hdValidator (ipbf-ttImportPrep.cust-no, NO, ipbf-ttImportPrep.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.

END PROCEDURE.

