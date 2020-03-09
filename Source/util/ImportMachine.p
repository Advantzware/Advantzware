
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

DEFINE TEMP-TABLE ttImportMach
    FIELD Company                 AS CHARACTER 
    FIELD Location                AS CHARACTER 
    FIELD m-code                  AS CHARACTER FORMAT "x(6)" COLUMN-LABEL "Machine Code" HELP "Required - Size:6" 
    FIELD m-dscr                  AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Machine Description" HELP "Optional - Size:20"
    FIELD dept1                   AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department(1)" HELP "Optional - - Size:2"
    FIELD dept2                   AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department(2)" HELP "Optional - Size:2"
    FIELD dept3                   AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department(3)" HELP "Optional - Size:2"
    FIELD dept4                   AS CHARACTER FORMAT "x(2)" COLUMN-LABEL "Department(4)" HELP "Optional - Size:2"
    FIELD loc                     AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Location" HELP "Required - Size:5"
    FIELD sch-m-code              AS CHARACTER FORMAT "x(6)" COLUMN-LABEL "Schedule Machine" HELP "Required - Size:6"
    FIELD d-seq                   AS INTEGER FORMAT ">9" COLUMN-LABEL "Dept. Sequence" HELP "Optional - Integer"
    FIELD m-seq                   AS INTEGER FORMAT "99" COLUMN-LABEL "Mach Seq" HELP "Optional - Integer"
    FIELD p-type                  AS CHARACTER FORMAT "x" COLUMN-LABEL "Feed" HELP "Required - Size:1"
    FIELD run-spoil               AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Run Spoilage %" HELP "Optional - Decimal"
    FIELD mr-waste                AS INTEGER FORMAT ">>>9" COLUMN-LABEL "MR Waste" HELP "Optional - Integer"
    FIELD daily-prod-hours        AS INTEGER FORMAT "->,>>>,>>9" COLUMN-LABEL "Lag TIME" HELP "Optional - Integer"
    FIELD therm                   AS CHARACTER FORMAT "Yes/No" COLUMN-LABEL "Use Lineal Feet in RUN Matrix" HELP "Optional - Logical Default No"
    FIELD lab-rate1               AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Labor Rate" HELP "Optional - Decimal"
    FIELD lab-rate2               AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Labor Rate(2)" HELP "Optional - Decimal"
    FIELD lab-rate3               AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Labor Rate(3)" HELP "Optional - Decimal"
    FIELD mrk-rate                AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "MIN Charge" HELP "Optional - Decimal"
    FIELD mr-crusiz               AS DECIMAL FORMAT ">9.99" COLUMN-LABEL "Setup Crew" HELP "Optional - Decimal" 
    FIELD run-crusiz              AS DECIMAL FORMAT ">9.99" COLUMN-LABEL "Run Crew" HELP "Optional - Decimal"
    FIELD lab-drate               AS INTEGER FORMAT ">" COLUMN-LABEL "Default" HELP "Optional - Integer"
    FIELD mr-rate                 AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Setup D.L." HELP "Optional - Decimal"
    FIELD mr-varoh                AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Var OH" HELP "Optional - Decimal"  
    FIELD mr-fixoh                AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Fixed OH" HELP "Optional - Decimal"
    FIELD mr-trate                AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "MR Total" HELP "Optional - Decimal"
    FIELD run-rate                AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Run D.L." HELP "Optional - Decimal"
    FIELD run-varoh               AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Run Var OH" HELP "Optional - Decimal"
    FIELD run-fixoh               AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Run Fixed OH" HELP "Optional - Decimal"
    FIELD run-trate               AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Run Total Rate" HELP "Optional - Decimal"
    FIELD min-len                 AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Front-To-Back(MIN)" HELP "Optional - Decimal"
    FIELD max-len                 AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Front-To-Back(MAX)" HELP "Optional - Decimal"
    FIELD min-wid                 AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Side-To-Side(MIN WIDTH)" HELP "Optional - Decimal"
    FIELD max-wid                 AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Side-To-Side(MAX Width)" HELP "Optional - Decimal"   
    FIELD min-triml               AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Side-To-Side(Trim Lgth)" HELP "Optional - Decimal" 
    FIELD min-trimw               AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Front-To-Back(Trim)" HELP "Optional - Decimal"
    FIELD min-cal                 AS DECIMAL FORMAT "9.99999" COLUMN-LABEL "Caliper/Depth(MIN)" HELP "Optional - Decimal"
    FIELD max-cal                 AS DECIMAL FORMAT "9.99999" COLUMN-LABEL "Caliper/Depth(Max)" HELP "Optional - Decimal"
    FIELD min-run                 AS INTEGER FORMAT ">>,>>>,>>9" COLUMN-LABEL "Run Qty(MIN)" HELP "Optional - Integer"
    FIELD max-run                 AS INTEGER FORMAT ">>,>>>,>>9" COLUMN-LABEL "Run Qty(MAX)" HELP "Optional - Integer"
    FIELD min-pan-l               AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Slot/Score Panel(MIN)" HELP "Optional - Decimal"
    FIELD max-pan-l               AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Slot/Score Panel(Max)" HELP "Optional - Decimal"
    FIELD min-pan-w               AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Min Panel (Hd-Hd)" HELP "Optional - Decimal"
    FIELD max-pan-w               AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Max Panel (Hd-Hd)" HELP "Optional - Decimal"
    FIELD min-dep                 AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Slot SIZE(MIN)" HELP "Optional - Decimal"
    FIELD max-dep                 AS DECIMAL FORMAT ">>9.99999" COLUMN-LABEL "Slot Size(Max)" HELP "Optional - Decimal"
    FIELD pr-type                 AS CHARACTER FORMAT "x" COLUMN-LABEL "Printer Type" HELP "Optional - Size:1"
    FIELD washup                  AS DECIMAL FORMAT ">9.99" COLUMN-LABEL "Washup Hrs" HELP "Optional - Decimal"
    FIELD col-pass                AS CHARACTER FORMAT "x" COLUMN-LABEL "Color/Pass" HELP "Required - Size:1"
    FIELD max-color               AS INTEGER FORMAT ">9" COLUMN-LABEL "Max # of colors" HELP "Optional - Integer"
    FIELD coater                  AS CHARACTER FORMAT "x" COLUMN-LABEL "Coater on Press" HELP "Required - Logical Format Y/N"
    FIELD col-wastesh             AS INTEGER FORMAT ">>>9" COLUMN-LABEL "MR Waste per Color" HELP "Optional - Integer"
    FIELD ink-waste               AS DECIMAL FORMAT ">9.999" COLUMN-LABEL "Ink Waste Lbs\MR" HELP "Optional - Decimal"
    FIELD col-wastelb             AS DECIMAL FORMAT ">9.999" COLUMN-LABEL "Lbs/Color" HELP "Optional - Decimal"
    FIELD tan-mrp                 AS DECIMAL FORMAT ">9.99" COLUMN-LABEL "Tandem MR/Plate" HELP "Optional - Decimal"
    FIELD tan-mrf                 AS DECIMAL FORMAT ">9.99" COLUMN-LABEL "/Fountain" HELP "Optional - Decimal"
    FIELD num-wid                 AS INTEGER FORMAT ">9" COLUMN-LABEL "MAX Num WIDTH" HELP "Optional - Integer"
    FIELD num-len                 AS INTEGER FORMAT ">9" COLUMN-LABEL "MAX Num on Length" HELP "Optional - Integer"
    FIELD industry                AS CHARACTER FORMAT "x" COLUMN-LABEL "Industry" HELP "Optional - Size:1"
    FIELD gang-jobs               AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Gang Jobs?" HELP "Optional - Logical Default - No "
    FIELD plan-job                AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Plain Jobs Only" HELP "Optional - Logical Default - No"
    FIELD Obsolete                AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Inactive" HELP "Optional - Logical Default - No"
    
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportMach"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportMach FOR ttImportMach.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.
    DEFINE BUFFER bf-mach FOR mach.

    FIND FIRST bf-mach EXCLUSIVE-LOCK 
        WHERE bf-mach.company EQ ipbf-ttImportMach.Company
        AND bf-mach.m-code EQ ipbf-ttImportMach.m-code
        NO-ERROR.

    IF NOT AVAILABLE bf-mach THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE bf-mach.
        ASSIGN 
            bf-mach.company   = ipbf-ttImportMach.Company
            bf-mach.loc       = ipbf-ttImportMach.loc
            bf-mach.m-code      = ipbf-ttImportMach.m-code .
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                                         
    RUN pAssignValueC (ipbf-ttImportMach.m-dscr, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.m-dscr).                                                   
    RUN pAssignValueC (ipbf-ttImportMach.dept1, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.dept[1]).                               
    RUN pAssignValueC (ipbf-ttImportMach.dept2, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.dept[2]).                                                   
    RUN pAssignValueC (ipbf-ttImportMach.dept3, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.dept[3]).                                                   
    RUN pAssignValueC (ipbf-ttImportMach.dept4, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.dept[4]).                                       
    RUN pAssignValueC (ipbf-ttImportMach.loc, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.loc).                                         
    RUN pAssignValueC (ipbf-ttImportMach.sch-m-code, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.sch-m-code).                      
    RUN pAssignValueI (ipbf-ttImportMach.d-seq, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.d-seq).                                 
    RUN pAssignValueI (ipbf-ttImportMach.m-seq, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.m-seq).                                     
    RUN pAssignValueC (ipbf-ttImportMach.p-type, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.p-type).                                                 
    RUN pAssignValueD (ipbf-ttImportMach.run-spoil, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.run-spoil).                   
    RUN pAssignValueI (ipbf-ttImportMach.mr-waste, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.mr-waste).                                         
    RUN pAssignValueI (ipbf-ttImportMach.daily-prod-hours, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.daily-prod-hours).                                 
    RUN pAssignValueD (ipbf-ttImportMach.lab-rate1, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.lab-rate[1]).                                       
    RUN pAssignValueD (ipbf-ttImportMach.lab-rate2, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.lab-rate[2]).                                 
    RUN pAssignValueD (ipbf-ttImportMach.lab-rate3, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.lab-rate[3]).                                   
    RUN pAssignValueD (ipbf-ttImportMach.mrk-rate, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.mrk-rate).                             
    RUN pAssignValueD (ipbf-ttImportMach.mr-crusiz, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.mr-crusiz).                                 
    RUN pAssignValueD (ipbf-ttImportMach.run-crusiz, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.run-crusiz).                             
    RUN pAssignValueI (ipbf-ttImportMach.lab-drate, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.lab-drate).                                 
    RUN pAssignValueD (ipbf-ttImportMach.mr-rate, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.mr-rate).                                          
    RUN pAssignValueD (ipbf-ttImportMach.mr-varoh, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.mr-varoh).                                         
    RUN pAssignValueD (ipbf-ttImportMach.mr-fixoh, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.mr-fixoh).                         
    RUN pAssignValueD (ipbf-ttImportMach.mr-trate, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.mr-trate).                                 
    RUN pAssignValueD (ipbf-ttImportMach.run-rate, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.run-rate).                                          
    RUN pAssignValueD (ipbf-ttImportMach.run-varoh, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.run-varoh).                                     
    RUN pAssignValueD (ipbf-ttImportMach.run-fixoh, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.run-fixoh).                             
    RUN pAssignValueD (ipbf-ttImportMach.run-trate, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.run-trate).                                           
    RUN pAssignValueD (ipbf-ttImportMach.min-len, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.min-len).                                                 
    RUN pAssignValueD (ipbf-ttImportMach.max-len, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.max-len).                             
    RUN pAssignValueD (ipbf-ttImportMach.min-wid, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.min-wid).                             
    RUN pAssignValueD (ipbf-ttImportMach.max-wid, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.max-wid).                             
    RUN pAssignValueD (ipbf-ttImportMach.min-triml, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.min-triml).                                           
    RUN pAssignValueD (ipbf-ttImportMach.min-trimw, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.min-trimw).                                                 
    RUN pAssignValueD (ipbf-ttImportMach.min-cal, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.min-cal).                                         
    RUN pAssignValueD (ipbf-ttImportMach.max-cal, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.max-cal).                     
    RUN pAssignValueI (ipbf-ttImportMach.min-run, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.min-run).                                           
    RUN pAssignValueI (ipbf-ttImportMach.max-run, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.max-run).                         
    RUN pAssignValueD (ipbf-ttImportMach.min-pan-l, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.min-pan-l).                 
    RUN pAssignValueD (ipbf-ttImportMach.max-pan-l, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.max-pan-l).                         
    RUN pAssignValueD (ipbf-ttImportMach.min-pan-w, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.min-pan-w).                                       
    RUN pAssignValueD (ipbf-ttImportMach.max-pan-w, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.max-pan-w).                         
    RUN pAssignValueD (ipbf-ttImportMach.min-dep, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.min-dep).                                 
    RUN pAssignValueD (ipbf-ttImportMach.max-dep, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.max-dep).
    RUN pAssignValueC (ipbf-ttImportMach.pr-type, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.pr-type).
    RUN pAssignValueD (ipbf-ttImportMach.washup, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.washup).
    RUN pAssignValueC (ipbf-ttImportMach.col-pass, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.col-pass).
    RUN pAssignValueI (ipbf-ttImportMach.max-color, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.max-color).
    RUN pAssignValueI (ipbf-ttImportMach.col-wastesh, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.col-wastesh).
    RUN pAssignValueD (ipbf-ttImportMach.ink-waste, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.ink-waste).
    RUN pAssignValueD (ipbf-ttImportMach.col-wastelb, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.col-wastelb).
    RUN pAssignValueD (ipbf-ttImportMach.tan-mrp, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.tan-mrp).
    RUN pAssignValueD (ipbf-ttImportMach.tan-mrf, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.tan-mrf).
    RUN pAssignValueI (ipbf-ttImportMach.num-wid, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.num-wid).
    RUN pAssignValueI (ipbf-ttImportMach.num-len, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.num-len).
    RUN pAssignValueC (ipbf-ttImportMach.industry, iplIgnoreBlanks, INPUT-OUTPUT bf-mach.industry).
    
    
    IF ipbf-ttImportMach.therm   EQ "Yes" THEN
        ASSIGN bf-mach.therm   = YES.
    ELSE bf-mach.therm = NO .
    
    IF ipbf-ttImportMach.coater   EQ "Y" THEN
        ASSIGN bf-mach.coater   = YES.
    ELSE bf-mach.coater = NO .
    
    IF ipbf-ttImportMach.gang-jobs   EQ "Yes" THEN
        ASSIGN bf-mach.gang-jobs   = YES.
    ELSE bf-mach.gang-jobs = NO .
    
    IF ipbf-ttImportMach.plan-job   EQ "Yes" THEN
        ASSIGN bf-mach.plain-job   = YES.
    ELSE bf-mach.plain-job = NO .
    
    IF ipbf-ttImportMach.Obsolete   EQ "Yes" THEN
        ASSIGN bf-mach.Obsolete   = YES.
    ELSE bf-mach.Obsolete = NO .      

   RELEASE bf-mach .
                                                                                                                               
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportMach FOR ttImportMach.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportMach FOR ttImportMach.


    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportMach.m-CODE EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Prep Code is Blank".
    END.  
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportMach.loc EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Location is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportMach.sch-m-code EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Schedule Machine is Blank".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportMach NO-LOCK 
            WHERE bf-ttImportMach.Company EQ ipbf-ttImportMach.Company
            AND bf-ttImportMach.m-code EQ ipbf-ttImportMach.m-code
            AND ROWID(bf-ttImportMach) NE ROWID(ipbf-ttImportMach)
            NO-ERROR.
        IF AVAILABLE bf-ttImportMach THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST mach NO-LOCK 
            WHERE mach.company EQ ipbf-ttImportMach.Company
            AND mach.m-code EQ ipbf-ttImportMach.m-code
            NO-ERROR .
        IF AVAIL mach THEN
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
        IF oplValid AND ipbf-ttImportMach.dept1 NE "" THEN 
            RUN pIsValidDept (ipbf-ttImportMach.dept1, YES, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportMach.dept2 NE "" THEN 
            RUN pIsValidDept (ipbf-ttImportMach.dept2, YES, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportMach.dept3 NE "" THEN 
            RUN pIsValidDept (ipbf-ttImportMach.dept3, YES, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportMach.dept4 NE "" THEN 
            RUN pIsValidDept (ipbf-ttImportMach.dept4, YES, OUTPUT oplValid, OUTPUT cValidNote).              
        
        IF oplValid AND ipbf-ttImportMach.p-type NE "" THEN 
            RUN pIsValidFromList ("FEED" ,ipbf-ttImportMach.p-type, "S,R,B,P,A", OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportMach.pr-type NE "" THEN 
            RUN pIsValidFromList ("Printer Type" ,ipbf-ttImportMach.pr-type, "O,L,F,S,G", OUTPUT oplValid, OUTPUT cValidNote).               
        IF oplValid AND ipbf-ttImportMach.col-pass NE "" THEN 
            RUN pIsValidFromList ("Color/Pass" ,ipbf-ttImportMach.col-pass, "C,P", OUTPUT oplValid, OUTPUT cValidNote).               
        IF oplValid AND ipbf-ttImportMach.industry NE "" THEN 
            RUN pIsValidFromList ("Industry" ,ipbf-ttImportMach.industry, ",1,2,X", OUTPUT oplValid, OUTPUT cValidNote).                   
                
        IF oplValid AND ipbf-ttImportMach.loc NE "" THEN 
            RUN pIsValidWarehouse (ipbf-ttImportMach.loc, NO, ipbf-ttImportMach.Company, OUTPUT oplValid, OUTPUT cValidNote).      
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
END PROCEDURE.

