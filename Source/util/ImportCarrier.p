/*------------------------------------------------------------------------
    File        : ImportCarrier.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Carrier	
    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportCarrier
    FIELD Company       AS CHARACTER 
    FIELD Location      AS CHARACTER
    FIELD Carrier       AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Carrier" HELP "Required - Size:8"
    FIELD Dscr      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Description" HELP "Optional - Size:30" 
    FIELD Loc        AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Location" HELP "Required - Size:8" 
    FIELD LocDscr      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Loc Description" HELP "Optional - Size:30" 
    FIELD chg-method      AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Charge Method" HELP "Required - MSF,Pallet,Weight"
    FIELD Inactive      AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Inactive" HELP "Required - Yes or No"
    
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportCarrier"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCarrier FOR ttImportCarrier.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportCarrier FOR ttImportCarrier.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCarrier.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCarrier.Carrier EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Carrier".
    END.
    
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCarrier.Loc EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Location".
    END.
  
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCarrier.chg-method EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Charge Method.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCarrier.Carrier EQ 'FEDX' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Carrier 'FEDX' not allowed to upload..".
    END.
        /*Field level validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportCarrier.Loc NE "" THEN 
            RUN pIsValidWarehouse (ipbf-ttImportCarrier.Loc, YES, ipbf-ttImportCarrier.Company, OUTPUT oplValid, OUTPUT cValidNote).

       IF oplValid AND ipbf-ttImportCarrier.chg-method NE "" THEN 
            RUN pIsValidFromList ("Charge Method",ipbf-ttImportCarrier.chg-method, "MSF,Pallet,Weight",  OUTPUT oplValid, OUTPUT cValidNote).
       
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
    IF oplValid THEN DO:
        FIND FIRST carrier NO-LOCK 
             WHERE carrier.company EQ ttImportCarrier.Company
               AND carrier.carrier EQ ttImportCarrier.carrier
               AND carrier.loc     EQ ttImportCarrier.Loc
             NO-ERROR.
        IF AVAILABLE carrier THEN DO:
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate Exists - Will be skipped"
                    .
            ELSE 
                ASSIGN 
                    opcNote = "Update record - All fields to be overwritten".           
        END.      
        ELSE 
            opcNote = "Add Record".
                
    END.    
   
END PROCEDURE.
  
PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCarrier FOR ttImportCarrier.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-carrier FOR carrier .
    
    FIND FIRST bf-carrier EXCLUSIVE-LOCK
        WHERE bf-carrier.company EQ ipbf-ttImportCarrier.Company
        AND bf-carrier.carrier EQ ipbf-ttImportCarrier.carrier
        AND bf-carrier.loc EQ ipbf-ttImportCarrier.Loc
        NO-ERROR.  
    IF NOT AVAILABLE bf-carrier THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE bf-carrier.
        ASSIGN 
            bf-carrier.company = ipbf-ttImportCarrier.Company
            bf-carrier.carrier = ipbf-ttImportCarrier.carrier
            bf-carrier.loc = ipbf-ttImportCarrier.Loc
            .
    END.
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportCarrier.carrier, iplIgnoreBlanks, INPUT-OUTPUT bf-carrier.carrier).
    RUN pAssignValueC (ipbf-ttImportCarrier.dscr, iplIgnoreBlanks, INPUT-OUTPUT bf-carrier.dscr).
    RUN pAssignValueC (ipbf-ttImportCarrier.loc, iplIgnoreBlanks, INPUT-OUTPUT bf-carrier.loc).
    RUN pAssignValueC (substring(ipbf-ttImportCarrier.chg-method,1,1), iplIgnoreBlanks, INPUT-OUTPUT bf-carrier.chg-method).
    IF ipbf-ttImportCarrier.Inactive EQ "Yes" THEN DO:
        RUN AddTagInactive(bf-carrier.rec_key,"carrier"). 
    END.
    ELSE if ipbf-ttImportCarrier.Inactive EQ "No" THEN DO: 
        RUN ClearTagsInactive(bf-carrier.rec_key).
    END.                                    

    RELEASE bf-carrier.
    
END PROCEDURE.
