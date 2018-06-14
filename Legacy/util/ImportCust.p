/*------------------------------------------------------------------------
    File        : ImportCust.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Customer	
    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportCust
    FIELD Company      AS CHARACTER 
    FIELD Location     AS CHARACTER 
    FIELD CustNo       AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Customer" HELP "Required - Size:8" 
    FIELD CustName     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer Name" HELP "Optional - Size:30" 
    FIELD CustAdd1     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer Addr1" HELP "Optional - Size:30"
    FIELD CustAdd2     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer Addr2" HELP "Optional - Size:30"
    FIELD CustCity     AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "City" HELP "Optional - Size:15"
    FIELD CustState    AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "State" HELP "Optional - Size:2" 
    FIELD CustCountry  AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Country" HELP "Optional - Size:10"
    FIELD CustZip      AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Zip Code" HELP "Optional - Size:10"
    FIELD CustSman     AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "SalesMan"  HELP "Optional - Size:3"
    FIELD CustAreaCode AS CHARACTER FORMAT "xxx" COLUMN-LABEL "Area Code"  HELP "Optional - Size:3"
    FIELD CustPhone    AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Phone"  HELP "Optional - Size:7"
    FIELD CustFax      AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Fax #"  HELP "Optional - Size:7"
    FIELD CreditLimit  AS DECIMAL   FORMAT ">>>,>>>,>>9.99" COLUMN-LABEL "Credit Limit" HELP "Optional - Decimal" 
    FIELD CustStatus   AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "Status"  HELP "Optional - Size:1"
    FIELD CreditHold   AS CHARACTER FORMAT "X" COLUMN-LABEL "Credit Hold" HELP "Optional - Y or N"  
    FIELD CustType     AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Customer Type" HELP "Optional - Size:8"   
    FIELD Terms        AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Terms Code" HELP "Optional - Size:5" 
    FIELD FedID        AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Tax Resale#" HELP "Optional - Size:8"   
    FIELD Note1        AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Note 1" HELP "Optional - Size:30"  
    FIELD Note2        AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Note 2" HELP "Optional - Size:30"  
    FIELD Note3        AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Note 3" HELP "Optional - Size:30"  
    FIELD Note4        AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Note 4" HELP "Optional - Size:30"  
    FIELD ShipName     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "ShipTo Name" HELP "Optional - Size:30"  
    FIELD ShipAdd1     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "ShipTo Address 1" HELP "Optional - Size:30"  
    FIELD ShipAdd2     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "ShipTo Address 2" HELP "Optional - Size:30"  
    FIELD ShipCity     AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "ShipTo City" HELP "Optional - Size:15"   
    FIELD ShipState    AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "ShipTo State" HELP "Optional - Size:2"
    FIELD ShipZip      AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "ShipTo Zip" HELP "Optional - Size:10"  
    FIELD Contact      AS CHARACTER FORMAT "X(25)" COLUMN-LABEL "Contact Name" HELP "Optional - Size:25"  
    FIELD DateAdded    AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Date Added" HELP "Optional - Date"  
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportCust"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCust FOR ttImportCust.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportCust FOR ttImportCust.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.CustNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.CustType EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer Type".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.Terms EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer Terms".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.CreditLimit LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Credit Limit can not be Negative.".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipbf-ttImportCust.Company
            AND cust.cust-no EQ ipbf-ttImportCust.CustNo
            NO-ERROR .
        IF AVAILABLE cust THEN 
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
    
    /*Field level validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportCust.CustStatus NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Active", ipbf-ttImportCust.CustStatus, "A,X,S,E,I", OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportCust.CustSman NE "" THEN 
            RUN pIsValidSalesRep IN hdValidator (ipbf-ttImportCust.CustSman, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportCust.CustType NE "" THEN 
            RUN pIsValidCustomerType IN hdValidator (ipbf-ttImportCust.CustType, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportCust.CustState NE "" THEN 
            RUN pIsValidState IN hdValidator (ipbf-ttImportCust.CustState, NO, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportCust.Terms NE "" THEN 
            RUN pIsValidTerms IN hdValidator (ipbf-ttImportCust.Terms, NO, ipbf-ttImportCust.Company, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid AND ipbf-ttImportCust.ShipState NE "" THEN 
            RUN pIsValidState IN hdValidator (ipbf-ttImportCust.ShipState, NO, OUTPUT oplValid, OUTPUT cValidNote).    
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCust FOR ttImportCust.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
     
    FIND FIRST cust EXCLUSIVE-LOCK
        WHERE cust.company EQ ipbf-ttImportCust.Company
        AND cust.cust-no EQ ipbf-ttImportCust.CustNo
        NO-ERROR.  
    IF NOT AVAILABLE cust THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE cust.
        ASSIGN 
            cust.company = ipbf-ttImportCust.Company
            cust.cust-no = ipbf-ttImportCust.CustNo
            .
    END.
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportCust.CustName, iplIgnoreBlanks, INPUT-OUTPUT cust.name).
    RUN pAssignValueC (ipbf-ttImportCust.CustAdd1, iplIgnoreBlanks, INPUT-OUTPUT cust.addr[1]).
    RUN pAssignValueC (ipbf-ttImportCust.CustAdd2, iplIgnoreBlanks, INPUT-OUTPUT cust.addr[2]).
    RUN pAssignValueC (ipbf-ttImportCust.CustCity, iplIgnoreBlanks, INPUT-OUTPUT cust.city).
    RUN pAssignValueC (ipbf-ttImportCust.CustState, iplIgnoreBlanks, INPUT-OUTPUT cust.state).
    RUN pAssignValueC (ipbf-ttImportCust.CustCountry, iplIgnoreBlanks, INPUT-OUTPUT cust.fax-country).
    RUN pAssignValueC (ipbf-ttImportCust.CustCountry, iplIgnoreBlanks, INPUT-OUTPUT cust.country).
    RUN pAssignValueC (ipbf-ttImportCust.CustZip, iplIgnoreBlanks, INPUT-OUTPUT cust.zip).
    RUN pAssignValueC (ipbf-ttImportCust.CustSman, iplIgnoreBlanks, INPUT-OUTPUT cust.sman).
    RUN pAssignValueC (ipbf-ttImportCust.CustAreaCode, iplIgnoreBlanks, INPUT-OUTPUT cust.area-code).
    RUN pAssignValueC (ipbf-ttImportCust.CustPhone, iplIgnoreBlanks, INPUT-OUTPUT cust.phone).  
    RUN pAssignValueC (ipbf-ttImportCust.CustFax, iplIgnoreBlanks, INPUT-OUTPUT cust.fax).
    RUN pAssignValueD (ipbf-ttImportCust.CreditLimit, iplIgnoreBlanks, INPUT-OUTPUT cust.cr-lim).
    RUN pAssignValueC (ipbf-ttImportCust.CustStatus, YES, INPUT-OUTPUT cust.active).
    RUN pAssignValueCToL (ipbf-ttImportCust.CreditHold, iplIgnoreBlanks, INPUT-OUTPUT cust.cr-hold).
    RUN pAssignValueC (ipbf-ttImportCust.CustType, YES, INPUT-OUTPUT cust.type).
    RUN pAssignValueC (ipbf-ttImportCust.Terms, YES, INPUT-OUTPUT cust.terms).
    RUN pAssignValueC (ipbf-ttImportCust.FedID, iplIgnoreBlanks, INPUT-OUTPUT cust.tax-id).
    RUN pAssignValueC (ipbf-ttImportCust.Contact, iplIgnoreBlanks, INPUT-OUTPUT cust.contact).
    RUN pAssignValueDate (ipbf-ttImportCust.DateAdded, YES, INPUT-OUTPUT cust.date-field).
        
    FIND FIRST shipto EXCLUSIVE-LOCK 
        WHERE shipto.company EQ cust.company
        AND shipto.cust-no EQ cust.cust-no
        AND shipto.ship-id EQ cust.cust-no
        NO-ERROR.
    IF NOT AVAILABLE shipto THEN 
    DO:
        CREATE shipto.
        ASSIGN 
            shipto.company      = cust.company
            shipto.cust-no      = cust.cust-no
            shipto.ship-id      = cust.cust-no
            .
    END.
    RUN pAssignValueC (ipbf-ttImportCust.ShipName, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-name).
    RUN pAssignValueC (ipbf-ttImportCust.ShipAdd1, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-addr[1]).
    RUN pAssignValueC (ipbf-ttImportCust.ShipAdd2, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-addr[2]).
    RUN pAssignValueC (ipbf-ttImportCust.ShipCity, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-city).
    RUN pAssignValueC (ipbf-ttImportCust.ShipState, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-state).
    RUN pAssignValueC (ipbf-ttImportCust.ShipZip, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-zip).
        
    FIND FIRST soldto EXCLUSIVE-LOCK 
        WHERE soldto.company EQ cust.company
        AND soldto.cust-no EQ cust.cust-no
        AND soldto.sold-id EQ cust.cust-no
        NO-ERROR.
    IF NOT AVAILABLE soldto THEN 
    DO:
        CREATE soldto.
        ASSIGN 
            soldto.company      = cust.company
            soldto.cust-no      = cust.cust-no
            soldto.sold-id      = cust.cust-no
            soldto.sold-name    = cust.name
            soldto.sold-addr[1] = cust.addr[1]
            soldto.sold-addr[2] = cust.addr[2]
            soldto.sold-city    = cust.city
            soldto.sold-state   = cust.state
            soldto.sold-zip     = cust.zip
            .
    END.
            
    RUN pAddNote (cust.rec_key,
        ipbf-ttImportCust.Note1,
        "Misc Message 1",
        "",
        "C").
    RUN pAddNote (cust.rec_key,
        ipbf-ttImportCust.Note2,
        "Misc Message 2",
        "",
        "C").
    RUN pAddNote (cust.rec_key,
        ipbf-ttImportCust.Note3,
        "Mfg. Inst.",
        "",
        "C").
    RUN pAddNote (cust.rec_key,
        ipbf-ttImportCust.Note4,
        "B/L Message",
        "",
        "C"). 
    
END PROCEDURE.

PROCEDURE pAddNote:
    /*------------------------------------------------------------------------------
     Purpose: Adds a note to supplied rec_key and parameters
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER.
    DEFINE INPUT PARAMETER ipcText AS CHARACTER.
    DEFINE INPUT PARAMETER ipcTitle AS CHARACTER.
    DEFINE INPUT PARAMETER ipcCode AS CHARACTER.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER.

    IF ipcText NE "" THEN 
    DO:
        CREATE notes.
        ASSIGN
            notes.rec_key    = ipcRecKey
            notes.note_date  = TODAY
            notes.note_time  = TIME
            notes.note_text  = ipcText
            notes.note_title = ipcTitle
            notes.note_code  = ipcCode
            notes.user_id    = "asi"
            notes.note_type  = ipcType
            .                    
    END.                           

END PROCEDURE.
