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
    FIELD Company        AS CHARACTER 
    FIELD Location        AS CHARACTER 
    FIELD cCustNo        AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Customer" 
    FIELD cCustName      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer Name" 
    FIELD cCustAdd1      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer Addr1" 
    FIELD cCustAdd2      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Customer Addr2" 
    FIELD cCustCity      AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "City" 
    FIELD cCustState     AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "State" 
    FIELD cCustCountry   AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Country" 
    FIELD cCustZip       AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Zip Code" 
    FIELD cCustSman      AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "SalesMan"  
    FIELD cCustAreaCode  AS CHARACTER FORMAT "xxx" COLUMN-LABEL "Area Code"  
    FIELD cCustPhone     AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Phone"  
    FIELD cCustFax       AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Fax #"  
    FIELD cCreditLimit   AS DECIMAL   FORMAT ">>>,>>>,>>9.99" COLUMN-LABEL "Credit Limit"  
    FIELD cStatus        AS CHARACTER FORMAT "X(11)" COLUMN-LABEL "Status"  
    FIELD cCreditHold    AS LOGICAL   FORMAT "yes/no" COLUMN-LABEL "Credit hold"   
    FIELD cCustType      AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Customer Type"   
    FIELD cTerms         AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Terms Code"   
    FIELD cFedID         AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Tax Resale#"   
    FIELD cNote1         AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Note 1"   
    FIELD cNote2         AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Note 2"   
    FIELD cNote3         AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Note 3"   
    FIELD cNote4         AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Note 4"   
    FIELD cShipName      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "ShipTo Name"   
    FIELD cShipAdd1      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Shipto Address 1"   
    FIELD cShipAdd2      AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Shipto Address 2"   
    FIELD cShipCity      AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "Shipto City"   
    FIELD cShipState     AS CHARACTER FORMAT "X(2)" COLUMN-LABEL "Shipto Sate"   
    FIELD cShipZip       AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Shipto zip"   
    FIELD cContact       AS CHARACTER FORMAT "X(25)" COLUMN-LABEL "Contact Name"   
    FIELD cDateAdded     AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Date Added"   
    .

DEFINE VARIABLE giIndexOffset AS INTEGER   NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

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
        IF ipbf-ttImportCust.cCustNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.cCustType EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer Type".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.cTerms EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer Terms".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCust.cCreditLimit LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Credit Limit can not be Nagetive.".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipbf-ttImportCust.Company
            AND cust.cust-no EQ ipbf-ttImportCust.cCustNo
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

    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportCust.cStatus NE "" THEN 
            DO:
            IF LOOKUP(ipbf-ttImportCust.cStatus,"A,X,S,E,I") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid status"
                    .
        END.
        IF oplValid AND ipbf-ttImportCust.cCustSman NE "" THEN 
            DO:
            FIND FIRST sman NO-LOCK 
                WHERE sman.company EQ ipbf-ttImportCust.Company
                AND sman.sman EQ ipbf-ttImportCust.cCustSman
                NO-ERROR.
            IF NOT AVAILABLE sman THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Salesman"
                    .
        END.
        IF oplValid AND ipbf-ttImportCust.cCustType NE "" THEN 
            DO:
            FIND FIRST custype NO-LOCK
                WHERE custype.company = ipbf-ttImportCust.Company
                AND custype.custype = ipbf-ttImportCust.cCustType
                NO-ERROR.
            IF NOT AVAILABLE custype THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Type"
                    .
        END.

        IF oplValid AND ipbf-ttImportCust.cCustState NE "" THEN 
            DO:
            FIND FIRST state NO-LOCK
                WHERE state.state = ipbf-ttImportCust.cCustState
                NO-ERROR.
            IF NOT AVAILABLE state THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid State"
                    .
        END.
        IF oplValid AND ipbf-ttImportCust.cTerms NE "" THEN 
            DO:
            FIND FIRST terms NO-LOCK 
                WHERE terms.company = ipbf-ttImportCust.Company
                AND terms.t-code EQ ipbf-ttImportCust.cTerms NO-ERROR.
            IF NOT AVAILABLE terms THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Terms"
                    .
        END.
        IF oplValid AND ipbf-ttImportCust.cShipState NE "" THEN 
            DO:
            FIND FIRST state NO-LOCK
                WHERE state.state = ipbf-ttImportCust.cShipState
                NO-ERROR.
            IF NOT AVAILABLE state THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid shipto State"
                    .
        END.

    END.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCust FOR ttImportCust.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
     
    MAIN:
    FOR EACH ipbf-ttImportCust NO-LOCK
       /* WHERE ttImportCust.lValid EQ YES */
        : 
        FIND FIRST cust EXCLUSIVE-LOCK
            WHERE cust.company EQ ipbf-ttImportCust.company
            AND cust.cust-no EQ ipbf-ttImportCust.cCustNo
            NO-ERROR.  
        IF NOT AVAILABLE cust THEN 
        DO:
            iopiAdded = iopiAdded + 1.

                CREATE cust.
        END.
        ASSIGN
        cust.company     = ipbf-ttImportCust.company.
        cust.cust-no     = ipbf-ttImportCust.cCustNo.
        cust.name        = ipbf-ttImportCust.cCustName.
        cust.addr[1]     = ipbf-ttImportCust.cCustAdd1.
        cust.addr[2]     = ipbf-ttImportCust.cCustAdd2.
        cust.city        = ipbf-ttImportCust.cCustCity.
        cust.state       = ipbf-ttImportCust.cCustState.
        cust.fax-country = ipbf-ttImportCust.cCustCountry.
        cust.country     = ipbf-ttImportCust.cCustCountry.
        cust.zip         = ipbf-ttImportCust.cCustZip.
        cust.sman        = ipbf-ttImportCust.cCustSman.
        cust.area-code   = ipbf-ttImportCust.cCustAreaCode.
        cust.phone       = REPLACE(ipbf-ttImportCust.cCustPhone,"-","").
        cust.fax         = REPLACE(ipbf-ttImportCust.cCustFax,"-","").
        cust.cr-lim      = DECIMAL(ipbf-ttImportCust.cCreditLimit).
        cust.active      = ipbf-ttImportCust.cStatus.
        cust.cr-hold     = ipbf-ttImportCust.cCreditHold. /* = "YES" THEN TRUE ELSE FALSE.*/
        cust.type        = ipbf-ttImportCust.cCustType.
        cust.terms       = ipbf-ttImportCust.cTerms.
        cust.tax-id      = ipbf-ttImportCust.cFedID.
        cust.contact     = ipbf-ttImportCust.cContact.
        cust.date-field  = DATE(ipbf-ttImportCust.cDateAdded). 
        
        FIND FIRST shipto EXCLUSIVE-LOCK 
            WHERE shipto.company EQ ipbf-ttImportCust.company
            AND shipto.cust-no EQ ipbf-ttImportCust.cCustNo
            AND shipto.ship-id EQ ipbf-ttImportCust.cCustNo
            NO-ERROR.
        IF NOT AVAILABLE shipto THEN 
            DO:
            CREATE shipto.
            ASSIGN 
                shipto.company   = ipbf-ttImportCust.company
                shipto.cust-no   = ipbf-ttImportCust.cCustNo
                shipto.ship-id   = ipbf-ttImportCust.cCustNo
                shipto.ship-name = ipbf-ttImportCust.cShipName
                .
            END.
            ASSIGN 
                shipto.ship-addr[1] = ipbf-ttImportCust.cShipAdd1
                shipto.ship-addr[2] = ipbf-ttImportCust.cShipAdd2
                shipto.ship-city    = ipbf-ttImportCust.cShipCity
                shipto.ship-state   = ipbf-ttImportCust.cShipState
                shipto.ship-zip     = ipbf-ttImportCust.cShipZip
                .
            FIND FIRST soldto EXCLUSIVE-LOCK 
                WHERE soldto.company EQ ipbf-ttImportCust.company
                AND soldto.cust-no EQ ipbf-ttImportCust.cCustNo
                AND soldto.sold-id EQ ipbf-ttImportCust.cCustNo
                NO-ERROR.
            IF NOT AVAILABLE soldto THEN 
                DO:
                CREATE soldto.
                ASSIGN 
                    soldto.company   = ipbf-ttImportCust.company
                    soldto.cust-no   = ipbf-ttImportCust.cCustNo
                    soldto.sold-id   = ipbf-ttImportCust.cCustNo
                    soldto.sold-name = ipbf-ttImportCust.cCustName
                    .
            END.
            ASSIGN 
                soldto.sold-addr[1] = ipbf-ttImportCust.cCustAdd1
                soldto.sold-addr[2] = ipbf-ttImportCust.cCustAdd2
                soldto.sold-city    = ipbf-ttImportCust.cCustCity
                soldto.sold-state   = ipbf-ttImportCust.cCustState
                soldto.sold-zip     = ipbf-ttImportCust.cCustZip
                .
            RUN pAddNote (cust.rec_key,
                          ipbf-ttImportCust.cNote1,
                          "Misc Message 1",
                          "",
                          "C").
            RUN pAddNote (cust.rec_key,
                          ipbf-ttImportCust.cNote2,
                          "Misc Message 2",
                          "",
                          "C").
            RUN pAddNote (cust.rec_key,
                          ipbf-ttImportCust.cNote3,
                          "Mfg. Inst.",
                          "",
                          "C").
            RUN pAddNote (cust.rec_key,
                          ipbf-ttImportCust.cNote4,
                          "B/L Message",
                          "",
                          "C"). 
    END.
    
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
