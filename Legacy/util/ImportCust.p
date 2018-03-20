
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
DEFINE VARIABLE gcWidths      AS CHARACTER NO-UNDO INIT "60,100,100,100,60,40, 100,100,150,100,60,60,100,60,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100". 
DEFINE VARIABLE lUpdateDup AS LOGICAL NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */


PROCEDURE pAddRecord:
    /*------------------------------------------------------------------------------
     Purpose: Accepts a Data Array, validates it and adds a temp-table record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcData AS CHARACTER NO-UNDO EXTENT.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdTempTableBuffer AS HANDLE.
    DEFINE VARIABLE cData             AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportCust FOR ttImportCust.
    lUpdateDup = iplUpdateDuplicates.
    oplValid = YES.
    CREATE ttImportCust.
    ASSIGN 
        ttImportCust.Company = ipcCompany.
    FOR EACH ttImportMap
        WHERE ttImportMap.cType EQ 'Cust':
        cData = ipcData[ttImportMap.iImportIndex].
        hdTempTableBuffer = TEMP-TABLE ttImportCust:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ttImportMap.iIndex + giIndexOffset):HANDLE.
        CASE ttImportMap.cDataType:
            WHEN "integer" THEN 
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = INT(cData).
            WHEN "logical" THEN 
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = cData BEGINS "Y".
            WHEN "decimal" THEN 
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = DEC(cDaTa).
            WHEN "date" THEN 
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = DATE(cData). 
            OTHERWISE 
            ASSIGN 
                hdTempTableBuffer:BUFFER-VALUE = cData.
        END CASE.              
    END.   
    IF oplValid THEN 
    DO:
        IF ttImportCust.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportCust.cCustNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer".
    END.
    
    IF oplValid THEN 
    DO:
        IF ttImportCust.cCustType EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer Type".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportCust.cTerms EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer Terms".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportCust.cCreditLimit LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Credit Limit can not be Nagetive.".
    END.

    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ttImportCust.cStatus NE "" THEN 
            DO:
            IF LOOKUP(ttImportCust.cStatus,"A,X,S,E,I") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid status"
                    .
        END.
        IF oplValid AND ttImportCust.cCustSman NE "" THEN 
            DO:
            FIND FIRST sman NO-LOCK 
                WHERE sman.company EQ ttImportCust.Company
                AND sman.sman EQ ttImportCust.cCustSman
                NO-ERROR.
            IF NOT AVAILABLE sman THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Salesman"
                    .
        END.
        IF oplValid AND ttImportCust.cCustType NE "" THEN 
            DO:
            FIND FIRST custype NO-LOCK
                WHERE custype.company = ttImportCust.Company
                AND custype.custype = ttImportCust.cCustType
                NO-ERROR.
            IF NOT AVAILABLE custype THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Type"
                    .
        END.

        IF oplValid AND ttImportCust.cCustState NE "" THEN 
            DO:
            FIND FIRST state NO-LOCK
                WHERE state.state = ttImportCust.cCustState
                NO-ERROR.
            IF NOT AVAILABLE state THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid State"
                    .
        END.
        IF oplValid AND ttImportCust.cTerms NE "" THEN 
            DO:
            FIND FIRST terms NO-LOCK 
                WHERE terms.company = ttImportCust.Company
                AND terms.t-code EQ ttImportCust.cTerms NO-ERROR.
            IF NOT AVAILABLE terms THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Terms"
                    .
        END.
        IF oplValid AND ttImportCust.cShipState NE "" THEN 
            DO:
            FIND FIRST state NO-LOCK
                WHERE state.state = ttImportCust.cShipState
                NO-ERROR.
            IF NOT AVAILABLE state THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid shipto State"
                    .
        END.

    END.
    
    IF NOT oplValid THEN DELETE ttImportCust.
    
END PROCEDURE.

PROCEDURE pExportData:
    /*------------------------------------------------------------------------------
     Purpose:  Runs the Export Data Program for Estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriContext AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iopcFile AS CHARACTER NO-UNDO.


END PROCEDURE.

PROCEDURE pInitialize:
    /*------------------------------------------------------------------------------
     Purpose: Initializes the specific Column Mapping for Estimates   
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLoadFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFields     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabels     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataTypes  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormats    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndexStart AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportCust.
    EMPTY TEMP-TABLE ttImportMap.
    
    iIndexStart = 1 + giIndexOffset.
    
    IF ipcLoadFile EQ '' THEN 
    DO:
        ASSIGN 
            cFields    = ""
            cDataTypes = ""
            cFormats   = ""
            cLabels    = ""
            .
        DO iIndex = iIndexStart TO TEMP-TABLE ttImportCust:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
            ASSIGN 
                cFields    = cFields + TEMP-TABLE ttImportCust:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):NAME + ","
                cDataTypes = cDataTypes + TEMP-TABLE ttImportCust:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):DATA-TYPE + ","
                cFormats   = cFormats + TEMP-TABLE ttImportCust:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):FORMAT + ","
                cLabels    = cLabels + TEMP-TABLE ttImportCust:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):COLUMN-LABEL + ","
                .
            
        
        END.
        ASSIGN 
            cFields    = TRIM(cFields,",")
            cDataTypes = TRIM(cDataTypes,",")
            cFormats   = TRIM(cFormats,",")
            cLabels    = TRIM(cLabels,",")
            .
        DO iIndex = 1 TO NUM-ENTRIES(cFields):
            CREATE ttImportMap.
            ASSIGN 
                ttImportMap.cType         = "Cust"
                ttImportMap.cLabel        = ENTRY(iIndex,cFields)
                ttImportMap.iIndex        = iIndex
                ttImportMap.iImportIndex  = iIndex
                ttImportMap.cDataType     = ENTRY(iIndex,cDataTypes)
                ttImportMap.cColumnLabel  = ENTRY(iIndex,cLabels)
                ttImportMap.cColumnFormat = ENTRY(iIndex,cFormats)
                .
            IF iIndex LE NUM-ENTRIES(gcWidths)  THEN 
                ttImportMap.iColumnWidth = INT(ENTRY(iIndex,gcWidths)).
        END. 
    
    END.
    ELSE 
    DO:
    /*Load from Config File provided*/
    END.

END PROCEDURE.

PROCEDURE pProcessImport:
    /*------------------------------------------------------------------------------
     Purpose: Processes the temp-table already loaded and returns counts
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiUpdated AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAdded AS INTEGER NO-UNDO.

    DEFINE VARIABLE riEb      AS ROWID     NO-UNDO.
    DEFINE VARIABLE cIndustry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEstType  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE riItemfg  AS ROWID     NO-UNDO.
     
    MAIN:
    FOR EACH ttImportCust NO-LOCK
       /* WHERE ttImportCust.lValid EQ YES */
        : 
        FIND FIRST cust EXCLUSIVE-LOCK
            WHERE cust.company EQ ttImportCust.company
            AND cust.cust-no EQ ttImportCust.cCustNo
            NO-ERROR.  
        IF NOT AVAILABLE cust THEN 
        DO:
            opiAdded = opiAdded + 1.
                       
          /*  IF NOT iplLogOnly THEN 
            DO:*/

                CREATE cust.
        END.
        ELSE DO:
            IF not(lUpdateDup) THEN NEXT MAIN.
        opiUpdated = opiUpdated + 1.
        END.
                ASSIGN
                    cust.company     = ttImportCust.company.
                    cust.cust-no     = ttImportCust.cCustNo.
                    cust.name        = ttImportCust.cCustName.
                    cust.addr[1]     = ttImportCust.cCustAdd1.
                    cust.addr[2]     = ttImportCust.cCustAdd2.
                    cust.city        = ttImportCust.cCustCity.
                    cust.state       = ttImportCust.cCustState.
                    cust.fax-country = ttImportCust.cCustCountry.
                    cust.country     = ttImportCust.cCustCountry.
                    cust.zip         = ttImportCust.cCustZip.
                    cust.sman        = ttImportCust.cCustSman.
                    cust.area-code   = ttImportCust.cCustAreaCode.
                    cust.phone       = REPLACE(ttImportCust.cCustPhone,"-","").
                    cust.fax         = REPLACE(ttImportCust.cCustFax,"-","").
                    cust.cr-lim      = DECIMAL(ttImportCust.cCreditLimit).
                    cust.active      = ttImportCust.cStatus.
                    cust.cr-hold     = ttImportCust.cCreditHold. /* = "YES" THEN TRUE ELSE FALSE.*/
                    cust.type        = ttImportCust.cCustType.
                    cust.terms       = ttImportCust.cTerms.
                    cust.tax-id      = ttImportCust.cFedID.
                    cust.contact     = ttImportCust.cContact.
                    cust.date-field  = DATE(ttImportCust.cDateAdded). 
                    
                FIND FIRST shipto EXCLUSIVE-LOCK 
                    WHERE shipto.company EQ ttImportCust.company
                    AND shipto.cust-no EQ ttImportCust.cCustNo
                    AND shipto.ship-id EQ ttImportCust.cCustNo
                    NO-ERROR.
                IF NOT AVAILABLE shipto THEN 
                DO:
                    CREATE shipto.
                    ASSIGN 
                        shipto.company   = ttImportCust.company
                        shipto.cust-no   = ttImportCust.cCustNo
                        shipto.ship-id   = ttImportCust.cCustNo
                        shipto.ship-name = ttImportCust.cShipName
                        .
                END.
                ASSIGN 
                    shipto.ship-addr[1] = ttImportCust.cShipAdd1
                    shipto.ship-addr[2] = ttImportCust.cShipAdd2
                    shipto.ship-city    = ttImportCust.cShipCity
                    shipto.ship-state   = ttImportCust.cShipState
                    shipto.ship-zip     = ttImportCust.cShipZip
                    .
                FIND FIRST soldto EXCLUSIVE-LOCK 
                    WHERE soldto.company EQ ttImportCust.company
                    AND soldto.cust-no EQ ttImportCust.cCustNo
                    AND soldto.sold-id EQ ttImportCust.cCustNo
                    NO-ERROR.
                IF NOT AVAILABLE soldto THEN 
                DO:
                    CREATE soldto.
                    ASSIGN 
                        soldto.company   = ttImportCust.company
                        soldto.cust-no   = ttImportCust.cCustNo
                        soldto.sold-id   = ttImportCust.cCustNo
                        soldto.sold-name = ttImportCust.cCustName
                        .
                END.
                ASSIGN 
                    soldto.sold-addr[1] = ttImportCust.cCustAdd1
                    soldto.sold-addr[2] = ttImportCust.cCustAdd2
                    soldto.sold-city    = ttImportCust.cCustCity
                    soldto.sold-state   = ttImportCust.cCustState
                    soldto.sold-zip     = ttImportCust.cCustZip
                    .
                RUN pAddNote (cust.rec_key,
                    ttImportCust.cNote1,
                    "Misc Message 1",
                    "",
                    "C").
                RUN pAddNote (cust.rec_key,
                    ttImportCust.cNote2,
                    "Misc Message 2",
                    "",
                    "C").
                RUN pAddNote (cust.rec_key,
                    ttImportCust.cNote3,
                    "Mfg. Inst.",
                    "",
                    "C").
                RUN pAddNote (cust.rec_key,
                    ttImportCust.cNote4,
                    "B/L Message",
                    "",
                    "C"). 
            
    END.
    opiUpdated = opiUpdated - opiAdded.

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
