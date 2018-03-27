
/*------------------------------------------------------------------------
    File        : ImportShipTo.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Ship Tos	

    Author(s)   : BV
    Created     : Fri Nov 24 16:18:38 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportShipTo
    FIELD Company             AS CHARACTER 
    FIELD CustomerID          AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Cust ID"
    FIELD ShipToID            AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ship ID"
    FIELD ShipName            AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Name"
    FIELD ShipAddress1        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Address 1"
    FIELD ShipAddress2        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Address 2"
    FIELD ShipCity            AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "City"
    FIELD ShipState           AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "State"
    FIELD ShipCode            AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Zip/Postal"
    FIELD Contact             AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Contact"
    FIELD PhoneArea           AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Area Code"
    FIELD Phone               AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Phone"
    FIELD Fax                 AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Fax"
    FIELD SalesRep            AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Sales Rep Code"
    FIELD TaxCode             AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Tax Group"
    FIELD Note1               AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Note 1"
    FIELD Note2               AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Note 2"
    FIELD Note3               AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Note 3"
    FIELD Note4               AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Note 4"
    FIELD Warehouse           AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Warehouse"
    FIELD Bin                 AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Bin"
    FIELD Carrier             AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Carrier"
    FIELD Zone                AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Zone"
    FIELD Pallet              AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Pallet Code"
    FIELD ShipperID           AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Shipper ID"
    FIELD MemberID            AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Member ID"
    FIELD DockID              AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Dock ID"
    FIELD DockHours           AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Dock Hours"
    FIELD Charge              AS DECIMAL FORMAT ">,>>9.99<<" COLUMN-LABEL "Charge"
    FIELD DaysTransit         AS DECIMAL FORMAT ">>9" COLUMN-LABEL "Days Transit"
    FIELD DaysSamples         AS INTEGER FORMAT ">>9" COLUMN-LABEL "Days Samples"
    FIELD DaysDockAppt        AS INTEGER FORMAT ">>9" COLUMN-LABEL "Days Dock Appointment"
    FIELD DaysEarliestAllowed AS INTEGER FORMAT ">>9" COLUMN-LABEL "Days Earliest Allowed"
    FIELD DaysLatestAllowed   AS INTEGER FORMAT ">>9" COLUMN-LABEL "Days Latest Allowed"
    FIELD ShipByCaseAllowed   AS LOGICAL FORMAT "Yes/No" COLUMN-LABEL "Ship By Case Allowed"
    FIELD Broker              AS LOGICAL FORMAT "Yes/No" COLUMN-LABEL "Broker"
    FIELD Billable            AS LOGICAL FORMAT "Yes/No" COLUMN-LABEL "Billable"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


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
    DEFINE VARIABLE cData AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportShipTo FOR ttImportShipto.

    oplValid = YES.
    CREATE ttImportShipTo.
    ASSIGN 
        ttImportShipTo.Company = ipcCompany.
    FOR EACH ttImportMap
        WHERE ttImportMap.cType EQ 'ShipTo':
        cData = ipcData[ttImportMap.iImportIndex].
        hdTempTableBuffer = TEMP-TABLE ttImportShipTo:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ttImportMap.iIndex + giIndexOffset):HANDLE.
        CASE ttImportMap.cDataType:
            WHEN "integer" THEN 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = INT(cData).
            WHEN "logical" THEN 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = cData BEGINS "Y".
            WHEN "decimal" THEN 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = DEC(cDaTa).
            WHEN "date" THEN 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = DATE(cData). 
            OTHERWISE 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = cData.
        END CASE.              
    END.
    IF oplValid THEN 
    DO:
        IF ttImportShipTo.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportShipTo.CustomerID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: CustomerID".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportShipTo.ShipToID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: ShipToID".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ttImportShipTo.Company
            AND cust.cust-no EQ ttImportShipTo.CustomerID
            NO-ERROR. 
        IF NOT AVAILABLE cust THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Key Field Invalid: CustomerID"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportShipTo NO-LOCK 
            WHERE bf-ttImportShipTo.Company EQ ttImportShipTo.Company
            AND bf-ttImportShipTo.CustomerID EQ ttImportShipTo.CustomerID
            AND bf-ttImportShipTo.ShipToID EQ ttImportShipTo.ShipToID
            AND ROWID(bf-ttImportShipTo) NE ROWID(ttImportShipTo)
            NO-ERROR.
        IF AVAILABLE bf-ttImportShipTo THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST shipto NO-LOCK 
            WHERE shipto.company EQ ttImportShipTo.Company
            AND shipto.cust-no EQ ttImportShipTo.CustomerID
            AND shipto.ship-id EQ ttImportShipTo.ShipToID
            NO-ERROR .
        IF AVAILABLE shipto THEN 
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
        FIND FIRST carrier NO-LOCK 
            WHERE carrier.company EQ ttImportShipTo.Company
            AND carrier.carrier EQ ttImportShipTo.Carrier
            NO-ERROR.
        IF NOT AVAILABLE carrier THEN 
            ASSIGN 
                oplValid = NO 
                opcNote = "Invalid Carrier"
                .
        IF oplValid THEN DO:
            FIND FIRST carr-mtx NO-LOCK 
                WHERE carr-mtx.company EQ ttImportShipTo.Company
                AND carr-mtx.carrier EQ ttImportShipTo.Carrier
                AND carr-mtx.del-zone EQ ttImportShipTo.Zone
                NO-ERROR.
            IF NOT AVAILABLE carr-mtx THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Zone for Carrier"
                    . 
        END.
        IF oplValid THEN DO:
            FIND FIRST loc NO-LOCK 
                WHERE loc.company EQ ttImportShipTo.Company
                AND loc.loc EQ ttImportShipTo.Warehouse
                NO-ERROR.
            IF NOT AVAILABLE carrier THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Warehouse"
                    .
                    
        END.
        IF oplValid THEN DO:
            FIND FIRST fg-bin NO-LOCK 
                WHERE fg-bin.company EQ ttImportShipTo.Company
                AND fg-bin.loc EQ ttImportShipTo.Warehouse
                AND fg-bin.loc-bin EQ ttImportShipTo.Bin
                AND fg-bin.i-no EQ ""
                NO-ERROR.
            IF NOT AVAILABLE fg-bin THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Bin for Warehouse"
                    .
                    
        END.
        IF oplValid AND ttImportShipTo.SalesRep NE "" THEN DO:
            FIND FIRST sman NO-LOCK 
                WHERE sman.company EQ ttImportShipTo.Company
                AND sman.sman EQ ttImportShipTo.SalesRep
                NO-ERROR.
            IF NOT AVAILABLE sman THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Sales Rep Code"
                    .
                
        END.
        IF oplValid AND ttImportShipTo.TaxCode NE "" THEN DO:
            FIND FIRST stax NO-LOCK 
                WHERE stax.company EQ ttImportShipTo.Company
                AND stax.tax-group EQ ttImportShipTo.TaxCode
                NO-ERROR.
            IF NOT AVAILABLE stax THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid TaxGroup"
                    .
                    
        END.
        IF oplValid AND ttImportShipTo.Pallet NE "" THEN DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ttImportShipTo.Company
                AND item.i-no EQ ttImportShipTo.Pallet
                AND item.mat-type EQ "D"
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Pallet Code"
                    .
                    
        END.
    END.
    IF NOT oplValid THEN DELETE ttImportShipTo.
    
END PROCEDURE.

PROCEDURE pExportData:
/*------------------------------------------------------------------------------
 Purpose:  Runs the Export Data Program for ShipTo
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipriContext AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iopcFile AS CHARACTER NO-UNDO.


END PROCEDURE.

PROCEDURE pInitialize:
    /*------------------------------------------------------------------------------
     Purpose: Initializes the specific Column Mapping for ShipTos   
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLoadFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFields    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabels    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataTypes AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWidths    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormats   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    DEFINE VARIABLE iIndexStart AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportShipTo.
    EMPTY TEMP-TABLE ttImportMap.
    
    iIndexStart = 1 + giIndexOffset.
    cWidths    = "60,60,150,150,150,150,150,50,150,50,50,50,60,60," +
                  "150,150,150,150," +
                  "60,60,60,60,60,60,60,60,60,60,60," +
                  "60,60,60,60,60,60,60,60"
                  .

    IF ipcLoadFile EQ '' THEN 
    DO:
        ASSIGN 
            cFields = ""
            cDataTypes = ""
            cFormats = ""
            cLabels = ""
            .
        DO iIndex = iIndexStart TO TEMP-TABLE ttImportShipTo:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
            ASSIGN 
                cFields = cFields + TEMP-TABLE ttImportShipTo:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):NAME + ","
                cDataTypes = cDataTypes + TEMP-TABLE ttImportShipTo:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):DATA-TYPE + ","
                cFormats = cFormats + TEMP-TABLE ttImportShipTo:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):FORMAT + ","
                cLabels = cLabels + TEMP-TABLE ttImportShipTo:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):COLUMN-LABEL + ","
                .
            
        
        END.
        ASSIGN 
            cFields = TRIM(cFields,",")
            cDataTypes = TRIM(cDataTypes,",")
            cFormats = TRIM(cFormats,",")
            cLabels = TRIM(cLabels,",")
            .
        DO iIndex = 1 TO NUM-ENTRIES(cFields):
            CREATE ttImportMap.
            ASSIGN 
                ttImportMap.cType        = "ShipTo"
                ttImportMap.cLabel       = ENTRY(iIndex,cFields)
                ttImportMap.iIndex       = iIndex
                ttImportMap.iImportIndex = iIndex
                ttImportMap.cDataType    = ENTRY(iIndex,cDataTypes)
                ttImportMap.cColumnLabel = ENTRY(iIndex,cLabels)
                ttImportMap.cColumnFormat = ENTRY(iIndex,cFormats)
                .
                IF iIndex LE NUM-ENTRIES(cWidths)  THEN 
                    ttImportMap.iColumnWidth = INT(ENTRY(iIndex,cWidths)).
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

FOR EACH ttImportShipTo NO-LOCK:
    IF ttImportShipTo.CustomerID EQ "" THEN NEXT.
    IF ttImportShipTo.ShipToID EQ "" THEN NEXT.
    FIND FIRST shipto EXCLUSIVE-LOCK 
        WHERE shipto.company EQ ttImportShipTo.Company
        AND shipto.cust-no EQ ttImportShipTo.CustomerID
        AND shipto.ship-id EQ ttImportShipTo.ShipToID
        NO-ERROR.
    IF NOT AVAILABLE shipto THEN DO:
        opiAdded = opiAdded + 1.
        CREATE shipto.
        ASSIGN
            shipto.company = ttImportShipTo.Company
            shipto.cust-no = ttImportShipto.CustomerID
            shipto.ship-id = ttImportShipTo.ShipToID
            . 
    END.
    opiUpdated = opiUpdated + 1.
    ASSIGN 
        shipto.ship-name = ttImportShipTo.ShipName
        shipto.ship-addr[1] = ttImportShipTo.ShipAddress1
        shipto.ship-addr[2] = ttImportShipTo.ShipAddress2
        shipto.ship-city = ttImportShipto.ShipCity
        shipto.ship-state = ttImportShipTo.ShipState
        shipto.ship-zip = ttImportShipTo.ShipCode
        shipto.contact = ttImportShipTo.Contact        
        shipto.area-code = ttImportShipTo.PhoneArea
        shipto.phone = ttImportShipTo.Phone
        shipto.fax = ttImportShipto.Fax
        shipto.spare-char-1 = ttImportShipTo.SalesRep
        shipto.tax-code = ttImportShipTo.TaxCode
        shipto.loc = ttImportShipTo.Warehouse
        shipto.loc-bin = ttImportShipto.Bin
        shipto.carrier = ttImportShipTo.Carrier
        shipto.dest-code = ttImportShipto.Zone
        shipto.pallet = ttImportShipTo.Pallet
        shipto.spare-char-4 = ttImportShipTo.ShipperID
        shipto.spare-char-5 = ttImportShipTo.MemberID
        shipto.dock-loc = ttImportShipTo.DockID
        shipto.dock-hour = ttImportShipTo.DockHours
        shipto.del-chg = ttImportShipto.Charge
        shipto.del-time = ttImportShipTo.DaysTransit
        shipto.spare-int-1 = ttImportShipTo.DaysSamples
        shipto.spare-int-2 = ttImportShipTo.DaysDockAppt
        shipto.spare-int-3 = ttImportShipTo.DaysEarliestAllowed
        shipto.spare-int-4 = ttImportShipTo.DaysLatestAllowed
        shipto.ship-meth = ttImportShipTo.ShipByCaseAllowed
        shipto.bill = ttImportShipTo.Billable
        shipto.broker = ttImportShipTo.Broker
        shipto.notes[1] = ttImportShipTo.Note1
        shipto.notes[2] = ttImportShipTo.Note2
        shipto.notes[3] = ttImportShipTo.Note3
        shipto.notes[4] = ttImportShipTo.Note4
       .
    
END.
opiUpdated = opiUpdated - opiAdded.

END PROCEDURE.

