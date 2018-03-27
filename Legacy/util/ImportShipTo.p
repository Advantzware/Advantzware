
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
    FIELD Location            AS CHARACTER  
    FIELD CustomerID          AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Cust ID" HELP "Required - Must be valid - Size:8"
    FIELD ShipToID            AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Ship ID" HELP "Required - Must be valid - Size:8"
    FIELD ShipName            AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Name" HELP "Optional - Size:30"
    FIELD ShipAddress1        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Address 1" HELP "Optional - Size:30"
    FIELD ShipAddress2        AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Address 2" HELP "Optional - Size:30"
    FIELD ShipCity            AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "City" HELP "Optional - Size:30"
    FIELD ShipState           AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "State" HELP "Optional - Size:10"
    FIELD ShipCode            AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Zip/Postal" HELP "Optional - Size:20"
    FIELD Contact             AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Contact" HELP "Optional - Size:30"
    FIELD PhoneArea           AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Area Code" HELP "Optional - Size:3"
    FIELD Phone               AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Phone" HELP "Optional - Size:20"
    FIELD Fax                 AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Fax" HELP "Optional - Size:20"
    FIELD SalesRep            AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Sales Rep Code" HELP "Optional - Field Validated - Size:3"
    FIELD TaxCode             AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Tax Group" HELP "Optional - Field Validated - Size:3"
    FIELD Note1               AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Note 1" HELP "Optional - Size:50"
    FIELD Note2               AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Note 2" HELP "Optional - Size:50"
    FIELD Note3               AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Note 3" HELP "Optional - Size:50"
    FIELD Note4               AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Note 4" HELP "Optional - Size:50"
    FIELD Warehouse           AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Warehouse" HELP "Required - Must Be Valid - Size:5"
    FIELD Bin                 AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Bin" HELP "Optional - Size:10"
    FIELD Carrier             AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Carrier" HELP "Required - Field Validated - Size:10"
    FIELD Zone                AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Zone" HELP "Required - Field Validated - Size:10"
    FIELD Pallet              AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Pallet Code" HELP "Optional - Field Validated - Size:10"
    FIELD ShipperID           AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Shipper ID" HELP "Optional - Size:20"
    FIELD MemberID            AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Member ID" HELP "Optional - Size:20"
    FIELD DockID              AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Dock ID" HELP "Optional - Size:20"
    FIELD DockHours           AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Dock Hours" HELP "Optional - Size:20"
    FIELD Charge              AS DECIMAL FORMAT ">,>>9.99<<" COLUMN-LABEL "Charge" HELP "Optional - Decimal"
    FIELD DaysTransit         AS DECIMAL FORMAT ">>9" COLUMN-LABEL "Days Transit" HELP "Optional - Decimal"
    FIELD DaysSamples         AS INTEGER FORMAT ">>9" COLUMN-LABEL "Days Samples" HELP "Optional - Integer"
    FIELD DaysDockAppt        AS INTEGER FORMAT ">>9" COLUMN-LABEL "Days Dock Appointment" HELP "Optional - Integer"
    FIELD DaysEarliestAllowed AS INTEGER FORMAT ">>9" COLUMN-LABEL "Days Earliest Allowed" HELP "Optional - Integer"
    FIELD DaysLatestAllowed   AS INTEGER FORMAT ">>9" COLUMN-LABEL "Days Latest Allowed" HELP "Optional - Integer"
    FIELD ShipByCaseAllowed   AS LOGICAL FORMAT "Yes/No" COLUMN-LABEL "Ship By Case Allowed" HELP "Optional - Logical"
    FIELD Broker              AS LOGICAL FORMAT "Yes/No" COLUMN-LABEL "Broker" HELP "Optional - Logical"
    FIELD Billable            AS LOGICAL FORMAT "Yes/No" COLUMN-LABEL "Billable" HELP "Optional - Logical"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportShipTo"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportShipTo FOR ttImportShipTo.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportShipTo FOR ttImportShipTo.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportShipTo.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportShipTo.CustomerID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: CustomerID".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportShipTo.ShipToID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: ShipToID".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportShipTo.Warehouse EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Warehouse".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipbf-ttImportShipTo.Company
            AND cust.cust-no EQ ipbf-ttImportShipTo.CustomerID
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
            WHERE bf-ttImportShipTo.Company EQ ipbf-ttImportShipTo.Company
            AND bf-ttImportShipTo.CustomerID EQ ipbf-ttImportShipTo.CustomerID
            AND bf-ttImportShipTo.ShipToID EQ ipbf-ttImportShipTo.ShipToID
            AND ROWID(bf-ttImportShipTo) NE ROWID(ipbf-ttImportShipTo)
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
            WHERE shipto.company EQ ipbf-ttImportShipTo.Company
            AND shipto.cust-no EQ ipbf-ttImportShipTo.CustomerID
            AND shipto.ship-id EQ ipbf-ttImportShipTo.ShipToID
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
            WHERE carrier.company EQ ipbf-ttImportShipTo.Company
            AND carrier.carrier EQ ipbf-ttImportShipTo.Carrier
            NO-ERROR.
        IF NOT AVAILABLE carrier THEN 
            ASSIGN 
                oplValid = NO 
                opcNote = "Invalid Carrier"
                .
        IF oplValid THEN DO:
            FIND FIRST carr-mtx NO-LOCK 
                WHERE carr-mtx.company EQ ipbf-ttImportShipTo.Company
                AND carr-mtx.carrier EQ ipbf-ttImportShipTo.Carrier
                AND carr-mtx.del-zone EQ ipbf-ttImportShipTo.Zone
                NO-ERROR.
            IF NOT AVAILABLE carr-mtx THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Zone for Carrier"
                    . 
        END.
        IF oplValid THEN DO:
            FIND FIRST loc NO-LOCK 
                WHERE loc.company EQ ipbf-ttImportShipTo.Company
                AND loc.loc EQ ipbf-ttImportShipTo.Warehouse
                NO-ERROR.
            IF NOT AVAILABLE carrier THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Warehouse"
                    .
                    
        END.
        IF oplValid THEN DO:
            FIND FIRST fg-bin NO-LOCK 
                WHERE fg-bin.company EQ ipbf-ttImportShipTo.Company
                AND fg-bin.loc EQ ipbf-ttImportShipTo.Warehouse
                AND fg-bin.loc-bin EQ ipbf-ttImportShipTo.Bin
                AND fg-bin.i-no EQ ""
                NO-ERROR.
            IF NOT AVAILABLE fg-bin THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Bin for Warehouse"
                    .
                    
        END.
        IF oplValid AND ipbf-ttImportShipTo.SalesRep NE "" THEN DO:
            FIND FIRST sman NO-LOCK 
                WHERE sman.company EQ ipbf-ttImportShipTo.Company
                AND sman.sman EQ ipbf-ttImportShipTo.SalesRep
                NO-ERROR.
            IF NOT AVAILABLE sman THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Sales Rep Code"
                    .
                
        END.
        IF oplValid AND ipbf-ttImportShipTo.TaxCode NE "" THEN DO:
            FIND FIRST stax NO-LOCK 
                WHERE stax.company EQ ipbf-ttImportShipTo.Company
                AND stax.tax-group EQ ipbf-ttImportShipTo.TaxCode
                NO-ERROR.
            IF NOT AVAILABLE stax THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid TaxGroup"
                    .
                    
        END.
        IF oplValid AND ipbf-ttImportShipTo.Pallet NE "" THEN DO:
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ ipbf-ttImportShipTo.Company
                AND item.i-no EQ ipbf-ttImportShipTo.Pallet
                AND item.mat-type EQ "D"
                NO-ERROR.
            IF NOT AVAILABLE item THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote = "Invalid Pallet Code"
                    .
                    
        END.
    END.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportShipto FOR ttImportShipTo.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    FIND FIRST shipto EXCLUSIVE-LOCK 
        WHERE shipto.company EQ ipbf-ttImportShipTo.Company
        AND shipto.cust-no EQ ipbf-ttImportShipTo.CustomerID
        AND shipto.ship-id EQ ipbf-ttImportShipTo.ShipToID
        NO-ERROR.
    IF NOT AVAILABLE shipto THEN DO:
        iopiAdded = iopiAdded + 1.
        CREATE shipto.
        ASSIGN
            shipto.company = ipbf-ttImportShipTo.Company
            shipto.cust-no = ipbf-ttImportShipTo.CustomerID
            shipto.ship-id = ipbf-ttImportShipTo.ShipToID
            . 
    END.
    ASSIGN 
        shipto.ship-name = ipbf-ttImportShipTo.ShipName
        shipto.ship-addr[1] = ipbf-ttImportShipTo.ShipAddress1
        shipto.ship-addr[2] = ipbf-ttImportShipTo.ShipAddress2
        shipto.ship-city = ipbf-ttImportShipTo.ShipCity
        shipto.ship-state = ipbf-ttImportShipTo.ShipState
        shipto.ship-zip = ipbf-ttImportShipTo.ShipCode
        shipto.contact = ipbf-ttImportShipTo.Contact        
        shipto.area-code = ipbf-ttImportShipTo.PhoneArea
        shipto.phone = ipbf-ttImportShipTo.Phone
        shipto.fax = ipbf-ttImportShipTo.Fax
        shipto.spare-char-1 = ipbf-ttImportShipTo.SalesRep
        shipto.tax-code = ipbf-ttImportShipTo.TaxCode
        shipto.loc = ipbf-ttImportShipTo.Warehouse
        shipto.loc-bin = ipbf-ttImportShipTo.Bin
        shipto.carrier = ipbf-ttImportShipTo.Carrier
        shipto.dest-code = ipbf-ttImportShipTo.Zone
        shipto.pallet = ipbf-ttImportShipTo.Pallet
        shipto.spare-char-4 = ipbf-ttImportShipTo.ShipperID
        shipto.spare-char-5 = ipbf-ttImportShipTo.MemberID
        shipto.dock-loc = ipbf-ttImportShipTo.DockID
        shipto.dock-hour = ipbf-ttImportShipTo.DockHours
        shipto.del-chg = ipbf-ttImportShipTo.Charge
        shipto.del-time = ipbf-ttImportShipTo.DaysTransit
        shipto.spare-int-1 = ipbf-ttImportShipTo.DaysSamples
        shipto.spare-int-2 = ipbf-ttImportShipTo.DaysDockAppt
        shipto.spare-int-3 = ipbf-ttImportShipTo.DaysEarliestAllowed
        shipto.spare-int-4 = ipbf-ttImportShipTo.DaysLatestAllowed
        shipto.ship-meth = ipbf-ttImportShipTo.ShipByCaseAllowed
        shipto.bill = ipbf-ttImportShipTo.Billable
        shipto.broker = ipbf-ttImportShipTo.Broker
        shipto.notes[1] = ipbf-ttImportShipTo.Note1
        shipto.notes[2] = ipbf-ttImportShipTo.Note2
        shipto.notes[3] = ipbf-ttImportShipTo.Note3
        shipto.notes[4] = ipbf-ttImportShipTo.Note4
       .
    
END PROCEDURE.

