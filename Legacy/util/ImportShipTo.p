
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
    FIELD cExportId          AS CHARACTER FORMAT "x(16)" COLUMN-LABEL "Export ID#" HELP "Optional - Size:16"
    FIELD DockID              AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Dock ID" HELP "Optional - Size:20"
    FIELD DockHours           AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Dock Hours" HELP "Optional - Size:20"
    FIELD Charge              AS DECIMAL   FORMAT ">>9.99" COLUMN-LABEL "Charge" HELP "Optional - Decimal"
    FIELD DaysTransit         AS DECIMAL   FORMAT ">>9" COLUMN-LABEL "Days Transit" HELP "Optional - Decimal"
    FIELD DaysSamples         AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Days Samples" HELP "Optional - Integer"
    FIELD DaysDockAppt        AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Days Dock Appointment" HELP "Optional - Integer"
    FIELD DaysEarliestAllowed AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Days Earliest Allowed" HELP "Optional - Integer"
    FIELD DaysLatestAllowed   AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Days Latest Allowed" HELP "Optional - Integer"
    FIELD ShipByCaseAllowed   AS CHARACTER FORMAT "X" COLUMN-LABEL "Ship By Case Allowed" HELP "Optional - Y or N"
    FIELD Broker              AS CHARACTER FORMAT "X" COLUMN-LABEL "Broker" HELP "Optional - Y or N"
    FIELD Billable            AS CHARACTER FORMAT "X" COLUMN-LABEL "Billable" HELP "Optional - Y or N"
    FIELD cManTax            AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Mandatory Tax" HELP "Optional - Yes or N0"
    FIELD cInactive          AS CHARACTER FORMAT "X(1)" COLUMN-LABEL "Inactive" HELP "Optional - Yes or N0"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/
DEFINE VARIABLE hTags    AS HANDLE NO-UNDO.
RUN system/TagProcs.p PERSISTENT SET hTags.
SESSION:ADD-SUPER-PROCEDURE (hTags).

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetNextShipNo RETURNS INTEGER 
    (ipcCompany AS CHARACTER,
    ipcCustNo AS CHARACTER) FORWARD.


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
        IF oplValid AND ipbf-ttImportShipTo.Carrier NE "" THEN 
            RUN pIsValidCarrier IN hdValidator (ipbf-ttImportShipTo.Carrier, NO, ipbf-ttImportShipTo.Company, OUTPUT oplValid, OUTPUT cValidNote).
       
        IF oplValid AND ipbf-ttImportShipTo.Carrier NE "" AND ipbf-ttImportShipTo.Zone NE "" THEN 
            RUN pIsValidDeliveryZone IN hdValidator (ipbf-ttImportShipTo.Carrier, ipbf-ttImportShipTo.Zone, NO, ipbf-ttImportShipTo.Company, OUTPUT oplValid, OUTPUT cValidNote).
       
        IF oplValid AND ipbf-ttImportShipTo.Warehouse NE "" THEN 
            RUN pIsValidWarehouse IN hdValidator (ipbf-ttImportShipTo.Warehouse, NO, ipbf-ttImportShipTo.Company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportShipTo.Bin NE "" THEN 
            RUN pIsValidFGBin IN hdValidator (ipbf-ttImportShipTo.Bin, "", NO, ipbf-ttImportShipTo.Company, OUTPUT oplValid, OUTPUT cValidNote).
                                
        IF oplValid AND ipbf-ttImportShipTo.SalesRep NE "" THEN 
            RUN pIsValidSalesRep IN hdValidator (ipbf-ttImportShipTo.SalesRep, NO, ipbf-ttImportShipTo.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportShipTo.TaxCode NE "" THEN 
            RUN pIsValidTaxGroup IN hdValidator (ipbf-ttImportShipTo.TaxCode, NO, ipbf-ttImportShipTo.Company, OUTPUT oplValid, OUTPUT cValidNote).
            
        IF oplValid AND ipbf-ttImportShipTo.Pallet NE "" THEN 
            RUN pIsValidItemForType IN hdValidator (ipbf-ttImportShipTo.Pallet, "D", NO, ipbf-ttImportShipTo.Company, OUTPUT oplValid, OUTPUT cValidNote).

    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    IF ipbf-ttImportShipTo.cInactive EQ "Yes" THEN
         ipbf-ttImportShipTo.cInactive = "I".
    ELSE ipbf-ttImportShipTo.cInactive = "".
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportShipto FOR ttImportShipTo.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    FIND FIRST shipto EXCLUSIVE-LOCK 
        WHERE shipto.company EQ ipbf-ttImportShipTo.Company
        AND shipto.cust-no EQ ipbf-ttImportShipTo.CustomerID
        AND shipto.ship-id EQ ipbf-ttImportShipTo.ShipToID
        NO-ERROR.
    IF NOT AVAILABLE shipto THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE shipto.
        ASSIGN
            shipto.company = ipbf-ttImportShipTo.Company
            shipto.cust-no = ipbf-ttImportShipTo.CustomerID
            shipto.ship-id = ipbf-ttImportShipTo.ShipToID
            shipto.ship-no = fGetNextShipNo(shipto.company,shipto.cust-no)
            . 
    END.
    RUN pAssignValueC (ipbf-ttImportShipTo.ShipName, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-name).
    RUN pAssignValueC (ipbf-ttImportShipTo.ShipAddress1, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-addr[1]).
    RUN pAssignValueC (ipbf-ttImportShipTo.ShipAddress2, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-addr[2]).
    RUN pAssignValueC (ipbf-ttImportShipTo.ShipCity, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-city).
    RUN pAssignValueC (ipbf-ttImportShipTo.ShipState, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-state).
    RUN pAssignValueC (ipbf-ttImportShipTo.ShipCode, iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-zip).
    RUN pAssignValueC (ipbf-ttImportShipTo.Contact, iplIgnoreBlanks, INPUT-OUTPUT shipto.contact).
    RUN pAssignValueC (ipbf-ttImportShipTo.PhoneArea, iplIgnoreBlanks, INPUT-OUTPUT shipto.area-code).
    RUN pAssignValueC (ipbf-ttImportShipTo.Phone, iplIgnoreBlanks, INPUT-OUTPUT shipto.phone).
    RUN pAssignValueC (ipbf-ttImportShipTo.Fax, iplIgnoreBlanks, INPUT-OUTPUT shipto.fax).
    RUN pAssignValueC (ipbf-ttImportShipTo.SalesRep, iplIgnoreBlanks, INPUT-OUTPUT shipto.spare-char-1).
    RUN pAssignValueC (ipbf-ttImportShipTo.TaxCode, iplIgnoreBlanks, INPUT-OUTPUT shipto.tax-code).
    RUN pAssignValueC (ipbf-ttImportShipTo.Warehouse, iplIgnoreBlanks, INPUT-OUTPUT shipto.loc).
    RUN pAssignValueC (ipbf-ttImportShipTo.Bin, iplIgnoreBlanks, INPUT-OUTPUT shipto.loc-bin).
    RUN pAssignValueC (ipbf-ttImportShipTo.Carrier, YES, INPUT-OUTPUT shipto.carrier).
    RUN pAssignValueC (ipbf-ttImportShipTo.Zone, YES, INPUT-OUTPUT shipto.dest-code).
    RUN pAssignValueC (ipbf-ttImportShipTo.Pallet, iplIgnoreBlanks, INPUT-OUTPUT shipto.pallet).
    RUN pAssignValueC (ipbf-ttImportShipTo.ShipperID, iplIgnoreBlanks, INPUT-OUTPUT shipto.spare-char-4).
    RUN pAssignValueC (ipbf-ttImportShipTo.MemberID, iplIgnoreBlanks, INPUT-OUTPUT shipto.spare-char-5).
    RUN pAssignValueC (ipbf-ttImportShipTo.cExportId, iplIgnoreBlanks, INPUT-OUTPUT shipto.exportCustID).
    RUN pAssignValueC (ipbf-ttImportShipTo.DockID, iplIgnoreBlanks, INPUT-OUTPUT shipto.dock-loc).
    RUN pAssignValueC (ipbf-ttImportShipTo.DockHours, iplIgnoreBlanks, INPUT-OUTPUT shipto.dock-hour).
    RUN pAssignValueC (ipbf-ttImportShipTo.Charge, iplIgnoreBlanks, INPUT-OUTPUT shipto.del-chg).
    RUN pAssignValueC (ipbf-ttImportShipTo.DaysTransit, iplIgnoreBlanks, INPUT-OUTPUT shipto.del-time).
    RUN pAssignValueI (ipbf-ttImportShipTo.DaysSamples, iplIgnoreBlanks, INPUT-OUTPUT shipto.spare-int-1).
    RUN pAssignValueI (ipbf-ttImportShipTo.DaysDockAppt, iplIgnoreBlanks, INPUT-OUTPUT shipto.spare-int-2).
    RUN pAssignValueI (ipbf-ttImportShipTo.DaysEarliestAllowed, iplIgnoreBlanks, INPUT-OUTPUT shipto.spare-int-3).
    RUN pAssignValueI (ipbf-ttImportShipTo.DaysLatestAllowed, iplIgnoreBlanks, INPUT-OUTPUT shipto.spare-int-4).
    RUN pAssignValueCToL (ipbf-ttImportShipTo.ShipByCaseAllowed,"Y",iplIgnoreBlanks, INPUT-OUTPUT shipto.ship-meth).
    RUN pAssignValueCToL (ipbf-ttImportShipTo.Billable,"Y",iplIgnoreBlanks, INPUT-OUTPUT shipto.bill).
    RUN pAssignValueCToL (ipbf-ttImportShipTo.Broker,"Y",iplIgnoreBlanks, INPUT-OUTPUT shipto.broker).
    RUN pAssignValueC (ipbf-ttImportShipTo.Note1, iplIgnoreBlanks, INPUT-OUTPUT shipto.notes[1]).
    RUN pAssignValueC (ipbf-ttImportShipTo.Note2, iplIgnoreBlanks, INPUT-OUTPUT shipto.notes[2]).
    RUN pAssignValueC (ipbf-ttImportShipTo.Note3, iplIgnoreBlanks, INPUT-OUTPUT shipto.notes[3]).
    RUN pAssignValueC (ipbf-ttImportShipTo.Note4, iplIgnoreBlanks, INPUT-OUTPUT shipto.notes[4]).
    RUN pAssignValueC (ipbf-ttImportShipTo.cManTax, YES, INPUT-OUTPUT shipto.tax-mandatory).
    IF ipbf-ttImportShipTo.cInactive EQ "I" AND DYNAMIC-FUNCTION("IsActive",shipto.rec_key) THEN DO:
     RUN AddTagInactive(shipto.rec_key,"shipto").
     shipto.statusCode = "I".
    END.
    ELSE IF ipbf-ttImportShipTo.cInactive EQ "" AND NOT DYNAMIC-FUNCTION("IsActive",shipto.rec_key) THEN DO: 
     RUN ClearTagsInactive(shipto.rec_key).
     shipto.statusCode = "".
    END.
    
    RELEASE shipto.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetNextShipNo RETURNS INTEGER 
    ( ipcCompany AS CHARACTER ,
    ipcCustNo AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next ship-no for a given customer 
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE iNextShipNo AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-shipto FOR shipto.
    FIND LAST bf-shipto NO-LOCK 
        WHERE bf-shipto.company EQ ipcCompany
        AND bf-shipto.cust-no EQ ipcCustNo 
        USE-INDEX ship-no
        NO-ERROR.
    iNextShipNo = IF AVAILABLE bf-shipto THEN bf-shipto.ship-no + 1 ELSE 1.
    RETURN iNextShipNo.
    
END FUNCTION.

