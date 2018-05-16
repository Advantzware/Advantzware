
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

DEFINE TEMP-TABLE ttImportRelease
    FIELD Company            AS CHARACTER 
    FIELD Location           AS CHARACTER  
    FIELD iOrderNo           AS INTEGER   FORMAT ">>>>>9" COLUMN-LABEL "Order No" HELP "Required - Must be Valid - Size:6"
    FIELD cItemID            AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Item" HELP "Required - Must be valid - Size:15"
    FIELD cReleaseType       AS CHARACTER FORMAT "!" INITIAL "B" COLUMN-LABEL "Release Type" HELP "Optional - Must be valid - Defaults to B - Size:1"
    FIELD cShipTo            AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Ship To" HELP "Required - Field Validated - Size:8"
    FIELD cShipFrom          AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Ship From" HELP "Required - Field Validated - size:5"
    FIELD cCarrier           AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Carrier" HELP "Required - Must be valid-based on Ship From - size:5"
    FIELD dQuantityScheduled AS DECIMAL   FORMAT "->>,>>>,>>9" COLUMN-LABEL "Sched Qty" HELP "Optional - Size:11"
    FIELD cCustomerPO        AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Customer PO#" HELP "Required - Field Validated - Size:15"
    FIELD cCustomerLot       AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Customer Lot #" HELP "Optional - Size:15"
    FIELD dReleaseDate       AS DATE      INITIAL TODAY FORMAT "99/99/9999" COLUMN-LABEL "Rel Date" HELP "Defaults to Today - Field Validated"
    FIELD dSellPrice         AS DECIMAL   FORMAT ">>,>>>,>>9.99<<<<" COLUMN-LABEL "Sell Price" HELP "optional - decimal"
    FIELD dSellPriceIsZero   AS DECIMAL   FORMAT "9" COLUMN-LABEL "$0" INITIAL 0 HELP "Required - size:1"
    FIELD cFreightPay        AS CHARACTER FORMAT "x(1)" COLUMN-LABEL "Frt Pay" HELP "Optional - Must be P,C,B,T"
    FIELD cFOB               AS CHARACTER FORMAT "x(1)" COLUMN-LABEL "FOB" HELP "Optional - Must be D(est) or O(rig)".
    

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

                                    
/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportRelease"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportRelease FOR ttImportRelease.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportRelease FOR ttImportRelease.

    DEFINE VARIABLE iFutureDays AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lContinue   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidate   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValid      AS LOGICAL   NO-UNDO.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.iOrderNo EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Order No.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.cItemID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Item.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.cShipTo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Ship To.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.cCarrier EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Carrier.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.cShipFrom EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Ship From.".
    END.

    IF oplValid AND iplFieldValidation THEN 
    DO:
        
        IF oplValid AND ipbf-ttImportRelease.iOrderNo NE 0 THEN 
        DO:

            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ ipbf-ttImportRelease.Company
                AND oe-ord.ord-no  EQ ipbf-ttImportRelease.iOrderNo NO-ERROR.
            IF NOT AVAILABLE oe-ord THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Order No."
        
                    .
        END.

        IF oplValid AND ipbf-ttImportRelease.cItemID NE "" THEN 
        DO:

            FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ ipbf-ttImportRelease.Company
                AND oe-ordl.ord-no  EQ INTEGER(ipbf-ttImportRelease.iOrderNo )
                AND oe-ordl.i-no    EQ ipbf-ttImportRelease.cItemID 
                NO-ERROR.
            IF NOT AVAILABLE oe-ordl THEN
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Item No."
                    .
        END.
        IF oplValid AND ipbf-ttImportRelease.cReleaseType NE "" THEN 
        DO:
            IF LOOKUP(ipbf-ttImportRelease.cReleaseType,"B,I,S,T") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Release Type"
                    .
        END.
        IF oplValid AND ipbf-ttImportRelease.cShipTo NE "" THEN 
        DO:

            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ ipbf-ttImportRelease.Company 
                AND oe-ord.ord-no  EQ ipbf-ttImportRelease.iOrderNo NO-ERROR.

            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ ipbf-ttImportRelease.Company 
                AND shipto.cust-no EQ oe-ord.cust-no
                AND shipto.ship-id EQ ipbf-ttImportRelease.cShipTo 
                USE-INDEX ship-id NO-ERROR.
            
            IF NOT AVAILABLE shipto AND ipbf-ttImportRelease.cReleaseType EQ "T" THEN 
            DO:
                FOR EACH cust NO-LOCK
                    WHERE cust.company EQ ipbf-ttImportRelease.Company 
                    AND cust.active  EQ "X",
                    EACH shipto
                    WHERE shipto.company EQ cust.company
                    AND shipto.cust-no EQ cust.cust-no
                    AND shipto.ship-id EQ ipbf-ttImportRelease.cShipTo :
                    LEAVE.
                END.
                IF NOT AVAILABLE shipto THEN 
                    ASSIGN
                        oplValid = NO 
                        opcNote  = "Invalid Ship To."
                        .
            END.
            IF NOT AVAILABLE shipto AND LOOKUP(ipbf-ttImportRelease.cReleaseType,"B,I,S")  NE 0 THEN 
            DO:
                FOR EACH cust NO-LOCK
                    WHERE cust.company EQ ipbf-ttImportRelease.Company 
                    AND cust.active  EQ "X",
                    EACH shipto
                    WHERE shipto.company EQ cust.company
                    AND shipto.cust-no EQ cust.cust-no
                    AND shipto.ship-id EQ ipbf-ttImportRelease.cShipTo:
                    LEAVE.
                END.
                IF AVAILABLE shipto THEN 
                DO:
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Billable Releases must be shipped to Ship To Locations for Customer on Order..."
                        .
                END.
                ELSE 
                DO:
                    ASSIGN
                        oplValid = NO 
                        opcNote  = "Invalid Ship To."
                        .
                END.
            END.
        END.

        IF oplValid AND ipbf-ttImportRelease.cCarrier NE "" THEN 
        DO:

            FIND FIRST carrier NO-LOCK 
                WHERE carrier.company EQ ipbf-ttImportRelease.Company
                AND carrier.carrier EQ ipbf-ttImportRelease.cCarrier NO-ERROR.
            IF NOT AVAILABLE carrier THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Carrier"
                    .
                
        END.
        IF oplValid AND ipbf-ttImportRelease.cShipFrom NE "" THEN 
        DO:

            FIND FIRST loc NO-LOCK 
                WHERE loc.company EQ ipbf-ttImportRelease.Company
                AND loc.loc EQ ipbf-ttImportRelease.cShipFrom
                NO-ERROR.
            IF NOT AVAILABLE loc THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Warehouse".
        END.
        
        IF oplValid AND ipbf-ttImportRelease.cCustomerPO NE "" THEN 
        DO:
            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ ipbf-ttImportRelease.Company
                AND oe-ord.ord-no  EQ ipbf-ttImportRelease.iOrderNo
                NO-ERROR.
           
            IF AVAILABLE oe-ord THEN
                FIND FIRST cust NO-LOCK
                    WHERE cust.company EQ oe-ord.company
                    AND cust.cust-no EQ oe-ord.cust-no
                    AND cust.po-mandatory
                    NO-ERROR.
            IF AVAILABLE cust AND ipbf-ttImportRelease.cCustomerPO EQ "" THEN 
            DO:
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "PO# is mandatory for this Customer...".
            END.
        END.

        IF oplValid AND ipbf-ttImportRelease.cFreightPay NE "" THEN 
        DO:

            IF LOOKUP(ipbf-ttImportRelease.cFreightPay,"P,C,B,T") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Frt Pay"
                    .
        END.
        IF oplValid AND ipbf-ttImportRelease.cFOB NE "" THEN 
        DO:

            IF LOOKUP(ipbf-ttImportRelease.cFOB,"O,D") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid FOB"
                    .
        END.
        IF oplValid AND ipbf-ttImportRelease.dReleaseDate NE ? THEN 
        DO:

            RUN sys/ref/nk1look.p (ipbf-ttImportRelease.Company, "OeDateWarn", "I", NO, NO, "", "", 
                OUTPUT cReturn, OUTPUT lFound).
            IF lFound THEN
                iFutureDays = INT(cReturn).
            ELSE
                iFutureDays = 0.
            RUN sys/ref/nk1look.p (ipbf-ttImportRelease.Company, "OeDateWarn", "L", NO, NO, "", "", 
                OUTPUT cReturn, OUTPUT lFound).    
            lValidate = LOGICAL(cReturn).
          
            IF iFutureDays EQ 0 THEN    
                iFutureDays = 90.
          
            ASSIGN 
                lContinue = TRUE
                lValid    = TRUE.
            IF lValidate THEN 
            DO:
                lValid = YES.
                IF ipbf-ttImportRelease.dReleaseDate GT TODAY + iFutureDays THEN 
                DO:
                    lValid = NO.
                    ASSIGN 
                        oplValid = NO
                        opcNote  = "Date entered " + STRING(ipbf-ttImportRelease.dReleaseDate) +  ' is is more than ' + STRING(iFutureDays) +  " days into the future."
                        .
                END.
            END.
        END.
    END.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportRelease FOR ttImportRelease.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    DEFINE VARIABLE inextRelNo          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cShipId             LIKE oe-rel.ship-id NO-UNDO.
    DEFINE VARIABLE cCarrier            LIKE oe-rel.carrier NO-UNDO.
    DEFINE VARIABLE cFirstShipId        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFirstReleaseOfItem AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCustNo             LIKE cust.cust-no NO-UNDO.

    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    DEFINE BUFFER bf-cust   FOR cust.

    FOR EACH cust NO-LOCK
        WHERE cust.company EQ ipbf-ttImportRelease.Company
        AND cust.active  EQ "X":
        cCustNo = cust.cust-no.
        LEAVE.
    END.

    FIND FIRST oe-ord NO-LOCK 
        WHERE oe-ord.company EQ ipbf-ttImportRelease.Company
        AND oe-ord.ord-no EQ ipbf-ttImportRelease.iOrderNo NO-ERROR.

    IF AVAILABLE oe-ord THEN
        FIND FIRST oe-ordl NO-LOCK 
            WHERE oe-ordl.company EQ ipbf-ttImportRelease.Company
            AND oe-ordl.ord-no EQ ipbf-ttImportRelease.iOrderNo
            AND oe-ordl.i-no EQ ipbf-ttImportRelease.cItemID NO-ERROR.
    
    IF AVAILABLE oe-ordl THEN
        FIND FIRST oe-rel EXCLUSIVE-LOCK
            WHERE oe-rel.company EQ oe-ord.company
            AND oe-rel.ord-no EQ oe-ordl.ord-no
            AND oe-rel.i-no EQ oe-ordl.i-no 
            AND oe-rel.LINE EQ oe-ordl.LINE NO-ERROR.
    
    iopiAdded = iopiAdded + 1.
          
    RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT inextRelNo).
    CREATE oe-rel.
    ASSIGN
        oe-rel.company  = ipbf-ttImportRelease.Company
        oe-rel.loc      = ipbf-ttImportRelease.Location
        oe-rel.r-no     = inextRelNo
        oe-rel.ord-no   = oe-ordl.ord-no
        oe-rel.i-no     = oe-ordl.i-no
        oe-rel.cust-no  = oe-ord.cust-no
        oe-rel.rel-date = TODAY
        oe-rel.line     = oe-ordl.line. 
    
    FIND FIRST bf-cust NO-LOCK
        WHERE bf-cust.cust-no EQ oe-ord.cust-no NO-ERROR.

    ASSIGN
        cShipId = ipbf-ttImportRelease.cShipTo .
    cCarrier =  ipbf-ttImportRelease.cCarrier.

    IF AVAILABLE(bf-cust) AND bf-cust.ACTIVE EQ "X" AND ipbf-ttImportRelease.cShipTo GT "" THEN
        cShipId = ipbf-ttImportRelease.cShipTo.

    FIND FIRST bf-oe-rel NO-LOCK
        WHERE bf-oe-rel.company EQ oe-ord.company
        AND bf-oe-rel.ord-no EQ oe-ord.ord-no
        AND bf-oe-rel.i-no EQ oe-ordl.i-no
        AND bf-oe-rel.LINE EQ oe-ordl.LINE NO-ERROR.

    cFirstShipId = ipbf-ttImportRelease.cShipTo.
    
    IF AVAILABLE oe-ordl THEN 
    DO:
        FIND FIRST oe-ord OF oe-ordl NO-LOCK.

        FIND FIRST sys-ctrl NO-LOCK
            WHERE sys-ctrl.company EQ ipbf-ttImportRelease.Company
            AND sys-ctrl.name EQ "OECARIER" NO-ERROR.

        IF cCarrier EQ "" THEN 
        DO:  /* NK1 OECARIER */
            IF sys-ctrl.char-fld EQ "ShipTo" THEN 
            DO:
                FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ oe-ord.company
                    AND shipto.cust-no EQ oe-ord.cust-no
                    AND shipto.ship-id EQ cShipId NO-ERROR.
                
                cCarrier = IF AVAILABLE shipto THEN shipto.carrier ELSE "".
            END.
            ELSE IF sys-ctrl.char-fld EQ "Header" THEN cCarrier = oe-ord.carrier.
        END.
        FIND FIRST shipto NO-LOCK
            WHERE shipto.company EQ oe-ord.company
            AND shipto.cust-no EQ oe-ord.cust-no
            AND shipto.ship-id EQ cShipId NO-ERROR.

        IF NOT AVAILABLE shipto THEN
            FOR EACH shipto NO-LOCK
                WHERE shipto.company  EQ ipbf-ttImportRelease.Company
                AND shipto.cust-no EQ (IF cCustNo NE "" AND
                oe-rel.s-code EQ "T" THEN cCustNo
                ELSE oe-ord.cust-no)
                BREAK BY shipto.ship-no DESCENDING:

                IF shipto.ship-id EQ oe-ord.cust-no OR LAST(shipto.ship-no) THEN LEAVE.

            END.
     
        IF cCarrier EQ "" AND AVAILABLE shipto THEN cCarrier = shipto.carrier.
        FIND FIRST bf-oe-rel NO-LOCK
            WHERE bf-oe-rel.company EQ ipbf-ttImportRelease.Company
            AND bf-oe-rel.ord-no EQ oe-ord.ord-no
            AND bf-oe-rel.i-no EQ oe-ordl.i-no
            AND ROWID(bf-oe-rel) NE ROWID(oe-rel) NO-ERROR.
    
        iFirstReleaseOfItem = IF AVAILABLE bf-oe-rel THEN NO ELSE YES.  

        ASSIGN 
            oe-rel.i-no         = ipbf-ttImportRelease.cItemID 
            oe-rel.cust-no      = oe-ord.cust-no
            oe-rel.qty          = 0 
            oe-rel.fob-code     = ipbf-ttImportRelease.cFOB
            oe-rel.frt-pay      = ipbf-ttImportRelease.cFreightPay
            oe-rel.zeroPrice    = ipbf-ttImportRelease.dSellPriceIsZero
            oe-rel.sell-price   = ipbf-ttImportRelease.dSellPrice
            oe-rel.lot-no       = ipbf-ttImportRelease.cCustomerLot
            oe-rel.rel-date     = ipbf-ttImportRelease.dReleaseDate
            oe-rel.tot-qty      = ipbf-ttImportRelease.dQuantityScheduled
            oe-rel.po-no        = ipbf-ttImportRelease.cCustomerPO
            oe-rel.s-code       = ipbf-ttImportRelease.cReleaseType
            oe-rel.s-comm[1]    = oe-ord.s-comm[1]
            oe-rel.s-comm[2]    = oe-ord.s-comm[2]
            oe-rel.s-comm[3]    = oe-ord.s-comm[3]
            oe-rel.s-name[1]    = oe-ord.sname[1]
            oe-rel.s-name[2]    = oe-ord.sname[2]
            oe-rel.s-name[3]    = oe-ord.sname[3]
            oe-rel.s-pct[1]     = oe-ord.s-pct[1]
            oe-rel.s-pct[2]     = oe-ord.s-pct[2]
            oe-rel.s-pct[3]     = oe-ord.s-pct[3]
            oe-rel.sman[1]      = oe-ord.sman[1]
            oe-rel.sman[2]      = oe-ord.sman[2]
            oe-rel.sman[3]      = oe-ord.sman[3]
            oe-rel.sold-no      = oe-ord.sold-no
            oe-rel.carrier      = cCarrier
            oe-rel.ship-id      = cShipId
            oe-rel.spare-char-1 = ipbf-ttImportRelease.cShipFrom.
        .
        IF ipbf-ttImportRelease.cCustomerPO EQ "" THEN
            oe-rel.po-no = IF oe-ordl.po-no NE "" THEN oe-ordl.po-no 
            ELSE oe-ord.po-no.
        IF ipbf-ttImportRelease.cReleaseType EQ "" THEN
            oe-rel.s-code  = "B".
        IF ipbf-ttImportRelease.dReleaseDate EQ ? THEN
            oe-rel.rel-date     = TODAY.

        IF iFirstReleaseOfItem THEN 
            oe-rel.spare-char-4 = STRING(oe-ord.due-date) + ",,". 
      
                                  
        IF oe-rel.qty LT 0 THEN oe-rel.qty = 0.

        IF AVAILABLE shipto THEN
            ASSIGN
                oe-rel.ship-addr[1] = shipto.ship-addr[1]
                oe-rel.ship-city    = shipto.ship-city
                oe-rel.ship-state   = shipto.ship-state
                oe-rel.ship-zip     = shipto.ship-zip
                oe-rel.ship-no      = shipto.ship-no
                oe-rel.ship-id      = IF cFirstShipId NE "" THEN cFirstShipId ELSE shipto.ship-id
                oe-rel.ship-i[1]    = shipto.notes[1]
                oe-rel.ship-i[2]    = shipto.notes[2]
                oe-rel.ship-i[3]    = shipto.notes[3]
                oe-rel.ship-i[4]    = shipto.notes[4].
        ELSE 
            ASSIGN 
                oe-rel.ship-no   = oe-ord.sold-no
                oe-rel.ship-id   = IF cFirstShipId NE "" THEN cFirstShipId ELSE oe-ord.sold-id
                oe-rel.ship-i[1] = oe-ord.ship-i[1]
                oe-rel.ship-i[2] = oe-ord.ship-i[2]
                oe-rel.ship-i[3] = oe-ord.ship-i[3]
                oe-rel.ship-i[4] = oe-ord.ship-i[4].

        IF NOT CAN-FIND(FIRST shipto 
            WHERE shipto.company EQ ipbf-ttImportRelease.Company
            AND shipto.ship-id EQ oe-rel.ship-id) THEN 
        do:
            
            FOR EACH shipto NO-LOCK
                WHERE shipto.company EQ ipbf-ttImportRelease.Company
                AND shipto.cust-no EQ oe-rel.cust-no
                BY shipto.ship-id:

                IF AVAILABLE shipto THEN
                    ASSIGN 
                        oe-rel.ship-id      = shipto.ship-id
                        oe-rel.ship-addr[1] = shipto.ship-addr[1]
                        oe-rel.ship-city    = shipto.ship-city
                        oe-rel.ship-state   = shipto.ship-state
                        oe-rel.ship-zip     = shipto.ship-zip
                        oe-rel.ship-no      = shipto.ship-no
                        oe-rel.ship-i[1]    = shipto.notes[1]
                        oe-rel.ship-i[2]    = shipto.notes[2]
                        oe-rel.ship-i[3]    = shipto.notes[3]
                        oe-rel.ship-i[4]    = shipto.notes[4].
                LEAVE .
            END.
        END.
    END.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

