
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
    FIELD Company             AS CHARACTER 
    FIELD Location            AS CHARACTER  
    FIELD OrderNo             AS INTEGER FORMAT ">>>>>9" COLUMN-LABEL "Order No" HELP "Required - Must be Valid - Size:6"
    FIELD ItemID              AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Item" HELP "Required - Must be valid - Size:15"
    FIELD ReleaseType         AS CHARACTER FORMAT "!" INITIAL "B" COLUMN-LABEL "Release Type" HELP "Optional - Must be valid - Defaults to B - Size:1"
    FIELD ShipTo              AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Ship To" HELP "Required - Field Validated - Size:8"
    FIELD ShipFrom             AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Ship From" HELP "Required - Field Validated - size:5"
    FIELD Carrier             AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Carrier" HELP "Required - Must be valid-based on Ship From - size:5"
    FIELD QuantityScheduled   AS DECIMAL   FORMAT "->>,>>>,>>9" COLUMN-LABEL "Sched Qty" HELP "Optional - Size:11"
    FIELD CustomerPO          AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Customer PO#" HELP "Required - Field Validated - Size:15"
    FIELD CustomerLot         AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Customer Lot #" HELP "Optional - Size:15"
    FIELD ReleaseDate         AS DATE INITIAL TODAY  FORMAT "99/99/9999" COLUMN-LABEL "Rel Date" HELP "Defaults to Today - Field Validated"
    FIELD SellPrice           AS DECIMAL      FORMAT ">>,>>>,>>9.99<<<<" COLUMN-LABEL "Sell Price" HELP "optional - decimal"
    FIELD SellPriceIsZero     AS DECIMAL      FORMAT "9" COLUMN-LABEL "$0" INITIAL 0 HELP "Required - size:1"
    FIELD FreightPay          AS CHARACTER FORMAT "x(1)" COLUMN-LABEL "Frt Pay" HELP "Optional - Must be P,C,B,T"
    FIELD FOB                 AS CHARACTER FORMAT "x(1)" COLUMN-LABEL "FOB" HELP "Optional - Must be D(est) or O(rig)".
    

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

   DEFINE VARIABLE iFutureDays AS INTEGER NO-UNDO.
   DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
   DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
   DEFINE VARIABLE lValidate AS LOGICAL NO-UNDO.
   DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.

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
        IF ipbf-ttImportRelease.OrderNo EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Order No.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.ItemID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Item.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.ShipTo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Ship To.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.carrier EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Carrier.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportRelease.ShipFrom EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Ship From.".
    END.

    IF oplValid AND iplFieldValidation THEN 
    DO:
        
        IF oplValid AND ipbf-ttImportRelease.OrderNo NE 0 THEN 
        DO:

            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ ipbf-ttImportRelease.Company
                AND oe-ord.ord-no  EQ ipbf-ttImportRelease.OrderNo NO-ERROR.
            IF NOT AVAILABLE oe-ord THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Order No."
        
                    .
        END.

        IF oplValid AND ipbf-ttImportRelease.ItemID NE "" THEN 
        DO:

            FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ ipbf-ttImportRelease.Company
                AND oe-ordl.ord-no  EQ INTEGER(ipbf-ttImportRelease.OrderNo )
                AND oe-ordl.i-no    EQ ipbf-ttImportRelease.ItemID 
                NO-ERROR.
            IF NOT AVAILABLE oe-ordl THEN
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Item No."
                    .
        END.
        IF oplValid AND ipbf-ttImportRelease.ReleaseType NE "" THEN 
            DO:
            IF LOOKUP(ipbf-ttImportRelease.ReleaseType,"B,I,S,T") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Release Type"
                    .
        END.
        IF oplValid AND ipbf-ttImportRelease.ShipTo NE "" THEN 
        DO:

            FIND FIRST oe-ord
                WHERE oe-ord.company EQ ipbf-ttImportRelease.Company 
                AND oe-ord.ord-no  EQ ipbf-ttImportRelease.OrderNo
                NO-LOCK.

            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ ipbf-ttImportRelease.Company 
                AND shipto.cust-no EQ oe-ord.cust-no
                AND shipto.ship-id EQ ipbf-ttImportRelease.ShipTo 
                USE-INDEX ship-id NO-ERROR.
            
            IF NOT AVAIL shipto AND ipbf-ttImportRelease.ReleaseType EQ "T" THEN DO:
                FOR EACH cust NO-LOCK
                    WHERE cust.company EQ ipbf-ttImportRelease.Company 
                    AND cust.active  EQ "X",
                    EACH shipto
                    WHERE shipto.company EQ cust.company
                    AND shipto.cust-no EQ cust.cust-no
                    AND shipto.ship-id EQ ipbf-ttImportRelease.ShipTo :
                    LEAVE.
                END.
                IF NOT AVAIL shipto THEN 
                    ASSIGN
                    oplValid = NO 
                    opcNote  = "Invalid Ship To."
                    .
            END.
            IF NOT AVAIL shipto AND LOOKUP(ipbf-ttImportRelease.ReleaseType,"B,I,S")  <> 0 THEN DO:
                FOR EACH cust NO-LOCK
                    WHERE cust.company EQ ipbf-ttImportRelease.Company 
                    AND cust.active  EQ "X",
                    EACH shipto
                    WHERE shipto.company EQ cust.company
                    AND shipto.cust-no EQ cust.cust-no
                    AND shipto.ship-id EQ ipbf-ttImportRelease.ShipTo:
                    LEAVE.
                END.
                IF AVAIL shipto THEN DO:
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Billable Releases must be shipped to Ship To Locations for Customer on Order..."
                        .
                END.
                ELSE DO:
                    ASSIGN
                        oplValid = NO 
                        opcNote  = "Invalid Ship To."
                        .
                END.
            END.
        END.

        IF oplValid AND ipbf-ttImportRelease.carrier NE "" THEN 
        DO:

           FIND FIRST carrier NO-LOCK 
               WHERE carrier.company EQ ipbf-ttImportRelease.Company
               AND carrier.carrier EQ ipbf-ttImportRelease.Carrier NO-ERROR.
           IF NOT AVAILABLE carrier THEN 
               ASSIGN 
                oplValid = NO 
                opcNote  = "Invalid Carrier"
                .
                
        END.
        IF oplValid AND ipbf-ttImportRelease.ShipFrom NE "" THEN 
        DO:

           FIND FIRST loc NO-LOCK 
                WHERE loc.company EQ ipbf-ttImportRelease.Company
                AND loc.loc EQ ipbf-ttImportRelease.ShipFrom
                NO-ERROR.
            IF NOT AVAILABLE loc THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Warehouse".
        END.
        
        IF oplValid AND ipbf-ttImportRelease.CustomerPO NE "" THEN 
        DO:
           FIND FIRST oe-ord NO-LOCK
               WHERE oe-ord.company EQ ipbf-ttImportRelease.Company
               AND oe-ord.ord-no  EQ ipbf-ttImportRelease.OrderNo
               NO-ERROR.
           
           IF AVAIL oe-ord THEN
               FIND FIRST cust NO-LOCK
               WHERE cust.company EQ oe-ord.company
               AND cust.cust-no EQ oe-ord.cust-no
               AND cust.po-mandatory
               NO-ERROR.
           IF AVAIL cust AND ipbf-ttImportRelease.CustomerPO EQ "" THEN DO:
               ASSIGN 
                    oplValid = NO 
                    opcNote  = "PO# is mandatory for this Customer...".
           END.
        END.

        IF oplValid AND ipbf-ttImportRelease.FreightPay NE "" THEN 
        DO:

           IF LOOKUP(ipbf-ttImportRelease.FreightPay,"P,C,B,T") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Frt Pay"
                    .
        END.
        IF oplValid AND ipbf-ttImportRelease.FOB NE "" THEN 
        DO:

           IF LOOKUP(ipbf-ttImportRelease.FOB,"O,D") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid FOB"
                    .
        END.
        IF oplValid AND ipbf-ttImportRelease.ReleaseDate NE ? THEN 
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
          
          ASSIGN lContinue = TRUE
              lValid    = TRUE.
          IF lValidate THEN DO:
              lValid = YES.
            IF ipbf-ttImportRelease.ReleaseDate GT TODAY + iFutureDays THEN DO:
                lValid = NO.
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Date entered " + STRING(ipbf-ttImportRelease.ReleaseDate) +  ' is is more than ' + STRING(iFutureDays) +  " days into the future."
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

    DEF VAR v-qty-sum    AS INT  NO-UNDO.
    DEF VAR v-nxt-r-no   AS INT  NO-UNDO.
    DEF VAR v-lst-rel    AS DATE INIT TODAY NO-UNDO.
    DEF VAR v-pct-chg    AS DEC  NO-UNDO.
    DEF VAR v-ship-id    LIKE oe-rel.ship-id NO-UNDO.
    DEF VAR v-carrier    LIKE oe-rel.carrier NO-UNDO.
    DEF VAR v-shipfrom   LIKE loc.loc NO-UNDO.
    DEF VAR v-num-shipto AS INT  NO-UNDO.
    DEF VAR v-qty-mod    AS LOG  NO-UNDO.
    DEF BUFFER bf-rel  FOR oe-rel.
    DEF BUFFER bf-cust FOR cust.
    DEF    VAR      v-first-ship-id     AS cha     NO-UNDO.
    DEF    VAR      v-qty-released      AS INT     NO-UNDO.
    DEFINE VARIABLE rShipTo AS ROWID NO-UNDO.
    DEFINE VARIABLE lFirstReleaseOfItem AS LOGICAL NO-UNDO.
    DEF VAR lv-rel-recid AS RECID NO-UNDO.
    DEFINE VARIABLE clvtext AS CHARACTER NO-UNDO .
    DEF VAR lv-cust-x LIKE cust.cust-no NO-UNDO.

    FOR EACH cust NO-LOCK
    WHERE cust.company EQ ipbf-ttImportRelease.Company
      AND cust.active  EQ "X":
      lv-cust-x = cust.cust-no.
      LEAVE.
    END.

    FIND FIRST oe-ord NO-LOCK 
       WHERE oe-ord.company EQ ipbf-ttImportRelease.Company
        AND oe-ord.ord-no EQ ipbf-ttImportRelease.OrderNo NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST oe-ordl NO-LOCK 
        WHERE oe-ordl.company EQ ipbf-ttImportRelease.Company
        AND oe-ordl.ord-no EQ ipbf-ttImportRelease.OrderNo
        AND oe-ordl.i-no EQ ipbf-ttImportRelease.ItemID
        NO-ERROR.
    IF AVAIL oe-ordl THEN
        FIND FIRST oe-rel EXCLUSIVE-LOCK
        WHERE oe-rel.company = oe-ord.company
        AND oe-rel.ord-no = oe-ordl.ord-no
        AND oe-rel.i-no = oe-ordl.i-no 
        AND oe-rel.LINE = oe-ordl.LINE NO-ERROR.
   
       iopiAdded = iopiAdded + 1.

        RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
        CREATE oe-rel.
        ASSIGN
            oe-rel.company      = ipbf-ttImportRelease.Company
            oe-rel.loc          = ipbf-ttImportRelease.Location
            oe-rel.r-no         = v-nxt-r-no
            oe-rel.ord-no       = oe-ordl.ord-no
            oe-rel.i-no         = oe-ordl.i-no
            oe-rel.cust-no      = oe-ord.cust-no
            oe-rel.rel-date     = TODAY  
            oe-rel.line         = oe-ordl.line
            . 
    
    FIND FIRST bf-cust WHERE bf-cust.cust-no EQ oe-ord.cust-no NO-LOCK NO-ERROR.
    ASSIGN
        v-ship-id =  ipbf-ttImportRelease.shipTo 
        v-carrier =  ipbf-ttImportRelease.carrier 
        .
    IF AVAIL(bf-cust) AND bf-cust.ACTIVE = "X" AND ipbf-ttImportRelease.ShipTo GT "" THEN
        v-ship-id = ipbf-ttImportRelease.ShipTo.
    FIND FIRST bf-rel WHERE bf-rel.company = oe-ord.company
        AND bf-rel.ord-no = oe-ord.ord-no
        AND bf-rel.i-no = oe-ordl.i-no 
        AND bf-rel.LINE = oe-ordl.LINE
        NO-LOCK NO-ERROR.
    v-first-ship-id = ipbf-ttImportRelease.ShipTo.
    
    lv-rel-recid = RECID(oe-rel).
    ASSIGN 
        v-qty-sum      = 0
        v-qty-released = 0.

    IF AVAIL oe-ordl THEN 
    DO:
        FIND FIRST oe-ord OF oe-ordl NO-LOCK.

        FOR EACH bf-rel WHERE bf-rel.company = oe-ord.company
            AND bf-rel.ord-no = oe-ord.ord-no
            AND bf-rel.i-no = oe-ordl.i-no 
            AND bf-rel.LINE = oe-ordl.LINE
            NO-LOCK:

            IF bf-rel.s-code = "" OR CAN-DO("B,S",bf-rel.s-code) THEN 
            DO:
                v-qty-sum = v-qty-sum + bf-rel.qty.
                IF LOOKUP(bf-rel.stat, "C,Z,P,A,B") GT 0 THEN
                    v-qty-released = v-qty-released + bf-rel.qty .  /*Task 11011304  */ /* task  09021403*/
                ELSE
                    v-qty-released = v-qty-released + ipbf-ttImportRelease.QuantityScheduled .  /*Task 11011304  */ /* task  09021403*/
            END.
        END.

        FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ ipbf-ttImportRelease.Company
            AND sys-ctrl.name    EQ "OECARIER"
            NO-LOCK NO-ERROR.
        
        IF v-carrier = "" THEN 
        DO:  /* NK1 OECARIER */
            IF sys-ctrl.char-fld EQ "ShipTo" THEN 
            DO:
                FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ oe-ord.company
                    AND shipto.cust-no EQ oe-ord.cust-no
                    AND shipto.ship-id EQ v-ship-id NO-ERROR.
                v-carrier = IF AVAIL shipto THEN shipto.carrier ELSE "".
            END.
            ELSE IF sys-ctrl.char-fld EQ "Header" THEN v-carrier = oe-ord.carrier.
        END.
        FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ oe-ord.company
                    AND shipto.cust-no EQ oe-ord.cust-no
                    AND shipto.ship-id EQ v-ship-id NO-ERROR.
    
        IF NOT AVAIL shipto THEN
            FOR EACH shipto
                WHERE shipto.company  EQ ipbf-ttImportRelease.Company
                AND shipto.cust-no EQ (IF lv-cust-x NE ""         AND
                oe-rel.s-code EQ "T" THEN lv-cust-x
                ELSE oe-ord.cust-no)
                NO-LOCK
                BREAK BY shipto.ship-no DESC:
                IF shipto.ship-id EQ oe-ord.cust-no OR LAST(shipto.ship-no) THEN LEAVE.
            END.
     
        IF v-carrier EQ "" AND AVAIL shipto THEN v-carrier = shipto.carrier.
        FIND FIRST bf-rel WHERE bf-rel.company EQ ipbf-ttImportRelease.Company
            AND bf-rel.ord-no EQ oe-ord.ord-no
            AND bf-rel.i-no EQ oe-ordl.i-no
            AND ROWID(bf-rel) NE ROWID(oe-rel)
            NO-LOCK NO-ERROR.
    
        lFirstReleaseOfItem = IF AVAIL bf-rel THEN NO ELSE YES.  

        ASSIGN 
            
            oe-rel.i-no         = ipbf-ttImportRelease.ItemID 
            oe-rel.cust-no      = oe-ord.cust-no
            oe-rel.qty          = 0 /*oe-ordl.qty - v-qty-sum*/
            oe-rel.fob-code     = ipbf-ttImportRelease.FOB
            oe-rel.frt-pay      = ipbf-ttImportRelease.FreightPay
            oe-rel.zeroPrice    = ipbf-ttImportRelease.SellPriceIsZero
            oe-rel.sell-price   = ipbf-ttImportRelease.SellPrice
            oe-rel.lot-no       = ipbf-ttImportRelease.CustomerLot
            oe-rel.rel-date     = ipbf-ttImportRelease.ReleaseDate
            oe-rel.tot-qty      = ipbf-ttImportRelease.QuantityScheduled
            oe-rel.po-no        = ipbf-ttImportRelease.CustomerPO
            oe-rel.s-code       = ipbf-ttImportRelease.ReleaseType
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
            oe-rel.carrier      = v-carrier
            oe-rel.ship-id      = v-ship-id
            oe-rel.spare-char-1 = ipbf-ttImportRelease.ShipFrom
             .
       IF ipbf-ttImportRelease.CustomerPO EQ "" THEN
        oe-rel.po-no        = IF oe-ordl.po-no NE "" THEN oe-ordl.po-no 
                                                     ELSE oe-ord.po-no.
       IF ipbf-ttImportRelease.ReleaseType EQ "" THEN
          oe-rel.s-code  = "B".
       IF ipbf-ttImportRelease.ReleaseDate EQ ? THEN
          oe-rel.rel-date     = TODAY.
       

        /* stores oe-rel due date */
        IF lfirstReleaseofItem THEN 
            oe-rel.spare-char-4 = STRING(oe-ord.due-date) + ",,". 
      
                                  
        IF oe-rel.qty LT 0 THEN oe-rel.qty = 0.

        IF AVAIL shipto THEN
            ASSIGN oe-rel.ship-addr[1] = shipto.ship-addr[1]
                oe-rel.ship-city    = shipto.ship-city
                oe-rel.ship-state   = shipto.ship-state
                oe-rel.ship-zip     = shipto.ship-zip
                oe-rel.ship-no      = shipto.ship-no
                oe-rel.ship-id      = IF v-first-ship-id <> "" THEN v-first-ship-id ELSE shipto.ship-id
                oe-rel.ship-i[1]    = shipto.notes[1]
                oe-rel.ship-i[2]    = shipto.notes[2]
                oe-rel.ship-i[3]    = shipto.notes[3]
                oe-rel.ship-i[4]    = shipto.notes[4].
        ELSE ASSIGN oe-rel.ship-no   = oe-ord.sold-no
                oe-rel.ship-id   = IF v-first-ship-id <> "" THEN v-first-ship-id ELSE oe-ord.sold-id
                oe-rel.ship-i[1] = oe-ord.ship-i[1]
                oe-rel.ship-i[2] = oe-ord.ship-i[2]
                oe-rel.ship-i[3] = oe-ord.ship-i[3]
                oe-rel.ship-i[4] = oe-ord.ship-i[4].

        IF NOT CAN-FIND(FIRST shipto 
               WHERE shipto.company EQ ipbf-ttImportRelease.Company
               AND shipto.ship-id EQ oe-rel.ship-id) THEN do:
            
           FOR EACH shipto
                WHERE shipto.company EQ ipbf-ttImportRelease.Company
                AND shipto.cust-no EQ oe-rel.cust-no NO-LOCK BY shipto.ship-id:

            IF AVAIL shipto THEN
            ASSIGN 
                oe-rel.ship-id   = shipto.ship-id
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

