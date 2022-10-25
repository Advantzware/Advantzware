/*------------------------------------------------------------------------
  File:         r-ontime.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 05.31.2022
------------------------------------------------------------------------*/
/*  Mod:                                                                */

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttOnTimeDeliveries 
{sys/inc/var.i}
{aoa/tempTable/ttOnTimeDeliveries.i}  

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE tt-date-reasons
    FIELD reason-code AS CHARACTER
    FIELD occurrences AS INTEGER.
    
DEFINE TEMP-TABLE ttgroup
    FIELD group-name AS CHARACTER
    FIELD deliver    AS INTEGER
    FIELD ontime     AS INTEGER . 
/* Local Variable Definitions ---                                       */

&Scoped-define subjectID 204
{AOA/includes/subjectID{&subjectID}Defs.i}    


/* subject business logic */
/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE dCheckQty      LIKE oe-ordl.qty NO-UNDO.
    DEFINE VARIABLE cLast          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStatus        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFGCategory    LIKE itemfg.procat NO-UNDO.
    DEFINE VARIABLE dSqft          LIKE itemfg.t-sqft NO-UNDO.
    DEFINE VARIABLE dtCompare      AS DATE      NO-UNDO.
    DEFINE VARIABLE lcXLLine       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dShipmentValue AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMsf           AS DECIMAL   FORMAT "->,>>>,>>9.9999" NO-UNDO.
    DEFINE VARIABLE cCustNo        LIKE cust.cust-no NO-UNDO.
    DEFINE VARIABLE cName          LIKE cust.NAME NO-UNDO.
    DEFINE VARIABLE iDel           AS INTEGER   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE iOnt           LIKE iDel NO-UNDO.
    
    /* subject business logic */
    FOR EACH oe-ord
        WHERE oe-ord.company  EQ cCompany
        AND oe-ord.cust-no  GE cStartCustNo
        AND oe-ord.cust-no  LE cEndCustNo
        AND (IF lCustList THEN CAN-FIND(FIRST ttCustList
        WHERE ttCustList.cust-no EQ oe-ord.cust-no
        AND ttCustList.log-fld EQ TRUE)
        ELSE TRUE)   
        AND oe-ord.ord-date GE dtStartOrderDate
        AND oe-ord.ord-date LE dtEndOrderDate
        USE-INDEX cust NO-LOCK,

        EACH oe-ordl OF oe-ord
        WHERE oe-ordl.i-no GE cStartFGItem
        AND oe-ordl.i-no LE cEndFGItem
        NO-LOCK,

        EACH oe-rel
        WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        AND oe-rel.link-no NE 0
        NO-LOCK,

        FIRST oe-rell
        WHERE oe-rell.company EQ oe-rel.company
        AND oe-rell.r-no    EQ oe-rel.link-no
        AND oe-rell.i-no    EQ oe-rel.i-no
        AND oe-rell.line    EQ oe-rel.line
        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
        USE-INDEX r-no NO-LOCK,

        EACH oe-boll
        WHERE oe-boll.company  EQ oe-rell.company
        AND oe-boll.r-no     EQ oe-rell.r-no
        AND oe-boll.ord-no   EQ oe-rell.ord-no
        AND oe-boll.rel-no   EQ oe-rell.rel-no
        AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
        AND oe-boll.i-no     EQ oe-rell.i-no
        AND oe-boll.line     EQ oe-rell.line
        NO-LOCK,

        FIRST oe-bolh
        WHERE oe-bolh.b-no     EQ oe-boll.b-no
        AND oe-bolh.bol-date GE dtStartBOLDate
        AND oe-bolh.bol-date LE dtEndBOLDate
        NO-LOCK

        BREAK BY oe-ord.cust-no
        BY oe-bolh.bol-date
        BY oe-ord.ord-no
        BY oe-ordl.i-no:

              
        ASSIGN
            dCheckQty  = 0
            cLast = "".
     
       
        FIND FIRST fg-rcpth NO-LOCK
            WHERE fg-rcpth.company   EQ oe-ordl.company
            AND fg-rcpth.job-no    EQ oe-ordl.job-no
            AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
            AND fg-rcpth.i-no      EQ oe-ordl.i-no
            AND fg-rcpth.rita-code EQ 'R'
            USE-INDEX tdate
            NO-ERROR.
        IF AVAILABLE fg-rcpth THEN
            cLast = STRING(fg-rcpth.trans-date,"99/99/99").         


        RUN fg/GetProductionQty.p (INPUT oe-ordl.company,
            INPUT oe-ordl.job-no,
            INPUT oe-ordl.job-no2,
            INPUT oe-ordl.i-no,
            INPUT NO,
            OUTPUT dCheckQty).

        IF dCheckQty LT oe-ordl.qty * (100 - oe-ordl.under-pct) / 100 THEN
            cLast = "Not Complete".

        cStatus = IF cLast EQ "Not Complete" THEN
            IF TODAY GT oe-ordl.req-date THEN "Late" ELSE ""
            ELSE
            IF DATE(cLast) LE oe-ordl.req-date THEN "On Time"
            ELSE TRIM(STRING(DATE(cLast) - oe-ordl.req-date),">,>>9").
        
        FIND FIRST itemfg WHERE itemfg.company = oe-boll.company AND 
            itemfg.i-no    = oe-boll.i-no NO-LOCK NO-ERROR.
        ASSIGN
            dSqft = IF AVAILABLE itemfg THEN itemfg.t-sqft ELSE 0.
        cFGCategory = IF AVAILABLE itemfg THEN itemfg.procat ELSE "".
        dMsf       = (oe-boll.qty * dSqft )/ 1000
            . 
      
        IF AVAILABLE oe-rell THEN
            FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
      
        IF FIRST-OF(oe-ord.cust-no) THEN 
        DO:
            FIND FIRST cust
                WHERE cust.company EQ cCompany
                AND cust.cust-no EQ oe-ord.cust-no
                NO-LOCK NO-ERROR.
            ASSIGN
                cCustNo = oe-ord.cust-no
                cName    = IF AVAILABLE cust THEN cust.name ELSE "Not on File".

            IF FIRST(oe-ord.cust-no) THEN DISPLAY "" WITH FRAME r-top.
            ELSE PAGE.
        END.

        iDel[1] = iDel[1] + 1.
        dtCompare = (IF cUserSelectionDate EQ "2" THEN oe-rel.rel-date
        ELSE oe-ordl.prom-date).
        IF oe-bolh.bol-date LE dtCompare THEN 
        DO:
            iOnt[1] = iOnt[1] + 1.

        END.
        ELSE 
        DO:
            FIND FIRST tt-date-reasons 
                WHERE tt-date-reasons.reason-code = oe-rel.spare-char-2
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tt-date-reasons THEN 
            DO:

                CREATE tt-date-reasons.
                tt-date-reasons.reason-code = oe-rel.spare-char-2.
            END.
            tt-date-reasons.occurrences = tt-date-reasons.occurrences + 1.
        END.

        FIND FIRST ttgroup NO-LOCK
            WHERE ttgroup.group-name EQ (IF AVAILABLE cust THEN  STRING(cust.spare-char-2) ELSE "") NO-ERROR.

        IF NOT AVAILABLE ttgroup THEN 
        DO:
            CREATE ttgroup .
            ASSIGN 
                ttgroup.group-name = (IF AVAILABLE cust THEN  STRING(cust.spare-char-2) ELSE "").
            ttgroup.deliver    = 1 .
            IF oe-bolh.bol-date LE dtCompare THEN
                ttgroup.ontime     =  1 .
        END.
        ELSE 
        DO:
            ttgroup.deliver    = ttgroup.deliver + 1 .
            IF oe-bolh.bol-date LE dtCompare THEN
                ttgroup.ontime     =ttgroup.ontime + 1 .
        END.
      
        RUN Conv_CalcTotalPrice(cCompany, 
            oe-ordl.i-no,
            DECIMAL(oe-boll.qty),
            DECIMAL(oe-ordl.price),
            oe-ordl.pr-uom,
            DECIMAL(oe-ordl.disc),
            DECIMAL(oe-ordl.cas-cnt),    
            OUTPUT dShipmentValue).

             
        CREATE ttOnTimeDeliveries.
          
        ASSIGN
            ttOnTimeDeliveries.cPartNo          = oe-ordl.part-no 
            ttOnTimeDeliveries.cFGItem          = oe-ordl.i-no 
            ttOnTimeDeliveries.iOrdNo           = oe-ord.ord-no    
            ttOnTimeDeliveries.dtOrdDate        = oe-ord.ord-date 
            ttOnTimeDeliveries.dtDueDate        = oe-rel.rel-date
            ttOnTimeDeliveries.dtBolDate        = oe-bolh.bol-date
            ttOnTimeDeliveries.cOnTime          = STRING(oe-bolh.bol-date LE dtCompare,"Y/N")
            ttOnTimeDeliveries.dtPromDate       = oe-ordl.prom-date   
            ttOnTimeDeliveries.cReason          = STRING(oe-rel.spare-char-2)
            ttOnTimeDeliveries.dMsf             = dMsf
            ttOnTimeDeliveries.dWeight          = oe-boll.weight
            ttOnTimeDeliveries.cTrailer         = oe-bolh.trailer
            ttOnTimeDeliveries.cCustGroup       = IF AVAILABLE cust THEN  STRING(cust.spare-char-2) ELSE "" 
            ttOnTimeDeliveries.cReceDate        = cLast
            ttOnTimeDeliveries.iOrdQty          = oe-ordl.qty 
            ttOnTimeDeliveries.cCustName        = cName
            ttOnTimeDeliveries.dUnitPrice       = oe-ordl.price
            ttOnTimeDeliveries.cPriceUom        = oe-ordl.pr-uom
            ttOnTimeDeliveries.iPalletCount     = oe-ordl.cases-unit
            ttOnTimeDeliveries.dtManuDate       = oe-ordl.prom-date
            ttOnTimeDeliveries.dtCompletionDate = oe-ordl.req-date
            ttOnTimeDeliveries.cFGCat           = cFGCategory
            ttOnTimeDeliveries.iBolNo           = oe-bolh.bol-no   
            ttOnTimeDeliveries.cBolCarrier      = oe-bolh.carrier     
            ttOnTimeDeliveries.iBolShipQty      = oe-boll.qty      
            ttOnTimeDeliveries.dShipmentValue   = dShipmentValue    
            ttOnTimeDeliveries.iRelQty          = IF oe-ordl.t-rel-qty GT 0 THEN oe-ordl.t-rel-qty ELSE 0
            ttOnTimeDeliveries.dtRelDueDate     = DATE(ENTRY(1, oe-rel.spare-char-4))  
            ttOnTimeDeliveries.dtRelDate        = oe-rel.rel-date
            ttOnTimeDeliveries.iRelNumber       = IF AVAILABLE oe-relh THEN oe-relh.release# ELSE 0
            .   

      
    END. /* each oe-ord */
END PROCEDURE.


{AOA/dynBL/pBuildCustList.i}
