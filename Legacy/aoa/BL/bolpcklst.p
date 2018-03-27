/*------------------------------------------------------------------------
  File: bolpcklst.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* BOL Packing List.rpa */
{aoa/tempTable/ttBOLPackingList.i}
{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttBOLPackingList.
{aoa/includes/pBOLPackingList.i}

/* local variables */

/* subject business logic */
FOR EACH oe-bolh NO-LOCK
    WHERE oe-bolh.company  EQ ipcCompany
      AND oe-bolh.bol-no   GE iStartBOL
      AND oe-bolh.bol-no   LE iEndBOL
      AND oe-bolh.bol-date GE dtStartBOLDate
      AND oe-bolh.bol-date LE dtEndBOLDate
      AND oe-bolh.cust-no  GE cStartCustNo
      AND oe-bolh.cust-no  LE cEndCustNo,
    EACH oe-boll NO-LOCK
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no
      AND oe-boll.ord-no  GE iStartOrderNo
      AND oe-boll.ord-no  LE iEndOrderNo
    BY oe-bolh.company
    BY oe-bolh.cust-no
    :
    IF lCustList AND
       NOT CAN-FIND(FIRST ttCustList
                    WHERE ttCustList.cust-no EQ oe-bolh.cust-no
                      AND ttCustList.log-fld EQ TRUE) THEN
    NEXT.
    CREATE ttBOLPackingList.
    ASSIGN
        ttBOLPackingList.bolNo       = oe-boll.bol-no
        ttBOLPackingList.shipDate    = oe-boll.bol-date
        ttBOLPackingList.pickTicket  = ""
        ttBOLPackingList.custNo      = IF oe-boll.cust-no NE "" THEN oe-boll.cust-no
                                       ELSE oe-bolh.cust-no
        ttBOLPackingList.itemNo      = oe-boll.i-no
        ttBOLPackingList.relNo       = oe-bolh.release#
        ttBOLPackingList.orderNo     = oe-boll.ord-no
        ttBOLPackingList.jobNo       = oe-boll.job-no + STRING(oe-boll.job-no2,"99")
        ttBOLPackingList.prntr       = cPrinter
        ttBOLPackingList.xxSort      = ""
        .
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ ipcCompany
           AND cust.cust-no EQ ttBOLPackingList.custNo
         NO-ERROR.
    IF AVAILABLE cust THEN
    ttBOLPackingList.custName = cust.name. 
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ oe-boll.company
           AND loadtag.item-type EQ NO
           AND loadtag.tag-no    EQ oe-boll.tag
           AND loadtag.job-no    EQ oe-boll.job-no
           AND loadtag.i-no      EQ oe-boll.i-no
         NO-ERROR.
    IF NOT AVAILABLE loadtag THEN NEXT.
    ASSIGN
        ttBOLPackingList.poNo       = loadtag.po-no
        ttBOLPackingList.tagDate    = loadtag.tag-date
        ttBOLPackingList.qtyCase    = loadtag.qty-case
        ttBOLPackingList.caseBundle = loadtag.case-bundle
        ttBOLPackingList.partial    = loadtag.partial
        .
    FIND FIRST rfidtag NO-LOCK
         WHERE rfidtag.company   EQ loadtag.company
           AND rfidtag.item-type EQ loadtag.item-type
           AND rfidtag.tag-no    EQ loadtag.tag-no
         NO-ERROR.
    IF AVAILABLE rfidtag AND LENGTH(rfidtag.rfidtag) GT 5 THEN
    ttBOLPackingList.ticket = SUBSTR(rfidtag.rfidtag,LENGTH(rfidtag.rfidtag) - 5).
    RUN pMSF (OUTPUT ttBOLPackingList.msfPrice).
END. /* each oe-bolh */

PROCEDURE pMSF:
    DEFINE OUTPUT PARAMETER opdMSFPrice AS DECIMAL NO-UNDO.
     
    DEFINE VARIABLE dPct         AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPriceAmount LIKE oe-ord.t-revenue NO-UNDO.
    DEFINE VARIABLE dOrdQty      LIKE oe-ordl.qty      NO-UNDO.
    DEFINE VARIABLE dSqft        AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dRevenue     LIKE oe-ordl.t-price  NO-UNDO.
    DEFINE VARIABLE idx          AS   INTEGER          NO-UNDO.
    
    DEFINE BUFFER bItemFG FOR itemfg.
    
    FIND FIRST oe-ordl NO-LOCK 
         WHERE oe-ordl.company EQ oe-boll.company
           AND oe-ordl.ord-no  EQ oe-boll.ord-no
           AND oe-ordl.i-no    EQ oe-boll.i-no
         NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany
               AND itemfg.i-no    EQ oe-ordl.i-no
             NO-ERROR.
        dPct = 100.
        DO idx = 1 TO 3:
            IF oe-ordl.s-man[idx] EQ "" THEN NEXT.
            dPct = oe-ordl.s-pct[idx].
            LEAVE.
        END. /* do idx */
        ASSIGN
            dPct         = dPct / 100
            dOrdQty      = oe-ordl.qty * dPct
            dPriceAmount = oe-ordl.t-price * dPct
            .
        IF AVAILABLE itemfg AND itemfg.isaset THEN DO:
           dSqft = 1.
           FOR EACH fg-set FIELDS(part-no part-qty) NO-LOCK
               WHERE fg-set.company EQ itemfg.company
                 AND fg-set.set-no  EQ itemfg.i-no,
               FIRST bItemFG FIELDS(t-sqft) NO-LOCK
               WHERE bItemFG.company EQ itemfg.company
                 AND bItemFG.i-no EQ fg-set.part-no
               :
               dSqft = dSqft + (dOrdQty
                     * (IF fg-set.part-qty GE 0 THEN fg-set.part-qty
                        ELSE (-1 / fg-set.part-qty))
                     * bItemFG.t-sqft / 1000).
           END. /* each fg-set */
        END. /* if avail itemfg */
        ELSE dSqft = IF AVAILABLE itemfg THEN (itemfg.t-sqft * dOrdQty / 1000) ELSE 1.
        opdMSFPrice = dPriceAmount / dSqft.
    END. /*if avail */
END PROCEDURE.

{aoa/BL/pBuildCustList.i}
