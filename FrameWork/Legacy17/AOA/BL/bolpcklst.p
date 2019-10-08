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
END. /* each oe-bolh */

{aoa/BL/pBuildCustList.i}
