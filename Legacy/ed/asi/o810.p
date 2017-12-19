/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\810.P
**       By: Christopher A. Heins, Report Concepts, Inc. (c) 1997
**            All Rights Reserved *
** Descript: Invoice interface, application to EDI database

03.27.2004 CAH:
1.  Eliminated location_number (now ST_Code) and ordering_store_number (by_code)
which were causing untold confusion.

03.20.2004 by CAH on \\ricky\robj8\dev Log#0000:
1.  Corrected terms discount amount, was scaling by 100 wrong direction.

07.14.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Updated to prevent duplicate edivline errors when reprinting invoices.

04.21.98 CAH on \\ricky\robj8\ Log#0000:
1.  Program was assigning edivline.qty-shipped from inv-line.qty.
Hoever, inv-line.qty holds the original order quantity,
not the quantity shipped.  inv-line.inv-qty is the correct field.

**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i}

DEF buffer billto FOR edshipto.
DEF buffer dc FOR edshipto.
DEF buffer bystore FOR edshipto.

DEF var conv_fact AS decimal NO-UNDO FORMAT "-9999999999.9999999999".
DEF var price_fact LIKE conv_fact NO-UNDO.
DEF VAR bt_code AS char NO-UNDO.    /* billto code */
DEF var st_code AS char NO-UNDO.    /* optional distribution center */
DEF var by_code AS char NO-UNDO.    /* ultimate destination */
DEF var vcCustPOLine AS char NO-UNDO.
DEFINE VARIABLE dTaxRate        AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTaxRateFreight AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-shipto-code AS CHARACTER NO-UNDO.
def var v-shipto-name  as char format "x(30)" NO-UNDO.
def var v-shipto-addr  as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city  as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip   as char format "x(10)" NO-UNDO.
def var v-billto-name  as char format "x(30)" NO-UNDO.
def var v-billto-addr  as char format "x(30)" extent 2 NO-UNDO.
def var v-billto-city  as char format "x(15)" NO-UNDO.
def var v-billto-state as char format "x(2)" NO-UNDO.
def var v-billto-zip   as char format "x(10)" NO-UNDO.
FUNCTION fUOM-CF returns decimal (INPUT pUOM AS CHAR):
DEF var UOM-cf AS decimal NO-UNDO DECIMALS 10.

FIND FIRST uom
  WHERE uom.uom = pUOM NO-LOCK NO-ERROR.

IF AVAIL uom AND uom.mult <> 0
  THEN
UOM-cf = UOM.Mult.
ELSE
DO:
  IF (pUOM = "CT" OR pUOM = 'CA' OR pUOM = "CS")
    AND AVAIL itemfg
    AND itemfg.case-count > 0 THEN
  UOM-cf = itemfg.case-count.
  ELSE
  IF CAN-DO("EACH,EA,1,UNIT,CT,CN,CS,CASE", pUOM) OR pUOM = "" THEN
  UOM-cf = 1.
  ELSE
  IF CAN-DO("HP,C,HUN", pUOM) THEN
  UOM-cf = 100.
  ELSE
  IF CAN-DO("DZ,DOZ,DOZEN", pUOM) THEN
  UOM-cf = 12.
  ELSE
  IF CAN-DO("TP,THOU*,M", pUOM) THEN
  UOM-cf = 1000.
  ELSE
  IF pUOM = "CWT" AND AVAIL itemfg
    AND itemfg.weight-100 > 0 THEN
  UOM-cf = (1 / itemfg.weight-100).
  ELSE
  IF pUOM BEGINS "L"  AND AVAIL itemfg
    AND itemfg.weight-100 > 0 THEN
  UOM-cf =
  (1 / (itemfg.weight-100 / 100)).
  ELSE
  DO:
    UOM-cf = 1.
    RUN rc/debugmsg.p ("*** Called with UOM Code of: " + pUOM).
  END.
END.  /* no UOM */

IF top-debug THEN
RUN rc/debugmsg.p ("pUOM: " + pUOM + "--> " + string(uom-cf)).

RETURN UOM-cf.

END FUNCTION.

/* ------------------------------ Mainline Code ------------------------- */

FIND inv-head WHERE RECID(inv-head) = ws_process_rec NO-LOCK NO-ERROR.
IF avail inv-head then do:
    run edi-oe.ip.
end.
else do:
    find ar-inv where recid(ar-inv) = ws_process_rec
        no-lock no-error.
    if avail ar-inv then do:
        run edi-ar.ip.
    end.
    else RETURN "NO INV-HEAD/AR-INV PASSED".
END.    

ASSIGN NO-ERROR.
RETURN RETURN-VALUE.


/* ------------------------  Internal Procedures ------------------------- */

procedure edi-ar.ip:

if top-debug then run rc/debugmsg.p ("...start of ar-inv invoice").
    assign  
        v-shipto-name    = ar-inv.sold-name
        v-shipto-addr[1] = ar-inv.sold-addr[1]
        v-shipto-addr[2] = ar-inv.sold-addr[2]
        v-shipto-city    = ar-inv.sold-city
        v-shipto-state   = ar-inv.sold-state
        v-shipto-zip     = ar-inv.sold-zip
        v-ShipTo-code    = ar-inv.sold-id.
    FIND FIRST shipto NO-LOCK WHERE shipto.company EQ ar-inv.company
        AND shipto.cust-no EQ ar-inv.cust-no
        AND shipto.ship-id EQ ar-inv.sold-id
        NO-ERROR.
    IF AVAILABLE shipto THEN 
        ASSIGN v-shipto-code    = ar-inv.sold-id
            v-shipto-name    = shipto.ship-name
            v-shipto-addr[1] = shipto.ship-addr[1]
            v-shipto-addr[2] = shipto.ship-addr[2]
            v-shipto-city    = shipto.ship-city
            v-shipto-state   = shipto.ship-state
            v-shipto-zip     = shipto.ship-zip
            .
            
run edi-010.ip (
    input ar-inv.company,
    input ar-inv.inv-no,
    input ar-inv.cust-no,
    input v-shipto-code
    ).
    
if return-value > "" then do:
    return return-value.
end.    

FOR EACH ar-invl 
where ar-invl.x-no = ar-inv.x-no:

  IF top-debug THEN
  RUN rc/debugrec.s ("", RECID(ar-invl)) "ar-invl".
  
  run edi-020.ip (input ar-invl.po-no).
  

  FIND oe-bolh OF ar-invl NO-LOCK NO-ERROR.

  /* ar-inv* records cannot be linked to a release ... 

  /* 03.24.2004 CAH to provide a source for ship-date */
  FIND oe-rel OF ar-invl NO-LOCK NO-ERROR.
  IF NOT AVAIL oe-rel THEN
  FIND oe-rel OF ar-inv NO-LOCK NO-ERROR.

  IF top-debug AND AVAIL oe-rel
    THEN
  RUN rc/debugrec.s ("", RECID(oe-rel)) "oe-rel".
  

  FIND oe-ship OF ar-inv NO-LOCK NO-ERROR.

  IF top-debug AND AVAIL oe-ship
    THEN
  RUN rc/debugrec.s ("", RECID(oe-ship)) "oe-ship".
  
  ... */
  

  IF AVAIL oe-bolh
    THEN
  DO:
    IF top-debug THEN
    RUN rc/debugrec.s ("", RECID(oe-bolh)) "oe-bolh".
    FIND FIRST oe-boll
      WHERE oe-boll.company = ar-invl.company
      AND   oe-boll.inv-no  = ar-invl.inv-no
      AND   oe-boll.ord-no  = ar-invl.ord-no
      AND   oe-boll.i-no    = ar-invl.i-no
      NO-LOCK NO-ERROR.

    FIND FIRST carrier
      WHERE carrier.carrier = oe-bolh.carrier NO-LOCK NO-ERROR.

  END.
  else do:  /* if no bolh then find carrier from ar-inv */
    FIND FIRST carrier
      WHERE carrier.carrier = ar-inv.carrier NO-LOCK NO-ERROR.
  end.

    IF top-debug AND AVAIL carrier
      THEN
    RUN rc/debugrec.s ("", RECID(carrier)) "carrier".

    IF AVAIL carrier THEN
    FIND edshipvia
      WHERE edshipvia.carrier = carrier.carrier NO-LOCK NO-ERROR.

    IF top-debug AND AVAIL edshipvia
      THEN
    RUN rc/debugrec.s ("", RECID(edshipvia)) "edshipvia".

  FIND oe-ord OF ar-invl NO-LOCK NO-ERROR.
  IF AVAIL oe-boll AND AVAIL oe-ord
    THEN
  FIND oe-ordl OF oe-ord
    WHERE oe-ordl.line = oe-boll.line NO-LOCK NO-ERROR.
  ELSE
  FIND FIRST oe-ordl
    WHERE oe-ordl.company = ar-invl.company
    AND oe-ordl.ord-no = ar-invl.ord-no
    AND oe-ordl.i-no = ar-invl.i-no NO-LOCK NO-ERROR.

  IF top-debug AND AVAIL oe-ord
    THEN
  RUN rc/debugrec.s ("", RECID(oe-ord)) "oe-ord".

  IF top-debug AND AVAIL oe-ordl
    THEN
  RUN rc/debugrec.s ("", RECID(oe-ordl)) "oe-ordl".


  IF AVAIL oe-ord AND oe-ord.sold-id > "" THEN
  DO:
    run edi-030.ip (
        INPUT ar-inv.cust-no,
        INPUT oe-ord.sold-id).
  END.    /* if avail and > "" */

  /* assume billto/shipto is the same if there is no dc found */
  st_code = (IF AVAIL dc THEN
  dc.by-code ELSE
  by_code).
  
  run edi-040.ip (
    INPUT ar-invl.i-no,
    INPUT string(ar-invl.ord-no),
    ?,  /* rel-no */
    input ar-inv.terms
    ).
    
  run edi-050.ip (
    ar-invl.line,
    ar-invl.i-no
    ).    
    
    
  IF top-debug THEN
  RUN rc/debugrec.s ("Bottom of Line Loop", RECID(edivline)) "edivline".
END.    /* each ar-invl of ar-inv */

/* no ar-invmisc ... 

FOR EACH inv-misc OF ar-inv NO-LOCK
    WHERE inv-misc.deleted = FALSE:
    
    if top-debug then run rc/debugrec.s ("top of inv-misc loop", recid(inv-misc)) "inv-misc".    
    
  run gen-addon.ip (
    ws_eddoc_rec, 
    inv-misc.ar-invl,      
    inv-misc.charge,
    inv-misc.Dscr,
    inv-misc.amt,
    inv-misc.bill,
    "ESTIMATE# " + inv-misc.est-no
        ).
    
END.    /* each inv-misc of ar-inv */

... */

if ar-inv.freight <> 0 then do:
  run gen-addon.ip (
    ws_eddoc_rec, 
    0,          /* line # */
    "FRT",
    "FREIGHT",
    ar-inv.freight,
    0,          /* rate */    
    "Y",
    "Invoice Level Freight Charge"
        ).
end.

if ar-inv.tax-amt <> 0 then do:
  find stax-group
  where stax-group.company = ar-inv.company
  and stax-group.tax-group = ar-inv.tax-code no-lock no-error.
  
  RUN ar/cctaxrt.p (INPUT ar-inv.company, ar-inv.tax-code /* oe-ord.tax-gr */,
        OUTPUT dTaxRate, OUTPUT dTaxRateFreight).  
  run gen-addon.ip (
    ws_eddoc_rec, 
    0,          /* line # */
    "TAX",
    (if avail stax-group then stax-group.tax-dscr else "TAXES"),
    ar-inv.tax-amt,
    dTaxRate,          /* rate */ 
    "Y",
    "Invoice Level Taxes"
        ).
end.

IF top-debug THEN
RUN rc/debugrec.s ("Bottom of Program", RECID(edivtran)) "edivtran".

if top-debug then run rc/debugmsg.p ("...end of ar-inv invoice").

RETURN "".  /* 03.22.2004 CAH, CALLER LOOKS FOR THIS */

end procedure.  /* edi-ar.ip */

procedure gen-addon.ip:
def input param pDoc as recid no-undo.
def input param pLine as int no-undo.
def input param pCode as char no-undo.
def input param pDesc as char no-undo.
def input param pAmt as decimal no-undo.
def input param pRate as decimal no-undo.
def input param pBill as char no-undo.
def input param pRef as char no-undo.

def buffer bAddon for edivaddon.

find eddoc where recid(eddoc) = pDoc no-lock no-error.
if not avail eddoc then return error "no eddoc".

    FIND edivtran
      WHERE edivtran.partner      = eddoc.partner
      AND   edivtran.seq          = eddoc.seq
      NO-LOCK NO-ERROR.
    
    if pLine > 0 then 
    FIND FIRST edivline of edivtran
      WHERE edivline.line         = pLine
      NO-LOCK NO-ERROR.

  FIND FIRST edivaddon
    WHERE edivaddon.partner     = eddoc.partner
    AND edivaddon.seq  = eddoc.seq
    AND edivaddon.line        = pline
    EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAIL edivaddon THEN
  DO:
    FIND LAST bAddon of edivtran
      NO-LOCK NO-ERROR.
    ws_int =
    (IF AVAIL edivaddon THEN
    edivaddon.addon-line ELSE
    0) + 1.

    /* ### if both of the above fail then this addon will be an orphan */
    CREATE edivaddon.
    ASSIGN
      edivaddon.partner       = edmast.partner
      edivaddon.company       = edivtran.company
      edivaddon.invoice-no    = edivtran.invoice-no
      edivaddon.line          = pline
      edivaddon.Addon-line    = ws_int
      edivaddon.seq           = eddoc.seq
      .
  END.

  ASSIGN
    edivaddon.Description[1]  = pDesc
    edivaddon.Description[2]  = ""
    edivaddon.allow-charge    = IF pAmt >= 0 THEN FALSE ELSE TRUE
    edivaddon.Amount          = PAmt
    edivaddon.hand-meth       =
    IF pBill = "Y" THEN "02" /* off invoice */ ELSE
    IF pBill = "N" THEN "05" /* paid by vendor */
    ELSE "02"
    edivaddon.Agency-qual     = ""
    edivaddon.agency-code     = pCode /* ### requires xlate */
    edivaddon.Ref-Num         = pRef
    edivaddon.special-svc-code = ""
    EDIVAddon.Rate = pRate
    .
  /*
  edivaddon.Uom-code       = {2}.Uom-code
  edivaddon.Qty            = {2}.Qty
  edivaddon.Rate           = {2}.Rate
  edivaddon.Percent        = {2}.Percent
  edivaddon.Basis-qual     = {2}.Basis-qual
  edivaddon.Note[1]        = {2}.Note[1]
  edivaddon.Note[2]        = {2}.Note[2]
  edivaddon.Note[3]        = {2}.Note[3]
  edivaddon.Note[4]        = {2}.Note[4]
  edivaddon.Note[5]        = {2}.Note[5]
  edivaddon.Note[6]        = {2}.Note[6]
  edivaddon.Note[7]        = {2}.Note[7]
  edivaddon.Note[8]        = {2}.Note[8]
  edivaddon.Note[9]        = {2}.Note[9]
  edivaddon.option-code    = {2}.option-code
  */
  IF top-debug THEN
  RUN rc/debugrec.s ("bottom of addon block", RECID(edivaddon)) "edivaddon".
  
  release edivaddon.

end procedure.


procedure edi-010.ip:
def input param pCo as char no-undo.
def input param pInvoice as char no-undo.
def input param pCust as char no-undo.
def input param pStore as char no-undo.

FIND FIRST asi.company 
WHERE asi.company.company = pCo NO-LOCK NO-ERROR.

FIND FIRST edmast
  WHERE edmast.cust = pCust NO-LOCK NO-ERROR.
IF NOT AVAIL edmast THEN
RETURN.
ASSIGN
  invoice_number = STRING(pInvoice)
  ws_company = pCo
  ws_partner = edmast.partner.
.

FOR EACH eddoc
    WHERE eddoc.partner = ws_partner
    AND eddoc.fgid = "IV"
    AND eddoc.docid = invoice_number EXCLUSIVE:
  RUN ed/fm810del.p (INPUT RECID(eddoc)). /* 9807 CAH */
  DELETE eddoc.
END.

  FIND FIRST billto
    WHERE billto.partner = ws_partner
    AND billto.ref-type = "BT"
    AND billto.cust = pCust
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL billto THEN
  DO:
    CREATE billto.
    ASSIGN
      billto.partner = ws_partner
      billto.ref-type = "BT"
      billto.by-code = ""
      billto.cust = pCust.
  END.
  
  ASSIGN bt_code = billto.by-code.

  IF top-debug THEN
  RUN rc/debugmsg.p ("Billto code set to " + bt_code).

  IF billto.name = '' THEN
  DO:
    if avail inv-head then ASSIGN
      billto.name       = inv-head.cust-name
      billto.addr1      = inv-head.addr[1]
      billto.addr2      = inv-head.addr[2]
      billto.city       = inv-head.city
      billto.state      = inv-head.state
      billto.zip        = inv-head.zip
      billto.attention  = inv-head.contact
      .
    else if avail ar-inv then ASSIGN
      billto.name       = ar-inv.cust-name
      billto.addr1      = ar-inv.addr[1]
      billto.addr2      = ar-inv.addr[2]
      billto.city       = ar-inv.city
      billto.state      = ar-inv.state
      billto.zip        = ar-inv.zip
      billto.attention  = ar-inv.contact
      .
    IF billto.country EQ "" THEN 
      billto.country = "US".
  END.

  IF top-debug THEN
  RUN rc/debugrec.s ("Billto Record", RECID(billto)) "edshipto".

  pStore = v-shipto-code.
  /* find the store from the shipto record */
  IF pStore > "" THEN
  DO:

    FIND FIRST bystore
      WHERE bystore.partner = ws_partner
      AND bystore.ref-type = "BY"
      AND bystore.cust = pCust
      AND bystore.ship-to = pStore
      EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL bystore THEN 
    FIND FIRST bystore
      WHERE bystore.partner = ws_partner
      AND bystore.ref-type = "BY"
      AND bystore.BY-CODE = ""
      EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL bystore THEN
    DO:
      CREATE bystore.
      ASSIGN
        bystore.partner = ws_partner
        bystore.ref-type = "BY"
        bystore.by-code = ""    /* partner must assign this */
        bystore.cust = pCust
        bystore.ship-to = pStore
        .
        IF ws_partner EQ "AMAZ" THEN
          ASSIGN bystore.siteID = "2855507686"
                 .
           
    END.

    IF bystore.name = '' THEN
    DO:
      if avail inv-head then ASSIGN
        bystore.name       = inv-head.sold-name
        bystore.addr1      = inv-head.sold-addr[1]
        bystore.addr2      = inv-head.sold-addr[2]
        bystore.city       = inv-head.sold-city
        bystore.state      = inv-head.sold-state
        bystore.zip        = inv-head.sold-zip
        by_code            = inv-head.sold-no
        .
      else if avail ar-inv then ASSIGN
      bystore.name       = ar-inv.sold-name
      bystore.addr1      = ar-inv.sold-addr[1]
      bystore.addr2      = ar-inv.sold-addr[2]
      bystore.city       = ar-inv.sold-city
      bystore.state      = ar-inv.sold-state
      bystore.zip        = ar-inv.sold-zip        
        by_code           = ar-inv.sold-id
       .
      IF v-shipto-name NE "" THEN 
        ASSIGN 
          bystore.name       = v-shipto-name                    
          bystore.addr1      = v-shipto-addr[1] 
          bystore.addr2      = v-shipto-addr[2] 
          bystore.city       = v-shipto-city    
          bystore.state      = v-shipto-state   
          bystore.zip        = v-shipto-zip     
          .      
      IF bystore.country EQ "" THEN 
        bystore.country = "US".
    END.
    
    ASSIGN by_code = bystore.ship-to.
    IF top-debug THEN
    RUN rc/debugmsg.p ("ByStore code set to " + by_code).

    IF top-debug THEN
    RUN rc/debugrec.s ("ByStore Record", RECID(bystore)) "edshipto".

  END.    /* pStore > "" */
  
return "".  

end procedure.

procedure edi-020.ip:
def input param pPO as char no-undo.

def var viPOLine as int no-undo.
/* wfk - Don't know why this was here                           */
/*  IF NUM-ENTRIES(pPo, "-") = 2 THEN                           */
/*  DO:                                                         */
/*    purchase_order_number = ENTRY(1, pPo, "-").               */
/*    vcCustPoLine  = ENTRY(2, pPo, "-").                       */
/*    viPOLine = integer(vcCustPoLIne) NO-ERROR.                */
/*    if error-status:error or vcCustPoLine = '' or vipoline = ?*/
/*    then assign purchase_order_number = pPo                   */
/*        vcCustPoLine = "".                                    */
/*  END.                                                        */
/*  ELSE                                                        */
/*  DO:                                                         */
    purchase_order_number = pPo.
    vcCustPoLine = "".
/*  END.*/

  IF top-debug THEN
  RUN rc/debugmsg.p ("pPo: " + pPo
    + " po# " + purchase_order_number
    + " line " + vcCustPoLine).
    
return "".    

end procedure.

procedure edi-030.ip:
def input param pCust as char no-undo.
def input param pDC as char no-undo.    

    FIND FIRST dc
      WHERE dc.partner = ws_partner
      AND dc.ref-type = "ST"
      AND dc.cust = pCust
      AND dc.ship-to = pDC
      EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL dc THEN
    DO:
      /* alternate lookup? */
      IF AVAIL bystore
        AND bystore.st-code > ""
        THEN
      FIND FIRST dc
        WHERE dc.partner = ws_partner
        AND dc.ref-type = 'ST'
        AND dc.by-code = bystore.st-code EXCLUSIVE-LOCK NO-ERROR.
    END.

    IF NOT AVAIL dc THEN
    DO:
      /* 03.26.2004 CAH: optional, not created if not found ...
      CREATE dc.
      ASSIGN
      dc.partner = ws_partner
      dc.ref-type = "ST"
      dc.by-code = ""
      dc.cust = pCust
      dc.ship-to = pDC.
      ... */
    END.

    /* can't default this like this ...
    IF dc.name = '' THEN
    DO:
    if avail inv-head then ASSIGN
    dc.name       = inv-head.sold-name
    dc.addr1      = inv-head.sold-addr[1]
    dc.addr2      = inv-head.sold-addr[2]
    dc.city       = inv-head.sold-city
    dc.state      = inv-head.sold-state
    dc.zip        = inv-head.sold-zip
    .
    END.
    */

    IF top-debug THEN
    RUN rc/debugrec.s ("Shipto/DC Record", RECID(dc)) "edshipto".
    
return "".    

end procedure.

procedure edi-040.ip:
def input param pItem as char no-undo.
def input param pOrder as char no-undo.
def input param pRel  as char no-undo.
def input param pTerms as char no-undo.

  FIND FIRST edpotran
    WHERE edpotran.partner      = edmast.partner
    AND edpotran.cust-po      = purchase_order_number /* 03.26.2004 CAH: inv-line.po-no */
    NO-LOCK NO-ERROR.

  IF AVAIL edpotran
    THEN
  DO:
    IF top-debug THEN
    RUN rc/debugmsg.p ("", RECID(edpotran)) "edpotran".

    /* 03.26.2004 CAH: Better method for looking up PO Line when po# is PO-Line format
    which we split into components */
    IF vcCustPoLine > ""
      THEN
    FIND FIRST edpoline OF edpotran
      WHERE edpoline.cust-po-line = vcCustPoLine
      AND edpoline.item-no = pItem NO-LOCK NO-ERROR.

    IF NOT AVAIL edpoline
      THEN
    FIND FIRST edpoline OF edpotran
      WHERE edpoline.item-no = pItem
      AND edpoline.by-code = by_code NO-LOCK NO-ERROR.

    IF NOT AVAIL edpoline
      THEN
    FIND FIRST edpoline OF edpotran
      WHERE edpoline.item-no = pItem
      AND edpoline.st-code = by_code NO-LOCK NO-ERROR.

    IF st_code = "" AND edpotran.st-code > ""
      THEN
    st_code = edpotran.st-code.

    IF top-debug AND AVAIL edpoline
      THEN
    RUN rc/debugrec.s ("", RECID(edpoline)) "edpoline".

  END. /* avail edpotran */

  FIND FIRST edivtran
    WHERE edivtran.company = ws_company
    AND   edivtran.invoice-no = invoice_number
    AND   edivtran.cust-po    = purchase_order_number /* 03.26.2004 CAH: inv-line.po-no */
    EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAIL edivtran THEN
  DO:
    FIND FIRST edcode
      WHERE edcode.partner = edmast.partner
      AND edcode.setid = "810"
      AND edcode.direction = "o" NO-LOCK NO-ERROR.
    IF NOT AVAIL edcode THEN
    DO:
      RETURN.
    END.

    FIND edmast OF edcode EXCLUSIVE-LOCK NO-ERROR.

    RUN ed/gendoc.p (RECID(edcode), invoice_number, OUTPUT ws_eddoc_rec).
    FIND  eddoc WHERE RECID(eddoc) = ws_eddoc_rec EXCLUSIVE.
    
    ASSIGN
      eddoc.userref     = "R-NO: " + string(pRel)
      eddoc.version     = STRING(edcode.version)
      /* eddoc.docseq     integer(by_code) 9806 can fail */
      eddoc.openitem    = TRUE
      eddoc.unique-order-no = integer(pOrder)
      .
    /* returns zero if input param is alphanumeric */
    RUN rc/str2int.p (by_code, OUTPUT eddoc.docseq).
    CREATE edivtran.
    ASSIGN
      edivtran.company            = ws_company
      edivtran.partner            = eddoc.partner
      edivtran.seq                = eddoc.seq
      edivtran.Cust               = billto.cust /* inv-head.BILL-TO */
      edivtran.Invoice-no         = invoice_number
      edivtran.Cust-po            = purchase_order_number /* 03.26.2004 CAH: inv-line.po-no */
      edivtran.by-code            = by_code
      edivtran.bt-code            = bt_code
      edivtran.lines              = 0
      edivtran.last-line          = 0
      edivtran.vendor             = edmast.we-vend-no
      edivtran.carrier            =
      (IF AVAIL oe-bolh AND oe-bolh.carrier > "" THEN oe-bolh.carrier
      ELSE if avail inv-head then inv-head.carrier
      else if avail ar-inv then ar-inv.carrier
      else '')
      edivtran.carrier-code       =
      (IF AVAIL edshipvia THEN edshipvia.carrier-code
      ELSE '')
      edivtran.st-code            = st_code
      edivtran.vn-code            =
      (IF AVAIL edpotran AND edpotran.vn-code > ""
      THEN edpotran.vn-code
      ELSE edmast.we-vend-no)
      edivtran.cust-dept          =
      (IF AVAIL edpotran THEN edpotran.cust-dept ELSE "")
      edivtran.cust-div          =
      (IF AVAIL edpotran THEN edpotran.cust-div ELSE "")
      edivtran.promo-code         =
      (IF AVAIL edpotran THEN edpotran.promo-code ELSE "")
      edivtran.sf-code            = edmast.sf-code
      .
    FIND FIRST company NO-LOCK WHERE company.company EQ EDIVTran.Company NO-ERROR.
   
      EDIVTran.Curr-seller = IF AVAILABLE company THEN  company.curr-code ELSE "".
  
    if avail inv-head then ASSIGN
      edivtran.Invoice-date       = inv-head.Inv-date
      edivtran.BOL-No             = STRING(inv-head.BOL-No)
      edivtran.terms              = inv-head.terms
      edivtran.terms-desc[1]      = inv-head.terms-d
      edivtran.terms-desc[2]      = ''
      edivtran.routing[1]         = inv-head.ship-i[1]
      edivtran.routing[2]         = inv-head.ship-i[2]
      edivtran.routing[3]         = inv-head.ship-i[3]
      edivtran.routing[4]         = inv-head.ship-i[4]
      edivtran.tot-Gross      = inv-head.T-Inv-rev
      edivtran.tot-frt        = inv-head.t-inv-freight
      edivtran.tot-wght       = inv-head.t-inv-weight
      edivtran.ship-stat      = inv-head.stat
      edivtran.FOB-Code           =
      (IF inv-head.frt-pay = "P" THEN "PP" ELSE
      IF inv-head.frt-pay = "C" THEN "CC"
      ELSE "")
      edivtran.FOB-Qual           =
      (IF inv-head.fob-code = "DEST" THEN "DE" ELSE "OR")   /* origin */
      edivtran.contact-name       = inv-head.contact
      .
    else if avail ar-inv then assign
      edivtran.Invoice-date       = ar-inv.Inv-date
      edivtran.BOL-No             = ?   
      edivtran.terms              = ar-inv.terms
      edivtran.terms-desc[1]      = ar-inv.terms-d
      edivtran.terms-desc[2]      = ''
      edivtran.routing[1]         = ar-inv.ship-i[1]
      edivtran.routing[2]         = ar-inv.ship-i[2]
      edivtran.routing[3]         = ar-inv.ship-i[3]
      edivtran.routing[4]         = ar-inv.ship-i[4]
      edivtran.tot-Gross      = ar-inv.gross
      edivtran.tot-frt        = ar-inv.freight
      edivtran.tot-wght       = ar-inv.t-weight
      edivtran.ship-stat      = ar-inv.stat
      edivtran.FOB-Code           =
      (IF ar-inv.frt-pay = "P" THEN "PP" ELSE
      IF ar-inv.frt-pay = "C" THEN "CC"
      ELSE "")
      edivtran.FOB-Qual           =
      (IF ar-inv.fob-code = "DEST" THEN "DE" ELSE "OR")   /* origin */
      edivtran.contact-name       = ar-inv.contact
      .

    FIND FIRST asi.terms WHERE asi.terms.company = ws_company
      AND asi.terms.t-code = pTerms NO-LOCK NO-ERROR.

    IF NOT AVAIL asi.terms
      THEN
    FIND FIRST asi.terms WHERE asi.terms.t-code = pTerms NO-LOCK NO-ERROR.

    IF AVAIL terms THEN
    DO:
      ASSIGN
        edivtran.terms-type         =
        (IF terms.type = "P" THEN "09"
        ELSE IF terms.disc-rate = 0 THEN "05"
        ELSE IF edivtran.partner BEGINS "3m" THEN "08"
        ELSE "01")
        /*  01 = Basic
        02 = End of Month
        03 = Fixed Date
        04 = Installment
        05 = Discount not applicable
        08 = Basic discount (Sonoco)
        09 = Proximo
        */
        edivtran.terms-basis       = "3"
        /*  1 = ship date
        2 = delivery date
        3 = invoice date
        */
        edivtran.terms-disc-pct     = terms.disc-rate
        edivtran.terms-disc-days    = terms.disc-days
        edivtran.terms-net-days     = terms.net-days
        edivtran.terms-disc-amt     = ROUND(edivtran.Tot-Gross * (terms.disc-rate / 100), 2)
        edivtran.terms-net-date     = edivtran.invoice-date + edivtran.terms-net-days
        edivtran.terms-disc-date    = edivtran.invoice-date + edivtran.terms-disc-days
        /*
        edivtran.terms-day-of-month = inv-head.terms-day-of-month
        */
        .
    END.    /* terms */

    ASSIGN
      edivtran.Ship-Date      =
      (IF AVAIL oe-bolh AND oe-bolh.ship-date <> ? THEN oe-bolh.Ship-Date ELSE
      IF AVAIL oe-rel AND oe-rel.ship-date <> ? THEN oe-rel.ship-date
      ELSE edivtran.invoice-date)
      edivtran.release-no         =
      (IF AVAIL oe-bolh AND oe-bolh.rel-no > 0 THEN STRING(oe-bolh.rel-no) ELSE
      IF AVAIL oe-ship AND oe-ship.rel-no > 0 THEN string(oe-ship.rel-no) ELSE
      IF AVAIL edpotran AND edpotran.release-no > "" THEN edpotran.release-no
      ELSE purchase_order_Number /* 03.26.2004 CAH: inv-line.po-no */ )
      edivtran.Trailer-Number     =
      (IF AVAIL oe-bolh THEN oe-bolh.trailer
      ELSE "")
      .

    ASSIGN
      edivtran.cust-po-date       =
      (IF AVAIL oe-ord     THEN oe-ord.ord-date    ELSE
      IF AVAIL edpotran   THEN edpotran.order-date ELSE
      IF AVAIL inv-line then inv-line.ord-date else
      if avail ar-inv then ar-inv.ord-date
      else edivtran.cust-po-date
      )
      edivtran.del-date-qual      = ""
      edivtran.del-date           =
      (IF avail inv-line and inv-line.req-date <> ? THEN inv-line.req-date ELSE
      IF AVAIL oe-ordl AND oe-ordl.req-date <> ? THEN oe-ordl.req-date
      else if avail ar-inv and ar-inv.due-date <> ? then ar-inv.due-date
      ELSE edivtran.del-date)
      edivtran.FOB-Text           =
      (IF EDIVTRAN.FOB-Qual = "OR" AND AVAIL asi.company THEN company.city + ", " + company.state ELSE
      IF edivtran.fob-qual = "DE" AND AVAIL dc THEN dc.city + ", " + dc.state
      ELSE "")  /* location city/state? */
      .

    FIND FIRST edshipto
      WHERE edshipto.partner = edmast.partner
      AND edshipto.ref-type = "RE"
      AND edshipto.cust = edmast.cust NO-LOCK NO-ERROR.
    ASSIGN edivtran.re-code =
      IF AVAIL edshipto
      THEN edshipto.by-code
      ELSE edmast.re-code.

    /*
    assign
    edivtran.carton-uom-code    = inv-head.carton-uom-code
    edivtran.tot-volume     = inv-head.tot-volume
    edivtran.volume-uom         = inv-head.volume-uom
    edivtran.wght-uom           = inv-head.wght-uom
    edivtran.EDShipto           = inv-head.EDShipto
    edivtran.ship-date-code     = edco.ship-date-code
    edivtran.special-svc-code   = inv-head.special-svc-code
    edivtran.contract           = inv-head.contract
    edivtran.sales-region       = inv-head.sales-region
    edivtran.st-code            = inv-head.st-code
    edivtran.by-code            = inv-head.by-code
    edivtran.sn-code            = inv-head.sn-code
    edivtran.Pro-Number         = inv-head.Pro-Number
    edivtran.contact-funct-code = inv-head.contact-funct-code
    edivtran.contact-phone-qual = inv-head.contact-phone-qual
    edivtran.contact-phone      = inv-head.contact-phone
    edivtran.curr-buyer         = inv-head.curr-buyer
    edivtran.curr-rate-buyer    = inv-head.curr-rate-buyer
    edivtran.curr-seller        = inv-head.curr-seller
    edivtran.curr-rate-seller   = inv-head.curr-rate-seller
    edivtran.Cust-div           = inv-head.Cust-div
    edivtran.misc-date1-code    = inv-head.misc-date1-code
    edivtran.misc-date1         = inv-head.misc-date1
    edivtran.ref2-code          = inv-head.ref2-code
    edivtran.ref2               = inv-head.ref2
    edivtran.ref3-code          = inv-head.ref3-code
    edivtran.ref3               = inv-head.ref3
    .
    */

  END. /* NOT AVAIL edivtran */
  else do:
    find eddoc of edivtran exclusive-lock no-error.
    if avail eddoc then ws_eddoc_rec = recid(eddoc).
  end.

  ASSIGN
    edivtran.tot-disc       = edivtran.tot-disc
        + (if avail inv-line then inv-line.disc 
           else if avail ar-invl then ar-invl.disc
           else 0)
    edivtran.tot-net        = edivtran.tot-gross - edivtran.tot-disc
    edivtran.tot-cartons    = edivtran.tot-cartons
        + (IF AVAIL oe-boll THEN oe-boll.cases ELSE 1)
    .
    
return "".    

end procedure.

procedure edi-050.ip:
def input param pLine as int no-undo.
def input param pItem as char no-undo.   /* i-No */

  FIND edivline
    WHERE edivline.partner      = edivtran.partner
    AND edivline.seq          = edivtran.seq
    AND edivline.line         = pLine
    EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAIL edivline THEN
  DO:
    CREATE edivline.
    ASSIGN
      edivline.partner      = edivtran.partner
      edivline.seq          = edivtran.seq
      edivline.line         = pLine
      edivtran.lines            = edivtran.lines + 1
      edivtran.last-line        = max(edivtran.last-line, edivline.line)
      .
  END.
  ELSE
  ASSIGN edivtran.tot-qty          = edivtran.tot-qty - edivline.qty-shipped
    .
    
  ASSIGN    /* these are for both ar-line and inv-line */  
    edivline.company          = edivtran.company
    edivline.Invoice-no       = edivtran.Invoice-no
    edivline.by-code          = by_code
    .
    
    
    /* regardless of ar or oe */
    
  /* 03.26.2004 CAH: Added as source of UPC Code */
  FIND itemfg 
  where itemfg.company = ws_company
  and itemfg.i-no = pItem NO-LOCK NO-ERROR.
  
  ASSIGN
    edivline.Item-no          = pItem
    edivline.UPC              =
    (IF AVAIL itemfg AND itemfg.upc-no > "" THEN itemfg.upc-no
    ELSE IF AVAIL edpoline AND edpoline.upc > "" THEN edpoline.upc ELSE "")
    edivline.sf-code        =
    IF AVAIL oe-bolh THEN oe-bolh.loc ELSE ""
    edivline.cust-po-line     =
    (IF AVAIL edpoline AND edpoline.cust-po-line > "" THEN edpoline.cust-po-line ELSE
    IF vcCustpoLine > "" THEN vcCustPoLine ELSE
    IF AVAIL edpoline THEN STRING(edpoline.line) ELSE
    IF AVAIL oe-ordl THEN string(oe-ordl.line)
    ELSE string(pLine))
    .
    
    if avail inv-line then do:      /* specific to inv-line */

  /* 03.20.2004 CAH added scaling factors for amount extension */
  conv_fact = fUOM-CF (inv-line.pr-qty-uom).    /* blank in asi.inv-line */
  price_fact = fUOM-CF (inv-line.pr-uom).       /* M in asi.inv-line */

  ASSIGN
    edivline.Description[1]   = inv-line.i-name /* part-dscr1 */
    edivline.Description[2]   = inv-line.part-dscr1 /* 2 */
    edivline.unit-price       = inv-line.price
    edivline.qty-shipped      = inv-line.inv-qty
    /* 9804 CAH> was inv-line.qty, which appears to be original ordered */
    edivline.Qty-ord-orig     =
    IF AVAIL oe-ordl THEN oe-ordl.qty ELSE inv-line.qty
    edivline.qty-var          = edivline.qty-ord-orig - edivline.qty-shipped
    /* 9804 CAH> used to be: inv-line.qty - edivline.qty-ord-orig */
    edivline.cust-item-no     = inv-line.part-no
    edivline.pack-size        = inv-line.cas-cnt
    edivline.taxable          = inv-line.tax
    edivline.item-gross       = inv-line.t-price + inv-line.disc
    edivline.item-disc-amount = inv-line.disc
    edivline.item-net         = inv-line.t-price
    edivline.item-wght-each   = inv-line.t-weight / inv-line.ship-qty
    /* 9804 CAH was .qty */
    edivline.item-ctn-wght    = edivline.item-wght-each * inv-line.cas-cnt
    /* 9804 CAH was inv-line.t-weight / inv-line.qty * inv-line.cas-cnt */
    edivline.ship-stat        = inv-line.stat
    .

        IF inv-line.pr-uom = "CS"
            AND inv-line.cas-cnt > 0 THEN
        DO:  /* scale qty by case count */
            edivline.qty-shipped = inv-line.inv-qty / inv-line.cas-cnt.
            edivline.uom-code = "CT".   /* carton is correct code for EDI */
        END.
        ELSE
        DO:
            /* quantity unit */
            edivline.uom-code =
                (IF inv-line.pr-qty-uom > "" THEN
                inv-line.pr-qty-uom
                ELSE "EA"
                ).

            IF edivline.uom-code = "CS"
                THEN
                edivline.uom-code = "CT".    /* 9705 CAH */

        END.
        
        CASE inv-line.pr-uom :
            WHEN 'CS' THEN 
                DO:
                    edivline.uom-code = "CT". 
                    IF inv-line.cas-cnt > 0 THEN
                        edivline.qty-shipped = inv-line.inv-qty / inv-line.cas-cnt.
                END.
            WHEN "M" THEN 
                DO:
                    ASSIGN 
                        edivline.uom-code      = "EA"
                        EDIVLine.Selling-price = EDIVLine.Selling-price / 1000
                        EDIVLine.unit-price = EDIVLine.unit-price / 1000
                        .
                END.
            OTHERWISE 
            DO:
            END. 
        END CASE.        
  IF edivline.price-basis = "M" AND ws_partner BEGINS "3m"
    THEN
  edivline.price-basis = "TP".
  
  END.  /*********************** avail inv-line ***************************/
  else if avail ar-invl then do:
  /* 03.20.2004 CAH added scaling factors for amount extension */
  conv_fact = fUOM-CF (ar-invl.pr-qty-uom).    /* blank in asi.ar-invl */
  price_fact = fUOM-CF (ar-invl.pr-uom).       /* M in asi.ar-invl */

  ASSIGN
    edivline.Description[1]   = ar-invl.i-name /* part-dscr1 */
    edivline.Description[2]   = ar-invl.part-dscr1 /* 2 */
    edivline.unit-price       = ar-invl.unit-pr
    edivline.qty-shipped      = ar-invl.inv-qty
    /* 9804 CAH> was ar-invl.qty, which appears to be original ordered */
    edivline.Qty-ord-orig     =
    IF AVAIL oe-ordl THEN oe-ordl.qty ELSE ar-invl.qty
    edivline.qty-var          = edivline.qty-ord-orig - edivline.qty-shipped
    /* 9804 CAH> used to be: ar-invl.qty - edivline.qty-ord-orig */
    edivline.cust-item-no     = ar-invl.part-no
    edivline.pack-size        = ar-invl.cas-cnt
    edivline.taxable          = ar-invl.tax
    edivline.item-gross       = ar-invl.amt + ar-invl.disc
    edivline.item-disc-amount = ar-invl.disc
    edivline.item-net         = ar-invl.amt
    edivline.item-wght-each   = ar-invl.t-weight / ar-invl.ship-qty
    /* 9804 CAH was .qty */
    edivline.item-ctn-wght    = edivline.item-wght-each * ar-invl.cas-cnt
    /* 9804 CAH was ar-invl.t-weight / ar-invl.qty * ar-invl.cas-cnt */
    /* edivline.ship-stat        = ar-invl.stat */
    .

  IF ar-invl.pr-uom = "CS"
    AND ar-invl.cas-cnt > 0 THEN
  DO:  /* scale qty by case count */
    edivline.qty-shipped = ar-invl.inv-qty / ar-invl.cas-cnt.
    edivline.uom-code = "CT".   /* carton is correct code for EDI */
  END.
  ELSE
  DO:
    /* quantity unit */
    edivline.uom-code =
    (IF ar-invl.pr-qty-uom > "" THEN
    ar-invl.pr-qty-uom
    ELSE "EA"
    ).

    IF edivline.uom-code = "CS"
      THEN
    edivline.uom-code = "CT".    /* 9705 CAH */

  END.

    CASE ar-invl.pr-uom :
        WHEN 'CS' THEN 
            DO:
                edivline.uom-code = "CT". 
                IF ar-invl.cas-cnt > 0 THEN
                    edivline.qty-shipped = ar-invl.inv-qty / ar-invl.cas-cnt.
            END.
        WHEN "M" THEN 
            DO:
                ASSIGN 
                    edivline.uom-code        = "EA"
                    EDIVLine.Selling-price = EDIVLine.Selling-price / 1000
                    EDIVLine.unit-price = EDIVLine.unit-price / 1000                     
                    .
            END.
        OTHERWISE 
        DO:
        END. 
    END CASE.
    
  end.

  /*
  ASSIGN
  edivline.edi-prod-id      = edivtran.edi-prod-id
  edivline.ememo[1]         = edivtran.ememo[1]
  edivline.ememo[2]         = edivtran.ememo[2]
  edivline.product-type     = edivtran.product-type
  edivline.special-svc-code = edivtran.special-svc-code
  edivline.bo-flag          = edivtran.bo-flag
  edivline.color-desc       = edivtran.color-desc
  edivline.size-desc        = edivtran.size-desc
  edivline.size-qual[1]     = edivtran.size-qual[1]
  edivline.size-qual[2]     = edivtran.size-qual[2]
  edivline.size-qual[3]     = edivtran.size-qual[3]
  edivline.dimension[1]     = edivtran.dimension[1]
  edivline.dimension[2]     = edivtran.dimension[2]
  edivline.dimension[3]     = edivtran.dimension[3]
  edivline.item-each-cube   = edivtran.item-each-cube
  edivline.item-ctn-cube    = edivtran.item-ctn-cube
  edivline.config-code      = edivtran.config-code
  .
  */
  
  ASSIGN    /* common to ar and oe */
    edivtran.tot-qty          = edivtran.tot-qty + edivline.qty-shipped
    edivline.selling-price    = (edivline.qty-shipped / conv_fact)
    * (edivline.unit-price /  price_fact)
    .
    
return "".    

end.


procedure edi-oe.ip:    

if top-debug then run rc/debugmsg.p ("...start of inv-head invoice").
    assign  
        v-shipto-name    = inv-head.sold-name
        v-shipto-addr[1] = inv-head.sold-addr[1]
        v-shipto-addr[2] = inv-head.sold-addr[2]
        v-shipto-city    = inv-head.sold-city
        v-shipto-state   = inv-head.sold-state
        v-shipto-zip     = inv-head.sold-zip
        v-ShipTo-code       = inv-head.sold-no.
FIND FIRST shipto NO-LOCK WHERE shipto.company EQ inv-head.company
  AND shipto.cust-no EQ inv-head.cust-no
  AND shipto.ship-id EQ inv-head.sold-no
  NO-ERROR.
IF AVAILABLE shipto THEN 
  ASSIGN v-shipto-code = inv-head.sold-no
         v-shipto-name = shipto.ship-name
         v-shipto-addr[1] = shipto.ship-addr[1]
         v-shipto-addr[2] = shipto.ship-addr[2]
         v-shipto-city = shipto.ship-city
         v-shipto-state = shipto.ship-state
         v-shipto-zip = shipto.ship-zip
         .

run edi-010.ip (
    input inv-head.company,
    input inv-head.inv-no,
    input inv-head.bill-to,
    input v-shipto-code
    ).
    
if return-value > "" then do:
    return return-value.
end.    

FOR EACH inv-line OF inv-head
    WHERE TRUE /* inv-line.edi-seq = 0 */:

  IF top-debug THEN
  RUN rc/debugrec.s ("", RECID(inv-line)) "inv-line".
  
  run edi-020.ip (input inv-line.po-no).

  FIND oe-bolh OF inv-line NO-LOCK NO-ERROR.

  /* 03.24.2004 CAH to provide a source for ship-date */
  FIND oe-rel OF inv-line NO-LOCK NO-ERROR.
  IF NOT AVAIL oe-rel THEN
  FIND oe-rel OF inv-head NO-LOCK NO-ERROR.

  IF top-debug AND AVAIL oe-rel
    THEN
  RUN rc/debugrec.s ("", RECID(oe-rel)) "oe-rel".

  FIND oe-ship OF inv-head NO-LOCK NO-ERROR.

  IF top-debug AND AVAIL oe-ship
    THEN
  RUN rc/debugrec.s ("", RECID(oe-ship)) "oe-ship".

  IF AVAIL oe-bolh
    THEN
  DO:
    IF top-debug THEN
    RUN rc/debugrec.s ("", RECID(oe-bolh)) "oe-bolh".
    FIND FIRST oe-boll
      WHERE oe-boll.company = inv-line.company
      AND   oe-boll.inv-no  = inv-line.inv-no
      AND   oe-boll.ord-no  = inv-line.ord-no
      AND   oe-boll.i-no    = inv-line.i-no
      NO-LOCK NO-ERROR.

    FIND FIRST carrier
      WHERE carrier.carrier = oe-bolh.carrier NO-LOCK NO-ERROR.

    IF top-debug AND AVAIL carrier
      THEN
    RUN rc/debugrec.s ("", RECID(carrier)) "carrier".

    IF AVAIL carrier THEN
    FIND edshipvia
      WHERE edshipvia.carrier = carrier.carrier NO-LOCK NO-ERROR.

    IF top-debug AND AVAIL edshipvia
      THEN
    RUN rc/debugrec.s ("", RECID(edshipvia)) "edshipvia".
  END.

  FIND oe-ord OF inv-line NO-LOCK NO-ERROR.
  IF AVAIL oe-boll AND AVAIL oe-ord
    THEN
  FIND oe-ordl OF oe-ord
    WHERE oe-ordl.line = oe-boll.line NO-LOCK NO-ERROR.
  ELSE
  FIND FIRST oe-ordl
    WHERE oe-ordl.company = inv-line.company
    AND oe-ordl.ord-no = inv-line.ord-no
    AND oe-ordl.i-no = inv-line.i-no NO-LOCK NO-ERROR.

  IF top-debug AND AVAIL oe-ord
    THEN
  RUN rc/debugrec.s ("", RECID(oe-ord)) "oe-ord".

  IF top-debug AND AVAIL oe-ordl
    THEN
  RUN rc/debugrec.s ("", RECID(oe-ordl)) "oe-ordl".



  IF AVAIL oe-ord AND oe-ord.sold-id > "" THEN
  DO:
    run edi-030.ip (
        INPUT inv-head.bill-to,
        INPUT oe-ord.sold-id).
  END.    /* if avail and > "" */

  /* assume billto/shipto is the same if there is no dc found */
  st_code = (IF AVAIL dc THEN
  dc.by-code ELSE
  by_code).
  
  run edi-040.ip (
    INPUT inv-line.i-no,
    INPUT string(inv-line.ord-no),
    INPUT string(inv-head.r-no),
    input inv-head.terms
    ).
    
  run edi-050.ip (
    inv-line.line,
    inv-line.i-no
    ).    
    
    
  IF top-debug THEN
  RUN rc/debugrec.s ("Bottom of Line Loop", RECID(edivline)) "edivline".
END.    /* each inv-line of inv-head */

FOR EACH inv-misc OF inv-head NO-LOCK
    WHERE inv-misc.deleted = FALSE:
    
    if top-debug then run rc/debugrec.s ("top of inv-misc loop", recid(inv-misc)) "inv-misc".    
    
  run gen-addon.ip (
    ws_eddoc_rec, 
    inv-misc.inv-line,      
    inv-misc.charge,
    inv-misc.Dscr,
    inv-misc.amt,
    0,             /* rate */
    inv-misc.bill,
    "ESTIMATE# " + inv-misc.est-no
        ).
    
END.    /* each inv-misc of inv-head */

if inv-head.t-inv-freight <> 0 then do:
  run gen-addon.ip (
    ws_eddoc_rec, 
    0,          /* line # */
    "FRT",
    "FREIGHT",
    inv-head.t-inv-freight,
    0,            /* rate */
    "Y",
    "Invoice Level Freight Charge"
        ).
end.

if inv-head.t-inv-tax <> 0 then do:
  find stax-group
  where stax-group.company = inv-head.company
  and stax-group.tax-group = inv-head.tax-gr no-lock no-error.
  RUN ar/cctaxrt.p (INPUT inv-head.company, inv-head.tax-gr ,
      OUTPUT dTaxRate, OUTPUT dTaxRateFreight).   
  run gen-addon.ip (
    ws_eddoc_rec, 
    0,          /* line # */
    "TAX",
    (if avail stax-group then stax-group.tax-dscr else "TAXES"),
    inv-head.t-inv-tax,
    dTaxRate,          /* rate */
    YES,
    "Invoice Level Taxes"
        ).
end.

IF top-debug THEN
RUN rc/debugrec.s ("Bottom of Program", RECID(edivtran)) "edivtran".

if top-debug then run rc/debugmsg.p ("...end of inv-head invoice").

RETURN "".  /* 03.22.2004 CAH, CALLER LOOKS FOR THIS */

end procedure.  /* edi-oe.ip */
