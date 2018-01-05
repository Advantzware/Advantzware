/***************************************************************************\
*****************************************************************************
** Program: D:\RPRODEV\ED\856.P
** By: Christopher A. Heins, Report Concepts, Inc. (c) 1997
** All Rights Reserved *
** Descript: ASN interface, application to EDI database
10.17.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Lines were all glomming onto the same carton and tare.  Added assignment
of pallet mark as integer of customer po number and carton mark as customer
po number when oe-ord is available, with fallback to ord-no and bol-line
if it is not.
06.10.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Changed docid to be string(v-mast-bol-no), new variable added by asi.
This is not presently stamped into the header so it cannot be used as the
basis for rerun.
2.  Moved YYYYMMDD-Trailer# to userref.  This is indexed but not by partner.
05.29.97 by CAH on novell@asi<w:\clientdb\royal> Log#0000:
1.  Modified to hook into master bill of lading print routine.
05.12.97 by CAH on ricky@812<rprodev> Log#0000:
1.  Added check of oe-boll.s-code.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i}
def shared var v-emp-id like soldto.sold-id no-undo.
def shared var v-emp-recid as recid no-undo.
def SHARED var v-mast-bol-no like oe-bolh.bol-no format ">>>>>9" no-undo.
def var local-debug as logical no-undo initial false.
def var i as int no-undo.
DEF BUFFER dc FOR edshipto.
def var ws_bol_chardate as char no-undo format "x(08)".
def var ws_userref like eddoc.userref no-undo.
DEF VAR ws_pallet-mark LIKE edshline.pallet-mark no-undo.
DEF VAR ws_carton-mark LIKE edshline.carton-mark no-undo.
FIND oe-bolh WHERE RECID(oe-bolh) = ws_process_rec NO-LOCK NO-ERROR.
IF NOT AVAIL oe-bolh THEN
RETURN.
FIND edmast WHERE RECID(edmast) = ws_edmast_rec
  EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL edmast THEN
RETURN.
FIND edcode WHERE RECID(edcode) = ws_edcode_rec
  NO-LOCK NO-ERROR.
IF NOT AVAIL edcode THEN
RETURN.
ASSIGN
  ws_company = oe-bolh.company
  ws_partner = edmast.partner
  ws_bol_chardate
	= string(year (oe-bolh.bol-date),"9999")
	+ string(month(oe-bolh.bol-date),"99")
	+ string(day  (oe-bolh.bol-date),"99")
  bill_of_lading_number = string(v-mast-bol-no)
  ws_userref =
    ws_bol_chardate + '-' + oe-bolh.trailer
    .
FOR EACH oe-boll
    WHERE oe-boll.b-no = oe-bolh.b-no
    and can-do("S,B", oe-boll.s-code)   /* not invoice only */
    :
/*  FIND carrier OF oe-bolh NO-LOCK NO-ERROR. */
  FIND carrier where carrier.company eq oe-bolh.company
		 and carrier.carrier eq oe-bolh.carrier
		 and carrier.loc     eq oe-bolh.loc
		USE-INDEX carrier NO-LOCK NO-ERROR.
  IF AVAIL carrier THEN
  FIND edshipvia
    WHERE edshipvia.carrier = carrier.carrier NO-LOCK NO-ERROR.
  FIND oe-ord OF oe-boll NO-LOCK NO-ERROR.
  IF AVAIL oe-boll AND AVAIL oe-ord
    THEN
  FIND oe-ordl OF oe-ord
    WHERE oe-ordl.line = oe-boll.line NO-LOCK NO-ERROR.
  ELSE
  FIND FIRST oe-ordl
    WHERE oe-ordl.company = oe-boll.company
    AND oe-ordl.ord-no = oe-boll.ord-no
    AND oe-ordl.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
  /* 9706 CAH: v-emp-id is set by caller, contains the DC ship code.  If blank
    we can fallback to sold-id in order */
  if v-emp-id = "" and avail oe-ord
  then v-emp-id = oe-ord.sold-id.
  if avail oe-ord then do:
  /* asi does not have these fields */
  def var p1 as char no-undo.
  def var p2 as char no-undo.
  def var i1 as int no-undo.
  def var i2 as int no-undo.
  p1 = oe-ord.po-no.
  p2 = "".
  i1 = 1.
  i2 = 0.
  /* create a string with only numeric digits from the po number */
  do i1 = 1 to length(p1):
    if  substring(p1,i1,1) >= "0"
    and substring(p1,i1,1) <= "9"
    then assign i2 = i2 + 1
	substring(p2,i2,1) = substring(p1,i1,1).
  end.
  ws_pallet-mark = integer(p2).
  ws_carton-mark = oe-ord.po-no.
  end.  /* if avail oe-ord */
  else assign ws_pallet-mark = oe-boll.ord-no
    ws_carton-mark = string(oe-boll.line).
  FIND FIRST inv-line
    WHERE inv-line.company = oe-bolh.company
    AND   inv-line.ord-no = oe-boll.ord-no
    AND   inv-line.line   = oe-boll.line
    NO-LOCK NO-ERROR.
  IF NOT AVAIL inv-line THEN
  FIND FIRST inv-line
    WHERE inv-line.company = oe-bolh.company
    AND   inv-line.ord-no = oe-boll.ord-no
    AND   inv-line.i-no   = oe-boll.i-no
    NO-LOCK NO-ERROR.
  IF AVAIL inv-line THEN
  DO:
    RUN ed/asi/lkupstby.p
      (INPUT RECID(inv-line),
      OUTPUT location_number,
      OUTPUT ordering_store_number).
    find inv-head of inv-line no-lock no-error.
  END.
  ELSE
  ASSIGN location_number = "" ordering_store_number = "".
  FIND FIRST edshipto
    WHERE edshipto.partner = edmast.partner
    AND edshipto.ref-type = "by"
    AND edshipto.cust = edmast.cust
    AND edshipto.ship-to = oe-bolh.ship-id NO-LOCK NO-ERROR.
  if avail edshipto then ordering_store_number = edshipto.by-code.
  IF local-debug and NOT AVAIL edshipto THEN
  DO:
    BELL.
    MESSAGE "Could not locate BY via;" edmast.partner "by" edmast.cust
      oe-bolh.ship-id.
    PAUSE.
  END.
  /* 9706 CAH: v-emp-id overrides st code */
  if v-emp-id > "" then location_number = v-emp-id.
  FIND FIRST edpotran
    WHERE edpotran.partner      = edmast.partner
    AND edpotran.cust-po        = oe-ord.po-no
    NO-LOCK NO-ERROR.
  IF AVAIL edpotran
    THEN
  DO:
    FIND FIRST edpoline OF edpotran
      WHERE edpoline.line = oe-boll.line NO-LOCK NO-ERROR.
    IF NOT AVAIL edpoline
      THEN
    FIND FIRST edpoline OF edpotran
      WHERE edpoline.item-no = oe-boll.i-no
      AND edpoline.by-code = ordering_store_number NO-LOCK NO-ERROR.
    IF NOT AVAIL edpoline
      THEN
    FIND FIRST edpoline OF edpotran
      WHERE edpoline.item-no = oe-boll.i-no
      AND edpoline.st-code = ordering_store_number NO-LOCK NO-ERROR.
  END.
  IF AVAIL oe-ordl THEN
  FIND FIRST uom WHERE uom.uom = oe-ordl.pr-uom.
  FIND FIRST dc
    WHERE dc.partner = edmast.partner
    AND dc.ref-type = "st"
    AND dc.cust = edmast.cust
    AND dc.ship-to = v-emp-id /* oe-ord.sold-id 9706 CAH */ NO-LOCK NO-ERROR.
  IF NOT AVAIL dc THEN
  FIND FIRST dc
    WHERE dc.partner = edmast.partner
    AND dc.ref-type = "by"
    AND dc.cust = edmast.cust
    AND dc.ship-to = v-emp-id /* oe-ord.sold-id 9706 CAH */ NO-LOCK NO-ERROR.
  IF local-debug and noT AVAIL dc THEN
  DO:
    BELL.
    MESSAGE "Could not locate ST via:" edmast.partner "by" edmast.cust
      "st" oe-ord.sold-id "emp" v-emp-id.
    PAUSE.
  END.
  FIND FIRST edshtran
    WHERE edshtran.bol-no = bill_of_lading_number
    AND edshtran.st-code = v-emp-id
    EXCLUSIVE NO-ERROR.
  IF NOT AVAIL edshtran THEN
  DO:
    run ed/gendoc.p (recid(edcode), bill_of_lading_number, output ws_eddoc_rec).
    find eddoc where recid(eddoc) = ws_eddoc_rec exclusive.
    ASSIGN
      eddoc.docseq        = INTEGER(location_number)
      eddoc.st-code       = location_number
      eddoc.userref       = ws_userref
	/* "B-NO: " + STRING(oe-bolh.b-no) */
      .
    CREATE edshtran.
    ASSIGN
      edshtran.partner  = eddoc.partner
      edshtran.seq      = eddoc.seq
      edshtran.bol-no   = bill_of_lading_number
      /* STRING(oe-bolh.bol-no) */
      edshtran.st-code  = location_number /* 970610 CAH: dc.st-code */
      edshtran.ship-name = oe-ord.sold-name
      edshtran.ship-address[1] = oe-ord.sold-addr[1]
      edshtran.ship-address[2] = oe-ord.sold-addr[2]
      edshtran.ship-city  = oe-ord.sold-city
      edshtran.ship-st = oe-ord.sold-state
      edshtran.ship-zip   = oe-ord.sold-zip
      edshtran.ship-date-code = "011"   /* shipped */
      edshtran.ship-date = oe-bolh.bol-date
      edshtran.ship-method-code = "T"   /* best way */
      edshtran.carrier  = oe-bolh.carrier
      edshtran.carrier-code =
      (IF AVAIL edshipvia AND edshipvia.carrier-code > ""
      THEN edshipvia.carrier-code ELSE "")
      edshtran.trailer = oe-bolh.trailer
      edshtran.sf-code = oe-bolh.loc
      edshtran.vn-code = edmast.we-vend-no
      edshtran.bol-adddate = today
      edshtran.bol-addtime = time
      edshtran.package-code =
	(if oe-bolh.tot-pallets > 0 then "PLT" else "CTN")
	+ "25" /* corrugated cartons */.
      .
      do i = 1 to 4:
	edshtran.routing[i] = oe-bolh.ship-i[i].
      end.
  END.
  FIND FIRST edshline
    WHERE edshline.partner = edshtran.partner
    and edshline.seq = edshtran.seq
    and edshline.bol-no = edshtran.bol-no
    AND edshline.st-code = edshtran.st-code
    AND edshline.cust-po = oe-ord.po-no
    AND edshline.cust-po-line = STRING(oe-ordl.line)
    AND edshline.pallet-mark = ws_pallet-mark /* edivline.pallet-mark */
    AND edshline.carton-mark = ws_carton-mark /* edivline.carton-mark */
    AND edshline.by-code = ordering_store_number /* edshipto.by-code */
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edshline THEN
  DO:
    find itemfg
    where itemfg.company = oe-boll.company
    and itemfg.i-no = oe-boll.i-no no-lock no-error.
    CREATE edshline.
    ASSIGN
      edshline.partner  = edshtran.partner
      edshline.seq      = edshtran.seq
      edshline.invoice-no = string(oe-boll.inv-no)
      edshline.uom-code = oe-ordl.pr-uom
      edshline.bol-no   = edshtran.bol-no
      edshline.st-code  = edshtran.st-code
      edshline.cust-po  = oe-ord.po-no
      edshline.cust-po-line = STRING(oe-ordl.line)
      edshline.cust-po-date = oe-ord.ord-date
      edshline.cust-item = oe-ordl.part-no
      edshline.qty-orig-ord = oe-ordl.qty
      edshline.order-no = string(oe-boll.ord-no)
      edshline.upc = if avail itemfg then itemfg.upc else ""
      edshline.by-code  = ordering_store_number /* 9706: edshipto.by-code */
      edshline.pallet-mark = ws_pallet-mark /* edivline.pallet-mark */
      edshline.carton-mark = ws_carton-mark
      edshline.unit-price = oe-ordl.price
      edshline.pack-size = oe-boll.qty-case
      edshline.item-no = oe-boll.i-no
      edshtran.lines = edshtran.lines + 1
      .
  END.
  ASSIGN
    {rc/accum.i edshline.qty-shipped "(oe-boll.qty / oe-boll.qty-case)" }
    {rc/accum.i edshline.tot-cartons oe-boll.cases}
    {rc/accum.i edshline.tot-volume  0}
    {rc/accum.i edshline.tot-wght    oe-boll.weight}
    edshline.case-wght =
	if edshline.tot-cartons > 0 then
	round(edshline.tot-wght / edshline.tot-cartons, 0)
	else 0
    {rc/accum.i edshtran.tot-cartons oe-boll.cases}
    {rc/accum.i edshtran.tot-volume  0}
    {rc/accum.i edshtran.tot-wght    oe-boll.weight}
    .
  FIND FIRST edshord OF edshtran
    WHERE   edshord.bol-no = edshline.bol-no
    AND     edshord.cust-po = edshline.cust-po
    AND     edshord.order-no = edshline.order-no
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edshord THEN
  DO:
    CREATE edshord.
    ASSIGN
      edshord.partner = edshtran.partner
      edshord.seq = edshtran.seq
      edshord.bol-no = edshline.bol-no
      edshord.cust-po = edshline.cust-po
      edshord.order-no = edshline.order-no
      edshord.by-code = edshline.by-code
      edshord.st-code = edshline.st-code
      edshord.cust-po-date = edshline.cust-po-date
      edshord.invoice-no = edshline.invoice-no
      edshord.invoice-date =
	if avail inv-head then inv-head.inv-date else ?
      edshord.cust-dept =
	if avail edpotran then edpotran.cust-dept else ""
      edshord.cust-div          =
	if avail edpotran then edpotran.cust-div else
	if avail itemfg   then itemfg.procat else ""
      .
  END.
  ASSIGN
    {rc/incr.i edshord.lines}
    edshord.last-line = MAX(edshord.last-line,
    INTEGER(edshline.cust-po-line))
    {rc/accum.i edshord.tot-cartons edshline.tot-cartons}
    {rc/accum.i edshord.tot-volume  edshline.tot-volume}
    {rc/accum.i edshord.tot-wght    edshline.tot-wght}
    .
  FIND FIRST edshtare OF edshtran
    WHERE edshtare.bol-no = edshtran.bol-no
    AND edshtare.pallet-mark = edshline.pallet-mark
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edshtare THEN
  DO:
    CREATE edshtare.
    ASSIGN
      edshtare.partner = edshtran.partner
      edshtare.seq = edshtran.seq
      edshtare.bol-no = edshline.bol-no
      edshtare.pallet-mark = edshline.pallet-mark
      edshtare.cust-po = edshline.cust-po
      edshtare.package-code = ""
      .
  END.
  ASSIGN
    {rc/accum.i edshtare.tot-cartons edshline.tot-cartons}
    {rc/accum.i edshtare.tot-volume  edshline.tot-volume}
    {rc/accum.i edshtare.tot-wght    edshline.tot-wght}
    .
  FIND FIRST edshpack OF edshtran
    WHERE edshpack.bol-no = edshline.bol-no
    AND edshpack.pallet-mark = edshline.pallet-mark
    AND edshpack.carton-mark = edshline.carton-mark
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edshpack THEN
  DO:
    CREATE edshpack.
    ASSIGN
      edshpack.partner = edshtran.partner
      edshpack.seq = edshtran.seq
      edshpack.bol-no = edshline.bol-no
      edshpack.pallet-mark = edshline.pallet-mark
      edshpack.carton-mark = edshline.carton-mark
      edshpack.cust-po = edshline.cust-po
      edshpack.package-code = ""
      .
  END.
  ASSIGN
    {rc/accum.i edshpack.tot-cartons edshline.tot-cartons}
    {rc/accum.i edshpack.tot-volume  edshline.tot-volume}
    {rc/accum.i edshpack.tot-wght    edshline.tot-wght}
    .
END.    /* each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no */
