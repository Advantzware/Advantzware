{ed/sharedv.i}
{rc/statline.i}
{rc/scrvars.i}
def temp-table x
field   b-rec       as recid
field   l-rec       as recid
field   i-rec       as recid
field   mbol        like oe-bolh.master-bol-no
field   bol         like oe-bolh.bol-no
field   i-no        like oe-boll.i-no
field   Pallets     as int
field   Cases     as int
index byitem is primary mbol bol i-no
/* index byrec  is unique b-rec l-rec  */.
def new global shared var ws_mbol             like x.mbol no-undo.
def var hash_pallets        like x.pallets.
def var hash_cases          like x.cases.
def var answer as logical no-undo.
{rc/viewline.i &displayf="
    ws_mbol                 label 'Master BOL#' 
    oe-bolh.cust-no         label 'Cust#' 
    oe-bolh.sold-id         label 'ST'
    oe-bolh.ship-date       label 'Shipped' 
    oe-bolh.carrier         label 'Via'
    oe-bolh.master-bol-printed label 'Prt?' format 'y/n' "}
form
    x.bol                                          column-label "Bol#"
    oe-bolh.bol-date                               column-label "Date"
    oe-bolh.ship-id                 format 'x(05)' column-label "To"
    x.i-no                          format "x(10)" 
    itemfg.i-name                   format 'x(25)'    
    itemfg.case-pall                format ">>>9"  column-label "C/P"
    x.pallets                       format ">>>9"  column-label "#Pal"
        help 'Enter number of pallets, press f9 to change cases per pallet'
    x.Cases                         format ">>>9"  column-label "#Ctn"
        help 'Enter number of loose cartons'
with frame f-det down row 3 column 2.
form
    "" at 1 "Totals: " at 30 hash_pallets hash_Cases "" at 80
with frame f-hash row 22 side-labels center no-box
color white/magenta.
form
    itemfg.case-pall
with frame f-pallets overlay side-labels color value(c_pop)
row frame-row(f-det) + frame-line(f-det) + 1 column 40
.
view frame f-view.
/* view frame f-det. */
view frame f-hash.
_mbol: repeat:
update ws_mbol with frame f-view.
for each x: delete x. end.
assign hash_pallets = 0 hash_cases = 0.
for each oe-bolh
where oe-bolh.master-bol-no = ws_mbol
break by oe-bolh.b-no:
    if first-of (oe-bolh.b-no) then do:
        display
        oe-bolh.cust-no oe-bolh.ship-date oe-bolh.carrier
        oe-bolh.master-bol-printed
        oe-bolh.sold-id
        with frame f-view.
        pause 0.
    end.
    for each oe-boll where oe-boll.b-no = oe-bolh.b-no
    and can-do("S,B", oe-boll.s-code)   /* not invoice only */
    and not(oe-boll.deleted):
        find x 
        where x.b-rec = recid(oe-bolh)
          and x.l-rec = recid(oe-boll) exclusive-lock no-error.
    if not avail x then do:
        find itemfg of oe-boll no-lock no-error.
        create x.
        ws_int = if avail itemfg and itemfg.case-pall > 0
                then truncate(oe-boll.cases / itemfg.case-pall, 0)
                else 0.
 
        assign 
            x.b-rec     = recid(oe-bolh)
            x.l-rec     = recid(oe-boll)
            x.i-rec     = if avail itemfg then recid(itemfg) else ?
            x.mbol      = oe-bolh.master-bol-no
            x.bol       = oe-bolh.bol-no
            x.i-no      = oe-boll.i-no
            x.pallets   = ws_int
            x.cases     = oe-boll.cases
                - (ws_int * if avail itemfg then itemfg.case-pall else  0)
            hash_cases = hash_cases + x.cases
            hash_pallets = hash_pallets + x.pallets.
    end.
    end.
end.
assign lib_recid_ret = ?.
{rc/scrfm3.i
&FUNCTIONS  = "NYNY"
&ROWS       = 15
&TITLE      = "PALLETIZING SETUP"
&FILE       = "x"
&INDEX      = " "
&CONDITION  = "where x.mbol = ws_mbol"
&POSIT      = " "
&DETFUNCT   = 
"find itemfg where recid(itemfg) = i-rec no-lock no-error.
 find oe-bolh where recid(oe-bolh) = b-rec no-lock no-error.
 display x.bol
    itemfg.case-pall when avail itemfg oe-bolh.ship-id oe-bolh.bol-date 
    itemfg.i-name when avail itemfg
 with frame f-det.
"
&CHOOSE     = "i-no"
&KEYEDIT    = " "
&DISPLAYF   = "x.pallets x.Cases"
&DATAEDIT   = "if keylabel(lastkey) = 'f9' then do:
    find itemfg where recid(itemfg) = i-rec exclusive-lock no-error.
    if not avail itemfg then do:
        bell.
        message color value (c_err) 'Cannot find itemfg of this line'.
        next.
    end.
    update itemfg.case-pall with frame f-pallets.
    hide frame f-pallets no-pause.
    display itemfg.case-pall with frame f-det.
    next-prompt pallets with frame f-det.
    next.
    end. "
&TERMKEY    = " "
&UPFLDS     = " "
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    = " "
&DATAGO     = " "
&DETGO      = " "
&HASHDISP   = " 
    display hash_pallets hash_cases with frame f-hash."
&HASHPLUS   = "assign hash_cases = hash_cases + x.cases
    hash_pallets = hash_pallets + x.pallets."
&HASHMINUS  = "assign hash_cases = hash_cases - x.cases
    hash_pallets = hash_pallets - x.pallets."
}
message "Export Label printing data?" update answer.
if answer then do:
    top-debug = false.
    run gen_856.ip.
    top-debug = false.
end.
end.    /* repeat for mbol */
procedure gen_856.ip:
{ed/edivars.i}
DEF BUFFER dc FOR edshipto.
def var v-emp-id like soldto.sold-id no-undo.
def var v-emp-recid as recid no-undo.
def var v-mast-bol-no like oe-bolh.bol-no format ">>>>>9" no-undo.
def var local-debug as logical no-undo initial false.
def var ws_bol_chardate as char no-undo format "x(08)".
def var ws_userref like eddoc.userref no-undo.
DEF VAR ws_pallet-mark LIKE edshline.pallet-mark no-undo.
DEF VAR ws_carton-mark LIKE edshline.carton-mark no-undo.
DEF VAR NUM_PACKAGES AS INT NO-UNDO.
def var package_list as char no-undo.
def var package_type as char no-undo.
def var i as int no-undo.
local-debug = top-debug.
for each x
break by mbol:
FIND oe-bolh WHERE RECID(oe-bolh) = x.b-rec NO-LOCK NO-ERROR.
find oe-boll where recid(oe-boll) = x.l-rec no-lock no-error.
find itemfg  where recid(itemfg) = x.i-rec no-lock no-error.
assign 
    v-emp-id = oe-bolh.sold-id
    v-mast-bol-no = oe-bolh.master-bol-no.
    
IF NOT AVAIL oe-bolh THEN return error.
if not avail oe-boll then return error.
FIND first edmast where edmast.cust = oe-bolh.cust-no no-lock no-error.
if not avail edmast THEN RETURN error.
find first edcode of edmast
where edcode.direction = 'o' and edcode.setid = '856' no-lock no-error.
IF NOT AVAIL edcode THEN RETURN error.
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
  
  end.
  
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
    AND edpotran.cust-po        = oe-bolh.po-no
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
      /* eddoc.docseq        = INTEGER(location_number) 9806 CAH can fail */
      eddoc.st-code       = location_number
      eddoc.userref       = ws_userref
        /* "B-NO: " + STRING(oe-bolh.b-no) */
      .
    run rc/str2int.p (input location_number, output eddoc.docseq).
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
  else if first-of (x.mbol) then do:
    for each edshline of edshtran:
        delete edshline.
    end.
    for each edshtare of edshtran:
        delete edshtare.
    end.
    for each edshpack of edshtran:
        delete edshpack.
    end.
    for each edshord of edshtran:
        delete edshord.
    end.    
    assign edshtran.tot-cartons = 0
           edshtran.tot-volume  = 0
           edshtran.tot-wght    = 0
           .
  end.
  NUM_PACKAGES = x.pallets + x.cases.
  package_list = fill("P", x.pallets) + fill("C", x.cases).
  
  repeat i = 1 to num_packages:
  
  assign ws_carton-mark = ? ws_pallet-mark = ?.
  package_type = substring(package_list,i,1).
  run gen_scc18.ip (input package_type, output ws_carton-mark).
  /* pallet mark is an integer field and this causes overflow ...
  if package_type = "P"
  then assign ws_pallet-mark = integer(ws_carton-mark)
    ws_carton-mark = ?.
  */
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
  end.  /* repeat for num packages */
  
END.    /* each x */
end procedure.
procedure gen_scc18.ip:
/* given packaging type, return next serial number */
def input  param p_type as char no-undo. /* P=Pallet, C=Carton/case */
def output param p_code as char no-undo.
/* local variables */
def var ws_app_ident    as char no-undo format "x(02)" initial "00". 
    /* UCC 128 SCC */
def var ws_package_type as char no-undo format "x(01)" initial "0". 
    /*  0=case/carton/hanging container
        1=pallet
        2=larger than pallet, e.g. container load or truckload
        3=Undefined
        4=Ship to defined
      5-9=Shipper specific */
def var ws_ucc_code     as char no-undo  format "x(07)"     initial "0000000".
def var ws_scc_code     as int  no-undo  format "999999999" initial 0.
def var ws_checkdigit   as int  no-undo  format "9"         initial 0.
p_code = ?.
if not can-do("P*,C*", p_type) then p_type = "C". /* assume carton */
else if length(p_type) > 1 then p_type = substring(p_type,1,1).
case p_type:
when "C" then ws_package_type = "0".
when "P" then ws_package_type = "1".
otherwise     ws_package_type = "0".
end case.
run rc/regkey.ip ("UCC/Mfr-ID", output ws_ucc_code).
run rc/regkey.ip ("UCC/SCC_serial", output ws_char).
ws_scc_code = integer(ws_char) + 1.
ws_char = string(ws_scc_code).
run rc/regsave.ip (ws_company, "UCC/SCC_serial", ws_char).
ws_char = ws_app_ident 
       + ws_package_type
       + ws_ucc_code
       + string(ws_scc_code,"999999999").
run rc/cknum10.p (ws_char, "", output p_code, output ws_checkdigit).        
if true or top-debug then run rc/debugmsg.p ("code in: " + ws_char + 
    " code out " + p_code).
    
end procedure.
procedure rc/regkey.ip:
DEF INPUT   PARAM p_key AS CHAR NO-UNDO.
DEF OUTPUT  PARAM ws_value AS CHAR NO-UNDO.
FIND FIRST asi.sys-ctrl
  WHERE asi.sys-ctrl.company = ws_company AND asi.sys-ctrl.name = p_key NO-LOCK NO-ERROR.
IF NOT AVAIL asi.sys-ctrl THEN
DO:
  FIND FIRST asi.sys-ctrl
    WHERE asi.sys-ctrl.name = p_key NO-LOCK NO-ERROR.
  IF NOT AVAIL asi.sys-ctrl THEN
  DO:
    INPUT FROM TERMINAL.
    CREATE asi.sys-ctrl.
    ASSIGN asi.sys-ctrl.company = ws_company
      asi.sys-ctrl.name = p_key
      asi.sys-ctrl.descrip = "Created on-the-fly from " +
      PROGRAM-NAME(2).    UPDATE
      asi.sys-ctrl
      WITH FRAME f-reg CENTER TITLE "Please Touchup asi.sys-ctrl Entry"
      1 COLUMN ROW 4 OVERLAY.
    HIDE FRAME f-reg NO-PAUSE.
  END.
END.
IF substring(sys-ctrl.descrip,1,1) = 'l' THEN
ws_value =
STRING(asi.sys-ctrl.log-fld,'Yes/No').
ELSE
IF substring(sys-ctrl.descrip,1,1) = 'c' THEN
ws_value = asi.sys-ctrl.char-fld.
ELSE
IF substring(sys-ctrl.descrip,1,1) = 'i' THEN
ws_value = STRING(asi.sys-ctrl.int-fld).
end procedure.
procedure rc/regsave.ip:
DEF INPUT PARAM p_co    AS char NO-UNDO.
DEF INPUT PARAM p_key   AS CHAR NO-UNDO.
DEF INPUT PARAM p_value AS CHAR NO-UNDO.
DEF var top-debug AS logical NO-UNDO.
IF top-debug THEN
DO:
  MESSAGE "in" PROGRAM-NAME(1) p_co p_key p_value.
END.
DO ON error UNDO, LEAVE:
  FIND FIRST asi.sys-ctrl
    WHERE asi.sys-ctrl.company = p_co
    AND   asi.sys-ctrl.name = p_key EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL asi.sys-ctrl THEN
  FIND asi.sys-ctrl
    WHERE asi.sys-ctrl.name = p_key EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL asi.sys-ctrl THEN
  DO:
    IF substring(sys-ctrl.descrip,1,1) = 'l' THEN
    ASSIGN sys-ctrl.log-fld =
      (IF p_value BEGINS 'y' THEN TRUE ELSE FALSE).
    ELSE
    IF substring(sys-ctrl.descrip,1,1) = 'c' THEN
    ASSIGN sys-ctrl.char-fld = p_value.
    ELSE
    IF substring(sys-ctrl.descrip,1,1) = 'i' THEN
    ASSIGN sys-ctrl.int-fld = integer(p_value).
    IF top-debug THEN
    DO:
      MESSAGE "sys-ctrl entry" p_key "value updated to" p_value.
    END.
  END.
END.
end procedure.
