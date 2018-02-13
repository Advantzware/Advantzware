/***************************************************************************\
*****************************************************************************
**  Program: e:\asi\patch\edi\ed\asi\fmb
**       By: Chris Heins for Royal Pioneer Industries
** Descript: Edit packaging by ship to to allow for UCC128 labels and ASN
**
*****************************************************************************
\***************************************************************************/
/*
{ed/sharedv.i}
{rc/statline.i}
{rc/scrvars.i}
DEF temp-table x
  FIELD   b-rec       AS RECID
  FIELD   l-rec       AS RECID
  FIELD   i-rec       AS RECID
  FIELD   mbol        LIKE oe-bolh.master-bol-no
  FIELD   bol         LIKE oe-bolh.bol-no
  FIELD   i-no        LIKE oe-boll.i-no
  FIELD   Pallets     AS int    FORMAT '>>9'
  FIELD   Cases       AS int    FORMAT '>>9'
  FIELD   Combo       AS char   FORMAT 'x(09)'
  FIELD   ship-id     AS char
  INDEX byitem IS primary mbol bol i-no
  INDEX byrec  IS UNIQUE  b-rec l-rec
  INDEX byship      ship-id i-no bol.
DEF temp-table y
  FIELD   ship-id     LIKE oe-bolh.ship-id
  FIELD   name        LIKE shipto.ship-name
  FIELD   pallets     LIKE x.pallets
  FIELD   cases       LIKE x.cases
  FIELD   combo-list  AS char COLUMN-LABEL "Combo Pallets"
  FIELD   updated     AS logical FORMAT "#/ "
  FIELD   posted      AS logical FORMAT "P/ "
  INDEX byship IS primary UNIQUE ship-id.
DEF var w AS widget-handle NO-UNDO.
DEFINE button b-load    LABEL "Load BOL".
DEFINE button b-Save    LABEL "Save BOL".
DEFINE button b-cancel  LABEL "Cancel".
DEFINE button b-quit    LABEL "Quit".
DEF QUERY b FOR y scrolling.
DEF browse b QUERY b
  DISPLAY
  y.ship-id
  y.name      FORMAT 'X(20)'
  y.pallets
  y.cases
  y.combo-list
  y.updated   LABEL "Chg?"
  y.posted    LABEL "Pst?"
  WITH no-row-markers 5 DOWN CENTER TITLE "Select Destination"
  row 3 COLOR VALUE(c_det).
DEF FRAME f
  b SKIP
  b-load  AT 5
  b-save  AT 25
  b-cancel AT 45
  b-quit  AT 65
  WITH CENTER.
DEF NEW GLOBAL SHARED var ws_mbol             LIKE x.mbol NO-UNDO.
DEF var hash_pallets        LIKE x.pallets.
DEF var hash_cases          LIKE x.cases.
DEF var answer AS logical NO-UNDO.
DEF var last_combo          AS char initial "".
{rc/viewline.i &displayf="
    ws_mbol                 label 'Mstr BOL#'
    oe-bolh.cust-no         label 'Cust#'
    oe-bolh.sold-id         label 'ST'
    oe-bolh.ship-date       label 'On'
    oe-bolh.carrier         label 'Via'
    oe-bolh.master-bol-printed label 'Prt?' format 'y/n' "}
FORM
  x.bol                                          COLUMN-LABEL "Bol#"
  x.i-no                          FORMAT "x(10)"
  itemfg.i-name                   FORMAT 'x(25)'
  itemfg.case-pall                FORMAT ">>>9"  COLUMN-LABEL "C/P"
  x.pallets                       FORMAT ">>>9"  COLUMN-LABEL "#Pal"
  HELP '# OF pallets, f6=NEXT Combo; f9=Change cases per pallet'
  x.Cases                         FORMAT ">>>9"  COLUMN-LABEL "#Ctn"
  HELP 'Enter number OF loose cartons'
  x.combo
  HELP 'Enter pallet code A-Z[,Quantity] FOR combination pallets'
  WITH FRAME f-det DOWN row 13 CENTER.
FORM
  "Totals-" AT 1 "M-BOL#:" hash_pallets hash_Cases
  y.ship-id LABEL "Store" y.pallets y.cases "" AT 80
  WITH FRAME f-hash row 22 side-labels CENTER no-box
  COLOR white/magenta.
FORM
  itemfg.case-pall
  WITH FRAME f-pallets OVERLAY side-labels COLOR VALUE(c_pop)
  row FRAME-ROW(f-det) + FRAME-LINE(f-det) + 1 COLUMN 40
  .
VIEW FRAME f-view.
/* view frame f-det. */
VIEW FRAME f-hash.
w = CURRENT-WINDOW.
VIEW w.
_mbol:
DO WITH FRAME f:
  ENABLE b-load b-quit WITH FRAME f.
  ON choose OF b-load
  DO:
    RUN load_bol.ip.
    OPEN QUERY b
      FOR EACH y BY ship-id BY i-no.
    DISABLE ALL WITH FRAME f.
    ENABLE b b-save b-cancel b-quit WITH FRAME f.
    b:sensitive = TRUE.
    apply "ENTRY":U to b.
  END.
  ON RETURN, default-action, mouse-select-click OF b
    DO:
    ASSIGN ws_char = y.ship-id ws_recid = ? lib_recid_ret = ?.
    RUN edit_bol.ip.
  END.
  ON choose OF b-save
  DO:
    MESSAGE "Export Label printing data?" UPDATE answer.
    IF answer THEN
    DO:
      top-debug = FALSE.
      STATUS DEFAULT "Wait, saving shipment data ...".
      RUN gen_856.ip.
      IF answer THEN
      DO:
        DISABLE b b-save WITH FRAME f.
        top-debug = FALSE.
        y.posted = TRUE.
        y.updated = FALSE.
        APPLY "choose" TO b-cancel.
        ENABLE b-load WITH FRAME f.
      END.
      STATUS DEFAULT.
    END.
  END.  /* save button */
  ON choose OF b-cancel
  DO:
    STATUS DEFAULT "Wait, clearing workspace ...".
    FOR EACH x:
      DELETE x.
    END.
    FOR EACH y:
      DELETE y.
    END.
    CLEAR FRAME f NO-PAUSE.
    clear frame f-hash no-pause.
    clear frame f-view no-pause.
    STATUS DEFAULT.
    DISABLE b WITH FRAME f.
    ENABLE b-load b-quit WITH FRAME f.
    apply "choose" to b-load.
  END.
  ON CHOOSE OF b-quit
  DO:
    APPLY "choose":U TO b-cancel.
    APPLY "WINDOW-close":U TO THIS-PROCEDURE.
  END.
  RUN rc/debugmsg.p ("before wait-for").
  WAIT-FOR WINDOW-close OF THIS-PROCEDURE FOCUS b-load IN FRAME f.
  RUN rc/debugmsg.p ("after wait-for").
END.    /* repeat for mbol */
    HIDE FRAME f-det no-pause.
    HIDE FRAME f-hash no-pause.
    HIDE FRAME f no-pause.
    hide frame f-view no-pause.
/* ================== INTERNAL PROCEDURES ==================== */
procedure load_bol.ip:
UPDATE ws_mbol WITH FRAME f-view.
FOR EACH x:
  DELETE x.
END.
FOR EACH y:
  DELETE y.
END.
ASSIGN hash_pallets = 0 hash_cases = 0.
FOR EACH oe-bolh
    WHERE oe-bolh.master-bol-no = ws_mbol
    BREAK BY oe-bolh.b-no:
  IF FIRST-OF (oe-bolh.b-no) THEN
  DO:
    DISPLAY
      oe-bolh.cust-no oe-bolh.ship-date oe-bolh.carrier
      oe-bolh.master-bol-printed
      oe-bolh.sold-id
      WITH FRAME f-view.
    PAUSE 0.
  END.
  FIND FIRST y
    WHERE y.ship-id = oe-bolh.ship-id EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL y THEN
  DO:
    CREATE y.
    FIND shipto OF oe-bolh NO-LOCK NO-ERROR.
    ASSIGN
      y.ship-id = oe-bolh.ship-id
      y.name = IF AVAIL shipto THEN
      (shipto.ship-CITY + " " + SHIP-STATE)
      ELSE "*Undefined*".
    .
  END.
  FOR EACH oe-boll WHERE oe-boll.b-no = oe-bolh.b-no
      AND CAN-DO("S,B", oe-boll.s-code)   /* not invoice only */
      AND NOT(oe-boll.deleted):
    FIND x
      WHERE x.b-rec = RECID(oe-bolh)
      AND x.l-rec = RECID(oe-boll) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL x THEN
    DO:
      FIND itemfg OF oe-boll NO-LOCK NO-ERROR.
      CREATE x.
      ws_int = IF AVAIL itemfg AND itemfg.case-pall > 0
        THEN
      truncate(oe-boll.cases / itemfg.case-pall, 0)
      ELSE
      0.
      ASSIGN
        x.b-rec     = RECID(oe-bolh)
        x.l-rec     = RECID(oe-boll)
        x.i-rec     = IF AVAIL itemfg THEN RECID(itemfg) ELSE ?
        x.mbol      = oe-bolh.master-bol-no
        x.bol       = oe-bolh.bol-no
        x.i-no      = oe-boll.i-no
        x.pallets   = ws_int
        x.cases     = oe-boll.cases
        - (ws_int * IF AVAIL itemfg THEN itemfg.case-pall ELSE  0)
        hash_cases = hash_cases + x.cases
        hash_pallets = hash_pallets + x.pallets
        x.ship-id = oe-bolh.ship-id
        y.pallets    = y.pallets + x.pallets
        y.cases      = y.cases + x.cases
        .
    END.
  END.
END.
display hash_pallets hash_cases with frame f-hash.
END procedure.  /* load bol */
DEF temp-table SAVE_BUFF LIKE x.
procedure edit_bol.ip:
FIND y WHERE y.ship-id = ws_char EXCLUSIVE-LOCK NO-ERROR.
DEF var dirty_flag AS logical NO-UNDO.
{rc/scrfm3.i
  &FUNCTIONS  = "NYNY"
  &ROWS       = 5
  &TITLE      = "PALLETIZING SETUP"
  &FILE       = "x"
  &INDEX      = " "
  &CONDITION  = "where x.ship-id = y.ship-id"
  &POSIT      = " "
  &DETFUNCT   =
  "find itemfg where recid(itemfg) = x.i-rec no-lock no-error.
 find oe-bolh where recid(oe-bolh) = x.b-rec no-lock no-error.
 display x.bol
    itemfg.case-pall when avail itemfg
    itemfg.i-name when avail itemfg
 with frame f-det.
"
  &CHOOSE     = "i-no"
  &KEYEDIT    = "FOR EACH SAVE_BUFF EXCLUSIVE:
    DELETE SAVE_BUFF. END.
    create save_buff. BUFFER-COPY x to save_buff. dirty_flag = false."
  &DISPLAYF   = "x.pallets x.Cases x.combo"
  &DATAEDIT   =
  "if keylabel(lastkey) = 'f9' then do:
    find itemfg where recid(itemfg) = x.i-rec exclusive-lock no-error.
    if not avail itemfg then do:
        bell.
        message color value (c_err) 'Cannot find itemfg of this line'.
        next.
    end.
    update itemfg.case-pall with frame f-pallets.
    hide frame f-pallets no-pause.
    display itemfg.case-pall with frame f-det.
    next-prompt x.pallets with frame f-det.
    next.
  end.
  if keylabel(lastkey) = 'f6' then do:
    scr_paint_rec = recid(x).
    release x.
    run auto_pall.ip.
    if not error-status:error then assign dirty_flag = true y.updated = true.
    assign scr_paint = true scrollbar_down = 'v' scr_paint_dir = +1.
    next looper.
  end."
  &TERMKEY    = " "
  &UPFLDS     = " "
  &HELPKEY    = " "
  &DETEDIT    = " "
  &ADDCODE    = " "
  &ADDPOST    = " "
  &DATAGO     = " "
  &DETGO      = " "
  &HASHDISP   = "
    display hash_pallets hash_cases y.pallets y.cases
    y.ship-id with frame f-hash."
  &HASHPLUS   =
  "assign
    hash_cases = hash_cases + x.cases
    hash_pallets = hash_pallets + x.pallets
    y.pallets = y.pallets + x.pallets
    y.cases = y.cases + x.cases.
    do ws_int = 1 to num-entries(x.combo):
    ws_char = CAPS(entry(ws_int, x.combo)).
    if not(ws_char >= 'A' and ws_char <= 'z') then next.
    if ws_char > last_combo then last_combo = ws_char.
    if not can-do(y.combo-list, ws_char)
    then do: {rc/listadd.i y.combo-list ws_char}.
    end.
    end.
    buffer-compare x to save_buff
    save result in ws_logical.
    dirty_flag = not ws_logical.
    delete save_buff.
    y.updated = y.updated or dirty_flag.
    /* display y.combo-list with frame f. */ "
  &HASHMINUS  =
  "assign
    hash_cases = hash_cases - x.cases
    hash_pallets = hash_pallets - x.pallets
    y.pallets = y.pallets - x.pallets
    y.cases = y.cases - x.cases."
  }
HIDE FRAME f-det NO-PAUSE.
CLEAR FRAME f-det ALL NO-PAUSE.
END procedure.    /* edit bol */
procedure auto_pall.ip:
FIND FIRST x
  WHERE x.ship-id = y.ship-id
  AND x.combo = "" AND x.cases > 0 NO-LOCK NO-ERROR.
IF NOT AVAIL x THEN
DO:
  MESSAGE "No lines are available to create combination".
  RETURN error.
END.
IF last_combo = "" THEN
last_combo = "A".
ELSE
last_combo = CHR(ASC(last_combo) + 1).
FOR EACH x EXCLUSIVE-LOCK
    WHERE x.ship-id = y.ship-id
    AND x.cases > 0
    AND x.combo = "":
  x.combo = last_combo.
END.
{rc/listadd.i y.combo-list last_combo}.
END.
procedure gen_856.ip:
{ed/edivars.i}
DEF BUFFER dc FOR edshipto.
DEF var v-emp-id LIKE soldto.sold-id NO-UNDO.
DEF var v-emp-recid AS RECID NO-UNDO.
DEF var v-mast-bol-no LIKE oe-bolh.bol-no FORMAT ">>>>>9" NO-UNDO.
DEF var local-debug AS logical NO-UNDO initial FALSE.
DEF var ws_bol_chardate AS char NO-UNDO FORMAT "x(08)".
DEF var ws_userref LIKE eddoc.userref NO-UNDO.
DEF VAR ws_pallet-mark LIKE edshline.pallet-mark NO-UNDO.
DEF VAR ws_carton-mark LIKE edshline.carton-mark NO-UNDO.
DEF VAR NUM_PACKAGES AS INT NO-UNDO.
DEF var package_list AS char NO-UNDO.
DEF var package_type AS char NO-UNDO.
DEF var i AS int NO-UNDO.
local-debug = top-debug.
FOR EACH x
    BREAK BY mbol:
  FIND oe-bolh WHERE RECID(oe-bolh) = x.b-rec NO-LOCK NO-ERROR.
  FIND oe-boll WHERE RECID(oe-boll) = x.l-rec NO-LOCK NO-ERROR.
  FIND itemfg  WHERE RECID(itemfg) = x.i-rec NO-LOCK NO-ERROR.
  ASSIGN
    v-emp-id = oe-bolh.sold-id
    v-mast-bol-no = oe-bolh.master-bol-no.
  IF NOT AVAIL oe-bolh THEN
  RETURN error.
  IF NOT AVAIL oe-boll THEN
  RETURN error.
  FIND FIRST edmast WHERE edmast.cust = oe-bolh.cust-no NO-LOCK NO-ERROR.
  IF NOT AVAIL edmast THEN
  RETURN error.
  FIND FIRST edcode OF edmast
    WHERE edcode.direction = 'o' AND edcode.setid = '856' NO-LOCK NO-ERROR.
  IF NOT AVAIL edcode THEN
  RETURN error.
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
  IF v-emp-id = "" AND AVAIL oe-ord
    THEN
  v-emp-id = oe-ord.sold-id.
  IF AVAIL oe-ord THEN
  DO:
    /* asi does not have these fields */
    DEF var p1 AS char NO-UNDO.
    DEF var p2 AS char NO-UNDO.
    DEF var i1 AS int NO-UNDO.
    DEF var i2 AS int NO-UNDO.
    p1 = oe-ord.po-no.
    p2 = "".
    i1 = 1.
    i2 = 0.
  END.
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
    FIND inv-head OF inv-line NO-LOCK NO-ERROR.
  END.
  ELSE
  ASSIGN location_number = "" ordering_store_number = "".
  FIND FIRST edshipto
    WHERE edshipto.partner = edmast.partner
    AND edshipto.ref-type = "by"
    AND edshipto.cust = edmast.cust
    AND edshipto.ship-to = oe-bolh.ship-id NO-LOCK NO-ERROR.
  IF AVAIL edshipto THEN
  ordering_store_number = edshipto.by-code.
  IF local-debug AND NOT AVAIL edshipto THEN
  DO:
    BELL.
    MESSAGE "Could not locate BY via;" edmast.partner "by" edmast.cust
      oe-bolh.ship-id.
    PAUSE.
  END.
  /* 9706 CAH: v-emp-id overrides st code */
  IF v-emp-id > "" THEN
  location_number = v-emp-id.
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
  IF local-debug AND NOT AVAIL dc THEN
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
    RUN ed/gendoc.p (RECID(edcode), bill_of_lading_number, OUTPUT ws_eddoc_rec).
    FIND eddoc WHERE RECID(eddoc) = ws_eddoc_rec EXCLUSIVE.
    ASSIGN
      /* eddoc.docseq        = INTEGER(location_number) 9806 CAH can fail */
      eddoc.st-code       = location_number
      eddoc.userref       = ws_userref
      /* "B-NO: " + STRING(oe-bolh.b-no) */
      .
    RUN rc/str2int.p (INPUT location_number, OUTPUT eddoc.docseq).
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
      edshtran.bol-adddate = TODAY
      edshtran.bol-addtime = TIME
      edshtran.package-code =
      (IF oe-bolh.tot-pallets > 0 THEN "PLT" ELSE "CTN")
      + "25" /* corrugated cartons */.
    .
    DO i = 1 TO 4:
      edshtran.routing[i] = oe-bolh.ship-i[i].
    END.
  END.
  ELSE
  IF FIRST-OF (x.mbol) THEN
  DO:
    FOR EACH edshline OF edshtran:
      DELETE edshline.
    END.
    FOR EACH edshtare OF edshtran:
      DELETE edshtare.
    END.
    FOR EACH edshpack OF edshtran:
      DELETE edshpack.
    END.
    FOR EACH edshord OF edshtran:
      DELETE edshord.
    END.
    ASSIGN edshtran.tot-cartons = 0
      edshtran.tot-volume  = 0
      edshtran.tot-wght    = 0
      .
  END.
  NUM_PACKAGES = x.pallets + x.cases.
  package_list = FILL("P", x.pallets) + FILL("C", x.cases).
  REPEAT i = 1 TO num_packages:
    ASSIGN ws_carton-mark = ? ws_pallet-mark = ?.
    package_type = substring(package_list,i,1).
    RUN gen_scc18.ip (INPUT package_type, OUTPUT ws_carton-mark).
    /* pallet mark is an integer field and this causes overflow ...
    if package_type = "P"
    then assign ws_pallet-mark = integer(ws_carton-mark)
    ws_carton-mark = ?.
    */
    FIND FIRST edshline
      WHERE edshline.partner = edshtran.partner
      AND edshline.seq = edshtran.seq
      AND edshline.bol-no = edshtran.bol-no
      AND edshline.st-code = edshtran.st-code
      AND edshline.cust-po = oe-ord.po-no
      AND edshline.cust-po-line = STRING(oe-ordl.line)
      AND edshline.pallet-mark = ws_pallet-mark /* edivline.pallet-mark */
      AND edshline.carton-mark = ws_carton-mark /* edivline.carton-mark */
      AND edshline.by-code = ordering_store_number /* edshipto.by-code */
      EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL edshline THEN
    DO:
      FIND itemfg
        WHERE itemfg.company = oe-boll.company
        AND itemfg.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
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
        edshline.upc = IF AVAIL itemfg THEN itemfg.upc ELSE ""
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
      IF edshline.tot-cartons > 0 THEN
      round(edshline.tot-wght / edshline.tot-cartons, 0)
      ELSE 0
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
        IF AVAIL inv-head THEN inv-head.inv-date ELSE ?
        edshord.cust-dept =
        IF AVAIL edpotran THEN edpotran.cust-dept ELSE ""
        edshord.cust-div          =
        IF AVAIL edpotran THEN edpotran.cust-div ELSE
        IF AVAIL itemfg   THEN itemfg.procat ELSE ""
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
  END.  /* repeat for num packages */
END.    /* each x */
END procedure.
procedure gen_scc18.ip:
/* given packaging type, return next serial number */
DEF INPUT  PARAM p_type AS char NO-UNDO. /* P=Pallet, C=Carton/case */
DEF OUTPUT PARAM p_code AS char NO-UNDO.
/* local variables */
DEF var ws_app_ident    AS char NO-UNDO FORMAT "x(02)" initial "00".
/* UCC 128 SCC */
DEF var ws_package_type AS char NO-UNDO FORMAT "x(01)" initial "0".
/*  0=case/carton/hanging container
1=pallet
2=larger than pallet, e.g. container load or truckload
3=Undefined
4=Ship to defined
5-9=Shipper specific */
DEF var ws_ucc_code     AS char NO-UNDO  FORMAT "x(07)"     initial "0000000".
DEF var ws_scc_code     AS int  NO-UNDO  FORMAT "999999999" initial 0.
DEF var ws_checkdigit   AS int  NO-UNDO  FORMAT "9"         initial 0.
p_code = ?.
IF NOT CAN-DO("P*,C*", p_type) THEN
p_type = "C". /* assume carton */
ELSE
IF length(p_type) > 1 THEN
p_type = substring(p_type,1,1).
CASE p_type:
WHEN "C" THEN
ws_package_type = "0".
WHEN "P" THEN
ws_package_type = "1".
OTHERWISE     ws_package_type = "0".
END CASE.
RUN rc/regkey.ip ("UCC/Mfr-ID", OUTPUT ws_ucc_code).
RUN rc/regkey.ip ("UCC/SCC_serial", OUTPUT ws_char).
ws_scc_code = integer(ws_char) + 1.
ws_char = string(ws_scc_code).
RUN rc/regsave.ip (ws_company, "UCC/SCC_serial", ws_char).
ws_char = ws_app_ident
+ ws_package_type
+ ws_ucc_code
+ string(ws_scc_code,"999999999").
RUN rc/cknum10.p (ws_char, "", OUTPUT p_code, OUTPUT ws_checkdigit).
IF TRUE OR top-debug THEN
RUN rc/debugmsg.p ("code in: " + ws_char +
  " code out " + p_code).
END procedure.
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
      PROGRAM-NAME(2).
    UPDATE
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
END procedure.
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
END procedure.
*/
