/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\I8503
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF SHARED STREAM s-in.
DEF SHARED STREAM s-out.
{ed/sharedv.i}
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "SHARED"}
{rc/stats.i         "SHARED"}
{rc/stringv.i       "shared"}
{rc/ercvars.i 60 " " " "}
{rc/datev.i}
{rc/fcurrent.i}
/* local variable declarations */
DEF var ws_customer                     AS char NO-UNDO.
DEF VAR n AS INT NO-UNDO FORMAT "9" INITIAL 1.
DEF VAR item_offset AS INTEGER NO-UNDO.
DEF var first_time AS logical initial TRUE NO-UNDO.
DEF var needs_header AS logical initial TRUE NO-UNDO.
DEF var needs_detail AS logical initial FALSE NO-UNDO.
DEF var has_shipto_address AS logical NO-UNDO initial FALSE.
DEF var note_array AS char NO-UNDO extent 9.    /* 9810 CAH */
DEF var i AS int NO-UNDO.
FIND edcode WHERE RECID(edcode) = ws_edcode_rec NO-LOCK.
FIND edmast WHERE RECID(edmast) = ws_edmast_rec NO-LOCK.
/* Assign the sequence number of the lowest detail record */
RUN set_break_rec.ip.
ws_docid = string(header_isa,"999999999")
+ string(header_gs, "999999999")
+ string(header_st, "999999999").
VIEW FRAME f-current.
/* parse input file until 000 segment is encountered or end of file */
_outer:
REPEAT:
  ASSIGN
    {rc/substr.i ws_segment       07  3}
    {rc/substr.i ws_rec_code      10  3 integer}
    {rc/substr.i ws_char          13  5}
    .
  IF top-debug THEN
  DO:
    RUN rc/debugmsg.p ("ws_rec_code=" + string(ws_rec_code,"zz9")
      + " Last_rec_code=" + string(last_rec_code,"zz9")
      + " str_buffa=" + str_buffa).
  END.
  DISPLAY
    ws_partner
    ws_setid
    ws_segment      COLUMN-LABEL "Sgmt"
    ws_rec_code     COLUMN-LABEL "Seq"
    str_buffa       COLUMN-LABEL "Raw data"
    FORMAT "x(40)" WITH FRAME f-current.
  IF ws_rec_code <= dtl_rec_code AND needs_detail AND NOT needs_header THEN
  RUN create_detail.ip.
  /* put override segment processing here */
  IF ws_segment = "000" THEN
  DO:
    ASSIGN
      needs_detail = FALSE
      needs_header = TRUE
      note_sequence_number = 0
      note_array = ""
      .
    RUN set_break_rec.ip.
    IF first_time THEN
    DO:
      first_time = FALSE.
    END.
    ELSE    /* we have encountered a new transaction set in the TDF
    which might not be the type this program parses */
    LEAVE _outer.
  END.  /* 000 */
  ELSE
  DO:
    run parse_segment.ip.
  END.
  IF WS_REC_CODE >= DTL_REC_CODE AND LAST_REC_CODE < DTL_REC_CODE
    AND needs_header THEN
  DO:
    ws_docid = purchase_order_number.
    RUN create_header.ip.
  END.
  IF ws_segment = "NTE" THEN
  DO:    /* 9810 CAH: Save in array */
    IF note_sequence_number < 9 THEN
    DO:
      ASSIGN
        note_sequence_number = note_sequence_number + 1
        note_array[note_sequence_number] = note.
    END.
    ELSE
    RUN rc/debugmsg.p ("Note sequence array overflow").
  END.
  IF ws_segment = "MSG" THEN
  DO:    /* 9903 CAH: In prep for 4010 MSG replaces NTE */
    IF note_sequence_number < 9 THEN
    DO:
      ASSIGN
        note_sequence_number = note_sequence_number + 1
        note_array[note_sequence_number] = message_text.
    END.
    ELSE
    RUN rc/debugmsg.p ("Note sequence array overflow").
  END.
  IF ws_segment = "N4" THEN
  DO:
    IF entity_id = "ST" THEN
    ASSIGN
      ordering_store_number = shipto_store_number   /* 9810 CAH */
      shipto_address1 = address1
      shipto_address2 = address2
      shipto_city     = city
      shipto_state    = state
      shipto_zip      = zip
      address1 = ''
      address2 = ''
      city = ''
      state = ''
      zip = ''
      has_shipto_address = TRUE.
  END.
  IF ws_segment = "PO1" THEN
  needs_detail = TRUE.
  /* partner specific touchups at the segment level */
  CASE ws_partner:
  WHEN "sears" THEN
  DO:
  END.
  WHEN "FEDER" THEN
  DO:             /* they do not sent a by segment */
    IF ws_segment = "N1" AND entity_id = "ST"
      THEN
    ASSIGN ordering_store_number = shipto_store_number.
  END.
END CASE.
IMPORT STREAM s-in str_buffa.           /* get the next TDF record */
{rc/incr.i ws_recs_read}.
last_rec_code = ws_rec_code.            /* save break record */
END.    /* _outer repeat */
/* when we fall out of the repeat,
test to see if the final record values are available */
IF needs_detail THEN
RUN create_detail.ip.
IF ws_segment <> "000" THEN
tdf_eof = TRUE.
HIDE FRAME f-current NO-PAUSE.
IF top-debug THEN
DO:
  RUN rc/debugmsg.p ("at return, ws_segment=" + ws_segment + " tdf eof="
    + string(tdf_eof) ).
END.
RETURN.
procedure create_header.ip:
_doc_header:
DO:
  FIND FIRST eddoc
    WHERE eddoc.partner = ws_partner
    AND eddoc.setid = ws_setid
    AND eddoc.docid = ws_docid
    NO-ERROR.
  IF NOT AVAIL eddoc THEN
  DO:
    FIND edmast
      WHERE RECID(edmast) = ws_edmast_rec EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL edmast THEN
    DO:
      {rc/listadd.i erclist 201}
      erctoken[1] = ws_partner.
      LEAVE _doc_header.
    END.
    ASSIGN
      ws_customer = edmast.cust
      location_number = "".
    FIND edcode WHERE RECID(edcode) = ws_edcode_rec EXCLUSIVE-LOCK
      NO-ERROR.
    IF NOT AVAIL edcode THEN
    DO:
      {rc/listadd.i erclist 301}
      erctoken[3] = ws_setid.
      LEAVE _doc_header.
    END.
    RUN ed/gendoc.p (RECID(edcode), ws_docid, OUTPUT ws_eddoc_rec).
    /* creates eddoc, assigns opening values */
    FIND eddoc WHERE RECID(eddoc) = ws_eddoc_rec EXCLUSIVE.
    ASSIGN
      eddoc.docseq        = INTEGER(location_number)
      eddoc.st-code       = shipto_store_number
      eddoc.status-flag   = "RCV"
      eddoc.isa           = header_isa
      eddoc.gs            = header_gs
      eddoc.st            = header_st
      eddoc.fgsender      = header_partner
      eddoc.setid         = header_setid
      eddoc.version       = header_std-ver
      eddoc.userref       = header_int-cd
      eddoc.fgrecvid      = header_partner
      eddoc.fgid          = "PO"
      .
    IF top-debug THEN
    DO:
      RUN rc/debugmsg.p ("after create eddoc").
      DISPLAY STREAM s-out eddoc
        WITH FRAME f-eddoc 3 COLUMNS width 144.
    END.
    {rc/incr.i ws_amt_added}.
  END.
  ELSE
  IF AVAIL eddoc THEN
  DO:
    PUT STREAM s-out UNFORMATTED SKIP
      "Duplicate document detected for partner: " eddoc.partner
      " DocID: " eddoc.docid.
    IF eddoc.stat > 0 THEN
    DO:
      /* duplicate ??? */
      PUT STREAM s-out UNFORMATTED
        " has already been processed - duplicate skipped" SKIP.
      {rc/listadd.i erclist 1002}
      erctoken[7] = ws_docid.
      {rc/incr.i ws_amt_inerror}.
      skip_doc = TRUE.
    END.
    ELSE
    DO:
      PUT STREAM s-out UNFORMATTED
        " not yet processed - duplicate deleted" SKIP.
      ws_char = string(eddoc.seq).
      next_program = "ed/fm" + ws_setid + "del.p".
      IF SEARCH(next_program) <> ? THEN
      RUN VALUE(next_program) (RECID(eddoc)).
      {rc/incr.i ws_amt_changed}.
    END.
  END.
  ws_eddoc_rec = RECID(eddoc).
  FIND edpotran
    WHERE edpotran.partner = eddoc.partner
    AND edpotran.seq = eddoc.seq EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edpotran THEN
  DO:
    FIND edmast WHERE RECID(edmast) = ws_edmast_rec NO-LOCK NO-ERROR.
    FIND edco   WHERE edco.company = ws_company NO-LOCK NO-ERROR.
    CREATE edpotran.
    ASSIGN
      edpotran.partner    = eddoc.partner
      edpotran.seq        = eddoc.seq.
  END.
  ASSIGN
    edpotran.cust    = ws_customer
    edpotran.cust-po = purchase_order_number
    edpotran.cust-dept = department_number
    edpotran.scheduled-code1 = scheduled_code_1
    edpotran.ship-method-code = transportation_method_code
    edpotran.routing[1] = routing
    edpotran.vn-code = vendor_number
    /* edpotran.zz-code = afe_number 9810 CAH: Not in Schema */
    edpotran.cust-div = sales_division /* 9704 CAH */
    ws_edpotran_rec = RECID(edpotran)
    {rc/incr.i ws_recs_added}
    edpotran.sf-code =
    IF edmast.sf-code > "" THEN edmast.sf-code ELSE edco.sf-code
    .
  /* 9810 CAH: for Sears AFE number ... */
  IF afe_number > ""
    THEN
  ASSIGN edpotran.ref2-code = "AE" edpotran.ref2 = afe_number.
  /* 970515 CAH: Patch if only given ST, not BY */
  if top-debug then run rc/debugmsg.p 
  ("(a) shipto_store_number: " + shipto_store_number 
 + " ordering_store_number: " + ordering_store_number). 
  IF shipto_store_number <= ""
    AND ordering_store_number > ""
    THEN
  shipto_store_number = ordering_store_number.
  IF ordering_store_number <= ""
    AND shipto_store_number > ""
    THEN
  ordering_store_number = shipto_store_number.
  if top-debug then run rc/debugmsg.p 
  ("(b) shipto_store_number: " + shipto_store_number 
 + " ordering_store_number: " + ordering_store_number). 
 
  RUN ed/fixidin.p (RECID(edmast), shipto_store_number,
    OUTPUT edpotran.st-code).
  RUN ed/fixidin.p (RECID(edmast), ordering_store_number,
    OUTPUT edpotran.by-code).
  /*  this is done automatically in DTM procedure ...
  {rc/xyymmdd.i purchase_order_date edpotran.order-date}
  {rc/xyymmdd.i cancel_date edpotran.cancel-date}
  {rc/xyymmdd.i ship_date   edpotran.request-date}
  */
  ASSIGN
    edpotran.order-date         = purchase_order_date#
    edpotran.cancel-date        = cancel_date#
    edpotran.cancel-date-code   = cancel_date_qualifier
    edpotran.request-date       = ship_date#
    edpotran.ship-date-code    = ship_date_qualifier
    /* DTM procedure saves these qualifiers ...
    cancel-date-code = IF cancel-date <> ? THEN "001" ELSE ""
    ship-date-code   = IF request-date <> ? THEN "010" ELSE ""
    */
    eddoc.st-code = edpotran.st-code    /* 9809 CAH */
    ordering_store_number = edpotran.by-code
    shipto_store_number = edpotran.st-code
    .
  /* 9810 CAH per federated */
  IF has_shipto_address THEN
  ASSIGN
    edpotran.ship-name = shipto_name
    edpotran.ship-address[1] = shipto_address1
    edpotran.ship-address[2] = shipto_address2
    edpotran.ship-address[3] = contact_name
    edpotran.ship-city = shipto_city
    edpotran.ship-st = shipto_state
    edpotran.ship-zip = shipto_zip
    shipto_name = ''
    shipto_address1 = ''
    shipto_address2 = ''
    contact_name = ''
    shipto_city = ''
    shipto_state = ''
    shipto_zip = ''
    has_shipto_address = FALSE
    .
  IF top-debug THEN
  RUN rc/debugrec.p
    ("Created header", RECID(edpotran)) "edpotran".
  IF note_sequence_number > 0
    AND note_array[1] > "" THEN
  DO:
    FIND FIRST edpoaddon OF edpotran
      WHERE edpoaddon.order-line = 0 NO-ERROR.
    IF NOT AVAIL edpoaddon THEN
    DO:
      FIND LAST edpoaddon OF edpotran NO-LOCK NO-ERROR.
      ws_int = IF AVAIL edpoaddon THEN
      edpoaddon.line ELSE
      0.
      CREATE edpoaddon.
      ASSIGN
        edpoaddon.partner = edpotran.partner
        edpoaddon.seq = edpotran.seq
        edpoaddon.line = ws_int + 1
        edpoaddon.order-line = 0.
    END.
    DO i = 1 TO note_sequence_number:
      ASSIGN edpoaddon.note[i] = note_array[i].
    END.
    RELEASE edpoaddon.
  END.
  
  assign 
    note_sequence_number = 0
    note_array = "".
  RELEASE edpotran.
  needs_header = FALSE.
END.    /* doc header do */
END procedure.
procedure create_detail.ip:
{rc/incr.i ws_recs_selected}.
FIND edpotran WHERE RECID(edpotran) = ws_edpotran_rec EXCLUSIVE NO-ERROR.
IF NOT AVAIL edpotran THEN
DO:
  RUN rc/debugmsg.p
    ("edpotran find failed in " + PROGRAM-NAME(2)).
  RETURN error.
END.
IF NOT AVAIL edpoline THEN
DO:
  CREATE edpoline.
  ASSIGN
    edpoline.partner = edpotran.partner
    edpoline.seq = edpotran.seq
    edpotran.lines = edpotran.lines + 1
    edpotran.last-line = edpotran.last-line + 1
    edpoline.line = edpotran.last-line
    edpoline.st-code = edpotran.st-code
    edpoline.by-code = ordering_store_number
    {rc/incr.i ws_recs_added}.
END.
ASSIGN
  edpoline.cust-po-line =
  IF customer_line_number > 0
  THEN STRING(customer_line_number)
  ELSE STRING(edpoline.line)
  edpoline.cust-item-no = customer_item_number
  edpoline.qty-orig-ord   = quantity_ordered
  edpoline.uom-code = unit_of_measure
  edpoline.unit-price = unit_price
  ws_edpoline_rec = RECID(edpoline)
  edpoline.description[1] = item_description
  .
  /* 9903 CAH: Added this code to support line item level notes */
  IF note_sequence_number > 0
    AND note_array[1] > "" THEN
  DO:
    FIND FIRST edpoaddon OF edpotran
      WHERE edpoaddon.order-line = integer(edpoline.cust-po-line) NO-ERROR.
    IF NOT AVAIL edpoaddon THEN
    DO:
      FIND LAST edpoaddon OF edpotran NO-LOCK NO-ERROR.
      ws_int = IF AVAIL edpoaddon THEN
      edpoaddon.line ELSE
      0.
      CREATE edpoaddon.
      ASSIGN
        edpoaddon.partner = edpotran.partner
        edpoaddon.seq = edpotran.seq
        edpoaddon.line = ws_int + 1
        edpoaddon.order-line = integer(edpoline.cust-po-line).
    END.
    DO i = 1 TO note_sequence_number:
      ASSIGN edpoaddon.note[i] = note_array[i].
    END.
    RELEASE edpoaddon.
  END.
IF top-debug THEN
RUN rc/debugrec.p ("Created line", RECID(edpoline)) "edpoline".
IF NOT NEW (edpoline) THEN
{rc/incr.i ws_recs_changed}.
RELEASE edpoline.
/* reset the edi variables */
ASSIGN
  shipto_store_number = ""
  company_name = ""
  address1 = ""
  address2 = ""
  city = ""
  state = ""
  zip = ""
  country = ""
  contact_name = ""
  contact_phone_number = ""
  contact_fax_number = ""
  delivery_zone = ""
  needs_detail = FALSE
  afe_number = ""       /* 9810 CAH */
  note_sequence_number = 0
  note_array = ""
  .
END procedure.
procedure set_break_rec.ip:
IF header_std-ver matches "*VICS*"
  THEN
dtl_rec_code = 21.
ELSE
dtl_rec_code = 70.
IF top-debug THEN
RUN rc/debugmsg.p ("Set break dtl_rec_code to: "
  + string(dtl_rec_code) + " std-ver " + header_std-ver).
END procedure.
procedure parse_segment.ip:
next_program = "ed/tdf/" + lc(ws_segment) + ".p".
IF SEARCH(next_program) <> ? THEN
DO:
DEF var sub_cmd AS char NO-UNDO initial "I".
/* 9904 CAH enhanced this */
  error-status:error = false.
  RUN VALUE(next_program) (sub_cmd, INPUT-OUTPUT str_buffa, OUTPUT ws_erc)
    no-error.
  if error-status:error then do:
    run rc/debugmsg.p ("After return from " + next_program
        + " Error-status is true, error-number is "
        + string(error-status:get-number(1)) ).
  end.
END.
ELSE
DO:
  RUN rc/debugmsg.p ("Unrecognized segment: " + ws_segment).
  {rc/incr.i ws_recs_inerror}.
END.
END procedure.
