/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\i8503060.p
**       By: Chuck Palenik (c) 1998 Report Concepts, Inc. ALL Rights Reserved
** Descript: Import Sterling TDF format 850 v. 3060
05.20.99 by CAH on \\ricky\robj8 Log#0000:
1.  Initial 4010 release from 860.3060.
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
DEF var needs_detail AS logical initial FALSE NO-UNDO.
FIND edcode WHERE RECID(edcode) = ws_edcode_rec NO-LOCK.
FIND edmast WHERE RECID(edmast) = ws_edmast_rec NO-LOCK.
/* Assign the sequence number of the lowest detail record */
dtl_rec_code = 77. /* X ANSI version, Note that VICS=21. */
ws_docid = string(header_isa,"999999999")
+ string(header_gs, "999999999")
+ string(header_st, "999999999").
pause 0.
VIEW FRAME f-current.
VIEW FRAME f-stats.
/* parse input file until 000 segment is encountered or end of file */
_outer:
REPEAT:
    if top-debug then run rc/debugmsg.p ("top of _outer repeat block: "
        + str_buffa).
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
  IF ws_rec_code <= dtl_rec_code AND needs_detail THEN
  RUN create_detail.ip.
  /* put override segment processing here */
  IF ws_segment = "000" THEN
  DO:
    needs_detail = FALSE.
    IF first_time THEN
    DO:
      first_time = FALSE.
    END.
    ELSE    /* we have encountered a new transaction set in the TDF
    which might not be the type this program parses */
    LEAVE _outer.
  END.  /* 000 */
  ELSE
  IF ws_segment = "BCH" THEN
  DO:
    RUN parse_segment.ip.
    ws_docid = purchase_order_number
        + (if release_number > "" then "-" + release_number else "")
        + ("-" + extra_date).
    RUN create_header.ip.
  END.
  ELSE
  IF ws_segment = "POC" THEN
  DO:
    RUN parse_segment.ip.
    needs_detail = TRUE.
  END.
  ELSE
  IF ws_segment = "N4" THEN
  DO:
    IF entity_id = "ST" THEN
    ASSIGN
      shipto_address1 = address1
      shipto_address2 = address2
      shipto_city     = city
      shipto_state    = state
      shipto_zip      = zip
      .
  END.
  ELSE
  DO:
    RUN parse_segment.ip.
  END.
  /* partner specific touchups at the segment level */
  CASE ws_partner:
  WHEN "sears" THEN
  DO:
  END.
END CASE.
IMPORT STREAM s-in str_buffa.           /* get the next TDF record */
{rc/incr.i ws_recs_read}.
last_rec_code = ws_rec_code.            /* save break record */
{rc/statsdis.i}
    if top-debug then run rc/debugmsg.p ("bottom of _outer repeat block: "
        + str_buffa).
END.    /* _outer repeat */
/* when we fall out of the repeat,
test to see if the final record values are available */
IF needs_detail THEN
RUN create_detail.ip.
IF ws_segment <> "000" THEN
tdf_eof = TRUE.
/* VIEW STREAM s-out FRAME f-stats. */
HIDE FRAME f-stats NO-PAUSE.
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
      eddoc.st-code       = ""
      eddoc.status-flag   = "RCV"
      eddoc.isa           = header_isa
      eddoc.gs            = header_gs
      eddoc.st            = header_st
      eddoc.fgsender      = header_partner
      eddoc.setid         = header_setid
      eddoc.version       = header_std-ver
      eddoc.userref       = header_int-cd
      eddoc.fgrecvid      = header_partner
      eddoc.fgid          = "PC"
      .
    IF top-debug THEN
    DO:
      RUN rc/debugmsg.p ("after create eddoc").
      DISPLAY STREAM s-out eddoc
        WITH FRAME f-eddoc 3 COLUMNS width 144.
    END.
    /* 9905 CAH: Added: */
    if ws_print-opt then 
    PUT STREAM s-out UNFORMATTED SKIP
      "+++ Created document for partner: " eddoc.partner
      " Seq# " eddoc.seq
      " DocID: " eddoc.docid format 'x(20)'.    
    {rc/incr.i ws_amt_added}.
  END.
  ELSE
  IF AVAIL eddoc THEN
  DO:
    PUT STREAM s-out UNFORMATTED SKIP
      "--- Duplicate document detected for partner: " eddoc.partner
      " Seq# " eddoc.seq
      " DocID: " eddoc.docid format 'x(20)'.    
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
    FIND edco   WHERE recid(edco)   = ws_edco_rec NO-LOCK NO-ERROR.
    CREATE edpotran.
    ASSIGN
      edpotran.partner    = eddoc.partner
      edpotran.seq        = eddoc.seq.
  END.
  ASSIGN
    /* BCH */
    edpotran.purpose-code   = transaction_purpose_code
    edpotran.order-type     = transaction_type_code
    edpotran.misc-date1     = purchase_order_date#
    edpotran.routing[1]     = "Original PO Date: " + 
        (if edpotran.misc-date1 <> ? 
        then string(edpotran.misc-date1,"99/99/9999")
        else "unknown")
    edpotran.cust-po        = purchase_order_number
    edpotran.cust           = ws_customer
    /* REF */
    edpotran.cust-dept      = department_number
    edpotran.vn-code        = vendor_number
    edpotran.cust-div       = sales_division /* 9704 CAH */
    ws_edpotran_rec         = RECID(edpotran)
    {rc/incr.i ws_recs_added}
    edpotran.sf-code =
        (IF edmast.sf-code > "" THEN edmast.sf-code ELSE 
        if avail edco then edco.sf-code
        else "")
    .
    /* PER */
    if contact_function_code > "" then assign
        edpotran.routing[2] = "Contact: " + contact_name + "/" + contact_phone_number.
  /* 970515 CAH: Patch if only given ST, not BY */
  IF shipto_store_number <= ""
    AND ordering_store_number > ""
    THEN
  shipto_store_number = ordering_store_number.
  IF ordering_store_number <= ""
    AND shipto_store_number > ""
    THEN
  ordering_store_number = shipto_store_number.
  RUN ed/fixidin.p (RECID(edmast), shipto_store_number,
    OUTPUT edpotran.st-code).
  RUN ed/fixidin.p (RECID(edmast), ordering_store_number,
    OUTPUT edpotran.by-code).
  {rc/xyymmdd.i extra_date edpotran.order-date}
  {rc/xyymmdd.i cancel_date edpotran.cancel-date}
  {rc/xyymmdd.i ship_date   edpotran.request-date}
  ASSIGN
    cancel-date-code = IF cancel-date <> ? THEN "001" ELSE ""
    ship-date-code   = IF request-date <> ? THEN "010" ELSE ""
    .
  if top-debug then DISPLAY STREAM s-out edpotran WITH FRAME f-tran
    3 COLUMNS width 144.
  RELEASE edpotran.
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
    {rc/incr.i ws_recs_added}.
END.
ASSIGN
  edpoline.cust-po-line =
  IF customer_line_number > 0
  THEN STRING(customer_line_number)
  ELSE STRING(edpoline.line)
  edpoline.cust-item-no = customer_item_number
  edpoline.uom-code = unit_of_measure
  edpoline.unit-price = unit_price
  ws_edpoline_rec = RECID(edpoline)
  edpoline.description[1] = item_description.
  
  CASE change_indicator:
  when "AI" then do:
    edpoline.description[2] = "ADD ADDITIONAL ITEM(S)".
  END.
  WHEN "CA" THEN DO:
    edpoline.description[2] = "CHANGE TO LINE ITEMS".
  END.
  WHEN "CT" THEN DO:
    edpoline.description[2] = "CHANGE OF DATES (SEE HEADER)".
  END.
  WHEN "DI" THEN DO:
    edpoline.description[2] = "ORDER LINE ITEM DELETED".
  END.
  WHEN "PC" THEN DO:
    edpoline.description[2] = "PRICE CHANGE TO "
        + string(unit_price) 
        + (if price_basis > " " then " Per " + price_basis else " ").
  END.
  WHEN "QD" THEN DO:
    edpoline.description[2] = "QUANTITY DECREASE BY "
        + STRING(order_change_quantity) + " TO " + STRING(quantity_cumulative).
    edpoline.qty-change = order_change_quantity.    
  END.
  WHEN "QI" THEN DO:
    edpoline.description[2] = "QUANTITY INCREASE BY "
        + STRING(order_change_quantity) + " TO " + STRING(quantity_cumulative).
  END.
  OTHERWISE DO:
    RUN rc/debugmsg.p ("Unrecognized change indicator: " + change_indicator).
  END.
  END CASE.
  
  if edpoline.description[1] = ""
  then assign edpoline.description[1] = edpoline.description[2]
  edpoline.description[2] = "".
if top-debug then do:
    DISPLAY STREAM s-out edpoline
  WITH FRAME f-det width 144 3 COLUMNS.
DOWN STREAM s-out WITH FRAME f-det.
end.
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
  customer_item_number = ""
  unit_of_measure = ""
  unit_price = 0
  ws_edpoline_rec = RECID(edpoline)
  item_description = ""
  .
END procedure.
procedure parse_segment.ip:
next_program = "ed/tdf/" + lc(ws_segment) + ".p".
IF SEARCH(next_program) <> ? THEN
DO:
  DEF var sub_cmd AS char NO-UNDO initial "I".
  if ws_print-opt = true then sub_cmd = "IP".
  /* 9904 CAH enhanced this */
  error-status:error = FALSE.
  RUN VALUE(next_program) (sub_cmd, INPUT-OUTPUT str_buffa, OUTPUT ws_erc)
    NO-ERROR.
  IF error-status:error THEN
  DO:
    RUN rc/debugmsg.p ("After return from " + next_program
      + " Error-status is true, error-number is "
      + string(error-status:get-number(1)) ).
  END.
END.
ELSE
DO:
  RUN rc/debugmsg.p ("Unrecognized segment: " + ws_segment).
  {rc/incr.i ws_recs_inerror}.
END.
END procedure.
