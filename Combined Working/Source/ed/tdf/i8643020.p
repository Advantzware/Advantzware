/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\i864
**       By: Chris Heins, Report Concepts, Inc. (c) 1998 All Rights Reserved.
** Descript: Process 864 Text message into Shipto Addresses.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i       "shared"}
DEF var ws_customer                     AS char NO-UNDO.
DEF var store_opening_date              AS date NO-UNDO.
{rc/datev.i}
{rc/fcurrent.i}
{rc/stats.i         "SHARED"}
{ed/tdf/sharedv.i   "SHARED"}
DEF var first_time AS logical initial TRUE NO-UNDO.
DEF var needs_detail AS logical NO-UNDO initial FALSE.
DEF SHARED STREAM s-in.
DEF SHARED STREAM s-out.
{rc/stringv.i       "shared"}
{rc/ercvars.i 60 " " " "}
FIND edcode WHERE RECID(edcode) = ws_edcode_rec EXCLUSIVE-LOCK.
FIND edmast WHERE RECID(edmast) = ws_edmast_rec EXCLUSIVE-LOCK.
/* Assign the sequence number of the lowest detail record */
dtl_rec_code = 6.
ws_docid = string(header_isa,"999999999")
+ string(header_gs, "999999999")
+ string(header_st, "999999999").
PAUSE 0.    /* 9809 cah */
VIEW FRAME f-current.
VIEW FRAME f-stats.
/* parse input file until 000 segment is encountered or end of file */
_outer:
REPEAT:
  ASSIGN
    {rc/substr.i ws_segment      7  3}
    {rc/substr.i ws_rec_code    10  3 integer}
    {rc/substr.i ws_char    13  5}
    .
  IF TRUE OR top-debug THEN
  DO:
    RUN rc/debugmsg.p ("ws_rec_code=" + string(ws_rec_code,"99")
      + " Last_rec_code=" + string(last_rec_code,"99")
      + " str_buffa=" + str_buffa).
  END.
  DISPLAY
    ws_partner
    ws_setid
    ws_segment      COLUMN-LABEL "Sgmt"
    ws_rec_code     COLUMN-LABEL "Seq"
    str_buffa       COLUMN-LABEL "Raw data"
    FORMAT "x(40)" WITH FRAME f-current.
  IF ws_rec_code <= dtl_rec_code THEN
  DO:
    IF needs_detail THEN
    RUN create_detail.ip.
  END.
  /* put override segment processing here */
  IF ws_segment = "000" THEN
  DO:
    IF first_time THEN
    DO:
      RUN create_header.ip.
      first_time = FALSE.
    END.
    ELSE    /* we have encountered a new transaction set in the TDF
    which might not be the type this program parses */
    LEAVE _outer.
  END.  /* 000 */
  ELSE
  IF ws_segment = "MIT" THEN
  DO:
  END.    /* MIT */
  ELSE
  DO:
    next_program = "ed/tdf/" + lc(ws_segment) + ".p".
    IF SEARCH(next_program) <> ? THEN
    DO:
      RUN VALUE(next_program) ("I", INPUT-OUTPUT str_buffa, OUTPUT ws_erc).
    END.
    ELSE
    DO:
      RUN rc/debugmsg.p ("Unrecognized segment: " + ws_segment).
      {rc/incr.i ws_recs_inerror}.
    END.
  END.
  IF ws_segment = "N1" THEN
  needs_detail = TRUE.
  /* partner specific touchups at the segment level */
  CASE ws_partner:
  WHEN "sears" THEN
  DO:
    IF ws_segment = "REF" AND ref_qual = "ST" THEN
    DO:
      /* sears puts the store opening date in YYMMDD format
      in the ref_desc */
      {rc/xyymmdd.i ref_desc store_opening_date}.
      shipto_store_number = "".
    END.
  END.
END CASE.
IMPORT STREAM s-in str_buffa.           /* get the next TDF record */
{rc/incr.i ws_recs_read}.
last_rec_code = ws_rec_code.            /* save break record */
{rc/statsdis.i}
END.    /* _outer repeat */
/* when we fall out of the repeat,
test to see if the final record values are available */
IF id_code > "" THEN
RUN create_detail.ip.
IF ws_segment <> "000" THEN
tdf_eof = TRUE.
VIEW STREAM s-out FRAME f-stats.
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
      RUN ed/fm864del.p (RECID(eddoc)).
      {rc/incr.i ws_amt_changed}.
    END.
  END.
  ws_eddoc_rec = RECID(eddoc).
END.
END procedure.
procedure create_detail.ip:
{rc/incr.i ws_recs_selected}.
FIND eddoc WHERE RECID(eddoc) = ws_eddoc_rec EXCLUSIVE.
IF NOT AVAIL eddoc THEN
DO:
  RUN rc/debugmsg.p ("EDDoc not available in create detail").
  LEAVE.
END.
/* remap the code for sears ... */
IF id_code_qualifier = "92" THEN
id_code_qualifier = "BY".
IF store_opening_date = ?
  THEN
store_opening_date = extra_date#.
FIND edshipto
  WHERE edshipto.partner = ws_partner
  AND edshipto.ref-type = id_code_qualifier
  AND edshipto.by-code = id_code EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL edshipto THEN
DO:
  CREATE edshipto.
  ASSIGN
    edshipto.partner = ws_partner
    edshipto.ref-type = id_code_qualifier
    edshipto.by-code = id_code
    {rc/incr.i ws_recs_added}.
END.
ASSIGN
  edshipto.St-code     = shipto_store_number
  edshipto.Cust        = ws_customer
  edshipto.Name        = company_Name
  edshipto.Addr1       = Address1
  edshipto.Addr2       = Address2
  edshipto.City        = City
  edshipto.State       = State
  edshipto.Zip         = Zip
  edshipto.Country     = Country
  edshipto.Attention   = contact_name
  edshipto.Phone       = contact_phone_number
  edshipto.Fax         = contact_Fax_number
  edshipto.Opened      = store_opening_date
  edshipto.Description = string(eddoc.seq)
  edshipto.Comments[1] = ""
  edshipto.Comments[2] = ""
  edshipto.Comments[3] = ""
  edshipto.Comments[4] = ""
  edshipto.Comments[5] = ""
  edshipto.Ship-to     = ""
  edshipto.Dest-Zone   = delivery_zone
  edshipto.Cust-Region = ""
  .
DISPLAY STREAM s-out
  edshipto.partner            COLON 11
  EDShipto.Ref-type             COLON 11
  EDShipto.Name               COLON 41
  EDShipto.Description    COLON 91
  EDShipto.By-code              COLON 11
  EDShipto.Addr1              COLON 41
  EDShipto.Comments[1]    AT 81 NO-LABEL
  EDShipto.St-code              COLON 11
  EDShipto.Addr2              COLON 41
  EDShipto.Comments[2]    AT 81 NO-LABEL
  EDShipto.Cust                 COLON 11
  EDShipto.City               COLON 41
  EDShipto.State              NO-LABEL
  EDShipto.Zip                NO-LABEL
  EDShipto.Comments[3]    AT 81 NO-LABEL
  EDShipto.Ship-to              COLON 11
  EDShipto.Attention          COLON 41  FORMAT "x(25)"
  EDShipto.Country            NO-LABEL
  EDShipto.Comments[4]    AT 81 NO-LABEL
  EDShipto.Cust-Region          COLON 11 LABEL "Cust-Reg"
  EDShipto.Phone              COLON 41
  EDShipto.Comments[5]    AT 81 NO-LABEL
  EDShipto.Dest-Zone            COLON 11
  EDShipto.Fax                COLON 41
  EDShipto.Opened         FORMAT "99/99/9999" COLON 91
  WITH FRAME f-shipto SIDE-LABELS WIDTH 144.
DOWN STREAM s-out WITH FRAME f-shipto.
IF NOT NEW (edshipto) THEN
{rc/incr.i ws_recs_changed}.
RELEASE edshipto.
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
  store_opening_date = ?
  needs_detail = FALSE.
END procedure.
