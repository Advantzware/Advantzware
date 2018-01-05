/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\i8124
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
DEF var first_time AS logical initial TRUE NO-UNDO.
DEF var needs_detail AS logical initial FALSE NO-UNDO.
DEF var sub_cmd AS char NO-UNDO initial "I".
if ws_print-opt = true then sub_cmd = "IP".
else sub_cmd = "I".
DEF var print_codes AS char NO-UNDO initial "*".
FIND edcode WHERE RECID(edcode) = ws_edcode_rec NO-LOCK.
FIND edmast WHERE RECID(edmast) = ws_edmast_rec NO-LOCK.
/* Assign the sequence number of the lowest detail record */
dtl_rec_code = 22. /* X ANSI version, Note that VICS=21. */
ws_docid = string(header_isa,"999999999")
+ string(header_gs, "999999999")
+ string(header_st, "999999999").
PAUSE 0.
VIEW FRAME f-current.
VIEW FRAME f-stats.
/* parse input file until 000 segment is encountered or end of file */
_outer:
REPEAT:
  IF top-debug THEN
  RUN rc/debugmsg.p ("top of _outer repeat block: "
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
  if ws_rec_code >= dtl_rec_code
  then do:
    if can-do(print_codes, string(ws_rec_code,"999"))
    then sub_cmd = "IP".
    else sub_cmd = "I".    /* parse segments */
  end.  
  else sub_cmd = "IP".  /* print header segments */
  /* put override segment processing here */
  IF ws_segment = "000" THEN
  DO:
    needs_detail = FALSE.
    IF first_time THEN
    DO:
      first_time = FALSE.
      sub_cmd = "IP".
    END.
    ELSE    /* we have encountered a new transaction set in the TDF
    which might not be the type this program parses */
    LEAVE _outer.
  END.  /* 000 */
  DO:
    RUN parse_segment.ip.
  END.
  IF ws_segment = "BCD" THEN
  DO:
    RUN create_header.ip.
  END.
  IF ws_segment = "CDD" THEN
  needs_detail = TRUE.
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
IF top-debug THEN
RUN rc/debugmsg.p ("bottom of _outer repeat block: "
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
  {rc/incr.i ws_recs_selected}.
END.    /* doc header do */
END procedure.
procedure create_detail.ip:
{rc/incr.i ws_recs_added}.
{rc/accum.i ws_amt_added total_amount1}.
END procedure.
procedure parse_segment.ip:
next_program = "ed/tdf/" + lc(ws_segment) + ".p".
IF SEARCH(next_program) <> ? THEN
DO:
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
