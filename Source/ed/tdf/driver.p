/***************************************************************************\
*****************************************************************************
**  Program: E:\robj8\patch\ed\tdf\drive
**       By: Chris Heins, Report Concepts, Inc. (c) 1998 All Rights Reserved.
** Descript: Parse TDF File, call set level drivers as encountered.
**
04.28.99 by CAH on \\ricky\asi\patch Log#0000:
1.  Added no-error to run on 000 so that if it returns error-status:error
program will still fall through and set skip_doc = true to jump to the
next header.
10.23.98 by CAH on \\ricky\rv8 Log#0000:
1.  Removed the conditional renaming of the input file based on error-status,
which is almost always true but not due to the type of error
for which we would want to save the input file.
2.  Replaced archiving statements with run rc/filearch.
3.  Added puts on records changed and deleted.
*****************************************************************************
\***************************************************************************/
PAUSE 0.
DEF NEW SHARED STREAM s-in.
DEF SHARED STREAM s-out.
{ed/sharedv.i}
{ed/edivars.i       "new shared"}
{ed/tdf/sharedv.i   "new shared"}
{rc/stats.i         "new shared"}
{rc/stringv.i       "new shared"}
{rc/datev.i}
{rc/timev.i}
{rc/ercvars.i 60 " " "new"}
DEF VAR fid AS CHAR FORMAT 'x(12)' NO-UNDO EXTENT 1 INITIAL
  [ "" ].
DEF VAR temp_fid      AS CHAR NO-UNDO.
DEF VAR err_fid AS CHAR NO-UNDO.
DEF VAR curr_fid     AS CHAR NO-UNDO.  /* used for deletion when done */
DEF VAR n AS INT NO-UNDO FORMAT "9" INITIAL 1.
DEF VAR ws_customer LIKE edmast.cust NO-UNDO.
/* set true if we have to skip to the next header */
DEF var ws_message AS char FORMAT 'x(40)' NO-UNDO LABEL "Message".
DEF VAR item_offset AS INTEGER NO-UNDO.
DEF VAR ws_test-prod LIKE edcode.test-prod INITIAL FALSE NO-UNDO.
DEF VAR partner_list AS CHAR NO-UNDO    INITIAL "*"
  FORMAT "x(60)" LABEL "Partners".
DEF VAR setid_list AS CHAR NO-UNDO  INITIAL "850,860,852,832"
  FORMAT "x(60)" LABEL "Tran Sets".
DEF VAR ws_status AS CHAR NO-UNDO FORMAT 'x(30)' LABEL "Status".
DEF VAR process_tran AS LOGICAL NO-UNDO EXTENT 10 INITIAL FALSE
  LABEL "OK?".
DEF VAR keyfile AS CHAR NO-UNDO INITIAL '' EXTENT 10
  LABEL "Header File"
  FORMAT 'x(30)'.
DEF VAR status_msg AS CHAR NO-UNDO FORMAT 'x(15)' EXTENT 10
  LABEL "Status".
DEF VAR i AS INT NO-UNDO.
DEF VAR at_least_one AS LOGICAL NO-UNDO.
DEF VAR f-det-title AS CHAR NO-UNDO.
DEF VAR did_some AS LOGICAL NO-UNDO.
{rc/statline.i}
{rc/viewline.i &displayf="
    ws_company label 'Co ' 
        help 'Enter Company code to process'
    ws_test-prod format 'T/P'
        help 'Enter (P)roduction, (T)est, or ? for BOTH'
        auto-return
    ws_print-opt format 'Y/N' label 'Prt?' 
        help 'Enter Y to print EDI, N to suppress'
        auto-return
    ws_direction 
    top-debug format 'Y/N' label 'Dbg?' 
        help 'Enter Y to enable debugging output to rpro_dbg.txt'
        auto-return
    ws_status format 'x(27)'
    "
  &shared="shared"}
{ed/getpath.i}
ASSIGN
  temp_fid = "t_" + STRING(TIME,"99999") + ".q".
err_fid = edco.path-err + dirsep + "edierr.prn".
start = TIME.
n = 1.
_files:
DO n = 1 TO 1:
  IF top-debug THEN
  DO:
    RUN rc/debugmsg.p ("top of _files repeat loop").
  END.
  curr_fid = ws_edi_path + (IF fid[n] > "" THEN
  dirsep + LC(fid[n]) ELSE ""
  ).
  /* 9807 CAH: Removed curr_fid = search(curr_fid),
  was causing ? to appear on the edit list
  This conversion is done in the ELSE below ... */
  IF SEARCH(curr_fid) = ? THEN
  DO:
    IF n <= 1 /* no header file */ THEN
    DO:
      PUT STREAM s-out UNFORMATTED SKIP(1)
        "Input file: " curr_fid " was not found, cannot continue" SKIP.
      RETURN.
    END.
    ELSE
    NEXT _files.
  END.
  /* assigment required to put the archive in the same directory */
  ELSE
  curr_fid = SEARCH(curr_fid).
  IF top-debug THEN
  DO:
    RUN rc/debugmsg.p ("running quoter on " + curr_fid + " into " + temp_fid).
  END.
  RUN rc/osquoter.p
    (curr_fid, ?, ?, temp_fid).
  INPUT STREAM s-in FROM VALUE(temp_fid) NO-ECHO.
  tdf_eof = FALSE.
  PUT STREAM s-out UNFORMATTED SKIP(1) "Processing from file: " curr_fid SKIP.
  PAUSE 0.
  _main:
  REPEAT:
    IF tdf_eof THEN
    LEAVE _main.
    ASSIGN str_buffa = '' erclist = ''.
    IMPORT STREAM s-in str_buffa.
    IF top-debug THEN
    RUN rc/debugmsg.p ("after import str_buffa: " + str_buffa).
    {rc/incr.i ws_recs_read}.
    _process:
    REPEAT:
      IF tdf_eof THEN
      LEAVE _process.
      IF top-debug THEN
      DO:
        RUN rc/debugmsg.p ("top of _process, str_buffa=" + str_buffa +  " skip_doc="
          + string(skip_doc) ).
      END.
      IF str_buffa <= " " THEN
      NEXT _main.
      IF SUBSTRING(str_buffa,7,11) = header_sep-code THEN
      DO:
        ASSIGN
          ws_rec_code = 0
          last_rec_code = 0
          skip_doc = FALSE. /* reset at start of new document */
        ws_segment = substring(str_buffa, 7, 3).
      END.
      ELSE
      DO:
        IF skip_doc THEN
        NEXT _main.
        {rc/substr.i ws_rec_code 10 3 integer}.
      END.
      IF top-debug THEN
      RUN rc/debugmsg.p ("rec_code: " + string(ws_rec_code)
        + " skip_doc: " + string(skip_doc)
        + " tdf_eof:" + string(tdf_eof)
        + " error-status: " + string(error-status:error) ).
      IF ws_rec_code = 0 THEN
      DO: /* header record */
        IF top-debug THEN
        RUN rc/debugmsg.p ("in header processing ").
        error-status:error = FALSE.
        RUN ed/tdf/000.p
          (INPUT "I", INPUT-OUTPUT str_buffa, OUTPUT ws_erc) NO-ERROR.
        /* 9809 cah was INPUT */
        IF error-status:error THEN
        DO:
          skip_doc = TRUE.
          NEXT _main.

        END.
        IF top-debug THEN
        RUN rc/debugmsg.p ("after ed/tdf/000").
        {rc/incr.i ws_recs_selected}.
        RUN rc/str2int.p (header_std-ver, OUTPUT ws_int).  /* 9807 CAH */
        IF top-debug THEN
        RUN rc/debugmsg.p ("after str2int").
        ASSIGN
          ws_partner = TRIM(substring(header_partner,1,5))
          ws_setid   = TRIM(header_setid)
          ws_version = string(ws_int,"9999")
          .
        IF top-debug THEN
        DO:
          DISPLAY STREAM s-out
            header_partner
            header_setid
            header_std-ver
            header_int-cd
            header_fgid
            header_isa
            header_gs
            header_st
            header_std-ver
            header_std-rcvd
            header_std-used
            header_rcvd-test-prod
            header_part-test-prod
            SKIP
            ws_partner
            ws_setid
            WITH FRAME f-debug-1 side-labels width 144.
          RUN rc/debugmsg.p ("ws_partner: " + ws_partner
            + " ws_setid: " + ws_setid
            + " ws_version: " + ws_version).
        END.
        FIND edmast
          WHERE edmast.partner = ws_partner NO-LOCK NO-ERROR.
        IF NOT AVAIL edmast THEN
        DO:
          {rc/listadd.i erclist 201}.
          erctoken[2] = ws_partner.
        END.
        ELSE
        ASSIGN ws_edmast_rec = RECID(edmast).
        FIND FIRST edcode
          WHERE edcode.partner = ws_partner
          AND edcode.setid   = ws_setid
          AND edcode.version = ws_int
          AND edcode.direction = "I" NO-LOCK NO-ERROR.
        IF NOT AVAIL edcode THEN
        DO:
          {rc/listadd.i erclist 301}
          erctoken[3] = ws_setid.
          skip_doc = TRUE.
        END.
        IF top-debug THEN
        RUN rc/debugmsg.p ("before erclist check: " + erclist).
        IF erclist = '' THEN
        DO:
          IF top-debug THEN
          RUN rc/debugmsg.p ("about to search for document procedure").
          ws_edcode_rec = RECID(edcode).
          DEF VAR run_ok AS LOGICAL NO-UNDO.
          run_ok = FALSE.
          _search:
          DO:
            next_program =
            "ed" + dirsep +  "tdf" + dirsep
            + ws_direction + TRIM(edcode.setid) + TRIM(STRING(edcode.version,"9999")) + ".p".
            IF top-debug THEN
            RUN rc/debugmsg.p ("searching for " + next_program).
            RUN rc/chkifok.p (next_program, OUTPUT run_ok, OUTPUT ws_char).
            IF run_ok THEN
            DO:
              ws_status = "Program: " + next_program.
              LEAVE _search.
            END.
            ELSE
            DO:
              ws_status = "Program Not Found".
              {rc/listadd.i erclist 10}
              erctoken[1] = next_program.
              PAUSE 1.
            END.
          END.    /* search i loop */
        END.  /* erclist = "" */
        IF run_ok THEN
        DO:
          DISPLAY ws_status WITH FRAME f-view.
          /*          HIDE FRAME f-current NO-PAUSE. */
          IF top-debug THEN
          RUN rc/debugmsg.p ("running " + next_program).
          RUN VALUE(next_program).
          IF top-debug THEN
          RUN rc/debugmsg.p ("after return, tdf_eof=" + string(tdf_eof)
            + " ws_segment=" + ws_segment).
          PAUSE 0.
          /*          VIEW FRAME f-current. */
        END.
        IF erclist > '' THEN
        DO:
          IF top-debug THEN
          RUN rc/debugmsg.p ("erclist: " + erclist).
          {rc/incr.i ws_recs_inerror}.
          DISPLAY STREAM s-out
            ws_partner
            ws_setid
            ws_version
            header_std-ver
            header_int-cd
            header_isa
            header_gs
            header_st
            WITH FRAME f-tdf-header width 132.
          {rc/ercput.i "stream s-out"}
        END.
        IF top-debug THEN
        RUN rc/debugmsg.p ("before end of 0 rec processing").
        PAUSE 0.
      END.    /* separator rec, code = 0  */
      IF tdf_eof THEN
      LEAVE _process.
      last_rec_code = ws_rec_code.
      IF top-debug THEN
      RUN rc/debugmsg.p ("before end of process repeat loop").
      PAUSE 0.
    END.    /* process repeat */
    IF tdf_eof THEN
    LEAVE _main.
    IF top-debug THEN
    RUN rc/debugmsg.p ("before end of main repeat loop").
    PAUSE 0.
  END. /* MAIN repeat */
  PUT STREAM s-out UNFORMATTED
    SKIP(1) "Records added        : " ws_recs_added  FORMAT "-99999" SKIP.
  IF ws_recs_changed > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Records changed      : " ws_recs_changed FORMAT "-99999" SKIP.
  IF ws_recs_deleted > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Records deleted      : " ws_recs_deleted FORMAT "-99999" SKIP.
  IF ws_amt_inerror > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Duplicates rejected  : " ws_amt_inerror FORMAT "-99999" SKIP.
  IF ws_amt_changed > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Duplicated recycled  : " ws_amt_changed FORMAT "-99999" SKIP.
  INPUT STREAM s-in CLOSE.
  IF top-debug THEN
  RUN rc/debugmsg.p ("before rename of " + curr_fid).
  /* 98.10.23 replaced inline code with this call ... */
  RUN rc/filearch.p (curr_fid, "", "", OUTPUT ws_char).
  IF OS-ERROR = 0 THEN
  DO:
    PUT STREAM s-out UNFORMATTED
      SKIP(1) "Input file archived as " ws_char SKIP.
  END.
  RUN rc/osdel.p (temp_fid).
  IF top-debug THEN
  RUN rc/debugmsg.p ("after rename").
  PAUSE 0.
END.
PAUSE 0.
