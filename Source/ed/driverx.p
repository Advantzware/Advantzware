/***************************************************************************\
*****************************************************************************
**  Program: ed\inbound.p
**       By: Christopher Heins (c) 1997 Report Concepts, Inc.
** Descript: EDI Import Driver
09.02.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added proc-order to sort, e.g. to bring in store directory then po then chg.
08.13.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added error handing of returned value in ws_erc via ercput.i.  This saves
the driver programs from each having to include error variables. However if
included they must be NEW shared or matching extent (10).
11.20.97 by CAH on \\ricky\robj8\ Log#0000:
1.  Added NO-LOCK to FOR EACH on edmast and edcode.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i "new"}
{rc/ercvars.i 10 " " "new"}     /* 9808 CAH */
{rc/stringv.i}                  /* 9808 CAH */
DEF NEW SHARED STREAM s-out.
DEF NEW SHARED STREAM s-err.
DEF NEW SHARED FRAME hdg-std.
{rc/hdg-wide.i "ed/driver.p" "EDI PROCESSING EDIT LIST" "(s-out)" }
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
{rc/ftopsl.i}
{rc/fcurrent.i &init="ROW 3" &shared="new shared"
  &displayf=" edmast.partner edcode.setid
      edcode.version
      edcode.test-prod
      next_program column-label 'Driver Program'
      ws_erc column-label 'Erc' "}
{rc/statline.i}
{rc/viewline.i &displayf="ws_company label 'Co ' ws_test-prod ws_direction ws_status top-debug format 'Y/N' label 'Dbg?' "
  &shared="new shared"}
IF PROGRAM-NAME(2) BEGINS "ed/outbound" THEN
ws_direction = "o".
ELSE
ws_direction = "i".    /* 9808 CAH */
IF ws_company = "" THEN
DO:
  FIND FIRST edco NO-LOCK NO-ERROR.
  ws_company = IF AVAIL edco THEN
  edco.company ELSE ""
  .
END.
DISPLAY ws_company ws_direction
  WITH FRAME f-view.
UPDATE
  ws_company ws_test-prod top-debug
  WITH FRAME f-view.
UPDATE
  partner_list    COLON 15
  WITH FRAME f-top.
setid_list = "".
FOR EACH edcode
    WHERE edcode.direction = ws_direction
    AND edcode.test-prod = ws_test-prod
    AND CAN-DO(partner_list, edcode.partner):
  IF NOT CAN-DO(setid_list, edcode.setid) THEN
  DO:
    {rc/listadd.i setid_list edcode.setid}
  END.
END.
UPDATE
  setid_list  COLON 15
  WITH FRAME f-top.
/*
{ed/partlist.i}
{ed/codelist.i}
*/
ws_direction = LC(ws_direction).     /* for unix */
FIND edco WHERE edco.company = ws_company NO-LOCK NO-ERROR.
IF NOT AVAIL edco THEN
FIND FIRST edco NO-LOCK NO-ERROR.
IF NOT AVAIL edco THEN
DO:
  RUN ed/fmco.p.
  FIND FIRST edco NO-LOCK NO-ERROR.
END.
ws_company = edco.company.
DISPLAY ws_company WITH FRAME f-view.
/* {rc/getprint.i "stream s-out" 56} */
output stream s-out to chris.prn paged page-size 56 append.
OUTPUT STREAM s-err TO chris.err paged page-size 56.
{rc/hdg-noco.i}
{rc/ctrtext.i hdg_name 40}.
{rc/ctrtext.i hdg_desc 40}.
hdg_text = "".
VIEW STREAM s-out FRAME hdg-std.
HIDE FRAME f-top NO-PAUSE.
VIEW FRAME f-current.
FOR EACH edmast NO-LOCK
    WHERE CAN-DO(partner_list, edmast.partner):
  DISPLAY edmast.partner WITH FRAME f-current.
  ws_partner = edmast.partner.
  IF top-debug THEN
  RUN rc/debugmsg.p ("Processing partner: " + edmast.partner).
  FOR EACH edcode OF edmast NO-LOCK
      WHERE CAN-DO(setid_list, edcode.setid)
      AND (IF ws_test-prod = ? THEN TRUE ELSE edcode.test-prod = ws_test-prod)
      AND edcode.direction = ws_direction
      /* 9809 CAH: Added proc-order to sorting sequence */
      BREAK BY edcode.partner /* BY edcode.proc-order */BY edcode.setid:
    IF top-debug THEN
    RUN rc/debugmsg.p
      ("Processing Code: "
      + edcode.setid + '/'
      + string(edcode.version) + '/'
      + string(edcode.test-prod) + '/').
    ASSIGN ws_erc = 0 ws_erc_desc = "".   /* 9808 CAH */
    DISPLAY edcode.setid
      edcode.version
      edcode.test-prod
      WITH FRAME f-current.
    ws_edcode_rec = RECID(edcode).
    ws_setid = edcode.setid.
    IF ws_direction = "O" AND FIRST-OF (edcode.setid)
      THEN
    DO:
      /* run outbound touchups pre-interface if available */
      next_program = "ed" + dirsep + edco.system + dirsep +
      "o" + edcode.setid + "fix.p".
      IF SEARCH(next_program) <> ? THEN
      DO:
        IF top-debug THEN
        RUN rc/debugmsg.p ("Running outbound preprocessor: "
          + next_program).
        RUN VALUE(next_program).
        DISPLAY
          next_program COLUMN-LABEL "Driver Program"
          ws_erc COLUMN-LABEL "Erc"
          WITH FRAME f-current.
        PAUSE 2.
        next_program = "".
      END.
    END.
    DEF VAR run_ok AS LOGICAL NO-UNDO.
    run_ok = FALSE.
    _search:
    DO i = 1 TO 2:
      next_program =
      "ed" + dirsep + (IF NOT customized THEN
      (edco.translator + dirsep)
      ELSE ""
      )
      +
      (IF edcode.customized THEN
      custom-proc
      ELSE
      ws_direction +
      TRIM(edcode.setid) +
        (IF version > 0
        AND i = 1 THEN TRIM(STRING(edcode.version,"9999"))
        ELSE ""))
        + ".p".
      PAUSE 0.
      DISPLAY next_program WITH FRAME f-current.
      IF SEARCH(next_program) <> ? THEN
      DO:
        run_ok = TRUE.
        LEAVE _search.
      END.
      ELSE
      DO:
        ws_status = "Program Not Found".
        IF top-debug THEN
        RUN rc/debugmsg.p
          ( ws_status + ':
        ' + next_program).
        PAUSE 0.
        DISPLAY ws_status WITH FRAME f-view.
        PAUSE 1.
      END.
    END.    /* search i loop */
    IF run_ok THEN
    DO:
      PAUSE 0.
      ws_status = "Processing: " + next_program.
      DISPLAY ws_status WITH FRAME f-view.
      PAUSE 0.
      STATUS DEFAULT (IF ws_direction = "I" THEN "Importing " ELSE "Exporting ")
        + edco.translator + " for " + edcode.partner
        + " set " + edcode.setid + " v " + STRING(edcode.version).
      IF top-debug THEN
      RUN rc/debugmsg.p ("Before running: " +  next_program).
      HIDE FRAME f-current NO-PAUSE.
      {rc/statdisp.i}
      RUN VALUE(next_program).
      IF top-debug THEN
      RUN rc/debugmsg.p ("After running: " + next_program).
      did_some = TRUE.
      DISPLAY ws_erc WITH FRAME f-current.
      PAUSE 2.
      next_program = "".
      STATUS DEFAULT.
    END.
    IF ws_direction = "I" AND LAST-OF (edcode.setid)
      AND did_some THEN
    DO:
      /* run app system interface if available */
      next_program = "ed" + dirsep + edco.system + dirsep +
      "i" + edcode.setid + ".p".
      /* 9809 CAH: Check for general interface/print ... */
      IF SEARCH(next_program) = ? THEN
      next_program = "ed" + dirsep + "i" + edcode.setid + ".p".
      IF top-debug THEN
      RUN rc/debugmsg.p
        ("Last of inbound code: " + edcode.setid + " looking for proc: "
        + next_program).
      IF SEARCH(next_program) <> ? THEN
      DO:
        PAUSE 0.
        ws_status = "Interfacing: " + next_program.
        DISPLAY ws_status WITH FRAME f-view.
        HIDE FRAME f-current NO-PAUSE.
        RUN VALUE(next_program).
        DISPLAY ws_erc WITH FRAME f-current.
        PAUSE 2.
        next_program = "".
        STATUS DEFAULT.
      END.
    END.
    IF ws_erc <> 0 THEN
    DO:
      IF ws_erc_desc > ""
        THEN
      DO erctokenx = 1 TO NUM-ENTRIES(ws_erc_desc):
        erctoken[erctokenx] = ENTRY(erctokenx, ws_erc_desc).
      END.
      erclist = string(ws_erc).
      {rc/ercput.i "stream s-out"}
    END.
  END.
END.
STATUS DEFAULT.
{rc/endprint.i "stream s-out"}
OUTPUT STREAM s-err close.
HIDE FRAME f-current.
HIDE FRAME f-view.
