/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\ASI\810HOOK.I
**       By: Chris Heins (c) 1997 Report Concepts, Inc.
** Descript: Include to insert into invoice to generate EDI files
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i "new"}
{rc/callvar.i "new"}
DEF INPUT PARAM i_rec               AS RECID NO-UNDO.
DEF INPUT PARAM original_print      AS LOGICAL NO-UNDO.
DEF INPUT PARAM force_asn           AS LOGICAL INITIAL FALSE.

DEF VAR run_ok        AS LOGICAL NO-UNDO.
DEF var searchx       AS int     NO-UNDO.
DEF VAR inv_or_credit AS LOGICAL INITIAL TRUE NO-UNDO.
DEF var did_some      AS logical NO-UNDO.
def var ws_billto     as char    no-undo.
def var vlAR-OE       as logical no-undo.

IF dirsep = "" OR dirsep = ? THEN
dirsep = "~\".    /* 03.26.2004 CAH */

/* these two variables, or their equivalents, must be set by params of
calling program ... */

FIND inv-head WHERE RECID(inv-head) = i_rec NO-LOCK NO-ERROR.
IF NOT AVAIL inv-head THEN do:
find ar-inv where recid(ar-inv) = i_rec no-lock no-error.
if not avail ar-inv then RETURN "NO INVOICE".
end.

if avail inv-head then do:
  ws_process_rec    = RECID(inv-head).
  inv_or_credit     = IF inv-head.t-inv-rev >= 0 THEN TRUE ELSE FALSE.
  ws_billto         = inv-head.bill-to.
  ws_company        = inv-head.company.
  vlAR-OE           = false.
end.
else do:
  ws_process_rec    = RECID(ar-inv).
  inv_or_credit     = IF ar-inv.gross >= 0 THEN TRUE ELSE FALSE.
  ws_billto         = ar-inv.cust-no.
  ws_company        = ar-inv.company.
  vlAR-OE           = true.
end.


ASSIGN
  /*
  original_print    = {&reprint}
  force_asn         = {&force_asn}
  */
  called = TRUE
  caller = PROGRAM-NAME(1).

EDI-FIND-LOOP:
DO:
  FIND FIRST edmast WHERE
    edmast.cust EQ ws_billto
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE edmast THEN
  RETURN "NO EDMAST FOR: " + WS_BILLTO.
  ASSIGN
    ws_edmast_rec = RECID(edmast)
    .

  FIND edco
    WHERE edco.company = ws_company NO-LOCK NO-ERROR.

  IF top-debug AND AVAIL EDCO
    THEN
  RUN rc/debugrec.s ("In EDI-FIND-LOOP", RECID(edco)) "EDCO".

  IF top-debug THEN
  RUN rc/debugrec.s
    ("In EDI-FIND-LOOP", RECID(edmast)) "EDMast".

  /* 810 process (inv-heads) */
  EDI-810-LOOP:
  DO:
    ws_setid = "810".
    FIND FIRST edcode WHERE
    ( edcode.partner   EQ edmast.partner or
    edcode.partner   eq edmast.partnerGrp)  AND
      edcode.setid EQ ws_setid AND
      edcode.direction EQ "O"
      NO-LOCK NO-ERROR.
    IF NOT AVAILABLE edcode THEN
    RETURN "NO EDCODE".
    ws_edcode_rec = RECID(edcode).

    IF top-debug THEN
    RUN rc/debugrec.s ("In EDI-FIND-LOOP", RECID(edcode)) "EDCOde".

    run_ok = FALSE.
    _810_search:
    DO searchx = 1 TO 2:
      next_program =
      "ed" + dirsep + edco.system + dirsep + "o" + TRIM(edcode.setid) +
        (IF version > 0
        AND searchx = 1 THEN TRIM(STRING(edcode.version,"9999"))
        ELSE "")
        + ".p".
      RUN rc/chkifok.p (next_program, OUTPUT run_ok, OUTPUT ws_char).
      IF run_ok THEN
      DO:
        IF top-debug THEN
        RUN rc/debugmsg.p ("found " + ws_char).
        LEAVE _810_search.
      END.
      ELSE
      DO:
        IF top-debug THEN
        RUN rc/debugmsg.p ("could not find " + next_program).
        ws_erc = -1.
      END.
    END.    /* search i loop */
	
    IF run_ok THEN
    DO:
	
      ASSIGN ws_edcode_rec = RECID(edcode).
      STATUS DEFAULT "Creating EDI Invoice ...".
      IF top-debug THEN
      RUN rc/debugmsg.p ("Before running " + next_program).
      RUN VALUE(next_program) NO-ERROR.
      IF top-debug THEN
      RUN rc/debugmsg.p ("After running " + next_program).

      IF error-status:error THEN
      RETURN-VALUE = "RUN RETURNED ERROR".
      IF top-debug THEN
      RUN rc/debugmsg.p ("After run of " + next_program).
      IF RETURN-VALUE > "" THEN
      RETURN RETURN-VALUE.

      STATUS DEFAULT.
      did_some = TRUE.
    END.
  END. /* end of EDI-810-LOOP */
  /* 856 process (inv-heads) */
  if vlAR-OE = false then EDI-856-LOOP:
    DO:
    IF ws_856_from_invoice THEN
    DO:
      ws_setid = "856".
      FIND FIRST edcode WHERE
      ( edcode.partner   EQ edmast.partner or
      edcode.partner   eq edmast.partnerGrp) AND
        edcode.setid EQ ws_setid AND
        edcode.direction EQ "O"
        NO-LOCK NO-ERROR.
      IF NOT AVAILABLE edcode THEN
      LEAVE EDI-856-LOOP.
      ws_edcode_rec = RECID(edcode).
      run_ok = FALSE.
      _856_search:
      DO searchx = 1 TO 2:
        next_program =
        "ed" + dirsep + edco.system + dirsep +
        (/* 9806 CAH:> this does not apply to APP -> EDI interface ...
        IF edcode.customized THEN
        custom-proc
        ELSE <*/
        TRIM(edcode.setid) +
          (IF version > 0
          AND searchx = 1 THEN "-" + TRIM(STRING(edcode.version,"9999"))
          ELSE ""))
          + ".p".
        RUN rc/chkifok.p (next_program, OUTPUT run_ok, OUTPUT ws_char).
        IF run_ok THEN
        DO:
          LEAVE _856_search.
        END.
        ELSE
        DO:
          ws_erc = -1.
        END.
      END.    /* search i loop */
      IF run_ok THEN
      DO:
        STATUS DEFAULT "Creating EDI Advance Ship Notice ...".
        ASSIGN ws_edcode_rec = RECID(edcode).
        FOR EACH oe-bolh
            WHERE oe-bolh.company = ws_company
            AND oe-bolh.bol-no = inv-head.bol-no
            NO-LOCK:
          ws_process_rec = RECID(oe-bolh).
          RUN VALUE(next_program).
          did_some = TRUE.
        END.
        STATUS DEFAULT.
      END.
    END.    /* 856 from invoice */
  END. /* end of EDI-856-LOOP */
END. /* end of EDI-FIND-LOOP */
ASSIGN
  called = FALSE
  caller = "" NO-ERROR.
RETURN "".
