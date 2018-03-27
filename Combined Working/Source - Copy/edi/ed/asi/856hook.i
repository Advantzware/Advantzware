/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\ASI\856HOOK.I
**       By: Chris Heins (c) 1997 Report Concepts, Inc.
** Descript: Include to insert into invoice to generate EDI files
**
*****************************************************************************
\***************************************************************************/

def var local-debug as logical no-undo initial false.
if local-debug then do: message "in 856hook.i".
pause. end.

{ed/sharedv.i "new"}
{rc/callvar.i "new"}

DEF VAR run_ok              AS LOGICAL NO-UNDO.
def var searchx             as int no-undo.
DEF VAR inv_or_credit       AS LOGICAL INITIAL TRUE NO-UNDO.
def var did_some            as logical no-undo.

/* these two variables, or their equivalents, must be set by params of
calling program ... */

DEF VAR original_print      AS LOGICAL NO-UNDO.
DEF VAR force_asn           AS LOGICAL INITIAL FALSE.

ASSIGN
  original_print    = {&reprint}
  force_asn         = {&force_asn}
  ws_process_rec    = RECID(oe-bolh)
  called = true
  caller = program-name(1)
  .

EDI-FIND-LOOP:
DO:

  FIND FIRST edmast WHERE
    edmast.cust EQ oe-bolh.cust-no
    NO-LOCK NO-ERROR.

  IF NOT AVAILABLE edmast THEN
  LEAVE EDI-FIND-LOOP.

  ws_partner = edmast.partner.
  if local-debug then do: message "found edmast".
  pause. end.

  find edco
  where edco.company = oe-bolh.company no-lock no-error.

  ASSIGN
    ws_edmast_rec = RECID(edmast)
    .

  if local-debug then do: message "found edco".
  pause. end.

  /* 856 process (shipment advice) */
  IF original_print OR force_asn THEN
  EDI-856-LOOP: /* 9601 CAH: No ASN on reprinted oe-bolh */
  DO:
    ws_set_id = "856".
    FIND edcode WHERE
      edcode.partner   EQ edmast.partner AND
      edcode.tran-type EQ ws_set_id AND
      edcode.direction EQ "O"
      NO-LOCK NO-ERROR.

    IF NOT AVAILABLE edcode THEN
    LEAVE EDI-856-LOOP.

    if local-debug then do: message "found edcode".
    pause. end.

    ws_edcode_rec = RECID(edcode).

    run_ok = FALSE.
    _856_search:
    DO searchx = 1 TO 2:
      next_program =
      "ed" + dirsep + edco.system + dirsep +
      (IF edcode.customized THEN
      custom-proc
      ELSE
      TRIM(edcode.tran-type) +
        (IF version > 0
        AND searchx = 1 THEN "-" + TRIM(STRING(edcode.version,"9999"))
        ELSE ""))
        + ".p".
      IF SEARCH(next_program) <> ? THEN
      DO:
        run_ok = TRUE.
        LEAVE _856_search.
      END.
      ELSE
      DO:
        ws_erc = -1.
      END.
    END.    /* search i loop */
    IF run_ok THEN
    DO:
      ASSIGN ws_edcode_rec = RECID(edcode).
      if local-debug then do: message "running " next_program.
      pause. end.
      ws_erc = 0.
      RUN VALUE(next_program).
      if local-debug then do: message "after " next_program "erc" ws_erc.
      pause. end.
      did_some = TRUE.
    END.
  END. /* end of EDI-856-LOOP */
END. /* end of EDI-FIND-LOOP */

called = false.
caller = "".
