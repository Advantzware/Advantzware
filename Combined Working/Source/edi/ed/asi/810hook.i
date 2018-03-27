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

DEF VAR run_ok              AS LOGICAL NO-UNDO.
def var searchx             as int no-undo.
DEF VAR inv_or_credit       AS LOGICAL INITIAL TRUE NO-UNDO.
def var did_some            as logical no-undo.

/* these two variables, or their equivalents, must be set by params of
calling program ... */

DEF VAR original_print      AS LOGICAL NO-UNDO.
DEF VAR force_asn           AS LOGICAL INITIAL FALSE.

ASSIGN
/* */
  original_print    = {&reprint}
  force_asn         = {&force_asn}
/* */
  ws_process_rec    = RECID(inv-head)
  inv_or_credit     = IF inv-head.t-inv-rev >= 0 THEN TRUE ELSE FALSE
  called = true.
  caller = program-name(1).
  .

EDI-FIND-LOOP:
DO:

  FIND FIRST edmast WHERE
    edmast.cust EQ inv-head.bill-to
    NO-LOCK NO-ERROR.

  IF NOT AVAILABLE edmast THEN
  LEAVE EDI-FIND-LOOP.

  find edco
  where edco.company = inv-head.company no-lock no-error.

  ASSIGN
    ws_edmast_rec = RECID(edmast)
    .

  /* 810 process (inv-heads) */
  EDI-810-LOOP:
  DO:

    ws_set_id = "810".

    FIND edcode WHERE
      edcode.partner   EQ ws_partner AND
      edcode.tran-type EQ ws_set_id AND
      edcode.direction EQ "O"
      NO-LOCK NO-ERROR.

    IF NOT AVAILABLE edcode THEN
    LEAVE EDI-810-LOOP.

    ws_edcode_rec = RECID(edcode).

    run_ok = FALSE.
    _810_search:
    DO searchx = 1 TO 2:
      next_program =
      "ed" + dirsep + edco.translator + dirsep +
      (IF edcode.customized THEN
      custom-proc
      ELSE
      TRIM(edcode.tran-type) +
        (IF version > 0
        AND searchx = 1 THEN "-" + TRIM(STRING(edcode.version,"9999"))
        ELSE ""))
        + ".p".
      DISPLAY next_program WITH FRAME f-current.
      IF SEARCH(next_program) <> ? THEN
      DO:
        run_ok = TRUE.
        LEAVE _810_search.
      END.
      ELSE
      DO:
        ws_erc = -1.
      END.
    END.    /* search i loop */
    IF run_ok THEN
    DO:
      ASSIGN ws_edcode_rec = RECID(edcode).
      RUN VALUE(next_program).
      did_some = TRUE.
    END.

  END. /* end of EDI-810-LOOP */

  /*    ... At present this is done inside then 810 program automatically
  /* 856 process (shipment advice) */
  IF original_print OR force_asn THEN
  EDI-856-LOOP: /* 9601 CAH: No ASN on reprinted inv-head */
  DO:
    ws_set_id = "856".
    FIND edcode WHERE
      edcode.partner   EQ ws_partner AND
      edcode.tran-type EQ ws_set_id AND
      edcode.direction EQ "O"
      NO-LOCK NO-ERROR.

    IF NOT AVAILABLE edcode THEN
    LEAVE EDI-856-LOOP.

    ws_edcode_rec = RECID(edcode).

    run_ok = FALSE.
    _856_search:
    DO searchx = 1 TO 2:
      next_program =
      "ed" + dirsep + edco.translator + dirsep +
      (IF edcode.customized THEN
      custom-proc
      ELSE
      TRIM(edcode.tran-type) +
        (IF version > 0
        AND searchx = 1 THEN "-" + TRIM(STRING(edcode.version,"9999"))
        ELSE ""))
        + ".p".
      DISPLAY next_program WITH FRAME f-current.
      IF SEARCH(next_program) <> ? THEN
      DO:
        run_ok = TRUE.
        LEAVE _856_search.
      END.
      ELSE
      DO:
        ws_status = "Program Not Found".
        DISPLAY ws_status WITH FRAME f-view.
        PAUSE 1.
      END.
    END.    /* search i loop */
    IF run_ok THEN
    DO:
      ASSIGN ws_edcode_rec = RECID(edcode).
      ws_status = "Processing: " + next_program.
      DISPLAY ws_status WITH FRAME f-view.
      RUN VALUE(next_program).
      did_some = TRUE.
      DISPLAY ws_erc WITH FRAME f-current.
      PAUSE 2.
    END.
  END. /* end of EDI-856-LOOP */
  */
END. /* end of EDI-FIND-LOOP */

called = false.
caller = "".
