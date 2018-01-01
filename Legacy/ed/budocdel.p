/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\BUDOCDEL.P
**       By: Chris Heins
** Descript: Batch Delete EDI Documents
06.10.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Added cutoff_date and override status option.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/statline.i}
{rc/fcurrent.i}
{rc/stats.i}
{rc/ftopsl.i}
def var override_status as logical no-undo initial false
    label "Ovrrd Stat?".
def var del-or-reset as logical no-undo initial true format "DELETE/RESET"
    label "Mode".
def var lo_trandate as date no-undo label "From create date".
def var hi_trandate as date no-undo label "Thru create date".    
    
{rc/viewline.i &displayf="ws_partner ws_setid override_status
    del-or-reset
     "}
IF ws_partner = "" THEN
ws_partner = "*".
IF ws_setid = "" THEN
ws_setid = "*".
UPDATE
  ws_partner
  HELP "Enter trading partner, HELP=Lookup, *=ALL"
  validate(can-find(edmast where edmast.partner = ws_partner),
    "This must be a valid partner")
  ws_setid
  HELP "Enter list of transaction set types, *=ALL (850=PO, 810=IV, 856=ASN)"
  override_status
  help "Enter YES to ignore document status, NO to purge only if status = 9"
  del-or-reset
  help "Enter DELETE to purge or RESET to reset status for reprocessing"
  WITH FRAME f-view.
  
    assign lo_trandate = today hi_trandate = today.
    {rc/getdates.i &frame=f-top}
    hide frame f-top no-pause.
{rc/confirm.i}
IF NOT confirm THEN
RETURN.
start = time.
view frame f-current.
view frame f-stats.
FOR EACH eddoc EXCLUSIVE
    WHERE eddoc.partner = ws_partner
    AND   (IF ws_setid  = '*' THEN TRUE
    ELSE CAN-DO(ws_setid, eddoc.setid))
    and eddoc.adddate >= lo_trandate
    and eddoc.adddate <= hi_trandate:
  {rc/incr.i ws_recs_read}.
  
    DISPLAY eddoc.partner
      eddoc.setid
      eddoc.docid FORMAT "x(10)"
      eddoc.adddate
      eddoc.posted
      eddoc.status-flag
      WITH FRAME f-current.
  IF (eddoc.stat = 9
  or override_status)
  THEN DO:
    {rc/incr.i ws_recs_selected}.
    
    if del-or-reset = true  /* purge */ then do:
    next_program = "ed/fm" + eddoc.setid + "del.p".
    IF SEARCH(next_program) <> ? THEN
    DO:
      RUN value(next_program) (RECID(eddoc)).
      DELETE eddoc.
      {rc/incr.i ws_recs_deleted}.
    END.
    else do:
        bell.
        ws_char = "Could not locate delete procedure for SetID: " + eddoc.setid.
        message color value (c_err) ws_char.
        run rc/debugmsg.p (ws_char).
        pause.
        {rc/incr.i ws_recs_inerror}.
    end.
    
    end.    /* purge mode */
    else do:
       assign eddoc.openitem = false 
        eddoc.stat = 0 
        eddoc.status-flag = 'RES'
        eddoc.posted = false
        eddoc.c-fatime = eddoc.c-fatime + 1 /* 9801 CAH resend counter */.
        display eddoc.status-flag eddoc.posted
        with frame f-current.
        {rc/incr.i ws_recs_changed}.
        pause 0.
    end.    /* reset mode */
  END.
  {rc/statsdis.i}
  PAUSE 0.
END.
{rc/statsdis.i}
hide frame f-stats.
hide frame f-current.
