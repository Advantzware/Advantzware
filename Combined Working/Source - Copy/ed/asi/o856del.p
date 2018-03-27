/***************************************************************************\
*****************************************************************************
**  Program: E:\CLIENTS\ASI\FOLD\RCO35\E
**       By: Chris Heins
** Descript: Delete an outbound 856 in order to recycle and reprint
06.10.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Changed to eddoc.userref as field basis for reprinting.  docid is now
the unique master bill of lading number.
**
*****************************************************************************
\***************************************************************************/
DEF VAR local-debug AS LOGICAL NO-UNDO INITIAL false.
IF local-debug THEN
DO:
  MESSAGE "in " PROGRAM-NAME(1).
  PAUSE.
END.
{ed/sharedv.i "new"}
def SHARED var v-mast-bol-no like oe-bolh.bol-no format ">>>>>9" no-undo.
DEF VAR ws_userref       AS CHAR    NO-UNDO.
DEF INPUT PARAM bol_rec             AS RECID   NO-UNDO.
DEF VAR run_ok              AS LOGICAL NO-UNDO.
DEF VAR searchx             AS INT NO-UNDO.
DEF VAR inv_or_credit       AS LOGICAL INITIAL TRUE NO-UNDO.
DEF VAR did_some            AS LOGICAL NO-UNDO.
/* these two variables, or their equivalents, must be set by params of
calling program ... */
FIND OE-BOLH
  WHERE RECID(oe-bolh) = bol_rec NO-LOCK NO-ERROR.
IF NOT AVAIL oe-bolh THEN
RETURN.
ASSIGN
  /*
  original_print    = {&reprint}
  force_asn         = {&force_asn}
  */
  ws_process_rec    = RECID(oe-bolh)
  called = TRUE
  caller = PROGRAM-NAME(1)
  .
EDI-FIND-LOOP:
DO:
  FIND FIRST edmast WHERE
    edmast.cust EQ oe-bolh.cust-no
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE edmast THEN
  LEAVE EDI-FIND-LOOP.
  ws_partner = edmast.partner.
  IF local-debug THEN
  DO:
    MESSAGE "found edmast".
    PAUSE.
  END.
  FIND edco
    WHERE edco.company = oe-bolh.company NO-LOCK NO-ERROR.
  ASSIGN
    ws_edmast_rec = RECID(edmast)
    .
  IF local-debug THEN
  DO:
    MESSAGE "found edco".
    PAUSE.
  END.
  ws_setid = "856".
  FIND edcode WHERE
    edcode.partner   EQ edmast.partner AND
    edcode.setid EQ ws_setid AND
    edcode.direction EQ "O"
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE edcode THEN
  RETURN.
  IF local-debug THEN
  DO:
    MESSAGE "found edcode".
    PAUSE.
  END.
  ws_edcode_rec = RECID(edcode).
  ws_userref =
  STRING(YEAR (oe-bolh.bol-date),"9999")
    + STRING(MONTH(oe-bolh.bol-date),"99")
    + STRING(DAY  (oe-bolh.bol-date),"99")
    + "-"
    + oe-bolh.trailer.
  IF local-debug THEN
  DO:
    MESSAGE "Searching for " edcode.partner edcode.setid
      edcode.direction ws_userref oe-bolh.bol-date.
    PAUSE.
  END.
  FIND LAST eddoc use-index byRef
    WHERE eddoc.partner     = edcode.partner
    AND eddoc.setid         = edcode.setid
    AND eddoc.direction     = edcode.direction
    AND eddoc.userref       = ws_userref EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL eddoc
    THEN
  DO:
    IF NOT eddoc.posted THEN
    DO:
      IF local-debug THEN
      DO:
	MESSAGE "running ed/fm856del.p for " eddoc.docid eddoc.seq.
	PAUSE.
      END.
      RUN ed/fm856del.p (INPUT RECID(eddoc)).
      ASSIGN
	eddoc.openitem = FALSE
	eddoc.stat = 9
	eddoc.status-flag = "DEL"
	{rc/stampcht.i eddoc}
	.
    END.
    ELSE
    DO:
      IF local-debug THEN
      DO:
	MESSAGE "document" eddoc.seq "already posted, cannot delete".
	PAUSE.
      END.
    END.
  END.
END.
