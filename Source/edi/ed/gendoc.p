/***************************************************************************\
*****************************************************************************
**  Program: EDi\GENDOC.P
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
{edi/sharedv.i}
DEF INPUT PARAM p_edcode_rec AS RECID.
DEF INPUT PARAM p_docid AS CHAR NO-UNDO.
DEF OUTPUT PARAM p_eddoc_rec AS RECID.

FIND edcode
  WHERE RECID(edcode) = p_edcode_rec NO-LOCK NO-ERROR.
FIND edmast OF edcode
  /* WHERE RECID(edmast) = ws_edmast_rec */
  EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL edmast THEN
DO:
  LEAVE.
END.

CREATE eddoc.
ASSIGN
  eddoc.setid         = edcode.setid
  eddoc.partner       = edcode.partner
  eddoc.docid         = p_docid
  eddoc.docseq        = ?
  eddoc.st-code       = ""
  {edi/stampadt.i eddoc}
  {edi/stampcht.i eddoc}
  eddoc.direction     = edcode.direction
  eddoc.status-flag   = "NEW"   
  eddoc.fgid          = "SH"
/*  IF edcode.setid = "850" THEN "PO" ELSE  */
/*  IF edcode.setid = "810" THEN "IN" ELSE  */
/*  IF edcode.setid = "832" THEN "VC" ELSE  */
/*  IF edcode.setid = "856" THEN "SH" ELSE  */
/*  IF edcode.setid = "852" THEN "PD" ELSE  */
/*  if edcode.setid = "816" then "OR" else ?*/
  eddoc.edi_agency    = edcode.agency
  eddoc.edi_standard  = STRING(edcode.version,"9999")
  eddoc.set-test-prod =
  IF edcode.test-prod THEN "T" ELSE "P"
  eddoc.isa           = ?
  eddoc.gs            = 0
  eddoc.st            = 0
  /*
  eddoc.fgsender      = rms_header_partner
  eddoc.fgrecvid      = rms_header_company-id
  eddoc.version       = rms_header_std-ver
  */
  EDMast.Seq = EDMast.Seq + 1 
  eddoc.seq           = EDMast.Seq
  eddoc.userref       = p_docid
  ws_eddoc_rec = RECID(eddoc)
  p_eddoc_rec = ws_eddoc_rec
  no-error.
