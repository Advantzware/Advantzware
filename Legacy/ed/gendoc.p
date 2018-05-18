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
DEFINE BUFFER bf-edmast FOR edmast.
FIND edcode NO-LOCK
  WHERE RECID(edcode) = p_edcode_rec  NO-ERROR.
FIND edmast EXCLUSIVE-LOCK OF edcode  
   NO-ERROR.
    
IF NOT AVAILABLE EDMast THEN 
    FIND FIRST EDMast EXCLUSIVE-LOCK WHERE EDMast.partner EQ ws_partner NO-ERROR.
    
FIND FIRST edPartnerGrp EXCLUSIVE-LOCK WHERE edPartnerGrp.partnerGrp EQ ws_partner NO-ERROR.

IF NOT AVAILABLE edPartnerGrp AND AVAILABLE(edmast) THEN 
    FIND FIRST edPartnerGrp EXCLUSIVE-LOCK WHERE edPartnerGrp.partnerGrp EQ edmast.partnerGrp NO-ERROR.
    
IF NOT AVAIL edmast OR NOT AVAILABLE edPartnerGrp THEN
DO:
  LEAVE.
END.


CREATE eddoc.
ASSIGN
  eddoc.setid         = edcode.setid
  eddoc.partner       = ws_partner
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
  edPartnerGrp.st = edPartnerGrp.st + 1 
  eddoc.seq            = edPartnerGrp.st
  eddoc.userref       = p_docid
  ws_eddoc_rec = RECID(eddoc)
  p_eddoc_rec = ws_eddoc_rec
   .

  RELEASE EDMast.
  RELEASE edPartnerGrp.
