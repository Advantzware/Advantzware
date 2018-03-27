/***************************************************************************\
*****************************************************************************
**  Program: ROBJ8\PATCH\ED\RPRO\o810fed.p 
**       By: Chris Heins
** Descript: Federated diskette invoicing format for royal pioneer.
05.16.98 by CAH on \\ricky\obj8\patch Log#0000:
1.  Initial write from ed/rms/o8103020.p
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i}
def shared stream s-out.
def shared stream s-err.
DEF STREAM s-edi.
DEF VAR n_recs AS INT NO-UNDO.
/* 9811 CAH: Required in order for this to compile ... */
def new shared var ws_version   like eddoc.version  no-undo
    label "Vers".
def var ws_tax  as decimal no-undo.
def var ws_misc as decimal no-undo.
{ed/getpath.i}
OUTPUT STREAM s-edi TO VALUE(ws_edi_path) APPEND.
FOR EACH eddoc EXCLUSIVE
    WHERE eddoc.setid = edcode.setid
    AND eddoc.partner = edcode.partner
    AND eddoc.error-count = 0
    AND eddoc.posted = FALSE 
    AND eddoc.direction = edcode.direction:
    
  FIND edivtran OF eddoc
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edivtran THEN
  DO:
    ws_erc = 1.
    EDDOC.error-count = 1.
    NEXT.
  END.
  
  assign ws_tax = 0 ws_misc = 0.
  for each edivaddon NO-LOCK
  of edivtran
  where hand-meth = "02" /* included in the invoice amount */ :
    ws_misc = ws_misc + edivaddon.amount.
  end.
  
  export stream s-edi /* delimiter */ ","
    edivtran.invoice-no
    edivtran.cust-po
    edivtran.invoice-date
    edivtran.tot-gross
    ws_tax
    edivtran.tot-frt
    ws_misc
    .
  ASSIGN
    {rc/stampcht.i eddoc}
    eddoc.openitem = FALSE
    eddoc.stat = 9
    eddoc.status-flag = "SNT"
    eddoc.posted = TRUE
    .
END.
OUTPUT STREAM s-edi CLOSE.
TOP-DEBUG = FALSE.
RETURN.
 
