/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\FM832DEL.P
**       By: Chris Heins
** Descript: Cascading Delete of 832
11.06.97 by CAH on \\ricky\robj8\ Log#0000:
1.  Initial write from ed/fm810del.p.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
DEF INPUT PARAM doc_rec AS RECID NO-UNDO.
FIND eddoc WHERE RECID(eddoc) = doc_rec NO-LOCK NO-ERROR.
IF NOT AVAIL eddoc THEN
DO:
  BELL.
  MESSAGE "Could not locate eddoc in cascade deletion: " PROGRAM-NAME(1).
  PAUSE.
  RETURN.
END.
FIND edcat OF eddoc EXCLUSIVE-LOCK NO-ERROR.
FOR EACH edcatline OF edcat:
  DELETE edcatline.
END.
FOR EACH edcatprice OF edcat:
  DELETE edcatprice.
END.
DELETE edcat.
