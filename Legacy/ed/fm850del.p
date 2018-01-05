/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\FM850DEL.P
**       By: Chris Heins
** Descript: Cascading Delete of 850
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
FIND edpotran OF eddoc EXCLUSIVE-LOCK NO-ERROR.
FOR EACH edpoline OF edpotran:
  DELETE edpoline.
END.
FOR EACH edpoaddon OF edpotran:
  DELETE edpoaddon.
END.
DELETE edpotran.
