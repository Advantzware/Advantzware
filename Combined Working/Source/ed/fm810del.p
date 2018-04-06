/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\FM810DEL.P
**       By: Chris Heins
** Descript: Cascading Delete of 810
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
FIND edivtran OF eddoc EXCLUSIVE-LOCK NO-ERROR.
FOR EACH edivline OF edivtran:
  DELETE edivline.
END.
FOR EACH edivaddon OF edivtran:
  DELETE edivaddon.
END.
DELETE edivtran.
