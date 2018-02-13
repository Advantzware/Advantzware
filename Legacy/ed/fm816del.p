/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\fm816del.
**       By:
** Descript:
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
ws_char = string(eddoc.seq).
FOR EACH edshipto
    WHERE edshipto.partner = eddoc.partner
    AND edshipto.description = ws_char:
  DELETE edshipto.
END.
