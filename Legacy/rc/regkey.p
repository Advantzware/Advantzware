/***************************************************************************\
*****************************************************************************
**  Program: C:\rprodemo\RC\REGKEY.P
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
{rc/loginv.i}
DEF INPUT PARAM p_key AS CHAR NO-UNDO.
DEF OUTPUT PARAM ws_value AS CHAR NO-UNDO.
RETURN.
/*
DEF STREAM s-key.
FIND FIRST rpro.registry
  WHERE rpro.registry.co = ws_co AND rpro.registry.key = p_key NO-LOCK NO-ERROR.
IF NOT AVAIL rpro.registry THEN
DO:
  FIND FIRST rpro.registry
    WHERE rpro.registry.key = p_key NO-LOCK NO-ERROR.
  IF NOT AVAIL rpro.registry THEN
  DO:
    INPUT FROM TERMINAL.
    CREATE rpro.registry.
    ASSIGN rpro.registry.co = ws_co
      rpro.registry.key = p_key
      rpro.registry.description = "Created on-the-fly from " +
      PROGRAM-NAME(2)
      rpro.registry.app = ws_app.
    UPDATE
      rpro.registry
      WITH FRAME f-reg CENTER TITLE "Please Touchup rpro.registry Entry"
      1 COLUMN ROW 4 OVERLAY.
    HIDE FRAME f-reg NO-PAUSE.
  END.
END.
IF rpro.registry.type = 'l' THEN
ws_value =
STRING(rpro.registry.V-Logical,'Yes/No').
ELSE
IF rpro.registry.type = 'c' THEN
ws_value = rpro.registry.V-Char.
ELSE
IF rpro.registry.type = 'i' THEN
ws_value = STRING(rpro.registry.V-Integer).
*/
