/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\asi\expsh
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
DEF STREAM s-in.
DEF STREAM s-out.
DEF var c AS char NO-UNDO extent 10.
DEF var in_fname AS char NO-UNDO FORMAT "x(20)"
  LABEL "Input from".
DEF var out_fname LIKE in_fname NO-UNDO
  LABEL "Output to".
UPDATE ws_partner in_fname out_fname
  WITH FRAME f-top CENTER side-labels.
INPUT  STREAM s-in FROM VALUE(in_fname).
OUTPUT STREAM s-out TO VALUE(out_fname).
REPEAT:
  c = "".
  IMPORT STREAM s-in c.
  c[1] = TRIM(c[1]).
  IF c[1] <= "" THEN
  NEXT.
  DISPLAY c[1] WITH FRAME f-current 1 DOWN CENTER.
  FIND FIRST edshipto
    WHERE edshipto.partner = ws_partner
    AND edshipto.by-code = c[1] NO-LOCK NO-ERROR.
  IF AVAIL edshipto THEN
  DO:
    DISPLAY name addr1 city state zip
      WITH FRAME f-current.
    EXPORT STREAM s-out
      DELIMITER ","
      by-code
      name
      attention
      addr1
      addr2
      city
      state
      zip
      phone
      dest-zone
      st-code
      .
    PAUSE 0.
  END.
END.
