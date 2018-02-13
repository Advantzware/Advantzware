/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\n2.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED STREAM s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF ws_segment <> "N2" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    name2                           18  60}
      {rc/substr.i    name3                           78  60}
      .
  END.
  OTHERWISE
  DO:
    ASSIGN
      {rc/substr.i    name2                           18  35}
      {rc/substr.i    name3                           53  87}
      .
  END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  IF name2 = "" AND name3 = ""
    THEN
  DO:
    RUN rc/debugmsg.p ("Mandatory elements missing in N2 (name2|name3)" ).
    RETURN error.
  END.
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    name2                           18  60}
      {rc/substr.i    name3                           78  60}
      .
  END.
  OTHERWISE
  DO:
    ASSIGN
      {rc/substr.i    name2                           18  35}
      {rc/substr.i    name3                           53  87}
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    name2  LABEL "Name2"
    name3  LABEL "Name3"
    WITH side-labels width 144 no-box.
END.
