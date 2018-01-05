/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\in3.p
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
IF ws_segment <> "N3" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    address1                            18  55}
      {rc/substr.i    address2                            73  55}
      .
  END.
  OTHERWISE
    DO:
    ASSIGN
      {rc/substr.i    address1                            18  35}
      {rc/substr.i    address2                            53  87}
      .
  END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  IF address1 = "" AND address2 = "" THEN
  RETURN error.
  IF address1 EQ "" AND address2 GT "" THEN 
    ASSIGN address1 = address2
           address2 = ""
           . 
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/outstr.i    address1                            18  55}
      {rc/outstr.i    address2                            73  55}
      .
  END.
  OTHERWISE
    DO:
    ASSIGN
      {rc/outstr.i    address1                            18  35}
      {rc/outstr.i    address2                            53  87}
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    address1 LABEL "Addr1"
    address2 LABEL "Addr2"
    WITH side-labels width 144 no-box.
END.
