/***************************************************************************\
*****************************************************************************
**  Program: E:\robj8\patch\ed\tdf\ctt.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
def shared stream s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF ws_segment <> "CTT" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {ed/tdf/substrde.i    number_of_line_items          18  7  0}
    .
  CASE ws_version:
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  ASSIGN
    {ed/tdf/outstrde.i    number_of_line_items          18  7  0}
    .
  CASE ws_version:
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    number_of_line_items        label "#Detail lines"   format "->>>>>9"
    skip(1)
    WITH side-labels width 144 no-box.
END.
