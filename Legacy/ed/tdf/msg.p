/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\imsg.
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
IF ws_segment <> "MSG" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i    message_text                    18  264}
    {rc/substr.i    printer_cc_code                282    2}
    .
  CASE ws_version:
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  ASSIGN
    {rc/outstr.i    message_text                    18  264}
    {rc/outstr.i    printer_cc_code                282    2}
    .
  CASE ws_version:
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    message_text    LABEL "Text"        FORMAT 'x(120)'
    WITH side-labels width 144 no-box.
END.
