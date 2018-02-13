/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\in1.p
**       By:
** Descript:
10.09.98 by CAH on \\ricky\rv8 Log#0000:
1.  Added ZZ = mark_for_store on inbound.
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
def shared stream s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
def var ENT01   AS DECIMAL FORMAT '>>>>>9'  LABEL "Assigned_number"     no-undo.
IF ws_segment <> "ENT" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
    {ed/tdf/substrde.i  ENT01                 18  03 0}
    .
  END.  
  END CASE.  /* version */
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
    {ed/tdf/outstrde.i  ENT01                 18  03 0}
    .
  END.  
  END CASE.  /* version */
END.    /* O */
if command matches "*P*" then do:
    display stream s-out
        ws_segment
        ENT01   label "Seq#"
    with side-labels width 144 no-box.  
end.      
