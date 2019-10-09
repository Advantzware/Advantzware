DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED VAR WS_PARTNER AS CHAR NO-UNDO.
def shared stream s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
def var rmr01   as char no-undo format "x(02)" label "Ref ID Qual".
def var rmr02   as char no-undo format "x(30)" label "Ref ID".
def var rmr03   as char no-undo format "x(02)" label "Pmt Action".
def var rmr04   as dec  no-undo format ">>>,>>>,>>>.99CR" label "Net Amount".
def var rmr05   as dec  no-undo format ">>>,>>>,>>>.99CR" label "Invoice Amt".
def var rmr06   as dec  no-undo format ">>>,>>>,>>>.99CR" label "Discount Taken".
def var rmr07   as char no-undo format "x(02)" label "Adj Reason".
def var rmr08   as dec  no-undo format ">>>,>>>,>>>.99CR" label "Adj Amount".
IF ws_segment <> "RMR" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    if substring(str_buffa,71,1) > '9' then do:
        def var ws_char as char no-undo.
        run ed/tdf/iopunch.p (input substring(str_buffa,54,18),
            output ws_char).
        substring(str_buffa,54,18) = ws_char.    
    end.    
    ASSIGN
    {rc/substr.i rmr01          18  03}
    {rc/substr.i rmr02          21  30}
    {rc/substr.i rmr03          51  02}
    {ed/tdf/substrde.i rmr04    53  19  2}  
    {ed/tdf/substrde.i rmr05    72  19  2}  
    {ed/tdf/substrde.i rmr06    91  19  2}  
    {rc/substr.i rmr07         110  02}
    {ed/tdf/substrde.i rmr08   112  19  2}  
    .
  END.  
  END CASE.  /* version */
  assign
    total_amount1 = rmr04
    total_amount2 = rmr05
    total_amount3 = rmr06
    total_amount4 = rmr08.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
    {rc/outstr.i rmr01          18  03}
    {rc/outstr.i rmr02          21  30}
    {rc/outstr.i rmr03          51  02}
    {ed/tdf/outstrde.i rmr04    53  19  2}  
    {ed/tdf/outstrde.i rmr05    72  19  2}  
    {ed/tdf/outstrde.i rmr06    91  19  2}  
    {rc/outstr.i rmr07         110  02}
    {ed/tdf/outstrde.i rmr08   112  19  2}  
    .
  END.  
  END CASE.  /* version */
END.    /* O */
if command matches "*P*" then do:
  display stream s-out
    ws_segment
    rmr02 space(0) '/' space(0) rmr01 no-label
    /* rmr03 */ 
    rmr04 rmr05 rmr06 
    /* rmr07 rmr08 */
  with side-labels width 144 no-box.  
end.      
