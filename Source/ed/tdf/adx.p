DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
def shared stream s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
def var ADX01   AS DECIMAL FORMAT '->>>,>>>,>>>.99CR'  LABEL "Amount"     no-undo.
def var adx02   as char no-undo format "x(02)" label "Adj Reason".
def var adx02d  as char no-undo format 'x(30)' label "-".
def var adx03   as char no-undo format "x(30)" label "Ref Qual".
def var adx04   as char no-undo format "x(30)" label "Ref ID".
IF ws_segment <> "ADX" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    if substring(str_buffa,36,1) > '9' then do:
        def var ws_char as char no-undo.
        run ed/tdf/iopunch.p (input substring(str_buffa,19,18),
            output ws_char).
        substring(str_buffa,19,18) = ws_char.    
    end.    
    ASSIGN
    {ed/tdf/substrde.i  ADX01               18  19 2}
    {rc/substr.i    ADX02                   37  02}
    {rc/substr.i    ADX03                   39  03}
    {rc/substr.i    ADX04                   42  30}
    .
  END.  
  END CASE.  /* version */
  
  case adx02:
    when "01" then adx02d = "PRICING ERROR".
    WHEN "06" THEN ADX02D = "QUANTITY CONTESTED".
    WHEN "12" THEN ADX02D = "RETURNS - QUALITY".
    WHEN "16" THEN ADX02D = "NON-INVOICE RELATED ALLOWANCE/CHARGE".
  END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
    {ed/tdf/outstrde.i  ADX01               18  19 2}
    {rc/outstr.i    ADX02                   37  02}
    {rc/outstr.i    ADX03                   39  03}
    {rc/outstr.i    ADX04                   42  30}
    .
  END.  
  END CASE.  /* version */
END.    /* O */
if command matches "*P*" then do:
    display stream s-out
        ws_segment
        ADX01
        adx02  
        adx02d  
        adx04 space(0) "/" space(0) adx03 no-label
    with side-labels width 144 no-box.  
end.      
