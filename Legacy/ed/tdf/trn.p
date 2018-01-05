DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa  AS char NO-UNDO.
DEF OUTPUT PARAM erc        AS int NO-UNDO.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
def shared stream s-out.
def var trn01   as char     format "x(02)"  label "Trace Type"          no-undo.
def var trn02   as char     format "x(30)"  label "Ref ID"              no-undo.
def var trn03   as char     format "x(10)"  label "Orig Company"        no-undo.
def var trn04   as char     format "x(30)"  label "Ref ID"              no-undo.
IF ws_segment <> "TRN" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
    CASE ws_version:
    OTHERWISE DO:
  ASSIGN
        {rc/substr.i    trn01       18  02}
        {rc/substr.i    trn02       20  30}
        {rc/substr.i    trn03       50  10}
        {rc/substr.i    trn04       60  30}
        .
        
    end.
    END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
    CASE ws_version:
    OTHERWISE DO:
  ASSIGN
        {rc/outstr.i    trn01       18  02}
        {rc/outstr.i    trn02       20  30}
        {rc/outstr.i    trn03       50  10}
        {rc/outstr.i    trn04       60  30}
    .
        end.
    END CASE.    
END.    /* O */
if command matches "*P*" then do:
    display stream s-out
 ws_segment    
 trn01
 trn02
 trn03
 trn04
    with no-box side-labels width 144.
        
end.
