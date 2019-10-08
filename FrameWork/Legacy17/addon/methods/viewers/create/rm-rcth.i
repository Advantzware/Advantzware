/* rm-rcth.i */

def var x as int no-undo.
def buffer xrm-rcth for rm-rcth.

x = 1.
FIND LAST xrm-rcth WHERE recid(xrm-rcth) ne recid({&FIRST-EXTERNAL-TABLE})
    USE-INDEX r-no NO-LOCK NO-ERROR.
IF AVAILABLE xrm-rcth THEN
x = xrm-rcth.r-no + 1.
FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
IF AVAILABLE rm-rcpth AND rm-rcpth.r-no >= x THEN
x = rm-rcpth.r-no + 1.

ASSIGN {&FIRST-EXTERNAL-TABLE}.company    = gcompany
       {&FIRST-EXTERNAL-TABLE}.rita-code  = "R"
       {&FIRST-EXTERNAL-TABLE}.r-no       = x  
       {&FIRST-EXTERNAL-TABLE}.loc        = gloc.

