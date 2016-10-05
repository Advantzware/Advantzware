/* rm-adj.i */

def var x as int no-undo.
def buffer xrm-rcpt for rm-rcpt.

x = 1.
FIND LAST xrm-rcpt WHERE recid(xrm-rcpt) ne recid({&FIRST-EXTERNAL-TABLE})
    USE-INDEX r-no NO-LOCK NO-ERROR.
IF AVAILABLE xrm-rcpt THEN
x = xrm-rcpt.r-no + 1.
FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
IF AVAILABLE rm-rcpth AND rm-rcpth.r-no >= x THEN
x = rm-rcpth.r-no + 1.

ASSIGN {&FIRST-EXTERNAL-TABLE}.company    = gcompany
       {&FIRST-EXTERNAL-TABLE}.rita-code  = "A"
       {&FIRST-EXTERNAL-TABLE}.r-no       = x  
       {&FIRST-EXTERNAL-TABLE}.loc        = gloc.

