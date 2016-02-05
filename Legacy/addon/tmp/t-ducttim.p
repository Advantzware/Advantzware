/* duct time test */
def var li-time as int no-undo.
def var li-hour as int no-undo.
def var li-mini as int no-undo.
def var li-dmin as int no-undo.
def var li-dtim as int no-undo.
def var li-ducttime as int no-undo.
def var li-dout as int no-undo.
def var li-do-min as int no-undo.

li-ducttime = 15.  /* 15 min */
li-time = 47650.
/* login In */

li-hour = truncate(li-time / 3600,0).
li-mini = li-time mod 3600.
li-mini = round(li-mini / 60,0).
li-dtim = trunc(li-mini / 15,0) + int(li-mini mod 15 > 0).
li-dmin = li-dtim * li-ducttime.

/* out */
li-dout = trunc(li-mini / 15,0).
li-do-min = li-dout * li-ducttime.

message li-time string(li-time,"hh:mm") li-time / 3600
        skip
        li-hour li-mini skip
        "IN: " li-hour ":" li-dmin   "   Out:" li-hour ":" li-do-min
        
        
          view-as alert-box.
