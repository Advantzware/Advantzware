/* ed/asi/getprint.p - select printer and return to rpro */
{sys/inc/var.i shared}
def var v-start-compress as char.
def var save_id as recid no-undo.
  DEF OUTPUT param printfid AS char FORMAT "x(50)" NO-UNDO.
  DEF OUTPUT param printdest AS char NO-UNDO.
  DEF OUTPUT param printstring as char NO-UNDO.
{sys/form/s-top.f}
{sys/inc/print2.i}
if avail printer then assign
    printfid = pr-name
    printdest = pr-port
    printstring = v-start-compress
    .
