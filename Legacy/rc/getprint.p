/* ed/asi/getprint.p - select printer and return to rpro */
  DEF OUTPUT param printfid AS char FORMAT "x(50)" NO-UNDO.
  DEF OUTPUT param printdest AS char NO-UNDO.
  DEF OUTPUT param printstring as char NO-UNDO.
  
  run ed/asi/getprint.p 
    (output printfid, output printdest, output printstring).
 
