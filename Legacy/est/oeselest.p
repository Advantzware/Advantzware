
DEF NEW SHARED VAR xcal AS DEC NO-UNDO.

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xeb FOR eb.

DEF VAR lv-form-no LIKE eb.form-no INIT 0 NO-UNDO.


IF AVAIL xeb THEN DO:
    FIND FIRST xest NO-LOCK WHERE 
        xest.company EQ xeb.company AND 
        xest.est-no EQ xeb.est-no 
        NO-ERROR.  
  lv-form-no = xeb.form-no.
END.

IF AVAIL xest THEN
DO:
   IF xest.est-type LE 4 THEN
      RUN ce/com/mach-seq.p (lv-form-no).
   ELSE
      RUN cec/mach-seq.p (lv-form-no,0,xest.est-type EQ 8).
END.
    
    
