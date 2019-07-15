/* -------------------------------------------------- ce/mach-ink.p 02/99 JLF */
/* create ink file for printing press routing                                 */
/* -------------------------------------------------------------------------- */

DEF PARAM BUFFER io-ef FOR ef.

DEF VAR li AS INT NO-UNDO.

{ce/mach-ink.i}


IF AVAIL io-ef THEN DO:
  FOR EACH eb
      WHERE eb.company EQ io-ef.company 
        AND eb.est-no  EQ io-ef.est-no
        AND eb.form-no EQ io-ef.form-no
      NO-LOCK:

    IF io-ef.est-type LE 4 THEN
    DO li = 1 TO EXTENT(eb.i-code2):
      IF eb.i-code2[li] GT "" THEN
        RUN do-work-file (eb.i-code2[li], eb.i-ps2[li]).
    END.

    ELSE
    DO li = 1 TO EXTENT(eb.i-code):
      RELEASE item.
      IF eb.i-code[li] GT "" THEN    
        RUN do-work-file (eb.i-code[li], eb.i-ps[li]).
    END.
  END.

  FOR EACH w-ink WHERE w-ink.form-no EQ io-ef.form-no
      BY w-ink.pass
      BY w-ink.i-no
      BY w-ink.inks + w-ink.varn + INT(w-ink.coat) DESC:
    FOR EACH b-ink
        WHERE b-ink.form-no  EQ w-ink.form-no
          AND b-ink.pass     EQ w-ink.pass
          AND b-ink.i-no     EQ w-ink.i-no
          AND b-ink.blank-no NE w-ink.blank-no:
      DELETE b-ink.
    END.
  END.
END.

RETURN.

PROCEDURE do-work-file:
  DEF INPUT PARAM ip-i-no LIKE w-ink.i-no NO-UNDO.
  DEF INPUT PARAM ip-pass LIKE w-ink.pass NO-UNDO.


  RELEASE item.

  IF ip-i-no NE "" THEN
  FIND FIRST item
      WHERE item.company EQ eb.company
        AND item.i-no    EQ ip-i-no
        AND INDEX("IV",item.mat-type) GT 0
      NO-LOCK NO-ERROR.  

  IF AVAIL item THEN DO:
    FIND FIRST w-ink
        WHERE w-ink.form-no  EQ eb.form-no
          AND w-ink.blank-no EQ eb.blank-no
          AND w-ink.i-no     EQ ip-i-no
          AND w-ink.pass     EQ ip-pass
        NO-ERROR.

    IF NOT AVAIL w-ink THEN DO:
      CREATE w-ink.
      ASSIGN
       w-ink.form-no  = eb.form-no
       w-ink.blank-no = eb.blank-no
       w-ink.i-no     = ip-i-no
       w-ink.pass     = ip-pass
       w-ink.press    = item.press-type.
    END.

    IF item.mat-type EQ "I" THEN
      w-ink.inks = w-ink.inks + 1.
    ELSE
    IF item.ink-type EQ "A" THEN
      w-ink.coat = YES.
    ELSE
      w-ink.varn = w-ink.varn + 1.
  END.

END PROCEDURE.

