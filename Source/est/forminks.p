/* -------------------------------------------------- ce/mach-ink.p 02/99 JLF */
/* create ink file for printing press routing                                 */
/* -------------------------------------------------------------------------- */

DEF PARAM BUFFER io-ef FOR ef.

DEF VAR li AS INT NO-UNDO.

{ce/mach-ink.i NEW}


IF AVAIL io-ef THEN DO:
  FOR EACH eb NO-LOCK
      WHERE eb.company EQ io-ef.company 
        AND eb.est-no  EQ io-ef.est-no
        AND eb.form-no EQ io-ef.form-no:

    IF eb.i-col    GT io-ef.f-col    THEN io-ef.f-col    = eb.i-col.
    IF eb.i-pass   GT io-ef.f-pass   THEN io-ef.f-pass   = eb.i-pass.
    IF eb.i-coat   GT io-ef.f-coat   THEN io-ef.f-coat   = eb.i-coat.
    IF eb.i-coat-p GT io-ef.f-coat-p THEN io-ef.f-coat-p = eb.i-coat-p.
  END.

  RUN ce/mach-ink1.p (BUFFER io-ef).

  IF NOT CAN-FIND(FIRST w-ink
                  WHERE w-ink.inks + w-ink.varn + INT(w-ink.coat) GT 1) THEN
  FOR EACH w-ink BREAK BY w-ink.pass BY w-ink.i-no:
    IF FIRST-OF(w-ink.pass) THEN DO:
      IF FIRST(w-ink.pass) THEN
        ASSIGN
         io-ef.f-col    = 0
         io-ef.f-pass   = 0
         io-ef.f-coat   = 0
         io-ef.f-coat-p = 0.

      IF CAN-FIND(FIRST b-ink WHERE b-ink.pass EQ w-ink.pass
                                AND b-ink.inks GT 0) THEN
        io-ef.f-pass   = io-ef.f-pass + 1.

      IF CAN-FIND(FIRST b-ink WHERE b-ink.pass                   EQ w-ink.pass
                                AND b-ink.varn + INT(b-ink.coat) GT 0) THEN
        io-ef.f-coat-p = io-ef.f-coat-p + 1.
    END.

    ASSIGN
     io-ef.f-col  = io-ef.f-col  + w-ink.inks
     io-ef.f-coat = io-ef.f-coat + w-ink.varn + INT(w-ink.coat).
  END.
END.

