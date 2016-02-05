/* -------------------------------------------------- ce/mach-ink.p 02/99 JLF */
/* create ink file for printing press routing                                 */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i SHARED}

DEF SHARED BUFFER xest FOR est.

DEF BUFFER bf-ef FOR ef.
DEF BUFFER bf-eb FOR eb.

DEF VAR v-inks AS   INT NO-UNDO.
DEF VAR v-coat AS   INT NO-UNDO.
DEF VAR v-c-on AS   LOG NO-UNDO.
DEF VAR li     AS   INT NO-UNDO.

{ce/mach-ink.i}


FOR EACH w-ink:
  DELETE w-ink.
END.

FOR EACH bf-ef
    WHERE bf-ef.company EQ xest.company
      AND bf-ef.est-no  EQ xest.est-no
    TRANSACTION:

  FOR EACH bf-eb NO-LOCK
      WHERE bf-eb.company EQ bf-ef.company 
        AND bf-eb.est-no  EQ bf-ef.est-no
        AND bf-eb.form-no EQ bf-ef.form-no:

    IF bf-eb.i-col    GT bf-ef.f-col    THEN bf-ef.f-col    = bf-eb.i-col.
    IF bf-eb.i-pass   GT bf-ef.f-pass   THEN bf-ef.f-pass   = bf-eb.i-pass.
    IF bf-eb.i-coat   GT bf-ef.f-coat   THEN bf-ef.f-coat   = bf-eb.i-coat.
    IF bf-eb.i-coat-p GT bf-ef.f-coat-p THEN bf-ef.f-coat-p = bf-eb.i-coat-p.
  END.
END.

FOR EACH bf-ef NO-LOCK
    WHERE bf-ef.company EQ xest.company
      AND bf-ef.est-no  EQ xest.est-no:

  RUN ce/mach-ink1.p (BUFFER bf-ef).

  FOR EACH w-ink
      WHERE w-ink.form-no EQ bf-ef.form-no
      BREAK BY w-ink.pass BY w-ink.blank-no:

    IF FIRST-OF(w-ink.pass) THEN
      ASSIGN
       v-inks = 0
       v-coat = 0
       v-c-on = NO.

    ASSIGN
     v-inks = v-inks + w-ink.inks
     v-coat = v-coat + w-ink.varn.

    IF w-ink.coat THEN v-c-on = YES.

    IF LAST-OF(w-ink.pass) THEN
      ASSIGN
       w-ink.inks = v-inks
       w-ink.varn = v-coat
       w-ink.coat = v-c-on.
  END.

  FOR EACH w-ink
      WHERE w-ink.form-no EQ bf-ef.form-no
      BREAK BY w-ink.pass
            BY w-ink.inks + w-ink.varn + INT(w-ink.coat) DESC:

    IF NOT FIRST-OF(w-ink.pass) THEN DELETE w-ink.
  END.

  IF xest.est-type EQ 4 OR xest.est-type EQ 8       OR
     ((xest.est-type EQ 2 OR xest.est-type EQ 6) AND
      bf-ef.blank-qty GT 1)                         THEN
  FOR EACH w-ink
      WHERE w-ink.form-no EQ bf-ef.form-no
      BREAK BY w-ink.pass:

    IF FIRST(w-ink.pass) AND LAST(w-ink.pass) THEN DO:
      IF bf-ef.f-col / bf-ef.f-pass GT w-ink.inks          OR
         (bf-ef.f-pass EQ 1 AND bf-ef.f-col LT w-ink.inks) THEN
        w-ink.inks = bf-ef.f-col / bf-ef.f-pass.

      IF bf-ef.f-coat-p GT 0 AND w-ink.varn GT 0 THEN
        IF bf-ef.f-coat / bf-ef.f-coat-p GT w-ink.varn OR
           (bf-ef.f-coat-p EQ 1 AND bf-ef.f-coat GT w-ink.varn) THEN
          w-ink.varn = bf-ef.f-coat / bf-ef.f-coat-p.
    END.
  END.
END.

