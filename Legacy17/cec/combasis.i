/* ------------------------------------------------- cec/combasis.i 10/96 JLF */

DEF VAR v-basis LIKE sman.commbasis  INIT ""    NO-UNDO.
DEF VAR v-com   LIKE eb.comm         INIT 0     NO-UNDO.
DEF VAR v-pct   LIKE eb.comm         INIT 0     NO-UNDO.


{sys/inc/cecomm.i}

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

v-pct = ce-ctrl.prof-mrkup.

FIND FIRST xeb
    WHERE xeb.company  EQ xest.company
      AND xeb.est-no   EQ xest.est-no
      AND xeb.form-no  NE 0
    NO-LOCK NO-ERROR.

IF AVAIL xeb THEN DO:
  v-com = xeb.comm.

  FIND FIRST cust
      WHERE cust.company eq cocode
        AND cust.cust-no eq xeb.cust-no
      NO-LOCK NO-ERROR.

  IF AVAIL cust THEN DO:
    RUN custom/combasis.p (cocode, xeb.sman, cust.type, xeb.procat, 0,
                           cust.cust-no,
                           OUTPUT v-basis).

    IF cust.markup NE 0 THEN v-pct = cust.markup.
  END.
END.

IF NOT cecomm-log THEN v-com = 0.
