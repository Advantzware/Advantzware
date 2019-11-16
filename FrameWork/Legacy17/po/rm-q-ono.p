
DEF PARAM BUFFER io-po-ordl FOR po-ordl.

DEF OUTPUT PARAM op-q-ono AS   DEC              NO-UNDO.

DEF VAR v-wid     LIKE item.s-wid       NO-UNDO.
DEF VAR v-len     LIKE item.s-len       NO-UNDO.
DEF VAR v-dep     LIKE item.s-dep       NO-UNDO.
DEF VAR v-bwt     LIKE item.basis-w     NO-UNDO.
DEF VAR ld        AS   DEC              NO-UNDO.


IF AVAIL io-po-ordl THEN
FIND FIRST item
    WHERE item.company EQ io-po-ordl.company
      AND item.i-no    EQ io-po-ordl.i-no
    NO-LOCK NO-ERROR.

IF AVAIL item THEN DO:
  ASSIGN
   v-len = io-po-ordl.s-len
   v-wid = io-po-ordl.s-wid
   v-dep = item.s-dep
   v-bwt = 0.

  {po/pol-dims.i io-}

  FOR EACH rm-rcpth
      WHERE rm-rcpth.company   EQ io-po-ordl.company
        AND rm-rcpth.po-no     EQ STRING(io-po-ordl.po-no)
        AND rm-rcpth.i-no      EQ io-po-ordl.i-no
        AND rm-rcpth.rita-code EQ "R"
      NO-LOCK,
      EACH rm-rdtlh
      WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
        AND rm-rdtlh.job-no  EQ io-po-ordl.job-no
        AND rm-rdtlh.job-no2 EQ io-po-ordl.job-no2
        AND rm-rdtlh.s-num   EQ io-po-ordl.s-num
      NO-LOCK:

    ld = rm-rdtlh.qty.

    IF rm-rcpth.pur-uom NE item.cons-uom THEN
      RUN sys/ref/convquom.p(rm-rcpth.pur-uom, item.cons-uom,
                             v-bwt, io-po-ordl.s-len, io-po-ordl.s-wid, item.s-dep,
                             ld, OUTPUT ld).

    op-q-ono = op-q-ono + ld.
  END.

  ld = io-po-ordl.ord-qty.

  IF io-po-ordl.pr-qty-uom NE item.cons-uom THEN
    RUN sys/ref/convquom.p(io-po-ordl.pr-qty-uom, item.cons-uom,
                           v-bwt, io-po-ordl.s-len, io-po-ordl.s-wid, item.s-dep,
                           ld, OUTPUT ld).

  IF item.cons-uom EQ "EA" THEN DO:
    {sys/inc/roundup.i ld}
    {sys/inc/roundup.i op-q-ono}
  END.

  op-q-ono = MAX(0,ld - op-q-ono).
END.
