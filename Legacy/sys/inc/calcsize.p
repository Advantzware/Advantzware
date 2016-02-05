  
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-size LIKE quoteitm.size NO-UNDO.
{sys/inc/VAR.i SHARED}
DEF VAR k_frac AS DEC INIT "6.25" NO-UNDO.
DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
DEF VAR lv-format AS CHAR INIT ">>>>>9.9<<<<" NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
DEF VAR op-size-blank AS LOG NO-UNDO.

{sys/inc/f16to32.i}

FIND eb NO-LOCK WHERE ROWID(eb) EQ ip-rowid NO-ERROR.

IF AVAIL eb THEN
FIND FIRST est NO-LOCK
    WHERE est.company EQ eb.company
      AND est.est-no  EQ eb.est-no
    NO-ERROR.

IF AVAIL est THEN
  IF est.est-type LE 4 THEN DO:
    IF est.metric THEN
      ASSIGN
       ld-metric = 25.4
       lv-format = "->>,>>>mm".

    IF est.est-type EQ 2                         AND
       eb.len EQ 0 AND eb.wid EQ 0 AND eb.dep EQ 0 THEN
      ASSIGN
       ld-len = eb.tr-len
       ld-wid = eb.tr-wid
       ld-dep = eb.tr-dep.
    ELSE
      ASSIGN
       ld-len = eb.len
       ld-wid = eb.wid
       ld-dep = eb.dep.

    ASSIGN
     ld-len = ld-len * ld-metric
     ld-wid = ld-wid * ld-metric
     ld-dep = ld-dep * ld-metric.

    IF est.metric THEN DO:
      {sys/inc/roundup.i ld-len}
      {sys/inc/roundup.i ld-wid}
      {sys/inc/roundup.i ld-dep}
    END.
  END.

  ELSE DO:
    lv-format = ">>>>9.99".

    /*if set dimensions blank, dimensions on quoteitm.size should be
      blank*/
    IF est.est-type EQ 6 THEN
    DO:
       IF NOT(eb.len EQ 0 AND eb.wid EQ 0 AND eb.dep EQ 0) THEN
         ASSIGN
          ld-len = eb.len
          ld-wid = eb.wid
          ld-dep = eb.dep.
       ELSE op-size-blank = YES.
    END.
    ELSE
    DO:
       IF eb.len EQ 0 AND eb.wid EQ 0 AND eb.dep EQ 0 THEN
         ASSIGN
          ld-len = eb.tr-len
          ld-wid = eb.tr-wid
          ld-dep = eb.tr-dep.
       ELSE
         ASSIGN
          ld-len = eb.len
          ld-wid = eb.wid
          ld-dep = eb.dep.
    END.

   ASSIGN
    ld-len = {sys/inc/k16v.i ld-len}
    ld-wid = {sys/inc/k16v.i ld-wid}
    ld-dep = {sys/inc/k16v.i ld-dep}.
  END.

IF NOT op-size-blank THEN
   op-size = TRIM(STRING(ld-len,lv-format)) + " x " +
             TRIM(STRING(ld-wid,lv-format)) + " x " +
             TRIM(STRING(ld-dep,lv-format)).
