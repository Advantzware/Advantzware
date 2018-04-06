DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR li-unit AS INT NO-UNDO.


FOR EACH company NO-LOCK,
    EACH oe-ordl WHERE oe-ordl.company EQ company.company,
    FIRST itemfg
    WHERE itemfg.company EQ oe-ordl.company
      AND itemfg.i-no    EQ oe-ordl.i-no
    NO-LOCK:

  DISPLAY oe-ordl.company LABEL "Company"
          oe-ordl.ord-no  LABEL "Order#"
          oe-ordl.i-no    LABEL "FG Item#"
                          FORMAT "X(20)".

  ASSIGN
   li-cnt  = itemfg.case-count
   li-unit = itemfg.case-pall.

  IF oe-ordl.est-no NE "" THEN
    RUN oe/oe-cnt.p (RECID(oe-ordl), OUTPUT li-cnt, OUTPUT li-unit).

  IF oe-ordl.cas-cnt EQ 0 THEN oe-ordl.cas-cnt = li-cnt.

  IF oe-ordl.cases-unit EQ 0 THEN oe-ordl.cases-unit = li-unit.

  /*ASSIGN
   oe-ordl.cases   = TRUNC((oe-ordl.qty - oe-ordl.partial) / oe-ordl.cas-cnt,0)
   oe-ordl.partial = oe-ordl.qty - (oe-ordl.cases * oe-ordl.cas-cnt) NO-ERROR.

  IF oe-ordl.cases EQ ? THEN
    ASSIGN
     oe-ordl.cases   = 1
     oe-ordl.cas-cnt = oe-ordl.qty
     oe-ordl.partial = 0.*/
END.

HIDE ALL NO-PAUSE.

