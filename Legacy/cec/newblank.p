
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF BUFFER bb FOR eb.

DEF VAR prev-cust LIKE eb.cust-no NO-UNDO.
DEF VAR prev-ship LIKE eb.ship-id NO-UNDO.
DEF VAR ls-part-no AS cha NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEFINE VARIABLE cPackCodeOverride AS CHARACTER NO-UNDO.

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.

FIND FIRST bb
    WHERE bb.company  EQ ef.company 
      AND bb.est-no   EQ ef.est-no
      AND bb.form-no  EQ 0
      AND bb.blank-no EQ 0
    NO-LOCK NO-ERROR.
ls-part-no = IF AVAIL bb THEN bb.part-no ELSE "".

FIND LAST bb NO-LOCK
    WHERE bb.company EQ ef.company
      AND bb.est-no  EQ ef.est-no
      AND bb.form-no NE 0
    USE-INDEX est-qty NO-ERROR.
IF AVAIL bb THEN
  ASSIGN
   prev-cust = bb.cust-no
   prev-ship = bb.ship-id.

FIND LAST bb NO-LOCK
    WHERE bb.company EQ ef.company
      AND bb.est-no  EQ ef.est-no
      AND bb.form-no EQ ef.form-no
    USE-INDEX est-qty NO-ERROR.
li = IF AVAIL bb THEN bb.blank-no ELSE 0.


CREATE eb. 
ASSIGN
 eb.est-type  = ef.est-type
 eb.company   = ef.company
 eb.loc       = ef.loc
 eb.e-num     = ef.e-num
 eb.est-no    = ef.est-no
 eb.est-int   = INT(ef.est-no)
 eb.eqty      = ef.eqty
 eb.form-no   = ef.form-no
 eb.blank-no  = li + 1
 eb.cust-seq  = 1
 eb.cas-no    = ce-ctrl.def-case
 eb.tr-no     = ce-ctrl.def-pal
 eb.cust-no   = prev-cust
 eb.ship-id   = prev-ship
 eb.tr-cas    = 1
 eb.i-pass    = 0
 eb.quantityPerSet   = 1
 eb.yld-qty   = 1
 eb.tab-in    = YES
 eb.len       = 0
 eb.wid       = 0
 eb.dep       = 0
 eb.procat    = IF AVAIL bb THEN bb.procat ELSE ""
 eb.flute     = ef.flute
 eb.test      = ef.test.


RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
IF cPackCodeOverride GT "" THEN 
    eb.cas-no = cPackCodeOverride.
IF ef.est-type EQ 6 AND ls-part-no NE "" THEN DO:
  li = 1.
  FOR EACH bb
      WHERE bb.company  EQ ef.company
        AND bb.est-no   EQ ef.est-no
        AND bb.form-no  NE 0
        AND bb.blank-no NE 0
        AND ROWID(bb)   NE ROWID(eb)
      NO-LOCK
      BY bb.form-no BY bb.blank-no:
    li = li + 1.
  END.
  DO WHILE TRUE:
    IF NOT CAN-FIND(FIRST bb
                    WHERE bb.company EQ ef.company
                      AND bb.est-no  EQ ef.est-no
                      AND bb.part-no EQ ls-part-no + "-" + STRING(li)
                      AND ROWID(bb)  NE ROWID(eb))
    THEN DO:
      eb.part-no = ls-part-no + "-" + STRING(li).
      LEAVE.
    END.
    li = li + 1.
  END.
END.

FIND FIRST item
    WHERE item.company  EQ eb.company
      AND item.mat-type EQ "C"  /* Case/Bundle */
      AND item.i-no     EQ eb.cas-no
    NO-LOCK NO-ERROR.
IF AVAIL item THEN DO:
  FIND FIRST e-item
      WHERE e-item.company EQ item.company
        AND e-item.loc     EQ item.loc
        AND e-item.i-no    EQ item.i-no
      NO-LOCK NO-ERROR.
  FIND FIRST itemfg
      WHERE itemfg.company EQ eb.company
        AND itemfg.i-no    EQ eb.stock-no
      NO-LOCK NO-ERROR.
  IF AVAIL e-item THEN
    ASSIGN
     eb.cas-len = e-item.case-l
     eb.cas-wid = e-item.case-w
     eb.cas-dep = e-item.case-d
     eb.cas-wt  = e-item.avg-w
     eb.cas-pal = e-item.case-pall
     eb.cas-cnt = IF AVAIL itemfg THEN itemfg.case-count ELSE e-item.box-case
              .
  IF eb.cas-len EQ 0 THEN eb.cas-len = item.case-l.
  IF eb.cas-wid EQ 0 THEN eb.cas-wid = item.case-w.
  IF eb.cas-dep EQ 0 THEN eb.cas-dep = item.case-d.
  IF eb.cas-wt  EQ 0 THEN eb.cas-wt  = item.avg-w.
  IF eb.cas-pal EQ 0 THEN eb.cas-pal = item.case-pall.
  IF eb.cas-cnt EQ 0 THEN eb.cas-cnt =
            IF AVAIL itemfg THEN itemfg.case-count ELSE item.box-case.
END.  /* avail item */
    
IF eb.est-type EQ 6 THEN DO:
  FIND FIRST bb
      WHERE bb.company  EQ eb.company
        AND bb.est-no   EQ eb.est-no 
        AND bb.form-no  EQ 0
        AND bb.blank-no EQ 0
      NO-LOCK NO-ERROR.
  IF AVAIL bb THEN eb.procat = bb.procat.
END.

op-rowid = ROWID(eb).
