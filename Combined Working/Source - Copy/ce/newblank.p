
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF VAR prev-style LIKE eb.style NO-UNDO.
DEF VAR prev-cust LIKE eb.cust-no NO-UNDO.
DEF VAR prev-ship LIKE eb.ship-id NO-UNDO.
DEF VAR prev-yrprice LIKE eb.yrprice NO-UNDO.
DEF VAR ls-part-no AS cha NO-UNDO.
DEFINE VARIABLE cPackCodeOverride AS CHARACTER NO-UNDO.
DEF VAR li AS INT NO-UNDO.


FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.

FOR EACH eb
    WHERE eb.company EQ ef.company
      AND eb.est-no  EQ ef.est-no
      AND eb.eqty    EQ ef.eqty
    NO-LOCK
    BY eb.form-no  DESC
    BY eb.blank-no DESC:
  prev-yrprice = eb.yrprice.
  LEAVE.
END.

FIND LAST eb NO-LOCK
    WHERE eb.company EQ ef.company
      AND eb.est-no  EQ ef.est-no
      AND eb.form-no NE 0
    USE-INDEX est-qty NO-ERROR.
IF AVAIL eb THEN
  ASSIGN
   prev-cust  = eb.cust-no
   prev-ship  = eb.ship-id
   prev-style = eb.style.

FIND LAST eb NO-LOCK
    WHERE eb.company EQ ef.company
      AND eb.est-no  EQ ef.est-no
      AND eb.form-no EQ ef.form-no
    USE-INDEX est-qty NO-ERROR.
li = IF AVAIL eb THEN eb.blank-no ELSE 0.

FIND FIRST eb
    WHERE eb.company  EQ ef.company 
      AND eb.est-no   EQ ef.est-no
      AND eb.form-no  EQ 0
      AND eb.blank-no EQ 0
    NO-LOCK NO-ERROR.
ls-part-no = IF AVAIL eb THEN eb.part-no ELSE "".

CREATE eb.
ASSIGN
 eb.est-type = ef.est-type
 eb.company  = ef.company
 eb.loc      = ef.loc
 eb.e-num    = ef.e-num
 eb.est-no   = ef.est-no
 eb.est-int  = INT(ef.est-no)
 eb.eqty     = ef.eqty
 eb.form-no  = ef.form-no
 eb.cust-seq = 1
 eb.blank-no = li + 1
 eb.part-no  = ls-part-no + IF ls-part-no NE "" THEN ("-" + STRING(eb.form-no) + "-" + STRING(eb.blank-no)) ELSE ""
 eb.cas-no   = ce-ctrl.def-case
 eb.tr-no    = ce-ctrl.def-pal
 eb.cust-no  = prev-cust
 eb.ship-id  = prev-ship
 eb.style    = prev-style
 eb.yrprice  = prev-yrprice
 eb.i-pass   = 0
 eb.cust-%   = INT(ef.est-type EQ 2).

RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
IF cPackCodeOverride GT "" THEN 
    eb.cas-no = cPackCodeOverride.
    
FIND FIRST ITEM WHERE ITEM.company = ef.company
                  AND ITEM.mat-type = "C"
                  AND item.i-no EQ eb.cas-no NO-LOCK NO-ERROR.
IF AVAIL item THEN DO:
   FIND FIRST e-item
          WHERE e-item.company EQ item.company
            AND e-item.loc     EQ item.loc
            AND e-item.i-no    EQ item.i-no   NO-LOCK NO-ERROR.
   FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ eb.stock-no   NO-LOCK NO-ERROR.
   IF AVAIL e-item THEN
      ASSIGN eb.cas-len = e-item.case-l
                 eb.cas-wid = e-item.case-w
                 eb.cas-dep = e-item.case-d
                 eb.cas-wt  = e-item.avg-w
                 eb.cas-pal = e-item.case-pall
                 eb.cas-cnt = IF AVAIL itemfg THEN   itemfg.case-count ELSE e-item.box-case.

   IF eb.cas-len EQ 0 THEN eb.cas-len = item.case-l.
   IF eb.cas-wid EQ 0 THEN eb.cas-wid = item.case-w.
   IF eb.cas-dep EQ 0 THEN eb.cas-dep = item.case-d.
   IF eb.cas-wt  EQ 0 THEN eb.cas-wt  = item.avg-w.
   IF eb.cas-pal EQ 0 THEN eb.cas-pal = item.case-pall.
   IF eb.cas-cnt EQ 0 THEN eb.cas-cnt =
            IF AVAIL itemfg THEN itemfg.case-count ELSE item.box-case.
END.

IF eb.est-type EQ 2 THEN DO:
  DEF BUFFER bfx-eb FOR eb.
  FIND FIRST bfx-eb
      WHERE bfx-eb.company  EQ eb.company
        AND bfx-eb.est-no   EQ eb.est-no 
        AND bfx-eb.form-no  EQ 0
        AND bfx-eb.blank-no EQ 0
      NO-LOCK NO-ERROR.
  IF AVAIL bfx-eb THEN eb.procat = bfx-eb.procat.
END.

op-rowid = ROWID(eb).
