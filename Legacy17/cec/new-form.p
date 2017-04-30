
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF VAR li AS INT NO-UNDO.


FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

FIND est WHERE ROWID(est) EQ ip-rowid NO-LOCK NO-ERROR.

FIND FIRST est-qty 
    WHERE est-qty.company EQ est.company
      AND est-qty.est-no  EQ est.est-no
    NO-LOCK NO-ERROR. 

FIND LAST ef 
    WHERE ef.company EQ est-qty.company
      AND ef.est-no  EQ est-qty.est-no
      AND ef.eqty    EQ est-qty.eqty
    USE-INDEX est-qty NO-LOCK NO-ERROR.
li = (IF AVAIL ef THEN ef.form-no ELSE 0) + 1.

CREATE ef. 
ASSIGN
 ef.est-type   = est.est-type
 ef.company    = est.company
 ef.loc        = est.loc
 ef.e-num      = est.e-num
 ef.est-no     = est.est-no
 ef.eqty       = est-qty.eqty
 ef.form-no    = li
 ef.cust-seq   = 1
 ef.lsh-wid    = ce-ctrl.ls-length
 ef.lsh-len    = ce-ctrl.ls-width
 ef.cost-uom  = "MSF".

RUN cec/newblank.p (ROWID(ef), OUTPUT op-rowid).
