/* -------------------------------------------------- est/calc-whs.p 01/03 JLF */

DEF INPUT  PARAM ip-sub AS INT NO-UNDO.
DEF OUTPUT PARAM op-cewhspct AS LOG NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef FOR ef.
DEF SHARED BUFFER xeb FOR eb.

{ce/print4.i SHARED SHARED}
{cec/print42.i SHARED}

DEF VAR v-qty     AS DEC NO-UNDO.
DEF VAR v-rel-qty AS DEC NO-UNDO.
DEF VAR v-pal-qty AS DEC NO-UNDO.

{sys/inc/cewhschg.i}

IF cewhschg-cha BEGINS "$" THEN DO:
  ASSIGN
   op-cewhspct = NO
   ctrl2[1]    = 0.

  FOR EACH brd
      WHERE (brd.form-no EQ v-form-no OR (NOT vmclean2))
        AND CAN-FIND(FIRST ITEM WHERE item.company  EQ cocode
                                  AND item.i-no     EQ brd.i-no
                                  AND item.mat-type EQ "D"),
      FIRST xeb
      WHERE xeb.company  EQ cocode
        AND xeb.est-no   EQ xest.est-no
        AND xeb.form-no  EQ brd.form-no
        AND xeb.blank-no EQ brd.blank-no
      NO-LOCK:
    
    v-qty = brd.cost * brd.qty / brd.cost-m * 1000.

    {sys/inc/roundup.i v-qty}

    v-rel-qty = v-qty / rels[ip-sub].

    {sys/inc/roundup.i v-rel-qty}

    v-pal-qty = v-qty / brd.qty.

    {sys/inc/roundup.i v-pal-qty}

    DO i = 1 TO rels[ip-sub] - 1:
      
    END.
  END.
END.

ELSE op-cewhspct = YES.
