
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF VAR ld-qty AS DEC NO-UNDO.


ASSIGN
 cocode = g_company
 locode = g_loc.

PAUSE 0 BEFORE-HIDE.

FOR EACH po-ordl
    WHERE po-ordl.company   EQ cocode
      AND po-ordl.item-type EQ YES
    USE-INDEX item,
    first po-ord
    where po-ord.company eq cocode
      and po-ord.po-no   eq po-ordl.po-no
    NO-LOCK,
    FIRST item
    WHERE item.company  EQ po-ordl.company
      AND item.i-no     EQ po-ordl.i-no
      /*AND item.i-code   EQ "R"
      AND item.mat-type EQ "M"
      AND item.stocked  EQ NO*/   
    NO-LOCK

    BREAK BY po-ordl.i-no
          BY po-ordl.po-no:

  DISPLAY "Processing RMItem#/PO#: " +
          TRIM(po-ordl.i-no) + "/" +
          TRIM(STRING(po-ordl.po-no,">>>>>>>>>>")) FORMAT "x(50)" WITH 1 DOWN.

  for each reftable
      {ap/ap-reftbW.i po-ordl.po-no}
      no-lock,
    
      each ap-inv
      where ap-inv.company eq cocode
        and ap-inv.i-no    eq int(reftable.code2)
        and ap-inv.vend-no eq po-ord.vend-no
        and (ap-inv.po-no  eq po-ordl.po-no or ap-inv.po-no eq 0)
        and ap-inv.posted  eq yes
      use-index i-no no-lock,
        
      each ap-invl
      where ap-invl.i-no       eq ap-inv.i-no
        and (ap-invl.po-no     eq po-ordl.po-no or ap-inv.po-no ne 0)
        and {ap/invlline.i -1} eq po-ordl.line
      use-index i-no no-lock
      BREAK BY reftable.reftable:

    IF FIRST(reftable.reftable) THEN po-ordl.t-inv-qty = 0.

    ld-qty = ap-invl.qty.

    IF ap-invl.cons-uom NE po-ordl.cons-uom THEN
      RUN sys/ref/convquom.p (ap-invl.cons-uom, po-ordl.cons-uom,
                            item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                            ld-qty, OUTPUT ld-qty).

    IF po-ordl.cons-uom EQ "EA" THEN DO:
      {sys/inc/roundup.i ld-qty}
    END.

    po-ordl.t-inv-qty = po-ordl.t-inv-qty + ld-qty.
  END. 

  RUN po/calc-rmr.p (BUFFER po-ordl).

  IF po-ordl.opened THEN
    IF po-ordl.t-rec-qty GE
       po-ordl.cons-qty * (1 - (po-ordl.under-pct / 100)) THEN
      po-ordl.stat = "C".
    ELSE po-ordl.stat = "P".

  IF LAST-OF(po-ordl.i-no) THEN RUN rm/rm-reset.p (RECID(item)).
END.

HIDE ALL NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
