/* -------------------------------------------------- rm/rm-reset.p 12/97 JLF */
/* item reset                                                                 */
/* -------------------------------------------------------------------------- */
/* 08/29/01 Copied and changed   YSK */

def input parameter rec-id as recid no-undo.

def new shared var cocode as cha no-undo.

def var v-hld-qty   as   dec                                            no-undo.
DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

find item where recid(item) eq rec-id.

assign
 cocode      = item.company
 item.q-onh  = 0
 item.q-ono  = 0.

/*** item.q-onh ***/
for each rm-bin
    where rm-bin.company eq item.company
      and rm-bin.i-no    eq item.i-no
    use-index i-no no-lock:
  item.q-onh = item.q-onh + rm-bin.qty.
end.

/*** item.q-ono from purchase orders ***/
FOR EACH po-ordl NO-LOCK
    {sys/look/item-posW.i}
    USE-INDEX item:

  RUN po/rm-q-ono.p (BUFFER po-ordl, OUTPUT v-hld-qty).
  
  item.q-ono = item.q-ono + v-hld-qty.
end.

FOR EACH itemfg NO-LOCK
    WHERE itemfg.company           EQ item.company
      AND itemfg.receiveAsRMItemID EQ item.i-no:
    RUN fg/calcqono.p (ROWID(itemfg), OUTPUT v-hld-qty).
    
    IF item.cons-uom NE "EA" THEN
        RUN Conv_QuantityFromUOMToUOM (
            INPUT  item.company,
            INPUT  item.i-no,
            INPUT  "RM",
            INPUT  v-hld-qty,
            INPUT  "EA", 
            INPUT  item.cons-uom,
            INPUT  0,
            INPUT  0,
            INPUT  0,
            INPUT  0,
            INPUT  0,
            OUTPUT v-hld-qty,
            OUTPUT lError,
            OUTPUT cMessage
            ).
      
    item.q-ono = item.q-ono + v-hld-qty.
END.

RUN rm/calcqcom.p (ROWID(item), OUTPUT item.q-comm).

item.q-avail = item.q-onh + item.q-ono - item.q-comm.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
