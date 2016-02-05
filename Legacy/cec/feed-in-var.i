DEF VAR ll-unitize-2 AS LOG NO-UNDO.
DEF VAR v-on-f-2 AS DEC NO-UNDO.
DEF VAR v-on-s-2 AS DEC NO-UNDO.
DEF VAR li-die-2 AS INT NO-UNDO.
DEF VAR p-qty-2 AS DEC NO-UNDO.
DEF VAR c-qty-2 AS DEC NO-UNDO.
DEF VAR save-qty-2 AS DEC NO-UNDO.
DEF VAR b-wt-2 AS DEC NO-UNDO.
DEF VAR v-corr-2 AS LOG NO-UNDO.
DEF VAR v-parts-2 AS DEC NO-UNDO.
def var v-num-up-2 as int NO-UNDO.
DEF VAR v-n-out-2 AS INT NO-UNDO.
DEF BUFFER b-eb-2 FOR eb.

def NEW SHARED TEMP-TABLE cas-2 NO-UNDO
     field    typ        as int
     field    id         as char
     field    snum       as int
     field    bnum       as int
     field    ino        like item.i-no
     field    dscr       like item.est-dscr
     field    t-qty      as int
     field    qty        as int
     field    cosm       as dec format ">>>9.99"
     field    cost       as dec format ">>>,>>9.99".
