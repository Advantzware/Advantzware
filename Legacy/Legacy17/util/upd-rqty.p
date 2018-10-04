/* util/upd-rqty.p   reset t-rel-qty in oe-ordl */
DEF VAR lv-bol-qty LIKE oe-ordl.t-rel-qty NO-UNDO.
DEF VAR lv-rel-qty LIKE oe-ordl.t-rel-qty NO-UNDO.
FOR EACH oe-ordl WHERE t-rel-qty < 0 :
    lv-rel-qty = 0.
    FOR EACH oe-rell WHERE oe-rell.company = oe-ordl.company
                       AND oe-rell.ord-no = oe-ordl.ord-no
                       AND oe-rell.i-no = oe-ordl.i-no
                       AND oe-rell.LINE = oe-ordl.LINE
                       AND oe-rell.posted:
        lv-rel-qty = lv-rel-qty + oe-rell.qty.
    END.

    lv-bol-qty = 0.
    FOR EACH oe-boll WHERE oe-boll.company = oe-ordl.company
                       AND oe-boll.ord-no = oe-ordl.ord-no
                       AND oe-boll.i-no = oe-ordl.i-no:
        lv-bol-qty = lv-bol-qty + oe-boll.qty.
    END.

    oe-ordl.t-rel-qty = lv-bol-qty. 
    DISP oe-ordl.ord-no form ">>>9"
         oe-ordl.i-no oe-ordl.qty label "O-Qty" form ">>,>>9"
         oe-ordl.t-rel-qty label "Ord-RQty" form "->>,>>9"
         lv-rel-qty form "->>,>>9"  label "R-qty" 
         lv-bol-qty label "Bol-Qty" form "->>,>>9".
END.
