
DEF {1} SHARED VAR v-trailer LIKE OE-BOLH.TRAILER NO-UNDO.
DEF {1} SHARED VAR save_id AS RECID NO-UNDO. /* passes recid of printer */
DEF {1} SHARED VAR pass_it AS RECID. /* passes recid of soldto */
def {1} SHARED var v-emp-id like soldto.sold-id no-undo.
def {1} SHARED var v-emp-recid as recid no-undo.
def {1} SHARED var v-mast-bol-no like oe-bolh.bol-no format ">>>>>9" no-undo.
