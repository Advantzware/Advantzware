
def {1} shared var v-s-cust as char format "x(8)"       init " " no-undo.
def {1} shared var v-e-cust as char format "x(8)"       init "zzzzzzzz" no-undo.
def {1} shared var v-s-bol  as int  format ">>>>>>>>"   init 0 no-undo.
def {1} shared var v-e-bol  as int  format ">>>>>>>>"   init 99999999 no-undo.
def {1} shared var v-s-ord  as int  format ">>>>>>>"    init 0 no-undo.
def {1} shared var v-e-ord  like v-s-ord                init 9999999 no-undo.
def {1} shared var v-s-date like oe-bolh.bol-date format "99/99/9999" init today.
def {1} shared var v-e-date like v-s-date.
def {1} shared var v-printed as log format "Y/N"        init no no-undo.
def {1} shared var v-print-pal as log format "Y/N"      init no no-undo.
def {1} shared var v-print-bol as log format "BOL/COC"  init yes no-undo.
def {1} shared var v-print-hdgs as log no-undo.
def {1} shared var v-print-mode as char no-undo.    /* align or prod */
def {1} shared var v-term-id as char no-undo.
def {1} shared var save_id as recid no-undo. /* passes recid of printer */
def {1} shared var v-coc-fmt as char no-undo.
DEF {1} SHARED VAR v-lines-per-page AS INT NO-UNDO.
DEF {1} SHARED VAR v-print-components AS LOG NO-UNDO.
DEF {1} SHARED VAR v-print-shipnotes AS LOG NO-UNDO.
DEF {1} SHARED VAR v-print-dept AS LOG NO-UNDO.
DEF {1} SHARED VAR v-depts AS CHAR NO-UNDO.
DEF {1} SHARED VAR v-sort AS log NO-UNDO.

def var v-last-page         as   int.
def var v-page-tot          as   dec format ">>9".

DEF TEMP-TABLE tt-boll LIKE oe-boll
    FIELD rec_id AS CHAR
    FIELD unitCount as INTEGER
    FIELD qty-sum   as INTEGER .

DEF BUFFER b-tt-boll FOR tt-boll.
