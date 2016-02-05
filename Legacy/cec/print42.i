/* -------------------------------------------------- cec/print42.i 07/96 JLF */

def {1} var vsuthrlnd       as   log                            no-undo.
def {1} var vmclean2        as   log init no                    no-undo.
def {1} var v-match-up      as   dec format ">>9.99%"           no-undo.
def {1} var v-form-no       like ef.form-no init 0              no-undo.

def {1} var dm-tot-3        like dm-tot extent 99               no-undo.
def {1} var dm-tot-4        like dm-tot extent 99               no-undo.
def {1} var dm-tot-5        like dm-tot extent 99               no-undo.

def {1} var op-tot-3        like dm-tot extent 99               no-undo.
def {1} var op-tot-4        like dm-tot extent 99               no-undo.
def {1} var op-tot-5        like dm-tot extent 99               no-undo.

def {1} var v-fac-tot       like fac-tot  extent 99             no-undo.
def {1} var v-tt-tot        like tt-tot   extent 99             no-undo.
def {1} var v-ord-cost      like ord-cost extent 99             no-undo.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
