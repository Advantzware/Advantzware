/* -------------------------------------------------- rm/rm-mkbin.p 11/97 JLF */
/* raw materials bin rebuild program                                          */
/* -------------------------------------------------------------------------- */

def input parameter rec-id as recid no-undo.

def var v-r-qty     as   dec                    no-undo.
def var v-i-qty     as   dec                    no-undo.
def var v-t-qty     as   dec                    no-undo.
DEF VAR ld-qty      AS   DEC                    NO-UNDO.
DEF VAR ld-cst      AS   DEC                    NO-UNDO.
DEF VAR lv-uom      AS   CHAR                   NO-UNDO.


FIND item WHERE RECID(item) EQ rec-id EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL item THEN DO:
  {rm/rmmkbin1.i TODAY}
END.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
