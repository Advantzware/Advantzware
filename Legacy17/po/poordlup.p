/* ---------------------------------------------------------------------------*/
/* po detail line - p/o module - update item quantity on order                */
/* ---------------------------------------------------------------------------*/

def input parameter v-recid  as   RECID NO-UNDO.
def input parameter v-factor as   INT   NO-UNDO.
def input parameter v-reopen as   LOG   NO-UNDO.

{sys/inc/var.i SHARED}


FIND po-ordl WHERE RECID(po-ordl) EQ v-recid NO-ERROR.

IF AVAIL po-ordl THEN DO:
  {po/poordlup.i}
END.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
