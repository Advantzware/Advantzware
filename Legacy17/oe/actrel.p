/* ---------------------------------------------------- oe/actrel.p  7/94 rd  */
/* order entry - Create actual releases from planned release line            */
/* -------------------------------------------------------------------------- */

def input parameter ip-recid as recid.
DEF INPUT-OUTPUT PARAMETER iocPrompt AS CHAR NO-UNDO.
{sys/inc/var.i shared}

def new shared var out-recid as recid no-undo.
def shared var relh-recid as recid no-undo.

def shared var v-auto as log no-undo.
def var choice as log no-undo.
DEF VAR v-merge-prompt AS LOG INIT ? NO-UNDO.
DEF VAR v-email AS LOG INIT YES NO-UNDO.
DEF VAR vr-relh AS ROWID NO-UNDO.


DEF BUFFER bf-rel FOR oe-rel.

{oe/chkordl.i}

{oe/relemail.i}

FIND oe-rel WHERE RECID(oe-rel) EQ ip-recid NO-LOCK NO-ERROR.

IF AVAIL oe-rel THEN DO:
  choice = YES.

  FIND FIRST oe-ordl
      WHERE oe-ordl.company EQ cocode
        AND oe-ordl.ord-no  EQ oe-rel.ord-no
        AND oe-ordl.i-no    EQ oe-rel.i-no
        AND oe-ordl.line    EQ oe-rel.line
      NO-LOCK NO-ERROR.
  IF NOT AVAIL oe-ordl THEN
      RETURN.

  RUN oe/chkordl.p (ROWID(oe-ordl)).

  FIND FIRST w-ordl WHERE w-rowid EQ ROWID(oe-ordl) AND w-ok NO-ERROR.

  IF AVAIL w-ordl THEN DO:
    IF w-auto THEN v-auto = YES.
    /* assign actual qty for release */
    /*FIND bf-rel OF oe-rel.
    IF bf-rel.qty = 0 THEN bf-rel.qty = bf-rel.tot-qty. 
    */
    /* wfk {oe/actrel.i} */
    RUN oe/actrelmerg.p (INPUT ROWID(oe-rel), INPUT "CREATE", INPUT-OUTPUT iocPrompt, OUTPUT vr-relh).

      PAUSE 1 MESSAGE " RELEASED ".
/*  wfk    LEAVE rel-block.
    END. */ /* rel-block */
  END.
END.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
