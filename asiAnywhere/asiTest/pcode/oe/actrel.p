/* ---------------------------------------------------- oe/actrel.p  7/94 rd  */
/* order entry - Create actual releases from planned release line            */
/* -------------------------------------------------------------------------- */

def input parameter ip-recid as recid.


{sys/inc/var.i shared}

def new shared var out-recid as recid no-undo.
def  shared var ARowid as recid no-undo.
def shared var relh-recid as recid no-undo.

def shared var v-auto as log no-undo.
def var choice as log no-undo.
def var ll-ans as log no-undo.
{oe/chkordl.i}

{oe/relemail.i}
   
    FIND oe-rel WHERE RECID(oe-rel) EQ ip-recid  NO-LOCK NO-ERROR.
IF AVAIL oe-rel THEN DO:
  choice = YES.

  FIND FIRST oe-ordl
      WHERE oe-ordl.company EQ oe-rel.company
        AND oe-ordl.ord-no  EQ oe-rel.ord-no
        AND oe-ordl.i-no    EQ oe-rel.i-no
      
      NO-LOCK.
  

MESSAGE "actrel" ROWID(oe-ordl) .
  RUN oe/chkordl.p (ROWID(oe-ordl)).
  
  FIND FIRST w-ordl WHERE w-rowid EQ ROWID(oe-ordl) AND w-ok NO-ERROR.
  IF  AVAIL w-ordl THEN DO:
     
    IF w-auto THEN v-auto = YES.

    {oe/actrel.i}
       /*RUN send-email-proc.*/
/*
      PAUSE 1 MESSAGE " RELEASED ".*/
      LEAVE rel-block.
    END. /* rel-block */
  END.
END.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
