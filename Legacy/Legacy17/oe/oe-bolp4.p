/* -------------------------------------------------- oe/oe-bolp4.p 02/99 JLF */
/* Bill Of Lading Posting - Check for CUST Warehouse                          */
/* -------------------------------------------------------------------------- */

def input parameter v-recid1 as recid.
def input parameter v-recid2 as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xfg-bin for fg-bin.
DEF BUFFER b-oe-ordl FOR oe-ordl.

{oe/oe-bolpi.i}

def var v-bol-qty2 like v-bol-qty.
def var v-bol-qty3 like v-bol-qty.
def var v-partial2 like v-partial.
def var v-partial3 like v-partial.
def var v-rcpth-no as   int.
def var v-part-qty as   dec.

{fg/fullset.i NEW}


find oe-boll where recid(oe-boll) eq v-recid1.
find oe-ordl where recid(oe-ordl) eq v-recid2.

find first oe-bolh where oe-bolh.b-no eq oe-boll.b-no no-lock.
find first oe-ord of oe-ordl no-lock.

find first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    no-lock no-error.

assign
 v-bol-qty2 = v-bol-qty
 v-partial2 = v-partial.

{oe/oe-bolp4.i}

assign
 v-bol-qty3 = v-bol-qty
 v-partial3 = v-partial.

IF itemfg.isaset AND itemfg.alloc                                       AND
   NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) THEN DO:
  RUN fg/fullset.p (ROWID(itemfg)).

  FOR EACH tt-fg-set,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ tt-fg-set.part-no
      NO-LOCK:

    ASSIGN
     v-bol-qty = v-bol-qty2 * tt-fg-set.part-qty-dec
     v-partial = 0.

    {oe/oe-bolp4.i}
  END.
END.

assign
 v-bol-qty = v-bol-qty3
 v-partial = v-partial3.

/* end ---------------------------------- copr. 1999  advanced software, inc. */
