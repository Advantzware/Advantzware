/* -------------------------------------------------- oe/invpost6.i 02/99 JLF */
/* Bill Of Lading and Invoice Posting - relieve bins                          */
/* -------------------------------------------------------------------------- */

def input parameter v-recid1 as recid.
def input parameter v-recid2 as recid.
def input parameter v-recid3 as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{oe/oe-bolpi.i}

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-fg-rcpth FOR fg-rcpth.
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.

def var v-bol-qty2 like v-bol-qty.
def var v-bol-qty3 like v-bol-qty.
def var v-partial2 like v-partial.
def var v-partial3 like v-partial.
def var v-rcpth-no as   int.
def var v-fg-qty   like oe-boll.qty.
def var v-part-qty as   dec.
def var v-loc      like oe-boll.loc.
def var v-loc-bin  like oe-boll.loc-bin.
def var v-tag      like oe-boll.tag.
DEF VAR li-stupid  LIKE v-partial NO-UNDO.

/* For ship only history */
DEF VAR lcShipJobNo AS CHAR NO-UNDO.
DEF VAR liShipJobNo2 AS INT NO-UNDO.
DEF VAR lcShipLoc AS CHAR NO-UNDO.

DEF SHARED TEMP-TABLE tt-bolh NO-UNDO LIKE oe-bolh.
DEF SHARED TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll.


{sys/inc/autopost.i}

{sys/inc/fgsetrec.i}

find tt-boll where recid(tt-boll) eq v-recid1.
find {1}     where recid({1})     eq v-recid2.
find fg-bin  where recid(fg-bin)  eq v-recid3 no-error.

find first tt-bolh where tt-bolh.b-no eq tt-boll.b-no no-lock.

find first itemfg
    where itemfg.company eq tt-boll.company
      and itemfg.i-no    eq tt-boll.i-no
    no-lock no-error.

IF NOT AVAIL oe-ordl AND "{1}" EQ "inv-line" AND tt-boll.ord-no NE 0 THEN
FIND FIRST oe-ordl
    WHERE oe-ordl.company EQ tt-boll.company
      AND oe-ordl.ord-no  EQ tt-boll.ord-no
      AND oe-ordl.i-no    EQ tt-boll.i-no
      AND oe-ordl.line    EQ tt-boll.line
    NO-LOCK NO-ERROR.

{oe/oe-bolp6.i "{1}" "Header"}

/* end ---------------------------------- copr. 1999  advanced software, inc. */
