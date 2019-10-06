/* -------------------------------------------------- oe/invpost6.i 02/99 JLF */
/* Bill Of Lading and Invoice Posting - relieve bins                          */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER v-recid1 AS RECID.
DEF INPUT PARAMETER v-recid2 AS RECID.
DEF INPUT PARAMETER v-recid3 AS RECID.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{oe/oe-bolpi.i}

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-fg-rcpth FOR fg-rcpth.
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.

DEF VAR v-bol-qty2 LIKE v-bol-qty.
DEF VAR v-bol-qty3 LIKE v-bol-qty.
DEF VAR v-partial2 LIKE v-partial.
DEF VAR v-partial3 LIKE v-partial.
DEF VAR v-rcpth-no AS   INT.
DEF VAR v-fg-qty   LIKE oe-boll.qty.
DEF VAR v-part-qty AS   DEC.
DEF VAR v-loc      LIKE oe-boll.loc.
DEF VAR v-loc-bin  LIKE oe-boll.loc-bin.
DEF VAR v-tag      LIKE oe-boll.tag.
DEF VAR li-stupid  LIKE v-partial NO-UNDO.
DEFINE VARIABLE cRtnChar          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE FgKeepZeroBin-log AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rSaveFgBinRow AS ROWID NO-UNDO.

/* For ship only history */
DEF VAR lcShipJobNo AS CHAR NO-UNDO.
DEF VAR liShipJobNo2 AS INT NO-UNDO.
DEF VAR lcShipLoc AS CHAR NO-UNDO.

DEF SHARED TEMP-TABLE tt-bolh NO-UNDO LIKE oe-bolh.
DEF SHARED TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll.


{sys/inc/autopost.i}

{sys/inc/fgsetrec.i}

FIND tt-boll WHERE RECID(tt-boll) EQ v-recid1.
FIND {1}     WHERE RECID({1})     EQ v-recid2.
FIND fg-bin  WHERE RECID(fg-bin)  EQ v-recid3 NO-ERROR.


RUN sys/ref/nk1look.p (INPUT cocode, "FGKEEPZEROBIN", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    FgKeepZeroBin-log = LOGICAL(cRtnChar) NO-ERROR.

FIND FIRST tt-bolh WHERE tt-bolh.b-no EQ tt-boll.b-no NO-LOCK.

FIND FIRST itemfg
    WHERE itemfg.company EQ tt-boll.company
      AND itemfg.i-no    EQ tt-boll.i-no
    NO-LOCK NO-ERROR.

IF NOT AVAIL oe-ordl AND "{1}" EQ "inv-line" AND tt-boll.ord-no NE 0 THEN
FIND FIRST oe-ordl
    WHERE oe-ordl.company EQ tt-boll.company
      AND oe-ordl.ord-no  EQ tt-boll.ord-no
      AND oe-ordl.i-no    EQ tt-boll.i-no
      AND oe-ordl.line    EQ tt-boll.line
    NO-LOCK NO-ERROR.

{oe/oe-bolp6.i "{1}" "Header"}

/* end ---------------------------------- copr. 1999  advanced software, inc. */
