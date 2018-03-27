/* -------------------------------------------------- oe/invpost5.i 02/99 JLF */
/* Bill Of Lading and Invoice Posting - CUST Warehouse Process                */
/* -------------------------------------------------------------------------- */

def input parameter v-recid1 as recid.
def input parameter v-recid2 as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{oe/oe-bolpi.i}

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-fg-bin  FOR fg-bin.
DEF BUFFER b-fg-rcpth FOR fg-rcpth.
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.

def var v-bol-qty2 like v-bol-qty.
def var v-bol-qty3 like v-bol-qty.
def var v-partial2 like v-partial.
def var v-partial3 like v-partial.
def var v-rcpth-no as   int.
def var v-part-qty as   dec.
DEF VAR v-tot-blank-bins AS DEC NO-UNDO.

DEF VAR lv-qty LIKE oe-boll.qty NO-UNDO.
DEF VAR lv-partial LIKE oe-boll.partial NO-UNDO.
DEF VAR li-stupid LIKE v-partial NO-UNDO.

/* For ship only history */
DEF VAR lcShipJobNo AS CHAR NO-UNDO.
DEF VAR liShipJobNo2 AS INT NO-UNDO.
DEF VAR lcShipLoc AS CHAR NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE FgKeepZeroBin-log AS LOGICAL NO-UNDO.
DEF SHARED TEMP-TABLE tt-bolh NO-UNDO LIKE oe-bolh.
DEF SHARED TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll.

RUN sys/ref/nk1look.p (INPUT cocode, "FGKEEPZEROBIN", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
     FgKeepZeroBin-log = LOGICAL(cRtnChar) NO-ERROR.
     
FIND tt-boll WHERE RECID(tt-boll) EQ v-recid1.
FIND {1}     WHERE RECID({1})     EQ v-recid2.

FIND FIRST tt-bolh WHERE tt-bolh.b-no EQ tt-boll.b-no.

FIND FIRST itemfg NO-LOCK
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ tt-boll.i-no
    NO-ERROR.

{oe/oe-bolp5.i "{1}" 1}

IF tt-boll.s-code = "S" THEN DO:
    FIND FIRST oe-rell WHERE oe-rell.r-no = tt-boll.r-no 
        AND oe-rell.s-code = "S" NO-LOCK NO-ERROR.
    
    /* If the release is ship-only and we're posting the BOL,
       clean up the fg-bin records that are placeholders, total the + and - */
    IF AVAIL oe-rell THEN DO:

        v-tot-blank-bins = 0.
        FOR EACH fg-bin WHERE fg-bin.company EQ tt-boll.company
            AND fg-bin.i-no EQ tt-boll.i-no
            /* AND fg-bin.loc  EQ tt-boll.loc */
            AND fg-bin.loc-bin EQ ""
            AND fg-bin.tag EQ "" NO-LOCK.
            v-tot-blank-bins = v-tot-blank-bins + fg-bin.qty.
            
        END.

        IF v-tot-blank-bins EQ 0 THEN DO:
          
            FOR EACH fg-bin WHERE fg-bin.company EQ tt-boll.company
                AND fg-bin.i-no EQ tt-boll.i-no
                /* AND fg-bin.loc  EQ tt-boll.loc */
                AND fg-bin.loc-bin EQ ""
                AND fg-bin.tag EQ "" EXCLUSIVE-LOCK.
                DELETE fg-bin.
            END. /* each fg-bin */
    
        END. /* if total blank bins = 0 */
    END. /* avail oe-rell */

END. /* if s-code is 'S' */

/* end ---------------------------------- copr. 1999  advanced software, inc. */
