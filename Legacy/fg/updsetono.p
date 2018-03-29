/* -------------------------------------------------- fg/updsetdm.p 12/00 JLF */
/* Update Set Dimensions                                                      */
/* 09/10/01 YSK Copied and changed to use est-no instead e-num                */
/* -------------------------------------------------------------------------- */

def input parameter v-rowid as ROWID.

{sys/inc/var.i shared}

def buffer b-itemfg for itemfg.
def buffer b-eb     for eb.

def var v-part-qty      as   dec.
def var v-yld-qty       as   dec.

{ce/msfcalc.i}

{fg/fullset.i NEW}


find itemfg where rowid(itemfg) eq v-rowid no-error.

if avail itemfg and itemfg.isaset then do:
  DO:
    RUN fg/fullset.p (ROWID(itemfg)).

    FOR EACH tt-fg-set,
        FIRST b-itemfg
        WHERE b-itemfg.company EQ cocode
          AND b-itemfg.i-no    EQ tt-fg-set.part-no
        EXCLUSIVE-LOCK
    
        break by tt-fg-set.set-no:
       FIND FIRST b-eb WHERE b-eb.company EQ b-itemfg.company
                         AND b-eb.est-no  EQ b-itemfg.est-no
                         AND b-eb.stock-no EQ b-itemfg.i-no
                       NO-LOCK NO-ERROR.
       b-itemfg.q-ono = itemfg.q-ono * (IF avail(b-eb) AND b-eb.spare-char-2 EQ "Y" THEN 1 ELSE tt-fg-set.qtyPerSet). 
       IF AVAIL(b-eb) THEN
         RUN fg/chkfgloc.p (INPUT b-itemfg.i-no, INPUT b-eb.loc).
       IF AVAIL(b-eb) THEN
         FIND FIRST itemfg-loc 
             WHERE itemfg-loc.company EQ b-itemfg.company
               AND itemfg-loc.i-no    EQ b-itemfg.i-no
               AND itemfg-loc.loc     EQ b-eb.loc
             EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL(itemfg-loc) THEN
         itemfg-loc.q-ono = itemfg-loc.q-ono * (IF avail(b-eb) AND b-eb.spare-char-2 EQ "Y" THEN 1 ELSE tt-fg-set.qtyPerSet).
       FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    end.
  END.
end.
/* end ---------------------------------- copr. 2000  advanced software, inc. */
