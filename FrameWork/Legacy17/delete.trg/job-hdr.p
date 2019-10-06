&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME job-hdr

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEF VAR v-fin-qty LIKE fg-act.qty NO-UNDO.

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}


ASSIGN
 cocode = g_company
 locode = g_loc.

/* not delete if estimate exists */
FIND est WHERE est.rec_key EQ {&TABLENAME}.rec_key NO-LOCK NO-ERROR.
IF NOT AVAIL est THEN DO:
  {methods/triggers/delete.i}
END.

run fg/chkfgloc.p (input {&TABLENAME}.i-no, input {&TABLENAME}.loc).
FOR EACH itemfg-loc
    WHERE itemfg-loc.company EQ {&TABLENAME}.company
      AND itemfg-loc.i-no    EQ {&TABLENAME}.i-no
      AND itemfg-loc.loc     EQ {&TABLENAME}.loc:

   FIND itemfg WHERE itemfg.company EQ itemfg-loc.company
                 AND itemfg.i-no EQ itemfg-loc.i-no 
              NO-LOCK NO-ERROR.
        
  v-fin-qty = 0.
  FOR EACH fg-act
      WHERE fg-act.company EQ {&TABLENAME}.company
        AND fg-act.job     EQ {&TABLENAME}.job
        AND fg-act.job-no  EQ {&TABLENAME}.job-no
        AND fg-act.job-no2 EQ {&TABLENAME}.job-no2
        AND fg-act.loc     EQ itemfg-loc.loc
        AND fg-act.i-no    EQ {&TABLENAME}.i-no
      NO-LOCK:
    v-fin-qty = v-fin-qty + fg-act.qty.
  END.

  IF v-fin-qty LT {&TABLENAME}.qty THEN DO:
    v-fin-qty = {&TABLENAME}.qty - v-fin-qty.

    IF NOT itemfg.pur-man THEN
      itemfg-loc.q-ono = itemfg-loc.q-ono - v-fin-qty.
              
  END.


  LEAVE.
END.

FOR EACH itemfg
    WHERE itemfg.company EQ {&TABLENAME}.company
      AND itemfg.i-no    EQ {&TABLENAME}.i-no:

  v-fin-qty = 0.
  FOR EACH fg-act
      WHERE fg-act.company EQ {&TABLENAME}.company
        AND fg-act.job     EQ {&TABLENAME}.job
        AND fg-act.job-no  EQ {&TABLENAME}.job-no
        AND fg-act.job-no2 EQ {&TABLENAME}.job-no2
        AND fg-act.i-no    EQ {&TABLENAME}.i-no
      NO-LOCK:
    v-fin-qty = v-fin-qty + fg-act.qty.
  END.

  IF v-fin-qty LT {&TABLENAME}.qty THEN DO:
    v-fin-qty = {&TABLENAME}.qty - v-fin-qty.

    IF NOT itemfg.pur-man THEN
      itemfg.q-ono = itemfg.q-ono - v-fin-qty.
              
    RUN fg/comp-upd.p (RECID(itemfg), v-fin-qty * -1,
                       "q-ono", {&TABLENAME}.est-no).
  END.

  ELSE RUN fg/prodcode.p (ROWID(itemfg)).

  LEAVE.
END.

FOR EACH mfvalues WHERE
    mfvalues.rec_key = {&TABLENAME}.company + "|jh" + STRING({&TABLENAME}.j-no):

    DELETE mfvalues.
END.
