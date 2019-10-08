
DEF PARAM BUFFER io-ordl FOR oe-ordl.
DEF PARAM BUFFER io-qqty FOR quoteqty.
DEF PARAM BUFFER io-invl FOR ar-invl.
DEF INPUT PARAM ip-delete  AS LOG NO-UNDO.
DEF INPUT PARAM ip-rec_key LIKE notes.rec_key NO-UNDO.

{custom/globdefs.i}

DEF BUFFER makenote FOR reftable.

DEF VAR li AS INT EXTENT 3 NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR lv-precede AS CHAR NO-UNDO.
DEF VAR li-recid AS RECID NO-UNDO.

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/notes.i}

IF v-prt-inst AND (AVAIL io-ordl OR AVAIL io-qqty OR AVAIL io-invl) THEN DO:
  CREATE notes.
  ASSIGN
   notes.rec_key    = ip-rec_key
   notes.note_date  = TODAY
   notes.note_time  = TIME
   notes.user_id    = USERID("NOSWEAT")
   notes.note_type  = "S"
   notes.note_code  = "CS"

   li-recid   = IF AVAIL io-ordl THEN RECID(io-ordl) ELSE
                IF AVAIL io-qqty THEN RECID(io-qqty) ELSE
                IF AVAIL io-invl THEN RECID(io-invl) ELSE ?
   lv-precede = TRIM(ENTRY(INT(ip-delete) + 1,"CHANGE,CANCEL")) + ":".

  FIND FIRST makenote
      WHERE makenote.reftable EQ "makenote.identifier"
        AND makenote.company  EQ cocode
        AND makenote.loc      EQ STRING(li-recid,"9999999999")
      NO-ERROR.
  IF NOT AVAIL makenote THEN DO:
    IF NOT ip-delete THEN lv-precede = "".
    CREATE makenote.
    ASSIGN
     makenote.reftable = "makenote.identifier"
     makenote.company  = cocode
     makenote.loc      = STRING(li-recid,"9999999999").
  END.
  IF ip-delete THEN DELETE makenote.

  IF AVAIL io-ordl THEN DO:
    notes.note_title = "Ord#: " +
                         TRIM(STRING(io-ordl.ord-no,">>>>>>>>>>")) + " " +
                       "Due: " +
                         TRIM(STRING(io-ordl.req-date,"99/99/99")) + " " +
                       "Qty: " +
                         TRIM(STRING(io-ordl.qty,"->,>>>,>>>,>>>")) + " " +
                       "Price: " +
                         TRIM(STRING(io-ordl.price,">,>>>,>>9.99<<<<<<")) +
                       "/" + TRIM(io-ordl.pr-uom).

    IF AVAIL makenote THEN DO:
      IF io-ordl.est-no NE "" THEN DO:
        FIND FIRST est NO-LOCK
            WHERE est.company EQ io-ordl.company
              AND est.est-no  EQ io-ordl.est-no
            NO-ERROR.
        IF AVAIL est THEN DO:
          li[1] = est.est-type - (IF est.est-type GT 4 THEN 4 ELSE 0).

          IF li[1] EQ 4 THEN DO:
            RUN ce/com/istandem.p (ROWID(est), OUTPUT ll).
            IF ll THEN li[1] = 3.
          END.

          notes.note_title = TRIM(notes.note_title) + " - " +
                            TRIM(ENTRY(li[1],"Single,Set,Tandem,Combo")).

          FOR EACH oe-ordl NO-LOCK
              WHERE oe-ordl.company EQ io-ordl.company
                AND oe-ordl.ord-no  EQ io-ordl.ord-no
                AND oe-ordl.LINE > 0:
            ASSIGN
             li[2] = li[2] + 1
             li[3] = li[3] + oe-ordl.qty.
          END.

          notes.note_title = TRIM(notes.note_title) + " - " +
                            TRIM(STRING(li[2],"->>>,>>>")) + " Items/Ttl " +
                            TRIM(STRING(li[3],"->>>,>>>,>>>")).        
        END.
      END.
    END.
  END.

  ELSE
  IF AVAIL io-qqty THEN DO:
    notes.note_title = "Quote#: " +
                        TRIM(STRING(io-qqty.q-no,">>>>>>>>>>")) + " " +
                       "Qty: " +
                        TRIM(STRING(io-qqty.qty,">,>>>,>>>,>>>")) + " " +
                       "Price: " +
                        TRIM(STRING(io-qqty.price,">,>>>,>>9.99<<<<<<")) +
                       "/" + TRIM(io-qqty.uom).
        
    IF AVAIL makenote THEN DO:
      RELEASE est.
      FIND FIRST quotehd NO-LOCK
          WHERE quotehd.company EQ io-qqty.company
            AND quotehd.loc     EQ io-qqty.loc
            AND quotehd.q-no    EQ io-qqty.q-no
            AND quotehd.est-no  NE ""
          NO-ERROR.
      IF AVAIL quotehd THEN
      FIND FIRST est NO-LOCK
          WHERE est.company EQ quotehd.company
            AND est.est-no  EQ quotehd.est-no
          NO-ERROR.
      IF AVAIL est THEN DO:
        li[1] = est.est-type - (IF est.est-type GT 4 THEN 4 ELSE 0).

        IF li[1] EQ 4 THEN DO:
          RUN ce/com/istandem.p (ROWID(est), OUTPUT ll).
          IF ll THEN li[1] = 3.
        END.

        notes.note_title = TRIM(notes.note_title) + " - " +
                          TRIM(ENTRY(li[1],"Single,Set,Tandem,Combo")).

        FOR EACH quoteqty NO-LOCK
            WHERE quoteqty.company EQ io-qqty.company
              AND quoteqty.loc     EQ io-qqty.loc
              AND quoteqty.q-no    EQ io-qqty.q-no
              AND (quoteqty.qty    EQ io-qqty.qty OR li[1] GT 2):
          ASSIGN
           li[2] = li[2] + 1
           li[3] = li[3] + quoteqty.qty.
        END.

        notes.note_title = TRIM(notes.note_title) + " - " +
                          TRIM(STRING(li[2],"->>>,>>>")) + " Items/Ttl " +
                          TRIM(STRING(li[3],"->>>,>>>,>>>")).        
      END.
    END.
  END.

  ELSE
  IF AVAIL io-invl THEN DO:
    notes.note_title = "Inv#: " +
                        TRIM(STRING(io-invl.inv-no,">>>>>>>>>>")) + " " +
                       "Qty: " +
                        TRIM(STRING(io-invl.inv-qty,"->,>>>,>>>,>>>")) + " " +
                       "Price: " +
                        TRIM(STRING(io-invl.unit-pr,">,>>>,>>9.99<<<<<<")) +
                       "/" + TRIM(io-invl.pr-uom).
  END.

  ASSIGN
   notes.note_title = TRIM(lv-precede + " " + TRIM(notes.note_title))
   notes.note_text  = notes.note_title.
END.
