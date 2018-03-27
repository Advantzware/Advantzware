DEF BUFFER b-rm-rh FOR rm-rcpth.
DEF BUFFER b-rm-rd FOR rm-rdtlh.

DEF VAR li AS INT NO-UNDO.
DEF VAR ld-iss AS DEC NO-UNDO.
DEF VAR ld-rec AS DEC NO-UNDO.
DEF VAR ll-neg AS LOG NO-UNDO.


SESSION:SET-WAIT-STATE ("general").

FOR EACH po-ordl
    WHERE po-ordl.item-type EQ YES
      AND CAN-FIND(FIRST ITEM WHERE ITEM.company EQ po-ordl.company
                                AND ITEM.i-no    EQ po-ordl.i-no
                                AND ITEM.i-code  EQ "E"):

  ASSIGN
   ld-iss = 0
   ld-rec = 0.

  FOR EACH rm-rcpth
      WHERE rm-rcpth.company   EQ po-ordl.company
        AND rm-rcpth.i-no      EQ po-ordl.i-no
        AND rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
        AND rm-rcpth.job-no    EQ po-ordl.job-no
        AND rm-rcpth.job-no2   EQ po-ordl.job-no2
        AND rm-rcpth.rita-code EQ "I"
      NO-LOCK,

      EACH rm-rdtlh
      WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
        AND rm-rdtlh.s-num     EQ po-ordl.s-num:

    ld-iss  = ld-iss + rm-rdtlh.qty.
  END.

  po-ordl.t-rec-qty = ld-iss.

  FOR EACH rm-rcpth
      WHERE rm-rcpth.company   EQ po-ordl.company
        AND rm-rcpth.i-no      EQ po-ordl.i-no
        AND rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
        AND rm-rcpth.job-no    EQ po-ordl.job-no
        AND rm-rcpth.job-no2   EQ po-ordl.job-no2
        AND rm-rcpth.rita-code EQ "R"
      NO-LOCK,
      EACH rm-rdtlh
        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND rm-rdtlh.s-num     EQ po-ordl.s-num:

    ld-rec = ld-rec + rm-rdtlh.qty.
  END.

  IF ld-rec NE ld-iss THEN DO:
    ASSIGN
     ld-rec = ld-iss - ld-rec
     ll-neg = ld-rec LT 0.

    IF ld-rec NE 0 THEN make-receipts:
    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ po-ordl.company
          AND rm-rcpth.i-no      EQ po-ordl.i-no
          AND rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
          AND rm-rcpth.job-no    EQ po-ordl.job-no
          AND rm-rcpth.job-no2   EQ po-ordl.job-no2
          AND rm-rcpth.rita-code EQ "I"
          AND CAN-FIND(FIRST rm-rdtlh
                       WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                         AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                         AND rm-rdtlh.s-num     EQ po-ordl.s-num)
        NO-LOCK
        BY rm-rcpth.trans-date
        BY rm-rcpth.r-no.

      RUN sys/ref/asiseq.p (INPUT po-ordl.company, INPUT "rm_rcpt_seq", OUTPUT li) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
        MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
           VIEW-AS ALERT-BOX INFO BUTTONS OK.


      CREATE b-rm-rh.
      BUFFER-COPY rm-rcpth TO b-rm-rh
      ASSIGN
       b-rm-rh.r-no       = li
       b-rm-rh.rita-code  = "R".

      FOR EACH rm-rdtlh
          WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
            AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
            AND rm-rdtlh.s-num     EQ po-ordl.s-num
          NO-LOCK:

        CREATE b-rm-rd.
        BUFFER-COPY rm-rdtlh TO b-rm-rd
        ASSIGN
         b-rm-rd.r-no      = b-rm-rh.r-no
         b-rm-rd.rita-code = b-rm-rh.rita-code.

        ld-rec = ld-rec - b-rm-rd.qty.

        IF (ld-rec GE 0 AND ll-neg)     OR
           (ld-rec LE 0 AND NOT ll-neg) THEN DO:
          b-rm-rd.qty = b-rm-rd.qty + ld-rec.
          LEAVE make-receipts.
        END.
      END.
    END.
  END.
END.

SESSION:SET-WAIT-STATE ("").

