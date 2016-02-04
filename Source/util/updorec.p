/* util/updorec.p  Update oe-ord,oe-ordl's rec_key from est */

SESSION:SET-WAIT-STATE("general").

FOR EACH oe-ord WHERE oe-ord.est-no <> "".
    FIND FIRST est WHERE est.company = oe-ord.company AND
                         est.est-no = oe-ord.est-no NO-LOCK NO-ERROR.
    IF AVAIL est THEN oe-ord.rec_key = est.rec_key.
END.

FOR EACH oe-ordl WHERE oe-ordl.est-no <> "" BREAK BY oe-ordl.ord-no.
    FIND FIRST est WHERE est.company = oe-ordl.company AND
                         est.est-no = oe-ordl.est-no NO-LOCK NO-ERROR.
    IF AVAIL est THEN oe-ordl.rec_key = est.rec_key.

    IF FIRST-OF(oe-ordl.ord-no) THEN DO:
       FIND oe-ord OF oe-ordl.
       IF oe-ord.est-no = "" THEN DO:
          oe-ord.rec_key = oe-ordl.rec_key.
       END.
    END.
END.
MESSAGE "Procedure completed. " VIEW-AS ALERT-BOX.
SESSION:SET-WAIT-STATE("").
