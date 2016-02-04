/* t-blqty.p   assign eb.bl-qty from est.est-qty[1] for folding */
disable triggers for load of eb.

FOR EACH eb WHERE eb.est-type < 5 :
    IF bl-qty = 0 THEN DO:
        FIND est WHERE est.company = eb.company AND
                       est.est-no = eb.est-no NO-LOCK.
        eb.bl-qty = est.est-qty[1].
    END.
END.
