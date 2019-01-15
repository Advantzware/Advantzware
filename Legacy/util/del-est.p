/* delete al estimate */
/*
MESSAGE "Are you sure you want to delete invalid estimates" 
      VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN DO:
*/

SESSION:SET-WAIT-STATE("general").
PAUSE 0.
FOR EACH est WHERE est.est-no = "" :
    DISP est.est-no.
    PAUSE 0.
    DELETE est.
END.

FOR EACH est-qty WHERE est-qty.est-no = "":
      DELETE est-qty.
END.
FOR EACH ef WHERE ef.est-no = "" :
    DELETE ef.
END.

FOR EACH eb WHERE eb.est-no = "" :
    DELETE eb.
END.

FOR EACH est-op WHERE est-no = "" :
    DELETE est-op.
END.

FOR EACH est-prep WHERE est-no = "" :
    DELETE est-prep.
END.

FOR EACH est-inst WHERE est-no = "" :
    DELETE est-inst.
END.

FOR EACH est WHERE NOT CAN-FIND(FIRST est-qty WHERE est-qty.company EQ est.company
                                                AND est-qty.est-no  EQ est.est-no)
                OR NOT CAN-FIND(FIRST ef OF est)
                OR NOT CAN-FIND(FIRST eb OF est):
    DELETE est.
END.

FOR EACH est-qty WHERE NOT CAN-FIND(FIRST est WHERE est.company EQ est-qty.company
                                                AND est.est-no  EQ est-qty.est-no)
                    OR NOT CAN-FIND(FIRST ef WHERE ef.company EQ est-qty.company
                                               AND ef.est-no  EQ est-qty.est-no
                                               AND ef.eqty EQ est-qty.eqty)
                    OR NOT CAN-FIND(FIRST eb WHERE eb.company EQ est-qty.company
                                               AND eb.est-no  EQ est-qty.est-no
                                               AND eb.eqty EQ est-qty.eqty):
    DELETE est-qty.
END.

FOR EACH ef WHERE NOT CAN-FIND(FIRST est-qty WHERE est-qty.company EQ ef.company
                                               AND est-qty.est-no  EQ ef.est-no
                                               AND est-qty.eqty EQ ef.eqty)
               OR NOT CAN-FIND(FIRST est OF ef)
               OR NOT CAN-FIND(FIRST eb OF ef):
    DELETE ef.
END.

FOR EACH eb WHERE NOT CAN-FIND(FIRST est-qty WHERE est-qty.company EQ eb.company
                                               AND est-qty.est-no  EQ eb.est-no
                                               AND est-qty.eqty EQ eb.eqty)
               OR NOT CAN-FIND(FIRST est WHERE est.company EQ eb.company AND est.est-no EQ eb.est-no)
               OR NOT CAN-FIND(FIRST ef OF eb WHERE eb.form-no NE 0):
    DELETE eb.
END.

SESSION:SET-WAIT-STATE("").

