
DEF BUFFER b-eb FOR eb.


FOR EACH company NO-LOCK,
    EACH eb
    WHERE eb.company EQ company.company
      AND eb.ship-id EQ "",
    FIRST b-eb NO-LOCK
    WHERE b-eb.company EQ eb.company
      AND b-eb.est-no  EQ eb.est-no
      AND b-eb.cust-no NE "" 
      AND b-eb.ship-id NE ""
      AND ROWID(b-eb)  NE ROWID(eb):

  DISPLAY eb.company LABEL "Company"
          eb.est-no  LABEL "Est#"
                     FORMAT "x(10)".

  IF eb.cust-no EQ "" THEN eb.cust-no = b-eb.cust-no.
  
  IF eb.cust-no EQ b-eb.cust-no THEN eb.ship-id = b-eb.ship-id.
END.

MESSAGE "Procedure complete..." VIEW-AS ALERT-BOX.

HIDE ALL NO-PAUSE.

