
FOR EACH company NO-LOCK,
    EACH eb 
    WHERE eb.company EQ company.company
      AND NOT CAN-FIND(FIRST shipto
                       WHERE shipto.company EQ eb.company
                         AND shipto.cust-no EQ eb.cust-no
                         AND shipto.ship-id EQ eb.ship-id),
    FIRST cust NO-LOCK
    WHERE cust.company EQ eb.company
      AND cust.cust-no EQ eb.cust-no:

  DISPLAY eb.company LABEL "Company"
          eb.est-no  LABEL "Est#"
                     FORMAT "x(10)".

  FOR EACH shipto
      WHERE shipto.company EQ cust.company
        AND shipto.cust-no EQ cust.cust-no
      NO-LOCK
      BREAK BY shipto.ship-no DESC:
    IF shipto.ship-id EQ cust.cust-no OR LAST(shipto.ship-no) THEN do:
      eb.ship-id = shipto.ship-id.
      LEAVE.
    END.
  END.
END.

MESSAGE "Procedure complete..." VIEW-AS ALERT-BOX.
