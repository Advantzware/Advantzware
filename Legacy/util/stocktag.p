
FOR EACH itemfg
    WHERE CAN-FIND(FIRST cust
                   WHERE cust.company EQ itemfg.company
                     AND cust.cust-no EQ itemfg.cust-no
                     AND cust.active  EQ "X")
    NO-LOCK
    TRANSACTION:

  DISPLAY itemfg.company LABEL "Company"
          itemfg.i-no    LABEL "FG#"    FORMAT "x(20)" WITH DOWN.

  FOR EACH loadtag
      WHERE loadtag.company     EQ itemfg.company
        AND loadtag.item-type   EQ NO
        AND loadtag.i-no        EQ itemfg.i-no
        AND loadtag.is-case-tag EQ NO
      USE-INDEX i-no:
    loadtag.ord-no = 0.
  END.
END.

MESSAGE "Process completed..." VIEW-AS ALERT-BOX.

RETURN.

