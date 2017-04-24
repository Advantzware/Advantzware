/* shipto.i */
DEF BUFFER bf-shipto FOR shipto.
DEF BUFFER bf-cust FOR cust.


{methods/run_link.i "RECORD-SOURCE" "Get-Values"
    "(OUTPUT op-company,OUTPUT op-cust-no)"}

ASSIGN
 shipto.company = op-company
 shipto.cust-no = op-cust-no.

FIND LAST bf-shipto
    WHERE bf-shipto.company EQ shipto.company
      AND bf-shipto.cust-no EQ shipto.cust-no
      AND ROWID(bf-shipto)  NE ROWID(shipto)
    USE-INDEX ship-no NO-LOCK NO-ERROR.

ASSIGN
 shipto.ship-no = (IF AVAIL bf-shipto THEN bf-shipto.ship-no ELSE 0) + 1
 shipto.ship-id = TRIM(STRING(shipto.ship-no,">>>>>>>>"))
 shipto.ship-meth = cust.ship-part.

RELEASE bf-shipto.

FIND FIRST bf-cust NO-LOCK
    WHERE bf-cust.company EQ shipto.company
      AND bf-cust.active  EQ "X"
    NO-ERROR.

IF AVAIL bf-cust THEN
FOR EACH bf-shipto
    WHERE bf-shipto.company EQ bf-cust.company
      AND bf-shipto.cust-no EQ bf-cust.cust-no
    BREAK BY bf-shipto.ship-no DESC:
  IF bf-shipto.ship-id EQ bf-shipto.cust-no OR
     LAST(bf-shipto.ship-no)                THEN DO:
    ASSIGN
     shipto.loc     = bf-shipto.loc
     shipto.loc-bin = bf-shipto.loc-bin.
    LEAVE.
  END.
END.

IF cust.sort EQ "Y" THEN shipto.tax-code = cust.tax-gr.


