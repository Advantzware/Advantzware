/* soldto.i */
DEF BUFFER bf-soldto FOR soldto.


{methods/run_link.i "RECORD-SOURCE" "Get-Values"
    "(OUTPUT op-company,OUTPUT op-cust-no)"}

ASSIGN
 soldto.company = op-company
 soldto.cust-no = op-cust-no.

FIND LAST bf-soldto
    WHERE bf-soldto.company EQ soldto.company
      AND bf-soldto.cust-no EQ soldto.cust-no
      AND ROWID(bf-soldto)  NE ROWID(soldto)
    USE-INDEX sold-no NO-LOCK NO-ERROR.

ASSIGN
 soldto.sold-no = (IF AVAIL bf-soldto THEN bf-soldto.sold-no ELSE 0) + 1
 soldto.sold-id = TRIM(STRING(soldto.sold-no,">>>>>>>>")).
