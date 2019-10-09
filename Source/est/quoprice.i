
DEF VAR lv-qty   LIKE {1}.qty   NO-UNDO.
DEF VAR lv-price LIKE {1}.price NO-UNDO.
DEF VAR lv-uom   LIKE {1}.uom   NO-UNDO.
DEFINE VARIABLE lMatrixExists AS LOGICAL     NO-UNDO.

  FIND FIRST itemfg
      WHERE itemfg.company  EQ cocode
        AND itemfg.part-no  EQ quoteitm.part-no{2}
        AND itemfg.part-no  NE ""
        AND (itemfg.cust-no EQ quotehd.cust-no OR
             itemfg.i-code  EQ "S")
      NO-LOCK NO-ERROR.

  IF NOT AVAIL itemfg THEN
      FIND FIRST itemfg
      WHERE itemfg.company  EQ cocode
        AND itemfg.i-no  EQ quoteitm.i-no{2}
        AND itemfg.part-no  NE ""
        AND (itemfg.cust-no EQ quotehd.cust-no OR
             itemfg.i-code  EQ "S")
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN DO:      

    lv-qty = DEC({1}.qty:SCREEN-VALUE IN BROWSE {&browse-name}).
    FIND FIRST cust
    {sys/ref/custW.i}
      AND cust.cust-no eq quotehd.cust-no
    USE-INDEX cust NO-LOCK NO-ERROR.
    IF AVAILABLE cust THEN 
        RUN GetPriceMatrixPriceSimple IN hdPriceProcs (itemfg.company, itemfg.i-no, cust.cust-no, "", DECIMAL(lv-qty),
                                                      OUTPUT lMatrixExists, INPUT-OUTPUT lv-price, INPUT-OUTPUT lv-uom).
 
    ASSIGN
     {1}.price:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-price)
     {1}.uom:SCREEN-VALUE IN BROWSE {&browse-name}   = lv-uom.
  END.
  
