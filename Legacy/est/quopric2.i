
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-date AS CHAR NO-UNDO.


lv-date = STRING(YEAR(TODAY),"9999") +
          STRING(MONTH(TODAY),"99")  +
          STRING(DAY(TODAY),"99").

  FIND FIRST cust
      {sys/ref/custW.i}
        AND cust.cust-no eq quotehd.cust-no
      USE-INDEX cust NO-LOCK NO-ERROR.

  IF AVAIL cust THEN DO:
    FOR EACH oe-prmtx NO-LOCK
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype            EQ cust.type
          AND oe-prmtx.cust-no            EQ quotehd.cust-no
          AND oe-prmtx.procat             EQ itemfg.procat
          AND oe-prmtx.i-no               BEGINS itemfg.i-no
          AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no
          AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date
        BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx NO-LOCK
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype            EQ cust.type
          AND oe-prmtx.cust-no            EQ ""
          AND oe-prmtx.procat             EQ itemfg.procat
          AND oe-prmtx.i-no               BEGINS itemfg.i-no
          AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no
          AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date
        BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx NO-LOCK
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype            EQ cust.type
          AND oe-prmtx.cust-no            EQ ""
          AND oe-prmtx.procat             EQ itemfg.procat
          AND SUBSTR(oe-prmtx.i-no,1,100) EQ ""
          AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date
        BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx NO-LOCK
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype            EQ ""
          AND oe-prmtx.cust-no            EQ ""
          AND oe-prmtx.procat             EQ itemfg.procat
          AND oe-prmtx.i-no               BEGINS itemfg.i-no
          AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no
          AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date
        BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx NO-LOCK
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype            EQ ""
          AND oe-prmtx.cust-no            EQ ""
          AND oe-prmtx.procat             EQ ""
          AND oe-prmtx.i-no               BEGINS itemfg.i-no
          AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no
          AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date
        BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
      LEAVE.
    END.

    IF AVAIL oe-prmtx THEN
    DO li = (IF cust.cust-level = 0 THEN cust.cust-level + 1 ELSE 1) TO 10:
      IF {1}qty LE oe-prmtx.qty[li] THEN DO:
        IF oe-prmtx.meth THEN
          ASSIGN
           {1}price = oe-prmtx.price[li]
           {1}uom   = oe-prmtx.uom[li].
        ELSE
          ASSIGN
           {1}price = itemfg.sell-price -
                      ROUND((itemfg.sell-price * oe-prmtx.discount[li]) / 100,2)
           {1}uom   = itemfg.sell-uom.
        LEAVE.
      END.
    END.
  END.
