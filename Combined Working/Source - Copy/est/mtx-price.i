/* -------------------------------------------------- est/mtx-price.i 03/11 SEWA */
/*                                                                            */
/* quote item entry - ITEM PRICING FROM PRICE MATRIX                               */
/*                   FOR STOCK BOXES ONLY                                     */
/* -------------------------------------------------------------------------- */

DEF VAR lv-date AS CHAR NO-UNDO.
define var v-i-item like itemfg.i-no no-undo. 
define var price-ent as log NO-UNDO.
DEFINE VAR matrixExists AS LOG NO-UNDO.
DEF VAR v-i-price LIKE itemfg.sell-price.
DEF VAR v-i-uom LIKE itemfg.sell-uom.

FIND FIRST cust WHERE cust.company = itemfg.company
                AND cust.cust-no = itemfg.cust-no NO-LOCK NO-ERROR.

ASSIGN
 lv-date = STRING(YEAR(TODAY),"9999") +
           STRING(MONTH(TODAY),"99")  +
           STRING(DAY(TODAY),"99") 
 v-i-item = itemfg.i-no   
    .
                         
  RELEASE oe-prmtx.

  FOR EACH oe-prmtx
      {oe/oe-prmtxW.i}
        AND oe-prmtx.custype             EQ cust.type
        AND oe-prmtx.cust-no             EQ cust.cust-no
        AND oe-prmtx.procat              EQ itemfg.procat
        AND oe-prmtx.i-no                BEGINS v-i-item
        AND SUBSTR(oe-prmtx.i-no,01,100) EQ v-i-item
        AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
      BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
    LEAVE.
  END.

  
    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype             EQ cust.type
          AND oe-prmtx.cust-no             EQ ""
          AND oe-prmtx.procat              EQ itemfg.procat
          AND oe-prmtx.i-no                BEGINS v-i-item
          AND SUBSTR(oe-prmtx.i-no,01,100) EQ v-i-item
          AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
        BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype             EQ cust.type
          AND oe-prmtx.cust-no             EQ ""
          AND oe-prmtx.procat              EQ itemfg.procat
          AND SUBSTR(oe-prmtx.i-no,01,100) EQ ""
          AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
        BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype             EQ ""
          AND oe-prmtx.cust-no             EQ ""
          AND oe-prmtx.procat              EQ itemfg.procat
          AND oe-prmtx.i-no                BEGINS v-i-item
          AND SUBSTR(oe-prmtx.i-no,01,100) EQ v-i-item
          AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
        BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
      LEAVE.
    END.

    IF NOT AVAIL oe-prmtx THEN
    FOR EACH oe-prmtx
        {oe/oe-prmtxW.i}
          AND oe-prmtx.custype             EQ ""
          AND oe-prmtx.cust-no             EQ ""
          AND oe-prmtx.procat              EQ ""
          AND oe-prmtx.i-no                BEGINS v-i-item
          AND SUBSTR(oe-prmtx.i-no,01,100) EQ v-i-item
          AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
        BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
      LEAVE.
    END.

  IF NOT AVAIL oe-prmtx THEN
  ASSIGN
    i = 0
    matrixExists = NO.
  else do:
    matrixExists = YES.
    if cust.auto-reprice then do:
      v-i-qty = class-qty[index("123456789XYZ",trim(itemfg.class)) + 1].
      if itemfg.class eq "Z"     and
         class-qty[1] gt v-i-qty then v-i-qty = class-qty[1].
    end.

    do i = (IF cust.cust-level EQ 0 THEN 1 ELSE cust.cust-level) to 10:
      if v-i-qty le oe-prmtx.qty[i] then do:
        if oe-prmtx.meth then
          assign
           v-i-price = oe-prmtx.price[i]
           v-i-uom = oe-prmtx.uom[i].
        else
          assign
           v-i-price = itemfg.sell-price -
                      round((itemfg.sell-price * oe-prmtx.discount[i]) / 100,2).
        i = 99.
        leave.
      end.
    end.
  end.  
/* end ---------------------------------- copr. 1996  advanced software, inc. */
