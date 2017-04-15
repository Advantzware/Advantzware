/* oe/updprmtx2.p copied from oe/updprmtx.p and added param ip-TransQ 
                  called from est/vp-prmtx.w */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-cust-no LIKE oe-prmtx.cust-no NO-UNDO.
DEF INPUT PARAM ip-qty AS DEC DECIMALS 10 NO-UNDO.
DEF INPUT PARAM ip-uom AS CHAR NO-UNDO.
DEF INPUT PARAM ip-prc AS DEC DECIMALS 10 NO-UNDO.
DEF INPUT PARAM ip-TransQ AS cha NO-UNDO.  /* Q for Qty or 1 form Minus 1 */

{sys/inc/VAR.i SHARED}

DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR lv-qty LIKE quoteqty.qty NO-UNDO.
DEF VAR ll-msg AS LOG INIT YES NO-UNDO.
DEF VAR lv-date AS CHAR NO-UNDO.
DEF VAR vdPrevPrice AS DEC NO-UNDO.

DEF TEMP-TABLE w-matrix NO-UNDO
    FIELD qty     AS DEC
    FIELD uom     AS CHAR
    FIELD price   AS DEC
    FIELD price-m AS DEC.

DEF BUFFER b-matrix FOR w-matrix.

SESSION:SET-WAIT-STATE ("general").
    
lv-date = STRING(YEAR(TODAY),"9999") +
          STRING(MONTH(TODAY),"99")  +
          STRING(DAY(TODAY),"99").

FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO:
  CREATE w-matrix.
  ASSIGN
   w-matrix.qty   = ip-qty
   w-matrix.uom   = ip-uom
   w-matrix.price = ip-prc.

  RUN update-matrix.
END.

ELSE DO:
  FIND quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.

  IF NOT AVAIL quotehd THEN DO:
    FIND quoteitm WHERE ROWID(quoteitm) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAIL quoteitm THEN DO:
      FIND FIRST quotehd
          WHERE quotehd.company EQ quoteitm.company
            AND quotehd.loc     EQ quoteitm.loc
            AND quotehd.q-no    EQ quoteitm.q-no
          NO-LOCK NO-ERROR.
      ll-msg = NO.
    END.
  END.
END.

RELEASE quoteitm.

IF AVAIL quotehd THEN DO:
  ip-cust-no = quotehd.cust-no.

  FOR EACH quoteitm
      WHERE quoteitm.company EQ quotehd.company
        AND quoteitm.loc     EQ quotehd.loc
        AND quoteitm.q-no    EQ quotehd.q-no
        AND (ROWID(quoteitm) EQ ip-rowid OR
             ROWID(quotehd)  EQ ip-rowid)
      NO-LOCK,

      FIRST itemfg
      WHERE itemfg.company  EQ quoteitm.company
        AND itemfg.part-no  EQ quoteitm.part-no
        AND itemfg.part-no  NE ""
        AND (itemfg.cust-no EQ quotehd.cust-no OR
             itemfg.i-code  EQ "S")
      NO-LOCK:

    FOR EACH w-matrix:
      DELETE w-matrix.
    END.

    FOR EACH quoteqty OF quoteitm NO-LOCK:
      CREATE w-matrix.
      ASSIGN
       w-matrix.qty   = quoteqty.qty
       w-matrix.uom   = quoteqty.uom
       w-matrix.price = quoteqty.price.
    END.
    
    IF ip-TransQ = "Q" THEN RUN update-matrix.
    ELSE IF ip-TransQ = "1" THEN RUN update-matrix-minus.

  END.
END.

IF ll-msg THEN DO:
  SESSION:SET-WAIT-STATE ("").

  MESSAGE "Create/update of FG price matrix completed..."
      VIEW-AS ALERT-BOX.
END.

RETURN.

PROCEDURE update-matrix.
  FOR EACH oe-prmtx
      WHERE oe-prmtx.company            EQ itemfg.company
        AND oe-prmtx.cust-no            EQ ip-cust-no
        AND oe-prmtx.i-no               BEGINS itemfg.i-no
        AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no
/*         AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:         */
      BY oe-prmtx.eff-date DESC:
    LEAVE.
  END.

  IF NOT AVAIL oe-prmtx THEN DO:
    CREATE oe-prmtx.
    ASSIGN
     oe-prmtx.company = itemfg.company
     oe-prmtx.cust-no = ip-cust-no
     oe-prmtx.eff-date = TODAY
     oe-prmtx.i-no = itemfg.i-no.
/*      oe-prmtx.i-no    = STRING(itemfg.i-no,"x(100)") + lv-date. */
  END.
  oe-prmtx.meth = YES.

  DO li = 1 TO EXTENT(oe-prmtx.qty):
    IF oe-prmtx.qty[li] NE 0 THEN DO:
      CREATE w-matrix.
      ASSIGN
       w-matrix.qty   = oe-prmtx.qty[li]
       w-matrix.uom   = oe-prmtx.uom[li]
       w-matrix.price = oe-prmtx.price[li].
    END.
  END.

  FOR EACH w-matrix
      WHERE w-matrix.qty   EQ 0
         OR w-matrix.price EQ 0:
    DELETE w-matrix.
  END.

  FOR EACH w-matrix:
    w-matrix.price-m = w-matrix.price.

    IF w-matrix.uom NE "M" THEN
      RUN sys/ref/convcuom.p (w-matrix.uom, "M", 0, 0, 0, 0,
                              w-matrix.price-m, OUTPUT w-matrix.price-m).
  END.

  FOR EACH b-matrix BY b-matrix.qty:
    FOR EACH w-matrix
        WHERE w-matrix.qty GE b-matrix.qty
          AND (w-matrix.price-m GE b-matrix.price-m OR
               w-matrix.qty EQ b-matrix.qty)
          AND ROWID(w-matrix) NE ROWID(b-matrix):
      DELETE w-matrix.
    END.
  END.

  ASSIGN
   oe-prmtx.qty   = 0
   oe-prmtx.uom   = ""
   oe-prmtx.price = 0
   li             = 0.

  FOR EACH w-matrix WHERE w-matrix.qty LE 0:
    DELETE w-matrix.
  END.

  FOR EACH w-matrix BREAK BY w-matrix.qty:
    IF LAST(w-matrix.qty) THEN DO:
      CREATE b-matrix.
      BUFFER-COPY w-matrix TO b-matrix
      ASSIGN
       b-matrix.qty = w-matrix.qty + 1.
    END.
  END.

  FOR EACH w-matrix BREAK BY w-matrix.qty DESC:
    li = li + 1.

    IF li LE EXTENT(oe-prmtx.qty) THEN
      ASSIGN
       oe-prmtx.qty[li]  = w-matrix.qty
       oe-prmtx.uom[li]  = w-matrix.uom
       oe-prmtx.price[li]= w-matrix.price.
  END.

END PROCEDURE.

PROCEDURE update-matrix-minus.
  FOR EACH oe-prmtx
      WHERE oe-prmtx.company            EQ itemfg.company
        AND oe-prmtx.cust-no            EQ ip-cust-no
        AND oe-prmtx.i-no               BEGINS itemfg.i-no
        AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no
/*         AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:         */
      BY oe-prmtx.eff-date DESC:
    LEAVE.
  END.

  IF NOT AVAIL oe-prmtx THEN DO:
    CREATE oe-prmtx.
    ASSIGN
     oe-prmtx.company = itemfg.company
     oe-prmtx.cust-no = ip-cust-no
     oe-prmtx.eff-date = TODAY
     oe-prmtx.i-no = itemfg.i-no.
/*      oe-prmtx.i-no    = STRING(itemfg.i-no,"x(100)") + lv-date. */
  END.
  oe-prmtx.meth = YES.

  DO li = 1 TO EXTENT(oe-prmtx.qty):
    IF oe-prmtx.qty[li] NE 0 THEN DO:
      CREATE w-matrix.
      ASSIGN
       w-matrix.qty   = oe-prmtx.qty[li]
       w-matrix.uom   = oe-prmtx.uom[li]
       w-matrix.price = oe-prmtx.price[li].
    END.
  END.

  FOR EACH w-matrix
      WHERE w-matrix.qty   EQ 0
         OR w-matrix.price EQ 0:
    DELETE w-matrix.
  END.

  FOR EACH w-matrix:
    w-matrix.price-m = w-matrix.price.

    IF w-matrix.uom NE "M" THEN
      RUN sys/ref/convcuom.p (w-matrix.uom, "M", 0, 0, 0, 0,
                              w-matrix.price-m, OUTPUT w-matrix.price-m).
  END.

  FOR EACH b-matrix BY b-matrix.qty:
    FOR EACH w-matrix
        WHERE w-matrix.qty GE b-matrix.qty
          AND (w-matrix.price-m GE b-matrix.price-m OR
               w-matrix.qty EQ b-matrix.qty)
          AND ROWID(w-matrix) NE ROWID(b-matrix):
      DELETE w-matrix.
    END.
  END.

  ASSIGN
   oe-prmtx.qty   = 0
   oe-prmtx.uom   = ""
   oe-prmtx.price = 0
   li             = 0.

  FOR EACH w-matrix WHERE w-matrix.qty LE 0:
    DELETE w-matrix.
  END.

  FOR EACH w-matrix BREAK BY w-matrix.qty:
    IF LAST(w-matrix.qty) THEN DO:
      CREATE b-matrix.
      BUFFER-COPY w-matrix TO b-matrix
      ASSIGN
       b-matrix.qty = w-matrix.qty + 1.
    END.
  END.

  vdPrevPrice = 0.
  FOR EACH w-matrix BREAK BY w-matrix.qty /*DESC*/:
    li = li + 1.
    
    IF li LE EXTENT(oe-prmtx.qty) THEN DO:

       /*IF li = 1 THEN ASSIGN oe-prmtx.qty[li]  = w-matrix.qty - 1
                             oe-prmtx.uom[li]  = w-matrix.uom
                             oe-prmtx.price[li]= 0.
       ELSE */ ASSIGN oe-prmtx.qty[li]  = w-matrix.qty - 1
                   oe-prmtx.uom[li]  = w-matrix.uom
                   oe-prmtx.price[li]= vdPrevPrice.

       vdPrevPrice = w-matrix.price.
/*MESSAGE "minus: " li SKIP
    oe-prmtx.qty[li] oe-prmtx.price[li] ":" vdPrevPrice w-matrix.qty - 1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
    END.
  END.

END PROCEDURE.
