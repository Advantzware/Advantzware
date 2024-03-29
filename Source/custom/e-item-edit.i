
DEF BUFFER b-{1}-edit FOR {1}.

/*assuming only called for item*/


DEF VAR li-{1} AS INT NO-UNDO.
    
DEF TEMP-TABLE tt-{1} NO-UNDO
    FIELD tt-{1}-qty AS DEC
    FIELD tt-{1}-cst AS DEC
    FIELD tt-{1}-stp AS DEC.

DISABLE TRIGGERS FOR LOAD OF b-{1}-edit.

EMPTY TEMP-TABLE tt-{1}.

DO li-{1} = 1 TO 10:
  CREATE tt-{1}.
  ASSIGN
   tt-{1}-qty = {1}.run-qty[li-{1}]
   tt-{1}-cst = {1}.run-cost[li-{1}]
   tt-{1}-stp = {1}.setups[li-{1}].
  RELEASE tt-{1}.
END.



IF AVAIL e-item-vend THEN
DO:
   DO li-{1} = 1 TO 10:
      CREATE tt-{1}.
      ASSIGN
         tt-{1}-qty = e-item-vend.runQtyXtra[li-{1}]
         tt-{1}-cst = e-item-vend.runCostXtra[li-{1}]
         tt-{1}-stp = e-item-vend.setupsXtra[li-{1}].
      RELEASE tt-{1}.
   END.
END.

ASSIGN
 li-{1}       = 0
 {1}.run-qty  = 0
 {1}.run-cost = 0
 {1}.setups   = 0.

FOR EACH tt-{1} WHERE tt-{1}-qty GT 0 BREAK BY tt-{1}-qty:

  li-{1} = li-{1} + 1.
  IF li-{1} LE EXTENT({1}.run-qty) THEN DO:
     ASSIGN
        {1}.run-qty[li-{1}]  = tt-{1}-qty
        {1}.run-cost[li-{1}] = tt-{1}-cst
        {1}.setups[li-{1}]   = tt-{1}-stp.

    /*IF LAST(tt-{1}-qty) THEN
       {1}.run-qty[li-{1}] = 9999999.9.*/
  END.
  ELSE
  DO:
     ASSIGN
        e-item-vend.runQtyXtra[li-{1} - 10] = tt-{1}-qty
        e-item-vend.runCostXtra[li-{1} - 10] = tt-{1}-cst
        e-item-vend.setupsXtra[li-{1} - 10] = tt-{1}-stp.

     IF LAST(tt-{1}-qty) OR li-{1} EQ 20 THEN
        e-item-vend.runQtyXtra[li-{1} - 10] = 9999999.9.
  END.
END.

&IF "{1}" EQ "e-item-vend" &THEN
  &SCOPED-DEFINE e-item e-item
&ELSE
  &SCOPED-DEFINE e-item e-itemfg
&ENDIF

RELEASE {&e-item}.
FIND FIRST {&e-item} NO-LOCK
    WHERE {&e-item}.company EQ {1}.company
      AND {&e-item}.i-no    EQ {1}.i-no
    NO-ERROR.
IF AVAIL {&e-item}                              AND
   NOT CAN-FIND(FIRST b-{1}-edit OF {&e-item}
                WHERE b-{1}-edit.item-type EQ YES
                  AND b-{1}-edit.vend-no   EQ "") THEN DO:
  CREATE b-{1}-edit.
  BUFFER-COPY {&e-item} EXCEPT rec_key TO b-{1}-edit
  ASSIGN
   b-{1}-edit.item-type = YES
   b-{1}-edit.vend-no   = "". 

  /* gdm - 06040918 */
  {custom/rec_key.i b-{1}-edit}

END.
