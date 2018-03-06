
DEF BUFFER b-{1}-edit FOR {1}.

/*assuming only called for item*/
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

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

FIND FIRST b-qty WHERE
     b-qty.reftable = "vend-qty" AND
     b-qty.company = e-item-vend.company AND
     b-qty.CODE    = e-item-vend.i-no AND
     b-qty.code2   = e-item-vend.vend-no
     NO-ERROR.

FIND FIRST b-cost WHERE
     b-cost.reftable = "vend-cost" AND
     b-cost.company = e-item-vend.company AND
     b-cost.CODE    = e-item-vend.i-no AND
     b-cost.code2   = e-item-vend.vend-no
     NO-ERROR.

FIND FIRST b-setup WHERE
     b-setup.reftable = "vend-setup" AND
     b-setup.company = e-item-vend.company AND
     b-setup.CODE    = e-item-vend.i-no AND
     b-setup.code2   = e-item-vend.vend-no
     NO-ERROR.

IF AVAIL b-qty AND AVAIL b-cost AND AVAIL b-setup THEN
DO:
   DO li-{1} = 1 TO 10:
      CREATE tt-{1}.
      ASSIGN
         tt-{1}-qty = b-qty.val[li-{1}]
         tt-{1}-cst = b-cost.val[li-{1}]
         tt-{1}-stp = b-setup.val[li-{1}].
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
        b-qty.val[li-{1} - 10] = tt-{1}-qty
        b-cost.val[li-{1} - 10] = tt-{1}-cst
        b-setup.val[li-{1} - 10] = tt-{1}-stp.

     IF LAST(tt-{1}-qty) OR li-{1} EQ 20 THEN
        b-qty.val[li-{1} - 10] = 9999999.9.
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
