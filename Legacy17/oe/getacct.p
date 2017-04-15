/* --------------------------------------------------- oe/getacct.p 9/93 rd  */
/* Invoicing  - Post Invoicing, Get A/R Account Numbers                      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i SHARED}

DEFINE SHARED VARIABLE v-ar-acct LIKE ar-ctrl.receivables.
DEFINE SHARED VARIABLE v-ar-freight LIKE ar-ctrl.freight.
DEFINE SHARED VARIABLE v-ar-stax LIKE ar-ctrl.stax.
DEFINE SHARED VARIABLE v-ar-sales LIKE ar-ctrl.sales.
DEFINE SHARED VARIABLE v-ar-disc LIKE ar-ctrl.discount.
DEFINE SHARED VARIABLE v-return AS logical.


FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK NO-ERROR.

v-return = NOT AVAIL ar-ctrl.
    
IF v-return THEN DO:
  MESSAGE "A/R Control File does not exist for this company..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

ELSE DO:
  /** GET A/R RECEIVABLES ACCOUNT # **/
  FIND FIRST account
      WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.receivables
      NO-LOCK NO-ERROR.
  v-return = NOT AVAIL account.
END.

IF v-return THEN do:
  MESSAGE "A/R Control File has a null or invalid Receivables Account..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

ELSE DO:
  /** GET A/R FREIGHT ACCOUNT # **/
  FIND FIRST account
      WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.freight
      NO-LOCK NO-ERROR.
  v-return = NOT AVAIL account.
END.
 
IF v-return THEN DO:
  MESSAGE "A/R Control File has a null or invalid Freight Account..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

ELSE DO:
  /** GET A/R SALES TAX ACCOUNT # **/
  FIND FIRST account
      WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.stax
      NO-LOCK NO-ERROR.
  v-return = NOT AVAIL account.
END.

IF v-return THEN DO:
  MESSAGE "A/R Control File has a null or invalid Sales Tax Account..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

ELSE DO:
  /** GET A/R SALES ACCOUNT # **/
  FIND FIRST account
      WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.sales
      NO-LOCK NO-ERROR.
  v-return = NOT AVAIL account.
END.
  
IF v-return THEN DO:
  MESSAGE "A/R Control File has a null or invalid Sales Account..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

ELSE DO:
  /** GET A/R DISCOUNT ACCOUNT # **/
  FIND FIRST account
      WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.discount
      NO-LOCK NO-ERROR.
  v-return = NOT AVAIL account.
END.

IF v-return THEN DO:
  MESSAGE "A/R Control File has a null or invalid Discount Account..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

ELSE
  assign
   v-ar-acct    = ar-ctrl.receivables
   v-ar-freight = ar-ctrl.freight
   v-ar-stax    = ar-ctrl.stax
   v-ar-sales   = ar-ctrl.sales
   v-ar-disc    = ar-ctrl.discount.

/* END ---------------------------------- copr. 1993  Advanced Software, Inc. */

