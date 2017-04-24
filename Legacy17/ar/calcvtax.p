/* ar/calcvtax.p */
DEF INPUT  PARAMETER v-tax-code     LIKE stax.tax-gr NO-UNDO.
DEF INPUT  PARAMETER v-frt          AS LOG NO-UNDO.
DEF INPUT  PARAMETER v-price-in     AS DEC NO-UNDO. /* item total price */
DEF OUTPUT PARAMETER v-tax-amt-out  AS DEC NO-UNDO. /* total tax amount */

{sys/inc/var.i shared}
/*{sys/form/s-top.f} */

DEF VAR v-amt-1 AS DEC NO-UNDO INIT 0. /* Dollar limit amount */
DEF VAR v-amt-2 AS DEC NO-UNDO INIT 0. /* Exceeded amount */

DEF VAR v-tax-amt-1 AS DEC NO-UNDO INIT 0. /* Tax amount for dollar limit */
DEF VAR v-tax-amt-2 AS DEC NO-UNDO INIT 0. /* Tax amount for exceeded. */


ASSIGN v-tax-amt-out = 0.

FIND FIRST stax WHERE stax.company = cocode AND
                      stax.tax-group = v-tax-code NO-LOCK NO-ERROR.

IF NOT AVAIL stax THEN RETURN.

/* Separate the dollar limit amt from the exceeded amount. */
ASSIGN v-amt-1 = stax.tax-rate1[5] /* Set to dollar limit. */
       v-amt-2 = (v-price-in - stax.tax-rate1[5]). /* exceeded amt */

/* Dollar limit amt taxed with all taxes (first 4). */
DO i = 1 TO 4:
  /* Skip if tax code is blank. */
  IF stax.tax-code1[i] = "" THEN NEXT.
  /* If v-frt = no or stax frt1 = yes... */
  IF NOT v-frt OR stax.tax-frt1[i] THEN DO:
    /* Calculate the tax amount. */
    ASSIGN v-tax-amt-1 = ROUND(v-amt-1 * stax.tax-rate1[i] / 100,2).
    
    /* if stax.accum-tax eq "yes" then add tax amount to item total price. */
    IF stax.accum-tax THEN 
        ASSIGN v-price-in  = (v-amt-1 + v-tax-amt-1).

    /* Accumulate total amount out. */
    ASSIGN v-tax-amt-out = v-tax-amt-out + v-tax-amt-1.
  END.
END.

/* Tax the exceeded amount at first rate. */
ASSIGN v-tax-amt-2 = ROUND(v-amt-2 * stax.tax-rate1[1] / 100,2).


/* Accumulate total tax amount out. */
ASSIGN v-tax-amt-out = v-tax-amt-out + v-tax-amt-2.

/* Round the tax amount out. */
ASSIGN v-tax-amt-out = ROUND(v-tax-amt-out,2).

RETURN.
