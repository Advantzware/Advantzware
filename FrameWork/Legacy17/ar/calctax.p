
DEF INPUT  PARAMETER v-tax-code     LIKE stax.tax-gr NO-UNDO.
DEF INPUT  PARAMETER v-frt          AS LOG NO-UNDO.
DEF INPUT  PARAMETER v-price-in     AS DEC NO-UNDO. /* item total price */
DEF OUTPUT PARAMETER v-tax-amt-out  AS DEC NO-UNDO INIT 0. /* total tax amount */

{sys/inc/var.i shared}
/*{sys/form/s-top.f} */

DEF VAR v-tax-amt AS DEC NO-UNDO INIT 0. /* Tax amount */

ASSIGN v-tax-amt-out = 0.

FIND FIRST stax WHERE stax.company = cocode AND
                      stax.tax-group = v-tax-code NO-LOCK NO-ERROR.


IF NOT AVAIL stax THEN RETURN.

DO i = 1 TO EXTENT(stax.tax-rate1):

    /* Skip if tax code is blank. */
  IF stax.tax-code1[i] = "" THEN NEXT.

  IF NOT v-frt or stax.tax-frt1[i] THEN DO:
    ASSIGN v-tax-amt = ROUND(v-price-in * stax.tax-rate1[i] / 100,2).

    /*if stax.company eq "yes" then v-in = v-in + v-amt. */
    IF stax.accum-tax THEN 
        ASSIGN v-price-in  = (v-price-in + v-tax-amt).

    /* Accumulate total amount out. */
    ASSIGN v-tax-amt-out = (v-tax-amt-out + v-tax-amt).
  END.
END.

/* Round the tax amount out. */
ASSIGN v-tax-amt-out = ROUND(v-tax-amt-out,2).
