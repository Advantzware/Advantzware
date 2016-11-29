
DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipdBoardCost AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcMarkupOn AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdMarkup AS DECIMAL NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE SHARED VARIABLE qty       AS INTEGER   NO-UNDO.

DEFINE        VARIABLE dYieldQty AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE dSqf      AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE dLookup   AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE cLookup   AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lFound    AS LOGICAL   NO-UNDO.

{cec/msfcalc.i}

FIND eb WHERE ROWID(eb) EQ ipriEb NO-LOCK NO-ERROR.

IF AVAILABLE eb THEN 
DO:
    RUN sys/ref/nk1look.p (eb.company,
        "CEMarkupMatrixLookup",
        "C",
        NO,
        NO,
        "",
        "",
        OUTPUT cLookup,
        OUTPUT lFound).

    IF NOT lFound OR cLookup EQ "" THEN 
        cLookup = "Square Feet".
    IF cLookup EQ "Square Feet" THEN 
    DO:
        ASSIGN
            dYieldQty = IF eb.est-type LE 4 THEN
                  IF eb.cust-% LT 0 THEN -1 / eb.cust-% ELSE
                  IF eb.cust-% EQ 0 THEN 1              ELSE eb.cust-%
                ELSE                  
                  IF eb.yld-qty LT 0 THEN -1 / eb.yld-qty ELSE
                  IF eb.yld-qty EQ 0 THEN 1               ELSE eb.yld-qty
            dSqf      = eb.t-sqin * qty * dYieldQty
            dLookup   = IF v-corr THEN (dSqf * .007) ELSE (dSqf / 144).
    END.
    ELSE 
        dLookup = ipdBoardCost.
        
    RUN est/GetMarkup(eb.company,
        eb.cust-no,
        eb.procat,
        eb.style,
        dLookup,
        OUTPUT iopdMarkup,
        OUTPUT iopcMarkupOn).
    
    MESSAGE dLookup SKIP
        "Return values " iopdMarkup iopcMarkupOn VIEW-AS ALERT-BOX.
/*    FOR EACH cust-markup NO-LOCK                          */
/*        WHERE cust-markup.company EQ eb.company           */
/*        AND cust-markup.cust-no EQ eb.cust-no             */
/*        AND (cust-markup.style  EQ eb.style OR            */
/*        cust-markup.style  EQ "")                         */
/*        AND (cust-markup.procat EQ eb.procat OR           */
/*        cust-markup.procat EQ "")                         */
/*        BREAK BY cust-markup.procat DESCENDING            */
/*        BY cust-markup.style  DESCENDING:                 */
/*                                                          */
/*                                                          */
/*        DO li = 1 TO EXTENT(cust-markup.run-qty):         */
/*            IF cust-markup.run-qty[li] GE dSqf THEN     */
/*            DO:                                           */
/*                ASSIGN                                    */
/*                    iopcMarkupOn = cust-markup.markup-on[li]*/
/*                    iopdMarkup     = cust-markup.markup[li].  */
/*                LEAVE.                                    */
/*            END.                                          */
/*        END.                                              */
/*                                                          */
/*        LEAVE.                                            */
/*    END.                                                  */
END.

IF iopcMarkupOn EQ "" THEN iopcMarkupOn = "N".

