
DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipdBoardCost AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdFactCost AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdFullCost AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdBoardPct AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcMarkupOn AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdMarkup AS DECIMAL NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE SHARED VARIABLE qty       AS INTEGER   NO-UNDO.

DEFINE        VARIABLE dQuantityPerSet AS DECIMAL   NO-UNDO.
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
    CASE cLookup:
        WHEN "Square Feet" THEN 
            DO:
                ASSIGN
                    dQuantityPerSet = IF eb.est-type LE 4 THEN
                  IF eb.cust-% LT 0 THEN -1 / eb.cust-% ELSE
                  IF eb.cust-% EQ 0 THEN 1              ELSE eb.cust-%
                ELSE                  
                  IF eb.quantityPerSet LT 0 THEN -1 / eb.quantityPerSet ELSE
                  IF eb.quantityPerSet EQ 0 THEN 1               ELSE eb.quantityPerSet
                    dSqf      = eb.t-sqin * qty * dQuantityPerSet
                    dLookup   = IF v-corr THEN (dSqf * .007) ELSE (dSqf / 144).
            END.
        WHEN "Board Cost" THEN 
            dLookup = ipdBoardCost.
        WHEN "Factory Cost" THEN 
            dLookup = ipdFactCost.
        WHEN "Full Cost" THEN 
            dlookup = ipdFullCost.
    END CASE. 
    
    RUN est/GetMarkup.p (eb.company,
        eb.cust-no,
        eb.procat,
        eb.style,
        dLookup,
        ipdBoardPct,
        INPUT-OUTPUT iopdMarkup,
        INPUT-OUTPUT iopcMarkupOn).
    
        
END.

IF iopcMarkupOn EQ "" THEN iopcMarkupOn = "N".

