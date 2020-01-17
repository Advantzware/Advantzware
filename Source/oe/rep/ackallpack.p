/* -------------------------------------------------oe/rep/acknowl.p 6/93 rd */
/* ORDER ACKNOLEDGEMENT                                                      */
/* for ORDER STATUS = (R), (U)                                               */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
DEFINE VARIABLE save_id AS RECID.

{oe/rep/acknowl.i}

DEFINE VARIABLE v-salesman         AS CHARACTER FORMAT "x(3)".
DEFINE VARIABLE v-fob              AS CHARACTER FORMAT "x(27)".
DEFINE VARIABLE v-shipvia          LIKE carrier.dscr.
DEFINE VARIABLE v-addr3            AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-addr4            AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-sold-addr3       AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-line             AS INTEGER.
DEFINE VARIABLE v-printline        AS INTEGER.
DEFINE VARIABLE v-ackhead          AS CHARACTER FORMAT "x(32)" INIT
    "A C K N O W L E D G E M E N T".
DEFINE VARIABLE v-len              AS INTEGER.
DEFINE VARIABLE v-totord           AS DECIMAL   FORMAT "->>,>>>,>>9.99".
DEFINE VARIABLE v-totlin           AS DECIMAL   FORMAT "->>,>>>,>>9.99".
DEFINE VARIABLE v-ans              AS LOG       INIT NO.
DEFINE VARIABLE lcnt               AS INTEGER   INIT 1.
DEFINE VARIABLE pagebreak          AS INTEGER   INIT 28.
DEFINE VARIABLE v-cust-phone       AS CHARACTER FORMAT "(999)999-9999" NO-UNDO.
DEFINE VARIABLE v-part             LIKE oe-ordl.part-no NO-UNDO.
DEFINE VARIABLE v-tax-rate         AS DECIMAL   FORMAT ">,>>9.99<<<".
DEFINE VARIABLE v-frt-tax-rate     LIKE v-tax-rate.
DEFINE VARIABLE ll-calc-disc-first AS LOG       NO-UNDO.

FORMAT
    v-line         TO  3 FORMAT ">>9"
    oe-ordl.i-no   AT  5
    oe-ordl.i-name AT 22
    oe-ordl.qty    TO 66 FORMAT "->>,>>>,>>9"
    oe-ordl.price  TO 77 FORMAT "->>>,>>9.99<<<<" SPACE(0)
    oe-ordl.pr-uom TO 80 FORMAT "x(3)"
    WITH FRAME detail NO-LABELS NO-BOX NO-UNDERLINE DOWN STREAM-IO WIDTH 90.

FORMAT
    v-line TO 3 FORMAT ">>9"
    oe-ordm.charge AT  5 SPACE(2)
    oe-ordm.dscr FORMAT "x(30)"
    oe-ordm.tax AT 65
    oe-ordm.amt TO 78 FORMAT "->>,>>9.99" SKIP(1)
    WITH FRAME detailm NO-LABELS NO-BOX NO-UNDERLINE DOWN STREAM-IO WIDTH 90.

FORMAT
    lcnt FORMAT ">9" TO 10
    oe-rel.qty          SPACE(3)
    oe-rel.rel-date     FORMAT "99/99/99"
    WITH FRAME sched-rel NO-LABELS NO-BOX NO-UNDERLINE DOWN STREAM-IO WIDTH 90.

FORMAT
    shipto.ship-name AT 18 SKIP
    shipto.ship-addr AT 18 SKIP
    v-addr4          AT 18 SKIP
    WITH FRAME shipto-rel NO-LABELS NO-BOX NO-UNDERLINE DOWN STREAM-IO WIDTH 90.


ll-calc-disc-first = NO.
FOR EACH sys-ctrl
    WHERE sys-ctrl.company  EQ cocode
    AND sys-ctrl.name     EQ "INVPRINT"
    AND sys-ctrl.char-fld EQ "Dayton"
    NO-LOCK:
    ll-calc-disc-first = YES.
    LEAVE.
END.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id :

    IF oe-ord.sman[2] EQ "" AND oe-ord.sman[3] EQ "" THEN
        v-salesman = oe-ord.sman[1].
    ELSE
        v-salesman = oe-ord.sman[1] + oe-ord.sman[2] + oe-ord.sman[3].

    IF oe-ord.fob-code EQ "ORIG" THEN
        v-fob = "Origin".
    ELSE
        v-fob = "Destination".

    FIND FIRST carrier
        WHERE carrier.company EQ oe-ord.company
        AND carrier.carrier EQ oe-ord.carrier
        NO-LOCK NO-ERROR.
    IF AVAILABLE carrier THEN
        v-shipvia = carrier.dscr.
    ELSE
        v-shipvia = "".

    ASSIGN
        v-addr3      = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
        v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
                      "  " + oe-ord.sold-zip
        v-line       = 1.

    FIND FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ oe-ord.cust-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE cust THEN v-cust-phone = cust.area-code + cust.phone.

     
    {oe/rep/ackallpack.i}

    FOR EACH oe-ordl
        WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no
        NO-LOCK:

        IF (v-printline + 3) GT pagebreak THEN 
        DO:
            v-printline = 0.
            PAGE.
            {oe/rep/ackallpack.i}
        END.

        DISPLAY v-line
            oe-ordl.i-no
            oe-ordl.i-name
            oe-ordl.qty
            oe-ordl.price
            oe-ordl.pr-uom
            WITH FRAME detail NO-ATTR-SPACE.

        v-printline = v-printline + 1.

        IF oe-ordl.i-no NE oe-ordl.part-no OR
            oe-ordl.part-dscr1 NE ""        THEN 
        DO:
            v-part = IF oe-ordl.i-no NE oe-ordl.part-no THEN oe-ordl.part-no
            ELSE "".
            PUT v-part             AT 5
                oe-ordl.part-dscr1 AT 22 SKIP.
            v-printline = v-printline + 1.
        END.

        IF oe-ordl.part-dscr2 NE "" THEN 
        DO:
            PUT oe-ordl.part-dscr2 AT 22 SKIP.
            v-printline = v-printline + 1.
        END.

        FOR EACH oe-rel
            WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
            AND ((oe-rel.link-no EQ 0 AND v-schrel)
            OR  (oe-rel.link-no NE 0 AND v-actrel))
            NO-LOCK BREAK BY oe-rel.link-no DESCENDING WITH FRAME sched-rel DOWN:

            IF FIRST-OF(oe-rel.link-no) THEN
                IF oe-rel.link-no EQ 0 THEN lcnt = 1.
                ELSE
                    IF FIRST(oe-rel.link-no) THEN lcnt = 1.

            IF (v-printline GE pagebreak) OR
                (v-printline GE pagebreak - 1 AND lcnt EQ 1) THEN 
            DO:
                v-printline = 0.
                PAGE.
                {oe/rep/ackallpack.i}
            END.

            IF FIRST-OF(oe-rel.link-no) THEN 
            DO:
                IF oe-rel.link-no EQ 0 THEN 
                DO:
                    PUT "Scheduled Releases:" AT 10 SKIP.
                    v-printline = v-printline + 1.
                END.
                ELSE
                    IF FIRST(oe-rel.link-no) THEN 
                    DO:
                        PUT "Actual Releases:" AT 10 SKIP.
                        v-printline = v-printline + 1.
                    END.
            END.

            {oe/rel-stat.i lv-stat}
            IF AVAILABLE oe-rell THEN
                FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.

            DISPLAY lcnt
                (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty) @ oe-rel.qty
                oe-rel.rel-date
                oe-relh.rel-date 
                WHEN AVAILABLE oe-relh @ oe-rel.rel-date.
            DOWN WITH FRAME sched-rel.
            ASSIGN
                v-printline = v-printline + 1
                lcnt        = lcnt + 1.
            IF v-shipto THEN 
            DO:
                FIND FIRST shipto
                    WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ oe-rel.cust-no
                    AND shipto.ship-id EQ oe-rel.ship-id
                    NO-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN
                    v-addr4 = shipto.ship-city + ", " +
                        shipto.ship-state + "  " + shipto.ship-zip.

                IF v-printline + 4 GE pagebreak THEN 
                DO:
                    v-printline = 0.
                    PAGE.
                    {oe/rep/ackallpack.i}
                END.

                DISPLAY shipto.ship-name shipto.ship-addr v-addr4
                    WITH FRAME shipto-rel.
                v-printline = v-printline + 4.
            END.
        END.   /* for each oe-rel  */

        PUT "" SKIP.
        ASSIGN
            v-line      = v-line + 1
            v-printline = v-printline + 1.

        IF v-printline GE pagebreak THEN 
        DO:
            v-printline = 0.
            PAGE.
            {oe/rep/ackallpack.i}
        END.

        IF oe-ordl.pr-uom BEGINS "L" AND oe-ordl.pr-uom NE "LB" THEN
            ASSIGN v-totlin = oe-ordl.price * IF oe-ordl.qty LT 0 THEN -1 ELSE 1.
                             
        ELSE
            IF oe-ordl.pr-uom EQ "CS" THEN
            DO:
                FIND FIRST itemfg {sys/look/itemfgrlW.i}
                    AND itemfg.i-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.

                v-totlin = oe-ordl.qty /
                    (IF oe-ordl.cas-cnt NE 0 THEN oe-ordl.cas-cnt ELSE
                    IF AVAILABLE itemfg AND itemfg.case-count NE 0
                    THEN itemfg.case-count ELSE 1) *
                    oe-ordl.price.
            END.
            ELSE
                IF oe-ordl.pr-uom EQ "C" THEN
                    v-totlin = oe-ordl.qty / 100 * oe-ordl.price.

                ELSE
                    IF oe-ordl.pr-uom EQ "M" THEN
                        v-totlin = oe-ordl.qty / 1000 * oe-ordl.price.

                    ELSE /** DEFAULT TO EACH **/
                        v-totlin = oe-ordl.qty * oe-ordl.price.

        v-totlin = ROUND(v-totlin,2).

        IF oe-ordl.disc NE 0 THEN
            v-totlin = IF ll-calc-disc-first THEN 
                (v-totlin - ROUND(v-totlin * oe-ordl.disc / 100,2))
                ELSE
                ROUND(v-totlin * (1 - (oe-ordl.disc / 100)),2).

        v-totord = v-totord + v-totlin.

        IF v-printline GE pagebreak THEN
        DO:
            ASSIGN 
                v-printline = 0.
            PAGE.
            {oe/rep/ackallpack.i}
        END.
        ELSE
            DOWN WITH FRAME detail.
    END. /* each oe-ordl */
    FOR EACH oe-ordm NO-LOCK WHERE oe-ordm.company EQ oe-ord.company AND
        oe-ordm.ord-no EQ oe-ord.ord-no BREAK BY ord-no:
        IF FIRST(oe-ordm.ord-no) THEN
        DO:
            PUT "** Miscellaneous Items **" AT 23.
            IF v-print-fmt EQ "HOP" THEN PUT "Taxable" AT 62.
            PUT SKIP(1).
            ASSIGN 
                v-printline = v-printline + 2.
        END.

        IF v-printline GE pagebreak THEN
        DO:
            ASSIGN 
                v-printline = 0.
            PAGE.
            {oe/rep/ackallpack.i}
        END.
        ELSE
            DOWN WITH FRAME detailm.
        IF oe-ordm.bill EQ "N" THEN
            DISPLAY
                v-line oe-ordm.charge oe-ordm.dscr "     N/C" @ oe-ordm.amt
                WITH FRAME detailm.
        ELSE
            DISPLAY
                v-line oe-ordm.charge oe-ordm.dscr
                oe-ordm.tax WHEN v-print-fmt EQ "HOP"
                oe-ordm.amt
                WITH FRAME detailm.
        ASSIGN 
            v-line      = v-line + 1
            v-printline = v-printline + 2.
        IF oe-ordm.bill NE "N" THEN
            ASSIGN v-totord = v-totord + oe-ordm.amt.
    END. /* each oe-ordm */
    IF v-prntinst THEN
    DO:
        DO i = 1 TO 4:
            IF oe-ord.bill-i[i] NE "" THEN
            DO:
                IF v-printline GE pagebreak THEN
                DO:
                    ASSIGN 
                        v-printline = 0.
                    PAGE.
                    {oe/rep/ackallpack.i}
                END.

                PUT oe-ord.bill-i[i] AT 5 SKIP.
                ASSIGN 
                    v-printline = v-printline + 1.
            END.
        END. /* 1 to 4 */
    END.

    IF v-print-fmt EQ "HOP" THEN 
    DO:
        PUT "Total Order Value:" TO 65 v-totord SKIP.
        IF oe-ord.tax GT 0 THEN 
        DO:
            RUN ar/cctaxrt.p (INPUT cocode, oe-ord.tax-gr,
                OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).
            PUT "Tax " + string(v-tax-rate,">9.99%") + ":" FORMAT "x(11)" TO 65
                oe-ord.tax FORMAT "->>,>>>,>>9.99" SKIP
                "Total w/Tax:" TO 65
                v-totord + oe-ord.tax FORMAT "->>,>>>,>>9.99" SKIP.
        END.
    END.

    ELSE 
    DO:
        PUT "Total Order Value:" TO 65 v-totord SKIP.
    END.

    ASSIGN
        v-totord        = 0
        oe-ord.ack-prnt = YES.
    PAGE.
    ASSIGN 
        v-printline = 0.
END. /* each oe-ord */

