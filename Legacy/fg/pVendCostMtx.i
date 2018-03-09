/* pVendCostMtx.i - rstark - 3.9.2018 - used in fg/v-eitem.w and fg/v-eitem.w */

DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

DEFINE VARIABLE cText    AS CHARACTER NO-UNDO EXTENT 11.
DEFINE VARIABLE dRunQty  AS DECIMAL   NO-UNDO EXTENT 10.
DEFINE VARIABLE dRunCost AS DECIMAL   NO-UNDO EXTENT 10.
DEFINE VARIABLE dSetups  AS DECIMAL   NO-UNDO EXTENT 10.
DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
DEFINE VARIABLE jdx      AS INTEGER   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    CASE ipcType:
        WHEN "ASSIGN" THEN DO:
            ASSIGN
                {&farmFields}
                dRunQty[01] = run-qty-01 dRunCost[01] = run-cost-01 dSetups[01] = setups-01
                dRunQty[02] = run-qty-02 dRunCost[02] = run-cost-02 dSetups[02] = setups-02
                dRunQty[03] = run-qty-03 dRunCost[03] = run-cost-03 dSetups[03] = setups-03
                dRunQty[04] = run-qty-04 dRunCost[04] = run-cost-04 dSetups[04] = setups-04
                dRunQty[05] = run-qty-05 dRunCost[05] = run-cost-05 dSetups[05] = setups-05
                dRunQty[06] = run-qty-06 dRunCost[06] = run-cost-06 dSetups[06] = setups-06
                dRunQty[07] = run-qty-07 dRunCost[07] = run-cost-07 dSetups[07] = setups-07
                dRunQty[08] = run-qty-08 dRunCost[08] = run-cost-08 dSetups[08] = setups-08
                dRunQty[09] = run-qty-09 dRunCost[09] = run-cost-09 dSetups[09] = setups-09
                dRunQty[10] = run-qty-10 dRunCost[10] = run-cost-10 dSetups[10] = setups-10
                .
            IF lVendCostMtx THEN DO:
                /* get highest bucket populated */
                DO idx = 1 TO EXTENT(dRunQty):
                    IF dRunQty[idx] EQ 0 THEN LEAVE.
                    jdx = jdx + 1.
                END. /* do idx */
                DO idx = 1 TO jdx:
                    dRunQty[idx] = dRunQty[idx] - .001.
                END. /* do idx */
                DO idx = jdx TO 1 BY -1:
                    dRunCost[idx + 1] = dRunCost[idx].
                END. /* do idx */
                ASSIGN
                    dRunQty[jdx + 1] = 9999999.9
                    dRunCost[1]      = 0
                    dSetups[jdx + 1] = dSetups[jdx]
                    .
            END. /* if lvendcostmtx */
            IF AVAILABLE e-itemfg-vend THEN
            DO idx = 1 TO EXTENT(dRunQty):
                ASSIGN
                    e-itemfg-vend.run-qty[idx]  = dRunQty[idx]
                    e-itemfg-vend.run-cost[idx] = dRunCost[idx]
                    e-itemfg-vend.setups[idx]   = dSetups[idx]
                    .
            END. /* do idx */
        END. /* assign */
        WHEN "DISPLAY" THEN DO:
            IF AVAILABLE e-itemfg-vend THEN
            DO idx = 1 TO EXTENT(dRunQty):
                ASSIGN
                    dRunQty[idx]  = e-itemfg-vend.run-qty[idx]
                    dRunCost[idx] = e-itemfg-vend.run-cost[idx]
                    dSetups[idx]  = e-itemfg-vend.setups[idx]
                    .
            END. /* do idx */
            IF lVendCostMtx THEN DO:
                /* get highest bucket populated */
                DO idx = 1 TO EXTENT(dRunQty):
                    IF dRunQty[idx] EQ 0 THEN LEAVE.
                    jdx = jdx + 1.
                END. /* do idx */
                DO idx = 1 TO jdx:
                    dRunQty[idx] = dRunQty[idx] + .001.
                    IF idx LT jdx THEN
                    dRunCost[idx] = dRunCost[idx + 1].
                END. /* do idx */
                IF jdx NE 0 THEN 
                ASSIGN
                    dRunQty[jdx]  = 0
                    dRunCost[jdx] = 0
                    dSetups[jdx]  = 0
                    .
                ENABLE btnShowVendCostMtx.
            END. /* if lvendcostmtx */
            ASSIGN
                run-qty-01 = dRunQty[01] run-cost-01 = dRunCost[01] setups-01 = dSetups[01]
                run-qty-02 = dRunQty[02] run-cost-02 = dRunCost[02] setups-02 = dSetups[02]
                run-qty-03 = dRunQty[03] run-cost-03 = dRunCost[03] setups-03 = dSetups[03]
                run-qty-04 = dRunQty[04] run-cost-04 = dRunCost[04] setups-04 = dSetups[04]
                run-qty-05 = dRunQty[05] run-cost-05 = dRunCost[05] setups-05 = dSetups[05]
                run-qty-06 = dRunQty[06] run-cost-06 = dRunCost[06] setups-06 = dSetups[06]
                run-qty-07 = dRunQty[07] run-cost-07 = dRunCost[07] setups-07 = dSetups[07]
                run-qty-08 = dRunQty[08] run-cost-08 = dRunCost[08] setups-08 = dSetups[08]
                run-qty-09 = dRunQty[09] run-cost-09 = dRunCost[09] setups-09 = dSetups[09]
                run-qty-10 = dRunQty[10] run-cost-10 = dRunCost[10] setups-10 = dSetups[10]
                .
            DISPLAY {&farmFields}.
        END. /* display */
        WHEN "SHOW" THEN DO:
            cText[1] = "      Qty To       Cost Per    Setup $".
            IF AVAILABLE e-itemfg-vend THEN
            DO idx = 1 TO EXTENT(dRunQty):
                cText[idx + 1] = STRING(idx,"z9") + ". "
                               + STRING(e-itemfg-vend.run-qty[idx],">>,>>>,>>9.9<<") + " "
                               + STRING(e-itemfg-vend.run-cost[idx],">>,>>9.9999") + " "
                               + STRING(e-itemfg-vend.setups[idx],"->>,>>9.99")
                               .
            END. /* do idx */
            ELSE cText[3] = "No Vendor Cost Matrix Available".
            MESSAGE
                cText[1]  SKIP
                cText[2]  SKIP
                cText[3]  SKIP
                cText[4]  SKIP
                cText[5]  SKIP
                cText[6]  SKIP
                cText[7]  SKIP
                cText[8]  SKIP
                cText[9]  SKIP
                cText[10] SKIP
                cText[11]
            VIEW-AS ALERT-BOX TITLE "Vendor Cost Matrix".
        END. /* show */
    END CASE.
END. /* with frame */
