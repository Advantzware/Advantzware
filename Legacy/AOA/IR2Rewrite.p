/* IR2Rewrite.p */

DEFINE VARIABLE dtAsOf    AS DATE    NO-UNDO INITIAL 10/31/2018.
DEFINE VARIABLE dCost     AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostMat  AS DECIMAL NO-UNDO.
DEFINE VARIABLE dtReceipt AS DATE    NO-UNDO.
DEFINE VARIABLE dTotCost  AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTotQty   AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin 
    FIELD rita-code    LIKE fg-rcpth.rita-code
    FIELD itemFGCustNo LIKE itemfg.cust-no
    FIELD itemFGINo    LIKE itemfg.i-no
    FIELD itemFGPartNO LIKE itemfg.part-no
    FIELD itemfgIName  LIKE itemfg.i-name
    FIELD uomMult      LIKE uom.mult
        INDEX tt-fg-bin IS PRIMARY
            company
            i-no
            .
DEFINE STREAM sExcel.

FUNCTION fReceiptDate RETURN DATE (ipcTag AS CHARACTER):
    DEFINE BUFFER bttFGBin FOR tt-fg-bin.
    
    FIND FIRST bttFGBin
         WHERE bttFGBin.tag EQ ipcTag
           AND bttFGBin.rita-code EQ "R"
         NO-ERROR.
    RETURN IF AVAILABLE bttFGBin THEN bttFGBin.aging-date ELSE ?.
END FUNCTION.

OUTPUT STREAM sExcel to "c:\tmp\IR2Final.csv".
PUT STREAM sExcel UNFORMATTED
    ",,,Receipt,,,,,,Quantity,Cost,,Total" SKIP
    "Customer,Item #,Tag #,Date,Days,Cust Part #,Description,"
    "Whse,Bin,Job #,On Hand,UOM,UOM Cost,Cost" SKIP
    .
OUTPUT stream sExcel CLOSE.

FOR EACH itemfg NO-LOCK
    WHERE itemfg.company EQ "001"
      /*
      AND itemfg.i-no    GE ""
      AND itemfg.i-no    LE "zzzzzzzzzzzzzzzzzzzzzz"
      AND itemfg.cust-no GE ""
      AND itemfg.cust-no LE "zzzzzzzzzz"
      AND itemfg.procat  GE ""
      AND itemfg.procat  LE "ZZZZZZZZZZZ"
      */
    USE-INDEX i-no
    BREAK BY itemfg.i-no
    :
    IF FIRST-OF(itemfg.i-no) THEN DO:
        DISPLAY "First" itemfg.i-no WITH STREAM-IO DOWN.
        DOWN.
        PAUSE 0.
        EMPTY TEMP-TABLE tt-fg-bin.
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company    EQ itemfg.company
              AND fg-rcpth.i-no       EQ itemfg.i-no
              AND fg-rcpth.trans-date LE dtAsOf
              /*
              AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                  TRIM(fg-rcpth.job-no) + STRING(fg-rcpth.job-no2,"99"))
               GE ""
              AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                  TRIM(fg-rcpth.job-no) + STRING(fg-rcpth.job-no2,"99"))
               LE "ZZZZZZZZZZZZ"
              */
            USE-INDEX tran,
            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
              /*
              AND fg-rdtlh.loc       GE ""
              AND fg-rdtlh.loc       LE "ZZZZZZZZZZZ"
              AND fg-rdtlh.loc-bin   GE ""
              AND fg-rdtlh.loc-bin   LE "ZZZZZZZZZZZ"
              AND fg-rdtlh.cust-no   EQ ""
              */
               BY fg-rcpth.trans-date
               BY fg-rdtlh.trans-time
               BY fg-rcpth.r-no
            :
            FIND FIRST tt-fg-bin
                WHERE tt-fg-bin.company EQ fg-rcpth.company
                  AND tt-fg-bin.i-no    EQ fg-rcpth.i-no
                  AND tt-fg-bin.job-no  EQ fg-rcpth.job-no
                  AND tt-fg-bin.job-no2 EQ fg-rcpth.job-no2
                  AND tt-fg-bin.loc     EQ fg-rdtlh.loc
                  AND tt-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                  AND tt-fg-bin.tag     EQ fg-rdtlh.tag
                  AND tt-fg-bin.cust-no EQ fg-rdtlh.cust-no
                NO-ERROR.
            IF NOT AVAILABLE tt-fg-bin THEN DO:
                CREATE tt-fg-bin.
                ASSIGN
                    tt-fg-bin.company      = fg-rcpth.company
                    tt-fg-bin.job-no       = fg-rcpth.job-no
                    tt-fg-bin.job-no2      = fg-rcpth.job-no2
                    tt-fg-bin.loc          = fg-rdtlh.loc
                    tt-fg-bin.loc-bin      = fg-rdtlh.loc-bin
                    tt-fg-bin.tag          = fg-rdtlh.tag
                    tt-fg-bin.cust-no      = fg-rdtlh.cust-no
                    tt-fg-bin.i-no         = fg-rcpth.i-no
                    tt-fg-bin.po-no        = fg-rcpth.po-no
                    tt-fg-bin.aging-date   = fg-rcpth.trans-date
                    tt-fg-bin.pur-uom      = itemfg.prod-uom
                    tt-fg-bin.std-tot-cost = itemfg.std-tot-cost
                    tt-fg-bin.std-mat-cost = itemfg.std-mat-cost
                    tt-fg-bin.std-lab-cost = itemfg.std-lab-cost
                    tt-fg-bin.std-var-cost = itemfg.std-var-cost
                    tt-fg-bin.std-fix-cost = itemfg.std-fix-cost
                    tt-fg-bin.rita-code    = fg-rcpth.rita-code
                    tt-fg-bin.itemFGCustNo = itemfg.cust-no
                    tt-fg-bin.itemFGINo    = itemfg.i-no
                    tt-fg-bin.itemFGPartNo = itemfg.part-no
                    tt-fg-bin.itemFGIName  = itemfg.i-name
                    .
                FIND FIRST uom NO-LOCK
                     WHERE uom.uom  EQ itemfg.prod-uom
                       AND uom.mult NE 0
                     NO-ERROR.
                tt-fg-bin.uomMult = IF AVAILABLE uom THEN uom.mult ELSE ?.
                FIND FIRST fg-bin NO-LOCK
                     WHERE fg-bin.company EQ tt-fg-bin.company
                       AND fg-bin.i-no    EQ tt-fg-bin.i-no
                       AND fg-bin.job-no  EQ tt-fg-bin.job-no
                       AND fg-bin.job-no2 EQ tt-fg-bin.job-no2
                       AND fg-bin.loc     EQ tt-fg-bin.loc
                       AND fg-bin.loc-bin EQ tt-fg-bin.loc-bin
                       AND fg-bin.tag     EQ tt-fg-bin.tag
                       AND fg-bin.cust-no EQ tt-fg-bin.cust-no
                     NO-ERROR.
                IF AVAILABLE fg-bin THEN
                ASSIGN
                    tt-fg-bin.std-tot-cost = fg-bin.std-tot-cost
                    tt-fg-bin.std-mat-cost = fg-bin.std-mat-cost
                    tt-fg-bin.std-lab-cost = fg-bin.std-lab-cost
                    tt-fg-bin.std-var-cost = fg-bin.std-var-cost
                    tt-fg-bin.std-fix-cost = fg-bin.std-fix-cost
                    .
            END.  /*create new tt-fg-bin*/
            IF tt-fg-bin.case-count LE 0 AND fg-rdtlh.qty-case GT 0 THEN
            tt-fg-bin.case-count = fg-rdtlh.qty-case.
            IF tt-fg-bin.units-pallet LE 0 AND fg-rdtlh.units-pallet GT 0 THEN
            tt-fg-bin.units-pallet = fg-rdtlh.units-pallet.
            IF tt-fg-bin.cases-unit LE 0 AND fg-rdtlh.stacks-unit GT 0 THEN
            tt-fg-bin.cases-unit = fg-rdtlh.stacks-unit.
            CASE fg-rcpth.rita-code:
                WHEN "S" OR WHEN "s" THEN
                    tt-fg-bin.qty = tt-fg-bin.qty - fg-rdtlh.qty.
                WHEN "C" OR WHEN "c" THEN 
                    tt-fg-bin.qty = fg-rdtlh.qty.
                WHEN "R" OR WHEN "r" THEN DO:
                    ASSIGN 
                        tt-fg-bin.qty        = tt-fg-bin.qty + fg-rdtlh.qty
                        tt-fg-bin.aging-date = fg-rcpth.trans-date
                        .
                    IF tt-fg-bin.aging-date EQ ? OR
                       tt-fg-bin.aging-date GT fg-rcpth.trans-date THEN
                    tt-fg-bin.aging-date = fg-rcpth.trans-date.
                END.
                OTHERWISE 
                    tt-fg-bin.qty = tt-fg-bin.qty + fg-rdtlh.qty.
            END CASE.
        END.  /*fg-rdtlh */
    END. /* if first-of */
    IF LAST-OF(itemfg.i-no) THEN DO:
        IF CAN-FIND(FIRST tt-fg-bin) EQ NO THEN NEXT.
        DISPLAY "Last" itemfg.i-no WITH STREAM-IO DOWN.
        DOWN.
        PAUSE 0.
        OUTPUT STREAM sExcel to "c:\tmp\IR2Final.csv" APPEND.
        FOR EACH tt-fg-bin NO-LOCK
            WHERE tt-fg-bin.company EQ "001"
              AND tt-fg-bin.qty     NE 0
            :
            ASSIGN
                dCost    = tt-fg-bin.std-tot-cost * tt-fg-bin.qty
                dCostMat = tt-fg-bin.std-mat-cost * tt-fg-bin.qty
                .
            IF tt-fg-bin.pur-uom    EQ "CS" AND
               tt-fg-bin.case-count NE 0 THEN
            dCost = dCost / tt-fg-bin.case-count.
            ELSE IF tt-fg-bin.pur-uom EQ "L" THEN
            ASSIGN
                dCostMat = dCostMat / tt-fg-bin.qty
                dCost    = dCostMat / tt-fg-bin.qty
                .
            ELSE
            dCost = IF tt-fg-bin.uomMult EQ ? THEN dCost / 1000
                    ELSE dCost / tt-fg-bin.uomMult.
            ASSIGN
                dtReceipt = fReceiptDate(tt-fg-bin.tag)
                dTotQty   = dTotQty + tt-fg-bin.qty
                dTotCost  = dTotCost + dCost
                .
            EXPORT STREAM sExcel DELIMITER ","
                tt-fg-bin.itemFGCustNo
                tt-fg-bin.itemFGINo
               (IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no THEN
                   SUBSTR(tt-fg-bin.tag,16,8) ELSE tt-fg-bin.tag)
                dtReceipt
                dtAsOf - dtReceipt
                tt-fg-bin.itemFGPartNo
                tt-fg-bin.itemFGIName
                tt-fg-bin.loc
                tt-fg-bin.loc-bin
                tt-fg-bin.job-no + "-" + STRING(tt-fg-bin.job-no2,"99")
                tt-fg-bin.qty
                tt-fg-bin.pur-uom
                tt-fg-bin.std-tot-cost
                dCost
                .
        END.
        OUTPUT stream sExcel CLOSE.
    END. /* if last-of */
END. /* each itemfg */

OUTPUT STREAM sExcel to "c:\tmp\IR2Final.csv" APPEND.
PUT STREAM sExcel UNFORMATTED
    ",,,,,,,,,Grand Total," dTotQty ",,," dTotCost
    SKIP.
OUTPUT stream sExcel CLOSE.
OS-COMMAND NO-WAIT start c:\tmp\IR2Final.csv.
