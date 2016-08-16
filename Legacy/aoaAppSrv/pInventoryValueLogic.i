    /* local variables */
    DEFINE VARIABLE tb_cust-whse-2 AS LOG.
    DEFINE VARIABLE lPrintSubtotals AS LOGICAL NO-UNDO.
    
    /* subject business logic */
    
    
   /* -fg/rep/fg-ibtag.p 9/91 cd */
    /* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG".                          */
    /* -------------------------------------------------------------------------- */
   
    DEFINE VARIABLE cDisplay         AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay    AS cha       NO-UNDO.
    DEFINE VARIABLE hField           AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField        AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue        AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue   AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList    AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName       AS cha       NO-UNDO.
    DEFINE VARIABLE str-tit4         AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5         AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line         AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE v-page           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lProcessRel      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lProcessLastSale AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-tot-fgsell     AS DECIMAL   EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-tot-ordsell    AS DECIMAL   EXTENT 4 NO-UNDO.

    DEFINE VARIABLE excelheader      AS CHARACTER NO-UNDO.
/*    DEFINE VARIABLE lCustList        AS LOG       INIT YES NO-UNDO. */

    cSelectedList = cTextListToSelect.
    /* cFieldListToSelect. */

    ASSIGN

        v-tot-qty     = 0
        v-tot-cst     = 0
        v-tot-ext     = 0
        v-tot-mat     = 0
        v-tot-lab     = 0
        v-tot-ordsell = 0
        v-tot-fgsell  = 0
        v-label1      = ""
        v-label2      = ""
        v-label3      = ""
        .

    ASSIGN
        v-tot-qty          = 0
        v-tot-cst          = 0 
        v-tot-ext          = 0
        v-tot-msf          = 0
        excel-header-var-1 = "" 
        excel-header-var-2 = ""
        excel-header-var-3 = "" 
        .

        
    /* For auto test only */
    ASSIGN 
        lshowCost                  = TRUE
        lPrintSetandComponentsOnly = TRUE
        lIncludeZeroBalance        = TRUE
        cSort                      = "PR"
        cItemType                  = "All"
      /*  lPrintSubtotals            = TRUE */ 
        .
    
    /* If 'only customer owned' is checked, no need to check 'include cust owned' */
    /*    IF tb_cust-whse-2 THEN                     */
    /*        lIncludeCustomerOwnerdWarehouse = TRUE.*/

    IF (LOOKUP("TOTAL COST", cSelectedList) > 0 
        OR LOOKUP("UOM COST", cSelectedList) > 0
        OR LOOKUP("LABOR COST", cSelectedList) > 0 
        OR LOOKUP("MAT COST", cSelectedList) > 0) AND NOT ll-secure THEN
        RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
        
        
    IF LOOKUP("REL QTY", cSelectedList) > 0 
        OR LOOKUP("REL PO#", cSelectedList) > 0 THEN
        lProcessRel = YES.

    IF LOOKUP("LAST SALE", cSelectedList) > 0 THEN
        lProcessLastSale = YES.

    IF lCustList THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-No  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN cStartCustNo = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-No NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN cEndCustNo = ttCustList.cust-no .
    END.
 
    DEFINE VARIABLE cslist AS cha NO-UNDO.

    EMPTY TEMP-TABLE tt-fg-bin.
    EMPTY TEMP-TABLE tt-itemfg
    
    . 
   
   cItemType = "A". 
   cStartCustNo = "ZOV100".  
   cEndCustNo = "ZOV100".   
message "ipccompany "ipccompany skip
  cStartCustNo skip   
  "end cust" cEndCustNo   skip 
  "lcustlist" lCustList skip  
  "start item " cStartItemNo 
  "end item"   cEndItemNo   skip   
  "citem type" cItemType skip   
  
  view-as alert-box.
    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany
        AND itemfg.cust-no GE cStartCustNo
        AND itemfg.cust-no LE cEndCustNo
        AND (IF lCustList THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ itemfg.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
        AND itemfg.i-no    GE cStartItemNo
        AND itemfg.i-no    LE cEndItemNo
        AND itemfg.procat  GE cStartProdCategory
        AND itemfg.procat  LE cEndProdCategory
        AND (itemfg.i-code EQ cItemType OR cItemType EQ "A")
        AND (itemfg.stat EQ "A" OR lIncludeInactiveItems)
        AND (NOT lPrintSetandComponentsOnly    OR
        itemfg.isaset OR
        CAN-FIND(FIRST fg-set
        WHERE fg-set.company EQ itemfg.company
        AND fg-set.part-no EQ itemfg.i-no))
        USE-INDEX customer
        :
 
        RUN fg/rep/tt-fgbin.p (BUFFER itemfg, dtAsOfDate, "", "zzzzzzzzzz",
            cStartLoc, cEndLoc, cStartLocBin, cEndLocBin,
            lIncludeZeroBalance, iShowQOHOlderThanDays, YES, lIncludeCustomerOwnerdWarehouse).
            
        
        
        FOR EACH tt-fg-bin
            WHERE tt-fg-bin.company EQ itemfg.company
            AND tt-fg-bin.i-no    EQ itemfg.i-no
            AND (lIncludeCustomerOwnerdWarehouse OR tb_cust-whse-2 OR
            (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST"))
            AND (NOT tb_cust-whse-2 OR
            (tt-fg-bin.cust-no NE "" OR tt-fg-bin.loc EQ "CUST"))
            USE-INDEX co-ino:

            IF tt-fg-bin.qty NE 0 OR lIncludeZeroBalance THEN 
            DO:
                CREATE tt-itemfg.
                BUFFER-COPY itemfg TO tt-itemfg
                    ASSIGN
                    tt-itemfg.row-id      = ROWID(itemfg) 
                    tt-itemfg.job-no      = tt-fg-bin.job-no
                    tt-itemfg.job-no2     = tt-fg-bin.job-no2
                    tt-itemfg.loc         = tt-fg-bin.loc
                    tt-itemfg.loc-bin     = tt-fg-bin.loc-bin
                    tt-itemfg.tag         = tt-fg-bin.tag
                    tt-itemfg.binCustNo = tt-fg-bin.cust-no
                    tt-itemfg.part-cust   = STRING(tt-itemfg.part-no,"x(20)") +
                                   STRING(tt-itemfg.cust-no,"x(20)") 
                    tt-itemfg.loc-bin-Tag = STRING(tt-itemfg.loc,"x(10)")         +
                                   STRING(tt-itemfg.loc-bin,"x(10)")     +
                                   STRING(tt-itemfg.tag,"x(20)")
                    . 
                .
            END.

            ELSE DELETE tt-fg-bin.
        END.
    END.

    cCustName = "" .

    FOR EACH tt-itemfg USE-INDEX cust-No NO-LOCK,    
        FIRST itemfg WHERE ROWID(itemfg) EQ tt-itemfg.row-id NO-LOCK,
        EACH tt-fg-bin
        WHERE tt-fg-bin.company EQ itemfg.company
        AND tt-fg-bin.i-no    EQ itemfg.i-no
        AND tt-fg-bin.job-no  EQ tt-itemfg.job-no
        AND tt-fg-bin.job-no2 EQ tt-itemfg.job-no2
        AND tt-fg-bin.loc     EQ tt-itemfg.loc
        AND tt-fg-bin.loc-bin EQ tt-itemfg.loc-bin
        AND tt-fg-bin.tag     EQ tt-itemfg.tag
        AND tt-fg-bin.cust-no EQ tt-itemfg.binCustNo
        AND ((cStartsalesRep EQ "" AND cEndSalesRep BEGINS "zzz") OR 
        CAN-FIND(FIRST cust 
        WHERE cust.company EQ itemfg.company
        AND cust.sman GE cStartsalesRep  
        AND cust.sman LE cEndSalesRep 
        AND cust.cust-no EQ itemfg.cust-no)) 
        USE-INDEX co-ino NO-LOCK

        BREAK BY tt-itemfg.cust-no
        BY tt-itemfg.i-no
        BY tt-itemfg.loc
        BY tt-itemfg.loc-bin
        BY tt-itemfg.job-no
        BY tt-itemfg.job-no2
        :

        FIND FIRST cust
            WHERE cust.company EQ itemfg.company
            AND cust.cust-no EQ itemfg.cust-no
            NO-LOCK NO-ERROR.
            
   
        v-sales-rep = "" .

        IF AVAILABLE CUST THEN ASSIGN cCustName = cust.NAME .
        ELSE ASSIGN cCustName = "" .

        IF AVAILABLE cust AND cust.ACTIVE NE "X" THEN 
        DO:

            FOR EACH cust-part WHERE cust-part.company = itemfg.company   
                AND cust-part.i-no = itemfg.i-no
                AND cust-part.cust-no EQ cust.cust-no
                NO-LOCK, 
                FIRST reftable WHERE reftable.reftable = "cp-lab-p" 
                AND reftable.company = cust-part.company  
                AND reftable.loc = cust-part.i-no AND reftable.code = cust-part.cust-no NO-LOCK:
         
                IF cust-part.spare-char-1 NE "" THEN 
                DO:
                    FIND FIRST sman WHERE sman.company = itemfg.company
                        AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
                    IF AVAILABLE sman THEN v-sales-rep = sman.sman.
                    LEAVE .
                END.
            END. /* end of cust-part */
         
            IF AVAILABLE cust AND v-sales-rep EQ "" THEN 
            DO:
                FIND FIRST sman WHERE sman.company = cust.company
                    AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                IF AVAILABLE sman THEN v-sales-rep = sman.sman.
            END.
        END.
        ELSE 
        DO:
            FIND FIRST sman WHERE sman.company = cust.company
                AND sman.sman = cust.sman NO-LOCK NO-ERROR.
            IF AVAILABLE sman THEN v-sales-rep = sman.sman.
        END.

        FIND FIRST fg-rdtlh
            WHERE fg-rdtlh.company EQ ipcCompany
            AND fg-rdtlh.i-no    EQ tt-fg-bin.i-no
            AND fg-rdtlh.job-no  EQ tt-fg-bin.job-no
            AND fg-rdtlh.job-no2 EQ INT(tt-fg-bin.job-no2)
            AND fg-rdtlh.loc     EQ tt-fg-bin.loc
            AND fg-rdtlh.loc-bin EQ tt-fg-bin.loc-bin
            AND fg-rdtlh.tag     EQ tt-fg-bin.tag 
            AND fg-rdtlh.stack-code <> "" NO-LOCK NO-ERROR.


        IF NOT AVAILABLE fg-rdtlh THEN
            FIND FIRST fg-rdtlh
                WHERE fg-rdtlh.company EQ ipcCompany
                AND fg-rdtlh.i-no    EQ tt-fg-bin.i-no
                AND fg-rdtlh.job-no  EQ tt-fg-bin.job-no
                AND fg-rdtlh.job-no2 EQ INT(tt-fg-bin.job-no2)
                AND fg-rdtlh.loc     EQ tt-fg-bin.loc
                AND fg-rdtlh.loc-bin EQ tt-fg-bin.loc-bin
                AND fg-rdtlh.stack-code <> "" NO-LOCK NO-ERROR.

        
        ASSIGN
            fgLotVal = IF AVAILABLE fg-rdtlh THEN fg-rdtlh.stack-code ELSE ""
            .
        
        IF FIRST-OF(tt-itemfg.cust-no) THEN v-first[2] = YES.
     
        IF FIRST-OF(tt-itemfg.i-no) THEN 
        DO:
            ASSIGN
                v-first[1] = YES
                v-tot-sum  = 0
                v-ext-sum  = 0
                v-qoh      = 0.

            IF cSort EQ "Wh" THEN v-first[2] = YES.
        END.

        ASSIGN
            v-procat = itemfg.procat
            v-bin    = NO.

        IF lPrintSummaryByBinQty AND FIRST-OF(tt-itemfg.job-no2) THEN 
        DO:
            ASSIGN 
                v-tot-bin-sum = 0
                v-ext-bin-sum = 0
                v-bin-qoh     = 0
                v-bin-msf     = 0
                v-bin-arq     = 0
                v-ordPrice    = 0.
        END.  

        lv-rct-date = tt-fg-bin.first-date.

        ASSIGN
            v-costl = tt-fg-bin.std-lab-Cost * tt-fg-bin.qty
            v-costm = tt-fg-bin.std-mat-Cost * tt-fg-bin.qty 
            v-cost1 = tt-fg-bin.std-tot-cost
            v-cost  = v-cost1 * tt-fg-bin.qty.

        /* Calculate Cost */
        IF tt-fg-bin.pur-uom EQ "CS" AND tt-fg-bin.case-count NE 0 THEN
            ASSIGN
                v-costl = v-costl / tt-fg-bin.case-count
                v-costm = v-costm / tt-fg-bin.case-count
                v-cost  = v-cost  / tt-fg-bin.case-count.
        ELSE
            IF tt-fg-bin.pur-uom EQ "L" THEN
                ASSIGN
                    v-costl = v-costl / tt-fg-bin.qty
                    v-costm = v-costm / tt-fg-bin.qty
                    v-cost  = v-costm / tt-fg-bin.qty.
            ELSE 
            DO:
                FIND FIRST uom
                    WHERE uom.uom  EQ itemfg.prod-uom
                    AND uom.mult NE 0
                    NO-LOCK NO-ERROR.
                IF AVAILABLE uom THEN
                    ASSIGN
                        v-costl = v-costl / uom.mult
                        v-costm = v-costm / uom.mult
                        v-cost  = v-cost  / uom.mult.
                ELSE
                    ASSIGN
                        v-costl = v-costl / 1000
                        v-costm = v-costm / 1000
                        v-cost  = v-cost  / 1000.
            END.

        ASSIGN
            lv-sellPrice-fg  = itemfg.sell-Price
            lv-sellPrice     = itemfg.sell-Price
            lv-sell-uom      = itemfg.sell-uom
            lv-sell-uom-fg   = itemfg.sell-uom
            lv-case-count    = itemfg.case-count
            lv-sellPrice-ord = 0
            lv-sellValueOrd  = 0
            lv-sellValueFg   = 0
            .

        IF tt-fg-bin.po-no NE "" THEN
        DO:
            FIND FIRST po-ordl WHERE
                po-ordl.company EQ tt-fg-bin.company AND
                po-ordl.po-no EQ INT(tt-fg-bin.po-no) AND
                po-ordl.i-no EQ tt-fg-bin.i-no
                NO-LOCK NO-ERROR.

            IF AVAILABLE po-ordl THEN
            DO:
                FIND LAST oe-ordl WHERE
                    oe-ordl.company EQ tt-fg-bin.company AND
                    oe-ordl.ord-no EQ po-ordl.ord-no AND
                    oe-ordl.i-no EQ tt-fg-bin.i-no AND
                    (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                    NO-LOCK NO-ERROR.

                IF AVAILABLE oe-ordl THEN
                    ASSIGN
                        lv-sellPrice-ord = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                        lv-sell-uom-ord  = oe-ordl.pr-uom
                        lv-case-count    = oe-ordl.cas-cnt.
            END.
        END.

        ELSE IF TRIM(tt-fg-bin.job-no) NE "" THEN
            DO:
                v-found-job = NO.

                FOR EACH job-hdr FIELDS(ord-no company i-No)
                    WHERE job-hdr.company EQ tt-fg-bin.company
                    AND job-hdr.job-no  EQ tt-fg-bin.job-no
                    AND job-hdr.job-no2 EQ tt-fg-bin.job-no2
                    AND job-hdr.i-no    EQ tt-fg-bin.i-no
                    AND job-hdr.ord-no  NE 0
                    USE-INDEX job-No NO-LOCK,
                    FIRST oe-ordl FIELDS(price pr-uom cas-cnt disc)
                    WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                    AND (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                    USE-INDEX item-ord NO-LOCK
                    BY job-hdr.ord-no DESCENDING:

                    ASSIGN
                        lv-sellPrice-ord = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                        lv-sell-uom-ord  = oe-ordl.pr-uom
                        lv-case-count    = oe-ordl.cas-cnt
                        v-found-job      = YES.
                    LEAVE.
                END.

                IF v-found-job = NO THEN
                DO:
                    FIND LAST oe-ordl WHERE
                        oe-ordl.company EQ tt-fg-bin.company AND
                        oe-ordl.job-no EQ tt-fg-bin.job-no AND
                        oe-ordl.job-no2 EQ tt-fg-bin.job-no2 AND
                        oe-ordl.i-no EQ tt-fg-bin.i-no AND
                        (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE oe-ordl THEN
                        ASSIGN
                            lv-sellPrice-ord = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                            lv-sell-uom-ord  = oe-ordl.pr-uom
                            lv-case-count    = oe-ordl.cas-cnt.
                END.
            END.
      
        /* Calculate Selling Price - FG */
        IF lv-sell-uom-fg EQ "CS" AND lv-case-count NE 0 THEN
            ASSIGN 
                v-ext          = (tt-fg-bin.qty * lv-sellPrice) / lv-case-count
                lv-sellValueFg = (tt-fg-bin.qty * lv-sellPrice-fg) / lv-case-count
                .
        ELSE 
        DO:
            FIND FIRST uom
                WHERE uom.uom  EQ lv-sell-uom-fg
                AND uom.mult NE 0
                NO-LOCK NO-ERROR.
            v-ext = tt-fg-bin.qty * lv-sellPrice /
                (IF AVAILABLE uom THEN uom.mult ELSE 1000).
            lv-sellValueFg = tt-fg-bin.qty * lv-sellPrice-fg /
                (IF AVAILABLE uom THEN uom.mult ELSE 1000).
        END.

        IF lv-sell-uom-fg EQ "L" THEN
            IF tt-fg-bin.qty LE 0 THEN 
                ASSIGN 
                    v-ext          = 0
                    lv-sellValueFg = 0
                    .
            ELSE 
                ASSIGN 
                    v-ext          = lv-sellPrice
                    lv-sellValueFg = lv-sellPrice-fg   
                    .
            
        /* Calculate Selling Price - Order */
        IF lv-sell-uom-ord EQ "CS" AND lv-case-count NE 0 THEN
            ASSIGN 
                lv-sellValueOrd = (tt-fg-bin.qty * lv-sellPrice-ord) / lv-case-count
                .
        ELSE 
        DO:
            FIND FIRST uom
                WHERE uom.uom  EQ lv-sell-uom-ord
                AND uom.mult NE 0
                NO-LOCK NO-ERROR.
            lv-sellValueOrd = tt-fg-bin.qty * lv-sellPrice-ord /
                (IF AVAILABLE uom THEN uom.mult ELSE 1000).
        END.
        
        IF lv-sell-uom-ord EQ "L" THEN
            IF tt-fg-bin.qty LE 0 THEN 
                lv-sellValueOrd = 0.
            ELSE 
                lv-sellValueOrd = lv-sellPrice-ord.
        


        ASSIGN
            v-ext             = ROUND(v-ext,2)  
            lv-sellValueOrd   = ROUND(lv-sellValueOrd,2)
            lv-sellValueFg    = ROUND(lv-sellValueFg,2)
            lv-sellValueFg-s  = lv-sellValueFg-s + lv-sellValueFg
            lv-sellValueOrd-s = lv-sellValueOrd-s + lv-sellValueOrd
            .
            
        IF v-costl EQ ? THEN v-costl = 0.
        IF v-costm EQ ? THEN v-costm = 0.
        IF v-cost  EQ ? THEN v-cost  = 0.
        IF v-ext   EQ ? THEN v-ext   = 0.

        ASSIGN
            v-qoh            = tt-fg-bin.qty
            v-tot-sum        = v-cost
            v-ext-sum        = v-ext
            v-msf-oh         = 0            
            v-msf-oh         = v-qoh * itemfg.t-sqft / 1000
            v-bin-qoh        = v-bin-qoh + v-qoh
            v-bin-msf        = v-bin-msf + v-msf-oh 
            v-tot-bin-sum    = v-tot-bin-sum + v-cost1
            v-ext-bin-sum    = v-ext-bin-sum + v-cost    
            v-tot-qty[1]     = v-tot-qty[1] + v-qoh
            v-tot-msf[1]     = v-tot-msf[1] + v-msf-oh
            v-tot-cst[1]     = v-tot-cst[1] + v-tot-sum
            v-tot-ext[1]     = v-tot-ext[1] + v-ext-sum  
            v-tot-mat[1]     = v-tot-mat[1] + v-costm
            v-tot-lab[1]     = v-tot-lab[1] + v-costl 
            v-tot-fgsell[1]  = v-tot-fgsell[1] + lv-sellValueFg
            v-tot-ordsell[1] = v-tot-ordsell[1] + lv-sellValueOrd
            .

        IF lIncludeZeroBalance OR v-qoh NE 0 THEN 
        DO:
            IF tt-fg-bin.job-no NE "" THEN
                jobNo = TRIM(tt-fg-bin.job-no) + "-" + string(tt-fg-bin.job-no2,"99").
            ELSE
                jobNo = "".

        
            FIND FIRST job-hdr
                WHERE job-hdr.company EQ ipcCompany
                AND job-hdr.job-no  EQ tt-fg-bin.job-no
                AND job-hdr.job-no2 EQ tt-fg-bin.job-no2
                AND job-hdr.i-no    EQ tt-fg-bin.i-no
                USE-INDEX job-No NO-LOCK NO-ERROR.
            IF AVAILABLE job-hdr THEN 
            DO:
            
                FIND FIRST oe-ordl
                    WHERE oe-ordl.company EQ ipcCompany
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE oe-ordl THEN 
                    ASSIGN  
                        v-po-no    = oe-ordl.po-no
                        v-ordPrice = oe-ordl.price .
            
                FIND FIRST oe-ord
                    WHERE oe-ord.company EQ ipcCompany
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE oe-ord THEN v-po-ord = oe-ord.po-no.
            
            END.
         
            ELSE ASSIGN
                    v-po-no  = ""
                    v-po-ord = "".
                    
                    /* Test code !!! */ 
                    v-qoh-f = ">>>,>>9.99".


            ASSIGN
                v-qoh-s = STRING(v-qoh,v-qoh-f)
                v-arq   = 0.

            IF lPrintSummaryByBinQty THEN
                ASSIGN v-qoh-s = STRING(v-bin-qoh,v-qoh-f).

            ASSIGN 
                v-last-inv = "".
            IF lProcessLastSale THEN
                FOR EACH ar-invl WHERE ar-invl.i-no EQ itemfg.i-no NO-LOCK, 
                    EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no NO-LOCK BY ar-inv.inv-date DESCENDING:

                    ASSIGN 
                        v-last-inv = STRING(ar-inv.inv-date) .
                    LEAVE.
                END.

            ASSIGN 
                v-po-rel = "" . 
            IF lProcessRel THEN
                FOR EACH oe-relh FIELDS(company r-no)
                    WHERE oe-relh.company EQ tt-fg-bin.company
                    AND oe-relh.posted  EQ NO
                    USE-INDEX post NO-LOCK,
                    EACH oe-rell FIELDS(qty po-no)
                    WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND oe-rell.i-no    EQ tt-fg-bin.i-no
                    AND oe-rell.loc     EQ tt-fg-bin.loc
                    AND oe-rell.loc-bin EQ tt-fg-bin.loc-bin
                    AND oe-rell.tag     EQ tt-fg-bin.tag
                    AND oe-rell.cust-no EQ tt-fg-bin.cust-no
                    USE-INDEX r-no NO-LOCK:
                    v-arq = v-arq + oe-rell.qty.
                    v-po-rel = oe-rell.po-no .
                END.

            FIND FIRST fg-set
                WHERE fg-set.company EQ itemfg.company
                AND fg-set.part-no EQ itemfg.i-no NO-LOCK NO-ERROR .

            ASSIGN 
                v-bin-arq = v-bin-arq + v-arq.
        /*
            IF  lPrintSummaryByBinQty AND  LAST-OF(tt-itemfg.job-no2) THEN 
            DO:
                
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".
                
                BUFFER b-itemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
                DO iCnt = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldListToSelect).

                    IF INDEX(cTmpField,".") > 0 THEN 
                    DO:
                        cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                        IF cTmpField = "custNo" OR
                            cTmpField = "Sman" THEN hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
                        ELSE hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
              
                        cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldLength))).
                        cDisplay = cDisplay + cTmpField + 
                            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

                        cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".      
                    END.
                    ELSE 
                    DO:            
                        CASE cTmpField:               
                            WHEN "tag" THEN 
                                cVarValue = tt-fg-bin.tag /*(SUBSTR(tt-fg-bin.tag,16,8))*/ .
                            WHEN "TAG #" THEN 
                                cVarValue = (SUBSTR(tt-fg-bin.tag,16,6)) .
                            WHEN "fgLotVal" THEN 
                                cvarValue = STRING(fgLotVal,"x(20)") .
                            WHEN "jobNo" THEN 
                                cVarValue = STRING(jobNo) .
                            WHEN "recDate" THEN 
                                cVarValue = IF lv-rct-date NE ? THEN STRING(lv-rct-date) ELSE "" .
                            WHEN "daysOld" THEN 
                                cVarValue = STRING(INT(dtAsOfDate - lv-rct-date),"->>>>>>9") .
                            WHEN "loc" THEN 
                                cVarValue = STRING(tt-fg-bin.loc).
                            WHEN "bin" THEN 
                                cVarValue =     STRING(tt-fg-bin.loc-bin).
                            WHEN "msfOnHand" THEN 
                                cVarValue = STRING(v-bin-msf,"->>9.999").
                            WHEN "costUom" THEN 
                                cVarValue = STRING(tt-fg-bin.pur-uom,"x(5)").
                            WHEN "relQty" THEN 
                                cVarValue = STRING(v-bin-arq,"->>,>>>,>>9") .
                            WHEN "qtyOnHand" THEN 
                                cVarValue = STRING(v-bin-qoh,"->>,>>>,>>9") .
                            WHEN "lastSale" THEN 
                                cVarValue = STRING(v-last-inv,"x(10)") .
                            WHEN "viewPo" THEN 
                                cVarValue = STRING(v-po-ord,"x(10)") .
                            WHEN "linePo" THEN 
                                cVarValue = STRING(v-po-no,"x(10)") .
                            WHEN "relPo" THEN 
                                cVarValue = STRING(v-po-rel,"x(11)") .
                            WHEN "ordPr" THEN 
                                cVarValue = STRING(lv-sellPrice-ord,"->>>,>>9.99").
                            WHEN "sellPrice" THEN 
                                cVarValue = STRING(itemfg.sell-Price,"->>>,>>9.99").
                            WHEN "uomCost" THEN 
                                cVarValue = /*(IF ll-secure THEN STRING(v-tot-bin-sum,"->>>>>9.999") ELSE*/ "" . /*Task# 01271402 */
                            WHEN "totCost" THEN 
                                cVarValue = (IF ll-secure THEN STRING(v-ext-bin-sum,"->>>,>>9.99") ELSE "").
                            WHEN "labCost" THEN 
                                cVarValue = (IF ll-secure THEN STRING(v-costl,"->>>,>>9.99") ELSE "") .
                            WHEN "matCost" THEN 
                                cVarValue = (IF ll-secure THEN STRING(v-costm,"->>>,>>9.99") ELSE "") .
                            WHEN "salesRep" THEN 
                                cVarValue = STRING(v-sales-rep,"x(3)").
                            WHEN "sellValueOrd" THEN 
                                cVarValue = STRING(lv-sellValueOrd-s,"->>,>>>,>>9.99") .    
                            WHEN "sellValueFg" THEN 
                                cVarValue = STRING(lv-sellValueFg-s,"->>,>>>,>>9.99") .           /*Task# 01101401*/
                            WHEN "custno" THEN 
                                cVarValue = STRING(tt-fg-bin.cust-no,"x(8)") .   
                            WHEN "setHeader" THEN 
                                cVarValue = IF AVAILABLE fg-set AND jobNo <> "" THEN STRING(fg-set.set-no,"X(15)") ELSE "" .
                            WHEN "qtyPerSet" THEN 
                                cVarValue = IF AVAILABLE fg-set AND jobNo <> "" THEN STRING(fg-set.part-qty,"->>,>>>,>>9") ELSE "" .
                            WHEN "custName" THEN 
                                cVarValue = STRING(cCustName,"X(30)") .
                
                        END CASE.
                        cExcelVarValue = cVarValue.  
                        cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
                    END.
                END.
                PUT UNFORMATTED cDisplay SKIP.
    
                ASSIGN 
                    lv-sellValueFg-s  = 0
                    lv-sellValueOrd-s = 0.
            END. /* summary by bin */

            ELSE IF NOT lPrintSummaryByBinQty THEN */ 
                DO:
                    /* Lowest level detail display */
                    ASSIGN 
                        cDisplay       = ""
                        cTmpField      = ""
                        cVarValue      = ""
                        cExcelDisplay  = ""
                        cExcelVarValue = "".
                        
                    BUFFER b-itemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
                    
                    
                    CREATE ttInventoryValue.
                   
                    DO iCnt = 1 TO NUM-ENTRIES(cSelectedlist):    
                        
                                                 
                        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldListToSelect).
                  
                        IF INDEX(cTmpField,".") > 0 THEN 
                        DO:
 
                            /* Calculate field value specially */
                            cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                            IF cTmpField = "custNo" OR
                                cTmpField = "Sman" THEN hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
                            ELSE hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
              
                            cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldLength))).
                            cDisplay = cDisplay + cTmpField + 
                                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

                                  
                        END. /* Calculate field value specially */
                        ELSE 
                        DO:            

                            /* Calculate field value normally */
                            CASE cTmpField:               
                                WHEN "tag" THEN 
                                    cVarValue = tt-fg-bin.tag.
                                WHEN "TAG #" THEN 
                                    cVarValue = IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no THEN (SUBSTR(tt-fg-bin.tag,16,5)) ELSE  "" .
                                WHEN "fgLotVal" THEN 
                                    cvarValue = STRING(fgLotVal,"x(20)") .
                                WHEN "jobNo" THEN 
                                    cVarValue = STRING(jobNo) .
                                WHEN "recDate" THEN 
                                    cVarValue = IF lv-rct-date NE ? THEN STRING(lv-rct-date) ELSE "" .
                                WHEN "daysOld" THEN 
                                    cVarValue = STRING(INT(dtAsOfDate - lv-rct-date)) .
                                WHEN "loc" THEN 
                                    cVarValue = STRING(tt-fg-bin.loc).
                                WHEN "bin" THEN 
                                    cVarValue =     STRING(tt-fg-bin.loc-bin).
                                WHEN "msfOnHand" THEN 
                                    cVarValue = STRING(v-msf-oh,"->>9.999").
                                WHEN "costUom" THEN 
                                    cVarValue = STRING(tt-fg-bin.pur-uom,"x(5)").
                                WHEN "relQty" THEN 
                                    cVarValue = STRING(v-bin-arq,"->>,>>>,>>9") .
                                WHEN "qtyOnHand" THEN 
                                    cVarValue = STRING(tt-fg-bin.qty,"->>,>>>,>>9") .
                                WHEN "lastSale" THEN 
                                    cVarValue = STRING(v-last-inv,"x(10)") .
                                WHEN "viewPo" THEN 
                                    cVarValue = STRING(v-po-ord,"x(10)") .
                                WHEN "linePo" THEN 
                                    cVarValue = STRING(v-po-no,"x(10)") .
                                WHEN "relPo" THEN 
                                    cVarValue = STRING(v-po-rel,"x(11)") .
                                WHEN "ordPr" THEN 
                                    cVarValue = STRING(lv-sellPrice-ord,"->>>,>>9.99").
                                WHEN "sellPrice" THEN 
                                    cVarValue = STRING(itemfg.sell-Price,"->>>,>>9.99").
                                WHEN "uomCost" THEN 
                                    cVarValue = (IF ll-secure THEN STRING(v-cost1,"->>>>>9.999") ELSE "") . /*Task# 01271402 */
                                WHEN "totCost" THEN 
                                    cVarValue = (IF ll-secure THEN STRING(v-cost,"->>>,>>9.99") ELSE "").
                                WHEN "labCost" THEN 
                                    cVarValue = (IF ll-secure THEN STRING(v-costl,"->>>,>>9.99") ELSE "") .
                                WHEN "matCost" THEN 
                                    cVarValue = (IF ll-secure THEN STRING(v-costm,"->>>,>>9.99") ELSE "") .
                                WHEN "salesRep" THEN 
                                    cVarValue = STRING(v-sales-rep,"x(3)").
                                WHEN "sellValueOrd" THEN 
                                    cVarValue = STRING(lv-sellValueOrd,"->>,>>>,>>9.99") .    
                                WHEN "sellValueFg" THEN 
                                    cVarValue = STRING(lv-sellValueFg,"->>,>>>,>>9.99") .
                                WHEN "custno" THEN 
                                    cVarValue = STRING(tt-fg-bin.cust-no,"x(8)") .
                                WHEN "setHeader" THEN 
                                    cVarValue = IF AVAILABLE fg-set AND jobNo <> "" THEN STRING(fg-set.set-no,"X(15)") ELSE "" .
                                WHEN "qtyPerSet" THEN 
                                    cVarValue = IF AVAILABLE fg-set AND jobNo <> "" THEN STRING(fg-set.part-qty,"->>,>>>,>>9") ELSE "" .
                                WHEN "custName" THEN 
                                    cVarValue = STRING(cCustName,"X(30)") .
                   
                            END CASE.
                            
                            cDisplay = cDisplay + cVarValue +
                                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).
                                
                       /* This line represents the equivalent of a display statement.  Temp table rec is passed to AOA */
                    
                        CASE cTmpField:
                            WHEN "custNo"  THEN      
                                ttInventoryValue.custNo     = cVarValue.
                            WHEN "custName"  THEN      
                                ttInventoryValue.custName   = cVarValue.
                            WHEN "iNo"       THEN      
                                ttInventoryValue.iNo        = cVarValue.
                            WHEN "TAG #"       THEN      
                                ttInventoryValue.tagNo        = cVarValue. 
                            WHEN "tag"        THEN      
                                ttInventoryValue.tag         = cVarValue.
                            WHEN "fgLotVal" THEN      
                                ttInventoryValue.fgLotVal  = cVarValue. 
                            WHEN "partNo"    THEN      
                                ttInventoryValue.partNo     = cVarValue.
                            WHEN "iName"     THEN      
                                ttInventoryValue.iName      = cVarValue.
                            WHEN "jobNo"   THEN      
                                ttInventoryValue.jobNo    = cVarValue. 
                            WHEN "recDate"    THEN      
                                ttInventoryValue.recDate     = cVarValue.
                            WHEN "loc"        THEN      
                                ttInventoryValue.loc         = cVarValue.
                            WHEN "bin"        THEN      
                                ttInventoryValue.bin         = cVarValue. 
                            WHEN "msfOnHand" THEN     
                                ttInventoryValue.msfOnHand = DECIMAL(cVarValue). 
                            WHEN "costUom"   THEN      
                                ttInventoryValue.costUom    = cVarValue. 
                            WHEN "relQty"    THEN      
                                ttInventoryValue.relQty     = INTEGER(cVarValue). 
                            WHEN "qtyOnHand" THEN     
                                ttInventoryValue.qtyOnHand = INTEGER(cVarValue). 
                            WHEN "lastSale"  THEN      
                                ttInventoryValue.lastSale   = cVarValue. 
                            WHEN "procat"     THEN      
                                ttInventoryValue.procat      = cVarValue. 
                            WHEN "viewPo"    THEN      
                                ttInventoryValue.viewPo     = cVarValue. 
                            WHEN "linePo"    THEN      
                                ttInventoryValue.linePo     = cVarValue. 
                            WHEN "relPo"     THEN      
                                ttInventoryValue.relPo      = cVarValue. 
                            WHEN "sellPrice" THEN      
                                ttInventoryValue.sellPrice  = DECIMAL(cVarValue). 
                            WHEN "ordPr"     THEN      
                                ttInventoryValue.ordPr      = DECIMAL(cVarValue). 
                            WHEN "uomCost"   THEN      
                                ttInventoryValue.uomCost    = DECIMAL(cVarValue).
                            WHEN "totCost" THEN      
                                ttInventoryValue.totCost  = DECIMAL(cVarValue). 
                            WHEN "matCost"   THEN      
                                ttInventoryValue.matCost    = DECIMAL(cVarValue). 
                            WHEN "labCost"   THEN      
                                ttInventoryValue.labCost    = DECIMAL(cVarValue). 
                            WHEN "salesRep"   THEN      
                                ttInventoryValue.salesRep    = cVarValue. 
                            WHEN "sellValueFg" THEN   
                                ttInventoryValue.sellValueFg = DECIMAL(cVarValue). 
                            WHEN "sellValueOrd" THEN  
                                ttInventoryValue.sellValueOrd = DECIMAL(cVarValue). 
                            WHEN "daysOld"   THEN      
                                ttInventoryValue.daysOld    = INTEGER(cVarValue). 
                            WHEN "custno"     THEN      
                                ttInventoryValue.custno      = cVarValue. 
                            WHEN "setHeader" THEN      
                                ttInventoryValue.setHeader  = cVarValue. 
                            WHEN "qtyPerSet" THEN     
                                ttInventoryValue.qtyPerSet = INTEGER(cVarValue).    
                        END CASE.
                                
                                             
                        END. /* Calculate field value from temp-table */
                        
                     END. /* i = 1 to selected field */
                    /* PUT UNFORMATTED cDisplay SKIP. */
    
                    ASSIGN 
                        lv-sellValueFg-s  = 0
                        lv-sellValueOrd-s = 0.

                END.  /*  not lPrintSummaryByBinQty */


            IF (lPrintSummaryByBinQty AND LAST-OF(tt-itemfg.job-no2)) OR NOT lPrintSummaryByBinQty THEN
                ASSIGN
                    v-prnt  = YES
                    v-first = NO
                    v-bin   = YES.

        END. /* lIncludeZeroBalance or on hand NE 0 */
      


        IF lIncludeZeroBalance AND NOT v-bin THEN 
        DO:
            v-qoh-s = STRING(0,v-qoh-f).

            v-arq = 0.
      
            FOR EACH oe-relh
                WHERE oe-relh.company EQ itemfg.company
                AND oe-relh.posted  EQ NO
                USE-INDEX post NO-LOCK,
                EACH oe-rell
                WHERE oe-rell.company EQ oe-relh.company
                AND oe-rell.r-no    EQ oe-relh.r-no
                AND oe-rell.i-no    EQ itemfg.i-no
                USE-INDEX r-no NO-LOCK:
                v-arq = v-arq + oe-rell.qty.
            END.             
      
            v-prnt = YES.
        END. /* if lIncludeZeroBalance and not v-bin */  

    
        IF LAST-OF(tt-itemfg.i-no) THEN 
        DO:
        
            IF "tt-itemfg.i-no" NE "1" AND cSort NE "PA" AND
                v-prnt[1] AND lPrintSubtotals                    THEN 
            DO:
        
       
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    
                    .
     
                DO iCnt = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldListToSelect).
       
                    CASE cTmpField:    
                        WHEN "itemfg.cust-no" THEN 
                            cVarValue = "" .
                        WHEN "itemfg.i-no" THEN 
                            cvarValue = "" .
                        WHEN "itemfg.part-no" THEN 
                            cVarValue = "" .
                        WHEN "itemfg.i-name" THEN 
                            cVarValue = "" .
                        WHEN "itemfg.procat" THEN 
                            cVarValue = "" .
                        WHEN "tag" THEN 
                            cVarValue = "" .
                        WHEN "TAG #" THEN 
                            cVarValue = "" .
                        WHEN "fgLotVal" THEN 
                            cvarValue = "" .
                        WHEN "jobNo" THEN 
                            cVarValue = "" .
                        WHEN "recDate" THEN 
                            cVarValue = "" .
                        WHEN "daysOld" THEN 
                            cVarValue = "" .
                        WHEN "loc" THEN 
                            cVarValue = "".
                        WHEN "bin" THEN 
                            cVarValue =    "".
                        WHEN "msfOnHand" THEN 
                            cVarValue = STRING(v-tot-msf[1],"->>9.999").
                        WHEN "costUom" THEN 
                            cVarValue = "".
                        WHEN "relQty" THEN 
                            cVarValue = "" .
                        WHEN "qtyOnHand" THEN 
                            cVarValue = STRING(v-tot-qty[1],"->>,>>>,>>9") .
                        WHEN "lastSale" THEN 
                            cVarValue = "" .
                        WHEN "viewPo" THEN 
                            cVarValue = "" .
                        WHEN "linePo" THEN 
                            cVarValue = "" .
                        WHEN "relPo" THEN 
                            cVarValue = "" .
                        WHEN "ordPr" THEN 
                            cVarValue = "".
                        WHEN "sellPrice" THEN 
                            cVarValue = "".
                        WHEN "uomCost" THEN 
                            cVarValue = "" .
                        WHEN "totCost" THEN 
                            cVarValue = (IF ll-secure THEN STRING(v-tot-cst[1],"->>>,>>9.99") ELSE "").
                        WHEN "labCost" THEN 
                            cVarValue = (IF ll-secure THEN STRING(v-tot-lab[1],"->>>,>>9.99") ELSE "") .
                        WHEN "matCost" THEN 
                            cVarValue = (IF ll-secure THEN STRING(v-tot-mat[1],"->>>,>>9.99") ELSE "") .
                        WHEN "salesRep" THEN 
                            cVarValue = "" .           /*Task# 01101401*/
                        WHEN "sellValueOrd" THEN 
                            cVarValue = STRING(lv-sellValueOrd,"->>,>>>,>>9.99") .    
                        WHEN "sellValueFg" THEN 
                            cVarValue = STRING(lv-sellValueFg,"->>,>>>,>>9.99") .
                        WHEN "custno" THEN 
                            cVarValue = "" .
                        WHEN "setHeader" THEN 
                            cVarValue = "" .
                        WHEN "qtyPerSet" THEN 
                            cVarValue = "" .
                        WHEN "custName" THEN 
                            cVarValue = "" .
                
                    END CASE.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                END.
 
        
            END.  /*  v-prnt[1] and lPrintSubtotals*/                  


            ASSIGN
                v-tot-qty[2]     = v-tot-qty[2] + v-tot-qty[1]
                v-tot-cst[2]     = v-tot-cst[2] + v-tot-cst[1]
                v-tot-ext[2]     = v-tot-ext[2] + v-tot-ext[1]
                v-tot-msf[2]     = v-tot-msf[2] + v-tot-msf[1]
                v-tot-mat[2]     = v-tot-mat[2] + v-tot-mat[1]
                v-tot-lab[2]     = v-tot-lab[2] + v-tot-lab[1]
                v-tot-fgsell[2]  = v-tot-fgsell[2] + v-tot-fgsell[1]
                v-tot-ordsell[2] = v-tot-ordsell[2] + v-tot-ordsell[1]

                v-tot-qty[1]     = 0
                v-tot-cst[1]     = 0
                v-tot-ext[1]     = 0
                v-tot-msf[1]     = 0
                v-tot-mat[1]     = 0
                v-tot-lab[1]     = 0
                v-tot-ordsell[1] = 0
                v-tot-fgsell[1]  = 0
                v-prnt[1]        = NO.
    
        END.

        IF LAST-OF(tt-itemfg.cust-no) THEN 
        DO: 
        
            IF v-prnt[2] AND lPrintSubtotals THEN 
            DO:
        
                
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    
                    .
     
                DO iCnt = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldListToSelect).
       
                    CASE cTmpField:    
                        WHEN "itemfg.cust-no" THEN 
                            cVarValue = "" .
                        WHEN "itemfg.i-no" THEN 
                            cvarValue = "" .
                        WHEN "itemfg.part-no" THEN 
                            cVarValue = "" .
                        WHEN "itemfg.i-name" THEN 
                            cVarValue = "" .
                        WHEN "itemfg.procat" THEN 
                            cVarValue = "" .
                        WHEN "tag" THEN 
                            cVarValue = "" .
                        WHEN "TAG #" THEN 
                            cVarValue = "" .
                        WHEN "fgLotVal" THEN 
                            cvarValue = "" .
                        WHEN "jobNo" THEN 
                            cVarValue = "" .
                        WHEN "recDate" THEN 
                            cVarValue = "" .
                        WHEN "daysOld" THEN 
                            cVarValue = "" .
                        WHEN "loc" THEN 
                            cVarValue = "".
                        WHEN "bin" THEN 
                            cVarValue =    "".
                        WHEN "msfOnHand" THEN 
                            cVarValue = STRING(v-tot-msf[2],"->>9.999").
                        WHEN "costUom" THEN 
                            cVarValue = "".
                        WHEN "relQty" THEN 
                            cVarValue = "" .
                        WHEN "qtyOnHand" THEN 
                            cVarValue = STRING(v-tot-qty[2],"->>,>>>,>>9") .
                        WHEN "lastSale" THEN 
                            cVarValue = "" .
                        WHEN "viewPo" THEN 
                            cVarValue = "" .
                        WHEN "linePo" THEN 
                            cVarValue = "" .
                        WHEN "relPo" THEN 
                            cVarValue = "" .
                        WHEN "ordPr" THEN 
                            cVarValue = "".
                        WHEN "sellPrice" THEN 
                            cVarValue = "".
                        WHEN "uomCost" THEN 
                            cVarValue = "" .
                        WHEN "totCost" THEN 
                            cVarValue = (IF ll-secure THEN STRING(v-tot-cst[2],"->>>,>>9.99") ELSE "").
                        WHEN "labCost" THEN 
                            cVarValue = (IF ll-secure THEN STRING(v-tot-lab[2],"->>>,>>9.99") ELSE "") .
                        WHEN "matCost" THEN 
                            cVarValue = (IF ll-secure THEN STRING(v-tot-mat[2],"->>>,>>9.99") ELSE "") .
                        WHEN "salesRep" THEN 
                            cVarValue = "" .
                        WHEN "sellValueOrd" THEN 
                            cVarValue = STRING(v-tot-ordsell[2],"->>,>>>,>>9.99") .    
                        WHEN "sellValueFg" THEN 
                            cVarValue = STRING(v-tot-fgsell[2],"->>,>>>,>>9.99") .
                        WHEN "custno" THEN 
                            cVarValue = "" .
                        WHEN "setHeader" THEN 
                            cVarValue = "" .
                        WHEN "qtyPerSet" THEN 
                            cVarValue = "" .
                        WHEN "custName" THEN 
                            cVarValue = "" .
                    END CASE.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                 
                END. /* When last of cust# */
       

            END.

            ASSIGN
                v-tot-qty[3]     = v-tot-qty[3] + v-tot-qty[2]
                v-tot-cst[3]     = v-tot-cst[3] + v-tot-cst[2]
                v-tot-ext[3]     = v-tot-ext[3] + v-tot-ext[2]
                v-tot-msf[3]     = v-tot-msf[3] + v-tot-msf[2]
                v-tot-mat[3]     = v-tot-mat[3] + v-tot-mat[2]
                v-tot-lab[3]     = v-tot-lab[3] + v-tot-lab[2]
                v-tot-ordsell[3] = v-tot-ordsell[3] + v-tot-ordsell[2]
                v-tot-fgsell[3]  = v-tot-fgsell[3] + v-tot-fgsell[2]
       
                v-tot-qty[2]     = 0
                v-tot-cst[2]     = 0
                v-tot-ext[2]     = 0
                v-tot-msf[2]     = 0
                v-tot-mat[2]     = 0
                v-tot-lab[2]     = 0
                v-tot-fgsell[2]  = 0
                v-tot-ordsell[2] = 0
                v-prnt[2]        = NO.
        

        END. /* last of tt-itemfg.cust-no */  
    
    
    END. /* each tt-itemfg */
                                      
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        .
        
    DO iCnt = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:    
            WHEN "itemfg.cust-no" THEN 
                cVarValue = "" .
            WHEN "itemfg.i-no" THEN 
                cvarValue = "" .
            WHEN "itemfg.part-no" THEN 
                cVarValue = "" .
            WHEN "itemfg.i-name" THEN 
                cVarValue = "" .
            WHEN "itemfg.procat" THEN 
                cVarValue = "" .
            WHEN "tag" THEN 
                cVarValue = "" .
            WHEN "TAG #" THEN 
                cVarValue = "" .
            WHEN "fgLotVal" THEN 
                cvarValue = "" .
            WHEN "jobNo" THEN 
                cVarValue = "" .
            WHEN "recDate" THEN 
                cVarValue = "" .
            WHEN "daysOld" THEN 
                cVarValue = "".
            WHEN "loc" THEN 
                cVarValue = "".
            WHEN "bin" THEN 
                cVarValue =    "".
            WHEN "msfOnHand" THEN 
                cVarValue = STRING(v-tot-msf[3],"->>9.999").
            WHEN "costUom" THEN 
                cVarValue = "".
            WHEN "relQty" THEN 
                cVarValue = "" .
            WHEN "qtyOnHand" THEN 
                cVarValue = STRING(v-tot-qty[3],"->>,>>>,>>9") .
            WHEN "lastSale" THEN 
                cVarValue = "" .
            WHEN "viewPo" THEN 
                cVarValue = "" .
            WHEN "linePo" THEN 
                cVarValue = "" .
            WHEN "relPo" THEN 
                cVarValue = "" .
            WHEN "ordPr" THEN 
                cVarValue = "".
            WHEN "sellPrice" THEN 
                cVarValue = "".
            WHEN "uomCost" THEN 
                cVarValue = "" .
            WHEN "totCost" THEN 
                cVarValue = (IF ll-secure THEN STRING(v-tot-cst[3],"->>>,>>9.99") ELSE "").
            WHEN "labCost" THEN 
                cVarValue = (IF ll-secure THEN STRING(v-tot-lab[3],"->>>,>>9.99") ELSE "") .
            WHEN "matCost" THEN 
                cVarValue = (IF ll-secure THEN STRING(v-tot-mat[3],"->>>,>>9.99") ELSE "") .
            WHEN "salerep" THEN 
                cVarValue = "" .
            WHEN "sellValueOrd" THEN 
                cVarValue = STRING(v-tot-ordsell[3],"->>,>>>,>>9.99") .    
            WHEN "sellValueFg" THEN 
                cVarValue = STRING(v-tot-fgsell[3],"->>,>>>,>>9.99") .
            WHEN "custno" THEN 
                cVarValue = "" .
            WHEN "setHeader" THEN 
                cVarValue = "" .
            WHEN "qtyPerSet" THEN 
                cVarValue = "" . 
            WHEN "cust-name" THEN 
                cVarValue = "" . 
        END CASE.
        
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(iCnt,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).
        
    END. /* Do after 'each tt-itemfg, don't know why? */
