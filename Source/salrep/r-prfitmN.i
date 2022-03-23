/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */

ASSIGN
    v-qty           = 0
    v-msf           = 0
    v-amt           = 0
    v-cst           = 0
    v-cst1          = 0
    v-cst2          = 0
    v-style         = ""
    v-test          = ""
    v-flute         = ""
    v-est-no        = ""
    v-len           = 0
    v-wid           = 0
    v-dep           = 0
    v-cust-part-no  = ""
    v-cust-part-no2 = ""
    v-date          = "" 
    v-order-date    = "".

FOR EACH cust
    WHERE cust.company EQ cocode
    AND cust.cust-no GE fcust
    AND cust.cust-no LE tcust
    AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
    AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
    NO-LOCK:
    {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    {sa/sa-sls03.i "fdate" "tdate"}
END.

FOR EACH tt-report
    WHERE tt-report.term-id EQ ""
    AND tt-report.key-01  EQ ""
    AND tt-report.key-02  EQ ""
    AND tt-report.key-03  EQ ""
    AND tt-report.key-04  EQ ""
    AND tt-report.key-05  EQ ""
    AND tt-report.key-06  EQ ""
    AND tt-report.key-07  EQ ""
    AND tt-report.key-08  EQ ""

    TRANSACTION:

    IF tt-report.key-10 EQ "ar-inv" THEN 
    DO:
        FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.

        FOR EACH ar-invl
            WHERE ar-invl.x-no    EQ ar-inv.x-no
            AND ar-invl.i-no    GE fitem
            AND ar-invl.i-no    LE titem
            AND ((tb_prep AND ar-invl.billable) OR NOT ar-invl.misc)
            NO-LOCK:

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ ar-invl.i-no
                AND itemfg.procat  GE fpcat
                AND itemfg.procat  LE tpcat
                NO-LOCK NO-ERROR.
          
            IF ("" LT fpcat OR "" GT tpcat) AND (NOT AVAILABLE itemfg) THEN NEXT.

            CREATE xtt-report.

            ASSIGN
                xtt-report.term-id = ""
                xtt-report.rec-id  = RECID(ar-invl)
                xtt-report.key-01  = IF sort-by-cust THEN tt-report.key-09
                                ELSE IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
                xtt-report.key-02  = ar-invl.i-no
                xtt-report.key-03  = STRING(ar-invl.inv-no,"99999999")
                xtt-report.key-09  = tt-report.key-09
                xtt-report.key-10  = "ar-invl".
        END.

        DELETE tt-report.
    END.

    ELSE
        IF tt-report.key-10 EQ "ar-cashl" THEN 
        DO:
            FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
            FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

            ASSIGN
                v-exc            = YES
                tt-report.key-01 = IF sort-by-cust THEN tt-report.key-09 ELSE ""
                tt-report.key-02 = ""
                tt-report.key-03 = STRING(ar-cashl.inv-no,"99999999").

            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

            ASSIGN
                lv-r-no = 0
                lv-type = "".
          
            IF AVAILABLE reftable THEN
                ASSIGN
                    lv-r-no = reftable.val[1]
                    lv-type = reftable.dscr.
            ELSE
                IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
                    ASSIGN
                        lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
                        lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

            IF lv-r-no NE 0 THEN 
            DO:
                IF lv-type EQ "items" THEN 
                DO:
                    RELEASE ar-invl.
                    FIND FIRST oe-retl
                        WHERE oe-retl.company EQ cocode
                        AND oe-retl.r-no    EQ lv-r-no
                        AND oe-retl.line    EQ ar-cashl.line
                        AND oe-retl.i-no    GE fitem
                        AND oe-retl.i-no    LE titem
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE oe-retl THEN
                        FIND FIRST ar-invl
                            WHERE ar-invl.company EQ cocode
                            AND ar-invl.cust-no EQ ar-cash.cust-no
                            AND ar-invl.inv-no  EQ ar-cashl.inv-no
                            AND ar-invl.i-no    EQ oe-retl.i-no
                            AND ((tb_prep AND ar-invl.billable) OR NOT ar-invl.misc)
                            NO-LOCK NO-ERROR.
                    IF AVAILABLE ar-invl THEN 
                    DO:
                        FIND FIRST itemfg
                            WHERE itemfg.company EQ cocode
                            AND itemfg.i-no    EQ ar-invl.i-no
                            AND itemfg.procat  GE fpcat
                            AND itemfg.procat  LE tpcat
                            NO-LOCK NO-ERROR.

                        CREATE xtt-report.
                        ASSIGN
                            v-exc            = NO
                            tt-report.key-01 = IF sort-by-cust THEN tt-report.key-09
                                ELSE IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
                            tt-report.key-02 = oe-retl.i-no.
                    END.
                END.

                ELSE
                    IF lv-type   EQ "freight"                  AND
                        "freight" GE fitem                      AND
                        "freight" LE titem                      AND
                        "frght"   GE fpcat                      AND
                        "frght"   LE tpcat                      THEN
                        ASSIGN
                            v-exc            = NO
                            tt-report.key-01 = "FRGHT"
                            tt-report.key-02 = "FREIGHT".

                    ELSE
                        IF lv-type EQ "tax"                    AND
                            "tax" GE fitem                      AND
                            "tax" LE titem                      AND
                            "tax" GE fpcat                      AND
                            "tax" LE tpcat                      THEN
                            ASSIGN
                                v-exc            = NO
                                tt-report.key-01 = "TAX"
                                tt-report.key-02 = "TAX".

                        ELSE
                            IF "" GE fitem AND
                                "" LE titem THEN v-exc = NO.
            END.

            ELSE
                IF "" GE fitem AND
                    "" LE titem THEN v-exc = NO.

            IF v-exc THEN DELETE tt-report.
        END.
END.

FOR EACH tt-report
    WHERE tt-report.term-id EQ ""

    BREAK BY tt-report.key-01
    BY tt-report.key-02
    BY tt-report.key-03

    WITH FRAME itemx DOWN

    TRANSACTION:

    CREATE w-data.
    ASSIGN
        w-data.i-no   = tt-report.key-02
        w-data.inv-no = int(tt-report.key-03)
        w-data.rec-id = tt-report.rec-id.

    ASSIGN
        v-job-no     = ""
        v-job-no2    = 0
        v-msf[1]     = 0
        v-cst[1]     = 0
        v-cst1[1]    = 0
        v-cst2[1]    = 0
        v-po-no-po   = 0
        v-style      = ""
        v-test       = ""
        v-flute      = ""
        v-est-no     = ""
        v-len        = 0
        v-wid        = 0
        v-dep        = 0
        v-date       = "" 
        v-order-date = ""
        iBolNo       = 0
        iOrdNo       = 0
        cPoNo        = ""
        .

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-data.i-no
        NO-LOCK NO-ERROR.

    v-color = 0.
    v-cust-part-no2 = "" .
    v-cust-part-no  = "" .
          
    FOR EACH itemfg-ink OF itemfg WHERE 
        itemfg-ink.i-no EQ itemfg.i-no NO-LOCK, 
        EACH item WHERE item.company EQ itemfg-ink.company AND 
        item.i-no EQ itemfg-ink.rm-i-no 
        NO-LOCK: 

        IF AVAILABLE (itemfg-ink) THEN 
        DO:
            v-color   = v-color + itemfg-ink.occurs.
        END.  
    END.

    IF AVAILABLE(itemfg) THEN 
    DO:
        /* IF RS_fgitem-cust-part-no = "Cust Part no" THEN */
        ASSIGN
            v-cust-part-no = itemfg.part-no.
        /* ELSE */
        ASSIGN
            v-cust-part-no2 = w-data.i-no.
       
        /*  IF TB_style-flute-test-lwd = YES THEN DO:*/
        ASSIGN
            v-style = itemfg.style
            v-len   = itemfg.l-score[50]
            v-wid   = itemfg.w-score[50]
            v-dep   = itemfg.d-score[50].
    END.
    FOR EACH eb FIELDS(test flute est-no) WHERE eb.company  EQ cocode
        AND eb.est-no   EQ itemfg.est-no
        AND eb.stock-no EQ itemfg.i-no NO-LOCK:
        ASSIGN 
            v-test  = eb.test
            v-flute = eb.flute.
        LEAVE.
    END.
    /*   END.*/
            
    IF tt-report.key-10 EQ "ar-invl" THEN 
    DO:
        FIND FIRST ar-invl WHERE RECID(ar-invl) EQ w-data.rec-id NO-LOCK.
        FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.
        ASSIGN
            v-cust-no  = ar-inv.cust-no
            v-date     = STRING(ar-inv.inv-date)
            v-pric     = ar-invl.unit-pr
            v-uom      = ar-invl.pr-uom
            v-job-no   = ar-invl.job-no
            v-job-no2  = ar-invl.job-no2
            v-po-no-po = ar-invl.po-no-po
            v-qty[1]   = IF ar-invl.ship-qty GT 0 THEN ar-invl.ship-qty ELSE ar-invl.inv-qty
            v-amt[1]   = ar-invl.amt
            v-msf[1]   = ar-invl.amt-msf
            iBolNo     = ar-invl.bol-no
            iOrdNo     = ar-invl.ord-no
            cPoNo      = ar-invl.po-no
            .

        FIND FIRST oe-ord WHERE oe-ord.company = cocode 
            AND oe-ord.ord-no  = ar-inv.ord-no NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ord THEN ASSIGN v-order-date = STRING(oe-ord.ord-date) .
        
        FIND FIRST job-hdr WHERE job-hdr.company = cocode
            AND job-hdr.job-no  = ar-invl.job-no
            AND job-hdr.job-no2 = ar-invl.job-no2
            AND job-hdr.i-no    = w-data.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE(job-hdr) AND job-hdr.est-no <> "" THEN
            v-est-no = TRIM(job-hdr.est-no).
        ELSE
            v-est-no = TRIM(ar-invl.est-no).
        IF v-est-no = "" THEN 
        DO:
            IF AVAILABLE itemfg THEN
                v-est-no = TRIM(itemfg.est-no) .
        END.
            
        RUN salrep/salecost.p ("4",
            ROWID(ar-invl),
            v-job-no,
            v-job-no2,
            v-qty[1],
            OUTPUT v-cst[1]).

        RUN salrep/salecost.p ("2",
            ROWID(ar-invl),
            v-job-no,
            v-job-no2,
            v-qty[1],
            OUTPUT v-cst1[1]).

        RUN salrep/salecost.p ("3",
            ROWID(ar-invl),
            v-job-no,
            v-job-no2,
            v-qty[1],
            OUTPUT v-cst2[1]).
    END.

    ELSE
        IF tt-report.key-10 EQ "ar-cashl" THEN 
        DO:
            FIND FIRST ar-cashl WHERE RECID(ar-cashl) EQ w-data.rec-id NO-LOCK.
            FIND FIRST ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.
            ASSIGN
                v-cust-no = ar-cash.cust-no
                v-date    = STRING(ar-cash.check-date)
                v-pric    = ar-cashl.amt-paid - ar-cashl.amt-disc
                v-uom     = ""
                v-qty[1]  = 0
                v-cst[1]  = 0
                v-cst1[1] = 0
                v-cst2[1] = 0
                v-amt[1]  = ar-cashl.amt-paid - ar-cashl.amt-disc.

            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

            ASSIGN
                lv-r-no = 0
                lv-type = "".
          
            IF AVAILABLE reftable THEN
                ASSIGN
                    lv-r-no = reftable.val[1]
                    lv-type = reftable.dscr.
            ELSE
                IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
                    ASSIGN
                        lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
                        lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).


            IF lv-r-no NE 0 THEN 
            DO:
                FIND FIRST oe-reth
                    WHERE oe-reth.company EQ cocode
                    AND oe-reth.r-no    EQ lv-r-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE oe-reth THEN
                    FOR EACH ar-inv
                        WHERE ar-inv.company EQ cocode
                        AND ar-inv.cust-no EQ oe-reth.cust-no
                        AND ar-inv.inv-no  EQ oe-reth.inv-no
                        NO-LOCK,
                        EACH ar-invl
                        WHERE ar-invl.x-no EQ ar-inv.x-no
                        AND ar-invl.i-no EQ w-data.i-no
                        NO-LOCK:
                        v-po-no-po = ar-invl.po-no-po.

                        FIND FIRST oe-ord WHERE oe-ord.company = cocode 
                            AND oe-ord.ord-no  = ar-inv.ord-no NO-LOCK NO-ERROR.
                        IF AVAILABLE oe-ord THEN ASSIGN v-order-date = STRING(oe-ord.ord-date) .
                        LEAVE.
                    END.

                FIND FIRST oe-retl
                    WHERE oe-retl.company EQ cocode
                    AND oe-retl.r-no    EQ lv-r-no
                    AND oe-retl.line    EQ ar-cashl.line
                    NO-LOCK NO-ERROR.

                IF AVAILABLE oe-retl THEN 
                DO:
                    ASSIGN
                        v-pric    = oe-retl.unit-pr
                        v-uom     = oe-retl.uom
                        v-job-no  = oe-retl.job-no
                        v-job-no2 = oe-retl.job-no2
                        v-qty[1]  = - oe-retl.tot-qty-return
                        v-cst[1]  = oe-retl.cost * v-qty[1] / 1000.
                    v-cst1[1] = oe-retl.cost * v-qty[1] / 1000.
                    v-cst2[1] = oe-retl.cost * v-qty[1] / 1000.

                    FIND FIRST job-hdr WHERE job-hdr.company = cocode
                        AND job-hdr.job-no  = oe-retl.job-no
                        AND job-hdr.job-no2 = oe-retl.job-no2 
                        AND job-hdr.i-no    = w-data.i-no NO-LOCK NO-ERROR.
                    IF AVAILABLE(job-hdr) AND job-hdr.est-no <> "" THEN 
                        v-est-no = TRIM(job-hdr.est-no).
                    ELSE 
                    DO:
                        IF AVAILABLE itemfg THEN
                            v-est-no = TRIM(itemfg.est-no) .
                    END.

                    FIND FIRST ar-invl
                        WHERE ar-invl.company EQ cocode
                        AND ar-invl.cust-no EQ ar-cash.cust-no
                        AND ar-invl.inv-no  EQ ar-cashl.inv-no
                        AND ar-invl.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE ar-invl THEN 
                    DO:
                        v-pric = ar-invl.unit-pr.

                        RUN salrep/salecost.p ("4",
                            ROWID(ar-invl),
                            v-job-no,
                            v-job-no2,
                            v-qty[1],
                            OUTPUT v-cst[1]).
                        RUN salrep/salecost.p ("2",
                            ROWID(ar-invl),
                            v-job-no,
                            v-job-no2,
                            v-qty[1],
                            OUTPUT v-cst1[2]).
                        RUN salrep/salecost.p ("3",
                            ROWID(ar-invl),
                            v-job-no,
                            v-job-no2,
                            v-qty[1],
                            OUTPUT v-cst2[3]).
                    END.
                END.
            END.
        END.


    IF v-msf[1] EQ 0 AND AVAILABLE itemfg THEN
        v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).

    IF v-msf[1] EQ 0 AND AVAILABLE itemfg THEN
        v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).

    /*   if v-unit eq "E" then
         assign
          v-brdc = v-cst[1]
          v-marg = v-amt[1] - v-cst[1].
          
       else do:*/
    v-brdc = v-cst[1] / (v-qty[1] / 1000).
    v-ordc = v-cst1[1] / (v-qty[1] / 1000).
    v-invc = v-cst2[1] / (v-qty[1] / 1000).
        
    /*    if v-unit eq "U" then
          v-marg = (v-amt[1] - v-cst[1]) / (v-qty[1] / 1000).
        else*/
    v-marg = v-cst[1] / v-msf[1].
    /*   end.*/

    /*   if v-cost3 eq "C" then
         v-brdp = v-cst[1] / v-amt[1] * 100.
       else*/
    v-brdp = (v-amt[1] - v-cst[1]) / v-amt[1] * 100.
       
    v-$msf = v-amt[1] / v-msf[1].

    IF v-brdc EQ ? THEN v-brdc = 0.
    IF v-ordc EQ ? THEN v-ordc = 0.
    IF v-invc EQ ? THEN v-invc = 0.
    IF v-marg EQ ? THEN v-marg = 0.
    IF v-brdp EQ ? THEN v-brdp = 0.
    IF v-$msf EQ ? THEN v-$msf = 0.

    /* IF tl_color = YES THEN */
    x-v-color = STRING(v-color).
    ASSIGN
        cSoldToName = ""
        .
    FOR EACH soldto NO-LOCK
        WHERE soldto.company EQ cocode
        AND soldto.cust-no   EQ v-cust-no
        AND soldto.sold-id   EQ ar-inv.ship-id
        :
        ASSIGN
            cSoldToName = soldto.sold-name
            .
    END.
    /* ELSE
        x-v-color = "". */
      
    /*  IF TB_style-flute-test-lwd = YES THEN DO:
         DISPLAY  
            v-cust-no
            v-order-date
            v-date
            w-data.inv-no
            w-data.i-no
            v-cust-part-no    WHEN RS_fgitem-cust-part-no = "Cust Part no" @ w-data.i-no
            itemfg.procat     WHEN AVAIL itemfg
            v-qty[1]
            itemfg.t-sqft     WHEN AVAIL itemfg
            v-msf[1]
            v-$msf
            v-est-no
            v-style
            v-flute
            v-test
            v-len
            v-wid
            v-dep
            x-v-color         
         WITH FRAME itemb NO-ERROR.
         DOWN WITH FRAME itemb. */
    {custom/statusMsg.i " 'Processing Customer#  '  + v-cust-no "}

    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField: 
            WHEN "cust"        THEN 
                cVarValue = STRING(v-cust-no) .
            WHEN "inv"         THEN 
                cVarValue = STRING(w-data.inv-no) .
            WHEN "ino"         THEN 
                cVarValue = REPLACE(v-cust-part-no2, '"', "") .
            WHEN "cust-part"   THEN 
                cVarValue = REPLACE(v-cust-part-no, '"', "") .
            WHEN "cat"         THEN 
                cVarValue = IF AVAILABLE itemfg THEN itemfg.procat ELSE "" .
            WHEN "qty-shp"     THEN 
                cVarValue = STRING(v-qty[1],"->>>,>>>,>>>") .
            WHEN "i-sq"        THEN 
                cVarValue = IF AVAILABLE itemfg THEN STRING(itemfg.t-sqft,">>>9.999<<") ELSE "" .
            WHEN "ttl-msf"     THEN 
                cVarValue = STRING(v-msf[1],"->>9.99") .
            WHEN "msf"         THEN 
                cVarValue = STRING(v-$msf,"->>>9") .
            WHEN "unt-prc"     THEN 
                cVarValue = STRING(v-pric,"->>>,>>>,>>9.99") .
            WHEN "uom"         THEN 
                cVarValue = v-uom .
            WHEN "brdcst-m"       THEN 
                cVarValue = STRING(v-brdc,"->>>,>>9.99") .
            WHEN "ordcst-m"       THEN 
                cVarValue = STRING(v-ordc,"->>>,>>9.99") .
            WHEN "invcst-m"       THEN 
                cVarValue = STRING(v-invc,"->>>,>>9.99") .
            WHEN "mar-m"       THEN 
                cVarValue = STRING(v-marg,"->>>,>>9.99") .
            WHEN "cst"         THEN 
                cVarValue = STRING(v-brdp,"->>>,>>9.99") .
            WHEN "colr"        THEN 
                cVarValue = x-v-color .
            WHEN "ord-dt"      THEN 
                cVarValue = v-order-date .
            WHEN "shp-dt"      THEN 
                cVarValue = v-date .
            WHEN "inv-amt"     THEN 
                cVarValue = STRING(v-amt[1],"->>>,>>>,>>9.99") .
            WHEN "est"         THEN 
                cVarValue = v-est-no .
            WHEN "styl"        THEN 
                cVarValue = v-style  .
            WHEN "flut"        THEN 
                cVarValue = STRING(v-flute) .
            WHEN "tst"         THEN 
                cVarValue = STRING(v-test)   .  
            WHEN "lnth"        THEN 
                cVarValue = STRING(v-len)  .   
            WHEN "wdth"        THEN 
                cVarValue = STRING(v-wid)  .   
            WHEN "dpth"        THEN 
                cVarValue = STRING(v-dep)  .   
            WHEN "bol-no"      THEN 
                cVarValue = STRING(iBolNo,">>>>>>>9")  .   
            WHEN "ord-no"      THEN 
                cVarValue = STRING(iOrdNo,">>>>>>>9")  .   
            WHEN "po-no"       THEN 
                cVarValue = STRING(cPoNo)  .   
            WHEN "cSoldToName" THEN 
                cVarValue = STRING(cSoldToName,"x(30)")  .  
                                                              
        END CASE.
                      
        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.
          
    PUT UNFORMATTED cDisplay SKIP.
    IF rd-dest = 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            cExcelDisplay SKIP.
    END.
    /*  END.
      ELSE DO:
         /*DISPLAY 
            v-cust-no
            w-data.inv-no
            w-data.i-no       
            v-cust-part-no    WHEN RS_fgitem-cust-part-no = "Cust Part no" @ w-data.i-no
            itemfg.procat     when avail itemfg
            v-qty[1]
            itemfg.t-sqft     when avail itemfg
            v-msf[1]
            v-$msf
            v-pric
            v-amt[1]          when v-unit eq "E" @ v-pric
            v-uom             when v-unit ne "E"
            v-brdc            when v-cost2 
            v-marg            when v-cost2
            v-brdp            when v-cost2 
            x-v-color.           
         DOWN. */
             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "cust"        THEN cVarValue = string(v-cust-no) .
                        WHEN "inv"         THEN cVarValue = STRING(w-data.inv-no) .
                        WHEN "ino"         THEN cVarValue = REPLACE(v-cust-part-no2, '"', "") .
                        WHEN "cust-part"   THEN cVarValue = REPLACE(v-cust-part-no, '"', "") .
                        WHEN "cat"         THEN cVarValue = IF AVAIL itemfg THEN itemfg.procat ELSE "" .
                        WHEN "qty-shp"     THEN cVarValue = STRING(v-qty[1],"->>>,>>>,>>>") .
                        WHEN "i-sq"        THEN cVarValue = IF AVAIL itemfg THEN string(itemfg.t-sqft,">>>9.999<<") ELSE "" .
                        WHEN "ttl-msf"     THEN cVarValue = STRING(v-msf[1],"->>9.99") .
                        WHEN "msf"         THEN cVarValue = STRING(v-$msf,"->>>9") .
                        WHEN "unt-prc"     THEN cVarValue = IF v-unit = "E" THEN string(v-amt[1],"->>>,>>>,>>9.99") ELSE string(v-pric,"->>>,>>>,>>9.99") .
                        WHEN "uom"         THEN cVarValue = IF v-unit NE "E" THEN v-uom else "" .
                        WHEN "cst-m"        THEN cVarValue = STRING(v-brdc,"->>>,>>9.99") .
                        WHEN "mar-m"       THEN cVarValue = STRING(v-marg,"->>>,>>9.99") .
                        WHEN "cst"            THEN cVarValue = STRING(v-brdp,"->>>,>>9.99") .
                        WHEN "colr"        THEN cVarValue = x-v-color .
                        WHEN "ord-dt"      THEN cVarValue = "".
                        WHEN "shp-dt"      THEN cVarValue = "".
                        WHEN "inv-amt"     THEN cVarValue = "".
                        WHEN "est"         THEN cVarValue = "".
                        WHEN "styl"        THEN cVarValue = "".
                        WHEN "flut"        THEN cVarValue = "".
                        WHEN "tst"         THEN cVarValue = "".
                        WHEN "lnth"        THEN cVarValue = "".
                        WHEN "wdth"        THEN cVarValue = "".
                        WHEN "dpth"        THEN cVarValue = "".
                                                              
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest = 3 THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
      END.*/

     
      

    /*  IF rd-dest = 3 THEN DO: 
         IF TB_style-flute-test-lwd = YES THEN
            PUT STREAM excel
               '"' v-cust-no                                      '",'  
               '"' v-order-date                                   '",'
               '"' v-date                                         '",'  
               '"' w-data.inv-no                                  '",'
               '"' REPLACE(v-cust-part-no, '"', "") FORMAT "X(15)"                 '",'  
               '"' (IF AVAIL itemfg THEN itemfg.procat ELSE "")   '",' 
               '"' v-qty[1]                                       '",'
               '"' (IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0)    '",'
               '"' v-msf[1]                                       '",'
               '"' v-$msf                                         '",'
               '"' v-pric format "->>>,>>>,>>9.99<<"              '",'
               '"' v-uom                                          '",'
               '"' v-amt[1] format "->>>,>>>,>>9.99<<"            '",'
               '"' v-est-no                                       '",'
               '"' v-style                                        '",'
               '"' v-flute                                        '",'
               '"' v-test                                         '",'
               '"' v-len                                          '",'
               '"' v-wid                                          '",'
               '"' v-dep                                          '",'
               '"' x-v-color                                        '",'
               SKIP.
         ELSE
            PUT STREAM excel 
               '"' v-cust-no                                      '",'
               '"' w-data.inv-no                                  '",'
               '"' REPLACE(v-cust-part-no, '"', "")  FORMAT "X(15)"                '",'  
               '"' (IF AVAIL itemfg THEN itemfg.procat ELSE "")   '",' 
               '"' v-qty[1]                                       '",'
               '"' (IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0)    '",'
               '"' v-msf[1]                                       '",'
               '"' v-$msf                                         '",'
               '"' (IF v-unit = "E" THEN v-amt[1] ELSE v-pric)    '",'
               '"' (IF v-unit NE "E" THEN v-uom else "")          '",'
               '"' v-brdc                                         '",'
               '"' v-marg                                         '",'
               '"' v-brdp                                         '",'
               '"' x-v-color                                        '",'
               SKIP.
      END. */
    ASSIGN
        v-qty[2]  = v-qty[2] + v-qty[1]
        v-msf[2]  = v-msf[2] + v-msf[1]
        v-cst[2]  = v-cst[2] + IF v-cst[1] EQ ? THEN 0 ELSE v-cst[1]
        v-cst1[2] = v-cst1[2] + IF v-cst1[1] EQ ? THEN 0 ELSE v-cst1[1]
        v-cst2[2] = v-cst2[2] + IF v-cst2[1] EQ ? THEN 0 ELSE v-cst2[1]
        v-amt[2]  = v-amt[2] + v-amt[1].

    IF LAST-OF(tt-report.key-02) THEN 
    DO:
        /* underline v-qty[1] v-msf[1] with frame itemx.
 
         if v-cost2 AND TB_style-flute-test-lwd = NO then underline v-brdc v-marg v-brdp with frame itemx.
 
         if v-unit eq "E" AND TB_style-flute-test-lwd = NO then underline v-pric with frame itemx. */

        IF (NOT FIRST-OF(tt-report.key-02)) THEN 
        DO:
            /*  if v-unit eq "E" then
                assign
                 v-brdc = v-cst[2]
                 v-marg = v-amt[2] - v-cst[2].
                 
              else do:*/
            v-brdc = v-cst[2] / (v-qty[2] / 1000)  .
            v-ordc = v-cst1[2] / (v-qty[2] / 1000)  .
            v-invc = v-cst2[2] / (v-qty[2] / 1000)  .
        
            /*    if v-unit eq "U" then
                  v-marg = (v-amt[2] - v-cst[2]) / (v-qty[2] / 1000).
                else */
            v-marg = v-cst[2] / v-msf[2].
            /*   end.*/

            /*    if v-cost3 eq "C" then
                  v-brdp = v-cst[2] / v-amt[2] * 100.
                else*/
            v-brdp = (v-amt[2] - v-cst[2]) / v-amt[2] * 100.
       
            v-$msf = v-amt[2] / v-msf[2].

            IF v-brdc EQ ? THEN v-brdc = 0.
            IF v-ordc EQ ? THEN v-ordc = 0.
            IF v-invc EQ ? THEN v-invc = 0.
            IF v-marg EQ ? THEN v-marg = 0.
            IF v-brdp EQ ? THEN v-brdp = 0.
            IF v-$msf EQ ? THEN v-$msf = 0.

            /* display "    ITEM" @ v-cust-no
                     "TOTALS"   @ w-data.i-no
                     v-qty[2]   @ v-qty[1]
                     v-msf[2]   @ v-msf[1]
                     v-$msf
                     v-amt[2]      when v-unit eq "E" AND TB_style-flute-test-lwd = NO @ v-pric
                     v-brdc        when v-cost2 AND TB_style-flute-test-lwd = NO
                     v-marg        when v-cost2 AND TB_style-flute-test-lwd = NO
                     v-brdp        when v-cost2 AND TB_style-flute-test-lwd = NO
   
                 with frame itemx.
   
             down with frame itemx. */
            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:
                    WHEN "cust"        THEN 
                        cVarValue = "" .
                    WHEN "inv"         THEN 
                        cVarValue = "" .
                    WHEN "ino"         THEN 
                        cVarValue = "" .
                    WHEN "cust-part"   THEN 
                        cVarValue = "" .
                    WHEN "cat"         THEN 
                        cVarValue = "" .
                    WHEN "qty-shp"     THEN 
                        cVarValue = STRING(v-qty[2],"->>>,>>>,>>>") .
                    WHEN "i-sq"        THEN 
                        cVarValue = "" .
                    WHEN "ttl-msf"     THEN 
                        cVarValue = STRING(v-msf[2],"->>9.99") .
                    WHEN "msf"         THEN 
                        cVarValue = STRING(v-$msf,"->>>9") .
                    WHEN "unt-prc"     THEN 
                        cVarValue = "" .
                    WHEN "uom"         THEN 
                        cVarValue = "" .
                    WHEN "brdcst-m"       THEN 
                        cVarValue = STRING(v-brdc,"->>>,>>9.99") .
                    WHEN "ordcst-m"       THEN 
                        cVarValue = STRING(v-ordc,"->>>,>>9.99") .
                    WHEN "invcst-m"       THEN 
                        cVarValue = STRING(v-invc,"->>>,>>9.99") .
                    WHEN "mar-m"       THEN 
                        cVarValue = STRING(v-marg,"->>>,>>9.99")  .
                    WHEN "cst"         THEN 
                        cVarValue = STRING(v-brdp,"->>>,>>9.99")  .
                    WHEN "colr"        THEN 
                        cVarValue = "" .
                    WHEN "ord-dt"      THEN 
                        cVarValue = "".
                    WHEN "shp-dt"      THEN 
                        cVarValue = "".
                    WHEN "inv-amt"     THEN 
                        cVarValue = STRING(v-amt[2],"->>>,>>>,>>9.99") .
                    WHEN "est"         THEN 
                        cVarValue = "".
                    WHEN "styl"        THEN 
                        cVarValue = "".
                    WHEN "flut"        THEN 
                        cVarValue = "".
                    WHEN "tst"         THEN 
                        cVarValue = "".
                    WHEN "lnth"        THEN 
                        cVarValue = "".
                    WHEN "wdth"        THEN 
                        cVarValue = "".
                    WHEN "dpth"        THEN 
                        cVarValue = "".
                    WHEN "bol-no"      THEN 
                        cVarValue = "".   
                    WHEN "ord-no"      THEN 
                        cVarValue = "".   
                    WHEN "po-no"       THEN 
                        cVarValue = "".
                    WHEN "cSoldToName" THEN 
                        cVarValue = "".
                                                              
                END CASE.
                      
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED 
                " ITEM TOTALS"  SUBSTRING(cDisplay,13,350) SKIP.
            PUT SKIP(1).
        END.

        ASSIGN
            v-qty[3]  = v-qty[3] + v-qty[2]
            v-msf[3]  = v-msf[3] + v-msf[2]
            v-cst[3]  = v-cst[3] + v-cst[2]
            v-cst1[3] = v-cst1[3] + v-cst1[2]
            v-cst2[3] = v-cst2[3] + v-cst2[2]
            v-amt[3]  = v-amt[3] + v-amt[2]

            v-qty[2]  = 0
            v-msf[2]  = 0
            v-cst[2]  = 0
            v-cst1[2] = 0
            v-cst2[2] = 0
            v-amt[2]  = 0.
    END.

    IF LAST-OF(tt-report.key-01) THEN 
    DO:
        /* underline v-qty[1] v-msf[1] with frame itemx.
 
         if v-cost2 AND TB_style-flute-test-lwd = NO then underline v-brdc v-marg v-brdp with frame itemx.
 
         if v-unit eq "E" AND TB_style-flute-test-lwd = NO then underline v-pric with frame itemx. */

        IF (NOT FIRST-OF(tt-report.key-01)) THEN 
        DO:
            /*  if v-unit eq "E" then
                assign
                 v-brdc = v-cst[3]
                 v-marg = v-amt[3] - v-cst[3].
                 
              else do:*/
            v-brdc = v-cst[3] / (v-qty[3] / 1000)  .
            v-ordc = v-cst1[3] / (v-qty[3] / 1000)  .
            v-invc = v-cst2[3] / (v-qty[3] / 1000)  .
        
            /*  if v-unit eq "U" then
                v-marg = (v-amt[3] - v-cst[3]) / (v-qty[3] / 1000).
              else */
            v-marg = v-cst[3] / v-msf[3].
            /*   end. */

            /*     if v-cost3 eq "C" then
                   v-brdp = v-cst[3] / v-amt[3] * 100.
                 else*/
            v-brdp = (v-amt[3] - v-cst[3]) / v-amt[3] * 100.
       
            v-$msf = v-amt[3] / v-msf[3].

            IF v-brdc EQ ? THEN v-brdc = 0.
            IF v-ordc EQ ? THEN v-ordc = 0.
            IF v-invc EQ ? THEN v-invc = 0.
            IF v-marg EQ ? THEN v-marg = 0.
            IF v-brdp EQ ? THEN v-brdp = 0.
            IF v-$msf EQ ? THEN v-$msf = 0.

            /* display "PROD CAT" @ v-cust-no
                     "CUSTOMER" when sort-by-cust @ v-cust-no
                     "TOTALS"   @ w-data.i-no
                     v-qty[3]   @ v-qty[1]
                     v-msf[3]   @ v-msf[1]
                     v-$msf
                     v-amt[3]      when v-unit eq "E" AND TB_style-flute-test-lwd = NO @ v-pric
                     v-brdc        when v-cost2 AND TB_style-flute-test-lwd = NO
                     v-marg        when v-cost2 AND TB_style-flute-test-lwd = NO
                     v-brdp        when v-cost2 AND TB_style-flute-test-lwd = NO
   
                 with frame itemx.
   
             down with frame itemx. */

            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:
                    WHEN "cust"        THEN 
                        cVarValue = "" .
                    WHEN "inv"         THEN 
                        cVarValue = "" .
                    WHEN "ino"         THEN 
                        cVarValue = "" .
                    WHEN "cust-part"   THEN 
                        cVarValue = "" .
                    WHEN "cat"         THEN 
                        cVarValue = "" .
                    WHEN "qty-shp"     THEN 
                        cVarValue = STRING(v-qty[3],"->>>,>>>,>>>") .
                    WHEN "i-sq"        THEN 
                        cVarValue = "" .
                    WHEN "ttl-msf"     THEN 
                        cVarValue = STRING(v-msf[3],"->>9.99") .
                    WHEN "msf"         THEN 
                        cVarValue = STRING(v-$msf,"->>>9") .
                    WHEN "unt-prc"     THEN 
                        cVarValue =   "" .
                    WHEN "uom"         THEN 
                        cVarValue = "" .
                    WHEN "brdcst-m"       THEN 
                        cVarValue = STRING(v-brdc,"->>>,>>9.99") .
                    WHEN "ordcst-m"       THEN 
                        cVarValue = STRING(v-ordc,"->>>,>>9.99") .
                    WHEN "invcst-m"       THEN 
                        cVarValue = STRING(v-invc,"->>>,>>9.99") .
                    WHEN "mar-m"       THEN 
                        cVarValue = STRING(v-marg,"->>>,>>9.99")  .
                    WHEN "cst"         THEN 
                        cVarValue = STRING(v-brdp,"->>>,>>9.99")  .
                    WHEN "colr"        THEN 
                        cVarValue = "" .
                    WHEN "ord-dt"      THEN 
                        cVarValue = "".
                    WHEN "shp-dt"      THEN 
                        cVarValue = "".
                    WHEN "inv-amt"     THEN 
                        cVarValue = STRING(v-amt[3],"->>>,>>>,>>9.99") .
                    WHEN "est"         THEN 
                        cVarValue = "".
                    WHEN "styl"        THEN 
                        cVarValue = "".
                    WHEN "flut"        THEN 
                        cVarValue = "".
                    WHEN "tst"         THEN 
                        cVarValue = "".
                    WHEN "lnth"        THEN 
                        cVarValue = "".
                    WHEN "wdth"        THEN 
                        cVarValue = "".
                    WHEN "dpth"        THEN 
                        cVarValue = "".
                    WHEN "bol-no"      THEN 
                        cVarValue = "".   
                    WHEN "ord-no"      THEN 
                        cVarValue = "".   
                    WHEN "po-no"       THEN 
                        cVarValue = "".
                    WHEN "cSoldToName" THEN 
                        cVarValue = "".
                                                              
                END CASE.
                      
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED 
                " PROD CAT CUSTOMER TOTALS"  SUBSTRING(cDisplay,26,350) SKIP.
            
            PUT SKIP(1).
        END.                
                                 
        ASSIGN
            v-qty[4]  = v-qty[4] + v-qty[3]
            v-msf[4]  = v-msf[4] + v-msf[3]
            v-cst[4]  = v-cst[4] + v-cst[3]
            v-cst1[4] = v-cst1[4] + v-cst1[3]
            v-cst2[4] = v-cst2[4] + v-cst2[3]
            v-amt[4]  = v-amt[4] + v-amt[3]

            v-qty[3]  = 0
            v-msf[3]  = 0
            v-cst[3]  = 0
            v-cst1[3] = 0
            v-cst2[3] = 0
            v-amt[3]  = 0.
    END.

    DELETE w-data.
    DELETE tt-report.
END.

/* display final totals */
IF v-qty[4] NE 0 OR v-cst[4] NE 0 OR v-cst1[4] NE 0 OR v-cst2[4] NE 0 OR v-amt[4] NE 0 THEN 
DO:
    PUT SKIP(1).

    /* underline v-qty[1] v-msf[1] with frame itemx.

     if v-cost2 AND TB_style-flute-test-lwd = NO then underline v-brdc v-marg v-brdp with frame itemx.*/

    /*   if v-unit eq "E" AND TB_style-flute-test-lwd = NO then do:
        /* underline v-pric with frame itemx.  */
         
         assign
          v-brdc = v-cst[4]
          v-marg = v-amt[4] - v-cst[4].
       end.
       
       else do:*/
    v-brdc = v-cst[4] / (v-qty[4] / 1000) .
    v-ordc = v-cst1[4] / (v-qty[4] / 1000) .
    v-invc = v-cst2[4] / (v-qty[4] / 1000) .
        
    /*   if v-unit eq "U" then
         v-marg = (v-amt[4] - v-cst[4]) / (v-qty[4] / 1000).
       else*/
    v-marg = v-cst[4] / v-msf[4].
    /* end.*/

    /*  if v-cost3 eq "C" then
        v-brdp = v-cst[4] / v-amt[4] * 100.
      else*/
    v-brdp = (v-amt[4] - v-cst[4]) / v-amt[4] * 100.
       
    v-$msf = v-amt[4] / v-msf[4].

    IF v-brdc EQ ? THEN v-brdc = 0.
    IF v-ordc EQ ? THEN v-ordc = 0.
    IF v-invc EQ ? THEN v-invc = 0.
    IF v-marg EQ ? THEN v-marg = 0.
    IF v-brdp EQ ? THEN v-brdp = 0.
    IF v-$msf EQ ? THEN v-$msf = 0.

    /* display "   GRAND" @ v-cust-no
             "TOTALS"   @ w-data.i-no
             v-qty[4]   @ v-qty[1]
             v-msf[4]   @ v-msf[1]
             v-$msf
             v-amt[4]          when v-unit eq "E" AND TB_style-flute-test-lwd = NO @ v-pric
             v-brdc            when v-cost2 AND TB_style-flute-test-lwd = NO
             v-marg            when v-cost2 AND TB_style-flute-test-lwd = NO
             v-brdp            when v-cost2 AND TB_style-flute-test-lwd = NO
         with frame itemx. */
    PUT SKIP str-line SKIP .
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:
            WHEN "cust"        THEN 
                cVarValue = "" .
            WHEN "inv"         THEN 
                cVarValue = "" .
            WHEN "ino"         THEN 
                cVarValue = "" .
            WHEN "cust-part"   THEN 
                cVarValue = "" .
            WHEN "cat"         THEN 
                cVarValue = "" .
            WHEN "qty-shp"     THEN 
                cVarValue = STRING(v-qty[4],"->>>,>>>,>>>") .
            WHEN "i-sq"        THEN 
                cVarValue = "" .
            WHEN "ttl-msf"     THEN 
                cVarValue = STRING(v-msf[4],"->>9.99") .
            WHEN "msf"         THEN 
                cVarValue = STRING(v-$msf,"->>>9") .
            WHEN "unt-prc"     THEN 
                cVarValue = ""  .
            WHEN "uom"         THEN 
                cVarValue = "" .
            WHEN "brdcst-m"       THEN 
                cVarValue = STRING(v-brdc,"->>>,>>9.99") .
            WHEN "ordcst-m"       THEN 
                cVarValue = STRING(v-ordc,"->>>,>>9.99") .
            WHEN "invcst-m"       THEN 
                cVarValue = STRING(v-invc,"->>>,>>9.99") .
            WHEN "mar-m"       THEN 
                cVarValue = STRING(v-marg,"->>>,>>9.99")  .
            WHEN "cst"         THEN 
                cVarValue = STRING(v-brdp,"->>>,>>9.99")  .
            WHEN "colr"        THEN 
                cVarValue = "" .
            WHEN "ord-dt"      THEN 
                cVarValue = "".
            WHEN "shp-dt"      THEN 
                cVarValue = "".
            WHEN "inv-amt"     THEN 
                cVarValue = STRING(v-amt[4],"->>>,>>>,>>9.99").
            WHEN "est"         THEN 
                cVarValue = "".
            WHEN "styl"        THEN 
                cVarValue = "".
            WHEN "flut"        THEN 
                cVarValue = "".
            WHEN "tst"         THEN 
                cVarValue = "".
            WHEN "lnth"        THEN 
                cVarValue = "".
            WHEN "wdth"        THEN 
                cVarValue = "".
            WHEN "dpth"        THEN 
                cVarValue = "".
            WHEN "bol-no"      THEN 
                cVarValue = "".   
            WHEN "ord-no"      THEN 
                cVarValue = "".   
            WHEN "po-no"       THEN 
                cVarValue = "".
            WHEN "cSoldToName" THEN 
                cVarValue = "".
                                                              
        END CASE.
                      
        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.
          
    PUT UNFORMATTED 
        " GRAND TOTALS"  SUBSTRING(cDisplay,14,350) SKIP.
            
END.
