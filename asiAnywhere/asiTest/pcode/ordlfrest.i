DEF VAR ip-new-ord AS LOG NO-UNDO.
DEF VAR v-d-ordqty-price LIKE quoteqty.price INIT -1 NO-UNDO.
DEF VAR v-d-ordqty-uom AS CHAR NO-UNDO.
DEF VAR v-chose-quote AS LOG NO-UNDO.
DEF VAR v-d-ordqty-error AS LOG NO-UNDO.
def var v-factor as dec no-undo.
DEF VAR ll-new-fg AS LOG NO-UNDO.
DEF VAR ll-do-entry AS LOG NO-UNDO.
DEF VAR ll-canceled AS LOG NO-UNDO.

    
find first xest where xest.company eq cocode
                  and xest.est-no  eq FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) 
                NO-LOCK no-error.
                ASSIGN
                    vEstType = xest.est-type 
                    lv-est-no = FILL(" ",8 - LENGTH(TRIM(xest.est-no))) + TRIM(xest.est-no).

          ASSIGN  v-est-run = xest.est-type - if xest.est-type gt 4 then 4 else 0.

        /* RECALC ESTIMATE */
         save_id = RECID(oe-ord).
        
       IF v-est-run EQ 3 OR v-est-run EQ 4 THEN  /* not done ??? */
        RUN VALUE(ENTRY(v-est-run + INT(xest.est-type EQ 8),v-run-list)).
  

FOR EACH blk NO-LOCK:
    
END.
        find first xeb  where xeb.company = xest.company 
            and xeb.est-no eq xest.est-no and xeb.form-no  eq 0 no-lock no-error.



   FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = xest.est-no AND eb.form-no = 0 NO-LOCK NO-ERROR.
   IF AVAIL eb  THEN DO:

       
       FIND LAST oe-ordl WHERE oe-ordl.ord-no = Ordernum NO-LOCK NO-ERROR.
       IF AVAIL oe-ordl THEN DO:
           ASSIGN    Orderline = oe-ordl.LINE + 1.
        END.  /*IF AVAIL bf-ord THEN DO:*/
        ELSE DO:
            ASSIGN Orderline = 1.
        END.
        
       create b-oe-ordl.
       assign 
           b-oe-ordl.company       = prmComp
           b-oe-ordl.ord-no        = Ordernum
           b-oe-ordl.LINE          = Orderline
           b-oe-ordl.type-code     = prmType
           b-oe-ordl.stat          = "W"
           b-oe-ordl.cust-no       = prmCustomer
           b-oe-ordl.po-no         = prmPonum
           b-oe-ordl.req-code      = prmDueCode
           b-oe-ordl.req-date      = prmDueDate
           b-oe-ordl.prom-code     = prmDueCode
           b-oe-ordl.prom-date     = prmDueDate
           b-oe-ordl.over-pct      = prmOverpct
           b-oe-ordl.under-pct     = prmUnderpct
           b-oe-ordl.job-no        = prmJob
           b-oe-ordl.job-no2       = prmJob2
           b-oe-ordl.s-man[1]      = prmSman  
           b-oe-ordl.s-pct[1]      = prmSales1
           b-oe-ordl.s-comm[1]     = prmComm1
           b-oe-ordl.q-no          = prmQuote

           b-oe-ordl.est-no        = xest.est-no
           b-oe-ordl.cust-no       = eb.cust-no 
           b-oe-ordl.part-dscr1    = eb.part-dscr2
           b-oe-ordl.i-name        = eb.part-dscr1
           b-oe-ordl.part-no       = eb.part-no
           /*b-oe-ordl.price         = prmPrice
           b-oe-ordl.qty           = eb.eqty */
           b-oe-ordl.pr-uom        = "M"
           b-oe-ordl.est-type      = xest.est-type
           .
       
           /* ASSIGN
             
                b-oe-ordl.qty           = eb.eqty 
                prmQty                  = eb.eqty
                  .*/
       
          
        ASSIGN
            fil_id = RECID(b-oe-ordl).

        FIND FIRST cust WHERE 
            cust.cust-no EQ b-oe-ordl.cust-no AND cust.company = cocode USE-INDEX cust NO-LOCK NO-ERROR.
        IF AVAIL cust THEN
        ASSIGN
            b-oe-ordl.disc  = cust.disc
            b-oe-ordl.tax   = cust.sort EQ "Y" AND oe-ord.tax-gr NE "" .
       
           IF vEstType = 2 OR vEstType = 6 THEN DO:
                    IF eb.set-is-assembled = TRUE THEN  ASSIGN isset = "Yes".
                    IF eb.set-is-assembled = FALSE THEN  ASSIGN isset = "No".
                    IF eb.set-is-assembled = ? THEN  ASSIGN isset = "Yes".
                    IF fgitem = "" THEN  ASSIGN fgitem  = eb.stock-no.
           END.
       
           RUN est/getcscnt.p (ROWID(eb),
                         OUTPUT li-cnt,OUTPUT li-cases).
           ASSIGN
               b-oe-ordl.cas-cnt      = li-cnt
               b-oe-ordl.cases-unit = li-cases 
               b-oe-ordl.partial    = b-oe-ordl.qty MOD b-oe-ordl.cas-cnt.
       
       IF fgitem = "" THEN DO:

            find first xeb  where xeb.company = prmComp 
               and xeb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) and xeb.form-no  eq 0
               no-lock no-error.
           
           if v-est-fg THEN
                   b-oe-ordl.i-no = if vEstType eq 2 and avail xeb then
                       xeb.part-no else eb.part-no.   
             else
                 if v-est-fg1 ne "Manual" then do:
                     find first itemfg
                         where itemfg.company eq cocode
                         and itemfg.part-no eq (if vEstType eq 2 and avail xeb then
                             xeb.part-no else eb.part-no)
                         and itemfg.cust-no eq eb.cust-no
                         no-lock no-error.
                      if avail itemfg then
                          assign
                          b-oe-ordl.i-no       = itemfg.i-no
                          b-oe-ordl.part-dscr2 = itemfg.part-dscr2.
                      end.
               
               IF v-est-fg1 EQ "Hughes" THEN
                   RUN fg/hughesfg.p (ROWID(eb), OUTPUT fgitem ).
                   ELSE
                       IF v-est-fg1 EQ "Fibre" THEN
                           RUN fg/fibre-fg.p (ROWID(eb), OUTPUT fgitem).
                        
                        run crt-eb-itemfg (fgitem, prmUom) NO-ERROR.
                       
                     IF (vEstType EQ 2 OR vEstType = 6) THEN DO:
                           FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = xest.est-no AND eb.form-no = 0  EXCLUSIVE-LOCK NO-ERROR.
                            IF AVAIL eb THEN DO:
                                ASSIGN
                                    eb.stock-no = fgitem .
                                
                            END.
                        END.
                        ELSE DO:
                            FIND CURRENT eb EXCLUSIVE-LOCK.
                            IF AVAIL eb THEN DO:
                                ASSIGN
                                    eb.stock-no = fgitem .
                                
                             END.
                        END.
                           
           END.

           
                   ASSIGN b-oe-ordl.i-no = fgitem
                       .
            

        v-tmp-price = if b-oe-ordl.pr-uom begins "L" AND b-oe-ordl.pr-uom NE "LB" then
                   if b-oe-ordl.qty lt 0 then -1 else 1
                 else
                 if b-oe-ordl.pr-uom eq "CS" then
                   b-oe-ordl.qty / (if prmCas ne 0 THEN prmCas else
                                    if  prmCas ne 0
                                                   then prmCas else
                                                        1)
                 else
                 if b-oe-ordl.pr-uom eq "C" then
                    b-oe-ordl.qty / 100
                 else
                 if  b-oe-ordl.pr-uom eq "M" then
                    b-oe-ordl.qty / 1000
                 else
                    b-oe-ordl.qty .
                            
    lv-t-price = v-tmp-price * b-oe-ordl.price  .
    prmTPrice = (lv-t-price - ROUND(lv-t-price * cust.disc / 100,2)).
    b-oe-ordl.t-price         = prmTPrice.

       FIND FIRST itemfg WHERE itemfg.company = prmComp AND
            itemfg.i-no = b-oe-ordl.i-no  EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL itemfg THEN DO:
            ASSIGN
                itemfg.q-ono = itemfg.q-ono + prmQty 
                itemfg.q-alloc =  itemfg.q-alloc + prmQty .
                
            END.
            ASSIGN
                    oe-ord.t-weight = oe-ord.t-weight - b-oe-ordl.t-weight
                    b-oe-ordl.t-weight = ( b-oe-ordl.qty / 100 ) * itemfg.weight-100
                    oe-ord.t-weight = oe-ord.t-weight + b-oe-ordl.t-weight.

            FIND FIRST po-ordl NO-LOCK
                WHERE po-ordl.company   EQ b-oe-ordl.company
                AND po-ordl.i-no      EQ b-oe-ordl.i-no
                AND po-ordl.po-no     EQ b-oe-ordl.po-no-po
                AND po-ordl.item-type EQ NO
                USE-INDEX item-ordno NO-ERROR.

            IF AVAIL po-ordl THEN DO:
                ASSIGN
                lv-uom       = po-ordl.cons-uom
                b-oe-ordl.cost = po-ordl.cons-cost.
               
                
            END.
            ELSE DO:
                ASSIGN
                    lv-uom       = itemfg.prod-uom
                    b-oe-ordl.cost = itemfg.total-std-cost.
             END.

                IF lv-uom NE "M" THEN DO:
                    RUN sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                                  b-oe-ordl.cost, OUTPUT ttcost).
                 ASSIGN
                     b-oe-ordl.cost = ttcost .
                 END.
                  
                 find first xest where xest.company eq cocode and
                   xest.est-no eq lv-est-no no-lock no-error.
               
               RUN itemfg-cost. 
               ASSIGN v-rel = b-oe-ordl.rel .
               RUN get-est-cost (lv-est-no).
                 

            RUN oe/ordlfrat.p (ROWID(b-oe-ordl), OUTPUT tfright ).
            
                ASSIGN
                    b-oe-ordl.t-freight = tfright
                    oe-ord.t-freight = oe-ord.t-freight + tfright.

               
                 RUN oe/ordfrate.p (ROWID(oe-ord)).
                     
                FIND xoe-ord WHERE RECID(xoe-ord) EQ save_id  NO-LOCK NO-ERROR.
                 IF AVAIL xoe-ord THEN RUN oe/oe-comm.p.
                
                 RUN oe/calcordt.p (ROWID(xoe-ord)).
                 FIND FIRST cust WHERE  cust.cust-no EQ b-oe-ordl.cust-no AND cust.company = b-oe-ordl.company USE-INDEX cust NO-LOCK NO-ERROR.
                  RUN oe/creditck.p (ROWID(cust), YES).  

               
   END. /*if avail eb*/
 
 /******************************est not blank********************************/

IF vEstType <> 2 AND vEstType <> 6  THEN DO:
    ASSIGN
        lv-qty = prmQty.
   
     FIND FIRST ce-ctrl where (ce-ctrl.company = cocode and 
       ce-ctrl.loc     = locode)  NO-LOCK NO-ERROR.

    find first xest where xest.company eq cocode
                  and xest.est-no  =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
                NO-LOCK no-error.
 assign
   j         = 1
   v-job-no  = if oe-ord.job-no <> "" then oe-ord.job-no else string(oe-ord.ord-no)
   v-job-no2 = oe-ord.job-no2 
   v-est-type = xest.est-type - IF xest.est-type GT 4 THEN 4 ELSE 0 .
 
  find first xeb
      where xeb.company = cocode 
        and xeb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
        and xeb.form-no  eq 0
      no-lock no-error.     


  for each eb
      where eb.company = prmComp 
        and eb.est-no  eq xest.est-no
        and eb.form-no  ne 0
        and eb.blank-no ne 0
        AND TRIM(eb.cust-no) NE ""
      no-lock,
      FIRST ef OF eb NO-LOCK,
      FIRST cust NO-LOCK
      where (cust.company = cocode)
        AND cust.cust-no eq eb.cust-no
      USE-INDEX cust    
      break by eb.est-no by eb.cust-no by eb.form-no by eb.blank-no
      TRANSACTION:

      
        FIND xoe-ord WHERE RECID(xoe-ord) EQ save_id .
        
       find first oe-ordl where oe-ordl.company  eq cocode
          and oe-ordl.ord-no   eq xoe-ord.ord-no
          and oe-ordl.cust-no  eq xoe-ord.cust-no
          and (oe-ordl.part-no eq eb.part-no
           or  (oe-ordl.i-no   eq eb.stock-no and eb.stock-no ne ""))
          and (v-est-type eq 4 or
               can-find(first tt-oe-ordl where tt-oe-ordl.row-id eq rowid(oe-ordl)))
        no-error.
   
    if not avail oe-ordl                                                    or
       can-find(first tt-oe-ordl where tt-oe-ordl.row-id eq rowid(oe-ordl)) then do:
      if not avail oe-ordl then do:
        li-line-no = 1.
        FOR EACH oe-ordl
            WHERE oe-ordl.company EQ xoe-ord.company
              AND oe-ordl.ord-no  EQ xoe-ord.ord-no
            BY oe-ordl.line DESC:
          li-line-no = oe-ordl.line + 1.
          LEAVE.
        END.
        create oe-ordl.
        assign
         oe-ordl.company    = cocode
         oe-ordl.ord-no     = xoe-ord.ord-no  /* input screen-value */
         oe-ordl.line       = li-line-no
         oe-ordl.po-no      = xoe-ord.po-no
         oe-ordl.job-no     = v-job-no
         oe-ordl.job-no2    = v-job-no2
         oe-ordl.req-code   = xoe-ord.due-code
         oe-ordl.prom-code  = xoe-ord.due-code
         oe-ordl.req-date   = xoe-ord.due-date
         oe-ordl.prom-date  = xoe-ord.due-date
         oe-ordl.i-no       =  eb.stock-no
         oe-ordl.qty        = if v-est-type eq 8 or v-est-type eq 4 THEN eb.bl-qty ELSE 0
         /*oe-ordl.qty        =  eb.bl-qty */
         v-qty-mod          = YES
         oe-ordl.over-pct   = xoe-ord.over-pct
         oe-ordl.under-pct  = xoe-ord.under-pct
         .
   

        IF oe-ordl.i-no EQ "0" THEN oe-ordl.i-no = "".

        if xoe-ord.est-no ne "" then
          assign
           oe-ordl.est-no = xoe-ord.est-no
           oe-ordl.pr-uom = "M".

        do i = 1 to 3:
          assign oe-ordl.s-man[i] = xoe-ord.sman[i]
                 oe-ordl.s-pct[i] = xoe-ord.s-pct[i]
                 oe-ordl.s-comm[i] = xoe-ord.s-comm[i].
        end.

        IF v-foamdate-log                                         AND
           CAN-FIND(FIRST style WHERE style.company EQ eb.company
                                  AND style.style   EQ eb.style
                                  AND style.type    EQ "F")       THEN
          oe-ordl.req-date = xoe-ord.ord-date + v-foamdate-int.
      end.  /* not avail oe-ordl */

     
      /*IF ll-new-fg THEN*/ ASSIGN  oe-ordl.i-no = eb.stock-no.

     
      if oe-ordl.i-no eq "" THEN DO:
       /* if v-est-fg THEN
          oe-ordl.i-no = if v-est-type eq 2 and avail xeb then
                           xeb.part-no else eb.part-no.   

        else*/
        if v-est-fg1 ne "Manual" then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.part-no eq (if v-est-type eq 2 and avail xeb then
                                         xeb.part-no else eb.part-no)
                and itemfg.cust-no eq eb.cust-no
              no-lock no-error.
          if avail itemfg then
            assign
             oe-ordl.i-no       = itemfg.i-no
             oe-ordl.part-dscr2 = itemfg.part-dscr2.
        end.
        
        IF v-est-fg1 EQ "Hughes" THEN  RUN fg/hughesfg.p (IF vEstType EQ 2  AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb), OUTPUT fgitem).
        ELSE
        IF v-est-fg1 EQ "Fibre" THEN  RUN fg/fibre-fg.p ( IF vEstType EQ 2  AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb), OUTPUT fgitem).
             
           ASSIGN
              oe-ordl.i-no   = fgitem .
                
            END.
      END.

     
        /* FIND FIRST itemfg WHERE itemfg.company = prmComp AND itemfg.i-no = oe-ordl.i-no EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL itemfg THEN DO:
               
               /* ASSIGN
                    itemfg.q-ono = itemfg.q-ono + prmQty . 
                    itemfg.q-alloc =  itemfg.q-alloc + oe-ordl.qty  .*/   */

      ASSIGN
       oe-ordl.i-name     = eb.part-dscr1
       oe-ordl.part-no    = eb.part-no
       oe-ordl.part-dscr1 = eb.part-dscr2
       oe-ordl.est-type   = eb.est-type
       oe-ordl.form-no    = eb.form-no
       oe-ordl.blank-no   = eb.blank-no
       oe-ordl.cust-no    = xoe-ord.cust-no
       oe-ordl.disc       = cust.disc
       oe-ordl.tax        = cust.sort EQ "Y" AND xoe-ord.tax-gr NE "".

      {custom/shptotax.i xoe-ord.cust-no xoe-ord.sold-id oe-ordl.tax}

      RUN est/getcscnt.p ((IF xest.est-type EQ 6 AND
                              AVAIL xeb          AND
                              xeb.cas-no NE ""   THEN ROWID(xeb) ELSE ROWID(eb)),
                         OUTPUT oe-ordl.cas-cnt,OUTPUT oe-ordl.cases-unit).
                   
      ASSIGN
       oe-ordl.cases      = TRUNC(oe-ordl.qty / oe-ordl.cas-cnt,0)
       oe-ordl.partial    = oe-ordl.qty MOD oe-ordl.cas-cnt.

      IF xest.est-type EQ 6 AND
         AVAIL xeb          AND
         xeb.pur-man        THEN
        ASSIGN
         /*oe-ordl.cases-unit = xeb.cas-pal task# 05010610*/
         oe-ordl.unit-count = xeb.tr-cnt.
      ELSE
        ASSIGN
         /*oe-ordl.cases-unit = eb.cas-pal   task# 05010610*/
         oe-ordl.unit-count = eb.tr-cnt.

      find first itemfg where itemfg.company eq cocode
                          and itemfg.i-no    eq oe-ordl.i-no
          no-lock no-error.  
      IF NOT AVAIL itemfg THEN          
          run crt-itemfg (fgitem, prmUom) NO-ERROR.

      if avail itemfg then 
        assign
         oe-ordl.price      = itemfg.sell-price
         oe-ordl.pr-uom     = itemfg.sell-uom
         oe-ordl.part-dscr2 = itemfg.part-dscr2.

      oe-ordl.type-code = 
            STRING(AVAIL itemfg AND
                   CAN-FIND(FIRST b-oe-ordl
                            WHERE b-oe-ordl.company EQ itemfg.company
                              AND b-oe-ordl.i-no    EQ itemfg.i-no
                              AND b-oe-ordl.ord-no  LT oe-ordl.ord-no
                              AND ROWID(b-oe-ordl)  NE ROWID(oe-ordl)),"R/O").

      if v-est-type eq 8 or v-est-type eq 4 then do:
          
        find first blk where blk.id eq eb.part-no no-lock no-error.   
        
        if avail blk then do:
          if v-est-type eq 4 OR v-est-type eq 8 then do:
             ASSIGN
                v-blk-qty = 0
                v-tot-comm-2 = 0
                v-blk-qty-2 = 0.
            
            for each blk where blk.id eq eb.part-no no-lock,
                first xjob
                where xjob.form-no  eq blk.snum
                  and xjob.blank-no eq blk.bnum:
              
              IF v-full-cost AND eb.est-type EQ 8 THEN
              DO:
                 IF v-est-fg1 EQ "Fibre" THEN
                    RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).
                
                 ASSIGN
                    lv-sell-by = ce-ctrl.sell-by
                    lv-sell-by-ce-ctrl = ce-ctrl.sell-by
                    v-pct-2 = ce-ctrl.prof-mrkup
                    v-probe-comm = eb.comm.
                
                 RUN custom/combasis.p (cocode,
                                        eb.sman,
                                        cust.type,
                                        eb.procat,
                                        0,
                                        cust.cust-no,
                                        OUTPUT v-basis).
                
                 IF cust.markup NE 0 THEN
                    v-pct-2 = cust.markup.
                
                 IF NOT cecomm-log THEN v-probe-comm = 0.
                
                 IF ll-use-margin THEN                 /* Get Margin% */
                    RUN est/getsmanmtrx.p (ROWID(xest), "M",
                                           INPUT-OUTPUT v-probe-comm,
                                           INPUT-OUTPUT v-mp).
                
                 RUN custom/markup.p (ROWID(xeb),
                                      INPUT-OUTPUT lv-sell-by,
                                      INPUT-OUTPUT v-pct-2).
                
                 IF ll-use-margin THEN
                    v-pct-2 = v-mp.
                
                 v-qty-2 = IF eb.yrprice THEN blk.qyld ELSE blk.qreq.
                
                 IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN DO:
                    ASSIGN
                       v-board-cst = 0
                       t-blkqty = 0.
                
                    FOR EACH b-eb2 fields(form-no yrprice yld-qty bl-qty) NO-LOCK WHERE
                        b-eb2.company EQ xest.company AND
                        b-eb2.est-no  EQ xest.est-no AND
                        b-eb2.form-no EQ eb.form-no:
                        /* set total # of blanks on all forms */
                
                        t-blkqty[b-eb2.form-no] = t-blkqty[b-eb2.form-no] +
                                                  if b-eb2.yrprice THEN b-eb2.yld-qty ELSE b-eb2.bl-qty.
                    END.
                
                    FOR EACH b-blk WHERE b-blk.id EQ xeb.part-no,
                        FIRST b-ef2 NO-LOCK
                        WHERE b-ef2.company EQ xest.company
                          AND b-ef2.est-no  EQ xest.est-no
                          AND b-ef2.form-no EQ b-blk.snum,
                        EACH brd WHERE brd.form-no EQ b-ef2.form-no:
                   
                        v-board-cst = v-board-cst + (brd.cost-m * b-blk.pct * (t-blkqty[b-ef2.form-no] / 1000)).
                    END.
                   
                    v-board-cst = v-board-cst / (v-qty-2 / 1000).
                 END.
                
                 RUN custom/sellpric.p (lv-sell-by-ce-ctrl,
                                        lv-sell-by,
                                        v-basis,
                                        (IF lv-sell-by-ce-ctrl NE "B" AND
                                            lv-sell-by EQ "B" THEN v-board-cst
                                         ELSE blk.fact / (v-qty-2 / 1000)),
                                        (IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0
                                         ELSE (blk.cost / (v-qty-2 / 1000)) - (blk.fact / (v-qty-2 / 1000))),
                                        (IF ll-use-margin OR
                                            (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN 0
                                         ELSE v-comm-2),
                                        v-pct-2,
                                        OUTPUT v-sell-price,
                                        OUTPUT v-comm-2).
                 IF ll-use-margin OR
                    (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN
                    v-comm-2 = v-sell-price * v-probe-comm / 100.
              END. /*v-full-cost AND eb.est-type EQ 8*/
              ELSE
                  v-comm-2 = 0.

              assign
               v-tot-comm-2 = v-tot-comm-2 + v-comm-2
               oe-ordl.t-weight  = oe-ordl.t-weight + blk.fg-wt
               oe-ordl.t-freight = oe-ordl.t-freight +
                                     (blk.fg-wt$ * (blk.fg-wt / 100))
               v-blk-qty           = v-blk-qty + blk.qyld.

              IF eb.est-type EQ 4 OR eb.est-type EQ 8 THEN
                  v-blk-qty-2 = v-blk-qty-2 + (IF eb.yrprice THEN blk.qyld ELSE blk.qreq).
       
              if v-full-cost then
                 oe-ordl.cost = oe-ordl.cost + blk.cost.
               ELSE
               DO:
                 IF eb.est-type EQ 4 THEN
                 DO:
                    ASSIGN
                       blk-fact = 0
                       v-qty-3 = IF eb.yrprice THEN blk.qyld ELSE blk.qreq.
      
                    FOR EACH bf-blk:
                        blk-fact = blk-fact + bf-blk.fact.
                    END. 
      
                    v-cost-2 = blk.fact * (fac-tot / blk-fact).
                 END.
                 ELSE
                    oe-ordl.cost = oe-ordl.cost +
                                  ((xjob.lab + xjob.mat + xjob.voh + xjob.foh) *
                                    v-qty-3 / 1000).
               END.
             end.
      
             IF eb.est-type EQ 4 AND v-full-cost EQ NO THEN
                oe-ordl.cost = oe-ordl.cost + (v-cost-2 / (v-blk-qty-2 / 1000)).
             ELSE
             IF eb.est-type EQ 8 THEN
                oe-ordl.cost = (oe-ordl.cost / (v-blk-qty-2 / 1000)) + v-tot-comm-2.
             ELSE
                oe-ordl.cost = (oe-ordl.cost / (v-blk-qty / 1000)) + v-tot-comm-2.
        
           end.

          else do:
            assign
             oe-ordl.t-weight  = blk.fg-wt
             oe-ordl.t-freight = blk.fg-wt$ * (blk.fg-wt / 100).
             
            if v-full-cost then
              oe-ordl.cost = blk.cost -
                               (((blk.fg-wt / 100) * blk.fg-wt$)
                                  * (blk.qyld / xest.est-qty[1])).
                                   
            else do:
              find first xjob no-error.
              if avail xjob then
                oe-ordl.cost = xjob.lab + xjob.mat + xjob.voh + xjob.foh.
            end.  
          end.
        end.  /* avail blk */

        else do:
          message "NO BLANK AVAILABLE!!!" eb.part-no.
          hide message.
        end.
      end.  /* est-type = 3 or 4 */
    end.
        
    RUN get-est-cost2 (lv-est-no).

    if avail xest and v-quo-price-log then do:
      run oe/getqpric.p (recid(xest), oe-ordl.part-no, "", oe-ordl.qty,
                              input-output oe-ordl.price,
                              input-output oe-ordl.pr-uom,
                              OUTPUT lv-q-no).
      IF lv-q-no NE 0 THEN DO:
        RUN oe/ordlq-no.p (ROWID(oe-ordl), lv-q-no).
        FIND CURRENT oe-ordl NO-ERROR.
      END.
    END.

    oe-ordl.t-price = oe-ordl.price * oe-ordl.qty /
                      (IF oe-ordl.pr-uom EQ "C" THEN 100  ELSE
                       IF oe-ordl.pr-uom EQ "M" THEN 1000 ELSE 
                       IF oe-ordl.pr-uom = "L" THEN oe-ordl.qty ELSE 1).

    {oe/defwhsed.i oe-ordl}

    FIND FIRST tt-oe-ordl WHERE tt-oe-ordl.row-id EQ ROWID(oe-ordl) NO-ERROR.
    IF AVAIL tt-oe-ordl THEN tt-oe-ordl.to-be-deleted = NO.
         
    if v-est-type eq 2 then leave. /** 2pc box & Set headers **/

    IF oeestcom-log = YES THEN        
    DO:
       RELEASE probe.

       FIND FIRST sman WHERE
            sman.company EQ eb.company AND
            sman.sman EQ eb.sman
            NO-LOCK NO-ERROR.

       IF AVAIL sman AND sman.commbasis EQ "M" THEN
       DO:
          /*DEF VAR v-price-per-1000 AS DEC NO-UNDO.*/
          /*DEF VAR v-tmp-price AS DEC NO-UNDO.*/

          IF oe-ordl.pr-uom NE "M" THEN
          DO:
             FIND FIRST itemfg WHERE
                  itemfg.company EQ cocode AND
                  itemfg.i-no EQ oe-ordl.i-no
                  NO-LOCK NO-ERROR.

             assign
              v-tmp-price = if oe-ordl.pr-uom begins "L" AND
                               oe-ordl.pr-uom NE "LB" then
                               if oe-ordl.qty lt 0 then -1
                               else 1
                            else
                            if oe-ordl.pr-uom eq "CS" then
                               oe-ordl.qty / (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                                  if avail itemfg and itemfg.case-count ne 0 THEN
                                           itemfg.case-count ELSE 1)
                             else
                             if oe-ordl.pr-uom eq "C" then
                                oe-ordl.qty / 100
                             else
                                oe-ordl.qty
                  
              v-tmp-price = v-tmp-price * oe-ordl.price
              v-price-per-1000 = v-tmp-price / ( oe-ordl.qty / 1000).
          END.
          ELSE
             v-price-per-1000 = oe-ordl.price.
              
          IF NOT(xest.est-type EQ 4 OR xest.est-type EQ 8) THEN
          DO:
             FOR EACH probe WHERE
                 probe.company = xest.company and
                 probe.est-no = xest.est-no and
                 probe.probe-date ne ? and
                 probe.est-qty eq INT(oe-ordl.qty) AND
                 probe.sell-price EQ v-price-per-1000
                 NO-LOCK
                 BY probe.probe-date DESC
                 BY probe.probe-time DESC:
                 
                 LEAVE.
             END.
            
             IF NOT AVAIL probe THEN
                FOR EACH probe WHERE
                    probe.company = xest.company and
                    probe.est-no = xest.est-no and
                    probe.probe-date ne ? and
                    probe.est-qty eq INT(oe-ordl.qty)
                    NO-LOCK
                    BY probe.probe-date DESC
                    BY probe.probe-time DESC:
                    
                    LEAVE.
                END.
          END.
          ELSE
          DO:
             FOR EACH probe WHERE
                 probe.company = xest.company and
                 probe.est-no = xest.est-no and
                 probe.probe-date ne ? and
                 probe.sell-price EQ v-price-per-1000
                 NO-LOCK
                 BY probe.probe-date DESC
                 BY probe.probe-time DESC:
                 
                 LEAVE.
             END.
            
             IF NOT AVAIL probe THEN
                FOR EACH probe WHERE
                    probe.company = xest.company and
                    probe.est-no = xest.est-no and
                    probe.probe-date ne ?
                    NO-LOCK
                    BY probe.probe-date DESC
                    BY probe.probe-time DESC:
                    
                    LEAVE.
                END.
          END.

          IF AVAIL probe THEN
          DO:
             ASSIGN
                oe-ordl.s-comm[1] = probe.comm
                xoe-ord.s-comm[1] = oe-ordl.s-comm[1].
         
             RELEASE probe.
          END.
       END.
    /*END.   */

   

  end. /* each eb */
  
 FIND CURRENT xoe-ord EXCLUSIVE-LOCK .
   FOR EACH oe-ordl OF xoe-ord EXCLUSIVE-LOCK
          WHERE oe-ordl.est-no NE "" 
            AND NOT oe-ordl.is-a-component  , 
          EACH eb EXCLUSIVE
          WHERE eb.company EQ xoe-ord.company
            AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
            AND ((eb.cust-no EQ xoe-ord.cust-no AND
                  eb.part-no EQ oe-ordl.part-no) /*OR
                 eb.est-type EQ 2 OR
                 eb.est-type EQ 6*/  )
          TRANSACTION:
            
        IF AVAIL eb AND AVAIL oe-ordl  THEN DO:
            
          ASSIGN
          
           eb.stock-no   = oe-ordl.i-no.
         
          ASSIGN
               eb.ord-no = oe-ordl.ord-no. 
        END.
        
        RELEASE eb.
        
        ASSIGN  fil_id = recid(oe-ordl). 
        /*run oe/ordlup.p.  */

      END. /* each oe-ordl */


 END.  /*  est-type <> 6 or 2 */   

/*************************************************************************/


 PROCEDURE get-est-cost2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def input param ip-est-no as CHAR NO-UNDO.

   def var v-run-list as CHAR NO-UNDO.

   if ip-est-no ne "" and not avail xest then
        find first xest where xest.company eq cocode and
                   xest.est-no eq ip-est-no no-lock no-error.

   

   IF AVAIL xest THEN DO :
       
     find first xeb
          where xeb.company = g_company 
            and xeb.est-no = ip-est-no
            and xeb.part-no = oe-ordl.part-no
          no-lock no-error.
     IF AVAIL xeb THEN
     find first xef
          where xef.company = g_company 
            and xef.est-no = ip-est-no
            and (xef.form-no = xeb.form-no OR xeb.form-no = 0)
          no-lock no-error.

     /*oe-ordl.qty = 100.*/
     

     ASSIGN
        v-run-list = "ce/print4.p,ce/box/print42.p,ce/tan/print4.p," +
                     "ce/com/print4.p,cec/print4.p,cec/box/print42.p," +
                     "cec/tan/print4.p,cec/com/print4.p" 
        qty = INT(oe-ordl.qty)
        v-shared-rel = v-rel.

     
     


     IF AVAIL xeb AND AVAIL xef                                     AND
        xest.est-type NE 3                                         /* AND
        xest.est-type NE 4                                          AND
        xest.est-type NE 8          */                                
        /*AND (oe-ordl.qty NE qty OR DEC(oe-ordl.cost) EQ 0) */ THEN DO: 


         

        RUN VALUE(ENTRY(xest.est-type,v-run-list)).     

        oe-ordl.cost = ((IF v-full-cost THEN tt-tot ELSE ord-cost) /
                                           (INT(oe-ordl.qty) / 1000)).
                                                        

     END.
   END.

END PROCEDURE.
