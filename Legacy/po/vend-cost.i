/* po/vend-cost.i */
 
    DEFINE variable ip-calc-cost AS LOG NO-UNDO.  
    DEFINE VARIABLE fiCount      AS INTEGER  NO-UNDO .
    DEFINE VARIABLE v-basis-w    AS DECIMAL NO-UNDO. /* for po/po-adder2.p */
    DEFINE VARIABLE v-len        LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid        LIKE po-ordl.s-wid NO-UNDO.
    DEFINE VARIABLE v-dep        LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-adder      AS DECIMAL EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-ord-qty    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE fi_pb-qty    AS DECIMAL  NO-UNDO.

    DEFINE BUFFER b-cost  FOR reftable.
    DEFINE BUFFER b-qty   FOR reftable.
    DEFINE BUFFER b-setup FOR reftable.
    ASSIGN  
        v-qty              = 0
        v-cost             = 0
        v-pb-qty           = 0
        v-pb-stp           = 0
        v-pb-cst           = 0
        v-pb-cns           = 0
        v-save-qty         = 0
        v-setup            = 0
        li                 = 0
        lv-added-cost      = 0
        lv-added-cons-cost = 0
        lv-adder-setup     = 0
        lv-recid           = 0
        lv-t-cost          = 0
        ld-dim-charge      = 0
        v-index            = 0
        ip-calc-cost       = logical({1}) .

    EMPTY TEMP-TABLE tt-ei.
    EMPTY TEMP-TABLE tt-eiv.
    
        ASSIGN
            v-len = DEC(po-ordl.s-len)
            v-wid = DEC(po-ordl.s-wid) .
            {po/calc10.i v-len} .
            {po/calc10.i v-wid}.

        FIND FIRST ITEM NO-LOCK 
            WHERE item.company EQ cocode
            AND item.i-no    EQ po-ordl.i-no
            NO-ERROR.

        ASSIGN
            v-basis-w = IF AVAILABLE ITEM THEN item.basis-w ELSE 0
            v-dep     = IF AVAILABLE ITEM THEN item.s-dep ELSE 0.

       IF  v-len EQ ?  THEN ASSIGN v-len = 0 .
       IF  v-wid EQ ?  THEN ASSIGN v-wid = 0 .
       IF  v-dep EQ ?  THEN ASSIGN v-dep = 0 .
       IF  v-basis-w EQ ?  THEN ASSIGN v-basis-w = 0 .
    

    DO WITH FRAME {&FRAME-NAME}:
        /* for adders */
        RELEASE job-mat.
        FIND FIRST job NO-LOCK
            WHERE job.company EQ po-ordl.company
            AND job.job-no  EQ po-ordl.job-no
            AND job.job-no2 EQ INT(po-ordl.job-no2)
            NO-ERROR.
        IF AVAILABLE job THEN
            FIND FIRST job-mat NO-LOCK
                WHERE job-mat.company  EQ job.company
                AND job-mat.job      EQ job.job
                AND job-mat.job-no   EQ job.job-no
                AND job-mat.job-no2  EQ job.job-no2
                AND job-mat.frm      EQ INT(po-ordl.s-num)
                AND job-mat.blank-no EQ INT(po-ordl.b-num) 
                USE-INDEX seq-idx NO-ERROR.
        
        IF AVAILABLE job-mat THEN lv-recid = RECID(job-mat).
        v-ord-qty = DEC(po-ordl.ord-qty).
        IF po-ordl.item-type EQ TRUE THEN 
        DO:
            FIND FIRST e-item NO-LOCK
                WHERE e-item.company EQ cocode
                AND e-item.i-no    EQ po-ordl.i-no
                NO-ERROR.
      
            IF AVAILABLE e-item THEN 
            DO:
                CREATE tt-ei.
                ASSIGN 
                    tt-ei.std-uom = e-item.std-uom.
      
                FIND FIRST e-item-vend NO-LOCK
                    WHERE e-item-vend.company EQ e-item.company
                    AND e-item-vend.i-no    EQ e-item.i-no
                    AND e-item-vend.vend-no EQ po-ord.vend-no
                    NO-ERROR.
      
                IF AVAILABLE e-item-vend THEN 
                DO:
                    CREATE tt-eiv.
                    tt-eiv.rec_key = e-item-vend.rec_key.
                    DO v-index = 1 TO 10:
                        
                        ASSIGN
                            tt-eiv.run-qty[v-index]  = e-item-vend.run-qty[v-index]
                            tt-eiv.run-cost[v-index] = e-item-vend.run-cost[v-index]
                            tt-eiv.setups[v-index]   = e-item-vend.setups[v-index].
                    END.
                    FIND FIRST b-qty NO-LOCK WHERE
                        b-qty.reftable = "vend-qty" AND
                        b-qty.company = e-item-vend.company AND
                        b-qty.CODE    = e-item-vend.i-no AND
                        b-qty.code2   = e-item-vend.vend-no
                        NO-ERROR.
         
                    IF AVAILABLE b-qty THEN
                    DO:
                        FIND FIRST b-cost NO-LOCK WHERE
                            b-cost.reftable = "vend-cost" AND
                            b-cost.company = e-item-vend.company AND
                            b-cost.CODE    = e-item-vend.i-no AND
                            b-cost.code2   = e-item-vend.vend-no
                            NO-ERROR.
                        FIND FIRST b-setup NO-LOCK WHERE
                            b-setup.reftable = "vend-setup" AND
                            b-setup.company = e-item-vend.company AND
                            b-setup.CODE    = e-item-vend.i-no AND
                            b-setup.code2   = e-item-vend.vend-no
                            NO-ERROR.
             
                        DO v-index = 1 TO 10:
                            
                            ASSIGN
                                tt-eiv.run-qty[v-index + 10]  = b-qty.val[v-index]
                                tt-eiv.run-cost[v-index + 10] = b-cost.val[v-index]
                                tt-eiv.setups[v-index + 10]   = b-setup.val[v-index].
                        END.
                    END.
                END.
            END.
        END.
        ELSE 
        DO:
            FIND FIRST e-itemfg NO-LOCK
                WHERE e-itemfg.company EQ cocode
                AND e-itemfg.i-no    EQ po-ordl.i-no
                NO-ERROR.
            IF AVAILABLE itemfg THEN
                fiCount = (itemfg.case-count).
            IF AVAILABLE e-itemfg THEN 
            DO:
                CREATE tt-ei.
                ASSIGN 
                    tt-ei.std-uom = e-itemfg.std-uom.
                IF po-ordl.cust-no NE "" THEN
                    FIND FIRST e-itemfg-vend NO-LOCK
                        WHERE e-itemfg-vend.company EQ e-itemfg.company
                        AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                        AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                        AND e-itemfg-vend.cust-no EQ po-ordl.cust-no
                        NO-ERROR.
                /* gdm - 06040918 - check for vendor */
                IF NOT AVAILABLE e-itemfg-vend THEN
                    FIND FIRST e-itemfg-vend NO-LOCK
                        WHERE e-itemfg-vend.company EQ e-itemfg.company
                        AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                        AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                        NO-ERROR.
                /* gdm - check for blank vendor */
                IF NOT AVAILABLE e-itemfg-vend THEN
                    FIND FIRST e-itemfg-vend NO-LOCK
                        WHERE e-itemfg-vend.company EQ e-itemfg.company
                        AND e-itemfg-vend.i-no    EQ e-itemfg.i-no 
                        AND e-itemfg-vend.vend-no EQ "" NO-ERROR.
                IF AVAILABLE e-itemfg-vend THEN 
                DO:            
                    CREATE tt-eiv.
                    tt-eiv.rec_key = e-itemfg-vend.rec_key.
                    DO v-index = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[v-index]  = e-itemfg-vend.run-qty[v-index]
                            tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                            tt-eiv.setups[v-index]   = e-itemfg-vend.setups[v-index].
                    END.
                    RELEASE e-itemfg-vend.
                END.
            END.
        END. /* if item-type ne RM */
        
            FIND FIRST tt-eiv NO-LOCK NO-ERROR. 
            
        IF AVAILABLE tt-eiv THEN 
        DO:      
            ASSIGN
                v-cost = 0 /*DEC(po-ordl.cost:SCREEN-VALUE)*/
                v-qty  = DEC(po-ordl.ord-qty).
            
            IF tt-ei.std-uom NE po-ordl.pr-qty-uom          AND
                (po-ordl.item-type                                        OR
                LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
                LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0)  THEN 
            DO:
               IF po-ordl.pr-qty-uom EQ "CS" AND po-ordl.item-type NE TRUE THEN 
                DO:
                    /* First convert to EA */
                    v-qty = v-qty * INT(fiCount).
                    /* Now convert to std-uom */
                    RUN sys/ref/convquom.p("EA",
                        tt-ei.std-uom, v-basis-w,
                        v-len, v-wid, v-dep,
                        v-qty, OUTPUT v-qty).  
                END.
                ELSE
                    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                        tt-ei.std-uom, v-basis-w,
                        v-len, v-wid, v-dep,
                        v-qty, OUTPUT v-qty).
                    
            END.
            v-save-qty = v-qty.
            IF po-ordl.job-no NE "" THEN
                RUN po/groupcst.p (po-ordl.job-no,
                    INT(po-ordl.job-no2),
                    po-ordl.i-no,
                    INT(po-ordl.s-num),
                    INT(po-ordl.b-num),
                    INPUT-OUTPUT v-qty).  
            ASSIGN
                v-save-qty = v-qty - v-save-qty
                v-setup    = 0
                v-pb-qty   = 0.
            
            RUN est/dim-charge.p (tt-eiv.rec_key,
                v-wid,
                v-len,
                INPUT-OUTPUT ld-dim-charge).
     
            DO li = 1 TO EXTENT(tt-eiv.run-qty):
                IF tt-eiv.run-qty[li] LT v-qty THEN NEXT.
                
                ASSIGN
                    v-cost   = (tt-eiv.run-cost[li] + ld-dim-charge) * v-qty
                    v-setup  = tt-eiv.setups[li]
                    v-pb-qty = tt-eiv.run-qty[li] - v-save-qty.
                
                IF li LT EXTENT(tt-eiv.run-qty) THEN
                    ASSIGN
                        v-pb-cst = tt-eiv.run-cost[li + 1] + ld-dim-charge
                        v-pb-stp = tt-eiv.setups[li + 1].
                LEAVE.
            END.
            IF poqty-log THEN 
            DO:
                IF v-pb-qty GE 9999999 THEN v-pb-qty = 0.
                IF v-pb-qty EQ 0 THEN v-pb-cst = 0.
                ELSE 
                DO:
                    v-pb-qty = v-pb-qty + .001.
                    v-pb-cst = v-pb-cst * v-pb-qty.
                    IF v-pb-qty NE 0 THEN v-pb-cst = (v-pb-cst /*+ v-pb-stp*/) / v-pb-qty.  
                    ELSE v-pb-cst = (v-pb-cst /*+ v-pb-stp*/).
                END.
                IF tt-ei.std-uom NE po-ordl.pr-qty-uom           AND
                    (po-ordl.item-type                                        OR
                    LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
                    LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0)  THEN 
                DO:
                    IF po-ordl.pr-qty-uom EQ "CS" AND po-ordl.item-type NE TRUE THEN 
                    DO:
                        /* convert to EA */
                        RUN sys/ref/convquom.p(tt-ei.std-uom,
                            "EA",
                            v-basis-w, v-len, v-wid, v-dep,
                            v-pb-qty, OUTPUT v-pb-qty).
                        /* Then Convert to CS */
                        v-pb-qty = v-pb-qty / INT(fiCount).
                    END.
              
                    ELSE
                        RUN sys/ref/convquom.p(tt-ei.std-uom,
                            po-ordl.pr-qty-uom,
                            v-basis-w, v-len, v-wid, v-dep,
                            v-pb-qty, OUTPUT v-pb-qty).
                END.
                IF tt-ei.std-uom NE po-ordl.pr-uom           AND
                    (po-ordl.item-type                                    OR
                    LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
                    LOOKUP(po-ordl.pr-uom,fg-uom-list) EQ 0)  THEN 
                DO:
                    IF po-ordl.pr-uom EQ "CS" AND po-ordl.item-type NE TRUE THEN 
                    DO:
                        /* Convert to EA cost */
                        RUN sys/ref/convcuom.p(tt-ei.std-uom,
                            "EA", v-basis-w,
                            v-len, v-wid, v-dep,
                            v-pb-cst, OUTPUT v-pb-cst).
                        /* Convert to CS */
                        v-pb-cst = v-pb-cst * INT(fiCount).
                    END.
                    ELSE
                        RUN sys/ref/convcuom.p(tt-ei.std-uom,
                            po-ordl.pr-uom, v-basis-w,
                            v-len, v-wid, v-dep,
                            v-pb-cst, OUTPUT v-pb-cst).
                END.
                IF po-ordl.pr-uom NE po-ordl.cons-uom AND
                    (po-ordl.item-type                                      OR
                    LOOKUP(po-ordl.pr-uom,fg-uom-list)   EQ 0 OR
                    LOOKUP(po-ordl.cons-uom,fg-uom-list) EQ 0)     THEN 
                DO:
                    IF po-ordl.pr-uom EQ "CS" AND po-ordl.item-type NE TRUE THEN 
                    DO:
                        /* Convert Cases to EA */
                        v-pb-cst = v-pb-cst * INT(fiCount).
                        /* Convert EA to cons-uom */
                        RUN sys/ref/convcuom.p("EA",
                            po-ordl.cons-uom, v-basis-w,
                            v-len, v-wid, v-dep,
                            v-pb-cst, OUTPUT v-pb-cns).
                    END.
                    ELSE
                        RUN sys/ref/convcuom.p(po-ordl.pr-uom,
                            po-ordl.cons-uom, v-basis-w,
                            v-len, v-wid, v-dep,
                            v-pb-cst, OUTPUT v-pb-cns).
                END.
                fi_pb-qty = IF v-pb-qty LE 0 THEN 0 ELSE (v-pb-qty).
            END.
            /*assumes v-qty in same uom as v-cost*/
            IF v-qty <> 0 THEN v-cost = (v-cost /*+ v-setup*/) / v-qty.  
            ELSE v-cost = (v-cost /*+ v-setup*/).
            IF ip-calc-cost NE ? THEN 
            DO:
                IF ip-calc-cost THEN 
                DO:            
                    IF tt-ei.std-uom NE po-ordl.pr-uom           AND
                        (po-ordl.item-type                                    OR
                        LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
                        LOOKUP(po-ordl.pr-uom,fg-uom-list) EQ 0)  THEN 
                    DO:
                        /* IF 'CS' then convert to EA first */
                        RUN sys/ref/convcuom.p(tt-ei.std-uom,
                            IF po-ordl.pr-uom NE "CS" THEN
                            po-ordl.pr-uom ELSE "EA", 
                            v-basis-w,
                            (IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE v-len),
                            v-wid, v-dep,
                            v-cost, OUTPUT v-cost).
                        /* If cases, convert from EA to CS */
                        IF po-ordl.pr-uom EQ "CS" AND po-ordl.item-type NE TRUE THEN
                            v-cost = v-cost * INT(fiCount).
                    END.
                    ASSIGN
                        ip-calc-cost  = YES
                        po-ordl.cost  = (v-cost)
                        po-ordl.setup = (v-setup).
                    IF po-ordl.pr-uom NE po-ordl.cons-uom AND
                        (po-ordl.item-type                                      OR
                        LOOKUP(po-ordl.pr-uom,fg-uom-list)   EQ 0 OR
                        LOOKUP(po-ordl.cons-uom,fg-uom-list) EQ 0)     THEN 
                    DO:
                        /* Convert cost from CS to EA first */
                        IF po-ordl.pr-uom EQ "CS" AND po-ordl.item-type NE TRUE THEN
                            v-cost = v-cost / INT(fiCount).
           
                        RUN sys/ref/convcuom.p(IF po-ordl.pr-uom NE "CS" THEN
                            po-ordl.pr-uom ELSE "EA",
                            po-ordl.cons-uom, v-basis-w,
                            (IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE v-len),
                            v-wid, v-dep,
                            v-cost, OUTPUT v-cost).           
                    END.
                    
                    po-ordl.cons-cost = (v-cost).     
          
                END. /* if calc cost */
                
            END. /* ip calc cost ne ? */
        END. /* avail tt-eiv */
        IF poqty-log THEN 
        DO:
            IF CAN-DO("L,LOT",po-ordl.pr-uom) THEN
                lv-t-cost = (v-pb-cst + v-pb-stp) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.
            ELSE 
            DO:
                v-ord-qty = DEC(fi_pb-qty).
                IF po-ordl.pr-qty-uom NE po-ordl.pr-uom AND
                    (po-ordl.item-type                                        OR
                    LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 OR
                    LOOKUP(po-ordl.pr-uom,fg-uom-list)     EQ 0)     THEN
   
                    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                        po-ordl.pr-uom,
                        v-basis-w, v-len, v-wid, v-dep,
                        v-ord-qty, OUTPUT v-ord-qty).
     
                lv-t-cost = (v-ord-qty * v-pb-cst) + v-pb-stp.
            END.
            IF DEC(po-ordl.disc) NE 0 THEN
                lv-t-cost = lv-t-cost * (1 - (DEC(po-ordl.disc) / 100)).
            
        END.
        IF ip-calc-cost NE ? THEN 
        DO:
            IF CAN-DO("L,LOT",po-ordl.pr-uom) THEN
                lv-t-cost = (DEC(po-ordl.cost) +
                    DEC(po-ordl.setup)) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.
            ELSE 
            DO:
                v-ord-qty = DEC(po-ordl.ord-qty).
                IF po-ordl.pr-qty-uom NE po-ordl.pr-uom AND
                    (po-ordl.item-type                                        OR
                    LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 OR
                    LOOKUP(po-ordl.pr-uom,fg-uom-list)     EQ 0)     THEN 
                DO:
       
                    IF po-ordl.pr-qty-uom EQ "CS" OR po-ordl.pr-uom EQ "CS" THEN 
                    DO:
                       IF po-ordl.pr-qty-uom EQ "CS" THEN 
                        DO:
                
                            /* Convert from CS to EA */
                            v-ord-qty = v-ord-qty * INT(fiCount).
                
                            RUN sys/ref/convquom.p("EA",
                                po-ordl.pr-uom,
                                v-basis-w, v-len, v-wid, v-dep,
                                v-ord-qty, OUTPUT v-ord-qty).
                        END.
                        ELSE IF po-ordl.pr-uom EQ "CS" THEN 
                            DO:
                                /* Convert from whatever it was to EA */
                                RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                    "EA",
                                    v-basis-w, v-len, v-wid, v-dep,
                                    v-ord-qty, OUTPUT v-ord-qty).
                
                                /* convert from EA to CS */
                                v-ord-qty = v-ord-qty / INT(fiCount).
                
                            END.
                    END.
                    ELSE 
                        RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                            po-ordl.pr-uom,
                            v-basis-w, v-len, v-wid, v-dep,
                            v-ord-qty, OUTPUT v-ord-qty).
                END.
                lv-t-cost = (v-ord-qty * DEC(po-ordl.cost)) +
                    DEC(po-ordl.setup).
            END.
            IF DEC(po-ordl.disc) NE 0 THEN
                lv-t-cost = lv-t-cost * (1 - (DEC(po-ordl.disc) / 100)).
            po-ordl.t-cost = (lv-t-cost).
        END.
    END. 
