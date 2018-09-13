/* --------------------------------------------- cec/box/pr42tots.p 02/96 JLF */
/* copy of com for 2 sheet boxes                                              */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

DEFINE SHARED VARIABLE cocode AS cha NO-UNDO.
DEFINE SHARED VARIABLE locode AS cha NO-UNDO.

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.
DEFINE SHARED BUFFER xop  FOR est-op.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEFINE SHARED VARIABLE qty              AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE v-do-gsa         LIKE do-gsa NO-UNDO.
DEFINE SHARED VARIABLE v-update-qty-gsa AS LOG       NO-UNDO.
DEFINE SHARED VARIABLE ld-gsa-brd       AS DECIMAL   NO-UNDO.
DEFINE SHARED VARIABLE ld-gsa-mat       AS DECIMAL   NO-UNDO.
DEFINE SHARED VARIABLE ld-gsa-lab       AS DECIMAL   NO-UNDO.

DEFINE        VARIABLE xxx              AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE j                AS INTEGER   NO-UNDO.
DEFINE        VARIABLE i                AS INTEGER   NO-UNDO.
DEFINE        VARIABLE z                AS INTEGER   NO-UNDO.
DEFINE        VARIABLE yyy              AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE fg-wt$           AS de        NO-UNDO.
DEFINE        VARIABLE fg-wt%           AS de        NO-UNDO.
DEFINE        VARIABLE qm               AS de        NO-UNDO .
DEFINE        VARIABLE v-yld            AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-sqft           AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-msf            AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-pct            AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-markup         AS DECIMAL   EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-cewhspct       AS LOG       NO-UNDO.
DEFINE        VARIABLE v-qty            AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-n-out          AS INTEGER   NO-UNDO.
DEFINE        VARIABLE ld-fg-rate       AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE ll-gsa-pct       AS LOG       NO-UNDO.
DEFINE        VARIABLE v-cust-no        AS CHARACTER NO-UNDO.
DEFINE        VARIABLE dFreightTemp     AS DECIMAL   NO-UNDO.

DEFINE BUFFER reftable-broker-pct FOR reftable.
DEFINE BUFFER reftable-pr         FOR reftable.

DEFINE VARIABLE vcarqty    AS DECIMAL NO-UNDO.
DEFINE VARIABLE vcarmsf    AS DECIMAL NO-UNDO.
DEFINE VARIABLE qmMclean2  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mclean2yld AS DECIMAL NO-UNDO.
DEFINE VARIABLE Mclean2Qty AS DECIMAL NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.
DEFINE VARIABLE isUnitized AS LOG NO-UNDO.


{cec/msfcalc.i}

DO TRANSACTION:
    {sys/inc/cewhschg.i}
    v-cewhspct = NOT cewhschg-cha BEGINS "$".
END.

ll-gsa-pct = CAN-FIND(FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "CEGSA"
    AND sys-ctrl.int-fld EQ 0).

FIND FIRST ce-ctrl WHERE ce-ctrl.company = cocode AND
    ce-ctrl.loc     = locode NO-LOCK NO-ERROR.

OUTPUT to value(outfile1) append .

fg-wt = 0.


FIND FIRST bf-eb WHERE bf-eb.company = xest.company AND
    bf-eb.est-no    = xest.est-no AND
    bf-eb.form-no = 0 NO-LOCK NO-ERROR.
isUnitized = AVAILABLE bf-eb AND bf-eb.pur-man.

FOR EACH car WHERE car.snum NE 0:
    FIND FIRST blk WHERE blk.id = car.id NO-ERROR.
    FIND FIRST xjob
        WHERE xjob.i-no EQ blk.id
        AND xjob.qty  EQ blk.qreq.

    ASSIGN
        ld-fg-rate = IF blk.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate
        blk.lab    = blk.lab  + (car.qty / 100 * ld-fg-rate)
        blk.cost   = blk.cost + (car.qty / 100 * ld-fg-rate)
        xjob.lab   = xjob.lab + (car.qty / 100 * ld-fg-rate)
        fg-wt$     = fg-wt$ + (car.qty / 100 * ld-fg-rate)
        fg-wt      = fg-wt + car.qty.

    FOR EACH eb FIELDS(form-no quantityPerSet) WHERE
        eb.company = xest.company AND
        eb.est-no = xest.est-no AND
        eb.form-no > 0 AND eb.form-no <= v-form-no        NO-LOCK:
        ASSIGN
            mclean2yld = IF eb.quantityPerSet LT 0 THEN -1 / eb.quantityPerSet ELSE eb.quantityPerSet
            mclean2Qty = mclean2Qty + (qtty[vmcl] * mclean2yld).
    END.

END.

FOR EACH car
    WHERE car.snum EQ v-form-no 
    OR NOT vmclean2
    /*        OR (car.snum EQ 0 /*AND v-form-no EQ xest.form-qty*/) */
    BREAK BY car.id:

    z = 0.
    FOR EACH eb FIELDS(form-no quantityPerSet) WHERE
        eb.company = xest.company AND
        eb.est-no = xest.est-no AND
        eb.part-no = car.id
        NO-LOCK:
        ASSIGN
            v-yld = IF eb.form-no EQ 0 THEN 1 ELSE
                     IF eb.quantityPerSet LT 0 THEN -1 / eb.quantityPerSet ELSE eb.quantityPerSet
            z     = z + (qtty[vmcl] * v-yld).
    END.
    FIND FIRST xeb WHERE xeb.company = xest.company AND
        xeb.est-no    = xest.est-no AND
        xeb.form-no  = car.snum   AND
        xeb.blank-no = car.bnum NO-LOCK NO-ERROR.
    FIND FIRST carrier WHERE carrier.company = cocode AND carrier.loc = locode
        AND carrier.carrier = car.carrier NO-LOCK NO-ERROR.
    IF AVAILABLE carrier THEN
        FIND FIRST carr-mtx 
            WHERE carr-mtx.company  EQ cocode
            AND carr-mtx.loc      EQ locode
            AND carr-mtx.carrier  EQ carrier.carrier
            AND carr-mtx.del-zone EQ car.dscr
            AND carr-mtx.del-zip EQ xeb.ship-zip
            NO-LOCK NO-ERROR.
    IF NOT AVAILABLE carr-mtx THEN 
        FIND FIRST carr-mtx
            WHERE carr-mtx.company  EQ cocode
            AND carr-mtx.loc      EQ locode
            AND carr-mtx.carrier  EQ carrier.carrier
            AND carr-mtx.del-zone EQ car.dscr
            NO-LOCK NO-ERROR.    

    IF v-cust-no EQ "" THEN
        v-cust-no = xeb.cust-no.

    ASSIGN 
        dFreightTemp = 0
        vcarqty = car.qty
        vcarmsf = car.msf.

    IF vmclean2 THEN 
    DO:
        
        IF car.snum = 0 AND AVAILABLE carrier AND carrier.chg-method EQ "W" THEN NEXT.
    END.
     
    IF xeb.fr-out-c NE 0 THEN
        dFreightTemp = dFreightTemp + (xeb.fr-out-c * (vcarqty / 100)).
    ELSE
        IF xeb.fr-out-m NE 0 THEN
            dFreightTemp = dFreightTemp + (xeb.fr-out-m * (z / 1000)).
        ELSE
            IF AVAILABLE carr-mtx THEN 
            DO:
                IF carrier.chg-method EQ "W" THEN
                DO i = 1 TO 10:
                    dFreightTemp = carr-mtx.rate[i] * vcarqty / 100.          
                    IF carr-mtx.weight[i] GE vcarqty THEN LEAVE.
                END.
        
                ELSE
                    IF carrier.chg-method EQ "P" THEN 
                    DO:
                        p-qty = 0.
                        /*##PN: cas temp-table only created with final form in set*/
                        /*##PN: cas is not usable since we need to get full freight cost*/  
                        /*##PN: so that we can pro-rate freight across all forms*/
                        IF isUnitized AND AVAILABLE bf-eb /*set header*/ THEN 
                        DO:
                            IF bf-eb.cas-pal NE 0 THEN 
                                p-qty = qty / (bf-eb.cas-pal * bf-eb.cas-cnt).
                        END.
                        ELSE 
                            FOR EACH cas
                                WHERE cas.typ EQ 3
                                AND cas.id  EQ xeb.part-no:
                                p-qty = p-qty + cas.qty.            
                            END.
                        DO i = 1 TO 10:
                            dFreightTemp = carr-mtx.rate[i] * p-qty.            
                            IF carr-mtx.weight[i] GE p-qty THEN LEAVE.
                        END.
                    END.
        
                    ELSE 
                    DO:  /*Ticket 20329 was only calculating the last form on a set with MSF calculation*/
                        DO i = 1 TO 10:
                            dFreightTemp = carr-mtx.rate[i] * vcarmsf.          
                            IF carr-mtx.weight[i] GE vcarmsf THEN LEAVE.
                        END.
                      
                    END.
        
                /*         find first bf-eb where bf-eb.company = xest.company and */
                /*                            bf-eb.est-no    = xest.est-no and    */
                /*                            bf-eb.form-no = 0 NO-LOCK NO-ERROR.  */
                /*         isUnitized = AVAIL bf-eb AND bf-eb.pur-man.             */
        

                /*##BL: Compare total rate vs. rate min rate * all shipments*/
                IF dFreightTemp LT carr-mtx.min-rate * rels[vmcl] THEN 
                    dFreightTemp = carr-mtx.min-rate  * rels[vmcl].
                IF isUnitized THEN dFreightTemp = dFreightTemp / xest.form-qty.
            
            END.
            IF vmclean2 THEN
                fr-tot = dFreightTemp. /*vmclean2 = SETPRINT=MCLEAN - separates totals per component*/
            ELSE IF car.snum NE 0 THEN 
                fr-tot = fr-tot + dFreightTemp.  /*SETPRINT=ASI - aggregates totals for set*/  
    
    /* wfk - 05251304 - 2 pc box, only include freight for form 0 */
    /*       find first bf-eb where bf-eb.company = xest.company and */
    /*                          bf-eb.est-no    = xest.est-no and    */
    /*                          bf-eb.form-no = 0 NO-LOCK NO-ERROR.  */
    /*       isUnitized = AVAIL bf-eb AND bf-eb.pur-man.             */
    /*Note This condition was change with ticket 20329 since this appears to include form 0 (header) msf when it shouldn't*/
    
    IF NOT (xest.est-type EQ 6
        AND avail(bf-eb) 
        AND bf-eb.pur-man = YES 
        AND bf-eb.set-is-assembled 
        AND bf-eb.stock-no EQ xeb.stock-no
        AND xeb.FORM-no NE 0) THEN      
        ASSIGN
            car.cost = car.cost + dFreightTemp
            fr-tot   = fr-tot + dFreightTemp.           
        
    FIND FIRST blk
        WHERE (blk.id = car.id AND blk.snum = car.snum AND blk.bnum = car.bnum)
        OR (blk.snum = xest.form-qty AND car.snum = 0)
        NO-ERROR.
    blk.sell = blk.sell + dFreightTemp . /* use sell for freight costs for now */
END.

IF fg-wt$ > 0 THEN PUT "Finished Goods Handling" fg-wt$ TO 80 SKIP.

ASSIGN
    op-tot[5] = op-tot[5] + fg-wt$
    v-qty     = 0
    .

IF xest.form-qty EQ 1 OR vmclean2 THEN
    FOR EACH eb FIELDS(quantityPerSet)
        WHERE eb.company EQ xest.company
        AND eb.est-no  EQ xest.est-no
        AND eb.form-no EQ v-form-no
        NO-LOCK:
        ASSIGN
            v-yld = IF eb.quantityPerSet LT 0 THEN -1 / eb.quantityPerSet ELSE eb.quantityPerSet
            v-qty = v-qty + (qtty[vmcl] * v-yld)     
            .

    END.

ELSE v-qty = qtty[vmcl].

IF vmclean THEN op-tot[4] = op-tot[4] / (v-qty / 1000).

PUT "TOTAL  OPERATIONS        " op-tot[3] FORMAT ">>>>9.99" TO 59
    op-tot[4] FORMAT ">>>>9.99" TO 68 op-tot[5] TO 80 SKIP(1).

IF vmclean THEN op-tot[4] = op-tot[4] * (v-qty / 1000).

{est/calcpcts.i xest}  

IF INDEX("SB",ce-ctrl.sell-by) EQ 0 THEN 
DO:
    /* mat */
    DO i = 1 TO 6:
        ctrl[9] = ce-ctrl.mat-pct[i] / 100.
        IF ce-ctrl.mat-cost[i] > dm-tot[5]  THEN LEAVE.
    END.
    /* lab */
    DO i = 1 TO 6:
        ctrl[10] = ce-ctrl.lab-pct[i] / 100.
        IF ce-ctrl.lab-cost[i] > op-tot[5]  THEN LEAVE.
    END.
    calcpcts.val[1] = ctrl[9] * 100.
END.

ASSIGN
    gsa-mat = ctrl[9]  * 100
    gsa-lab = ctrl[10] * 100
    gsa-com = ce-ctrl.comm-mrkup
    gsa-war = ctrl[1] * 100.

FIND FIRST cust WHERE
    cust.company EQ xest.company AND
    cust.cust-no EQ v-cust-no
    NO-LOCK NO-ERROR.

FIND FIRST probe    
      WHERE probe.company    EQ xest.company
        AND probe.est-no     EQ xest.est-no NO-LOCK NO-ERROR.
IF AVAIL probe THEN
      gsa-fm = int(probe.gsa-fm).
ELSE
    IF AVAILABLE cust AND cust.scomm NE 0 THEN
        gsa-fm = cust.scomm.
    ELSE
    DO:
         
          gsa-fm = ce-ctrl.broker-pct.

    END.

OUTPUT close.

RUN cec/gsa.p (ip-rowid, qtty[vmcl], rels[vmcl], INPUT YES,
    INPUT-OUTPUT v-update-qty-gsa,
    INPUT-OUTPUT ld-gsa-brd, INPUT-OUTPUT ld-gsa-mat, INPUT-OUTPUT ld-gsa-lab).
OUTPUT to value(outfile1) append .

ASSIGN
    ctrl[9]  = gsa-mat / 100
    ctrl[10] = gsa-lab / 100
    ctrl[1]  = gsa-war / 100
    ctrl[19] = gsa-fm / 100.

FIND FIRST xeb WHERE xeb.company = xest.company 
    AND xeb.est-no EQ xest.est-no
    AND xeb.form-no NE 0 NO-ERROR.

ASSIGN
    v-yld   = IF xeb.quantityPerSet LT 0 THEN -1 / xeb.quantityPerSet ELSE xeb.quantityPerSet
    qm      = qtty[vmcl] /* * (if vmclean2 then v-yld else 1) */ / 1000
    fac-tot = dm-tot[5] + op-tot[5] +
           tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3].

qmMclean2 = mclean2Qty.

IF xeb.chg-method EQ "P" AND ctrl[6] NE 0 THEN fac-tot = fac-tot + fr-tot.

IF ce-ctrl.sell-by EQ "B" THEN 
DO:
    FOR EACH xef WHERE xef.company = xest.company AND
        xef.est-no    EQ xest.est-no
        AND (xef.form-no EQ v-form-no OR (NOT vmclean2))  NO-LOCK,          
        FIRST eb WHERE eb.company = xest.company AND
        eb.est-no   EQ xef.est-no
        AND eb.form-no EQ xef.form-no
        NO-LOCK:

        RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).
          
        FIND FIRST style  WHERE style.company EQ cocode
            AND style.style   EQ eb.style
            NO-LOCK NO-ERROR.
        IF AVAILABLE style THEN v-markup[1] = v-markup[1] + style.royalty.

        FIND FIRST est-op WHERE est-op.company = xest.company 
            AND est-op.est-no EQ xest.est-no
            AND est-op.qty EQ v-op-qty
            AND est-op.s-num EQ xef.form-no
            AND est-op.line  GE 500
            NO-LOCK NO-ERROR.

        v-sqft = IF AVAILABLE est-op THEN est-op.num-sh
        ELSE (qtty[vmcl] * v-yld /
            (eb.num-up * v-n-out)).
             
        {sys/inc/roundup.i v-sqft}
     
        v-msf = v-msf +
            IF v-corr THEN
            ROUND(((xef.gsh-len * xef.gsh-wid) * .007) * v-sqft,0)
            ELSE
            ROUND(((xef.gsh-len * xef.gsh-wid) / 144) * v-sqft,0).
    END.
     
    v-msf = v-msf / 1000.
   
    {cec/sqftmrkp.i v-msf v-pct}
   
    v-markup[2] = (fac-tot - op-tot[7]) * v-pct / 100.
END.

FIND CURRENT calcpcts.

IF calcpcts.val[1] EQ 0 THEN calcpcts.val[2] = 0.

ASSIGN
    xxx             = dm-tot[5] - calcpcts.val[2] + tprep-mat + mis-tot[1] 
    ctrl2[9]        = xxx * ctrl[9]
    xxx             = op-tot[5] + tprep-lab + mis-tot[3]
    ctrl2[10]       = xxx * ctrl[10]
    calcpcts.val[2] = calcpcts.val[2] * calcpcts.val[1] / 100.

FIND CURRENT calcpcts NO-LOCK NO-ERROR.

IF v-cewhspct THEN
    ctrl2[1] = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10]) * ctrl[1].

ASSIGN
    ctrl2[13] = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10]) * ctrl[19]
    tt-tot    = dm-tot[5] + op-tot[5] + ctrl2[1] + ctrl2[13] +
            tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3] +
            calcpcts.val[2] + ctrl2[9] + ctrl2[10]
    ctrl2[4]  = 0
    ctrl2[5]  = 0.

IF ctrl[4] > 0 THEN 
DO:             /* set spec#1 */
    IF ctrl[4] <= 1
        THEN ctrl2[4] = (fac-tot + ctrl2[9] + ctrl2[10]) * ctrl[4].
    ELSE ctrl2[4] = ctrl[4].
END.
tt-tot = tt-tot + ctrl2[4].

IF ctrl[11] > 0 THEN 
DO:             /* set spec#2 */
    IF ctrl[11] <= 1
        THEN ctrl2[11] = fac-tot * ctrl[11].
    ELSE ctrl2[11] = ctrl[11].
END.
IF ctrl[12] > 0 THEN 
DO:             /* set spec#3 */
    IF ctrl[12] <= 1
        THEN ctrl2[12] = fac-tot * ctrl[12].
    ELSE ctrl2[12] = ctrl[12].
END.
   
IF ctrl[6] NE 0 THEN
    FOR EACH blk:
        blk.cost = blk.cost + blk.sell. 
        blk.sell = 0.
    END.

FOR EACH blk:
    FIND FIRST xeb NO-LOCK
        WHERE xeb.company  = xest.company
        AND xeb.est-no   = xest.est-no
        AND xeb.form-no  = blk.snum
        AND xeb.blank-no = blk.bnum
        NO-ERROR.

    ASSIGN
        blk.fact = blk.cost
        xxx      = blk.sell. /* xxx = 0 if freight already included! */

    IF (NOT vmclean) OR ctrl[16] NE 0 THEN
        blk.fact = blk.fact + ((blk.cost - blk.lab) * ctrl[9]) +
            (blk.lab * ctrl[10]).

    ASSIGN
        /* add material gsa amount to blk.lab */
        blk.sell = ((blk.cost - blk.lab) * ctrl[9])
        /* set blk.lab to labor gsa amount */
        blk.lab  = blk.lab * ctrl[10]
        /* add gsa's to blk.cost */
        blk.cost = blk.cost + blk.lab + blk.sell
        yyy      = blk.cost. /* yyy = total cost of blk */

    IF ctrl[1] NE 0 THEN  /* warehousing % */
        blk.cost = blk.cost + (yyy * ctrl[1]).

    blk.cost = blk.cost + xxx. /* add freight if not already done */

    IF ctrl[4] NE 0 THEN  /* special markup % */
        blk.cost = blk.cost + (yyy * ctrl[4]).
      
END.
   

FIND FIRST xeb WHERE xeb.company = xest.company 
    AND xeb.est-no EQ xest.est-no
    AND xeb.form-no NE 0 NO-ERROR.
                    
IF NOT vmclean THEN 
DO:

    DISPLAY SKIP(1)
        "   T  O  T  A  L  S                         Cost/M     MR $    Run $  Total Cost" SKIP
        "Direct Material"  dm-tot[5] / qm TO 50
        dm-tot[3] FORMAT ">>>>9.99" TO 59
        dm-tot[5] TO 80 SKIP WITH STREAM-IO NO-LABELS NO-BOX.
    PUT "Direct Labor" op-tot[5] / qm TO 50
        op-tot[3] FORMAT ">>>>9.99" TO 59
        op-tot[4] FORMAT ">>>>>9.99" TO 69
        op-tot[5] TO 80 FORMAT ">>>,>>9.99" SKIP.
                         
    IF tprep-mat NE 0 THEN 
    DO: 
        PUT
            "Prep.  Material" tprep-mat / qm TO 50 tprep-mat TO 80  SKIP.
        lin-count = lin-count + 1. 
    END.
    IF tprep-lab NE 0 THEN 
    DO: 
        PUT
            "Prep.  Labor   " tprep-lab / qm TO 50 tprep-lab TO 80  SKIP.
        lin-count = lin-count + 1. 
    END.
    IF mis-tot[1] NE 0 THEN 
    DO: 
        PUT
            "Misc.  Material" mis-tot[1] / qm TO 50 mis-tot[1] TO 80 SKIP.
        lin-count = lin-count + 1. 
    END.
    IF mis-tot[3] NE 0 THEN 
    DO: 
        PUT
            "Misc.  Labor   " mis-tot[3] / qm TO 50 mis-tot[3] TO 80 SKIP.
        lin-count = lin-count + 6. 
    END.

    IF xeb.chg-method EQ "P" AND ctrl[6] NE 0 AND fr-tot NE 0 THEN 
        PUT "Freight"       fr-tot / qm TO 50 fr-tot TO 80 SKIP.         
      
    PUT "DIRECT FACTORY COST" fac-tot / qm TO 50 fac-tot TO 80 SKIP.
      
    IF ctrl[14] = 1 AND ctrl[11] NE 0 THEN 
    DO:
        IF ctrl[11] > 0 THEN PUT
                ce-ctrl.spec-l[2] SPACE(1).
        IF ctrl[11] <= 1 THEN
            PUT STRING(ce-ctrl.spec-%[2] * 100,"->>9.99") + "%" TO 30.
        PUT ctrl2[11] / qm TO 50 ctrl2[11] TO 80 SKIP.
    END.
    IF ctrl[15] = 1 AND ctrl[12] NE 0 THEN 
    DO:
        IF ctrl[12] > 0  THEN PUT ce-ctrl.spec-l[3] SPACE(1).
        IF ctrl[12] <= 1 THEN
            PUT STRING(ce-ctrl.spec-%[3] * 100,"->>9.99") + "%" TO 30.
        PUT ctrl2[12] / qm TO 50 ctrl2[12] TO 80 SKIP.
    END.

    IF ctrl[16] NE 0 THEN 
    DO:

        IF calcpcts.val[2] NE 0 THEN 
        DO:

            PUT "GS&A Board".
            IF ll-gsa-pct THEN
                PUT STRING(calcpcts.val[1]) + "%" TO 30.
            PUT calcpcts.val[2] / qm TO 50
                calcpcts.val[2]      TO 80 SKIP.
        END.

        IF ctrl2[9] NE 0 THEN 
        DO:
            PUT "GS&A Material".
            IF ll-gsa-pct THEN
                PUT STRING(ctrl[9] * 100) + "%" TO 30.
            PUT ctrl2[9] / qm TO 50
                ctrl2[9]      TO 80 SKIP.
        END.

        IF ctrl2[10] NE 0 THEN 
        DO:
            PUT "GS&A Labor".
            IF ll-gsa-pct THEN
                PUT STRING(ctrl[10] * 100) + "%" TO 30.
            PUT ctrl2[10] / qm TO 50
                ctrl2[10]      TO 80 SKIP.
        END.

        fac-tot = fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10].
    END.

    PUT "TOTAL FACTORY COST"
        fac-tot / qm TO 50
        fac-tot      TO 80 SKIP.

    ASSIGN
        ord-cost = fac-tot + ctrl2[9] + ctrl2[10]
        tt-tot   = tt-tot + ctrl2[5].

    IF ctrl2[1] NE 0 THEN 
    DO:

        PUT "Warehousing" +
            IF v-cewhspct THEN (STRING(ctrl[1] * 100)  + "%") ELSE "" TO 30
            ctrl2[1] / qm TO 50
            ctrl2[1]      TO 80 SKIP.
    END.

    IF ctrl2[13] NE 0 THEN 
    DO:

        PUT "Broker Comm %" +
            string(ctrl[19] * 100)  + "%" TO 30
            ctrl2[13] / qm TO 50
            ctrl2[13]      TO 80 SKIP.
    END.

    IF xeb.chg-method EQ "P" AND ctrl[6] EQ 0 AND fr-tot NE 0 THEN 
    DO:
        PUT "Freight"       fr-tot / qm TO 50 fr-tot TO 80 SKIP.
        tt-tot = tt-tot + fr-tot.         
    END.

    IF ctrl2[4] > 0 THEN 
    DO:
        PUT ce-ctrl.spec-l[1].
        IF ctrl[4] <= 1 THEN
            PUT SPACE(1) STRING(ce-ctrl.spec-%[1]) + "%" TO 30.
        PUT ctrl2[4] / qm TO 50
            ctrl2[4] TO 80 SKIP.
    END.

    IF ctrl[16] NE 0 THEN 
    DO:
        IF calcpcts.val[2] NE 0 THEN 
        DO:

            PUT "GS&A Board".
            IF ll-gsa-pct THEN
                PUT STRING(calcpcts.val[1]) + "%" TO 30.
            PUT calcpcts.val[2] / qm TO 50
                calcpcts.val[2]      TO 80 SKIP.
        END.

        IF ctrl2[9] NE 0 THEN 
        DO:
            PUT "GS&A Material".
            IF ll-gsa-pct THEN
                PUT STRING(ctrl[9] * 100) + "%" TO 30.
            PUT ctrl2[9] / qm TO 50
                ctrl2[9]       TO 80 SKIP.
        END.

        IF ctrl2[10] NE 0 THEN 
        DO:
            PUT "GS&A Labor".
            IF ll-gsa-pct THEN
                PUT STRING(ctrl[10] * 100) + "%" TO 30.
            PUT ctrl2[10] / qm TO 50
                ctrl2[10]      TO 80 SKIP.
        END.

        tt-tot = tt-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10].
    END.
      
    IF v-markup[1] NE 0 THEN 
    DO:
        PUT "Style Markup"
            v-markup[1] / qm TO 48
            v-markup[1]      TO 80 SKIP.
        tt-tot = tt-tot + v-markup[1].
    END.
      
    IF v-markup[2] NE 0 THEN 
    DO:
        IF NOT vmclean2 THEN v-pct = v-markup[2] / fac-tot * 100.
         
        PUT "Board Markup" STRING(v-pct,"->>9.99") + "%" TO 30
            v-markup[2] / qm TO 48
            v-markup[2]      TO 80 SKIP.
        tt-tot = tt-tot + v-markup[2].
    END.
END.

ELSE 
DO:
    ASSIGN
        vmcl-desc = "Prep.  Material"
        vmcl-cost = tprep-mat / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 0}

    ASSIGN
        vmcl-desc = "Prep.  Labor"
        vmcl-cost = tprep-lab / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 1}

    ASSIGN
        vmcl-desc = "Misc.  Material"
        vmcl-cost = mis-tot[1] / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 2}

    ASSIGN
        vmcl-desc = "Misc.  Labor"
        vmcl-cost = mis-tot[3] / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 3}

    ASSIGN
        op-tot[5] = op-tot[5] - op-tot[6] - op-tot[7]
        fac-tot   = fac-tot   - op-tot[7]
        vmcl-desc = "Direct Material"
        vmcl-cost = dm-tot[5] / qm.

    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 4}

    ASSIGN
        vmcl-desc = "Direct Labor"
        vmcl-cost = op-tot[5] / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 5}

    ASSIGN
        vmcl-desc = "Variable Overhead"
        vmcl-cost = op-tot[6] / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 6}

    IF xeb.chg-method EQ "P" AND ctrl[6] NE 0 AND fr-tot NE 0 THEN 
    DO:
        ASSIGN
            vmcl-desc = "Freight"
            vmcl-cost = fr-tot / qm /*(IF vmclean2 THEN qmMclean2 ELSE qm)*/.  
       
        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 7}
    END.

    ASSIGN
        vmcl-desc = "DIRECT FACTORY COST"
        vmcl-cost = fac-tot / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 8}

    fac-tot2 = fac-tot.

    ASSIGN
        vmcl-desc = "Fixed Overhead"
        vmcl-cost = op-tot[7] / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 9}

    fac-tot2 = fac-tot2 + op-tot[7].
    IF ctrl[13] NE 0 THEN fac-tot2 = fac-tot2 + ctrl2[4].
    IF ctrl[14] NE 0 THEN fac-tot2 = fac-tot2 + ctrl2[11].
    IF ctrl[15] NE 0 THEN fac-tot2 = fac-tot2 + ctrl2[12].
    IF ctrl[18] NE 0 AND ce-ctrl.sell-by NE "B" THEN
        fac-tot2 = fac-tot2 + ctrl2[18].

    IF ctrl[13] = 1 THEN 
    DO:
        ASSIGN
            vmcl-desc = ce-ctrl.spec-l[1]
            vmcl-cost = ctrl2[4] / qm.

        IF ctrl[4] <= 1 THEN
            vmcl-desc = vmcl-desc + fill(" ",22 - length(TRIM(vmcl-desc))) +
                string(ce-ctrl.spec-%[1] * 100,"->>9.99%").
            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 10}
    END.

    IF ctrl[14] = 1 THEN 
    DO:
        ASSIGN
            vmcl-desc = ce-ctrl.spec-l[2]
            vmcl-cost = ctrl2[11] / qm.

        IF ctrl[11] <= 1 THEN
            vmcl-desc = vmcl-desc + fill(" ",22 - length(TRIM(vmcl-desc))) +
                string(ce-ctrl.spec-%[2] * 100,"->>9.99%").

            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 11}
    END.

    IF ctrl[15] = 1 THEN 
    DO:
        ASSIGN
            vmcl-desc = ce-ctrl.spec-l[3]
            vmcl-cost = ctrl2[12] / qm.

        IF ctrl[12] <= 1 THEN
            vmcl-desc = vmcl-desc + fill(" ",22 - length(TRIM(vmcl-desc))) +
                string(ce-ctrl.spec-%[3] * 100,"->>9.99%").

            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 12}
    END.

    IF ctrl[16] NE 0 THEN 
    DO:

        ASSIGN
            vmcl-desc = "GS&A Board"
            vmcl-cost = calcpcts.val[2] / qm
            fac-tot2  = fac-tot2 + calcpcts.val[2].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 13}

        IF ll-gsa-pct AND calcpcts.val[2] NE 0 THEN 
        DO:
            mclean.rec-type = "gsabrd".

            ASSIGN
                vmcl-desc = "    GS&A Board %"
                vmcl-cost = calcpcts.val[1].

            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 14}
            mclean.rec-type = "gsabrd".
        END.

        ASSIGN
            vmcl-desc = "GS&A Material"
            vmcl-cost = ctrl2[9] / qm
            fac-tot2  = fac-tot2 + ctrl2[9].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 15}

        IF ll-gsa-pct AND ctrl2[9] NE 0 THEN 
        DO:
            mclean.rec-type = "gsamat".

            ASSIGN
                vmcl-desc = "    GS&A Material %"
                vmcl-cost = ctrl[9] * 100.

            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 16}
            mclean.rec-type = "gsamat".
        END.

        ASSIGN
            vmcl-desc = "GS&A Labor"
            vmcl-cost = ctrl2[10] / qm
            fac-tot2  = fac-tot2 + ctrl2[10].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 17}

        IF ll-gsa-pct AND ctrl2[10] NE 0 THEN 
        DO:
            mclean.rec-type = "gsalab".

            ASSIGN
                vmcl-desc = "    GS&A Labor %"
                vmcl-cost = ctrl[10] * 100.

            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 18}
            mclean.rec-type = "gsalab".
        END.
    END.  
       
    IF ctrl[18] > 0 AND ce-ctrl.sell-by NE "B" THEN 
    DO:   /* Royalty */
        vmcl-desc = "Royalty".

        IF ctrl[18] <= 1 THEN
            ASSIGN
                vmcl-desc = vmcl-desc + fill(" ",22 - length(TRIM(vmcl-desc))) +
                      string(ctrl2[18] * 100,"->>9.99%")
                vmcl-cost = (ctrl2[18] * fac-tot).

        ELSE vmcl-cost = ctrl2[18].

        ASSIGN
/*            fac-tot2  = fac-tot2 + vmcl-cost - double counted above 26340*/
            vmcl-cost = vmcl-cost / qm.

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 19}
    END.

    ASSIGN
        vmcl-desc = "TOTAL FACTORY COST"
        vmcl-cost = fac-tot2 / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 20}

    ASSIGN
        ord-cost = fac-tot2
        tt-tot   = fac-tot2.

    IF ctrl2[1] NE 0 OR NOT v-cewhspct THEN 
    DO:
        ASSIGN
            vmcl-desc = "Warehousing"
            vmcl-cost = ctrl2[1] / qm 
            tt-tot    = tt-tot + ctrl2[1].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 21}
       
        IF v-cewhspct THEN 
        DO:
            ASSIGN
                mclean.rec-type = "warehousing"
                vmcl-desc       = "    Warehousing %"
                vmcl-cost       = ctrl[1] * 100.

            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 22}
            mclean.rec-type = "warehousing".
        END.
    END.

    IF ctrl2[13] NE 0 THEN 
    DO:
        ASSIGN
            vmcl-desc = "Broker Comm"
            vmcl-cost = ctrl2[13] / qm
            tt-tot    = tt-tot + ctrl2[13].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 23}
       
        ASSIGN
            mclean.rec-type = "broker"
            vmcl-desc       = "    Broker Comm %"
            vmcl-cost       = ctrl[19] * 100.

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 24}
        mclean.rec-type = "broker".
       
    END.

    IF xeb.chg-method EQ "P" AND ctrl[6] EQ 0 AND fr-tot NE 0 THEN 
    DO:
        ASSIGN
            vmcl-desc = "Freight"
            vmcl-cost = fr-tot / qm
            tt-tot    = tt-tot + fr-tot.
       
        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 25}
    END.

    IF ctrl[13] = 0 THEN 
    DO:
        vmcl-desc = ce-ctrl.spec-l[1].

        IF ctrl[4] <= 1 THEN
            ASSIGN
                vmcl-desc = vmcl-desc + fill(" ",22 - length(TRIM(vmcl-desc))) +
                      string(ce-ctrl.spec-%[1] * 100,"->>9.99%")
                ctrl2[4]  = fac-tot2 * ctrl[4].
        ELSE ctrl2[4] = ctrl[4].

        ASSIGN
            vmcl-cost = ctrl2[4] / qm
            tt-tot    = tt-tot + ctrl2[4].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 26}
    END.

    IF ctrl[14] = 0 THEN 
    DO:             /* set spec#2 */
        vmcl-desc = ce-ctrl.spec-l[2].

        IF ctrl[11] <= 1 THEN
            ASSIGN
                vmcl-desc = vmcl-desc + fill(" ",22 - length(TRIM(vmcl-desc))) +
                      string(ce-ctrl.spec-%[2] * 100,"->>9.99%")
                ctrl2[11] = fac-tot2 * ctrl[11].
        ELSE ctrl2[11] = ctrl[11].

        ASSIGN
            vmcl-cost = ctrl2[11] / qm
            tt-tot    = tt-tot + ctrl2[11].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 27}
    END.

    IF ctrl[15] = 0 THEN 
    DO:             /* set spec#3 */
        vmcl-desc = ce-ctrl.spec-l[3].

        IF ctrl[12] <= 1 THEN
            ASSIGN
                vmcl-desc = vmcl-desc + fill(" ",22 - length(TRIM(vmcl-desc))) +
                      string(ce-ctrl.spec-%[3] * 100,"->>9.99%")
                ctrl2[12] = fac-tot2 * ctrl[12].
        ELSE ctrl2[12] = ctrl[12].

        ASSIGN
            vmcl-cost = ctrl2[12] / qm
            tt-tot    = tt-tot + ctrl2[12].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 28}
    END.

    IF ctrl[16] EQ 0 THEN 
    DO:

        ASSIGN
            vmcl-desc = "GS&A Board"
            vmcl-cost = calcpcts.val[2] / qm
            tt-tot    = tt-tot + calcpcts.val[2].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 29}

        IF ll-gsa-pct AND calcpcts.val[2] NE 0 THEN 
        DO:
            ASSIGN
                mclean.rec-type = "gsabrd"
                vmcl-desc       = "    GS&A Board %"
                vmcl-cost       = calcpcts.val[1].

            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 30}
            mclean.rec-type = "gsabrd".
        END.

        ASSIGN
            vmcl-desc = "GS&A Material"
            vmcl-cost = ctrl2[9] / qm
            tt-tot    = tt-tot + ctrl2[9].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 31}

        IF ll-gsa-pct AND ctrl2[9] NE 0 THEN 
        DO:
            mclean.rec-type = "gsamat".

            ASSIGN
                vmcl-desc = "    GS&A Material %"
                vmcl-cost = ctrl[9] * 100.

            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 32}
            mclean.rec-type = "gsamat".
        END.

        ASSIGN
            vmcl-desc = "GS&A Labor"
            vmcl-cost = ctrl2[10] / qm
            tt-tot    = tt-tot + ctrl2[10].

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 33}

        IF ll-gsa-pct AND ctrl2[10] NE 0 THEN 
        DO:
            mclean.rec-type = "gsalab".

            ASSIGN
                vmcl-desc = "    GS&A Labor %"
                vmcl-cost = ctrl[10] * 100.

            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 34}
            mclean.rec-type = "gsalab".
        END.
    END.

    IF ctrl[18] EQ 0 AND ce-ctrl.sell-by NE "B" THEN 
    DO:   /* Royalty */
        vmcl-desc = "Royalty".

        IF ctrl[18] <= 1 THEN
            ASSIGN
                vmcl-desc = vmcl-desc + fill(" ",22 - length(TRIM(vmcl-desc))) +
                      string(ctrl2[18] * 100,"->>9.99%")
                vmcl-cost = (ctrl2[18] * fac-tot).

        ELSE vmcl-cost = ctrl2[18].

        ASSIGN
            tt-tot    = tt-tot + vmcl-cost
            vmcl-cost = vmcl-cost / qm.

        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 35}
    END.
     
    IF v-markup[1] NE 0 THEN 
    DO:
        ASSIGN
            vmcl-desc = "Style Markup"
            tt-tot    = tt-tot + v-markup[1]
            vmcl-cost = v-markup[1] / qm.
                
        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 36}
    END.
     
    IF v-markup[2] NE 0 THEN 
    DO:
        IF NOT vmclean2 THEN v-pct = v-markup[2] / fac-tot * 100.
         
        ASSIGN
            vmcl-desc = "Board Markup"
            tt-tot    = tt-tot + v-markup[2]
            vmcl-cost = v-markup[2] / qm.
        
        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 37}
        mclean.rec-type = "boardm".
       
        ASSIGN
            vmcl-desc = "    Board Markup %"
            vmcl-cost = v-pct.
        
        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 38}
        mclean.rec-type = "boardm".
    END.
END.
FIND FIRST ttCostHeader EXCLUSIVE-LOCK 
        WHERE ttCostHeader.estimateNo EQ xeb.est-no
        AND ttCostHeader.formNo EQ v-form-no
        AND ttCostHeader.blankNo EQ 0
        AND ttCostHeader.quantityMaster EQ qtty[vmcl]
        AND ttCostHeader.jobNo EQ cJobNo
        AND ttCostHeader.jobNo2 EQ iJobNo2
    NO-ERROR.
IF NOT AVAILABLE ttCostHeader THEN 
    FIND FIRST ttCostHeader EXCLUSIVE-LOCK 
        WHERE ttCostHeader.estimateNo EQ xeb.est-no
        AND ttCostHeader.formNo EQ v-form-no
        AND ttCostHeader.blankNo EQ xeb.blank-no
        AND ttCostHeader.quantityMaster EQ qtty[vmcl]
        AND ttCostHeader.jobNo EQ cJobNo
        AND ttCostHeader.jobNo2 EQ iJobNo2
    NO-ERROR.
    IF AVAILABLE ttCostHeader THEN DO: 
        ASSIGN 
            ttCostHeader.stdCostDirectMaterial =  dm-tot[5]
            ttCostHeader.stdCostDirectLabor =  op-tot[5]
            ttCostHeader.stdCostVariableOverhead = op-tot[6]
            ttCostHeader.stdCostPrepLabor = tprep-lab
            ttCostHeader.stdCostPrepMaterial =  tprep-mat
            ttCostHeader.stdCostMiscLabor =  mis-tot[3]
            ttCostHeader.stdCostMiscMaterial = mis-tot[1]
            ttCostHeader.stdCostDirectFactory = ttCostHeader.stdCostDirectLabor + 
                                                ttCostHeader.stdCostDirectMaterial + 
                                                ttCostHeader.stdCostVariableOverhead + 
                                                ttCostHeader.stdCostPrepLabor + 
                                                ttCostHeader.stdCostPrepMaterial + 
                                                ttCostHeader.stdCostMiscLabor +  
                                                ttCostHeader.stdCostMiscMaterial
            ttCostHeader.stdCostFixedOverhead =  op-tot[7]
            ttCostHeader.stdCostTotalFactory =  ttCostHeader.stdCostDirectFactory +
                                                ttCostHeader.stdCostFixedOverhead
            ttCostHeader.stdCostFreight = fr-tot
            ttCostHeader.stdCostGSALabor = ctrl2[10]
            ttCostHeader.stdCostGSAMaterial = ctrl2[9]
            ttCostHeader.stdCostWarehousing = ctrl2[1]
            ttCostHeader.stdCostBrokerComm = ctrl2[13]
            ttCostHeader.stdCostSpecial1 = ctrl2[4]
            ttCostHeader.stdCostSpecial2 = ctrl2[11]
            ttCostHeader.stdCostSpecial3 = ctrl2[12]
            ttCostHeader.stdCostGSABoard = calcpcts.val[2] 
            ttCostHeader.stdCostTotalGSA = ttCostHeader.stdCostGSABoard + ttCostHeader.stdCostGSALabor + ttCostHeader.stdCostGSAMaterial
            ttCostHeader.stdCostTotalOther = ttCostHeader.stdCostFreight +  
                                      ttCostHeader.stdCostWarehousing +
                                      ttCostHeader.stdCostBrokerComm +
                                      ttCostHeader.stdCostSpecial1 +
                                      ttCostHeader.stdCostSpecial2 +
                                      ttCostHeader.stdCostSpecial3 +
                                      ttCostHeader.stdCostTotalGSA + 
                                      ttCostHeader.stdCostRoyalty
            .        
        IF ctrl[13] NE 0 /*include S1 in Factory Costs*/ THEN 
            ASSIGN 
                ttCostHeader.stdCostTotalFactory = ttCostHeader.stdCostTotalFactory + ttCostHeader.stdCostSpecial1
                ttCostHeader.stdCostTotalOther = ttCostHeader.stdCostTotalOther - ttCostHeader.stdCostSpecial1
                .
        IF ctrl[14] NE 0 /*include S2 in Factory Costs*/ THEN 
            ASSIGN 
                ttCostHeader.stdCostTotalFactory = ttCostHeader.stdCostTotalFactory + ttCostHeader.stdCostSpecial2
                ttCostHeader.stdCostTotalOther = ttCostHeader.stdCostTotalOther - ttCostHeader.stdCostSpecial2
                .
        IF ctrl[15] NE 0 /*include S3 in Factory Costs*/ THEN 
            ASSIGN 
                ttCostHeader.stdCostTotalFactory = ttCostHeader.stdCostTotalFactory + ttCostHeader.stdCostSpecial3
                ttCostHeader.stdCostTotalOther = ttCostHeader.stdCostTotalOther - ttCostHeader.stdCostSpecial3
                .
        IF ctrl[16] NE 0 /*include GSA in Factory Costs*/ THEN 
            ASSIGN 
                ttCostHeader.stdCostTotalFactory = ttCostHeader.stdCostTotalFactory + ttCostHeader.stdCostTotalGSA
                ttCostHeader.stdCostTotalOther = ttCostHeader.stdCostTotalOther - ttCostHeader.stdCostTotalGSA
                .
        IF ctrl[6] NE 0 /*include Freight in Factory Costs - Should be total but direct right now - 26330*/ THEN 
            ASSIGN 
                ttCostHeader.stdCostDirectFactory = ttCostHeader.stdCostDirectFactory + ttCostHeader.stdCostFreight
                ttCostHeader.stdCostTotalFactory = ttCostHeader.stdCostTotalFactory + ttCostHeader.stdCostFreight
                ttCostHeader.stdCostTotalOther = ttCostHeader.stdCostTotalOther - ttCostHeader.stdCostFreight
                .
        IF ctrl[18] NE 0 /*include Royalty in Factory Costs*/ THEN 
            ASSIGN 
                ttCostHeader.stdCostTotalFactory = ttCostHeader.stdCostTotalFactory + ttCostHeader.stdCostRoyalty
                ttCostHeader.stdCostTotalOther = ttCostHeader.stdCostTotalOther - ttCostHeader.stdCostRoyalty
                .
            
        
    END.
OUTPUT close.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
