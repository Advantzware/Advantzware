/* fg/fg-post.i   in GUI  copied and changed */ 
/* Called from:
    fg/b-cons.w
    fg/fgpost.p
    fg/fgPostBatch.p
    fg/r-pstran.w
    oerep/r-BOLtag.w
    oerep/r-loadtg.w
    oerep/r-ltpost.p
    util/dev/impfgrcp.w
    In all cases '{1}' and '{2}' = w-fg-rctd (a temp-table defined in fg/fgPostBatch.i)
*/

RELEASE prod.
RELEASE po-ord.
RELEASE po-ordl.
RELEASE job.

DEFINE BUFFER b-itemfg-loc FOR itemfg-loc.
DEFINE VARIABLE deJobHdrQty AS DEC NO-UNDO.
DEFINE VARIABLE deFgActQty AS DEC NO-UNDO.

ASSIGN  
    v-newhdr = NO.
     
FIND FIRST prodl NO-LOCK WHERE 
    prodl.company EQ cocode AND 
    prodl.procat  EQ itemfg.procat AND 
    CAN-FIND(FIRST prod WHERE 
        prod.company EQ cocode AND 
        prod.prolin  EQ prodl.prolin)
    NO-ERROR.

IF AVAILABLE prodl THEN FIND FIRST prod NO-LOCK WHERE 
    prod.company EQ cocode AND 
    prod.prolin  EQ prodl.prolin
    NO-ERROR.

IF {1}.rita-code NE "E" 
AND {1}.rita-code NE "I" 
AND int({1}.po-no) NE 0 THEN FIND FIRST po-ord NO-LOCK WHERE 
    po-ord.company EQ cocode AND 
    po-ord.po-no   EQ int({1}.po-no)
    NO-ERROR.
          
IF AVAILABLE po-ord THEN FIND FIRST po-ordl NO-LOCK WHERE 
    po-ordl.company   EQ cocode AND 
    po-ordl.po-no     EQ po-ord.po-no AND 
    po-ordl.i-no      EQ {1}.i-no AND 
    po-ordl.deleted   EQ NO AND 
    po-ordl.item-type EQ NO AND 
    po-ordl.stat NE "C"
    NO-ERROR.
IF NOT AVAILABLE po-ordl THEN FIND FIRST po-ordl NO-LOCK WHERE 
    po-ordl.company   EQ cocode AND 
    po-ordl.po-no     EQ po-ord.po-no AND 
    po-ordl.i-no      EQ {1}.i-no AND 
    po-ordl.deleted   EQ NO AND 
    po-ordl.item-type EQ NO
    NO-ERROR.
  
IF "{1}" = "w-fg-rctd"
AND 
{1}.inv-no GT 0 THEN ASSIGN
    {1}.t-qty = {1}.inv-no
    {1}.inv-no = 0.

IF {1}.rita-code EQ "I" THEN DO:
    /* for return without job#*/
    IF {1}.job-no = "" THEN FOR EACH loadtag NO-LOCK WHERE 
        loadtag.company = g_company AND 
        loadtag.ITEM-type = NO AND 
        loadtag.tag-no = {1}.tag,
        EACH fg-bin NO-LOCK WHERE 
            fg-bin.company EQ cocode AND 
            fg-bin.i-no    EQ {1}.i-no AND 
            fg-bin.tag     EQ "" AND 
            fg-bin.qty     GE {2}.t-qty,
        EACH fg-rcpth NO-LOCK WHERE 
            fg-rcpth.company EQ cocode AND 
            fg-rcpth.i-no    EQ fg-bin.i-no AND 
            fg-rcpth.job-no  EQ fg-bin.job-no AND 
            fg-rcpth.job-no2 EQ fg-bin.job-no2
        BY fg-rcpth.trans-date
        BY fg-rcpth.r-no:
            
        IF NOT CAN-FIND (FIRST fg-rdtlh WHERE 
            fg-rdtlh.r-no      EQ fg-rcpth.r-no AND 
            fg-rdtlh.rita-code EQ fg-rcpth.rita-code AND 
            fg-rdtlh.loc       EQ fg-bin.loc AND 
            fg-rdtlh.loc-bin   EQ fg-bin.loc-bin AND 
            fg-rdtlh.tag       EQ fg-bin.tag) THEN 
            NEXT.
    
        ASSIGN
            {1}.rita-code = "T"
            {1}.job-no    = fg-bin.job-no
            {1}.job-no2   = fg-bin.job-no2
            {2}.loc2      = {2}.loc
            {2}.loc-bin2  = {2}.loc-bin
            {2}.tag2      = {2}.tag
            {2}.loc       = fg-bin.loc
            {2}.loc-bin   = fg-bin.loc-bin
            {2}.tag       = fg-bin.tag
            {2}.cust-no   = fg-bin.cust-no.
        LEAVE.
    END. /* {1}.job-no = ""*/
    ELSE FOR EACH fg-bin NO-LOCK WHERE 
        fg-bin.company EQ cocode AND 
        fg-bin.i-no    EQ {1}.i-no AND 
        fg-bin.job-no  EQ {1}.job-no AND 
        fg-bin.job-no2 EQ {1}.job-no2 AND 
        fg-bin.tag     EQ "" AND 
        fg-bin.qty     GE {2}.t-qty,
        EACH fg-rcpth NO-LOCK WHERE 
            fg-rcpth.company EQ cocode AND 
            fg-rcpth.i-no    EQ fg-bin.i-no AND 
            fg-rcpth.job-no  EQ fg-bin.job-no AND 
            fg-rcpth.job-no2 EQ fg-bin.job-no2
        BY fg-rcpth.trans-date
        BY fg-rcpth.r-no:

        IF NOT CAN-FIND(FIRST fg-rdtlh NO-LOCK WHERE 
            fg-rdtlh.r-no      EQ fg-rcpth.r-no AND 
            fg-rdtlh.rita-code EQ fg-rcpth.rita-code AND 
            fg-rdtlh.loc       EQ fg-bin.loc AND 
            fg-rdtlh.loc-bin   EQ fg-bin.loc-bin AND 
            fg-rdtlh.tag       EQ fg-bin.tag) THEN 
            NEXT.
        
        ASSIGN
            {1}.rita-code = "T"
            {2}.loc2      = {2}.loc
            {2}.loc-bin2  = {2}.loc-bin
            {2}.tag2      = {2}.tag
            {2}.loc       = fg-bin.loc
            {2}.loc-bin   = fg-bin.loc-bin
            {2}.tag       = fg-bin.tag
            {2}.cust-no   = fg-bin.cust-no.
        LEAVE.
    END.
END.
  
IF {1}.rita-code NE "I" THEN DO:
    /** Adjusting the Finish Good item quantitys **/
    IF {1}.rita-code EQ "R" THEN DO:    /** RECEIPTS **/
        ASSIGN 
            v-reduce-qty = 0.

        IF AVAILABLE po-ordl THEN DO:
            FIND CURRENT po-ordl EXCLUSIVE.
            IF DYNAMIC-FUNCTION("Conv_IsEAUOM",po-ordl.company, po-ordl.i-no, po-ordl.cons-uom) THEN
                v-reduce-qty = po-ordl.cons-qty.
            ELSE IF po-ordl.cons-qty NE 0 THEN
                RUN sys/ref/convquom.p (INPUT po-ordl.cons-uom,
                                        INPUT "EA", INPUT 0,
                                        INPUT po-ordl.s-len,
                                        INPUT po-ordl.s-wid,
                                        INPUT 0,
                                        INPUT po-ordl.cons-qty,
                                        OUTPUT v-reduce-qty).

            ASSIGN 
                v-overrun-qty     = v-reduce-qty * (1 - (po-ordl.over-pct / 100))
                v-underrun-qty    = v-reduce-qty * (1 - (po-ordl.under-pct / 100))
                po-ordl.t-rec-qty = po-ordl.t-rec-qty + {1}.t-qty.

            IF po-ordl.t-rec-qty GE v-underrun-qty THEN ASSIGN  
                v-reduce-qty = min(v-reduce-qty,v-reduce-qty - po-ordl.t-rec-qty + {1}.t-qty)
                po-ordl.stat = "C".       
            ELSE ASSIGN 
                po-ordl.stat = "P".
            IF v-reduce-qty LT 0 THEN ASSIGN 
                v-reduce-qty = 0.
            IF po-ordl.stat NE "C" THEN  ASSIGN 
                v-reduce-qty = min({1}.t-qty,v-reduce-qty). 
            ASSIGN 
                {1}.invoiced = YES.
            FIND CURRENT po-ordl NO-LOCK.
        END.   /* avail po-ordl */

        ELSE IF {1}.job-no NE "" THEN FIND FIRST job NO-LOCK WHERE 
            job.company EQ cocode AND 
            job.job-no  EQ {1}.job-no AND 
            job.job-no2 EQ {1}.job-no2 AND 
            CAN-FIND(FIRST job-hdr WHERE 
                        job-hdr.company EQ job.company AND 
                        job-hdr.job     EQ job.job AND 
                        job-hdr.job-no  EQ job.job-no AND 
                        job-hdr.job-no2 EQ job.job-no2)
            NO-ERROR.

        IF AVAILABLE job THEN DO:
            ASSIGN 
                v-est-no = job.est-no
                deJobHdrQty = 0
                deFgActQty = 0.
                      
            IF job.opened THEN FOR EACH job-hdr NO-LOCK WHERE 
                job-hdr.company EQ job.company AND 
                job-hdr.job     EQ job.job AND 
                job-hdr.job-no  EQ job.job-no AND 
                job-hdr.job-no2 EQ job.job-no2 AND 
                job-hdr.i-no    EQ {1}.i-no AND 
                job-hdr.qty     NE 0:
                ASSIGN 
                    deJobHdrQty = deJobHdrQty + job-hdr.qty.
            END.

            IF deJobHdrQty GT 0 THEN DO:
                FOR EACH fg-act NO-LOCK WHERE 
                    fg-act.company EQ job.company AND 
                    fg-act.job-no  EQ job.job-no AND 
                    fg-act.job-no2 EQ job.job-no2 AND 
                    fg-act.i-no    EQ {1}.i-no AND 
                    fg-act.qty     NE 0:
                    deFgActQty = deFgActQty + fg-act.qty.
                END.
                ASSIGN 
                    v-reduce-qty = deFgActQty + {2}.t-qty.
                IF v-reduce-qty GT deJobHdrQty THEN ASSIGN 
                    v-reduce-qty = {2}.t-qty - (v-reduce-qty - deJobHdrQty).
                ELSE ASSIGN              
                    v-reduce-qty = v-reduce-qty - deFgActQty.
            END.
 
            FIND FIRST sys-ctrl NO-LOCK WHERE 
                sys-ctrl.company EQ job.company AND 
                sys-ctrl.name    EQ "AUTOISSU"
                NO-ERROR.
                
            IF AVAILABLE sys-ctrl 
            AND sys-ctrl.char-fld EQ "FGPost" THEN DO:
                ASSIGN 
                    v-one-item = TRIM(job.est-no) NE "" AND
                                CAN-FIND(FIRST est WHERE 
                                    est.company   EQ job.company AND 
                                    est.est-no    EQ job.est-no AND 
                                    (est.est-type EQ 2 OR est.est-type EQ 6)).

                IF deJobHdrQty GT 0 THEN FOR EACH job-hdr NO-LOCK WHERE 
                    job-hdr.company EQ job.company AND 
                    job-hdr.job-no  EQ job.job-no AND 
                    job-hdr.job-no2 EQ job.job-no2 AND 
                    job-hdr.i-no    EQ {1}.i-no,
                    EACH job-mat NO-LOCK WHERE 
                        job-mat.company     EQ job-hdr.company AND 
                        job-mat.job         EQ job-hdr.job AND 
                        job-mat.job-no      EQ job-hdr.job-no AND 
                        job-mat.job-no2     EQ job-hdr.job-no2 AND 
                        (
                            (job-mat.frm EQ job-hdr.frm AND
                                (job-mat.blank-no EQ job-hdr.blank-no OR job-mat.blank-no EQ 0)
                            ) OR
                        v-one-item),
                    FIRST item NO-LOCK WHERE 
                        item.company EQ job-mat.company AND 
                        item.i-no    EQ job-mat.rm-i-no AND 
                        (NOT CAN-DO("C,D,5,6",item.mat-type) OR
                            NOT v-one-item)
                    BREAK BY job-mat.frm
                    BY job-mat.blank-no
                    BY job-mat.i-no
                    BY ROWID(job-mat):

                    IF FIRST-OF(job-mat.i-no) THEN DO:
                        IF CAN-DO("C,D,5,6",item.mat-type) THEN DO:
                            ASSIGN 
                                v-dec = {1}.cases.
                            FIND FIRST eb NO-LOCK WHERE 
                                eb.company EQ job-mat.company AND 
                                eb.est-no EQ job.est-no AND 
                                eb.form-no EQ job-mat.frm AND 
                                eb.blank-no EQ job-mat.blank-no
                                NO-ERROR.
                            IF AVAILABLE eb THEN DO:
                                IF ITEM.mat-type EQ "C" THEN DO:
                                    IF eb.spare-int-3 GT 0 THEN ASSIGN  
                                        v-dec = v-dec * eb.spare-int-3.
                                END.
                                IF ITEM.mat-type EQ "5" THEN DO:
                                    IF eb.spare-char-3 = "P" THEN ASSIGN 
                                        v-dec = v-dec / eb.cas-pal * eb.lp-up .
                                    ELSE ASSIGN 
                                        v-dec = v-dec * eb.lp-up .
                                END.
                                IF ITEM.mat-type EQ "6" THEN DO:
                                    IF eb.spare-char-4 = "P" THEN ASSIGN 
                                        v-dec = v-dec / eb.cas-pal * eb.div-up .
                                    ELSE ASSIGN 
                                        v-dec = v-dec * eb.div-up .
                                END.
                            END.
                            IF item.mat-type EQ "D" THEN ASSIGN
                                v-dec = v-dec / (IF {1}.cases-unit EQ 0 THEN 1 ELSE {1}.cases-unit)
                                v-dec = TRUNC(v-dec,0)
                                v-dec = v-dec + INT({1}.partial GT 0)
                                v-dec = v-dec - INT({1}.partial LT 0).
                                              
                            RUN jc/jc-autop.p (ROWID(job-mat), 0, v-dec).
                        END.
                        ELSE DO:
                            ASSIGN 
                                v-dec = {2}.t-qty / deJobHdrQty *
                                        (IF job-mat.blank-no EQ 0 THEN (job-hdr.sq-in / 100) ELSE 1). 
                            RUN jc/jc-autop.p (ROWID(job-mat), v-dec, 0).
                        END.
                    END.
                END.

                /* Case & Pallet Auto Issue for sets */
                IF v-one-item THEN FOR EACH reftable NO-LOCK WHERE 
                    reftable.reftable EQ "jc/jc-calc.p" AND 
                    reftable.company  EQ job.company AND 
                    reftable.loc      EQ "" AND 
                    reftable.code     EQ STRING(job.job,"999999999") AND 
                    reftable.code2    EQ {1}.i-no,
                    EACH job-mat NO-LOCK WHERE 
                        job-mat.company  EQ job.company AND 
                        job-mat.job      EQ job.job AND 
                        job-mat.job-no   EQ job.job-no AND 
                        job-mat.job-no2  EQ job.job-no2 AND 
                        job-mat.frm      EQ INT(reftable.val[12]) AND 
                        job-mat.blank-no EQ INT(reftable.val[13]),
                    FIRST item NO-LOCK WHERE 
                        item.company  EQ job.company AND 
                        item.i-no     EQ job-mat.rm-i-no AND 
                        CAN-DO("C,D,5,6",item.mat-type)
                    BREAK BY job-mat.frm
                    BY job-mat.blank-no
                    BY job-mat.i-no
                    BY ROWID(job-mat):

                    IF FIRST-OF(job-mat.i-no) THEN DO:
                        ASSIGN 
                            v-dec = {1}.cases.
                        IF item.mat-type EQ "5" OR ITEM.mat-type = "6"  THEN DO:
                            FIND FIRST eb NO-LOCK WHERE 
                                eb.company EQ job-mat.company AND 
                                eb.est-no EQ job.est-no AND 
                                eb.form-no EQ job-mat.frm AND 
                                eb.blank-no EQ job-mat.blank-no
                                NO-ERROR.
                            IF AVAILABLE eb THEN DO:
                                IF ITEM.mat-type EQ "5" THEN DO:
                                    IF eb.spare-char-3 = "P" THEN ASSIGN 
                                        v-dec = v-dec / eb.cas-pal * eb.lp-up .
                                    ELSE assign
                                        v-dec = v-dec * eb.lp-up .
                                END.
                                ELSE DO:
                                    IF eb.spare-char-4 = "P" THEN ASSIGN 
                                        v-dec = v-dec / eb.cas-pal * eb.div-up .
                                    ELSE ASSIGN 
                                        v-dec = v-dec * eb.div-up .
                                END.
                            END.
                        END.
                        IF item.mat-type EQ "D" THEN ASSIGN
                            v-dec = v-dec / (IF {1}.cases-unit EQ 0 THEN 1 ELSE {1}.cases-unit)
                            v-dec = TRUNC(v-dec,0)
                            v-dec = v-dec + INT({1}.partial GT 0)
                            v-dec = v-dec - INT({1}.partial LT 0).
                        RUN jc/jc-autop.p (ROWID(job-mat), 0, v-dec).
                    END.  /* FIRST-OF job-mat.i-no */
                END. /* FOR EACH reftable */  
            END.  /* AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "FGPost" */
        END.   /* AVAIL job */

        FIND CURRENT itemfg EXCLUSIVE.
        ASSIGN
            itemfg.q-prod     = itemfg.q-prod     + {2}.t-qty
            itemfg.q-prod-ptd = itemfg.q-prod-ptd + {2}.t-qty
            itemfg.q-prod-ytd = itemfg.q-prod-ytd + {2}.t-qty
            itemfg.q-onh      = itemfg.q-onh      + {2}.t-qty
            itemfg.q-ono      = itemfg.q-ono      - v-reduce-qty. 
        
        RUN fg/chkfgloc.p  (INPUT itemfg.i-no, 
                            INPUT {2}.loc).
        
        IF itemfg.def-loc EQ "" 
        AND itemfg.def-loc-bin EQ "" THEN ASSIGN 
            itemfg.def-loc     = {2}.loc
            itemfg.def-loc-bin = {2}.loc-bin.          
        IF itemfg.q-ono LT 0 THEN ASSIGN 
            itemfg.q-ono = 0.             

        FIND FIRST itemfg-loc EXCLUSIVE WHERE 
            itemfg-loc.company EQ itemfg.company AND 
            itemfg-loc.i-no    EQ itemfg.i-no AND 
            itemfg-loc.loc     EQ {2}.loc
            NO-ERROR.
        IF AVAILABLE itemfg-loc THEN DO:
            IF AVAILABLE job THEN FIND FIRST job-hdr NO-LOCK WHERE 
                job-hdr.company EQ job.company AND 
                job-hdr.job     EQ job.job AND 
                job-hdr.job-no  EQ job.job-no AND 
                job-hdr.job-no2 EQ job.job-no2 AND 
                job-hdr.i-no    EQ {1}.i-no
                NO-ERROR.
            IF {2}.po-no GT "" THEN FIND FIRST po-ordl NO-LOCK WHERE 
                po-ordl.company EQ {2}.company AND 
                po-ordl.po-no EQ INTEGER({2}.po-no) AND 
                po-ordl.i-no  EQ {2}.i-no
                NO-ERROR.
            IF AVAILABLE po-ordl THEN FIND po-ord NO-LOCK WHERE 
                po-ord.company = po-ordl.company AND 
                po-ord.po-no = po-ordl.po-no 
                NO-ERROR.
            ASSIGN
                itemfg-loc.q-prod     = itemfg-loc.q-prod     + {2}.t-qty
                itemfg-loc.q-prod-ptd = itemfg-loc.q-prod-ptd + {2}.t-qty
                itemfg-loc.q-prod-ytd = itemfg-loc.q-prod-ytd + {2}.t-qty
                itemfg-loc.q-onh      = itemfg-loc.q-onh      + {2}.t-qty.

            IF  (NOT AVAILABLE job-hdr OR (AVAILABLE(job-hdr) AND job-hdr.loc EQ {2}.loc)) 
            AND (NOT AVAILABLE po-ord  OR (AVAILABLE(po-ord)  AND po-ord.loc  EQ {2}.loc)) THEN ASSIGN 
                itemfg-loc.q-ono      = itemfg-loc.q-ono      - v-reduce-qty. 
            ELSE DO:     
                /* If job or po location was different, have to reduce q-ono for that location */
                IF AVAILABLE po-ord 
                AND po-ord.loc NE {2}.loc 
                AND AVAILABLE itemfg THEN FIND FIRST b-itemfg-loc EXCLUSIVE WHERE 
                    b-itemfg-loc.company EQ itemfg.company AND 
                    b-itemfg-loc.i-no    EQ itemfg.i-no AND 
                    b-itemfg-loc.loc     EQ po-ord.loc
                    NO-ERROR.
                ELSE IF AVAILABLE job-hdr 
                AND job-hdr.loc NE {2}.loc 
                AND AVAILABLE itemfg THEN FIND FIRST b-itemfg-loc EXCLUSIVE WHERE 
                    b-itemfg-loc.company EQ itemfg.company AND 
                    b-itemfg-loc.i-no    EQ itemfg.i-no AND 
                    b-itemfg-loc.loc     EQ job-hdr.loc
                    NO-ERROR.
   
                IF AVAILABLE b-itemfg-loc THEN ASSIGN 
                    b-itemfg-loc.q-ono = b-itemfg-loc.q-ono - v-reduce-qty.
            END.
            IF AVAILABLE itemfg-loc 
            AND itemfg-loc.q-ono LT 0 THEN ASSIGN 
                itemfg-loc.q-ono = 0.
        END.
        
        RUN fg/comp-upd.p ( RECID(itemfg), 
                            v-reduce-qty * -1, 
                            "q-ono",
                            v-est-no).

        IF {2}.pur-uom NE itemfg.prod-uom
        AND (NOT DYNAMIC-FUNCTION("Conv_IsEAUOM",itemfg.company, itemfg.i-no, {2}.pur-uom) OR
            NOT DYNAMIC-FUNCTION("Conv_IsEAUOM",itemfg.company, itemfg.i-no, itemfg.prod-uom))  THEN
            RUN sys/ref/convcuom.p ({2}.pur-uom, 
                                    itemfg.prod-uom, 
                                    0, 
                                    0, 
                                    0, 
                                    0,
                                    itemfg.last-cost, 
                                    OUTPUT itemfg.last-cost).

        IF AVAILABLE job THEN DO:
            FIND FIRST job-hdr NO-LOCK WHERE 
                job-hdr.company EQ job.company AND 
                job-hdr.job     EQ job.job AND 
                job-hdr.job-no  EQ job.job-no AND 
                job-hdr.job-no2 EQ job.job-no2 AND 
                job-hdr.i-no    EQ {1}.i-no
                NO-ERROR.
                    
            IF AVAILABLE job-hdr 
            AND (job-hdr.std-tot-cost GT 0 OR 
                job-hdr.std-mat-cost GT 0 OR 
                job-hdr.std-lab-cost GT 0 OR 
                job-hdr.std-fix-cost GT 0) THEN DO:
                IF itemfg.prod-uom EQ "EA" THEN ASSIGN 
                    itemfg.std-tot-cost = job-hdr.std-tot-cost / 1000
                    itemfg.std-mat-cost = job-hdr.std-mat-cost / 1000
                    itemfg.std-lab-cost = job-hdr.std-lab-cost / 1000
                    itemfg.std-fix-cost = job-hdr.std-fix-cost / 1000
                    itemfg.std-var-cost = job-hdr.std-var-cost / 1000
                    .
                ELSE ASSIGN 
                    itemfg.std-tot-cost = job-hdr.std-tot-cost
                    itemfg.std-mat-cost = job-hdr.std-mat-cost
                    itemfg.std-lab-cost = job-hdr.std-lab-cost
                    itemfg.std-fix-cost = job-hdr.std-fix-cost
                    itemfg.std-var-cost = job-hdr.std-var-cost
                    .
            END. /* AVAIL job-hdr */
        END. /* AVAIL job */
        ELSE DO:
            ASSIGN
                itemfg.std-mat-cost = {2}.ext-cost / {2}.t-qty.
            IF itemfg.prod-uom EQ "M" THEN ASSIGN
                    itemfg.std-mat-cost =  itemfg.std-mat-cost * 1000.
            ASSIGN 
                itemfg.std-tot-cost = itemfg.std-mat-cost.
        END. /* NOT AVAIL job */
        ASSIGN 
            itemfg.total-std-cost = itemfg.std-tot-cost                         
            itemfg.last-cost    = itemfg.std-tot-cost
            itemfg.avg-cost =  (((itemfg.q-onh - {2}.t-qty) * itemfg.avg-cost) + (itemfg.std-tot-cost * {2}.t-qty))  / itemfg.q-onh.
        
        FIND FIRST fg-ctrl NO-LOCK WHERE 
            fg-ctrl.company EQ itemfg.company
            NO-ERROR.
        IF AVAILABLE fg-ctrl 
        AND fg-ctrl.inv-meth = "A" THEN ASSIGN 
            itemfg.total-std-cost = itemfg.avg-cost.                     
        
        ASSIGN 
            itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
            itemfg.q-ship-ptd = MAXIMUM(0,itemfg.q-ship-ptd)
            itemfg.q-ship-ytd = MAXIMUM(0,itemfg.q-ship-ytd).
        FIND CURRENT itemfg NO-LOCK.

    END.  /* rita-code = "r" */
    ELSE IF {1}.rita-code EQ "S" THEN DO:   /** SHIPMENTS **/
        FIND CURRENT itemfg EXCLUSIVE.
        ASSIGN
            itemfg.q-ytd      = itemfg.q-ytd - {2}.t-qty
            itemfg.q-ship     = itemfg.q-ship + {2}.t-qty
            itemfg.q-ship-ptd = itemfg.q-ship-ptd + {2}.t-qty
            itemfg.q-ship-ytd = itemfg.q-ship-ytd + {2}.t-qty
            itemfg.q-onh      = itemfg.q-onh - {2}.t-qty.

        RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
        
        FIND FIRST itemfg-loc EXCLUSIVE WHERE 
            itemfg-loc.company EQ itemfg.company AND 
            itemfg-loc.i-no    EQ itemfg.i-no AND 
            itemfg-loc.loc     EQ {2}.loc
            NO-ERROR.
        ASSIGN
            itemfg-loc.q-ytd      = itemfg-loc.q-ytd - {2}.t-qty
            itemfg-loc.q-ship     = itemfg-loc.q-ship + {2}.t-qty
            itemfg-loc.q-ship-ptd = itemfg-loc.q-ship-ptd + {2}.t-qty
            itemfg-loc.q-ship-ytd = itemfg-loc.q-ship-ytd + {2}.t-qty
            itemfg-loc.q-onh      = itemfg-loc.q-onh - {2}.t-qty. 
        FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
        
        ASSIGN 
            itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
            itemfg.q-ship-ptd = MAXIMUM(0,itemfg.q-ship-ptd)
            itemfg.q-ship-ytd = MAXIMUM(0,itemfg.q-ship-ytd).
        FIND CURRENT itemfg NO-LOCK.
    END.
    ELSE IF {1}.rita-code EQ "T" THEN DO:  /** TRANSFERS **/
        FIND CURRENT itemfg EXCLUSIVE.
        ASSIGN
            itemfg.q-tran     = itemfg.q-tran + {2}.t-qty
            itemfg.q-tran-ptd = itemfg.q-tran-ptd + {2}.t-qty.

        /* Move the inventory between the two itemfg-loc records */
        RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
        
        FIND FIRST itemfg-loc EXCLUSIVE WHERE 
            itemfg-loc.company EQ itemfg.company AND 
            itemfg-loc.i-no    EQ itemfg.i-no AND 
            itemfg-loc.loc     EQ {2}.loc
            NO-ERROR.
        IF AVAILABLE itemfg-loc THEN ASSIGN
            itemfg-loc.q-onh = itemfg-loc.q-onh - {2}.t-qty
            itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.

        RUN fg/chkfgloc.p (INPUT itemfg.i-no, 
                            INPUT {2}.loc2).

        FIND FIRST itemfg-loc EXCLUSIVE WHERE 
            itemfg-loc.company EQ itemfg.company AND 
            itemfg-loc.i-no    EQ itemfg.i-no AND 
            itemfg-loc.loc     EQ {2}.loc2
            NO-ERROR.
        IF AVAILABLE itemfg-loc THEN ASSIGN
            itemfg-loc.q-onh = itemfg-loc.q-onh + {2}.t-qty
            itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.

        FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.

        /* Close transfer orders */
        IF {2}.bol-no GT 0 THEN 
            RUN oe/clsorditm.p (INPUT {2}.company, 
                                INPUT {2}.bol-no, 
                                INPUT {2}.i-no).
        
        ASSIGN 
            itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
            itemfg.q-ship-ptd = MAXIMUM(0,itemfg.q-ship-ptd)
            itemfg.q-ship-ytd = MAXIMUM(0,itemfg.q-ship-ytd).
        FIND CURRENT itemfg NO-LOCK.
                                        
    END.  /* TRANSFERS */
    ELSE IF {1}.rita-code EQ "A" THEN DO:       /** ADJUSTMENTS **/
        FIND CURRENT itemfg EXCLUSIVE.
        ASSIGN
            itemfg.q-adj     = itemfg.q-adj  + {2}.t-qty
            itemfg.q-adj-ytd = itemfg.q-adj-ytd  + {2}.t-qty
            itemfg.q-adj-ptd = itemfg.q-adj-ptd  + {2}.t-qty
            itemfg.q-onh     = itemfg.q-onh + {2}.t-qty.
        RUN fg/chkfgloc.p (INPUT itemfg.i-no, 
                            INPUT {2}.loc).
        FIND FIRST itemfg-loc EXCLUSIVE WHERE 
            itemfg-loc.company EQ itemfg.company AND 
            itemfg-loc.i-no    EQ itemfg.i-no AND 
            itemfg-loc.loc     EQ {2}.loc
            NO-ERROR.
        IF AVAILABLE itemfg-loc THEN DO:
            ASSIGN
                itemfg-loc.q-adj     = itemfg-loc.q-adj  + {2}.t-qty
                itemfg-loc.q-adj-ytd = itemfg-loc.q-adj-ytd  + {2}.t-qty
                itemfg-loc.q-adj-ptd = itemfg-loc.q-adj-ptd  + {2}.t-qty
                itemfg-loc.q-onh     = itemfg-loc.q-onh + {2}.t-qty.
            FIND CURRENT itemfg-loc NO-LOCK.
        END.
        ASSIGN 
            itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
            itemfg.q-ship-ptd = MAXIMUM(0,itemfg.q-ship-ptd)
            itemfg.q-ship-ytd = MAXIMUM(0,itemfg.q-ship-ytd).
        FIND CURRENT itemfg NO-LOCK.
    END.
    ELSE IF {1}.rita-code EQ "E" THEN DO:      /** CREDIT RETURNS **/
        FIND CURRENT itemfg EXCLUSIVE.
        ASSIGN
            itemfg.q-ytd      = itemfg.q-ytd + {2}.t-qty
            itemfg.q-ship     = itemfg.q-ship - {2}.t-qty
            itemfg.q-ship-ptd = itemfg.q-ship-ptd - {2}.t-qty
            itemfg.q-ship-ytd = itemfg.q-ship-ytd - {2}.t-qty
            itemfg.q-onh      = itemfg.q-onh + {2}.t-qty.
        RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
        FIND FIRST itemfg-loc EXCLUSIVE WHERE 
            itemfg-loc.company EQ itemfg.company AND 
            itemfg-loc.i-no    EQ itemfg.i-no AND 
            itemfg-loc.loc     EQ {2}.loc
            NO-ERROR.
        IF AVAILABLE itemfg-loc THEN DO:
            ASSIGN
                itemfg-loc.q-ytd      = itemfg-loc.q-ytd + {2}.t-qty
                itemfg-loc.q-ship     = itemfg-loc.q-ship - {2}.t-qty
                itemfg-loc.q-ship-ptd = itemfg-loc.q-ship-ptd - {2}.t-qty
                itemfg-loc.q-ship-ytd = itemfg-loc.q-ship-ytd - {2}.t-qty
                itemfg-loc.q-onh      = itemfg-loc.q-onh + {2}.t-qty.
            FIND CURRENT itemfg-loc NO-LOCK.
        END.
        ASSIGN 
            itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
            itemfg.q-ship-ptd = MAXIMUM(0,itemfg.q-ship-ptd)
            itemfg.q-ship-ytd = MAXIMUM(0,itemfg.q-ship-ytd).
        FIND CURRENT itemfg NO-LOCK.
    END.

    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
    FIND FIRST itemfg-loc EXCLUSIVE WHERE 
        itemfg-loc.company EQ itemfg.company AND 
        itemfg-loc.i-no    EQ itemfg.i-no AND 
        itemfg-loc.loc     EQ {2}.loc
        NO-ERROR.
    IF AVAILABLE itemfg-loc THEN DO:
        ASSIGN 
            itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
        FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    END.

    FIND FIRST job-hdr NO-LOCK WHERE 
        job-hdr.company EQ cocode AND 
        job-hdr.job-no  EQ {1}.job-no AND 
        job-hdr.job-no2 EQ {1}.job-no2 AND 
        job-hdr.i-no    EQ {1}.i-no
        NO-ERROR.
    IF AVAIL job-hdr THEN FIND FIRST fg-bin EXCLUSIVE WHERE 
        fg-bin.company EQ cocode AND 
        fg-bin.i-no    EQ {1}.i-no AND 
        fg-bin.job-no  EQ {1}.job-no AND 
        fg-bin.job-no2 EQ {1}.job-no2 AND 
        fg-bin.loc     EQ {2}.loc AND 
        fg-bin.loc-bin EQ {2}.loc-bin AND 
        fg-bin.tag     EQ {2}.tag AND 
        fg-bin.cust-no EQ {2}.cust-no
        NO-ERROR.
    IF NOT AVAILABLE fg-bin 
    AND INDEX("REA",{1}.rita-code) GT 0 THEN DO:
        CREATE fg-bin.
        ASSIGN
            fg-bin.company    = cocode
            fg-bin.i-no       = {1}.i-no
            fg-bin.job-no     = {1}.job-no
            fg-bin.job-no2    = {1}.job-no2
            fg-bin.loc        = {2}.loc
            fg-bin.loc-bin    = {2}.loc-bin
            fg-bin.tag        = {2}.tag
            fg-bin.cust-no    = {2}.cust-no.
    END.
    ELSE RETURN ERROR.  /*undo {3}, next {3}. */

    /* Transfers don't have PO # populated, so take from the "from" bin */
    IF NOT {1}.rita-code EQ "T" 
    AND NOT ({1}.rita-code EQ "A" AND {1}.qty LT 0) THEN ASSIGN 
        fg-bin.po-no      = {2}.po-no. /*01161406*/
    
    ASSIGN  
        fg-bin.tot-wt     = {2}.tot-wt .

    IF fg-bin.case-count   LE 0 
    OR (fg-bin.case-count  LT {2}.qty-case AND fg-bin.case-count EQ fg-bin.qty) THEN ASSIGN 
        fg-bin.case-count   = {2}.qty-case.
    IF fg-bin.units-pallet LE 0 THEN ASSIGN 
        fg-bin.units-pallet = {2}.units-pallet.
    IF fg-bin.cases-unit   LE 0 THEN ASSIGN  
        fg-bin.cases-unit   = {2}.cases-unit.

    /* For Transfers from & Shipments decrease the quantity in the BIN */
    IF INDEX("TS",{1}.rita-code) NE 0 THEN ASSIGN
        fg-bin.qty           = fg-bin.qty - {2}.t-qty
        fg-bin.partial-count = fg-bin.partial-count - {2}.partial.

    /* Store the PO-no to put into new bin */
    IF {1}.rita-code = "T" THEN ASSIGN 
        {2}.po-no = fg-bin.po-no.
    /* For Receipts increase the quantity in the BIN */
    ELSE IF {1}.rita-code EQ "R" THEN DO:
        ASSIGN 
            ld-cvt-cost = {2}.ext-cost / {2}.t-qty * 1000.
        IF {2}.cost-uom NE "M" THEN
               RUN sys/ref/convcuom.p ("M", 
                                      {2}.cost-uom, 
                                      0,
                                      IF AVAILABLE po-ordl THEN po-ordl.s-len ELSE 0,
                                      IF AVAILABLE po-ordl THEN po-ordl.s-wid ELSE 0,
                                      0, 
                                      ld-cvt-cost, 
                                      OUTPUT ld-cvt-cost).
                {fg/upd-bin.i "fg-bin" "{2}.cost-uom" "ld-cvt-cost" {2}}
    END.
    ELSE IF {1}.rita-code EQ "A" OR {1}.rita-code EQ "E" THEN ASSIGN
        fg-bin.qty           = fg-bin.qty + {2}.t-qty
        fg-bin.partial-count = fg-bin.partial-count + {2}.partial.

    /* This code is to handle the Transfer quantity to increase the BIN */
    IF {1}.rita-code EQ "T" THEN DO:
        FIND FIRST b-fg-bin EXCLUSIVE WHERE 
            b-fg-bin.company EQ {1}.company AND 
            b-fg-bin.job-no  EQ {1}.job-no AND 
            b-fg-bin.job-no2 EQ {1}.job-no2 AND 
            b-fg-bin.i-no    EQ {1}.i-no AND 
            b-fg-bin.loc     EQ {2}.loc2 AND 
            b-fg-bin.loc-bin EQ {2}.loc-bin2 AND 
            b-fg-bin.tag     EQ {2}.tag2 AND 
            b-fg-bin.cust-no EQ {2}.cust-no
            NO-ERROR.

        IF NOT AVAILABLE b-fg-bin THEN DO:
            CREATE b-fg-bin.
            ASSIGN
                b-fg-bin.company      = {1}.company
                b-fg-bin.i-no         = {1}.i-no
                b-fg-bin.job-no       = {1}.job-no
                b-fg-bin.job-no2      = {1}.job-no2
                b-fg-bin.loc          = {2}.loc2
                b-fg-bin.loc-bin      = {2}.loc-bin2
                b-fg-bin.tag          = {2}.tag2
                b-fg-bin.cust-no      = {2}.cust-no
                b-fg-bin.case-count   = {2}.qty-case
                b-fg-bin.units-pallet = fg-bin.units-pallet
                b-fg-bin.cases-unit   = fg-bin.cases-unit
                b-fg-bin.po-no        = {2}.po-no.
        END.
        
        IF {2}.po-no GT "" THEN ASSIGN 
            b-fg-bin.po-no = {2}.po-no.
        IF b-fg-bin.case-count   LE 0 THEN ASSIGN 
            b-fg-bin.case-count   = fg-bin.case-count.
        IF b-fg-bin.units-pallet LE 0 THEN ASSIGN 
            b-fg-bin.units-pallet = fg-bin.units-pallet.
        IF b-fg-bin.cases-unit   LE 0 THEN ASSIGN 
            b-fg-bin.cases-unit   = fg-bin.cases-unit.

        {fg/upd-bin.i "b-fg-bin" "fg-bin.pur-uom" "fg-bin.std-tot-cost" {2}}

        IF fg-bin.pur-uom EQ "M" THEN ASSIGN 
            {2}.ext-cost = fg-bin.std-tot-cost * {2}.t-qty / 1000.
        ELSE ASSIGN 
            {2}.ext-cost = fg-bin.std-tot-cost * {2}.t-qty.
       
        ASSIGN 
            b-fg-bin.tot-wt   = {2}.tot-wt .

        FIND CURRENT b-fg-bin NO-LOCK NO-ERROR.
    END. /* if rita-code eq "T" */
 
    /* Find first to prevent duplicates */
    FIND FIRST fg-rdtlh NO-LOCK WHERE 
        fg-rdtlh.r-no EQ {2}.r-no AND 
        fg-rdtlh.i-no EQ {2}.i-no AND 
        fg-rdtlh.cases EQ {2}.cases AND 
        fg-rdtlh.tag EQ {2}.tag AND 
        fg-rdtlh.qty EQ {2}.qty AND 
        fg-rdtlh.job-no EQ {2}.job-no AND 
        fg-rdtlh.job-no2 EQ {2}.job-no2 AND 
        fg-rdtlh.partial EQ {2}.partial
        NO-ERROR.
    IF NOT AVAILABLE fg-rdtlh THEN DO:
        CREATE fg-rdtlh.
        {fg/fg-rdtl.i fg-rdtlh {2}} /* Create Detail History Records */
        {fg/fg-fgact.i {1} {2}}         /* Create Job Costing F/G WIP Record */
        ASSIGN 
            fg-rdtlh.user-id = USERID('ASI')
            fg-rdtlh.upd-date = TODAY
            fg-rdtlh.upd-time = TIME.
    END.

    CREATE fg-rcpth.
    {fg/fg-rcpts.i fg-rcpth {1}}  /* Create Header History Records */
    ASSIGN 
        fg-rcpth.user-id = USERID('ASI')
        fg-rcpth.upd-date = TODAY
        fg-rcpth.upd-time = TIME.

    FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.
    FIND CURRENT fg-rcpth NO-LOCK NO-ERROR.

END. /*end IF {1}.rita-code NE "I"*/

