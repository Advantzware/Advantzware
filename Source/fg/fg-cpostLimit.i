// fg/fg-cpostLimit.i

FIND FIRST itemfg NO-LOCK
     WHERE itemfg.company EQ cocode
     AND itemfg.i-no EQ {1}fg-rctd.i-no
     NO-ERROR.        
IF NOT AVAILABLE itemfg THEN NEXT.

RELEASE b2-fg-bin.        
            
/* Find Bin for that specific Job# Sequence & if avail
   then use Standard Cost from that Bin. */
FIND FIRST b-fg-bin EXCLUSIVE-LOCK
    WHERE b-fg-bin.company   EQ {1}fg-rctd.company
    AND b-fg-bin.i-no      EQ {1}fg-rctd.i-no
    AND b-fg-bin.loc       EQ {1}fg-rctd.loc
    AND b-fg-bin.loc-bin   EQ {1}fg-rctd.loc-bin
    AND b-fg-bin.tag       EQ {1}fg-rctd.tag
    AND b-fg-bin.job-no    EQ {1}fg-rctd.job-no
    AND b-fg-bin.job-no2   EQ {1}fg-rctd.job-no2
    AND b-fg-bin.cust-no   EQ {1}fg-rctd.cust-no
    USE-INDEX co-ino
    NO-ERROR.
IF NOT AVAILABLE b-fg-bin AND {1}fg-rctd.t-qty NE 0 THEN 
DO:
    IF {1}fg-rctd.tag NE "" THEN      /* Check for Transfer/Count function */
        FIND FIRST b2-fg-bin EXCLUSIVE-LOCK
            WHERE b2-fg-bin.company         EQ {1}fg-rctd.company
            AND b2-fg-bin.i-no            EQ {1}fg-rctd.i-no
            AND b2-fg-bin.loc             EQ {1}fg-rctd.loc
            AND b2-fg-bin.loc-bin         EQ {1}fg-rctd.loc-bin
            AND b2-fg-bin.tag             EQ ""
            AND b2-fg-bin.job-no          EQ {1}fg-rctd.job-no
            AND b2-fg-bin.job-no2         EQ {1}fg-rctd.job-no2
            AND b2-fg-bin.cust-no         EQ {1}fg-rctd.cust-no
            USE-INDEX co-ino NO-ERROR.
            
    CREATE b-fg-bin.
    ASSIGN
        b-fg-bin.company = {1}fg-rctd.company
        b-fg-bin.job-no  = {1}fg-rctd.job-no
        b-fg-bin.job-no2 = {1}fg-rctd.job-no2
        b-fg-bin.loc     = {1}fg-rctd.loc
        b-fg-bin.loc-bin = {1}fg-rctd.loc-bin
        b-fg-bin.tag     = {1}fg-rctd.tag
        b-fg-bin.cust-no = {1}fg-rctd.cust-no
        b-fg-bin.i-no    = {1}fg-rctd.i-no
        b-fg-bin.qty     = {1}fg-rctd.t-qty
        .
    IF AVAILABLE b2-fg-bin THEN                      /* Transfer/Count */
        ASSIGN
            b-fg-bin.case-count   = b2-fg-bin.case-count
            b-fg-bin.cases-unit   = b2-fg-bin.cases-unit
            b-fg-bin.unit-count   = b2-fg-bin.unit-count
            b-fg-bin.units-pallet = b2-fg-bin.units-pallet
            b-fg-bin.std-mat-cost = b2-fg-bin.std-mat-cost
            b-fg-bin.std-lab-cost = b2-fg-bin.std-lab-cost
            b-fg-bin.std-fix-cost = b2-fg-bin.std-fix-cost
            b-fg-bin.std-var-cost = b2-fg-bin.std-var-cost
            b2-fg-bin.qty         = b2-fg-bin.qty - {1}fg-rctd.t-qty
            .      
    ELSE 
    DO:
        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company     EQ cocode
            AND job-hdr.i-no         EQ {1}fg-rctd.i-no
            AND job-hdr.job-no       EQ {1}fg-rctd.job-no
            AND job-hdr.job-no2      EQ {1}fg-rctd.job-no2
            USE-INDEX i-no
            NO-ERROR.                   
        IF AVAILABLE job-hdr THEN
            ASSIGN
                b-fg-bin.std-mat-cost = job-hdr.std-mat-cost
                b-fg-bin.std-lab-cost = job-hdr.std-lab-cost
                b-fg-bin.std-fix-cost = job-hdr.std-fix-cost
                b-fg-bin.std-var-cost = job-hdr.std-var-cost
                .      
        ELSE
            ASSIGN
                b-fg-bin.std-mat-cost = itemfg.std-mat-cost
                b-fg-bin.std-lab-cost = itemfg.std-lab-cost
                b-fg-bin.std-fix-cost = itemfg.std-fix-cost
                b-fg-bin.std-var-cost = itemfg.std-var-cost
                .
    END.        
    b-fg-bin.std-tot-cost = b-fg-bin.std-mat-cost
                          + b-fg-bin.std-lab-cost
                          + b-fg-bin.std-fix-cost
                          + b-fg-bin.std-var-cost
                          . 
END.

IF AVAILABLE b-fg-bin THEN 
DO:
    ASSIGN
        v-q-adj-ytd            = v-q-adj-ytd + ({1}fg-rctd.t-qty - b-fg-bin.qty)
        b-fg-bin.cases         = {1}fg-rctd.cases
        b-fg-bin.case-count    = {1}fg-rctd.qty-case
        b-fg-bin.units-pallet  = {1}fg-rctd.units-pallet
        b-fg-bin.cases-unit    = {1}fg-rctd.cases-unit
        b-fg-bin.partial-count = {1}fg-rctd.partial
        b-fg-bin.qty           = {1}fg-rctd.t-qty
        b-fg-bin.last-count    = {1}fg-rctd.t-qty
        b-fg-bin.last-date     = {1}fg-rctd.rct-date
        .       
    /* Update bin with any transactions after this cycle count */
    FOR EACH fg-rcpth NO-LOCK 
        WHERE fg-rcpth.company    EQ cocode
        AND fg-rcpth.i-no       EQ b-fg-bin.i-no
        AND fg-rcpth.trans-date GE {1}fg-rctd.rct-date
        AND fg-rcpth.job-no     EQ b-fg-bin.job-no
        AND fg-rcpth.job-no2    EQ b-fg-bin.job-no2  
        USE-INDEX tran,
        EACH fg-rdtlh NO-LOCK
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND fg-rdtlh.loc       EQ b-fg-bin.loc
        AND fg-rdtlh.loc-bin   EQ b-fg-bin.loc-bin
        AND fg-rdtlh.tag       EQ b-fg-bin.tag
        AND fg-rdtlh.cust-no   EQ b-fg-bin.cust-no
        BY fg-rcpth.trans-date
        BY fg-rdtlh.trans-time
        BY fg-rcpth.r-no
        :            
        IF fg-rcpth.trans-date EQ {1}fg-rctd.rct-date AND
           fg-rcpth.r-no       LT {1}fg-rctd.r-no THEN NEXT.               
            {fg/fg-mkbin.i b- b-}   
    END.
END.  
        
CREATE fg-rdtlh.
{fg/fg-rdtl.i fg-rdtlh {1}fg-rctd}
    
IF lLastBin AND AVAILABLE b2-fg-bin THEN 
DO:
    /* Transfer/Count */
    ASSIGN
        {1}fg-rctd.tag      = ""
        {1}fg-rctd.ext-cost = b2-fg-bin.qty
                            * ({1}fg-rctd.ext-cost / {1}fg-rctd.t-qty)
        {1}fg-rctd.t-qty    = b2-fg-bin.qty
        .
    CREATE fg-rdtlh.      
      {fg/fg-rdtl.i fg-rdtlh {1}fg-rctd}
END.

CREATE fg-rcpth.
{fg/fg-rcpts.i fg-rcpth {1}fg-rctd}   /* Create History Record */

/* update cost for non-job order lines for item */
IF lLastItem THEN 
DO:
    ASSIGN
        v-q-adj-ytd = 0
        v-qty-onh   = 0
        .
    FOR EACH b-fg-bin FIELDS(qty) NO-LOCK
        WHERE b-fg-bin.company EQ cocode
        AND b-fg-bin.i-no    EQ itemfg.i-no 
        AND b-fg-bin.loc     EQ {1}fg-rctd.loc
        AND b-fg-bin.onHold  EQ NO
        :
            
        v-qty-onh = v-qty-onh + b-fg-bin.qty.
    END. /* each b-fg-bin */
    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {1}fg-rctd.loc).
    DO WHILE TRUE:
        FIND itemfg-loc EXCLUSIVE-LOCK 
            WHERE itemfg-loc.company EQ itemfg.company
            AND itemfg-loc.i-no EQ itemfg.i-no
            AND itemfg-loc.loc  EQ {1}fg-rctd.loc
            NO-ERROR NO-WAIT.
        IF AVAILABLE itemfg-loc THEN
        DO:
            ASSIGN
                itemfg-loc.q-onh     = v-qty-onh
                itemfg-loc.q-avail   = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
                itemfg-loc.q-adj-ytd = itemfg-loc.q-adj-ytd + v-q-adj-ytd
                .         
            FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
            LEAVE.
        END.
    END.

    ASSIGN
        v-q-adj-ytd = 0
        v-qty-onh   = 0
        .
    FOR EACH b-fg-bin FIELDS(qty) NO-LOCK
        WHERE b-fg-bin.company EQ cocode
        AND b-fg-bin.i-no    EQ itemfg.i-no
        AND b-fg-bin.onHold  EQ NO 
        :            
        v-qty-onh = v-qty-onh + b-fg-bin.qty.
    END. /* each b-fg-bin */

    DO WHILE TRUE:
        FIND CURRENT itemfg EXCLUSIVE-LOCK NO-ERROR NO-WAIT.      
        IF AVAILABLE itemfg THEN
        DO:
            ASSIGN
                itemfg.q-onh     = v-qty-onh
                itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
                itemfg.q-adj-ytd = itemfg.q-adj-ytd + v-q-adj-ytd.         
            FIND CURRENT itemfg NO-LOCK NO-ERROR.
            LEAVE.
        END.
    END.

    /* Procedure updfgcs1.p re-calculates average cost,mat cost,lab cost etc. 
       for all bins for the given item while posting counts which is impacting 
       the performance of createinventorycount API . Re-calculation of costs 
       is not required for posting counts. So, updfgcs1.p call has been commented 
       out to improve the performance of createinventorycount API call.
           
       run fg/updfgcs1.p (recid(itemfg), NO, NO).
           
    */

    FIND b-itemfg NO-LOCK WHERE RECID(b-itemfg) EQ RECID(itemfg).

    FOR EACH oe-ordl EXCLUSIVE-LOCK
        WHERE oe-ordl.company EQ cocode
        AND oe-ordl.i-no    EQ {1}fg-rctd.i-no
        AND oe-ordl.opened  EQ YES
        AND oe-ordl.job-no  EQ ""
        AND oe-ordl.cost    EQ 0
        USE-INDEX item:
        RUN sys/ref/convcuom.p (b-itemfg.prod-uom, "M", 0, 0, 0, 0, b-itemfg.total-std-cost, OUTPUT oe-ordl.cost).
    END.
END. /* last-of i-no*/

IF "{1}" EQ "" AND {1}fg-rctd.t-qty NE 0 THEN 
DO:
    FIND FIRST loadtag EXCLUSIVE-LOCK
        WHERE loadtag.company   EQ {1}fg-rctd.company
        AND loadtag.item-type EQ NO
        AND loadtag.tag-no    EQ {1}fg-rctd.tag
        NO-ERROR.
    IF AVAILABLE loadtag THEN 
        ASSIGN
            loadtag.loc     = {1}fg-rctd.loc
            loadtag.loc-bin = {1}fg-rctd.loc-bin
            .
END.

DELETE {1}fg-rctd.
