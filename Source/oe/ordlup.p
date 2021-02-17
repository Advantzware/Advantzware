/* ---------------------------------------------------- oe/ordlup.p 03/98 JLF */
/* order lines update 2 - o/e module                                          */
/* UPDATES COSTS FOR ITEM                                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
/*{sys/form/s-top.f}
*/

DEFINE SHARED BUFFER xoe-ord FOR oe-ord.
/*def shared var v-new-item as log no-undo. */
DEFINE SHARED VARIABLE fil_id            AS RECID   NO-UNDO.
DEFINE SHARED VARIABLE nufile            AS LOG     NO-UNDO.

DEFINE        VARIABLE v-save-id         AS RECID.
DEFINE        VARIABLE v-job-job         LIKE job-hdr.job.
DEFINE        VARIABLE v-qty-ord         LIKE oe-ordl.qty.
DEFINE        VARIABLE v-part-qty        AS DECIMAL.
DEFINE        VARIABLE v-q-back          AS INTEGER NO-UNDO.
DEFINE        VARIABLE v-run-from-steps2 AS LOG     NO-UNDO.
DEFINE        VARIABLE K_FRAC            AS DECIMAL INIT 6.25 NO-UNDO.

DEFINE BUFFER xitemfg FOR itemfg.

{ce/print4a.i shared}

{fg/fullset.i NEW}


{ce/msfcalc.i}
    
{sys/inc/f16to32.i}

DISABLE TRIGGERS FOR LOAD OF itemfg.

v-run-from-steps2 = NO.
IF INDEX(PROGRAM-NAME(2), "steps2") GT 0 THEN
    v-run-from-steps2 = YES.

FIND oe-ordl WHERE RECID(oe-ordl) EQ fil_id NO-LOCK NO-ERROR.

IF NOT AVAILABLE job-hdr THEN
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
        AND job-hdr.i-no    EQ oe-ordl.i-no
        AND job-hdr.job-no  EQ oe-ordl.job-no
        AND job-hdr.job-no2 EQ oe-ordl.job-no2
        AND job-hdr.ord-no  EQ oe-ordl.ord-no
        USE-INDEX job-no NO-ERROR.

/**** create job-hdr --- xjob header file ****/
IF oe-ordl.est-no NE "" OR
    (oe-ordl.est-no EQ "" AND oe-ordl.job-no NE "") THEN 
DO:

    IF NOT AVAILABLE job-hdr THEN 
    DO:
        FIND FIRST job WHERE job.company EQ cocode
            AND job.job-no  EQ oe-ordl.job-no
            AND job.job-no2 EQ oe-ordl.job-no2
            NO-LOCK NO-ERROR.
        IF AVAILABLE job THEN v-job-job = job.job.
        ELSE 
        DO:
            FIND LAST job-hdr WHERE job-hdr.company EQ cocode
                USE-INDEX job NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job-hdr THEN 
            DO:
                CREATE job-hdr.
                ASSIGN 
                    job-hdr.company = cocode
                    job-hdr.job-no  = "FIRST".
            END.
            FIND LAST job WHERE job.company EQ cocode
                USE-INDEX job NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job THEN 
            DO:
                CREATE job.
                ASSIGN 
                    job.company = cocode
                    job.job-no  = "FIRST".
            END.
            IF job-hdr.job GT job.job THEN v-job-job = job-hdr.job + 1.
            IF job.job GE job-hdr.job THEN v-job-job = job.job + 1.

            CREATE job.
            ASSIGN 
                job.job     = v-job-job
                job.company = cocode
                job.loc     = locode
                job.est-no  = oe-ordl.est-no /* wfk - 02281301 */
                job.job-no  = oe-ordl.job-no
                job.job-no2 = oe-ordl.job-no2
                job.stat    = "P".
        END.

        CREATE job-hdr.
        ASSIGN
            job-hdr.company = cocode
            job-hdr.loc     = locode
            job-hdr.est-no  = oe-ordl.est-no
            job-hdr.i-no    = oe-ordl.i-no
            /*   job-hdr.qty        = oe-ordl.qty */
            job-hdr.job-no  = oe-ordl.job-no
            job-hdr.job-no2 = oe-ordl.job-no2
            job-hdr.job     = v-job-job
            job-hdr.cust-no = oe-ordl.cust-no
            job-hdr.ord-no  = oe-ordl.ord-no
            job-hdr.po-no   = oe-ordl.po-no
            job-hdr.frm     = oe-ordl.form-no.

    END. /* not avail job-hdr */

    /** if this is a new line item then write job number to order line **/
    IF nufile THEN 
    DO:
        FIND oe-ordl WHERE RECID(oe-ordl) EQ fil_id.
        oe-ordl.j-no = job-hdr.j-no.
        FIND oe-ordl WHERE RECID(oe-ordl) EQ fil_id NO-LOCK.
    END. /* nufile */

    FIND FIRST est WHERE est.company = oe-ordl.company
        AND est.est-no EQ oe-ordl.est-no NO-LOCK NO-ERROR.

    FIND FIRST xjob WHERE xjob.i-no EQ oe-ordl.i-no NO-ERROR.

    IF AVAILABLE xjob THEN 
    DO:
        IF AVAILABLE est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN 
        DO:
            ASSIGN
                job-hdr.std-mat-cost = 0
                job-hdr.std-lab-cost = 0
                job-hdr.std-fix-cost = 0
                job-hdr.std-var-cost = 0.
            FOR EACH xjob:
                ASSIGN
                    job-hdr.std-mat-cost = xjob.mat + job-hdr.std-mat-cost
                    job-hdr.std-lab-cost = xjob.lab + job-hdr.std-lab-cost
                    job-hdr.std-fix-cost = xjob.foh + job-hdr.std-fix-cost
                    job-hdr.std-var-cost = xjob.voh + job-hdr.std-var-cost.
            END.
        END.

        ELSE 
        DO:
            ASSIGN
                job-hdr.std-mat-cost = xjob.mat
                job-hdr.std-lab-cost = xjob.lab
                job-hdr.std-fix-cost = xjob.foh
                job-hdr.std-var-cost = xjob.voh.
        END.
    END.

    job-hdr.std-tot-cost = job-hdr.std-mat-cost +
        job-hdr.std-lab-cost +
        job-hdr.std-fix-cost +
        job-hdr.std-var-cost.
END.   /* if oe-ordl.est-no ne "" */   /*DAR*/

FIND FIRST itemfg EXCLUSIVE-LOCK
    WHERE itemfg.company EQ cocode
    AND itemfg.i-no    EQ oe-ordl.i-no
    USE-INDEX i-no NO-ERROR.

IF AVAILABLE itemfg THEN 
DO:
    itemfg.cust-po-no = oe-ordl.po-no.

    IF xoe-ord.type NE "T" THEN /*itemfg.q-alloc = itemfg.q-alloc + oe-ordl.qty.*/
        RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT itemfg.q-alloc, OUTPUT v-q-back).

    IF AVAIL(itemfg) AND AVAIL(xoe-ord) THEN 
    DO:          
        RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT xoe-ord.loc).
        FIND FIRST itemfg-loc 
            WHERE itemfg-loc.company EQ itemfg.company
            AND itemfg-loc.i-no    EQ itemfg.i-no
            AND itemfg-loc.loc     EQ xoe-ord.loc
            EXCLUSIVE-LOCK NO-ERROR.
    END.
    IF AVAIL(itemfg-loc) AND avail(itemfg) AND AVAIL(xoe-ord) AND xoe-ord.TYPE NE "T" THEN
        RUN fg/calcqabl.p (ROWID(itemfg), xoe-ord.loc, OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).

    IF itemfg.q-alloc LT 0 THEN 
    DO:
        itemfg.q-alloc = 0.
        IF AVAIL(itemfg-loc) THEN
            itemfg-loc.q-alloc = 0.
    END.
  
    FOR EACH itemfg-loc 
        WHERE itemfg-loc.company EQ itemfg.company
        AND itemfg-loc.i-no    EQ itemfg.i-no
        EXCLUSIVE-LOCK.

        RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).    
        ASSIGN
            itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.

    END.
    ASSIGN
        itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.

    IF AVAILABLE itemfg-loc THEN
        ASSIGN
            itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    RELEASE itemfg-loc.
    /*
    
          IF AVAIL(itemfg-loc) AND AVAIL bf-oe-ordl AND avail(itemfg) AND xoe-ord.TYPE NE "T" THEN
            RUN fg/calcqabl.p (ROWID(itemfg), xoe-ord.loc, OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).
    
          IF AVAIL(itemfg) AND itemfg.q-alloc LT 0 THEN do:
              itemfg.q-alloc = 0.
              IF AVAIL itemfg-loc THEN
                  itemfg-loc.q-alloc = 0.
    */   
    /* This procedure is run from final-steps2 as well as final-steps, 
       so the quantity could be doubled */
    IF NOT v-run-from-steps2 THEN
        ASSIGN
            itemfg.q-ptd     = itemfg.q-ptd + oe-ordl.qty
            itemfg.q-ord-ytd = itemfg.q-ord-ytd + oe-ordl.qty.
  
    IF itemfg.isaset THEN 
    DO:                 /** Update Set Parts */
        IF CAN-FIND(FIRST fg-set WHERE fg-set.company EQ itemfg.company
            AND fg-set.set-no  EQ itemfg.i-no
            AND fg-set.part-no NE fg-set.set-no) THEN
            RUN fg/fullset.p (ROWID(itemfg)).

        FOR EACH tt-fg-set BREAK BY tt-fg-set.set-no:
            IF FIRST(tt-fg-set.set-no) THEN DO:
                ASSIGN
                    itemfg.t-len  = 0
                    itemfg.t-wid  = 0
                    itemfg.t-sqin = 0
                    itemfg.t-sqft = 0.
                IF NOT itemfg.spare-int-1 EQ 1 THEN   /* freeze weight flag */
                    itemfg.weight-100 = 0.
            END. 

            FIND FIRST xitemfg EXCLUSIVE-LOCK 
                WHERE xitemfg.company EQ cocode
                AND xitemfg.i-no    EQ tt-fg-set.part-no
                USE-INDEX i-no NO-ERROR.

            IF NOT AVAILABLE xitemfg THEN NEXT.
    
            FIND FIRST eb
                WHERE eb.company EQ oe-ordl.company 
                AND eb.est-no  EQ oe-ordl.est-no
                AND eb.form-no EQ 0
                NO-LOCK NO-ERROR.

            IF AVAILABLE eb THEN
                ASSIGN
                    itemfg.w-score[50] = eb.wid
                    itemfg.l-score[50] = eb.len
                    itemfg.d-score[50] = eb.dep.
     
            ASSIGN
                itemfg.t-wid  = itemfg.t-wid      + (xitemfg.t-wid      * tt-fg-set.part-qty-dec)
                itemfg.t-len  = itemfg.t-len      + (xitemfg.t-len      * tt-fg-set.part-qty-dec)
                itemfg.t-sqin = itemfg.t-sqin     + (xitemfg.t-sqin     * tt-fg-set.part-qty-dec)
                itemfg.t-sqft = itemfg.t-sqft     + (xitemfg.t-sqft     * tt-fg-set.part-qty-dec).
            IF NOT itemfg.spare-int-1 EQ 1 THEN
                itemfg.weight-100  = itemfg.weight-100 + (xitemfg.weight-100 * tt-fg-set.part-qty-dec).

            IF xoe-ord.type NE "T" THEN
                /*xitemfg.q-alloc = xitemfg.q-alloc + (oe-ordl.qty * tt-fg-set.part-qty-dec).*/
                RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT itemfg.q-alloc, OUTPUT v-q-back).

            IF xitemfg.q-alloc LT 0 THEN xitemfg.q-alloc = 0.
    
            ASSIGN
                xitemfg.q-avail = xitemfg.q-onh + xitemfg.q-ono - xitemfg.q-alloc.
            IF NOT v-run-from-steps2 THEN
                ASSIGN 
                    xitemfg.q-ptd     = xitemfg.q-ptd     + (oe-ordl.qty * tt-fg-set.part-qty-dec)
                    xitemfg.q-ord-ytd = xitemfg.q-ord-ytd + (oe-ordl.qty * tt-fg-set.part-qty-dec).
            FIND CURRENT xitemfg NO-LOCK NO-ERROR.
            RELEASE xitemfg.
        END.
    END. /* isaset */

    ELSE 
    DO:
        FIND FIRST eb
            WHERE eb.company = oe-ordl.company
            AND eb.est-no    EQ oe-ordl.est-no
            AND eb.form-no  EQ oe-ordl.form-no
            AND eb.blank-no EQ oe-ordl.blank-no
            NO-LOCK NO-ERROR.
        {sys/inc/updfgdim.i "eb"}
    END.
    FIND CURRENT itemfg NO-LOCK NO-ERROR.
    RELEASE itemfg.
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
