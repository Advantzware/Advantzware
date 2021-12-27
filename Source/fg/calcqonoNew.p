
/*------------------------------------------------------------------------
    File        : calcqonoNew.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Dec 21 01:51:16 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-q-ono LIKE itemfg.q-ono NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEFINE VARIABLE ld-qty     LIKE fg-bin.qty  NO-UNDO.
DEFINE VARIABLE v-set-item LIKE itemfg.i-no NO-UNDO.

DEFINE VARIABLE v-part-qty     AS INTEGER NO-UNDO.
DEFINE VARIABLE v-set-job-qty  AS INTEGER NO-UNDO.
DEFINE VARIABLE v-set-rcv-qty  AS INTEGER NO-UNDO.
DEFINE VARIABLE v-part-qty-dec AS DECIMAL NO-UNDO.

{fg/fullset.i NEW}

DEFINE BUFFER b-itemfg   FOR itemfg.
DEFINE BUFFER bf-eb      FOR eb.
DEFINE BUFFER bf-itemfg  FOR itemfg.
DEFINE BUFFER bf-est     FOR est.
DEFINE BUFFER bf-po-ordl FOR po-ordl.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL itemfg THEN 
DO:
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company   EQ itemfg.company
        AND po-ordl.i-no      EQ itemfg.i-no
/*        AND po-ordl.job-no    EQ ""*/
        AND po-ordl.item-type EQ NO
        AND po-ordl.opened    EQ YES
        AND po-ordl.stat      NE "C",
        FIRST po-ord WHERE
        po-ord.company EQ itemfg.company AND
        po-ord.po-no   EQ po-ordl.po-no
        NO-LOCK:
        FIND FIRST job-hdr WHERE
            job-hdr.company EQ itemfg.company AND
            job-hdr.i-no    EQ itemfg.i-no AND
            job-hdr.opened  EQ YES
            NO-LOCK NO-ERROR.
            
        IF AVAILABLE job-hdr THEN 
        DO:
            FOR EACH job-hdr WHERE
                job-hdr.company EQ itemfg.company AND
                job-hdr.i-no    EQ itemfg.i-no AND
                job-hdr.opened  EQ YES AND
                CAN-FIND(FIRST job WHERE
                job.company EQ job-hdr.company AND
                job.job     EQ job-hdr.job AND
                job.job-no  EQ job-hdr.job-no AND
                job.job-no2 EQ job-hdr.job-no2)
                USE-INDEX i-no
                NO-LOCK:
                
                IF CAN-FIND(FIRST bf-po-ordl WHERE bf-po-ordl.i-no = job-hdr.i-no
                                               AND bf-po-ordl.s-num = job-hdr.frm
                                               AND bf-po-ordl.b-num = job-hdr.blank-no NO-LOCK) THEN
                    NEXT.
                
                RELEASE eb.

                IF NOT itemfg.isaset THEN
                    FIND FIRST eb WHERE 
                        eb.company  EQ job-hdr.company AND
                        eb.est-no   EQ job-hdr.est-no AND
                        eb.form-no  EQ job-hdr.frm AND
                        eb.blank-no EQ job-hdr.blank-no
                        NO-LOCK NO-ERROR.
     
                IF NOT(itemfg.isaset OR (AVAIL eb AND NOT eb.pur-man) OR
                    (NOT AVAIL eb AND NOT itemfg.pur-man)) THEN
                    NEXT.

                ld-qty = 0.
                RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                    INPUT job-hdr.job-no,
                    INPUT job-hdr.job-no2,
                    INPUT job-hdr.i-no,
                    INPUT NO,
                    OUTPUT ld-qty).
      
                IF ld-qty LT job-hdr.qty THEN
                    op-q-ono = op-q-ono + (job-hdr.qty - ld-qty).
        
            END. /* FOR EACH job-hdr */      
            
            /*Below logic should be added if there is old version calculation*/
            
            /*Add here the check for the calculation type "JobBuildVersion" */
            FOR EACH reftable NO-LOCK
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                AND reftable.company  EQ itemfg.company
                AND reftable.loc      EQ ""
                AND reftable.code2    EQ itemfg.i-no
                USE-INDEX code2,
                EACH job NO-LOCK
                WHERE job.company EQ itemfg.company
                AND job.job EQ INTEGER(reftable.CODE)
                AND job.opened  EQ YES 
                USE-INDEX job,
                FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ job.company
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                USE-INDEX job-no,
                FIRST b-itemfg NO-LOCK
                WHERE b-itemfg.company EQ job-hdr.company
                AND b-itemfg.i-no    EQ job-hdr.i-no
                AND b-itemfg.isaset  EQ YES:

                RUN fg/fullset.p (ROWID(b-itemfg)).
          
                FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ reftable.code2:
                    ld-qty = 0.
                    RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                        INPUT job-hdr.job-no,
                        INPUT job-hdr.job-no2,
                        INPUT job-hdr.i-no,
                        INPUT NO,
                        OUTPUT ld-qty).
                    IF ld-qty LT job-hdr.qty * tt-fg-set.part-qty-dec THEN
                        op-q-ono = op-q-ono + ((job-hdr.qty * tt-fg-set.part-qty-dec) - 
                            ld-qty).
              
                END. /* FOR EACH tt-fg-set*/
            END.
        END. /*available job-hdr*/
        ELSE 
        DO:
            FOR EACH bf-eb FIELDS(est-no) WHERE
                bf-eb.company  EQ itemfg.company AND
                bf-eb.stock-no EQ itemfg.i-no
                NO-LOCK:

                FOR EACH job-hdr FIELDS(company i-no) WHERE
                    job-hdr.company EQ itemfg.company AND
                    job-hdr.est-no  EQ bf-eb.est-no AND
                    job-hdr.opened  EQ YES
                    NO-LOCK:
          
                    FIND FIRST bf-itemfg WHERE
                        bf-itemfg.company EQ job-hdr.company AND
                        bf-itemfg.i-no    EQ job-hdr.i-no AND
                        bf-itemfg.isaset
                        NO-LOCK NO-ERROR.
          
                    IF AVAIL bf-itemfg THEN LEAVE.
                END.
            END.
     
            IF AVAIL bf-itemfg THEN 
            DO:
          
                IF bf-itemfg.alloc EQ YES THEN 
                DO:
                    FOR EACH job-hdr WHERE
                        job-hdr.company EQ itemfg.company AND
                        job-hdr.i-no    EQ bf-itemfg.i-no AND
                        job-hdr.opened  EQ YES AND
                        CAN-FIND(FIRST job WHERE
                        job.company EQ job-hdr.company AND
                        job.job     EQ job-hdr.job AND
                        job.job-no  EQ job-hdr.job-no AND
                        job.job-no2 EQ job-hdr.job-no2)
                        NO-LOCK
                        USE-INDEX i-no:
         
                        FIND FIRST bf-est WHERE
                            bf-est.company EQ job-hdr.company AND
                            bf-est.est-no EQ job-hdr.est-no
                            NO-LOCK NO-ERROR.

                        IF NOT AVAIL bf-est THEN NEXT.

                        IF bf-est.est-type NE 6 AND bf-est.est-type NE 2 THEN
                            FIND FIRST eb WHERE
                                eb.company  EQ job-hdr.company AND
                                eb.est-no   EQ job-hdr.est-no AND
                                eb.form-no  EQ job-hdr.frm AND
                                eb.blank-no EQ job-hdr.blank-no
                                NO-LOCK NO-ERROR.
                        ELSE
                        DO:
                            FIND FIRST eb WHERE
                                eb.company  EQ job-hdr.company AND
                                eb.est-no   EQ job-hdr.est-no AND
                                eb.form-no  EQ job-hdr.frm AND
                                eb.stock-no EQ itemfg.i-no
                                NO-LOCK NO-ERROR.

                            IF NOT AVAIL eb THEN
                                FIND FIRST eb WHERE
                                    eb.company  EQ job-hdr.company AND
                                    eb.est-no   EQ job-hdr.est-no AND
                                    eb.stock-no EQ itemfg.i-no
                                    NO-LOCK NO-ERROR.
                        END.

                        IF NOT((AVAIL eb AND NOT eb.pur-man) OR
                            (NOT AVAIL eb AND NOT itemfg.pur-man)) THEN
                            NEXT.

                        v-part-qty = 1.
                        FIND FIRST fg-set
                            WHERE fg-set.company EQ itemfg.company
                            AND fg-set.part-no EQ itemfg.i-no
                            NO-LOCK NO-ERROR.
                        IF AVAIL(fg-set) THEN 
                        DO:
                            {sys/inc/part-qty.i v-part-qty fg-set}
                            v-part-qty = 
                                (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE v-part-qty).
   
                        END.
    
                        ld-qty = 0.
                        RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                            INPUT job-hdr.job-no,
                            INPUT job-hdr.job-no2,
                            INPUT job-hdr.i-no,
                            INPUT NO,
                            OUTPUT ld-qty).
                        IF ld-qty LT job-hdr.qty THEN
                            op-q-ono = op-q-ono + ((job-hdr.qty - ld-qty) * v-part-qty).
             
                    END. /* FOR EACH job-hdr */         

                    FOR EACH reftable NO-LOCK
                        WHERE reftable.reftable EQ "jc/jc-calc.p"
                        AND reftable.company  EQ itemfg.company
                        AND reftable.loc      EQ ""
                        AND reftable.code2    EQ itemfg.i-no
                        USE-INDEX code2,
                        EACH job NO-LOCK
                        WHERE job.company EQ itemfg.company
                        AND job.job EQ INTEGER(reftable.CODE)
                        AND job.opened  EQ YES 
                        USE-INDEX job,
                        FIRST job-hdr NO-LOCK
                        WHERE job-hdr.company EQ job.company
                        AND job-hdr.job     EQ job.job
                        AND job-hdr.job-no  EQ job.job-no
                        AND job-hdr.job-no2 EQ job.job-no2
                        USE-INDEX job-no,
                        FIRST b-itemfg NO-LOCK
                        WHERE b-itemfg.company EQ job-hdr.company
                        AND b-itemfg.i-no    EQ job-hdr.i-no
                        AND b-itemfg.isaset  EQ YES:

                        RUN fg/fullset.p (ROWID(b-itemfg)).
         
                        FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ reftable.code2:
                            ld-qty = 0.
                            RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                                INPUT job-hdr.job-no,
                                INPUT job-hdr.job-no2,
                                INPUT job-hdr.i-no,
                                INPUT NO,
                                OUTPUT ld-qty).
                            IF ld-qty LT job-hdr.qty * tt-fg-set.part-qty-dec THEN
                                op-q-ono = op-q-ono + ((job-hdr.qty * tt-fg-set.part-qty-dec) - 
                                    ld-qty).
               
                        END. /* FOR EACH tt-fg-set*/
                    END. /* FOR EACH job */
         
                END.  /* if not assembled set */
                ELSE 
                DO:
                    /* assembled set logic */
            
                    v-set-item = bf-itemfg.i-no.
                    FOR EACH job-hdr WHERE
                        job-hdr.company EQ itemfg.company AND
                        job-hdr.i-no    EQ bf-itemfg.i-no AND
                        job-hdr.opened  EQ YES AND
                        CAN-FIND(FIRST job WHERE
                        job.company EQ job-hdr.company AND
                        job.job     EQ job-hdr.job AND
                        job.job-no  EQ job-hdr.job-no AND
                        job.job-no2 EQ job-hdr.job-no2)
                        NO-LOCK
                        USE-INDEX i-no:
         
                        FIND FIRST bf-est WHERE
                            bf-est.company EQ job-hdr.company AND
                            bf-est.est-no EQ job-hdr.est-no
                            NO-LOCK NO-ERROR.

                        IF NOT AVAIL bf-est THEN NEXT.

                        IF bf-est.est-type NE 6 AND bf-est.est-type NE 2 THEN
                            FIND FIRST eb WHERE
                                eb.company  EQ job-hdr.company AND
                                eb.est-no   EQ job-hdr.est-no AND
                                eb.form-no  EQ job-hdr.frm AND
                                eb.blank-no EQ job-hdr.blank-no
                                NO-LOCK NO-ERROR.
                        ELSE
                        DO:
                            FIND FIRST eb WHERE
                                eb.company  EQ job-hdr.company AND
                                eb.est-no   EQ job-hdr.est-no AND
                                eb.form-no  EQ job-hdr.frm AND
                                eb.stock-no EQ itemfg.i-no
                                NO-LOCK NO-ERROR.

                            IF NOT AVAIL eb THEN
                                FIND FIRST eb WHERE
                                    eb.company  EQ job-hdr.company AND
                                    eb.est-no   EQ job-hdr.est-no AND
                                    eb.stock-no EQ itemfg.i-no
                                    NO-LOCK NO-ERROR.
                        END.

                        IF NOT((AVAIL eb AND NOT eb.pur-man) OR
                            (NOT AVAIL eb AND NOT itemfg.pur-man)) THEN
                            NEXT.

                        ld-qty = 0.
                        RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                            INPUT job-hdr.job-no,
                            INPUT job-hdr.job-no2,
                            INPUT job-hdr.i-no,
                            INPUT NO,
                            OUTPUT ld-qty).
                        IF ld-qty LT job-hdr.qty THEN
                            ASSIGN v-set-job-qty = job-hdr.qty
                                v-set-rcv-qty =  ld-qty.
                    END. /* FOR EACH job-hdr */
 
                    FOR EACH job-hdr NO-LOCK
                        WHERE job-hdr.company EQ itemfg.company
                        AND job-hdr.i-no    EQ v-set-item,
                        FIRST job NO-LOCK
                        WHERE job.company EQ job-hdr.company
                        AND job.job     EQ job-hdr.job
                        AND job.job-no  EQ job-hdr.job-no
                        AND job.job-no2 EQ job-hdr.job-no2
                        AND job.opened  EQ YES,
                        FIRST b-itemfg NO-LOCK
                        WHERE b-itemfg.company EQ job-hdr.company
                        AND b-itemfg.i-no    EQ job-hdr.i-no
                        AND b-itemfg.isaset  EQ YES:
                   
                        RUN fg/fullset.p (ROWID(b-itemfg)).
           
                        FOR EACH  fg-set NO-LOCK
                            WHERE fg-set.company EQ itemfg.company
                            AND fg-set.part-no EQ itemfg.i-no:
                            {sys/inc/part-qty.i v-part-qty-dec fg-set}
                            IF v-set-rcv-qty LT v-set-job-qty THEN
                                op-q-ono = op-q-ono + ((v-set-job-qty - v-set-rcv-qty) * v-part-qty-dec).               
                        END. /* FOR EACH tt-fg-set*/
                    END. /* FOR EACH job */           
         
                END.
            END.
        END. /*Not available */    
            
    END.        
END.