/* copyOrder.i */

PROCEDURE copyOrder:
  DEFINE INPUT PARAMETER ipFromCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFromOrdNo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipToOrdNo AS INTEGER NO-UNDO.    

  DEF BUFFER b-oe-ord  FOR oe-ord.
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-oe-ordm FOR oe-ordm.
  DEF BUFFER b-oe-rel  FOR oe-rel.
  DEF BUFFER b-notes   FOR notes.
  DEF BUFFER b-reftable FOR reftable.

  DEF VAR lv-rec_key LIKE oe-ord.rec_key NO-UNDO.
  DEF VAR lv-r-no LIKE oe-rel.r-no NO-UNDO.
  DEF VAR n-est-no LIKE est.est-no NO-UNDO.

  FIND FIRST oe-ord
      WHERE oe-ord.company EQ ipFromCompany
        AND oe-ord.ord-no  EQ ipFromOrdNo
      NO-LOCK NO-ERROR.

  lv-rec_key = STRING(TODAY,"99999999") +
               STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
  CREATE rec_key.
  ASSIGN
   rec_key.rec_key    = lv-rec_key
   rec_key.table_name = "oe-ord".
      
  CREATE b-oe-ord.
  BUFFER-COPY oe-ord EXCEPT rec_key job-no job-no2 est-no TO b-oe-ord
  ASSIGN
   b-oe-ord.company = ipToCompany
   b-oe-ord.ord-no  = ipToOrdNo.
  IF ipToCompany EQ ipFromCompany THEN
  ASSIGN 
    b-oe-ord.est-no  = n-est-no.
  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no
      EXCLUSIVE-LOCK:

    CREATE b-oe-ordl.
    BUFFER-COPY oe-ordl EXCEPT rec_key job-no job-no2 TO b-oe-ordl
    ASSIGN
     b-oe-ordl.company = b-oe-ord.company
     b-oe-ordl.ord-no  = b-oe-ord.ord-no.    
    IF ipFromCompany NE ipToCompany THEN
      RUN copyEst (ipFromCompany,ipToCompany,oe-ordl.est-no,b-oe-ordl.i-no,b-oe-ordl.part-no,b-oe-ordl.ord-no, OUTPUT n-est-no).
    ELSE
      n-est-no = oe-ordl.est-no.

    RUN copyFg (ipFromCompany,ipToCompany,b-oe-ordl.i-no, n-est-no, oe-ordl.est-no).
    ASSIGN
      fil_id    = RECID(oe-ordl).
    RUN copyJob (ipFromCompany,ipToCompany,n-est-no,b-oe-ord.ord-no,b-oe-ord.loc).
    
    ASSIGN b-oe-ordl.est-no  = n-est-no
             /*oe-ordl.stat      = "C"
               oe-ordl.opened    = NO*/.

  END.
  IF ipToCompany NE ipFromCompany THEN
      ASSIGN 
        b-oe-ord.est-no  = n-est-no.

  FOR EACH oe-ordm
      WHERE oe-ordm.company EQ oe-ord.company
        AND oe-ordm.ord-no EQ oe-ord.ord-no
      NO-LOCK:

    CREATE b-oe-ordm.
    BUFFER-COPY oe-ordm EXCEPT rec_key TO b-oe-ordm
    ASSIGN
     b-oe-ordm.company = b-oe-ord.company
     b-oe-ordm.ord-no  = b-oe-ord.ord-no.
  END.
    
  FOR EACH oe-rel
      WHERE oe-rel.company EQ oe-ord.company
        AND oe-rel.ord-no    EQ oe-ord.ord-no
      NO-LOCK:

      /* FIND FIRST b-oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR. */
      /* lv-r-no = (IF AVAIL b-oe-rel THEN b-oe-rel.r-no ELSE 0) + 1.  */
      RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT lv-r-no).
      CREATE b-oe-rel.
      BUFFER-COPY oe-rel EXCEPT rec_key oe-rel.spare-dec-1 TO b-oe-rel
      ASSIGN
       b-oe-rel.company = b-oe-ord.company
       b-oe-rel.r-no    = lv-r-no
       b-oe-rel.ord-no  = b-oe-ord.ord-no.
     
      

      
      RUN fg/fgitmloc.p (INPUT b-oe-rel.i-no, INPUT ROWID(b-oe-rel)).

  END.

  FOR EACH notes WHERE notes.rec_key EQ oe-ord.rec_key NO-LOCK:
    CREATE b-notes.
    BUFFER-COPY notes EXCEPT rec_key TO b-notes
    ASSIGN b-notes.rec_key = lv-rec_key.
  END.
    
  RUN copyCust (ipFromCompany,ipToCompany,b-oe-ord.cust-no).

END PROCEDURE.


/***********************************************************************************/
PROCEDURE copyJob:
    DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipOrdno AS INT NO-UNDO.
    DEFINE INPUT PARAMETER ipLoc AS CHARACTER NO-UNDO.        
    
    def var v-job-no like oe-ord.job-no no-undo.
    def var v-job-no2 like oe-ord.job-no2 no-undo.
    DEFINE VARIABLE v-prod-cat AS CHARACTER  NO-UNDO.   
    def var lv-job-recid as recid no-undo.
    def var choice as log no-undo.
    def var hld-id as recid no-undo.
    def var hld-stat like job.stat no-undo.
    def var hld-nufile as log no-undo.
    DEF VAR v-run-schedule AS LOG NO-UNDO.

    FIND CURRENT oe-ord.
    DEF BUFFER b-oe-ordl1 FOR oe-ordl.
    DEF BUFFER b-oe-ord1 FOR oe-ord.   

    FIND FIRST eb WHERE eb.est-no = ipEstno 
                  AND eb.company = ipToCompany NO-LOCK NO-ERROR.
    IF AVAIL eb THEN
        ASSIGN
            v-prod-cat = eb.procat.
    ASSIGN
        cocode     = ipToCompany.

    v-job-no = fill(" ",6 - length(trim(string(ipOrdno)))) + string(ipOrdno).
        RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-prod-cat).
         
    IF v-job-no EQ "" THEN
      v-job-no = fill(" ",6 - length(trim(ipEstno))) + trim(ipEstno).

    IF v-job-no NE "" THEN DO:
          FIND FIRST job NO-LOCK
              WHERE job.company EQ ipToCompany
                AND job.job-no  EQ v-job-no
                AND job.job-no2 EQ v-job-no2
              NO-ERROR.
         
          IF AVAIL job AND TRIM(job.est-no) NE TRIM(ipEstno) THEN
            IF CAN-FIND(FIRST job-hdr
                        WHERE job-hdr.company EQ job.company
                          AND job-hdr.job     EQ job.job
                          AND job-hdr.job-no  EQ job.job-no
                          AND job-hdr.job-no2 EQ job.job-no2
                          AND job-hdr.ord-no  NE ipOrdno) OR
               CAN-FIND(FIRST b-oe-ord1
                        WHERE b-oe-ord1.company EQ job.company
                          AND b-oe-ord1.job-no  EQ job.job-no
                          AND b-oe-ord1.job-no2 EQ job.job-no2
                          AND b-oe-ord1.est-no  EQ job.est-no)   OR
               CAN-FIND(FIRST b-oe-ordl1
                        WHERE b-oe-ordl1.company EQ job.company
                          AND b-oe-ordl1.job-no  EQ job.job-no
                          AND b-oe-ordl1.job-no2 EQ job.job-no2
                          AND b-oe-ordl1.est-no  EQ job.est-no)  THEN RELEASE job.
            ELSE
            DO TRANSACTION:
              FIND CURRENT job NO-ERROR.
              IF AVAIL job THEN DELETE job.
            END.
         
          IF NOT AVAIL job THEN DO:
            RUN create-ord-job (ipFrmCompany,ipToCompany,ipEstno,ipOrdno,v-job-no,v-job-no2,ipLoc,OUTPUT lv-job-recid).
            FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
          END.        

          v-qty-mod = YES.

          IF AVAIL job AND INDEX("HWPRL",job.stat) NE 0 THEN DO:
            /*IF NOT v-qty-mod THEN
               RUN oe/job-qty.p (ROWID(oe-ord), OUTPUT v-qty-mod).*/
         
            IF v-qty-mod OR job.stat EQ "P" THEN DO:
              RUN jc/chkrebld.p (RECID(job), OUTPUT choice).     
              IF NOT choice THEN DO:
                ASSIGN hld-id     = fil_id
                       hld-nufile = nufile 
                       hld-stat   = job.stat
                       nufile     = YES.
         
                RUN jc/jc-calc.p(RECID(job), NO).
                ASSIGN fil_id   = hld-id
                       nufile   = hld-nufile.
               
                IF hld-stat NE "P" THEN DO TRANSACTION:
                  FIND CURRENT job EXCLUSIVE.
                  job.stat = hld-stat.
                  FIND CURRENT job NO-LOCK.
                END.
              END.
            END.
          END.
                
          find first sys-ctrl where
               sys-ctrl.company eq cocode AND
               sys-ctrl.name    eq "SCHEDULE"
               no-lock no-error.

          v-run-schedule = IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO
                           ELSE IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES
                           ELSE NO.

          FOR EACH oe-ordl NO-LOCK
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ ipOrdno
                AND oe-ordl.is-a-component EQ NO
         
              BREAK BY oe-ordl.job-no
                    BY oe-ordl.job-no2:
         
            IF LAST-OF(oe-ordl.job-no2) THEN DO:
              ASSIGN
               hld-id     = fil_id
               hld-nufile = nufile
               fil_id     = RECID(oe-ordl).
             
              RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.
              /* check oe-ordl.due-date and calc promised date and job's start-date */

              IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date.
              
              ASSIGN
               fil_id = hld-id
               nufile = hld-nufile.
            END.
          END.
    END.
END PROCEDURE.


/**************************************************/
PROCEDURE copyFg:
  DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipIno AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFEstno AS CHARACTER NO-UNDO.

  DEF BUFFER b-itemfg        FOR itemfg.
  DEF BUFFER b-cust-part     FOR cust-part.
  DEF BUFFER b-fg-set        FOR fg-set.
  DEF BUFFER s-fg-set        FOR fg-set.    
  DEF BUFFER b-itemfg-ink    FOR itemfg-ink.
  DEF BUFFER b-itemfg-bom    FOR itemfg-bom.
  DEF BUFFER b-itemfg-loc    FOR itemfg-loc.
  DEF BUFFER b-e-itemfg      FOR e-itemfg.
  DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
  DEF BUFFER b-itemfgdtl     FOR itemfgdtl.
  DEF BUFFER b-ref           FOR reftable.
  DEF BUFFER b-notes         FOR notes.

  DEF VAR lv-rec_key LIKE itemfg.rec_key NO-UNDO.
  def var x as int no-undo.
  def var y as int no-undo.
  

  SESSION:SET-WAIT-STATE("general").

  FOR EACH eb WHERE eb.est-no = ipFEstno 
              AND eb.company = ipFrmCompany NO-LOCK:  
  FIND FIRST b-itemfg
      WHERE b-itemfg.company EQ ipToCompany
        AND b-itemfg.i-no    EQ eb.stock-no
      NO-LOCK NO-ERROR.  

  IF NOT AVAIL b-itemfg THEN DO: 
    FIND FIRST itemfg
      WHERE itemfg.company EQ ipFrmCompany
        AND itemfg.i-no    EQ eb.stock-no
      NO-LOCK NO-ERROR.

    IF AVAIL itemfg THEN DO:    

        lv-rec_key = STRING(TODAY,"99999999") +
                  STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
        CREATE rec_key.
        ASSIGN
            rec_key.rec_key    = lv-rec_key
            rec_key.table_name = "ITEMFG".
      
        CREATE b-itemfg.
        BUFFER-COPY itemfg EXCEPT rec_key est-no TO b-itemfg
        ASSIGN
         b-itemfg.company    = ipToCompany
         b-itemfg.i-no       = eb.stock-no
         b-itemfg.beg-bal    = 0
         b-itemfg.beg-date   = TODAY
         b-itemfg.q-ptd      = 0
         b-itemfg.q-ord-ytd  = 0
         b-itemfg.u-ord      = 0
         b-itemfg.q-prod-ptd = 0
         b-itemfg.q-prod-ytd = 0
         b-itemfg.u-prod     = 0
         b-itemfg.q-ship-ptd = 0
         b-itemfg.q-ship-ytd = 0
         b-itemfg.u-ship     = 0
         b-itemfg.q-inv-ptd  = 0
         b-itemfg.q-inv-ytd  = 0
         b-itemfg.u-inv      = 0
         b-itemfg.ytd-msf    = 0
         b-itemfg.lyytd-msf  = 0
         b-itemfg.est-no     = ipEstno.

        FOR EACH cust-part
            WHERE cust-part.company EQ itemfg.company
              AND cust-part.i-no    EQ itemfg.i-no
            NO-LOCK:

           CREATE b-cust-part.
           BUFFER-COPY cust-part EXCEPT rec_key TO b-cust-part
           ASSIGN
            b-cust-part.company = b-itemfg.company
            b-cust-part.i-no    = b-itemfg.i-no.
        END.      

        FOR EACH fg-set
            WHERE fg-set.company EQ itemfg.company
              AND fg-set.set-no  EQ itemfg.i-no
            NO-LOCK:     

            find last s-fg-set where s-fg-set.company = b-itemfg.company use-index s-no no-lock no-error.
              x = if avail s-fg-set then s-fg-set.s-no + 1 else 1.
            find last s-fg-set where s-fg-set.set-no = b-itemfg.i-no AND s-fg-set.company = b-itemfg.company no-lock no-error.
              y = if avail s-fg-set then s-fg-set.line + 1 else 1. 

           CREATE b-fg-set.                                
           BUFFER-COPY fg-set EXCEPT rec_key s-no line TO b-fg-set 
           ASSIGN
            b-fg-set.company = b-itemfg.company
            b-fg-set.set-no  = b-itemfg.i-no
            b-fg-set.s-no    = x
            b-fg-set.line    = y.
        END.
    
        FOR EACH itemfg-ink
            WHERE itemfg-ink.company EQ itemfg.company
              AND itemfg-ink.i-no    EQ itemfg.i-no
            NO-LOCK:

           CREATE b-itemfg-ink.
           BUFFER-COPY itemfg-ink EXCEPT rec_key TO b-itemfg-ink
           ASSIGN
            b-itemfg-ink.company = b-itemfg.company
            b-itemfg-ink.i-no    = b-itemfg.i-no.


        END.
    
        FOR EACH itemfg-bom
            WHERE itemfg-bom.company  EQ itemfg.company
              AND itemfg-bom.parent-i EQ itemfg.i-no
            NO-LOCK:
           CREATE b-itemfg-bom.
           BUFFER-COPY itemfg-bom EXCEPT rec_key TO b-itemfg-bom
           ASSIGN
            b-itemfg-bom.company  = b-itemfg.company
            b-itemfg-bom.parent-i = b-itemfg.i-no.
        END.

        FOR EACH itemfg-loc
            WHERE itemfg-loc.company EQ itemfg.company
              AND itemfg-loc.i-no    EQ itemfg.i-no
            NO-LOCK:
           CREATE b-itemfg-loc.
           BUFFER-COPY itemfg-loc EXCEPT rec_key to b-itemfg-loc
           ASSIGN
            b-itemfg-loc.company = b-itemfg.company
            b-itemfg-loc.i-no    = b-itemfg.i-no.
        END.

        FOR EACH e-itemfg
            WHERE e-itemfg.company EQ itemfg.company
              AND e-itemfg.i-no    EQ itemfg.i-no
            NO-LOCK:
           CREATE b-e-itemfg.
           BUFFER-COPY e-itemfg EXCEPT rec_key TO b-e-itemfg
           ASSIGN
            b-e-itemfg.company = b-itemfg.company
            b-e-itemfg.i-no    = b-itemfg.i-no.
        END.
     
        FOR EACH e-itemfg-vend
            WHERE e-itemfg-vend.company EQ itemfg.company
              AND e-itemfg-vend.i-no    EQ itemfg.i-no
            NO-LOCK:
           CREATE b-e-itemfg-vend.
           BUFFER-COPY e-itemfg-vend EXCEPT rec_key est-no TO b-e-itemfg-vend
           ASSIGN
            b-e-itemfg-vend.company = b-itemfg.company
            b-e-itemfg-vend.i-no    = b-itemfg.i-no
            b-e-itemfg-vend.est-no  = ipEstno.
        END.
    
        FOR EACH itemfgdtl
            WHERE itemfgdtl.company EQ itemfg.company
              AND itemfgdtl.i-no    EQ itemfg.i-no
            NO-LOCK:

           CREATE b-itemfgdtl.
           BUFFER-COPY itemfgdtl EXCEPT rec_key est-no TO b-itemfgdtl
           ASSIGN
            b-itemfgdtl.company = b-itemfg.company
            b-itemfgdtl.i-no    = b-itemfg.i-no
            b-itemfgdtl.est-no  = ipEstno.
        END.

        FOR EACH reftable
            WHERE reftable.reftable EQ "FGSTATUS"
              AND reftable.company  EQ itemfg.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ itemfg.i-no
            NO-LOCK:
           CREATE b-ref.
           BUFFER-COPY reftable EXCEPT rec_key TO b-ref
           ASSIGN
            b-ref.company = b-itemfg.company
            b-ref.code    = b-itemfg.i-no.
        END.

        FOR EACH reftable
            WHERE reftable.reftable EQ "itemfg.exempt-disc"
              AND reftable.company  EQ itemfg.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ itemfg.i-no
            NO-LOCK:
           CREATE b-ref.
           BUFFER-COPY reftable EXCEPT rec_key TO b-ref
           ASSIGN
            b-ref.company = b-itemfg.company
            b-ref.code    = b-itemfg.i-no.
        END.

        FOR EACH notes WHERE notes.rec_key EQ itemfg.rec_key NO-LOCK:
          CREATE b-notes.
          BUFFER-COPY notes TO b-notes
          ASSIGN b-notes.rec_key = lv-rec_key.
        END.

        cocode = b-itemfg.company.
        RUN fg/fg-reset.p (RECID(b-itemfg)).
    END.
  END.
  END.
END PROCEDURE.


PROCEDURE copyCust:
  DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipCustno AS CHARACTER NO-UNDO.

  DEF BUFFER b-cust        FOR cust.
  DEF BUFFER b-cust-markup FOR cust-markup.
  DEF BUFFER b-shipto      FOR shipto.
  DEF BUFFER b-soldto      FOR soldto.
  DEF BUFFER b-notes       FOR notes.

  DEF VAR lv-rec_key LIKE cust.rec_key NO-UNDO.
  

  SESSION:SET-WAIT-STATE("general").

  FIND FIRST b-cust
      WHERE b-cust.company EQ ipToCompany
        AND b-cust.cust-no EQ ipCustno
      NO-LOCK NO-ERROR. 

  IF NOT AVAIL b-cust THEN DO:
      
      FIND FIRST cust
      WHERE cust.company EQ ipFrmCompany
        AND cust.cust-no EQ ipCustno
      NO-LOCK NO-ERROR.

      IF AVAIL cust THEN DO:
      
        lv-rec_key = STRING(TODAY,"99999999") +
               STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
        CREATE rec_key.
         ASSIGN
          rec_key.rec_key    = lv-rec_key
          rec_key.table_name = "CUST".
           
         CREATE b-cust.
         BUFFER-COPY cust EXCEPT rec_key TO b-cust
         ASSIGN
          b-cust.company    = ipToCompany
          b-cust.cust-no    = ipCustno
          b-cust.ytd-sales  = 0
          b-cust.lyr-sales  = 0
          b-cust.cost       = 0
          b-cust.comm       = 0
          b-cust.ytd-msf    = 0
          b-cust.lyytd-msf  = 0
          b-cust.hibal      = 0
          b-cust.hibal-date = ?
          b-cust.num-inv    = 0
          b-cust.lpay       = 0
          b-cust.lpay-date  = ?
          b-cust.avg-pay    = 0
          b-cust.ord-bal    = 0
          b-cust.acc-bal    = 0
          b-cust.on-account = 0.
         
         FOR EACH cust-markup
             WHERE cust-markup.company EQ cust.company
               AND cust-markup.cust-no    EQ cust.cust-no
             NO-LOCK:
         
            CREATE b-cust-markup.
            BUFFER-COPY cust-markup EXCEPT rec_key TO b-cust-markup
            ASSIGN
             b-cust-markup.company = b-cust.company
             b-cust-markup.cust-no = b-cust.cust-no.
         END.
         
         FOR EACH shipto
             WHERE shipto.company EQ cust.company
               AND shipto.cust-no EQ cust.cust-no
             NO-LOCK:

            CREATE b-shipto.
            BUFFER-COPY shipto EXCEPT rec_key TO b-shipto
            ASSIGN
             b-shipto.company = b-cust.company
             b-shipto.cust-no = b-cust.cust-no.
         END.
         
         FOR EACH soldto
             WHERE soldto.company EQ cust.company
               AND soldto.cust-no    EQ cust.cust-no
             NO-LOCK:
           CREATE b-soldto.
           BUFFER-COPY soldto EXCEPT rec_key TO b-soldto
           ASSIGN
            b-soldto.company = b-cust.company
            b-soldto.cust-no    = b-cust.cust-no.
         END.
         
         FOR EACH notes WHERE notes.rec_key EQ cust.rec_key NO-LOCK:
           CREATE b-notes.
           BUFFER-COPY notes TO b-notes
           ASSIGN b-notes.rec_key = lv-rec_key.
         END.
      END.
  END.
END PROCEDURE.         

PROCEDURE copyEst:
    DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipIno AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipPartno AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipOrdno AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER n-est-no like est.est-no.
         
    def buffer boxd-hdr for box-design-hdr.
    def buffer boxd-lin for box-design-line.
    def buffer reftable1 for reftable.
    def buffer b-est for est.
    def buffer b-ef for ef.
    def buffer b-eb for eb.   
    def buffer b-eb1 for eb.
    def buffer b-est-prep for est-prep.    
    def buffer b-est-inst for est-inst.
    def buffer b-est-flm for est-flm.
    def buffer b-est-qty for est-qty.
    def buffer b-notes for notes.
    def buffer reftable2 for reftable.    
    def buffer b-probe for probe.

    def var v-est-no like est.est-no.
    DEF VAR lv-rec_key LIKE est.rec_key NO-UNDO.
  

    SESSION:SET-WAIT-STATE("general").

    find first ce-ctrl WHERE ce-ctrl.company = ipToCompany EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN
           ce-ctrl.e-num = ce-ctrl.e-num + 1
           v-est-no      = STRING(ce-ctrl.e-num,">>>>>>>9").    

    FIND FIRST b-eb
      WHERE b-eb.company EQ ipToCompany
        AND b-eb.part-no EQ ipPartno
        AND b-eb.stock-no EQ ipIno
      NO-LOCK NO-ERROR.

    IF NOT AVAIL b-eb THEN DO:    
        FIND FIRST est
          WHERE est.company EQ ipFrmCompany
            AND est.est-no EQ ipEstno
          NO-LOCK NO-ERROR.        
        
        IF AVAIL est THEN DO:

            ASSIGN
                n-est-no   = v-est-no
                lv-rec_key = STRING(TODAY,"99999999") +
                      STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
            CREATE rec_key.
            ASSIGN
                rec_key.rec_key    = lv-rec_key
                rec_key.table_name = "EST".                   
        
            CREATE b-est.
            BUFFER-COPY est EXCEPT rec_key est-no ord-no TO b-est
            ASSIGN
                b-est.company    = ipToCompany
                b-est.est-no     = v-est-no
                b-est.ord-no     = ipOrdno.           
        
            FOR EACH ef
                WHERE ef.company EQ est.company
                AND ef.est-no    EQ est.est-no
                NO-LOCK:
        
               CREATE b-ef.
               BUFFER-COPY ef EXCEPT rec_key est-no TO b-ef
               ASSIGN
                b-ef.company = b-est.company
                b-ef.est-no  = v-est-no.


               FOR EACH reftable
                  where reftable.reftable eq "EST-MISC"
                  and reftable.company  eq ef.company                
                  and reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99")
                  no-lock:
                      CREATE reftable1.
                      BUFFER-COPY reftable EXCEPT rec_key TO reftable1.                      
               END.
            END.
        
            FOR EACH eb
                WHERE eb.company EQ est.company
                AND eb.est-no    EQ est.est-no
                NO-LOCK:                
               CREATE b-eb1.
               BUFFER-COPY eb EXCEPT rec_key est-no ord-no TO b-eb1
               ASSIGN
                b-eb1.company = b-est.company
                b-eb1.est-no  = v-est-no
                b-eb1.ord-no  = ipOrdno.

               FOR EACH box-design-hdr
                   WHERE box-design-hdr.design-no eq 0
                     AND box-design-hdr.company   eq eb.company
                     AND box-design-hdr.est-no    eq eb.est-no
                     AND box-design-hdr.form-no   eq eb.form-no
                     AND box-design-hdr.blank-no  eq eb.blank-no
                   use-index design no-lock:

                  CREATE boxd-hdr.
                  BUFFER-COPY box-design-hdr EXCEPT rec_key est-no TO boxd-hdr
                  ASSIGN
                    boxd-hdr.company = b-est.company
                    boxd-hdr.est-no  = v-est-no.

                  FOR EACH box-design-line of box-design-hdr:
                    CREATE boxd-lin.
                    BUFFER-COPY box-design-line EXCEPT rec_key est-no TO boxd-lin
                    ASSIGN
                        boxd-lin.company = b-est.company
                        boxd-lin.est-no  = v-est-no.
                  END.
               END.              
            END.                   

            FOR EACH est-prep
                WHERE est-prep.company EQ est.company
                AND est-prep.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-est-prep.
                BUFFER-COPY est-prep EXCEPT rec_key est-no TO b-est-prep
                ASSIGN
                    b-est-prep.company = b-est.company
                    b-est-prep.est-no  = v-est-no.
            END.

            FOR EACH est-inst
                WHERE est-inst.company EQ est.company
                AND est-inst.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-est-inst.
                BUFFER-COPY est-inst EXCEPT rec_key est-no TO b-est-inst
                ASSIGN
                    b-est-inst.company = b-est.company
                    b-est-inst.est-no  = v-est-no.
            END.

            FOR EACH est-flm
                WHERE est-flm.company EQ est.company
                AND est-flm.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-est-flm.
                BUFFER-COPY est-flm EXCEPT rec_key est-no TO b-est-flm
                ASSIGN
                    b-est-flm.company = b-est.company
                    b-est-flm.est-no  = v-est-no.
            END.

            FOR EACH est-qty
                WHERE est-qty.company EQ est.company
                AND est-qty.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-est-qty.
                BUFFER-COPY est-qty EXCEPT rec_key est-no TO b-est-qty
                ASSIGN
                    b-est-qty.company = b-est.company
                    b-est-qty.est-no  = v-est-no.
            END.

            FOR EACH notes WHERE notes.rec_key EQ est.rec_key NO-LOCK:
               CREATE b-notes.
               BUFFER-COPY notes TO b-notes
               ASSIGN b-notes.rec_key = lv-rec_key.
            END.

            FOR EACH reftable
                WHERE reftable.reftable EQ "est/getqty.w"
                AND reftable.company  EQ est.company              
                AND reftable.code     EQ est.est-no
                NO-LOCK:

                CREATE reftable2.
                BUFFER-COPY reftable EXCEPT reftable.rec_key reftable.code TO reftable2
                ASSIGN
                   reftable2.code = v-est-no.                   
            END.

            FOR EACH probe
                WHERE probe.company EQ est.company
                AND probe.est-no    EQ est.est-no
                NO-LOCK:
            
                CREATE b-probe.
                BUFFER-COPY probe EXCEPT rec_key est-no TO b-probe
                ASSIGN
                    b-probe.company = b-est.company
                    b-probe.est-no  = v-est-no.
            END.
        END.

        /*RUN build-route(RECID(b-est)).*/
        RUN copy-build-route(b-est.est-no,b-est.company).        

    END. /* if not avail b-eb */
    ELSE DO:    
        ASSIGN n-est-no = b-eb.est-no.
    END. /* if not avail b-eb */
END PROCEDURE.


PROCEDURE copy-build-route :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE INPUT PARAMETER inEstNum like est.est-no.
  DEFINE INPUT PARAMETER inComp like est.company.

  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR lv-msg1 AS CHAR NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.

  DEF BUFFER buff-est-qty FOR est-qty.

  {est/checkuse.i}  

  FIND FIRST est WHERE est.est-no = inEstNum
                 AND est.company = inComp NO-LOCK NO-ERROR.

  ASSIGN
      cocode = est.company.  

  find xest where recid(xest) eq recid(est).  

  FIND FIRST est-qty WHERE est-qty.est-no = xest.est-no 
                     AND est-qty.company = xest.company NO-LOCK NO-ERROR.

  ll = NO.

  IF xest.est-type ge 7 THEN DO:
    for each ef
        where ef.company eq est-qty.company
          and ef.est-no  eq est-qty.est-no
        no-lock:
      
      run set-lock (ef.form-no, no).
    end.    
    
    FIND FIRST xef WHERE xef.company = est-qty.company 
                     AND xef.est-no = est-qty.est-no
                   NO-LOCK NO-ERROR.
    FIND FIRST xeb WHERE xeb.company = est-qty.company 
                     AND xeb.est-no = est-qty.est-no
                     AND xeb.form-no = xef.form-no
                   NO-LOCK NO-ERROR.

    RUN cec/mach-seq.p (0, 0, xest.est-type EQ 8).
  END.

  ELSE DO:
    FOR EACH buff-est-qty
        WHERE buff-est-qty.company EQ est-qty.company
          AND buff-est-qty.est-no  EQ est-qty.est-no
        NO-LOCK BREAK BY buff-est-qty.eqty:

      IF FIRST(buff-est-qty.eqty)    AND
         NOT LAST(buff-est-qty.eqty) THEN
        MESSAGE "Build routings for all quantities?" SKIP
                "  (Yes=AllQtys    No=" +
                TRIM(STRING(est-qty.eqty,">>>,>>>,>>>")) + " Only)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.

      IF ll OR ROWID(est-qty) EQ ROWID(buff-est-qty) THEN DO:  
        FOR EACH ef
            WHERE ef.company EQ buff-est-qty.company
              AND ef.est-no  EQ buff-est-qty.est-no
            NO-LOCK:
      
          RUN set-lock (ef.form-no, NO).
        END.    
    
        FIND FIRST xef WHERE xef.company = buff-est-qty.company 
                         AND xef.est-no = buff-est-qty.est-no
                       NO-LOCK NO-ERROR.
        FIND FIRST xeb WHERE xeb.company = buff-est-qty.company 
                         AND xeb.est-no = buff-est-qty.est-no
                         AND xeb.form-no = xef.form-no
                       NO-LOCK NO-ERROR.

        RUN cec/mach-seq.p (0, buff-est-qty.eqty, NO).
      END.
    END.
  END.

END PROCEDURE.


PROCEDURE set-lock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-form-no like ef.form-no no-undo.
  def input parameter ip-op-lock like ef.op-lock no-undo.
  

  find first ef
      where ef.company eq est.company
        and ef.est-no  eq est.est-no
        and ef.form-no eq ip-form-no
      no-error.
  if avail ef then do:
    ef.op-lock = ip-op-lock.
    release ef.
  end.  
  
END PROCEDURE.

/*------------------------------------------------------------------------------*/
PROCEDURE create-ord-job :
  DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOrdno AS INT NO-UNDO.
  DEFINE INPUT PARAMETER ipJobno like oe-ord.job-no no-undo.
  DEFINE INPUT PARAMETER ipJobno2 like oe-ord.job-no2 no-undo.
  DEFINE INPUT PARAMETER ipLoc AS CHARACTER NO-UNDO.
  
  FIND CURRENT oe-ord.
  def output param op-recid as recid no-undo.

  DEF BUFFER v-ord-job-hdr FOR job-hdr.

  def var v-job-job like job.job no-undo.
  def var v-job-no like job.job-no no-undo.
  def var v-job-no2 like job.job-no2 no-undo.
  def var li-j-no as int no-undo.
    
  /* === from oe/oe-ord1.p  ============= */
         
  find last job where job.company eq ipToCompany no-lock no-error.
  v-job-job = if avail job then job.job + 1 else 1.
  ASSIGN
   v-job-no  = ipJobno
   v-job-no2 = ipJobno2.

  FOR EACH job
      WHERE job.company EQ ipToCompany
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2:
    DELETE job.
  END.

  create job.
  assign job.job        = v-job-job
         job.company    = ipToCompany
         job.loc        = ipLoc
         job.est-no     = ipEstno
         job.job-no     = v-job-no
         job.job-no2    = v-job-no2
         job.stat       = "P"
         op-recid       = recid(job).

  for each oe-ordl where oe-ordl.company eq ipToCompany
                     and oe-ordl.ord-no  eq ipOrdno exclusive:
      find first job-hdr no-lock
          where job-hdr.company eq ipToCompany
            and job-hdr.job-no  eq oe-ord.job-no
            and job-hdr.job-no2 eq oe-ord.job-no2
            and job-hdr.ord-no  eq ipOrdno
            and job-hdr.i-no    eq oe-ordl.i-no
          no-error.

      if not avail job-hdr then do:
         find first itemfg where itemfg.company eq oe-ordl.company
                             and itemfg.i-no    eq oe-ordl.i-no
                             no-lock no-error.   
         
         create job-hdr.
         assign job-hdr.company      = ipToCompany
                job-hdr.loc          = ipLoc
                job-hdr.est-no       = ipEstno
                job-hdr.i-no         = oe-ordl.i-no
                job-hdr.qty          = oe-ordl.qty 
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no
                job-hdr.blank-no     = oe-ordl.blank-no.

         if avail itemfg then
              assign job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         assign job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
      end.

      ELSE
      DO WHILE TRUE:
        FIND v-ord-job-hdr WHERE ROWID(v-ord-job-hdr) EQ ROWID(job-hdr)
            EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL v-ord-job-hdr THEN DO:
          FIND CURRENT v-ord-job-hdr NO-LOCK NO-ERROR.
          FIND CURRENT job-hdr NO-ERROR.
          LEAVE.
        END.
      END.

      assign job-hdr.est-no  = ipEstno
             job-hdr.job     = job.job
             job-hdr.job-no  = job.job-no
             job-hdr.job-no2 = job.job-no2
            /* oe-ordl.est-no  = job-hdr.est-no 10181204 */
             oe-ordl.job-no  = job-hdr.job-no
             oe-ordl.job-no2 = job-hdr.job-no2
             oe-ordl.j-no    = job-hdr.j-no.

      FIND CURRENT job-hdr NO-LOCK.
  end.

  FIND CURRENT job NO-LOCK.
   
END PROCEDURE.


/********************************************************************/

PROCEDURE update-start-date1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-update-job-stdate AS LOG  NO-UNDO.
 DEF VAR lv-prom-date AS DATE NO-UNDO.
 DEFINE VARIABLE v-run-schedule AS LOGICAL NO-UNDO.

 IF oe-ordl.job-no = "" THEN RETURN.

   DEF BUFFER bx-ordl FOR oe-ordl.
   DEF VAR lv-first-due-date AS DATE NO-UNDO.
   lv-first-due-date = oe-ordl.req-date.

  FOR EACH bx-ordl WHERE bx-ordl.company = oe-ordl.company
                      AND bx-ordl.job-no = oe-ordl.job-no
                      AND bx-ordl.job-no2 = oe-ordl.job-no2 
                      AND RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
       lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
                           ELSE lv-first-due-date.
  END.

  DEF BUFFER bf-hdr FOR job-hdr.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bf-job FOR job.
  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-m-time AS INT no-undo.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-job-time  AS INT NO-UNDO.
  DEF VAR lv-maccum-time AS INT NO-UNDO.
  DEF VAR lv-job-hr AS INT NO-UNDO.
  DEF VAR lv-job-day AS INT NO-UNDO.
  DEF VAR lv-wrk-st-time AS INT NO-UNDO.
  DEF VAR lv-chk-date AS DATE NO-UNDO.
  DEF VAR li-num-of-wkend AS INT NO-UNDO.
  DEF VAR lv-start-date-fr AS DATE NO-UNDO.

  /*===  calculate start date from due-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0.

  FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no 
                    AND bf-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK,
      EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                        AND bf-mch.job-no = bf-hdr.job-no
                        AND bf-mch.job-no2 = bf-hdr.job-no2 NO-LOCK:
          ASSIGN
             lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                             truncate(bf-mch.mr-hr,0) * 3600 +
                           ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
             lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                             truncate(bf-mch.run-hr,0) * 3600 +
                           ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
             lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
  END.
  
  ASSIGN
     lv-job-hr = IF lv-job-time MOD 3600 > 0 THEN truncate(lv-job-time / 3600,0) + 1
                 ELSE truncate(lv-job-time / 3600,0)
     lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN truncate(lv-job-hr / 8,0) + 1
                  ELSE TRUNCATE(lv-job-hr / 8,0)
     lv-start-date = lv-first-due-date - lv-job-day. /*- 1. */

  /*  get from mach-calendar 
  lv-chk-date = lv-start-date.
  li-num-of-wkend = 0.
  DO i = 1 TO lv-first-due-date - lv-start-date:
     IF WEEKDAY(lv-chk-date) = 1 OR WEEKDAY(lv-chk-date) = 7 THEN li-num-of-wkend = li-num-of-wkend + 1.
     lv-chk-date = lv-chk-date + 1.
  END.
  lv-start-date = lv-start-date - li-num-of-wkend.
  */
  FIND bx-ordl WHERE RECID(bx-ordl) = RECID(oe-ordl).
  lv-prom-date = TODAY + lv-job-day.
  IF lv-start-date < TODAY  /* ip-type = "Update-2" is from v-ord.w*/
  THEN DO:
     lv-update-job-stdate = NO.
     /*MESSAGE "JOB CANNOT BE COMPLETED BEFORE REQUESTED DUE DATE DUE TO TOTAL MACHINE HOURS."
         SKIP
         "PROMISED DATE WILL BE   " lv-prom-date SKIP
         "UPDATE JOB's START DATE & DUE DATE?" UPDATE lv-update-job-stdate
            VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
    */
     MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
             "Due Date is before Calculates Promised Date. Update Due Date?" UPDATE lv-update-job-stdate
             VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
     /*IF lv-update-job-stdate THEN .
     ELSE DO:
         bx-ordl.prom-date = lv-prom-date.           
         return.
     END. */
     lv-start-date = TODAY.
  END.
  
  v-run-schedule = NOT CAN-FIND(FIRST sys-ctrl
                                WHERE sys-ctrl.company EQ oe-ord.company
                                  AND sys-ctrl.name EQ 'SCHEDULE'
                                  AND sys-ctrl.char-fld EQ 'NoDate'
                                  AND sys-ctrl.log-fld EQ YES).
  IF v-run-schedule THEN DO: /* run if above does not exist */
  
  /* === reset start-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0
         li-num-of-wkend = 0.
  
  FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no
                    AND bf-hdr.job-no2 = oe-ordl.job-no2,
      EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                    AND bf-mch.job-no = bf-hdr.job-no
                    AND bf-mch.job-no2 = bf-hdr.job-no2
                    AND NOT bf-mch.anchored
               BREAK BY bf-mch.frm BY bf-mch.blank-no by bf-mch.pass BY bf-mch.m-code:

          FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                            AND mach-calendar.m-code = bf-mch.m-code
                            AND mach-calendar.m-date = lv-start-date
                            NO-LOCK NO-ERROR.
          lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                      ELSE 28800. /* 8 HRs*/
          IF lv-m-time LT 0 THEN lv-m-time = 28800.
          lv-maccum-time = lv-maccum-time + lv-m-time.
          IF FIRST(bf-mch.frm) THEN DO:
             FIND FIRST bf-job OF bf-hdr.
             ASSIGN
                bf-job.start-date = lv-start-date
                lv-wrk-st-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
          END.
          IF FIRST-OF(bf-mch.frm) THEN
                bf-hdr.start-date = job.start-date.
      
          ASSIGN
          lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.mr-hr,0) * 3600 +
                    ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
          lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.run-hr,0) * 3600 +
                    ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
          bf-mch.seq-no = 0                 
          bf-mch.start-time-su = lv-wrk-st-time
          bf-mch.start-time = lv-wrk-st-time + lv-mr-time
          bf-mch.start-date-su = lv-start-date
          lv-start-date-fr = lv-start-date
          lv-job-time = lv-job-time + lv-mr-time
          lv-start-date = lv-start-date + 
                          IF lv-mr-time > lv-m-time AND
                             lv-mr-time MOD lv-m-time > 0 THEN TRUNCATE(lv-mr-time / lv-m-time,0) 
                          ELSE IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0) - 1
                          ELSE 0
          lv-start-date-fr = lv-start-date.
          IF lv-m-time <> lv-maccum-time THEN DO:
             lv-start-date = lv-start-date + 
                          IF lv-job-time > lv-maccum-time AND
                             lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                          ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                          ELSE 0.
          END.
          ASSIGN
          lv-start-date-fr = lv-start-date
          bf-mch.end-date-su = lv-start-date
          bf-mch.start-date = lv-start-date
          lv-job-time = lv-job-time + lv-run-time
          lv-start-date = lv-start-date + 
                          IF lv-run-time > lv-m-time AND
                             lv-run-time MOD lv-m-time > 0 THEN TRUNCATE(lv-run-time / lv-m-time,0) 
                          ELSE IF lv-run-time > lv-m-time THEN TRUNCATE(lv-run-time / lv-m-time,0) - 1
                          ELSE 0
          lv-start-date-fr = lv-start-date.

          IF lv-m-time <> lv-maccum-time THEN
             lv-start-date = lv-start-date + 
                          IF lv-job-time > lv-maccum-time AND
                             lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                          ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                          ELSE 0.
          
          ASSIGN bf-mch.end-time = bf-mch.start-time + lv-run-time
                 bf-mch.end-time-su = bf-mch.start-time-su + lv-mr-time
                 bf-mch.end-date = lv-start-date           
                 lv-wrk-st-time = lv-wrk-st-time + lv-mr-time + lv-run-time.
  END.
  END. /* if v-run-schedule*/
  
  ASSIGN
     bx-ordl.prom-date = lv-prom-date
     bx-ordl.req-date = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.req-date.

END PROCEDURE.
