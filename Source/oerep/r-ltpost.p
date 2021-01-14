/* r-ltpost.p                                                           */
/* Created by: Ron C   2008                                              */   
   DEF INPUT PARAMETER i-fg-rctd-rowid AS ROWID.
   DEF INPUT PARAMETER plBatch AS LOGICAL NO-UNDO.
   
   FIND FIRST fg-rctd WHERE
             ROWID(fg-rctd)    = i-fg-rctd-rowid EXCLUSIVE-LOCK NO-WAIT NO-ERROR. 

   def var v-autobin        as cha no-undo.


   def buffer b-fg-rcpts    for fg-rcpts.
   def buffer b-fg-rdtl     for fg-rdtl.
   def buffer b-fg-bin      for fg-bin.
   def buffer b-itemfg1     for itemfg.
   def buffer ps-rctd       for fg-rctd .
   def buffer b-po-ordl     for po-ordl.
   def buffer b-oe-ordl     for oe-ordl.
   DEF BUFFER b-itemfg FOR itemfg.

   DEFINE VARIABLE lAnyJobCloses AS LOGICAL NO-UNDO.
   def var v-one-item as log.
   def var v-dec as dec decimals 10.
   def var v-po-no like rm-rcpt.po-no no-undo.
   def var v-r-qty like fg-rctd.qty no-undo.
   def var v-i-qty like fg-rctd.qty no-undo.
   def var v-t-qty like fg-rctd.qty no-undo.
   def var v-overrun-qty like fg-rctd.qty no-undo.
   def var v-underrun-qty like fg-rctd.qty no-undo.
   DEF VAR v-reduce-qty AS INT NO-UNDO.
   DEF VAR v-est-no AS cha NO-UNDO.
   def var v-recid as recid no-undo.
   DEF VAR v-cost AS DEC NO-UNDO.
   DEF VAR v-binqty AS INT NO-UNDO.
   DEF VAR v-qty AS INT NO-UNDO.
   DEF VAR v-tagcost AS DEC NO-UNDO.
   def var ld-cvt-qty as dec no-undo.
   def var ld-cvt-cost as dec DECIMALS 10 no-undo.
   def var v-newhdr as log no-undo. 
   def var v-fin-qty as dec no-undo.
   def var v-trnum like gl-ctrl.trnum no-undo.
   def var uperiod as int no-undo.
   def var sysdate as date init today no-undo.    
   def var v-date like sysdate no-undo.
   DEF VAR v-underrun AS DEC NO-UNDO.
   DEF VAR v-qty-received AS INT NO-UNDO.
   DEF VAR v-got-fgemail AS LOG NO-UNDO.
   DEF VAR v-fgemail-file AS cha NO-UNDO.
   DEF VAR li-tag-no AS INT NO-UNDO.
   DEF VAR ll-qty-changed AS LOG NO-UNDO.
   DEF VAR ll-whs-item AS LOG NO-UNDO.
   DEF VAR choice AS LOG.
   DEF VAR v-post-date AS DATE NO-UNDO INIT TODAY.
   DEF VAR v-fgpostgl AS CHAR NO-UNDO.
   DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.

   DEF STREAM logfile.
   DEF STREAM st-email.

   DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

/* ************************  Function Prototypes ********************** */
FUNCTION fCanCloseJob RETURNS LOGICAL 
    ( INPUT iprwJobRec AS RECID, INPUT ipcINo AS CHARACTER ) FORWARD.



   FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.

   IF AVAIL users AND users.user_program[2] NE "" THEN
      v-dir = users.user_program[2] + "\".
   ELSE
      v-dir = "c:\tmp\".

   {custom/globdefs.i}
   {sys/inc/VAR.i new SHARED }
   
   ASSIGN 
      cocode = g_company
      locode = g_loc.

   {sys/inc/closejob.i FGPost}
   {fg/fg-post3.i NEW}
   {jc/jcgl-sh.i  NEW}
   {sys/inc/fgemails.i}
   {fg/invrecpt.i NEW}
   {sys/inc/adjustgl.i}
   {sys/inc/fgpost.i}
   
    DEF TEMP-TABLE tt-email 
      FIELD tt-recid AS RECID
      FIELD job-no LIKE job-hdr.job-no
      FIELD job-no2 LIKE job-hdr.job-no2
      FIELD i-no LIKE itemfg.i-no
      FIELD qty AS INT
      FIELD cust-no AS cha
      INDEX tt-cust IS PRIMARY cust-no DESCENDING .

   DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD has-rec  AS LOG INIT NO
                                    FIELD invoiced AS LOG INIT NO.

   ASSIGN
      v-post-date = TODAY
      .

   FIND FIRST period NO-LOCK WHERE period.company EQ cocode
        AND period.pst     LE TODAY
        AND period.pend    GE TODAY.

   FIND FIRST sys-ctrl WHERE 
              sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "AUTOPOST" NO-LOCK NO-ERROR.
  
   v-autobin = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

   DISABLE TRIGGERS FOR LOAD OF itemfg.
   DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

   FOR EACH w-fg-rctd:
      DELETE w-fg-rctd.
   END.

   /* Create a single workfile record for the finished good being posted */
   CREATE w-fg-rctd.
   BUFFER-COPY fg-rctd TO w-fg-rctd
   ASSIGN 
      w-fg-rctd.row-id  = ROWID(fg-rctd)
      w-fg-rctd.has-rec = YES.

   FOR EACH w-fg-rctd
      BY w-fg-rctd.tag
      BY w-fg-rctd.rct-date
      BY w-fg-rctd.r-no:

      FIND FIRST itemfg WHERE 
            itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL itemfg THEN
        NEXT.

      {fg/fg-post.i w-fg-rctd w-fg-rctd}
      FIND CURRENT itemfg NO-LOCK NO-ERROR.


      FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      FIND CURRENT fg-bin NO-LOCK NO-ERROR.

      IF w-fg-rctd.rita-code = "R" THEN DO:
         {fg/fgemails.i}
      END.

      FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id
           EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL fg-rctd THEN DO:
         ASSIGN
            fg-rctd.rita-code = "P"  /* posted */
            fg-rctd.post-date = TODAY
            fg-rctd.tag2      = w-fg-rctd.tag2
            fg-rctd.created-by =  USERID("nosweat")
            .

         FOR EACH fg-rcpts
             WHERE fg-rcpts.company EQ fg-rctd.company
               AND fg-rcpts.r-no    EQ fg-rctd.r-no
             EXCLUSIVE-LOCK:
           fg-rcpts.rita-code = fg-rctd.rita-code.
         END.

         FIND CURRENT fg-rctd NO-LOCK.
      END.
  END.  /* for each fg-rctd */

  FOR EACH w-fg-rctd
      BREAK BY w-fg-rctd.i-no
            BY w-fg-rctd.job-no
            BY w-fg-rctd.job-no2
            BY w-fg-rctd.loc
            BY w-fg-rctd.loc-bin
            BY w-fg-rctd.tag:

    IF LAST-OF(w-fg-rctd.tag) THEN DO:
      IF TRIM(w-fg-rctd.tag) NE "" THEN 
      /* Ensure Bin/Tags Qty is correct.  Task 01270602 */
      
      FOR EACH fg-bin NO-LOCK
          WHERE fg-bin.company EQ g_company
            AND fg-bin.i-no    EQ loadtag.i-no
            AND fg-bin.tag     EQ loadtag.tag-no
          USE-INDEX tag:
        RUN fg/calcbinq.p (ROWID(fg-bin)).
      END.

      /* IF w-fg-rctd.tag <> "" then*/
      FIND FIRST loadtag
          WHERE loadtag.company   EQ g_company
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ w-fg-rctd.tag
            AND loadtag.i-no      EQ w-fg-rctd.i-no
            AND loadtag.job-no    EQ w-fg-rctd.job-no
          USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL loadtag THEN DO:
         FIND FIRST fg-bin
             WHERE fg-bin.company EQ g_company
               AND fg-bin.i-no    EQ loadtag.i-no
               AND fg-bin.tag     EQ loadtag.tag-no
             /*AND fg-bin.job-no = loadtag.job-no
               AND fg-bin.job-no2 = loadtag.job-no2*/
               AND fg-bin.qty     GT 0
             USE-INDEX tag NO-LOCK NO-ERROR.
         IF w-fg-rctd.rita-code = "T" AND /*loadtag.tot-cases = w-fg-rctd.cases*/
            TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN  /* full qty transfer*/ 
           ASSIGN
            loadtag.loc          = w-fg-rctd.loc2   
            loadtag.loc-bin      = w-fg-rctd.loc-bin2
            loadtag.qty          = fg-bin.qty
            loadtag.pallet-count = fg-bin.qty
            loadtag.partial      = fg-bin.partial-count
            loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.
         ELSE /*partial transfer */
           ASSIGN
            loadtag.loc     = w-fg-rctd.loc
            loadtag.loc-bin = w-fg-rctd.loc-bin.

         FIND CURRENT loadtag NO-LOCK.
      END.
    END.
  END.

  FOR EACH w-inv:
    DELETE w-inv.
  END.

  FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK:

    CREATE w-inv.
    w-inv.row-id = w-fg-rctd.row-id.
  END.

  RUN fg/invrecpt.p (?, 2).

  FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK
      BREAK BY w-fg-rctd.i-no:

    IF LAST-OF(w-fg-rctd.i-no) THEN DO:
      RUN fg/updfgcs1.p (RECID(itemfg), NO, NO).

      FOR EACH oe-ordl
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.opened  EQ YES
            AND oe-ordl.i-no    EQ w-fg-rctd.i-no
            AND oe-ordl.job-no  EQ ""
            AND oe-ordl.cost    EQ 0
          USE-INDEX opened NO-LOCK
          BREAK BY oe-ordl.ord-no
          TRANSACTION :

        DO i = 1 TO 1000:
          FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.
          IF AVAIL b-oe-ordl THEN DO:
            IF itemfg.prod-uom EQ "M" THEN
              b-oe-ordl.cost = itemfg.total-std-cost.
            ELSE
              RUN sys/ref/convcuom.p((IF DYNAMIC-FUNCTION("Conv_IsEAUOM",itemfg.company, itemfg.i-no, itemfg.prod-uom)
                                      THEN "EA" ELSE itemfg.prod-uom),
                                     "M", 0, 0, 0, 0,
                                     itemfg.total-std-cost, OUTPUT b-oe-ordl.cost).
            LEAVE.
          END.
        END.
      END.
    END.
  END.

  IF v-fgpostgl NE "None" THEN DO TRANSACTION:
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN v-trnum       = gl-ctrl.trnum + 1
               gl-ctrl.trnum = v-trnum.
        FIND CURRENT gl-ctrl NO-LOCK.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */


    RUN gl-from-work (1, v-trnum).
    RUN gl-from-work (2, v-trnum).
  END.
  
/* Remove w-job recs that should not be prompted to close */ 
EACH-JOB:   
FOR EACH w-job:             
    FIND FIRST job NO-LOCK 
        WHERE RECID(job) EQ w-job.rec-id
        NO-ERROR.
    IF NOT AVAIL job THEN 
        NEXT.
    lAnyJobCloses = NO.
    FOR EACH w-fg-rctd NO-LOCK
        WHERE w-fg-rctd.company EQ job.company
        AND w-fg-rctd.job-no EQ job.job-no
        AND w-fg-rctd.job-no2 EQ job.job-no2
        BREAK BY w-fg-rctd.job-no 
        BY w-fg-rctd.job-no2 
        BY w-fg-rctd.i-no
        :    

        IF LAST-OF(w-fg-rctd.i-no) AND fCanCloseJob(w-job.rec-id, w-fg-rctd.i-no) THEN
            lAnyJobCloses = YES.
    END.
    IF NOT lAnyJobCloses THEN DELETE w-job.  
END.

  find first w-job no-error.
  /* Run only when not batch process. */
  if avail w-job AND plBatch = NO THEN DO:
    run jc/d-jclose.w.
  END.

  if v-adjustgl then do TRANSACTION:
    /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN v-trnum       = gl-ctrl.trnum + 1
               gl-ctrl.trnum = v-trnum.
        FIND CURRENT gl-ctrl NO-LOCK.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */

    for each work-job break by work-job.actnum:
      
      RUN spCreateGLHist(cocode,
                         work-job.actnum,
                         "ADJUST",
                         (if work-job.fg then "ADJUSTMENT FG" else "ADJUSTMENT COGS"),
                         TODAY,
                         (if work-job.fg then - work-job.amt else work-job.amt),
                         v-trnum,
                         period.pnum,
                         "A",
                         TODAY,
                         "",
                         "FG").   
    end. /* each work-job */
  end.
  IF v-got-fgemail THEN DO:
    RUN send-fgemail (v-fgemail-file).
  END.



PROCEDURE gl-from-work:
 DEF INPUT PARAM ip-run AS INT NO-UNDO.
 DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
  
  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE TODAY
        AND period.pend    GE TODAY
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:
      
    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      
      RUN spCreateGLHist(cocode,
                         work-gl.actnum,
                         "FGPOST",
                         (if work-gl.job-no ne "" then "FG Receipt from Job" else "FG Receipt from PO"),
                         TODAY,
                         debits - credits,
                         v-trnum,
                         period.pnum,
                         "A",
                         TODAY,
                         "",
                         "FG").  

      assign
       debits  = 0
       credits = 0.
    end.
  end.

END PROCEDURE.

PROCEDURE SEND-fgemail:
  DEF INPUT PARAM ip-fgemail-file AS cha .

  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.

   FOR EACH tt-email,
       FIRST cust NO-LOCK WHERE cust.company = g_company
                           AND cust.cust-no = tt-email.cust-no
                           AND cust.active = "E" BREAK BY tt-email.cust-no:
       IF FIRST-OF(tt-email.cust-no) THEN DO:
          v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
          OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
          PUT STREAM st-email "JOB#       FG Item#              Qty    " SKIP
                              "========== =============== ============ " SKIP.
       END.
       PUT STREAM st-email UNFORMATTED
                 tt-email.job-no + "-" + string(tt-email.job-no2,"99") FORM "x(10)"
                 " " tt-email.i-no " " tt-email.qty FORM "->>>,>>>,>>9" 
                 SKIP.
       IF LAST-OF(tt-email.cust-no) THEN do:
           OUTPUT STREAM st-email CLOSE.
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}
           IF ls-to-list NE '' THEN DO:
             ASSIGN lv-mailto = "To:" + ls-to-list
                    lv-mailsubject = "Finished Goods Receipts have been posted"
                    lv-mailbody = "Finished Goods Receipts have been posted"
                    lv-mailattach = v-fgemail-file.
             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,1,OUTPUT retcode).
           END.
       END. /* last-of(tt-email.cust-no) */
   END.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fCanCloseJob RETURNS LOGICAL 
	( INPUT iprwJobRec AS RECID, INPUT ipcINo AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
   DEF VAR li-t-qty AS INT.
    DEF VAR lv-rowid AS ROWID NO-UNDO.
    DEF VAR ll-set AS LOG.  
    DEFINE VARIABLE ll-qty-changed AS LOG       NO-UNDO.
    DEFINE VARIABLE v-fin-qty      AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE choice         AS LOG       NO-UNDO. 
    DEFINE VARIABLE v-overrun-qty  LIKE fg-rctd.qty NO-UNDO. 
    DEFINE VARIABLE v-underrun-qty LIKE fg-rctd.qty NO-UNDO. 
    DEFINE VARIABLE v-reduce-qty   AS INTEGER   NO-UNDO.   
    DEFINE VARIABLE ll-whs-item    AS LOG       NO-UNDO.       
    DEFINE BUFFER b-itemfg FOR itemfg.
    FIND FIRST job
        WHERE RECID(job) EQ iprwJobRec
        NO-ERROR.
    IF NOT AVAIL job THEN 
        RETURN NO.

    ASSIGN 
        choice         = NO
        li-t-qty       = 0
        ll-set         = NO
        ll-qty-changed = NO 
        v-overrun-qty  = 0
        v-underrun-qty = 0
        v-reduce-qty   = 0
        ll-whs-item   = no
        .

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ ipcIno
        NO-ERROR.
    IF NOT AVAIL itemfg THEN 
        RETURN NO.
    /*    IF w-fg-rctd.job-no    NE ""  AND         */
    /*      w-fg-rctd.rita-code NE "T" THEN         */
    /*      FIND FIRST job NO-LOCK                  */
    /*        WHERE job.company EQ cocode           */
    /*          AND job.job-no  EQ w-job.job-no     */
    /*          AND job.job-no2 EQ w-fg-rctd.job-no2*/
    /*        NO-ERROR.                             */

    IF AVAIL job THEN 
    DO:

        /* Determine if job may be closed via li-t-qty */
        li-t-qty = w-fg-rctd.t-qty.

        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     eq job.job
            AND job-hdr.job-no  eq job.job-no
            AND job-hdr.job-no2 eq job.job-no2
            AND job-hdr.i-no    eq ipcIno
            NO-ERROR.
        IF AVAIL job-hdr THEN ll-set = NO.

        ELSE      /* Check for a set header to process instead */
            IF NOT itemfg.isaset THEN 
            DO:
                FIND FIRST reftable NO-LOCK
                    WHERE reftable.reftable EQ "jc/jc-calc.p"
                    AND reftable.company  EQ job.company
                    AND reftable.loc      EQ ""
                    AND reftable.code     EQ STRING(job.job,"999999999")
                    AND reftable.code2    EQ w-fg-rctd.i-no
                    NO-ERROR.


                /* Used to determine choice to close job for a set */
                RUN fg/setsrcvd.p (BUFFER job, BUFFER reftable, BUFFER job-hdr,
                    INPUT-OUTPUT li-t-qty).
                ll-set = AVAIL job-hdr.
            END.
        IF AVAILABLE job-hdr THEN 
        DO:
            IF job.opened                           AND
                (NOT ll-set OR (CAN-FIND(FIRST b-itemfg
                WHERE b-itemfg.company EQ job-hdr.company
                AND b-itemfg.i-no    EQ job-hdr.i-no
                AND b-itemfg.isaset  EQ YES
                AND b-itemfg.alloc   NE NO))) THEN 
            DO:

                RUN jc/qty-changed.p (BUFFER job, OUTPUT ll-qty-changed).

              /* Get underrun quantity, v-fin-qty, uses ll-qty-changed */
                {fg/closejob.i}

                IF v-close-job GT 0                                            AND
                    (job.stat EQ "W"                                OR
                    v-close-job GT 1                               OR
                    CAN-FIND(FIRST mat-act
                    WHERE mat-act.company EQ job.company
                    AND mat-act.job     EQ job.job
                    AND mat-act.job-no  EQ job.job-no
                    AND mat-act.job-no2 EQ job.job-no2) OR
                    CAN-FIND(FIRST mch-act
                    WHERE mch-act.company EQ job.company
                    AND mch-act.job     EQ job.job
                    AND mch-act.job-no  EQ job.job-no
                    AND mch-act.job-no2 EQ job.job-no2) OR
                    CAN-FIND(FIRST misc-act
                    WHERE misc-act.company EQ job.company
                    AND misc-act.job     EQ job.job
                    AND misc-act.job-no  EQ job.job-no
                    AND misc-act.job-no2 EQ job.job-no2))          AND
                    v-fin-qty + li-t-qty GE v-underrun-qty                     AND
                    w-fg-rctd.rita-code EQ "R" THEN 
                DO:

                    choice = YES.

                    RELEASE job-hdr.
                    FOR EACH job-hdr NO-LOCK
                        WHERE job-hdr.company EQ job.company
                        AND job-hdr.job     EQ job.job
                        AND job-hdr.job-no  EQ job.job-no
                        AND job-hdr.job-no2 EQ job.job-no2
                        AND ROWID(job-hdr)  NE lv-rowid
                        AND NOT CAN-FIND(FIRST b-itemfg
                        WHERE b-itemfg.company EQ job-hdr.company
                        AND b-itemfg.i-no    EQ job-hdr.i-no
                        AND b-itemfg.pur-man EQ YES)
                        AND NOT CAN-FIND(FIRST eb
                        WHERE eb.company  EQ job.company
                        AND eb.est-no   EQ job.est-no
                        AND eb.stock-no EQ job-hdr.i-no
                        AND eb.pur-man  EQ YES):
                  /* get underrun quantity, v-find-qty, uses ll-qty-changed */
                  {fg/closejob.i}

                        IF v-fin-qty LT v-underrun-qty THEN 
                        DO:
                            choice = NO.
                            LEAVE.
                        END.
                    END. /* For each job-hdr */

                END. /* If v-job-close gt 0 */
            END. /* If job.opened... */
        END. /* if avail job-hdr */
    END. /* If avail job */

    RETURN choice.

END FUNCTION.
