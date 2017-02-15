/* r-ltpost.p                                                           */
/* Created by: Ron C   2008                                              */   
   DEF INPUT PARAMETER i-fg-rctd-rowid AS ROWID.
   DEF INPUT PARAMETER plBatch AS LOGICAL NO-UNDO.
   
   FIND FIRST fg-rctd WHERE
             ROWID(fg-rctd)    = i-fg-rctd-rowid EXCLUSIVE-LOCK NO-WAIT NO-ERROR. 


   DEF VARIABLE fgPostLog   AS LOGICAL NO-UNDO.
   def var v-autobin        as cha no-undo.


   def buffer b-fg-rcpts    for fg-rcpts.
   def buffer b-fg-rdtl     for fg-rdtl.
   def buffer b-fg-bin      for fg-bin.
   def buffer b-itemfg1     for itemfg.
   def buffer ps-rctd       for fg-rctd .
   def buffer b-po-ordl     for po-ordl.
   def buffer b-oe-ordl     for oe-ordl.
   DEF BUFFER b-itemfg FOR itemfg.

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
   def var fg-uom-list  as char NO-UNDO.
   DEF VAR v-fgpostgl AS CHAR NO-UNDO.
   DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.

   DEF STREAM logfile.
   DEF STREAM st-email.

   DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.


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

   RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list). 

   DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD has-rec  AS LOG INIT NO
                                    FIELD invoiced AS LOG INIT NO.

   ASSIGN
      v-post-date = TODAY
      fgPostLog = SEARCH('logs/fgpstall.log') NE ?.

   IF fgPostLog THEN
      OUTPUT STREAM logFile TO VALUE('logs/fgpstall.' + STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').

   IF fgPostLog THEN RUN fgPostLog ('Started').

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

      IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).
      {fg/fg-post.i w-fg-rctd w-fg-rctd}
      FIND CURRENT itemfg NO-LOCK NO-ERROR.


      FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      FIND CURRENT fg-bin NO-LOCK NO-ERROR.

      IF fgPostLog THEN RUN fgPostLog ('End fg/fg-post.i - Start fg/fgemails.i').
      
      IF w-fg-rctd.rita-code = "R" THEN DO:
         {fg/fgemails.i}
      END.

      IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').

      FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id
           EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL fg-rctd THEN DO:
         ASSIGN
            fg-rctd.rita-code = "P"  /* posted */
            fg-rctd.post-date = TODAY
            fg-rctd.tag2      = w-fg-rctd.tag2.

         FIND FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id"
              AND reftable.company  EQ fg-rctd.company
              AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
              NO-LOCK NO-ERROR.
         IF NOT AVAIL reftable THEN DO:
           CREATE  reftable.
           ASSIGN  reftable.reftable = "fg-rctd.user-id"
                 reftable.company  = fg-rctd.company
                 reftable.loc      = STRING(fg-rctd.r-no,"9999999999")
                 reftable.code     = USERID("nosweat").
         END.

         FOR EACH fg-rcpts
             WHERE fg-rcpts.company EQ fg-rctd.company
               AND fg-rcpts.r-no    EQ fg-rctd.r-no
             EXCLUSIVE-LOCK:
           fg-rcpts.rita-code = fg-rctd.rita-code.
         END.

         FIND CURRENT fg-rctd NO-LOCK.
      END.

      IF fgPostLog THEN RUN fgPostLog ('End loop'). 
  END.  /* for each fg-rctd */

  IF fgPostLog THEN RUN fgPostLog ('End fg/fgemails.i - Start loadtag').
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
      IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').

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

  IF fgPostLog THEN RUN fgPostLog ('End First - Start Second For Each w-fg-rctd').
  FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK:

    CREATE w-inv.
    w-inv.row-id = w-fg-rctd.row-id.
  END.
  IF fgPostLog THEN RUN fgPostLog ('End Second For Each w-fg-rctd').

  IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/invrecpt.p').
  RUN fg/invrecpt.p (?, 2).
  IF fgPostLog THEN RUN fgPostLog ('End Run fg/invrecpt.p').

  IF fgPostLog THEN RUN fgPostLog ('End First - Start Third For Each w-fg-rctd').
  FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK
      BREAK BY w-fg-rctd.i-no:

    IF LAST-OF(w-fg-rctd.i-no) THEN DO:
      IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).
      RUN fg/updfgcs1.p (RECID(itemfg), NO).
      IF fgPostLog THEN RUN fgPostLog ('End Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).

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
              RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0
                                      THEN "EA" ELSE itemfg.prod-uom),
                                     "M", 0, 0, 0, 0,
                                     itemfg.total-std-cost, OUTPUT b-oe-ordl.cost).
            LEAVE.
          END.
        END.
      END.
    END.
  END.
  IF fgPostLog THEN RUN fgPostLog ('End Third For Each w-fg-rctd').

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


    IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').
    RUN gl-from-work (1, v-trnum).
    IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').
    RUN gl-from-work (2, v-trnum).
    IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').
  END.
  find first w-job no-error.
  /* Run only when not batch process. */
  if avail w-job AND plBatch = NO THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').
    run jc/d-jclose.w.
    IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').
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

    IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').
    for each work-job break by work-job.actnum:
       create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-job.actnum
       gltrans.jrnl    = "ADJUST"
       gltrans.tr-date = TODAY
       gltrans.period  = period.pnum
       gltrans.trnum   = v-trnum.

      if work-job.fg then
        assign
         gltrans.tr-amt  = - work-job.amt
         gltrans.tr-dscr = "ADJUSTMENT FG".
      else
        assign
         gltrans.tr-amt  = work-job.amt
         gltrans.tr-dscr = "ADJUSTMENT COGS".
    end. /* each work-job */
    IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
  end.
  IF v-got-fgemail THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
    RUN send-fgemail (v-fgemail-file).
    IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
  END.
  IF fgPostLog THEN RUN fgPostLog ('End').
  IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.


  PROCEDURE fgPostLog:

      DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.
        
 PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
     STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.

END PROCEDURE.

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
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "FGPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = TODAY
       gltrans.tr-dscr = if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"
       gltrans.trnum   = ip-trnum.

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
