
/*------------------------------------------------------------------------
    File        : fg/fgpost.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue May 03 16:29:24 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define TEMP-TABLE ipFGRctd like fg-rctd
                  field TableRowid as rowid.
define INPUT PARAM TABLE FOR ipFGRctd.
def input param ipTag as char.
def temp-table FGReceiptRow like ipFGRctd.

DEF STREAM logFile.
def stream st-email.

DEF TEMP-TABLE tt-rcpth LIKE fg-rcpth.
DEF TEMP-TABLE tt-rdtlh LIKE fg-rdtlh.

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD has-rec  AS LOG INIT NO
                                    FIELD invoiced AS LOG INIT NO.


DEF TEMP-TABLE tt-email NO-UNDO FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .
                        
{pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/fullset.i  NEW}
{fg/fg-post3.i NEW}

DEF VAR v-fgpostgl AS CHAR NO-UNDO.                        
def var v-post-date as date init today no-undo.
def var v-prgmname as cha no-undo.

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
{sys/inc/var.i "new shared"}

ASSIGN
  cocode = g_company
  locode = g_loc.
  
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

for each ipFGRctd no-lock:
    create FGReceiptRow.
    buffer-copy ipFGRctd to FGReceiptRow.
    assign FGReceiptRow.tag = ipTag.    
end.

run ProcFGPosting.

/* **********************  Internal Procedures  *********************** */

PROCEDURE fgPostLog:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.
        
   PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
     STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.

END PROCEDURE.

PROCEDURE get-next-tag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipc-i-no AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER opc-next-tag AS CHAR NO-UNDO.
  DEF BUFFER bf-loadtag FOR loadtag.
  DEF VAR io-tag-no AS INT NO-UNDO.

  FIND LAST bf-loadtag NO-LOCK
      WHERE bf-loadtag.company     EQ cocode
        AND bf-loadtag.item-type   EQ NO
        AND bf-loadtag.is-case-tag EQ NO
        AND bf-loadtag.tag-no      BEGINS ipc-i-no
        AND SUBSTR(bf-loadtag.tag-no,1,15) EQ ipc-i-no
      USE-INDEX tag NO-ERROR.

  io-tag-no = (IF AVAIL bf-loadtag THEN INT(SUBSTR(bf-loadtag.tag-no,16,5)) ELSE 0) + 1.

  opc-next-tag = STRING(CAPS(ipc-i-no),"x(15)") + STRING(io-tag-no,"99999").

END PROCEDURE.

PROCEDURE gl-from-work:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
  
  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
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
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    end.
  end.

END PROCEDURE.

PROCEDURE mail EXTERNAL "xpMail.dll" :
      DEF INPUT PARAM mailTo AS CHAR.
      DEF INPUT PARAM mailsubject AS CHAR.
      DEF INPUT PARAM mailText AS CHAR.
      DEF INPUT PARAM mailFiles AS CHAR.
      DEF INPUT PARAM mailDialog AS LONG.
      DEF OUTPUT PARAM retCode AS LONG.
END.

PROCEDURE ProcFGPosting:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF BUFFER b-fg-rcpts FOR fg-rcpts.
  DEF BUFFER b-fg-rdtl FOR fg-rdtl.
  DEF BUFFER b-fg-bin FOR fg-bin.
  DEF BUFFER b-itemfg FOR itemfg.
  DEF BUFFER b-itemfg1 FOR itemfg.
  DEF BUFFER ps-rctd FOR fg-rctd .
  DEF BUFFER b-po-ordl FOR po-ordl.
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR v-one-item AS LOG.
  DEF VAR v-dec AS DEC DECIMALS 10.
  DEF VAR v-po-no LIKE rm-rcpt.po-no NO-UNDO.
  DEF VAR x AS INT NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR v-r-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-i-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-t-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-overrun-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-underrun-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-reduce-qty AS INT NO-UNDO.
  DEF VAR v-est-no AS cha NO-UNDO.
  DEF VAR v-recid AS RECID NO-UNDO.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-binqty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-tagcost AS DEC NO-UNDO.
  DEF VAR ld-cvt-qty AS DEC NO-UNDO.
  DEF VAR ld-cvt-cost AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR v-autobin  AS cha NO-UNDO.
  DEF VAR v-newhdr AS LOG NO-UNDO. 
  DEF VAR v-fin-qty AS DEC NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR v-trnum LIKE gl-ctrl.trnum NO-UNDO.
  DEF VAR uperiod AS INT NO-UNDO.
  DEF VAR sysdate AS DATE INIT TODAY NO-UNDO.    
  DEF VAR v-date LIKE sysdate NO-UNDO.
  DEF VAR v-underrun AS DEC NO-UNDO.
  DEF VAR v-qty-received AS INT NO-UNDO.
  DEF VAR v-got-fgemail AS LOG NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR li-tag-no AS INT NO-UNDO.
  DEF VAR ll-qty-changed AS LOG NO-UNDO.
  DEF VAR ll-whs-item AS LOG NO-UNDO.

  DEFINE VARIABLE fgPostLog AS LOGICAL NO-UNDO.
  /*define var cocode as cha no-undo.*/
  /*define var g_company as cha no-undo.*/
  DEF VAR fg-uom-list  AS CHAR NO-UNDO.
  
  do transaction:
    {sys/inc/closejob.i FGPost}
    {sys/inc/fgpostgl.i}
    {sys/inc/adjustgl.i}
    {sys/inc/fgemails.i}
    {sys/inc/postdate.i}
    {sys/inc/fgpost.i}
  END.

  v-fgpostgl = fgpostgl.
  
  RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).
  
  fgPostLog = SEARCH('logs/fgpstall.log') NE ?.
  IF fgPostLog THEN do:
     file-info:file-name = search('logs/fgpstall.log').
     OUTPUT STREAM logFile TO
        VALUE(substring(file-info:full-pathname,1,length(file-info:full-pathname) - 17) + 'logs/fgpstall.' +
         STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').
  end.
  IF fgPostLog THEN RUN fgPostLog ('Started').
 
  for each FGReceiptRow no-lock /* where FGReceiptRow.TableRowid <> ? */ : 
  /*assign cocode = FGReceiptRow.company
         g_company = FGReceiptRow.company
         .
  */       
    
  FIND FIRST period NO-LOCK
      WHERE period.company EQ  cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date.

  FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
                         AND sys-ctrl.name    EQ "AUTOPOST"
       NO-LOCK NO-ERROR.
  v-autobin = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

  FOR EACH w-fg-rctd:
    DELETE w-fg-rctd.
  END.

  /* Create a single workfile record for the finished good being posted */
  CREATE w-fg-rctd.
  BUFFER-COPY FGReceiptRow TO w-fg-rctd
  ASSIGN w-fg-rctd.row-id  = FGReceiptRow.TableRowid
         w-fg-rctd.has-rec = YES.
  find fg-rctd where rowid(fg-rctd) = FGReceiptRow.tableRowid no-lock no-error.
  if avail fg-rctd then assign w-fg-rctd.r-no = fg-rctd.r-no
                               w-fg-rctd.trans-time = fg-rctd.trans-time.  
  
  FOR EACH w-fg-rctd,
        FIRST itemfg WHERE itemfg.company eq cocode
                       AND itemfg.i-no    EQ w-fg-rctd.i-no
        BY w-fg-rctd.tag
        BY w-fg-rctd.rct-date
        BY w-fg-rctd.r-no:

      IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).
      {fg/fg-post.i w-fg-rctd w-fg-rctd}

      FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      FIND CURRENT fg-bin NO-LOCK NO-ERROR.

      IF fgPostLog THEN RUN fgPostLog ('End fg/fg-post.i - Start fg/fgemails.i').
      IF w-fg-rctd.rita-code = "R" THEN DO:
         {fg/fgemails.i}
      END.

      IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').

      FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

      IF AVAIL fg-rctd THEN DO:
        ASSIGN
         fg-rctd.rita-code = "P"  /* posted */
         fg-rctd.post-date = v-post-date
         fg-rctd.tag2      = w-fg-rctd.tag2.

        FOR EACH fg-rcpts
            WHERE fg-rcpts.company EQ fg-rctd.company
              AND fg-rcpts.r-no    EQ fg-rctd.r-no:
          fg-rcpts.rita-code = fg-rctd.rita-code.
        END.
      END.

      IF fgPostLog THEN RUN fgPostLog ('End loop'). 
    END.  /* for each fg-rctd */

    FIND CURRENT itemfg NO-LOCK NO-ERROR.

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

          FIND CURRENT loadtag NO-LOCK NO-ERROR.
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
         
         IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').         
         RUN gl-from-work (1, v-trnum).
         IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').
         RUN gl-from-work (2, v-trnum).
         IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').
         
         LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */
    END.
    FIND FIRST w-job NO-ERROR.
    IF AVAIL w-job THEN DO:
      IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').
      RUN jc/d-jclose.w.
      IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').
    END.

    IF v-adjustgl THEN DO TRANSACTION:
      /** GET next G/L TRANS. POSTING # **/
      FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK.
      ASSIGN
       v-trnum       = gl-ctrl.trnum + 1
       gl-ctrl.trnum = v-trnum.
      FIND CURRENT gl-ctrl NO-LOCK.
      IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').
      FOR EACH work-job BREAK BY work-job.actnum:
         CREATE gltrans.
        ASSIGN
         gltrans.company = cocode
         gltrans.actnum  = work-job.actnum
         gltrans.jrnl    = "ADJUST"
         gltrans.tr-date = v-post-date
         gltrans.period  = period.pnum
         gltrans.trnum   = v-trnum.

        IF work-job.fg THEN
          ASSIGN
           gltrans.tr-amt  = - work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT FG".
        ELSE
          ASSIGN
           gltrans.tr-amt  = work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT COGS".

        RELEASE gltrans.
      END. /* each work-job */
      IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
    END.
    IF v-got-fgemail THEN DO:
      IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
      RUN send-fgemail (v-fgemail-file).
      IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
    END.
    IF fgPostLog THEN RUN fgPostLog ('End').
    IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
    
  end. /* for each FGReceiptRow */


END PROCEDURE.

PROCEDURE send-fgemail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-fgemail-file AS cha .

  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

  /* gdm - 12170901 */
  DEF BUFFER bf-job-hdr FOR job-hdr.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  DEF BUFFER bf-itemfg FOR itemfg.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     v-dir = users.user_program[2] + "\".
  ELSE
     v-dir = "c:\tmp\".

   FOR EACH tt-email,
       FIRST cust NO-LOCK WHERE cust.company = g_company
                           AND cust.cust-no = tt-email.cust-no
                           AND cust.active = "E" BREAK BY tt-email.cust-no:
       IF FIRST-OF(tt-email.cust-no) THEN DO:
          v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
          OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
          PUT STREAM st-email 
            "      Qty      JOB#       FG Item#          Part #          PO #            Item Name                 " SKIP
            "============ ========== =============== =============== =============== ==============================" SKIP.
       END.

       RELEASE bf-oe-ordl.

       /* gdm - 12170901 */
       FIND FIRST bf-job-hdr WHERE
            bf-job-hdr.company EQ g_company AND
            bf-job-hdr.job-no EQ tt-email.job-no AND
            bf-job-hdr.job-no2 EQ tt-email.job-no2 AND
            bf-job-hdr.i-no EQ tt-email.i-no AND
            bf-job-hdr.ord-no NE 0
            NO-LOCK NO-ERROR.

       IF AVAIL bf-job-hdr THEN
          FIND FIRST bf-oe-ordl WHERE
               bf-oe-ordl.company EQ g_company AND
               bf-oe-ordl.ord-no EQ bf-job-hdr.ord-no
               NO-LOCK NO-ERROR.
       
       FIND FIRST bf-itemfg WHERE
            bf-itemfg.company EQ g_company AND
            bf-itemfg.i-no EQ tt-email.i-no
            NO-LOCK NO-ERROR.

       PUT STREAM st-email UNFORMATTED
           tt-email.qty FORM "->>>,>>>,>>9" " " 
           tt-email.job-no + "-" + string(tt-email.job-no2,"99") FORM "x(10)"
           " " tt-email.i-no FORM "X(15)"
           " " (IF AVAIL bf-oe-ordl THEN bf-oe-ordl.part-no ELSE IF AVAIL bf-itemfg THEN bf-itemfg.part-no ELSE "") FORM "x(15)"
           " " (IF AVAIL bf-oe-ordl THEN bf-oe-ordl.po-no ELSE IF AVAIL bf-job-hdr THEN bf-job-hdr.po-no ELSE "") FORM "x(15)" 
           " " (IF AVAIL bf-oe-ordl THEN bf-oe-ordl.i-name ELSE IF AVAIL bf-itemfg THEN bf-itemfg.i-name ELSE "") FORM "x(30)"
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

