/*------------------------------------------------------------------------------
  Purpose:     
  meters:  <none>
  Notes:       not used anywhere else but oerep/r-loadtg.w in procedure run-report
               moved here b/c of size in appBuilder section editor
--------------------------*/

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-w-ord FOR w-ord.

DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF VAR lv-job-no LIKE job.job-no NO-UNDO.
DEF VAR lv-job-no2 LIKE job.job-no NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.

DEF VAR v-b-word-created AS LOG NO-UNDO.
DEF VAR lv-message AS cha NO-UNDO.
DEF VAR op-warning AS LOG NO-UNDO.
DEF VAR var-display-warning AS LOG NO-UNDO.

IF end_i-no EQ '' THEN end_i-no = 'zzzzzzzzzzzzzzz'.
MESSAGE " begin_ord-no " begin_ord-no end_ord-no begin_i-no end_i-no .
ASSIGN
  v-ford-no[1]   = begin_ord-no
  v-ford-no[2]   = end_ord-no
  v-fitem[1]     = begin_i-no
  v-fitem[2]     = end_i-no
  by-release     = tb_rel
  v-po-no-source = rd_print
  form#          = begin_form
  copy_count     = begin_labels
  form_fid       = begin_filename
  v-stat         = rd_order-sts.
  
  IF v-ford-no[1] EQ 0 AND v-ford-no[2] EQ 0 AND
     begin_job EQ '' AND end_job EQ '' AND 
     v-ord-list EQ '' AND v-job-list EQ '' AND
     v-fitem[1] EQ '' AND v-fitem[2] BEGINS 'z' THEN
  v-fitem[2] = ''. /* prevent mass create for all items */
  
  FOR EACH w-ord:
    DELETE w-ord.
  END.
  FOR EACH tt-tag:
    DELETE tt-tag.
  END.
  FOR EACH w-file:
    DELETE w-file.
  END.
  EMPTY TEMP-TABLE ttblJob.

  IF v-job-list <> "" and
     (asc(SUBSTRING(v-job-list,LENGTH(v-job-list),1)) = 10 OR
      asc(SUBSTRING(v-job-list,LENGTH(v-job-list),1)) = 13 )
  THEN v-job-list = substring(v-job-list,1,LENGTH(v-job-list) - 1).
  DO i = 1 TO NUM-ENTRIES(v-job-list).
    ASSIGN
     ll = YES
     lv-job-no  = ""
     lv-job-no2 = "".
    DO li = 1 TO LENGTH(ENTRY(i,v-job-list)):
      IF INDEX("/:-",SUBSTR(ENTRY(i,v-job-list),li,1)) GT 0 THEN
        IF ll THEN ll = NO.
        ELSE LEAVE.
      ELSE
      IF ll THEN lv-job-no = lv-job-no + SUBSTR(ENTRY(i,v-job-list),li,1).
            ELSE lv-job-no2 = lv-job-no2 + SUBSTR(ENTRY(i,v-job-list),li,1).
    END.
    lv-job-no = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no) +
                STRING(INT(lv-job-no2),"99") NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND
       lv-job-no NE "" AND NOT(lv-job-no GE FILL(" ",6 - LENGTH(TRIM(begin_job))) + TRIM(begin_job) + STRING(begin_job2,"99") AND
                               lv-job-no LE FILL(" ",6 - LENGTH(TRIM(end_job))) + TRIM(end_job) + STRING(end_job2,"99")) THEN
       RUN temp-job (lv-job-no).
  END.

  FOR EACH w-file:       
    RUN from-job (w-key,OUTPUT op-warning).

    IF op-warning THEN
       var-display-warning = YES.
  END.

  IF begin_job NE '' OR end_job NE '' THEN
  FOR EACH job
      WHERE job.company EQ cocode
        AND job.job-no  GE FILL(" ",6 - LENGTH(TRIM(begin_job))) + TRIM(begin_job)
        AND job.job-no  LE FILL(" ",6 - LENGTH(TRIM(end_job)))   + TRIM(end_job)
        AND FILL(" ",6 - LENGTH(TRIM(job.job-no))) + TRIM(job.job-no) + STRING(job.job-no2,"99")
                        GE
            FILL(" ",6 - LENGTH(TRIM(begin_job))) + TRIM(begin_job) + STRING(begin_job2,"99")
        AND FILL(" ",6 - LENGTH(TRIM(job.job-no))) + TRIM(job.job-no) + STRING(job.job-no2,"99")
                        LE
            FILL(" ",6 - LENGTH(TRIM(end_job))) + TRIM(end_job) + STRING(end_job2,"99")
        AND (v-stat EQ "A"                         OR
             (v-stat EQ "C" AND NOT job.opened) OR
             (v-stat EQ "O" AND job.opened))
      NO-LOCK:
    RUN from-job (ROWID(job),OUTPUT op-warning).

    IF op-warning THEN
       var-display-warning = YES.
  END.
  FOR EACH w-file:
    DELETE w-file.
  END.
  IF v-ord-list <> "" AND 
     (asc(SUBSTRING(v-ord-list,LENGTH(v-ord-list),1)) = 10 OR
     asc(SUBSTRING(v-ord-list,LENGTH(v-ord-list),1)) = 13 )
  THEN v-ord-list = substring(v-ord-list,1,LENGTH(v-ord-list) - 1).
  DO i = 1 TO NUM-ENTRIES(v-ord-list).
    lv-ord-no = INT(ENTRY(i,v-ord-list)) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND lv-ord-no NE 0 THEN DO:
      IF loadtagFunction EQ 'PO' THEN RUN temp-po (lv-ord-no).
                                 ELSE RUN temp-ord (lv-ord-no).
    END.
  END.
  FOR EACH w-file: 
    IF loadtagFunction EQ 'PO' THEN DO:
      FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ w-file.w-key.
      RUN from-po.
    END.
    ELSE RUN from-ord (w-file.w-key).
  END.
  CASE loadtagFunction:
    WHEN 'PO' THEN
    FOR EACH po-ord NO-LOCK
        WHERE po-ord.company EQ cocode
          AND po-ord.po-no   GE v-ford-no[1]
          AND po-ord.po-no   LE v-ford-no[2]
          AND (v-stat EQ "A"                         OR
               (v-stat EQ "C" AND NOT po-ord.opened) OR
               (v-stat EQ "O" AND po-ord.opened)):
      RUN from-po.
    END. /* each po-ord */
    WHEN 'Order' THEN
    FOR EACH oe-ord
        WHERE oe-ord.company EQ cocode
          AND oe-ord.ord-no  GE v-ford-no[1]
          AND oe-ord.ord-no  LE v-ford-no[2]
          AND (v-stat EQ "A"                         OR
               (v-stat EQ "C" AND NOT oe-ord.opened) OR
               (v-stat EQ "O" AND oe-ord.opened))
        NO-LOCK:
        MESSAGE "ordercheck " .
      RUN from-ord (ROWID(oe-ord)).
    END. /* each oe-ord */
  END CASE.

  

  FOR EACH w-ord,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-ord.i-no
        AND itemfg.isaset  EQ YES
      NO-LOCK:

    IF w-ord.dont-run-set EQ NO AND
        (rd_comps EQ "B" OR
        (rd_comps EQ "A" AND itemfg.alloc NE YES) OR
        (rd_comps EQ "U" AND itemfg.alloc)) THEN DO:
      RUN fg/fullset.p (ROWID(itemfg)).
      v-b-word-created = NO.
      FOR EACH tt-fg-set WHERE tt-fg-set.part-no <> w-ord.i-no,
          FIRST b-itemfg
          WHERE b-itemfg.company EQ cocode
            AND b-itemfg.i-no    EQ tt-fg-set.part-no
          NO-LOCK:
        CREATE b-w-ord.
        BUFFER-COPY w-ord TO b-w-ord
        ASSIGN
         b-w-ord.i-no    = tt-fg-set.part-no
         b-w-ord.i-name  = b-itemfg.i-name
         b-w-ord.ord-qty = w-ord.ord-qty * tt-fg-set.part-qty-dec
         b-w-ord.box-len = b-itemfg.l-score[50]
         b-w-ord.box-wid = b-itemfg.w-score[50]
         b-w-ord.box-dep = b-itemfg.d-score[50].
        v-b-word-created = YES.
        FIND FIRST est
            WHERE est.company eq cocode
              AND est.est-no  eq w-ord.est-no
            NO-LOCK NO-ERROR.
        RELEASE eb.
        IF AVAIL est THEN
        CASE loadtagFunction:
          WHEN 'Order' THEN
          FIND FIRST eb NO-LOCK WHERE eb.company EQ cocode
                                  AND eb.est-no EQ w-ord.est-no
                               /* AND eb.form-no EQ w-ord.form-no
                                  AND eb.part-no EQ w-ord.cust-part-no */ 
                                  AND eb.stock-no EQ tt-fg-set.part-no NO-ERROR.
          WHEN 'PO' THEN
          FIND FIRST eb NO-LOCK WHERE eb.company EQ cocode
                                  AND eb.est-no EQ w-ord.est-no
                                  AND eb.stock-no EQ w-ord.i-no NO-ERROR.
        END CASE.
        IF AVAIL eb THEN
          ASSIGN
           b-w-ord.flute      = eb.flute
           b-w-ord.test       = eb.test
           b-w-ord.pcs        = eb.cas-cnt
           b-w-ord.bundle     = eb.cas-pal
           b-w-ord.total-unit = b-w-ord.pcs * b-w-ord.bundle
           b-w-ord.form-no    = eb.form-no
           b-w-ord.cas-no     = eb.cas-no
           b-w-ord.pallt-no   = eb.tr-no.
           b-w-ord.total-tags = ((b-w-ord.ord-qty / b-w-ord.total-unit) + .49) +
                             (IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1) .
      END. /* each tt-fg-set */
      IF v-b-word-created AND itemfg.alloc THEN DELETE w-ord.
    END. /* if dont-run-set */
  END. /* each w-ord */


  IF NOT CAN-FIND(FIRST w-ord)  AND
     v-ford-no[1] EQ 0 AND v-ford-no[2] EQ 0 AND
     begin_job EQ '' AND end_job EQ '' AND 
     v-ord-list = "" AND v-job-list = "" THEN
     RUN createWOrdFromItem (v-fitem[1],v-fitem[2]).

  ASSIGN
   str-tit  = coname + " - " + loname
   str-tit2 = "DOWNLOAD LOADTAG DATA"
   x = (56 - length(str-tit)) / 2
   str-tit  = FILL(" ",x) + str-tit
   x = (56 - length(str-tit2)) / 2
   str-tit2 = FILL(" ",x) + str-tit2.

  RUN final-update.
/*  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  SESSION:SET-WAIT-STATE ("").*/

  /*IF var-display-warning AND v-ord-list EQ "" AND
     begin_ord-no EQ 0 AND end_ord-no EQ 0 THEN
     MESSAGE "Job does not contain an order number, hence data such as PO# will not print."
             VIEW-AS ALERT-BOX WARNING BUTTONS OK.*/
  

IF prmExtra = "First" THEN do:
 IF NOT tb_reprint-tag THEN DO:
     MESSAGE "tttable-check  " .
    /*RUN oerep/d-loadtg.w .*/
  FOR EACH w-ord NO-LOCK:
      
MESSAGE "tttable-check22  " .
  FIND FIRST itemfg NO-LOCK
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no EQ w-ord.i-no NO-ERROR.
  IF AVAIL itemfg THEN ASSIGN v-weight-100 = itemfg.weight-100.

  IF v-weight-100 > 0 
    THEN ASSIGN w-ord.unit-wt  = v-weight-100
                w-ord.pallt-wt = w-ord.bundle * v-weight-100.
    ELSE ASSIGN w-ord.unit-wt  = 0
                w-ord.pallt-wt = 0.

      CREATE ttSharpShooterLoadTag.
     ASSIGN
      ttSharpShooterLoadTag.ord-no        =  string(w-ord.ord-no )
      ttSharpShooterLoadTag.ldtag2        =  w-ord.cust-no
      ttSharpShooterLoadTag.job-no        =  w-ord.job-no     
      ttSharpShooterLoadTag.job-no2       =  w-ord.job-no2    
      ttSharpShooterLoadTag.i-no          =  w-ord.i-no            
      ttSharpShooterLoadTag.ord-qty       =  w-ord.ord-qty         
      ttSharpShooterLoadTag.over-pct      =  w-ord.over-pct        
      ttSharpShooterLoadTag.pcs           =  w-ord.pcs             
      ttSharpShooterLoadTag.bundle        =  w-ord.bundle          
      ttSharpShooterLoadTag.total-unit    =  w-ord.total-unit      
      ttSharpShooterLoadTag.total-tags    =  w-ord.total-tags      
      ttSharpShooterLoadTag.partial       =  w-ord.partial         
      ttSharpShooterLoadTag.unit-wt       =  w-ord.unit-wt         
      ttSharpShooterLoadTag.pallt-wt      =  w-ord.pallt-wt        
      ttSharpShooterLoadTag.lot           =  w-ord.lot             
      ttSharpShooterLoadTag.i-name        =  w-ord.i-name          
      ttSharpShooterLoadTag.cust-po-no    =  w-ord.cust-po-no  
      ttSharpShooterLoadTag.ldtag         =  w-ord.rec_key 
      ttSharpShooterLoadTag.tagno         =  prmScnCs_lbl  .
     
  END.
 END.
END.  /* end of prmExtra */



IF prmExtra = "Second" THEN DO:

     DEF VAR filepath AS CHAR FORM "x(200)" NO-UNDO.
     INPUT FROM VALUE(extra) APPEND.
        REPEAT TRANSACTION ON ERROR UNDO, LEAVE:
            CREATE tt-loadtag.
            /*IMPORT tt-csvrelbol.   */
            IMPORT DELIMITER "," tt-loadtag.
        END. /*REPEAT TRANSACTION*/
    INPUT CLOSE.
  
FOR EACH tt-loadtag NO-LOCK:
    IF tt-loadtag.tagrowid = ""  THEN NEXT .
    
    FIND FIRST w-ord WHERE w-ord.rec_key EQ tt-loadtag.tagrowid EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL w-ord THEN do:
    ASSIGN
        
        w-ord.over-pct      = tt-loadtag.over-pct      
        w-ord.pcs           = tt-loadtag.pcs           
        w-ord.bundle        = tt-loadtag.bundle        
        w-ord.total-unit    = tt-loadtag.total-unit    
        w-ord.total-tags    = tt-loadtag.total-tags    
        w-ord.partial       = tt-loadtag.partial       
        w-ord.lot           = tt-loadtag.lot           
          .
    
       FIND CURRENT w-ord NO-LOCK NO-ERROR.
         DELETE tt-loadtag .
    END.
    IF NOT AVAIL w-ord THEN DO:
        FIND FIRST w-ord NO-LOCK NO-ERROR.
        DEF BUFFER bf-word FOR w-ord.

            CREATE bf-word.
            BUFFER-COPY w-ord TO bf-word.
            ASSIGN bf-word.total-tags = 1
                bf-word.bundle = 0
                bf-word.partial = 0
                bf-word.total-unit = 0
                .

  IF lookup(v-loadtag,"SSLABEL,CentBox,SSBARONE") > 0 THEN DO:
     FIND itemfg WHERE itemfg.company = g_company AND
                       itemfg.i-no = w-ord.i-no NO-LOCK NO-ERROR.
     IF AVAIL itemfg THEN FIND style WHERE style.company = g_company
                                       AND style.style = itemfg.style NO-LOCK NO-ERROR.
     IF AVAIL style AND style.industry = "2" THEN bf-word.pcs = 0.
  END.
  ASSIGN
        w-ord.over-pct      = tt-loadtag.over-pct      
        w-ord.pcs           = tt-loadtag.pcs           
        w-ord.bundle        = tt-loadtag.bundle        
        w-ord.total-unit    = tt-loadtag.total-unit    
        w-ord.total-tags    = tt-loadtag.total-tags    
        w-ord.partial       = tt-loadtag.partial       
        w-ord.lot           = tt-loadtag.lot           
        .

    END. /* not avail w-ord */

END.



  choice = NO.
  FIND FIRST w-ord  NO-ERROR.
  /*lv-message = IF tb_reprint-tag  THEN "Are you Sure you Want to Reprint Loadtag File? " 
               ELSE "Are you Sure you Want to Create Loadtag File? "    .
  IF AVAIL w-ord THEN
     message lv-message
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.*/
  

  
   /* mdp adds logic to post to finish goods */
  
  {sys/inc/print1.i}
      
  {sys/inc/outprint.i value(lines-per-page)} 
      VIEW FRAME r-top.
      VIEW FRAME top.
  IF v-out = "" THEN v-out = "c:~\ba~\label~\loadtag.txt".
  ELSE do:
     IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
        SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
     ELSE v-out = v-out + "/".
     v-out = v-out + "loadtag.txt".
  END.
  
    RUN create-text-file.

END. /* END of prmExtra second  */
