/*------------------------------------------------------------------------------
  Purpose:     
  meters:  <none>
  Notes:       not used anywhere else but oerep/r-loadtg.w in procedure run-report
               moved here b/c of size in appBuilder section editor
--------------------------*/

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-w-ord FOR w-ord.
DEF BUFFER bf-w-ord FOR w-ord.

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
DEFINE VARIABLE cLoadtagFile AS CHARACTER NO-UNDO.

SESSION:SET-WAIT-STATE ("general").

IF end_i-no EQ '' THEN end_i-no = 'zzzzzzzzzzzzzzz'.

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
  IF v-fitem[1] GT v-fitem[2] THEN
    ASSIGN v-fitem[1]     = end_i-no
           v-fitem[2]     = begin_i-no.

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
        ((rd_comps EQ "B" OR
        (rd_comps EQ "A" AND itemfg.alloc NE YES) OR
        (rd_comps EQ "U" AND itemfg.alloc)) OR 
        tbPartSelect)  THEN DO:
      RUN fg/fullset.p (ROWID(itemfg)).
      v-b-word-created = NO.
      FOR EACH tt-fg-set WHERE tt-fg-set.part-no <> w-ord.i-no
          AND (NOT tbPartSelect OR CAN-FIND(FIRST tt-comps WHERE tt-comps.comp EQ tt-fg-set.part-no)),
          FIRST b-itemfg
          WHERE b-itemfg.company EQ cocode
            AND b-itemfg.i-no    EQ tt-fg-set.part-no
          NO-LOCK:
        CREATE b-w-ord.
        BUFFER-COPY w-ord TO b-w-ord
        ASSIGN
         b-w-ord.i-no    = tt-fg-set.part-no
         b-w-ord.i-name  = b-itemfg.i-name
         b-w-ord.is-component = YES
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
  IF tbPartSelect THEN DO:
    FOR EACH w-ord:
      IF NOT CAN-FIND(FIRST tt-comps WHERE tt-comps.comp EQ w-ord.i-no) THEN
        DELETE w-ord.
    END.
  END.
  

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  SESSION:SET-WAIT-STATE ("").

  IF var-display-warning AND v-ord-list EQ "" AND
     begin_ord-no EQ 0 AND end_ord-no EQ 0 and v-po-no-source <> "J" THEN
     MESSAGE "Job does not contain an order number, hence data such as PO# will not print."
             VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  
  IF tbPartSelect THEN DO:  
    FOR EACH w-ord:
      FIND FIRST bf-w-ord WHERE bf-w-ord.ord-no EQ w-ord.ord-no
        AND bf-w-ord.i-no EQ w-ord.i-no
        AND bf-w-ord.job-no EQ w-ord.job-no
        AND bf-w-ord.job-no2 EQ w-ord.job-no2
        AND ROWID(bf-w-ord) NE ROWID(w-ord)
        NO-LOCK NO-ERROR.
      IF AVAIL bf-w-ord THEN
        DELETE w-ord.
    END.
  END.
  IF NOT tb_reprint-tag THEN
    RUN oerep/d-loadtg.w .
  choice = NO.
  FIND FIRST w-ord  NO-ERROR.
  lv-message = IF tb_reprint-tag  THEN "Are you Sure you Want to Reprint Loadtag File? " 
               ELSE "Are you Sure you Want to Create Loadtag File? "    .
  IF AVAIL w-ord THEN
     message lv-message
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
  

  IF NOT choice THEN RETURN ERROR.

   /* mdp adds logic to post to finish goods */
 

  SESSION:SET-WAIT-STATE ("general").
  {sys/inc/print1.i}
  {sys/inc/outprint.i value(lines-per-page)} 
      VIEW FRAME r-top.
      VIEW FRAME top.
  IF cBarCodeProgram EQ 'Loftware' then 
        cLoadtagFile = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME) + SUBSTRING(STRING(NOW),21,3) + '.csv'.
  ELSE cLoadtagFile = 'loadtag.txt'.
  IF v-out = "" THEN v-out = "c:~\ba~\label~\" + cLoadtagFile.
  ELSE do:
     IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
        SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
     ELSE v-out = v-out + "/".
     v-out = v-out + cLoadtagFile.
  END.
  IF choice THEN DO:
    IF OPSYS eq "UNIX" and v-loadtag ne "TRIAD" THEN DO:
      MESSAGE "Unable to Create Loadtag File for Non MSDos Platform.".
      PAUSE.
      RETURN.
    END.
    
    RUN create-text-file.
  END.

SESSION:SET-WAIT-STATE ("").
