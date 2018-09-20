/* -------------------------------------------------- rm/rm-autoi.i 12/96 JLF */
/* Raw Materials - Create autopost rm issues                                  */
/* -------------------------------------------------------------------------- */

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ {3}.company
      AND sys-ctrl.name    EQ "AUTOISSU"
    NO-LOCK NO-ERROR.

RELEASE rm-bin.

ll-neg = {4} LT 0.

IF PROGRAM-NAME(1) BEGINS "jc/issuemat." THEN
  RUN rm/d-selbin.w (ROWID(job-mat), "Job Material Issue", INPUT-OUTPUT {4}).

ELSE
IF {3}.i-code EQ "R" THEN
FOR EACH rm-bin NO-LOCK
    WHERE rm-bin.company EQ {3}.company
      AND rm-bin.i-no    EQ {3}.i-no
    USE-INDEX i-no:

  CREATE tt-bin.
  BUFFER-COPY rm-bin TO tt-bin.
  tt-date = TODAY.

  /* Reduce the tt-bin.qty if it has already been used */
  FOR EACH rm-rctd WHERE rm-rctd.company EQ rm-bin.company
    AND rm-rctd.tag = rm-bin.tag
    AND rm-rctd.loc = rm-bin.loc
    AND rm-rctd.loc-bin = rm-bin.loc-bin
    AND rm-rctd.i-no    = rm-bin.i-no:  
    tt-bin.qty = tt-bin.qty - rm-rctd.qty.
  END.
  RELEASE rm-rcpth.
  
  RUN rm/GetRMBinAgeDate.p (INPUT ROWID(rm-bin), OUTPUT tt-date).  
  IF tt-date EQ ? THEN
    tt-date = 1/1/1900.
  
END.

DO WHILE {4} GT 0 OR ll-neg:
   dLastAssigned = 0.
  /* Test to see if new record is really required: */
  /* Without this code, the same tag could be used twice, */
  /* i.e. the quantity could be split between two records with */
  /* the same tag#. To avoid that, set up record values as they would */
  /* be created but in a temp-table record. Then see if a record with */
  /* the exact same values exists already. If it does, can just add the  */
  /* current quantity in without creating a new record for it */
  IF NOT ll-neg AND {3}.i-code EQ "R" THEN DO:
  
    CREATE tt-rm-rctd-qty.
  
    ASSIGN
     tt-rm-rctd-qty.r-no      = li
     tt-rm-rctd-qty.company   = job-mat.company
     tt-rm-rctd-qty.rct-date  = TODAY
     tt-rm-rctd-qty.rita-code = "{5}"
     tt-rm-rctd-qty.i-no      = CAPS(job-mat.rm-i-no)
     tt-rm-rctd-qty.i-name    = {3}.i-name
     tt-rm-rctd-qty.job-no    = job-mat.job-no
     tt-rm-rctd-qty.job-no2   = job-mat.job-no2
     tt-rm-rctd-qty.pur-uom   = item.cons-uom
     tt-rm-rctd-qty.loc       = {3}.loc
     tt-rm-rctd-qty.loc-bin   = {3}.loc-bin
     tt-rm-rctd-qty.s-num     = job-mat.frm
     tt-rm-rctd-qty.b-num     = job-mat.blank-no
     tt-rm-rctd-qty.job-no    = job-mat.job-no
     tt-rm-rctd-qty.job-no2   = job-mat.job-no2
     tt-rm-rctd-qty.pass      = job-mat.pass
     tt-rm-rctd-qty.cost      = job-mat.std-cost
     tt-rm-rctd-qty.cost-uom  = job-mat.sc-uom
     .
    IF NOT ll-bin AND {3}.i-code EQ "R" THEN
    FOR EACH tt-bin WHERE tt-bin.qty GT 0 
        BY tt-date BY tt-bin.rec_key:
     
      ASSIGN
       tt-rm-rctd-qty.loc      = tt-bin.loc
       tt-rm-rctd-qty.loc-bin  = tt-bin.loc-bin
       tt-rm-rctd-qty.tag      = tt-bin.tag
       tt-rm-rctd-qty.cost     = tt-bin.cost
       tt-rm-rctd-qty.cost-uom = item.cons-uom
       tt-rm-rctd-qty.qty      = MIN({4},tt-bin.qty).

      LEAVE.
    END.
    
    RELEASE {1}.
    FIND FIRST {1} 
      WHERE {1}.company   = tt-rm-rctd-qty.company
        AND {1}.rct-date  = tt-rm-rctd-qty.rct-date
        AND {1}.rita-code = tt-rm-rctd-qty.rita-code
        AND {1}.i-no      = tt-rm-rctd-qty.i-no
        AND {1}.i-name    = tt-rm-rctd-qty.i-name
        AND {1}.job-no    = tt-rm-rctd-qty.job-no
        AND {1}.job-no2   = tt-rm-rctd-qty.job-no2
        AND {1}.pur-uom   = tt-rm-rctd-qty.pur-uom
        AND {1}.loc       = tt-rm-rctd-qty.loc
        AND {1}.loc-bin   = tt-rm-rctd-qty.loc-bin
        AND {1}.s-num     = tt-rm-rctd-qty.s-num
        AND {1}.b-num     = tt-rm-rctd-qty.b-num
        AND {1}.job-no    = tt-rm-rctd-qty.job-no
        AND {1}.job-no2   = tt-rm-rctd-qty.job-no2
        AND {1}.pass      = tt-rm-rctd-qty.pass
        AND {1}.cost      = tt-rm-rctd-qty.cost
        AND {1}.cost-uom  = tt-rm-rctd-qty.cost-uom
        AND {1}.loc       = tt-rm-rctd-qty.loc
        AND {1}.loc-bin   = tt-rm-rctd-qty.loc-bin
        AND {1}.tag       = tt-rm-rctd-qty.tag
        AND {1}.cost      = tt-rm-rctd-qty.cost
        AND {1}.cost-uom  = tt-rm-rctd-qty.cost-uom
      EXCLUSIVE-LOCK NO-ERROR.
  END.
  /* End test to see if a new record is required */

  IF NOT AVAIL {1} THEN DO:

    li = 0.
    RUN sys/ref/asiseq.p (INPUT job-mat.company, INPUT "rm_rcpt_seq", OUTPUT li) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  
    CREATE {1}.
    ASSIGN
     {1}.r-no      = li
     {1}.company   = job-mat.company
     {1}.rct-date  = TODAY
     {1}.rita-code = "{5}"
     {1}.i-no      = CAPS(job-mat.rm-i-no)
     {1}.i-name    = {3}.i-name
     {1}.job-no    = job-mat.job-no
     {1}.job-no2   = job-mat.job-no2
     {1}.pur-uom   = item.cons-uom
     {1}.loc       = {3}.loc
     {2}.loc-bin   = {3}.loc-bin
     {2}.s-num     = job-mat.frm
     {2}.b-num     = job-mat.blank-no
     {2}.job-no    = job-mat.job-no
     {2}.job-no2   = job-mat.job-no2
     {2}.pass      = job-mat.pass
     {2}.cost      = job-mat.std-cost
     {2}.cost-uom  = job-mat.sc-uom
     ll-bin        = NO.
  
    IF ll-neg THEN
    FOR EACH mat-act
        WHERE mat-act.company EQ job-mat.company
          AND mat-act.job     EQ job-mat.job
          AND mat-act.job-no  EQ job-mat.job-no
          AND mat-act.job-no2 EQ job-mat.job-no2
          AND mat-act.i-no    EQ job-mat.i-no
          AND mat-act.rm-i-no EQ job-mat.i-no
          AND mat-act.s-num   EQ job-mat.frm
          AND mat-act.b-num   EQ job-mat.blank-no
        BY mat-act.mat-date DESC:
  
      ASSIGN
       {2}.loc     = mat-act.loc
       {2}.loc-bin = mat-act.loc-bin
       {2}.tag     = mat-act.tag
       {2}.cost    = mat-act.cost
       {2}.qty     = {4}
       {4}         = 0
       ll-bin      = YES
       dLastAssigned = {2}.qty.
  
      LEAVE.
    END.
  
    IF NOT ll-bin AND {3}.i-code EQ "R" THEN
    FOR EACH tt-bin WHERE tt-bin.qty GT 0 
        BY tt-date BY tt-bin.rec_key:
      
      ASSIGN
       {2}.loc      = tt-bin.loc
       {2}.loc-bin  = tt-bin.loc-bin
       {2}.tag      = tt-bin.tag
       {2}.cost     = tt-bin.cost
       {2}.cost-uom = item.cons-uom
       {2}.qty      = MIN({4},tt-bin.qty)
       {4}          = {4} - {2}.qty
       ll-bin       = YES
       dLastAssigned = {2}.qty.

      DELETE tt-bin.
  
      LEAVE.
    END.
  END. /* If not {1} already available */
  ELSE DO:
    FOR EACH tt-bin WHERE tt-bin.qty GT 0 
        BY tt-date BY tt-bin.rec_key:
          LEAVE.
    END.
    
   /* Wade Kaldawi   3/10/2016   Ticket 12826
      Items where the inventory is not tracked can go below 0 */    
    IF NOT AVAIL tt-bin AND item.inv-by-cust THEN DO:
        FOR EACH tt-bin WHERE tt-bin.qty LE 0 
          BY tt-date BY tt-bin.rec_key:
          LEAVE.
        END.
    END.
    
    IF AVAIL tt-bin THEN DO:  
    
      ll-bin = TRUE.
      /* Wade Kaldawi   3/10/2016   Ticket 12826
         Items where the inventory is not tracked can go below 0 */
      IF item.inv-by-cust AND tt-bin.qty LE 0 THEN
      
        ASSIGN
          {2}.qty      = {2}.qty + {4}
          {4}          = 0.
      
      ELSE
      
        ASSIGN
          {2}.qty      = {2}.qty + MIN({4},tt-bin.qty)
          {4}          = {4} - MIN({4},tt-bin.qty).
        
        
      dLastAssigned = {2}.qty.
        
 
      DELETE tt-bin.
      
    END.

    DELETE tt-rm-rctd-qty.
  END. /* If record already exists */

  /* {2}.qty is only decimals 6 */
  {4} = TRUNCATE({4},6).

  /* Able to assign any more? */
  FIND FIRST tt-bin WHERE tt-bin.qty GT 0 NO-ERROR.

  IF dLastAssigned EQ 0 AND NOT ll-neg AND NOT AVAIL(tt-bin) AND {4} GT 0 THEN DO:
    
    /* Loop will never exit */
    ll-bin = NO.
  END.

  IF NOT ll-bin THEN DO:
    IF {2}.loc EQ "" OR {2}.loc-bin eq "" THEN DO:
      FIND FIRST cust NO-LOCK
          WHERE cust.company EQ job-mat.company
            AND cust.active  EQ "X" 
          NO-ERROR.
      IF AVAIL cust THEN DO:
        FIND FIRST shipto NO-LOCK
            WHERE shipto.company EQ job-mat.company
              AND shipto.cust-no EQ cust.cust-no
            NO-ERROR.
        IF AVAIL shipto THEN
          ASSIGN   
           {2}.loc     = shipto.loc
           {2}.loc-bin = shipto.loc-bin.
      END.
    END.

    ASSIGN
     {2}.qty = {4}
     {4}     = 0
     dLastAssigned = {2}.qty.
  END.

  /*Post automatically*/
  IF sys-ctrl.int-fld EQ 1 THEN
  DO:
     RUN pre-post.
     RUN post-rm.
  END.

  /* Prevent Infinite Loop */
  IF dLastAssigned EQ 0 THEN
    LEAVE.
  IF ll-neg THEN LEAVE.
END.

PROCEDURE pre-post:

   def var v-po-no like rm-rctd.po-no.
   def var v-autoissue as log.
   DEF VAR ld AS DEC NO-UNDO.
   def var v-whse like rm-rctd.loc.
   def var v-ext-cost as de.
   DEF VAR ll-one-item AS LOG NO-UNDO.

   DEF BUFFER b-tt-rctd FOR tt-rctd.
   DEF BUFFER b-item FOR ITEM.

   v-autoissue = sys-ctrl.log-fld.

   EMPTY TEMP-TABLE tt-rctd.
   EMPTY TEMP-TABLE work-gl.
   EMPTY TEMP-TABLE tt-mat.

   CREATE tt-rctd.
   BUFFER-COPY rm-rctd TO tt-rctd
   ASSIGN
    tt-rctd.rm-row-id = ROWID(rm-rctd)
    tt-rctd.has-rec   = YES
    tt-rctd.seq-no    = 1.
   RELEASE tt-rctd.

   auto-issue:
    for each tt-rctd
        where tt-rctd.rita-code eq "R"
          and tt-rctd.job-no    ne ""
        no-lock,
        first item
        where item.company eq cocode
          and item.i-no    eq tt-rctd.i-no
        no-lock.

      release po-ordl.

      v-po-no = trim(tt-rctd.po-no).
      if v-po-no ne "" then do:
        do x = 1 to length(v-po-no):
          if substr(v-po-no,x,1) lt "0" or
             substr(v-po-no,x,1) gt "9" then next auto-issue.
        end.

        find first po-ordl
            where po-ordl.company   eq cocode
              and po-ordl.i-no      eq tt-rctd.i-no
              and po-ordl.po-no     eq int(v-po-no)
              and po-ordl.job-no    eq tt-rctd.job-no
              and po-ordl.job-no2   eq tt-rctd.job-no2
              and po-ordl.item-type eq yes
            use-index item-ordno no-lock no-error.
      end.

      IF item.mat-type NE "I" OR AVAIL po-ordl THEN
        IF (item.i-code EQ "E" AND
            NOT AVAIL po-ordl)      OR
           (item.i-code EQ "R" AND
            NOT v-autoissue)        THEN NEXT auto-issue.

      EMPTY TEMP-TABLE tt-mat.
      
      RELEASE job.
      IF tt-rctd.job-no NE "" AND tt-rctd.s-num EQ ? THEN
      FIND FIRST job
        WHERE job.company EQ cocode
          AND job.job-no  EQ tt-rctd.job-no
          AND job.job-no2 EQ tt-rctd.job-no2
        NO-LOCK NO-ERROR.

      IF AVAIL job THEN DO:
        ld = 0.

        FOR EACH job-mat
            WHERE job-mat.company EQ job.company
              AND job-mat.job     EQ job.job
              AND job-mat.job-no  EQ job.job-no
              AND job-mat.job-no2 EQ job.job-no2
              AND job-mat.rm-i-no EQ tt-rctd.i-no
            NO-LOCK
            BY job-mat.frm:

          CREATE tt-mat.
          ASSIGN
           tt-mat.frm = job-mat.frm
           tt-mat.qty = job-mat.qty
           ld         = ld + job-mat.qty.
        END.

        FOR EACH tt-mat:
          tt-mat.qty = tt-rctd.qty * (tt-mat.qty / ld).
          IF tt-rctd.pur-uom EQ "EA" THEN DO:
            {sys/inc/roundup.i tt-mat.qty} 
          END.
        END.

        ld = 0.
        FOR EACH tt-mat:
          ld = ld + tt-mat.qty.
        END.

        IF ld NE tt-rctd.qty THEN
        FOR EACH tt-mat:
          tt-mat.qty = tt-mat.qty + (tt-rctd.qty - ld).
          LEAVE.
        END.
      END.

      ELSE DO:
        CREATE tt-mat.
        ASSIGN
         tt-mat.frm = tt-rctd.s-num
         tt-mat.qty = tt-rctd.qty.
      END.

      FOR EACH tt-mat:
        CREATE b-tt-rctd.
        BUFFER-COPY tt-rctd EXCEPT rec_key TO b-tt-rctd
        ASSIGN
         b-tt-rctd.rita-code = "I"
         b-tt-rctd.tt-row-id = ROWID(tt-rctd)
         b-tt-rctd.seq-no    = 2
         b-tt-rctd.s-num     = tt-mat.frm
         /*b-tt-rctd.po-no     = ""*/
         b-tt-rctd.qty       = tt-mat.qty.
        DELETE tt-mat.
      END.
    end.

    issue-adder-for-board:
    for each tt-rctd
        where tt-rctd.rita-code eq "I"
          and tt-rctd.job-no    ne ""
        no-lock,
        first job
        where job.company eq cocode
          and job.job-no  eq tt-rctd.job-no
          and job.job-no2 eq tt-rctd.job-no2
        no-lock,

        first item
        where item.company  eq cocode
          and item.i-no     eq tt-rctd.i-no
          and item.mat-type eq "B"
        no-lock:

      {rm/rm-addcr.i E tt-rctd tt-rctd b-}
        ASSIGN
	 b-tt-rctd.rita-code = "I"
         b-tt-rctd.tt-row-id = ROWID(tt-rctd)
         b-tt-rctd.seq-no    = 3	 .
      END.
    end.
    
    FOR EACH tt-rctd
        BREAK BY tt-rctd.loc                                             
              BY tt-rctd.i-no                                            
              BY tt-rctd.job-no                                          
              BY tt-rctd.job-no2 
              BY tt-rctd.loc-bin                           
              BY tt-rctd.tag
              BY RECID(tt-rctd):                                                   

      

      find first item no-lock
          where item.company eq cocode
            and item.i-no    eq tt-rctd.i-no
          no-error.

      release costtype.
      if avail item then
      find first costtype no-lock
          where costtype.company   eq cocode
            and costtype.cost-type eq item.cost-type
          no-error.

      release po-ord.
      if int(tt-rctd.po-no) ne 0 and tt-rctd.rita-code eq "R" then                                         
      find first po-ord no-lock
          where po-ord.company eq cocode
            and po-ord.po-no   eq int(tt-rctd.po-no)
          no-error.

      release po-ordl.
      if avail po-ord then
      find first po-ordl no-lock
          where po-ordl.company   eq cocode
            and po-ordl.po-no     eq po-ord.po-no
            and po-ordl.i-no      eq tt-rctd.i-no
            and po-ordl.job-no    eq tt-rctd.job-no
            and po-ordl.job-no2   eq tt-rctd.job-no2
            and po-ordl.s-num     eq tt-rctd.s-num
            and po-ordl.b-num     eq tt-rctd.b-num
            and po-ordl.deleted   eq no
            and po-ordl.item-type eq yes
          no-error.

      v-ext-cost = tt-rctd.cost * tt-rctd.qty.
       
      IF rmpostgl AND AVAIL costtype AND costtype.inv-asset NE ""  AND
         v-ext-cost NE 0 AND v-ext-cost NE ?                       THEN DO:

        if tt-rctd.rita-code EQ "R"  AND  
           costtype.ap-accrued NE "" THEN DO:

          /* Debit RM Asset */
          FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.inv-asset NO-LOCK NO-ERROR.
          IF NOT AVAIL work-gl THEN DO:
            CREATE work-gl.
            work-gl.actnum = costtype.inv-asset.
          END.
          work-gl.debits = work-gl.debits + v-ext-cost.

          /* Credit RM AP Accrued */
          FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.ap-accrued NO-LOCK NO-ERROR.
          IF NOT AVAIL work-gl THEN DO:
            CREATE work-gl.
            work-gl.actnum = costtype.ap-accrued.
          END.
          work-gl.credits = work-gl.credits + v-ext-cost.
        END.

        ELSE
        IF tt-rctd.rita-code EQ "I" AND
           tt-rctd.job-no NE ""     THEN DO:

          FOR EACH job-hdr
              WHERE job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ tt-rctd.job-no
                AND job-hdr.job-no2 EQ tt-rctd.job-no2
              NO-LOCK,
              FIRST job OF job-hdr NO-LOCK
              BREAK BY job-hdr.frm:
            ll-one-item = FIRST(job-hdr.frm) AND LAST(job-hdr.frm).
            LEAVE.
          END.

          FOR EACH job-hdr
              WHERE job-hdr.company     EQ cocode
                AND job-hdr.job-no      EQ tt-rctd.job-no
                AND job-hdr.job-no2     EQ tt-rctd.job-no2
                AND ((job-hdr.frm       EQ tt-rctd.s-num AND
                      (job-hdr.blank-no EQ tt-rctd.b-num OR tt-rctd.b-num EQ 0))
                 OR  ll-one-item)
              NO-LOCK,
              FIRST job OF job-hdr NO-LOCK,
              FIRST itemfg
              WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ job-hdr.i-no
              NO-LOCK,
              FIRST prodl
              WHERE prodl.company EQ cocode
                AND prodl.procat  EQ itemfg.procat
                AND CAN-FIND(FIRST prod
                             WHERE prod.company EQ cocode
                               AND prod.prolin  EQ prodl.prolin)
              NO-LOCK,
              FIRST prod
              WHERE prod.company EQ cocode
                AND prod.prolin  EQ prodl.prolin
                AND prod.wip-mat NE ""
              NO-LOCK:

            ld = ROUND(v-ext-cost * (IF ll-one-item        OR
                                        tt-rctd.b-num NE 0 OR
                                        job-hdr.sq-in LE 0 OR
                                        job-hdr.sq-in EQ ? THEN 1
                                     ELSE (job-hdr.sq-in / 100)),2).

            /* Debit FG Wip Material */
            FIND FIRST work-gl
                WHERE work-gl.job     EQ job-hdr.job
                  AND work-gl.job-no  EQ job-hdr.job-no
                  AND work-gl.job-no2 EQ job-hdr.job-no2
                  AND work-gl.actnum  EQ prod.wip-mat 
                NO-LOCK NO-ERROR.
            IF NOT AVAIL work-gl THEN DO:
              CREATE work-gl.
              ASSIGN
               work-gl.job     = job-hdr.job
               work-gl.job-no  = job-hdr.job-no
               work-gl.job-no2 = job-hdr.job-no2
               work-gl.actnum  = prod.wip-mat.
            END.
            work-gl.debits = work-gl.debits + ld.

            /* Credit RM Asset */
            FIND FIRST work-gl
                WHERE work-gl.job     EQ job-hdr.job
                  AND work-gl.job-no  EQ job-hdr.job-no
                  AND work-gl.job-no2 EQ job-hdr.job-no2
                  AND work-gl.actnum  EQ costtype.inv-asset
                NO-LOCK NO-ERROR.
            IF NOT AVAIL work-gl THEN DO:
              CREATE work-gl.
              ASSIGN
               work-gl.job     = job-hdr.job
               work-gl.job-no  = job-hdr.job-no
               work-gl.job-no2 = job-hdr.job-no2
               work-gl.actnum  = costtype.inv-asset.
            END.
            work-gl.credits = work-gl.credits + ld.
          END.
        END.
      END.
    END.

END PROCEDURE.

PROCEDURE post-rm:

   def buffer xrm-rctd     for rm-rctd.
   def buffer xrm-bin      for rm-bin.
   def buffer b-rm-rctd    for rm-rctd.
   def buffer b-item       for item.
   def buffer b-po-ordl    for po-ordl.
   def buffer b-job-mat    for job-mat.
   
   def var v-avg-cst   as log.
   def var ld-cvt-qty as dec no-undo.
   def var v-trnum like gl-ctrl.trnum no-undo.
   
   def var v-r-qty     as   dec                    no-undo.
   def var v-i-qty     as   dec                    no-undo.
   def var v-t-qty     as   dec                    no-undo.
   def var cost        as   dec                    no-undo.
   def var out-qty     as   dec                    no-undo.
   def var v-bwt       like item.basis-w           no-undo.
   def var v-len       like item.s-len             no-undo.
   def var v-wid       like item.s-wid             no-undo.
   def var v-dep       like item.s-dep             no-undo.
   def var v-recid     as   recid                  no-undo.
   DEF VAR li          AS   INT                    NO-UNDO.
   DEF VAR v-post-date AS DATE INIT TODAY          NO-UNDO.

    transblok:
      FOR FIRST tt-rctd WHERE
          CAN-FIND(FIRST item WHERE item.company EQ cocode
                                AND item.i-no    EQ tt-rctd.i-no):
      
      RELEASE rm-rctd.
      RELEASE item.
      li = 0.

      DO WHILE (NOT AVAIL rm-rctd OR NOT AVAIL item) AND li LT 1000:
         li = li + 1.

         FIND rm-rctd EXCLUSIVE-LOCK WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id
            NO-WAIT NO-ERROR.
      
         FIND FIRST item EXCLUSIVE-LOCK
             WHERE item.company EQ rm-rctd.company
               AND item.i-no    EQ rm-rctd.i-no
             USE-INDEX i-no NO-WAIT NO-ERROR.
      END.

      IF NOT AVAIL rm-rctd OR NOT AVAIL item THEN LEAVE.

      IF rm-rctd.rita-code EQ "I" AND INT(rm-rctd.po-no) NE 0 THEN
      FOR EACH xrm-rctd
          WHERE xrm-rctd.company   EQ cocode
            AND xrm-rctd.i-no      EQ rm-rctd.i-no
            AND xrm-rctd.rita-code EQ "R"
            AND xrm-rctd.po-no     EQ rm-rctd.po-no
            AND xrm-rctd.r-no      LT rm-rctd.r-no
          NO-LOCK:

        UNDO transblok.
      END.

      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) +
                               TRIM(rm-rctd.job-no)
            AND job.job-no2 EQ rm-rctd.job-no2
          NO-ERROR.

      /** Find Bin & if not avail then create it **/
      FIND FIRST rm-bin
          WHERE rm-bin.company EQ rm-rctd.company
            AND rm-bin.loc     EQ rm-rctd.loc
            AND rm-bin.i-no    EQ rm-rctd.i-no
            AND rm-bin.loc-bin EQ rm-rctd.loc-bin
            AND rm-bin.tag     EQ rm-rctd.tag
          NO-ERROR.
      IF NOT AVAIL rm-bin THEN DO:
        CREATE rm-bin.
        ASSIGN
         rm-bin.company = rm-rctd.company
         rm-bin.loc     = rm-rctd.loc
         rm-bin.loc-bin = rm-rctd.loc-bin
         rm-bin.tag     = rm-rctd.tag
         rm-bin.i-no    = rm-rctd.i-no.
      END. /* not avail rm-bin */

      ld-cvt-qty = rm-rctd.qty.

      IF rm-rctd.pur-uom NE item.cons-uom AND item.cons-uom NE "" THEN
        RUN sys/ref/convquom.p (rm-rctd.pur-uom, item.cons-uom,
                              item.basis-w,
                              (if item.r-wid eq 0 then item.s-len else 12), 
                              (if item.r-wid eq 0 then item.s-wid else item.r-wid),
                              item.s-dep,
                              ld-cvt-qty, OUTPUT ld-cvt-qty).

      if rm-rctd.rita-code eq "R" then do:        /** RECEIPTS **/
        {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        assign
         rm-bin.qty     = rm-bin.qty + ld-cvt-qty
         item.last-cost = rm-rctd.cost
         item.q-onh     = item.q-onh + ld-cvt-qty.

        {rm/rm-poupd.i 2}

        item.q-avail = item.q-onh + item.q-ono - item.q-comm.
      end. /* R */

      else
      if rm-rctd.rita-code eq "I" then do:  /** ISSUES **/
        if avail job and job.job-no ne "" then do:
          run rm/mkjobmat.p (recid(rm-rctd),rm-rctd.company, output v-recid).

          find job-mat where recid(job-mat) eq v-recid no-error.

          if not avail job-mat then do:
            undo transblok.
          end.

          assign
           v-bwt = job-mat.basis-w
           v-len = job-mat.len
           v-wid = job-mat.wid
           v-dep = item.s-dep.

          if v-len eq 0 then v-len = item.s-len.

          if v-wid eq 0 then
            v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.

          if v-bwt eq 0 then v-bwt = item.basis-w.

          if index("RL",job.stat) ne 0 then job.stat = "W".

          {rm/rmmatact.i}            /* Create Actual Material */

          out-qty = rm-rctd.qty.
          IF rm-rctd.pur-uom NE job-mat.qty-uom AND rm-rctd.pur-uom NE "" THEN
            RUN sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.qty-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rctd.qty, output out-qty).

          cost = rm-rctd.cost.
          IF rm-rctd.pur-uom NE job-mat.sc-uom AND rm-rctd.pur-uom NE "" THEN
            RUN sys/ref/convcuom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rctd.cost, OUTPUT cost).

          assign
           mat-act.qty-uom = job-mat.qty-uom
           mat-act.cost    = cost
           mat-act.qty     = mat-act.qty     + out-qty
           job-mat.qty-iss = job-mat.qty-iss + out-qty
           job-mat.qty-all = job-mat.qty-all - out-qty
           item.q-comm     = item.q-comm     - rm-rctd.qty.

          run sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                 v-bwt, v-len, v-wid, v-dep,
                                 rm-rctd.qty, output out-qty).

          mat-act.ext-cost = mat-act.ext-cost + (cost * out-qty).

          /* Don't relieve more than were allocated */
          if job-mat.qty-all lt 0 then do:
            run sys/ref/convquom.p(job-mat.qty-uom, rm-rctd.pur-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   job-mat.qty-all, output out-qty).
            assign
             job-mat.qty-all = 0
             item.q-comm     = item.q-comm - out-qty.
          end.
          
          if item.q-comm lt 0 then item.q-comm = 0.

          IF item.mat-type EQ "B" THEN RUN rm/rm-addcr.p (ROWID(rm-rctd)).
        end.

        find first rm-bin
            where rm-bin.company eq rm-rctd.company
              and rm-bin.loc     eq rm-rctd.loc
              and rm-bin.i-no    eq rm-rctd.i-no
              and rm-bin.loc-bin eq rm-rctd.loc-bin
              and rm-bin.tag     eq rm-rctd.tag
            no-error.

        assign
         rm-bin.qty     = rm-bin.qty - ld-cvt-qty
         item.q-onh     = item.q-onh - ld-cvt-qty
         item.qlast-iss = rm-rctd.qty
         item.dlast-iss = rm-rctd.rct-date
         item.q-ytd     = item.q-ytd + rm-rctd.qty
         item.q-ptd     = item.q-ptd + rm-rctd.qty
         item.u-ptd     = item.u-ptd + (rm-rctd.cost * rm-rctd.qty)
         item.u-ytd     = item.u-ytd + (rm-rctd.cost * rm-rctd.qty)
         item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
      end.  /* I */

      else
      if rm-rctd.rita-code eq "A" then do:  /** ADJUSTMENTS **/
        if rm-rctd.cost ne 0 then do:
          {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}
        end.

        assign
         rm-bin.qty     = rm-bin.qty + ld-cvt-qty
         item.last-cost = if rm-rctd.cost ne 0 then rm-rctd.cost
                                               else item.last-cost
         item.q-onh     = item.q-onh + ld-cvt-qty
         item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
      end. /* A */

      else
      if rm-rctd.rita-code eq "T" then do:  /** TRANSFERS **/
        assign
         rm-bin.qty   = rm-bin.qty - rm-rctd.qty
         rm-rctd.cost = rm-bin.cost.

        /* This code is to handel the Transfer to quantity to increase the BIN
           using a buffer record so current rm-bin record is not updated. */

        find first xrm-bin
             where xrm-bin.company eq rm-rctd.company
               and xrm-bin.loc     eq rm-rctd.loc2
               and xrm-bin.i-no    eq rm-rctd.i-no
               and xrm-bin.loc-bin eq rm-rctd.loc-bin2
               and xrm-bin.tag     eq rm-rctd.tag2
             no-error.
        if not avail xrm-bin then do:
          create xrm-bin.
          assign
           xrm-bin.company = rm-rctd.company
           xrm-bin.loc     = rm-rctd.loc2
           xrm-bin.loc-bin = rm-rctd.loc-bin2
           xrm-bin.tag     = rm-rctd.tag2
           xrm-bin.i-no    = rm-rctd.i-no.
        end.

        {rm/rm-post.i "xrm-bin.qty" "xrm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        xrm-bin.qty = xrm-bin.qty + rm-rctd.qty.
      end. /* T */

/*       /** Delete Bins With Zero Quantities. **/ */
/*       IF rm-bin.qty EQ 0 THEN DELETE rm-bin.    */

      RELEASE loadtag.
      IF TRIM(rm-rctd.tag) NE "" THEN
      FIND FIRST loadtag EXCLUSIVE-LOCK 
          WHERE loadtag.company     EQ rm-rctd.company
            AND loadtag.item-type   EQ YES
            AND loadtag.tag-no      EQ rm-rctd.tag
            AND loadtag.i-no        EQ rm-rctd.i-no
            AND loadtag.is-case-tag EQ NO
          NO-ERROR.

      IF AVAIL loadtag THEN DO:
        IF rm-rctd.rita-code EQ "T" THEN 
          ASSIGN
           loadtag.loc     = rm-rctd.loc2
           loadtag.loc-bin = rm-rctd.loc-bin2.
        ELSE
          ASSIGN
           loadtag.loc     = rm-rctd.loc
           loadtag.loc-bin = rm-rctd.loc-bin.

        li = INDEX("RI",rm-rctd.rita-code).

        IF li EQ 1 AND (NOT AVAIL rm-bin OR rm-bin.qty LT 0) THEN li = 3.

        IF li GT 0 THEN loadtag.sts = ENTRY(li,"Received,Issued,Deleted").
      END.

      /*if last-of(tt-rctd.i-no) then /* Calculate average cost */*/
      for each rm-bin
          where rm-bin.company eq rm-rctd.company
            and rm-bin.i-no    eq rm-rctd.i-no
          no-lock use-index i-no
          break by rm-bin.i-no:

        if first(rm-bin.i-no) then
          assign
           v-i-qty = 0
           cost    = 0.

        v-r-qty = rm-bin.qty.

        if v-r-qty lt 0 then v-r-qty = v-r-qty * -1.

        assign
         v-i-qty = v-i-qty + v-r-qty
         cost    = cost    + (v-r-qty * rm-bin.cost).

        IF cost EQ ? THEN cost = 0.

        if last(rm-bin.i-no) and v-i-qty ne 0 AND cost NE 0 THEN item.avg-cost = cost / v-i-qty.

        /* gdm - 10280903 - Assign prep code received date */
        RUN assign-prep-info. 

      end. /* each rm-bin */      

      RUN final-steps.

      IF AVAIL rm-rctd AND
         rm-rctd.rita-code eq "ADDER" THEN
         rm-rctd.rita-code = "I".

      FIND CURRENT item NO-LOCK NO-ERROR.
      FIND CURRENT loadtag NO-LOCK NO-ERROR.
      FIND CURRENT rm-rcpth NO-LOCK NO-ERROR.
      FIND CURRENT rm-rdtlh NO-LOCK NO-ERROR.
      FIND CURRENT mat-act NO-LOCK NO-ERROR.
      FIND CURRENT job NO-LOCK NO-ERROR.
      FIND CURRENT job-mat NO-LOCK NO-ERROR.
    end. /* for each rm-rctd */

    IF rmpostgl THEN DO TRANSACTION:
      /* gdm - 11050906 */
      REPEAT:

        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
          WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAIL gl-ctrl THEN DO:
          ASSIGN v-trnum       = gl-ctrl.trnum + 1
                 gl-ctrl.trnum = v-trnum.

          FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.

          RUN gl-from-work (1, v-trnum).
          RUN gl-from-work (2, v-trnum).
          LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */
    END. /* IF rmpostgl */

END PROCEDURE.

PROCEDURE assign-prep-info:
   
   FOR EACH job-hdr WHERE
       job-hdr.company EQ rm-rctd.company AND
       job-hdr.job-no  EQ rm-rctd.job-no AND
       job-hdr.job-no2 EQ rm-rctd.job-no2
       NO-LOCK:

       FIND FIRST itemfg WHERE
            itemfg.company EQ job-hdr.company AND
            itemfg.i-no    EQ job-hdr.i-no
            NO-LOCK NO-ERROR.
       IF AVAIL itemfg THEN DO:

       IF itemfg.plate-no NE "" THEN DO: 
          FIND FIRST prep WHERE
               prep.company EQ cocode AND
               prep.code    EQ itemfg.plate-no
               NO-ERROR.
          IF AVAIL prep THEN ASSIGN prep.received-date = rm-rctd.rct-date.
       END.

       IF itemfg.die-no NE "" THEN DO:
         FIND FIRST prep WHERE
              prep.company EQ cocode AND
              prep.code    EQ itemfg.die-no
              NO-ERROR.
         IF AVAIL prep THEN ASSIGN prep.received-date = rm-rctd.rct-date.
       END.
       RELEASE prep.

     END. /* avail itemfg */
   END. /* EACH job-hdr */
    
END PROCEDURE.

PROCEDURE final-steps:

  DEF BUFFER b-tt-rctd FOR tt-rctd.
  DEF BUFFER rec-rm-rdtlh FOR rm-rdtlh.
  DEF BUFFER rec-rm-rcpth FOR rm-rcpth.

  DEF VAR v-int AS INT NO-UNDO.
  DEF VAR v-qty-received AS DEC NO-UNDO.
  DEF VAR v-post-date AS DATE INIT TODAY NO-UNDO.

  IF rm-rctd.rita-code EQ "I" AND TRIM(rm-rctd.tag) NE "" THEN
     FOR EACH rec-rm-rdtlh NO-LOCK
         WHERE rec-rm-rdtlh.company   EQ rm-rctd.company
           AND rec-rm-rdtlh.tag       EQ rm-rctd.tag
           AND rec-rm-rdtlh.rita-code EQ "R"
         USE-INDEX tag,
         FIRST rec-rm-rcpth
         WHERE rec-rm-rcpth.r-no      EQ rec-rm-rdtlh.r-no
           AND rec-rm-rdtlh.rita-code EQ rec-rm-rdtlh.rita-code
         NO-LOCK:
           
       IF rm-rctd.po-no EQ "" THEN rm-rctd.po-no = rec-rm-rcpth.po-no.
    
       IF rm-rctd.job-no EQ "" THEN
         ASSIGN
          rm-rctd.job-no = rec-rm-rcpth.job-no
          rm-rctd.job-no2 = rec-rm-rcpth.job-no2.
    
       LEAVE.
     END.
  
  {rm/rm-rctd.i rm-rcpth rm-rdtlh rm-rctd} /* Create History Records */

  DELETE rm-rctd.

  FOR EACH b-tt-rctd WHERE b-tt-rctd.tt-row-id EQ ROWID(tt-rctd):
    v-int = 0.
  RUN sys/ref/asiseq.p (INPUT job-mat.company, INPUT "rm_rcpt_seq", OUTPUT v-int) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


    CREATE rm-rctd.
    BUFFER-COPY b-tt-rctd TO rm-rctd
    ASSIGN
     rm-rctd.r-no        = v-int
     b-tt-rctd.r-no      = rm-rctd.r-no
     b-tt-rctd.has-rec   = YES
     b-tt-rctd.rm-row-id = ROWID(rm-rctd).
  END.

  DELETE tt-rctd.
  
END PROCEDURE.

PROCEDURE gl-from-work:

   DEF INPUT PARAM ip-run AS INT NO-UNDO.
   DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
   
   def var credits as dec init 0 no-undo.
   def var debits as dec init 0 no-undo. 
   DEF VAR v-post-date AS DATE INIT TODAY NO-UNDO.

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
        gltrans.jrnl    = "RMPOST"
        gltrans.period  = period.pnum
        gltrans.tr-amt  = debits - credits
        gltrans.tr-date = v-post-date
        gltrans.tr-dscr = if work-gl.job-no NE "" then "RM Issue to Job"
                                                  else "RM Receipt"
        gltrans.trnum   = ip-trnum
        debits  = 0
        credits = 0.
  
       RELEASE gltrans.
     end.
   end.
END.
