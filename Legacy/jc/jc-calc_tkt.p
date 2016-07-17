/* jc/jc-calc.p
  09/14/04  YSK  TASK 09130412   */
/* CF Production Control Test Run */

def input parameter rec-id as recid no-undo.
{sys/inc/var.i shared}
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xeb2 for eb.
def buffer x-job for job.
def var v-est-qty as dec no-undo.
def var save_id as recid no-undo.
def new shared var v-rebuild as log format "R/E" no-undo.
def var v-est-job like job.est-no no-undo.
def var v-up like job-mat.n-up no-undo.
def var v-out like ef.n-out no-undo.
def var v-job-hdr as log no-undo.
def var v-hold-qty like job-hdr.qty no-undo.
def var choice as log no-undo.
DEF VAR v-yld-qty AS DEC NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-format-f AS CHAR NO-UNDO.
DEF VAR lv-date AS DATE NO-UNDO.
DEF VAR v-on-f AS INT NO-UNDO.
DEF VAR v-print-job AS LOG NO-UNDO. /* flag for auto job ticket print Task#09180401*/
DEF VAR v-copies AS INT NO-UNDO.
DEF VAR v-printer-name AS cha NO-UNDO.

DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

def shared var nufile as log no-undo.
def new shared var qty as int NO-UNDO.

{ce/print4.i "new shared" "new shared"}
{ce/print42.i "new shared"}
   
def workfile work-ord
   field cust-no like job-hdr.cust-no
   field ord-no  like job-hdr.ord-no.

def var type-chk as char init "C,D,F,G,I,L,M,P,R,T,V,W" no-undo.
def var over-pct as dec no-undo.
def var v-add-overrn as log no-undo.
def var v-qty as int no-undo.

def var v-set-hdr like oe-ordl.i-no.
def var v-item-no    like itemfg.i-no.
def var v-part-qty   as dec.

DEF VAR ll-one-part AS LOG NO-UNDO.

DEF BUFFER x-eb FOR eb.
DEF BUFFER x-job-hdr FOR job-hdr.

{oe/oe-sysct1.i NEW}
 
{ce/msfcalc.i}
    
{sys/inc/f16to32.i}

DO TRANSACTION:
   {sys/inc/graphic.i}
END.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "JOBCARDF"
    NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN lv-format-f = sys-ctrl.char-fld.

{sys/ref/fgoecost.i}
 
RUN oe/oe-sysct.p.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name eq "JOB QTY"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
    sys-ctrl.company = cocode
    sys-ctrl.name = "JOB QTY"
    sys-ctrl.descrip = "Create Job Quantity with overrun % from OE?"
    sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-add-overrn = sys-ctrl.log-fld.

find first sys-ctrl where sys-ctrl.company eq cocode
           and sys-ctrl.name eq "JOBPRINT" no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
    sys-ctrl.company = cocode
    sys-ctrl.name = "JOBPRINT"
    sys-ctrl.descrip = "Print Job Ticket From Order Automatically?"
    sys-ctrl.int-fld = 1
    sys-ctrl.char-fld = SESSION:PRINTER-NAME
    sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
ASSIGN v-print-job = sys-ctrl.log-fld
       v-copies = sys-ctrl.int-fld
       v-printer-name = sys-ctrl.char-fld.

find first jc-ctrl where jc-ctrl.company eq cocode no-lock no-error.
 
mainloop:
do transaction:
  find job where recid(job) eq rec-id.

  IF nufile THEN DO:
    if not program-name(2) begins "util/updstand" then
    find first job-mat
        where job-mat.company eq cocode
          and job-mat.all-flg eq yes
          and job-mat.job     eq job.job
          and job-mat.job-no  eq job.job-no
          and job-mat.job-no2 eq job.job-no2
          and job-mat.qty-all ne 0

        no-lock no-error.
      
    choice = not avail job-mat.      
    if not choice then do on endkey undo, leave:
      message "WARNING: Raw materials will be decommitted, continue?"
              view-as alert-box button yes-no update choice.
    end.
    
    if choice then run jc/jc-dall.p (recid(job)).
  
    else leave.

    for each job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job     eq job.job
          and job-hdr.job-no  eq job.job-no
          and job-hdr.job-no2 eq job.job-no2
          and job-hdr.qty     eq 0:
      delete job-hdr.
    end.

    for each work-ord:
      delete work-ord.
    end.

    for each job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job     eq job.job
          and job-hdr.job-no  eq job.job-no
          and job-hdr.job-no2 eq job.job-no2
          and job-hdr.ord-no  ne 0
        no-lock
        break by job-hdr.cust-no:
    
      if last-of(job-hdr.cust-no) then do:
        create work-ord.
        assign
         work-ord.cust-no = job-hdr.cust-no
         work-ord.ord-no  = job-hdr.ord-no.
      end.  
    end.
  END.

  find first xest
      where xest.company eq cocode
        and xest.loc     eq locode
        and xest.est-no  eq job.est-no
      use-index est-no no-error.
  if not avail xest then do:
    choice = no.
    message "There is no estimate to build from for this job.  "
           "Would you like to create one?  " 
           view-as alert-box button yes-no update choice .
    if not choice then undo, leave.
    if choice then repeat:
      message "Enter the Estimate Number you wish to build from:  "
             update v-est-job.             
      run util/rjust.p (input-output v-est-job, input 8).
      find first xest where xest.company = cocode
                        AND xest.loc = locode
                        AND xest.est-no eq v-est-job no-error.
      if not avail xest then
        /*run sys/msg/wemsg.p("Estimate Does Not Exist.  Please Re-enter",
                            "E R R O R!", 4).
         */
         message "Estimate Does Not Exist. Please Re-enter." view-as alert-box error.                   
      if avail xest then do:
         job.est-no = xest.est-no.
         job.rec_key = xest.rec_key.  /* for notes */
  

         for each job-hdr where job-hdr.company eq cocode
                            and job-hdr.job     eq job.job
                            and job-hdr.job-no  eq job.job-no
                            and job-hdr.job-no2 eq job.job-no2,
             each oe-ordl where oe-ordl.company eq job-hdr.company
                            and oe-ordl.job-no  eq job-hdr.job-no
                            and oe-ordl.job-no2 eq job-hdr.job-no2
                            and oe-ordl.ord-no  eq job-hdr.ord-no,
             each oe-ord of oe-ordl:
 
             assign job-hdr.est-no = job.est-no
                    oe-ordl.est-no = job.est-no
                    oe-ord.est-no  = job.est-no.
         end.
         leave.
      end. /* avail xest */     
    end.  /* choice */
  end. /* not avail xest */

  li = 0.

  if nufile then
  for each xeb where xeb.company = xest.company and
                     xeb.est-no = xest.est-no
      break by xeb.est-no
            BY xeb.form-no
            BY xeb.blank-no:

      IF lv-format-f EQ "CentBox" THEN
        ASSIGN
         li         = li + 1
         xeb.spc-no = TRIM(job.job-no)         + "-" +
                      STRING(job.job-no2,"99") + "-" +
                      STRING(li,"99").
    
      FIND FIRST oe-ordl
          WHERE oe-ordl.company    EQ cocode
            AND oe-ordl.job-no     EQ job.job-no
            AND oe-ordl.job-no2    EQ job.job-no2
            AND ((oe-ordl.form-no  EQ xeb.form-no AND
                  oe-ordl.blank-no EQ xeb.blank-no) OR
                 xeb.est-type EQ 2 OR xeb.est-type EQ 6)
          NO-LOCK NO-ERROR.

      IF xeb.stock-no EQ "" AND AVAIL oe-ordl THEN xeb.stock-no = oe-ordl.i-no.

      find first itemfg where itemfg.company eq cocode
                          and itemfg.i-no    eq xeb.stock-no
                          no-lock no-error.

      if not avail itemfg OR xeb.stock-no EQ "" then repeat:
         IF xeb.stock-no EQ "" THEN DO:
           message " FG Item Not defined, Please Enter "
                   update xeb.stock-no.
           if xeb.stock-no eq "" then next.
         END.
         find first itemfg
              {sys/look/itemfgrlW.i}
                and itemfg.i-no   eq xeb.stock-no
              no-lock no-error.
          if not avail itemfg then do on endkey undo, next:
            choice = yes.
            message "Item: " + TRIM(xeb.stock-no) + " doesn't exist, would you like to create it?"
                    view-as alert-box question button yes-no update choice.
            if choice then do:
              {jc/fgadd.i}
              leave.
            end.
          end.
      end.

      if xeb.form-no eq 0 then do:
         v-set-hdr = xeb.stock-no.      
         find first eb where eb.company = xeb.company
                         and eb.est-no   eq xeb.est-no
                         and eb.cust-no ne ""
                         no-lock no-error.
      end.
           
      if xest.est-type ge 3 and xest.est-type le 4 then
         find first job-hdr where job-hdr.company eq cocode
                              and job-hdr.job     eq job.job
                              and job-hdr.job-no  eq job.job-no
                              and job-hdr.job-no2 eq job.job-no2
                              and job-hdr.frm     eq xeb.form-no
                              and job-hdr.i-no    eq xeb.stock-no
                              no-error.          
      else if xest.est-type eq 1 or xest.est-type eq 5 or xeb.form-no eq 0 then
         find first job-hdr where job-hdr.company  eq cocode
                              and job-hdr.job      eq job.job
                              and job-hdr.job-no   eq job.job-no
                              and job-hdr.job-no2  eq job.job-no2 no-error.          
      else do:
         if last(xeb.est-no)                           and
            (xest.est-type eq 2 or xest.est-type eq 6) then do:
                  {fg/addset.i v-set-hdr}
         end.   
         next.
      end.

      if not avail job-hdr then do:
         x = 0.
         find last x-job-hdr use-index j-no no-lock no-error.
         if avail x-job-hdr then x = x-job-hdr.j-no.

         create job-hdr.
         assign job-hdr.company      = cocode
                job-hdr.loc          = locode
                job-hdr.j-no         = x + 1
                job-hdr.e-num        = xest.e-num
                job-hdr.est-no       = xest.est-no
                job-hdr.i-no         = xeb.stock-no
                job-hdr.job-no       = job.job-no
                job-hdr.job-no2      = job.job-no2
                job-hdr.cust-no      = if xeb.form-no eq 0 and avail eb then eb.cust-no
                                       else xeb.cust-no
                job-hdr.frm          = xeb.form-no
                job-hdr.job          = job.job
                job-hdr.start-date   = job.start-date                           
                job-hdr.due-date     = job.due-date.
       
         find first work-ord where work-ord.cust-no eq job-hdr.cust-no no-error.
         if avail work-ord then job-hdr.ord-no = work-ord.ord-no.
      
         {util/mkjobkey.i}
      end.

      if job-hdr.frm eq 0 then job-hdr.frm = 1.

      job-hdr.blank-no = if xeb.form-no eq 0 then 1 else xeb.blank-no.
      RUN update-itemfg (ROWID(job-hdr), -1).

      ASSIGN
       v-qty      = 0
       v-hold-qty = job-hdr.qty.

      if xest.est-type lt 3 or xest.est-type gt 4 then
         assign  v-qty       = xest.est-qty[1]
                 job-hdr.qty = xest.est-qty[1].
      else for each eb where eb.company = xest.company 
                         and eb.est-no eq xest.est-no
                         and eb.stock-no eq xeb.stock-no no-lock:
        
           if eb.form-no  eq xeb.form-no and eb.blank-no eq xeb.blank-no then job-hdr.qty = eb.bl-qty.         
           v-qty = v-qty + eb.bl-qty.
      end.
    
      if job-hdr.qty eq 0 then job-hdr.qty = 1.
      if v-qty       eq 0 then v-qty       = 1.

      over-pct = 0.

      find first oe-ordl where oe-ordl.company  eq cocode
                           and ((oe-ordl.ord-no eq job-hdr.ord-no AND job-hdr.ord-no NE 0) OR
                                job-hdr.ord-no  EQ 0)
                           and oe-ordl.job-no   eq job-hdr.job-no
                           and oe-ordl.job-no2  eq job-hdr.job-no2
                           and oe-ordl.i-no     eq job-hdr.i-no
                           and oe-ordl.est-no   eq job-hdr.est-no  no-error.
      if avail oe-ordl then
         assign job-hdr.qty      = oe-ordl.qty * (job-hdr.qty / v-qty)
                job-hdr.ord-no   = oe-ordl.ord-no
                job-hdr.due-date = oe-ordl.req-date
                over-pct         = oe-ordl.over-pct.
      ELSE
      IF v-hold-qty NE 0 THEN job-hdr.qty = v-hold-qty.

      /*over-pct = 0.
      if job-hdr.ord-no ne 0 then do:
         find first oe-ord  where oe-ord.company eq cocode
                              and oe-ord.ord-no  eq job-hdr.ord-no
                              no-lock no-error.
         if avail oe-ord then over-pct = oe-ord.over-pct.
      end.*/
      
      if v-add-overrn then job-hdr.qty = job-hdr.qty + (job-hdr.qty * (over-pct * .01)).

      RUN update-itemfg (ROWID(job-hdr), 1).

      FOR EACH x-job-hdr
          WHERE x-job-hdr.company EQ job-hdr.company
            AND x-job-hdr.job     EQ job-hdr.job
            AND x-job-hdr.job-no  EQ job-hdr.job-no
            AND x-job-hdr.job-no2 EQ job-hdr.job-no2
            AND x-job-hdr.frm     EQ job-hdr.frm
            AND x-job-hdr.i-no    EQ job-hdr.i-no
            AND ROWID(x-job-hdr)  NE ROWID(job-hdr):

        RUN update-itemfg (ROWID(x-job-hdr), -1).

        DELETE x-job-hdr.
      END.
  END.  /* for each xeb */

  FOR EACH job-hdr
      WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
        AND NOT CAN-FIND(FIRST xeb
                         WHERE xeb.company      EQ job-hdr.company
                           AND xeb.est-no       EQ job-hdr.est-no
                           AND xeb.stock-no     EQ job-hdr.i-no
                           AND ((xeb.form-no    EQ 0 AND
                                 xeb.blank-no   EQ 0 AND
                                 (xest.est-type EQ 2 OR xest.est-type EQ 6)) OR
                                (xeb.form-no    EQ job-hdr.frm AND
                                 xeb.blank-no   EQ job-hdr.blank-no AND
                                 xest.est-type  NE 2 AND xest.est-type NE 6))):

    RUN update-itemfg (ROWID(job-hdr), -1).

    DELETE job-hdr.
  END.

  assign
   job.est-no  = xest.est-no
   job.rec_key = xest.rec_key.
   job.stat    = "R".

  find job where recid(job) eq rec-id no-lock.
  
  run jc/calc-est.p (recid(job)).
 
   find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "SCHEDULE" no-lock no-error.
   if not avail sys-ctrl then do:
     create sys-ctrl.
     ASSIGN sys-ctrl.company  = cocode
            sys-ctrl.name     = "SCHEDULE"
            sys-ctrl.char-fld = "None"
            sys-ctrl.descrip  = "Update Due date and Promise date for Schedule?".
     MESSAGE sys-ctrl.descrip
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE sys-ctrl.log-fld.                             
   END.
   IF nufile THEN run jc/startdat.p (ROWID(job)).
   
   find job where recid(job) eq rec-id exclusive-lock.
   /* task 09130412 YSK 09/14/04  */
   IF nufile AND AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld
   THEN job.start-date = ?.

  assign
   job.std-fix-cost = 0
   job.std-lab-cost = 0
   job.std-mat-cost = 0
   job.std-var-cost = 0.

  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
      exclusive-lock
      break by job-hdr.i-no:

    job-hdr.ftick-prnt = no.

    if job-hdr.sq-in eq 0 then do:
      if xest.est-type eq 2 or xest.est-type eq 6 then job-hdr.sq-in = 100.00.
      else do:
        find first xjob
            where xjob.form-no  eq job-hdr.frm
              and xjob.blank-no eq job-hdr.blank-no
              and xjob.stock-no eq job-hdr.i-no
            no-error.
        if avail xjob then
          assign
           job-hdr.blank-no = xjob.blank-no
           job-hdr.frm      = xjob.form-no
           job-hdr.sq-in    = xjob.pct * 100.
      end.
    end.

    assign
     job-hdr.lock         = yes
     job-hdr.std-lab-cost = 0
     job-hdr.std-mat-cost = 0
     job-hdr.std-var-cost = 0
     job-hdr.std-fix-cost = 0.

    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-hdr.job,"999999999"):
      DELETE reftable.
    END.

    FOR EACH xjob
        WHERE (xjob.form-no  EQ job-hdr.frm AND
               xjob.blank-no EQ job-hdr.blank-no AND
               xjob.stock-no EQ job-hdr.i-no)
           OR xest.est-type EQ 2
           OR xest.est-type EQ 6,

        FIRST eb
        WHERE eb.company  EQ job.company
          AND eb.est-no   EQ job.est-no
          AND eb.form-no  EQ xjob.form-no
          AND eb.blank-no EQ xjob.blank-no
        NO-LOCK:
           
      ASSIGN
       job-hdr.std-lab-cost = job-hdr.std-lab-cost + xjob.lab
       job-hdr.std-mat-cost = job-hdr.std-mat-cost + xjob.mat
       job-hdr.std-var-cost = job-hdr.std-var-cost + xjob.voh
       job-hdr.std-fix-cost = job-hdr.std-fix-cost + xjob.foh.

      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job-hdr.job,"999999999")
            AND reftable.code2    EQ eb.stock-no
            AND reftable.val[12]  EQ eb.form-no
            AND reftable.val[13]  EQ eb.blank-no
          NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
        CREATE reftable.
        ASSIGN
         reftable.reftable = "jc/jc-calc.p"
         reftable.company  = job.company
         reftable.loc      = ""
         reftable.code     = STRING(job.job,"999999999")
         reftable.code2    = eb.stock-no
         reftable.val[12]  = eb.form-no
         reftable.val[13]  = eb.blank-no.
      END.

      ASSIGN
       v-yld-qty       = IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty) ELSE eb.yld-qty
       reftable.val[1] = reftable.val[1] + (xjob.lab / v-yld-qty)
       reftable.val[2] = reftable.val[2] + (xjob.mat / v-yld-qty)
       reftable.val[3] = reftable.val[3] + (xjob.voh / v-yld-qty)
       reftable.val[4] = reftable.val[4] + (xjob.foh / v-yld-qty)
       reftable.val[5] = reftable.val[1] + reftable.val[2] +
                         reftable.val[3] + reftable.val[4].

      FIND CURRENT reftable NO-LOCK.
    END.

    assign
     job-hdr.std-tot-cost = job-hdr.std-fix-cost + job-hdr.std-lab-cost +
                            job-hdr.std-mat-cost + job-hdr.std-var-cost
     job.std-fix-cost     = job.std-fix-cost     + job-hdr.std-fix-cost
     job.std-lab-cost     = job.std-lab-cost     + job-hdr.std-lab-cost
     job.std-mat-cost     = job.std-mat-cost     + job-hdr.std-mat-cost
     job.std-var-cost     = job.std-var-cost     + job-hdr.std-var-cost
     job.std-tot-cost     = job.std-fix-cost     + job.std-lab-cost +
                            job.std-mat-cost     + job.std-var-cost.

    for each eb
        where eb.company  eq job-hdr.company
          and eb.est-no   eq job-hdr.est-no
          and eb.form-no  eq job-hdr.frm
          and eb.blank-no ne 0
        no-lock
        by eb.blank-no desc:
        
      job-hdr.n-on = eb.num-up.
      if eb.blank-no eq job-hdr.blank-no then leave.
    end.
  end.
  
  run util/jobsqin2.p (recid(job)).

  IF nufile THEN DO:
    RUN jc/delkids.p (ROWID(job), NO).

    x = 0.

    for each brd:
      if brd.cost eq ? then brd.cost = 0.
      if brd.cost-m eq ? then brd.cost-m = 0.
      if brd.qty eq ? then brd.qty = 0.
      find first item where item.company eq cocode
                        and item.i-no    eq brd.i-no no-lock.
       v-up = 0.
       if index("1234BPR",item.mat-type) GT 0 then
          run sys/inc/numup.p (job.company,job.est-no, brd.form-no, output v-up).
       v-out = 1.
      find first ef where ef.company = job.company
                      and ef.est-no   eq job.est-no
                      and ef.form-no eq brd.form-no
                      no-lock no-error.
      if avail ef then v-out = (if ef.n-out   eq 0 then 1 else ef.n-out  ) *
                               (if ef.n-out-l eq 0 then 1 else ef.n-out-l) *
                               (if ef.n-out-d eq 0 then 1 else ef.n-out-d).
      z = lookup(item.mat-type, type-chk).
      x = x + 1.

      CREATE job-mat.

      assign job-mat.company  = cocode
             job-mat.job      = job.job
             job-mat.job-no   = job.job-no
             job-mat.job-no2  = job.job-no2
             job-mat.line     = x
             job-mat.cost-m   = brd.cost-m
             job-mat.basis-w  = brd.basis-w
             job-mat.blank-no = brd.blank-no
             job-mat.frm      = brd.form-no
             job-mat.i-no     = brd.i-no
             job-mat.rm-i-no  = brd.i-no
             job-mat.len      = brd.len
             job-mat.wid      = brd.wid
             job-mat.n-up     = v-up * v-out
             job-mat.qty      = brd.qty
             job-mat.qty-mr   = brd.qty-mr
             job-mat.qty-wst  = brd.qty-wst
             job-mat.qty-uom  = brd.qty-uom
             job-mat.qty-all  = brd.qty
             job-mat.sc-uom   = brd.sc-uom
             job-mat.std-cost = brd.cost.
      if z gt 0 then job-mat.post = jc-ctrl.post[z].      

    end.

    for each op:
      run sys/inc/numup.p (job.company,job.est-no,op.form-no,output v-up).
      v-out = 1.
      find first ef where ef.company = job.company
                      and ef.est-no   eq job.est-no
                      and ef.form-no eq op.form-no
                      no-lock no-error.
      if avail ef then v-out = (if ef.n-out   eq 0 then 1 else ef.n-out  ) *
                               (if ef.n-out-l eq 0 then 1 else ef.n-out-l) *
                               (if ef.n-out-d eq 0 then 1 else ef.n-out-d).

      find first job-hdr where job-hdr.company  eq cocode
                           and job-hdr.job      eq job.job
                           and job-hdr.job-no   eq job.job-no
                           and job-hdr.job-no2  eq job.job-no2
                           and job-hdr.frm      eq op.form-no
                           and job-hdr.blank-no eq op.blank-no no-lock no-error.

      find first mach {sys/ref/machW.i}
                      and mach.m-code eq op.m-code no-lock.

      v-on-f = 1.

      FIND FIRST est-op
          WHERE est-op.company EQ cocode
            AND est-op.est-no  EQ job.est-no
            AND est-op.line    EQ op.line
          NO-LOCK NO-ERROR.

      IF NOT AVAIL est-op THEN
      FIND FIRST est-op
          WHERE est-op.company EQ cocode
            AND est-op.est-no  EQ job.est-no
            AND est-op.line    EQ op.line + 500
          NO-LOCK NO-ERROR.

      IF AVAIL est-op THEN
        run sys/inc/numout.p (recid(est-op), output v-on-f).

      CREATE job-mch.

      assign job-mch.company  = cocode
             job-mch.blank-no = if (mach.p-type eq "B" or
                                   (xest.est-type eq 3 and op.dept eq "PR"))
                                then op.blank-no else 0
             job-mch.job      = job.job
             job-mch.job-no   = job.job-no
             job-mch.job-no2  = job.job-no2
             job-mch.dept     = op.dept
             job-mch.frm      = op.form-no
             job-mch.i-no     = if avail job-hdr then job-hdr.i-no else op.i-no
             job-mch.i-name   = op.i-name
             job-mch.line     = op.line
             job-mch.m-code   = op.m-code
             job-mch.mr-fixoh = op.mr-fixoh
             job-mch.mr-hr    = op.mr-hr
             job-mch.mr-rate  = op.mr-rate
             job-mch.mr-varoh = op.mr-varoh
             job-mch.mr-waste = op.mr-waste
             job-mch.pass     = op.pass
             job-mch.run-hr   = op.run-hr
             job-mch.speed    = op.speed
             job-mch.wst-prct = op.wst-prct
             job-mch.start-date = job.start-date
             job-mch.run-qty  = op.run-qty
             job-mch.n-out    = IF AVAIL est-op THEN est-op.n-out ELSE 1
             job-mch.n-on     = IF INDEX("AB",mach.p-type) GT 0 THEN 1 ELSE
                                (v-up * v-out / v-on-f).

             /*task 09310402  */
      /*IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld
      THEN */ ASSIGN job-mch.start-date-su = job.start-date
                  job-mch.end-date = job.start-date
                  job-mch.end-date-su = job.start-date.
    end.
  
    z = 0.
    for each job-mch where job-mch.company eq job.company
                       and job-mch.job     eq job.job
                       and job-mch.job-no  eq job.job-no
                       and job-mch.job-no2 eq job.job-no2 exclusive
                       by job-mch.frm  by job-mch.line:
      z = z + 1.  
      job-mch.line = z.
    end.

    for each xprep:
      create job-prep.
      assign job-prep.company  = cocode
             job-prep.job      = job.job
             job-prep.job-no   = job.job-no
             job-prep.job-no2  = job.job-no2
             job-prep.frm      = xprep.frm
             job-prep.blank-no = xprep.blank-no
             job-prep.code     = xprep.code
             job-prep.ml       = xprep.ml
             job-prep.std-cost = xprep.std-cost
             job-prep.cost-m   = xprep.cost-m
             job-prep.opn      = yes
             job-prep.qty      = xprep.qty
             job-prep.simon    = xprep.simon.
  
      find first prep where prep.company eq cocode
                        and prep.loc     eq locode
                        and prep.code    eq xprep.code
                        no-lock no-error.
      job-prep.sc-uom = if avail prep then prep.uom else "EA".
    end.
  end.
/*run jc/kiwiexp2.p (recid(job)).*/  

end. /* trans */

/* print job ticket auto if JOBPRINT = yes task# 09180401 YSK */
IF v-print-job THEN RUN print-ticket.

RETURN.

PROCEDURE update-itemfg:
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-factor AS DEC NO-UNDO.

  DEF BUFFER b-job-hdr FOR job-hdr.


  FIND b-job-hdr WHERE ROWID(b-job-hdr) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-job-hdr THEN DO:
    FIND FIRST itemfg
        {sys/look/itemfgrlW.i}
          AND itemfg.i-no EQ b-job-hdr.i-no
        NO-ERROR.

    itemfg.q-ono = itemfg.q-ono + (b-job-hdr.qty * ip-factor).
         
    RUN fg/comp-upd.p (RECID(itemfg), b-job-hdr.qty * ip-factor,
                       "q-ono", b-job-hdr.est-no).
  END.
END PROCEDURE.

PROCEDURE print-ticket:
   DEF VAR lv-format AS cha NO-UNDO.
   DEF VAR lines-per-page AS INT NO-UNDO.
   DEF VAR lv-industry AS cha NO-UNDO.
   DEF VAR init-dir AS cha NO-UNDO.
   DEF VAR tmp-dir AS cha NO-UNDO.
   DEF VAR lv-prt-name AS cha NO-UNDO.
   DEF VAR list-name AS cha NO-UNDO.
   DEF VAR lv-font-no AS INT INIT 11 NO-UNDO.
   DEF VAR lv-ornt AS cha INIT "L" NO-UNDO.
   DEF VAR lv-copy AS INT NO-UNDO.

   find first xest where xest.company eq cocode
                     and xest.loc     eq locode
                     and xest.est-no  eq job.est-no use-index est-no no-error.
   IF AVAIL xest AND xest.est-type < 5 THEN DO:
      {sys/inc/jobcard.i "F"}
      lv-format = sys-ctrl.char-fld.
      lv-industry = "Fold".
      IF lookup(lv-format-f,"Interpac,FibreFC,Dayton,Livngstn,CentBox") > 0 THEN lines-per-page = 55.
   END.  
   ELSE IF AVAIL xest AND xest.est-type <= 6  THEN DO:
      {sys/inc/jobcard.i "C"}
      lv-industry = "Corr".
      lv-format = sys-ctrl.char-fld.
  END.
  /*
  FIND FIRST user-print WHERE user-print.company    EQ cocode  
                          AND user-print.program-id EQ "JOBPRINT"
                          AND user-print.batch  = "" NO-LOCK NO-ERROR.
  IF NOT AVAIL user-print THEN DO TRANSACTION:
     RUN custom/d-printB.w (OUTPUT lv-prt-name,OUTPUT lv-copy).
     CREATE user-print.
     ASSIGN user-print.company = cocode
            user-print.program-id = "JOBPRINT"
            user-print.BATCH = ""
            user-print.PRINTER-NAME = lv-prt-name
            .
  END.
  IF AVAIL user-print THEN lv-prt-name = user-print.PRINTER-NAME.
  */
  lv-prt-name = v-printer-name.

  {sys/inc/print1.i}
  {sys/inc/outprint.i value(lines-per-page) }

  IF lv-industry EQ "Corr" OR 
     LOOKUP(lv-format,"Interpac,FibreFC,Dayton,Livngstn,CentBox,Keystone") GT 0 THEN DO:
     PUT "<PRINTER" lv-prt-name "><COPIES>" v-copies.
  END.
  /* generate ticket */   
  IF lv-format = "FibreFC" THEN do:  
    PUT UNFORMATTED "<FORMAT=11X17><OLANDSCAPE><P10>" .
    RUN cerep/jobfibre.p (lv-format,0). /* gdm - 07130906 */
  END.
  ELSE IF lv-format = "ARTIOS" THEN do:   /* For Fibre */    
       PUT UNFORMATTED "<OLANDSCAPE><P10>" .
       RUN cecrep/jobfibre.p (lv-format). 
  END.

  /* print ticket*/
   IF lv-industry EQ "Corr" THEN DO:
       FILE-INFO:FILE-NAME = list-name.
       RUN printfile (FILE-INFO:FILE-NAME).
   END.
   ELSE DO:
      IF lookup(lv-format-f,"Interpac,FibreFC,Dayton,Livngstn,CentBox,Keystone") > 0
      THEN DO:
         FILE-INFO:FILE-NAME = list-name.
         RUN printfile (FILE-INFO:FILE-NAME).   
      END.
      ELSE RUN custom/prntproc.p (list-name, lv-font-no, lv-ornt).
   END.

END.
