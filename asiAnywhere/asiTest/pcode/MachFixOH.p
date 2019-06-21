
/*------------------------------------------------------------------------
    File        : MachHr.p
    Purpose     : Machine Hour

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{MachFixOH.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine as Character no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMachFixOH .

DEF VAR prmComp AS CHAR NO-UNDO.
def var rate like mach.run-rate no-undo.
def var mr-rate like mach.mr-rate no-undo.
def var v-pct as dec init 1.00 no-undo.
def var v-std-tot AS DEC NO-UNDO EXTENT 2.
def var v-act-tot AS DEC NO-UNDO EXTENT 2.
def var v-var-tot AS DEC NO-UNDO EXTENT 2.  
DEFINE VAR X AS INTEGER.
DEF VAR ct AS INT INIT 2 NO-UNDO.
DEF VAR hdr-id AS RECID INIT ? NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DO TRANSACTION:
   {jc\tspost.i}
END.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
/*IF prmJobNum = ? THEN ASSIGN prmJobNum = "".*/

FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = INT(prmOrderNum) AND oe-ordl.LINE = int(vLine) no-lock: 
    FOR EACH job WHERE job.company = oe-ordl.company 
                   AND job.job-no = oe-ordl.job-no 
                   AND job.job-no2 = oe-ordl.job-no2  NO-LOCK:
        ASSIGN X = 99.
        for each job-mch where job-mch.company = job.company 
                           AND job-mch.job = job.job
                            no-lock:
            find mach where mach.company = job.company 
                        AND mach.loc     = job.loc 
                        AND mach.m-code  = job-mch.m-code
                         no-lock no-error.
            if not available mach then next.
            {jc/jc-wipmr.i mach.run-crusiz mach.mr-crusiz "job-mch"}
                FIND job-hdr where job-hdr.company eq oe-ordl.company
                              and job-hdr.i-no    eq oe-ordl.i-no
                              and job-hdr.job-no  eq oe-ordl.job-no
                              use-index i-no NO-LOCK.
      
                if hdr-id = ? or (job-mch.frm = job-hdr.frm and
                                  (job-mch.blank-no = job-hdr.blank-no or job-mch.blank-no = 0)) 
                THEN do:
                    v-pct = 1.
                    if hdr-id <> ? and job-mch.blank-no = 0 then
                        v-pct = job-hdr.sq-in * .01.
                    create x-mch.
                    assign x-mch.form-no  = job-mch.frm
                        x-mch.line     = job-mch.line
                        x-mch.blank-no = job-mch.blank-no
                        x-mch.m-code   = job-mch.m-code
                        x-mch.i-no     = job-mch.i-no
                        x-mch.dept     = job-mch.dept
                        x-mch.wst-prct = job-mch.wst-prct
                        x-mch.est-speed = job-mch.speed
                        x-mch.std-hrs   = job-mch.run-hr.
                    IF job-mch.j-no EQ 0 THEN
                        IF ct LT 5 THEN
                            ASSIGN
                            x-mch.run-std = job-mch.run-hr * rate * v-pct
                            x-mch.mr-std  = job-mch.mr-hr * mr-rate * v-pct.
                        ELSE
                            IF ct EQ 5 THEN
                                ASSIGN
                                x-mch.run-std = job-mch.run-qty * v-pct
                                x-mch.mr-std  = job-mch.mr-waste * v-pct.
                            ELSE
                                ASSIGN
                                    x-mch.run-std = job-mch.run-qty * (job-mch.wst-prct * .01) * v-pct
                                    x-mch.mr-std  = job-mch.mr-waste * v-pct.
                end.
                else
                    next.
        end. /*for each job-mch*/
        for each mch-act where mch-act.company = job.company 
                           AND mch-act.job = job.job
                            no-lock:
            find mach where mach.company = job.company
                        AND mach.loc     = job.loc  
                        AND mach.m-code  = mch-act.m-code
                         no-lock no-error.
            if not available mach then next.
            find first x-mch where x-mch.form-no  = mch-act.frm 
                               AND x-mch.blank-no = mch-act.blank-no 
                               AND x-mch.m-code   = mch-act.m-code
                                no-error.
            if not available x-mch THEN do:
                FIND job-hdr where job-hdr.company eq oe-ordl.company
                              and job-hdr.i-no    eq oe-ordl.i-no
                              and job-hdr.job-no  eq oe-ordl.job-no
                              use-index i-no NO-LOCK.
                if hdr-id = ? or (mch-act.frm = job-hdr.frm and
                                  (mch-act.blank-no = job-hdr.blank-no or mch-act.blank-no = 0)) THEN do:
                    create x-mch.
                    assign x-mch.form-no  = mch-act.frm
                        x-mch.line     = x
                        x-mch.blank-no = mch-act.blank-no
                        x-mch.m-code   = mch-act.m-code
                        x-mch.i-no     = mch-act.i-no
                        x-mch.run-std  = 0
                        x-mch.mr-std   = 0
                        x-mch.dept = mach.dept[1].
                    x = x - 1.
                end.
                else
                    next.
            end. /*if not available x-mch*/
            {jc/jc-wipmr.i mch-act.crew mch-act.crew "mch-act"}
                v-pct = 1.
            if hdr-id <> ? and mch-act.blank-no = 0 then
                v-pct = job-hdr.sq-in * .01.
            find job-code where job-code.code = mch-act.CODE no-lock no-error.
            if not available job-code then next.
            if job-code.cat = "MR" then
                if ct < 5 then
                    x-mch.mr-act = x-mch.mr-act + ((mch-act.hours * mr-rate) * v-pct).
                else
                    x-mch.mr-act = x-mch.mr-act + ((mch-act.qty + mch-act.waste) * v-pct).
                    else if job-code.cat = "RUN" or job-code.cat = "DT" THEN do:
                        if ct < 5 then
                            x-mch.run-act = x-mch.run-act + ((mch-act.hours * rate) * v-pct).
                        else do:
                            if ct = 5 then
                                x-mch.run-act = x-mch.run-act + ((mch-act.qty + mch-act.waste) * v-pct).
                            else
                                x-mch.run-act = x-mch.run-act + (mch-act.waste * v-pct).
                         end.
                         x-mch.run-hrs = x-mch.run-hrs + (mch-act.hours * v-pct).
                         x-mch.act-qty = x-mch.act-qty + (mch-act.qty * v-pct).
                         x-mch.wst-qty = x-mch.wst-qty + (mch-act.waste * v-pct).
                    end. /*else if job-code.cat = "RUN" or*/
        end. /*for each mch-act*/
        ASSIGN
            v-std-tot = 0
            v-act-tot = 0
            v-var-tot = 0.
        FOR each x-mch break by x-mch.line:
            create FixOH.
            ASSIGN
                FixOH.f-line = x-mch.line
                FixOH.f-form-no = x-mch.form-no
                FixOH.f-blank-no = x-mch.blank-no
                FixOH.f-m-code = x-mch.m-code
                FixOH.f-i-no   = x-mch.i-no
                FixOH.f-dept = x-mch.dept
                FixOH.f-run-std = x-mch.run-std
                FixOH.f-run-act = x-mch.run-act
                FixOH.f-run-var = x-mch.run-var
                FixOH.f-mr-std = x-mch.mr-std
                FixOH.f-mr-act = x-mch.mr-act
                FixOH.f-mr-var = x-mch.mr-var
                .
             
            if FixOH.f-run-act > 0 and x-mch.est-speed <> 0 THEN do:
                if ct = 6 THEN FixOH.f-run-std = x-mch.act-qty * (x-mch.wst-prct * .01).
                if ct = 5 THEN FixOH.f-run-std = x-mch.run-hrs * x-mch.est-speed.
                if ct < 5 THEN FixOH.f-run-std = (x-mch.run-std / x-mch.std-hrs) *
                    ((x-mch.act-qty + x-mch.wst-qty) / x-mch.est-speed).
            end.
            assign
                FixOH.f-run-var  = FixOH.f-run-act - FixOH.f-run-std
                FixOH.f-mr-var   = FixOH.f-mr-act  - FixOH.f-mr-std
                v-std-tot[1] = v-std-tot[1] + FixOH.f-run-std
                v-act-tot[1] = v-act-tot[1] + FixOH.f-run-act
                v-var-tot[1] = v-var-tot[1] + FixOH.f-run-var
                v-std-tot[2] = v-std-tot[2] + FixOH.f-mr-std
                v-act-tot[2] = v-act-tot[2] + FixOH.f-mr-act
                v-var-tot[2] = v-var-tot[2] + FixOH.f-mr-var.
        end. /*FOR each x-mch break by x-FixOH.f-line:*/
        FIND FIRST FixOH WHERE FixOH.f-form-no  EQ 0
                         AND FixOH.f-blank-no EQ 0
                         AND FixOH.f-m-code   EQ "ALL"
                          NO-ERROR.
        IF AVAIL FixOH THEN DELETE FixOH.
        CREATE FixOH.
        ASSIGN
            FixOH.f-form-no  = 0
            FixOH.f-blank-no = 0
            FixOH.f-m-code   = "ALL"
            FixOH.f-run-std  = v-std-tot[1]
            FixOH.f-run-act  = v-act-tot[1]
            FixOH.f-run-var  = v-var-tot[1]
            FixOH.f-mr-std   = v-std-tot[2]
            FixOH.f-mr-act   = v-act-tot[2]
            FixOH.f-mr-var   = v-var-tot[2]
            .
        
        
    END. /*FOR EACH job*/
END.  /*   FOR EACH oe-ordl */
