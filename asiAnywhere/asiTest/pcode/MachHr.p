
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
{MachHr.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine as Character no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMachHr .

DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE v-pct as dec init 1.00 no-undo.
DEFINE VARIABLE v-std-tot AS DEC NO-UNDO EXTENT 2.
DEFINE VARIABLE  v-act-tot AS DEC NO-UNDO EXTENT 2.
DEFINE VARIABLE v-var-tot AS DEC NO-UNDO EXTENT 2.
DEFINE VARIABLE x AS Integer.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = INT(prmOrderNum) AND oe-ordl.LINE = int(vLine) no-lock: 
    FOR EACH job WHERE job.company = oe-ordl.company 
                   AND job.job-no = oe-ordl.job-no 
                   AND job.job-no2 = oe-ordl.job-no2  NO-LOCK:
        for each job-mch where job-mch.company = job.company and job-mch.job = job.job no-lock:
            create x-mch.
            assign
                x-mch.form-no  = job-mch.frm
                x-mch.line     = job-mch.line
                x-mch.blank-no = job-mch.blank-no
                x-mch.m-code   = job-mch.m-code
                x-mch.i-no     = job-mch.i-no
                x-mch.dept     = job-mch.dept
                x-mch.est-speed = job-mch.speed.
            IF job-mch.j-no EQ 0 THEN
                ASSIGN
                x-mch.run-hr = job-mch.run-hr
                x-mch.mr-hr  = job-mch.mr-hr.
        end. /*for each job-mch*/
        for each mch-act where mch-act.company = job.company AND mch-act.job = job.job no-lock:
            v-pct = 1.
            find first x-mch where x-mch.form-no  = mch-act.frm 
                               AND x-mch.blank-no = mch-act.blank-no 
                               AND x-mch.m-code   = mch-act.m-code 
                                 no-error.
            if not available x-mch THEN do:
                create x-mch.
                assign 
                    x-mch.form-no  = mch-act.frm
                    x-mch.line     = x
                    x-mch.blank-no = mch-act.blank-no
                    x-mch.m-code   = mch-act.m-code
                    x-mch.i-no     = mch-act.i-no
                    x-mch.run-hr   = 0
                    x-mch.mr-hr    = 0.
                x = x - 1.
                find mach where mach.company = job.company 
                            AND mach.loc     = job.loc 
                            AND mach.m-code  = mch-act.m-code
                              no-lock no-error.
                if available mach then
                    x-mch.dept = mach.dept[1].
            end. /*if not available x-mch*/
            find job-code where job-code.code = mch-act.code
                              no-lock no-error.
            if not available job-code then next.
            if job-code.cat = "MR" then
                x-mch.mr-act = x-mch.mr-act + (mch-act.hours * v-pct).
            else if job-code.cat = "RUN" then
                assign  
                x-mch.run-act = x-mch.run-act + (mch-act.hours * v-pct)
                x-mch.act-qty = x-mch.act-qty +
                ((mch-act.qty + mch-act.waste) * v-pct).
        end. /*for each mch-act*/
        ASSIGN
            v-std-tot = 0
            v-act-tot = 0
            v-var-tot = 0.
        for each x-mch break by x-mch.line:
            create mch.
            ASSIGN
                mch.v-line = x-mch.line
                mch.v-form-no = x-mch.form-no
                mch.v-blank-no = x-mch.blank-no
                mch.v-m-code = x-mch.m-code
                mch.v-i-no   = x-mch.i-no
                mch.v-dept = x-mch.dept
                mch.v-run-hr = x-mch.run-hr
                mch.v-run-act = x-mch.run-act
                mch.v-run-var = x-mch.run-var
                mch.v-mr-hr = x-mch.mr-hr
                mch.v-mr-act = x-mch.mr-act
                mch.v-mr-var = x-mch.mr-var
                .
            
            if mch.v-run-act > 0 and x-mch.est-speed <> 0 then
                mch.v-run-hr = x-mch.act-qty / x-mch.est-speed.
            assign
                mch.v-run-var  = mch.v-run-act - mch.v-run-hr
                mch.v-mr-var   = mch.v-mr-act  - mch.v-mr-hr
                v-std-tot[1] = v-std-tot[1] + mch.v-run-hr
                v-act-tot[1] = v-act-tot[1] + mch.v-run-act
                v-var-tot[1] = v-var-tot[1] + mch.v-run-var
                v-std-tot[2] = v-std-tot[2] + mch.v-mr-hr
                v-act-tot[2] = v-act-tot[2] + mch.v-mr-act
                v-var-tot[2] = v-var-tot[2] + mch.v-mr-var.
        end. /*for each x-mch*/
        FIND FIRST mch WHERE mch.v-form-no  EQ 0
                         AND mch.v-blank-no EQ 0
                         AND mch.v-m-code   EQ "ALL"
                          NO-ERROR.
        IF AVAIL mch THEN DELETE mch.
        CREATE mch.
        ASSIGN
            mch.v-form-no  = 0
            mch.v-blank-no = 0
            mch.v-m-code   = "ALL"
            mch.v-run-hr   = v-std-tot[1]
            mch.v-run-act  = v-act-tot[1]
            mch.v-run-var  = v-var-tot[1]
            mch.v-mr-hr    = v-std-tot[2]
            mch.v-mr-act   = v-act-tot[2]
            mch.v-mr-var   = v-var-tot[2]
            .
    END. /*FOR EACH job*/
END.  /*   FOR EACH oe-ordl */
