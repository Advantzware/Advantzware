
/*------------------------------------------------------------------------
    File        : Material.p
    Purpose     : Machine Hour

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{Material.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine as Character no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMaterial .

DEF NEW shared var cocode as CHAR no-undo.

DEFINE var v-pct as dec init 1.00 no-undo.
def var v-cost as dec no-undo.
def var v-qty as dec no-undo.
def var v-std-tot like mat.cst-std no-undo.
def var v-act-tot like mat.cst-act no-undo.
def var v-var-tot like mat.cst-var no-undo.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN 
    cocode = prmComp .

FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = INT(prmOrderNum) AND oe-ordl.LINE = int(vLine) no-lock: 
    FOR EACH job WHERE job.company = oe-ordl.company 
                   AND job.job-no = oe-ordl.job-no 
                   AND job.job-no2 = oe-ordl.job-no2  NO-LOCK:
        FOR EACH job-mat NO-LOCK WHERE job-mat.company EQ job.company
                                    AND job-mat.job     EQ job.job
                                    AND job-mat.job-no  EQ job.job-no
                                    AND job-mat.job-no2 EQ job.job-no2:
            find first ITEM where item.company eq job-mat.company
                              and item.i-no    eq job-mat.rm-i-no
                               use-index i-no no-lock.
            v-pct = 1.
            create mat.
            assign
                mat.form-no  = job-mat.frm
                mat.blank-no = job-mat.blank-no
                mat.rm-i-no  = job-mat.rm-i-no
                mat.basis-w  = job-mat.basis-w
                mat.len      = job-mat.len
                mat.wid      = job-mat.wid
                mat.cst-uom  = job-mat.sc-uom.

            
            IF job-mat.j-no EQ 0 THEN DO:
                assign
                    mat.qty-std = job-mat.qty * v-pct
                    v-cost      = job-mat.std-cost.
                
                IF job-mat.sc-uom NE job-mat.qty-uom THEN
                    if item.r-wid eq 0 then do:
                        run sys/ref/convcuom.p(job-mat.sc-uom,
                                               job-mat.qty-uom,
                                               (if job-mat.basis-w  ne 0 then job-mat.basis-w
                                                   else item.basis-w),
                                               (if job-mat.len      ne 0 then job-mat.len
                                                   else item.s-len),
                                               (if job-mat.wid      ne 0 then job-mat.wid
                                                   else item.s-wid),
                                               item.s-dep, 
                                               job-mat.std-cost,
                                               output v-cost).
                    end. /*if item.r-wid*/
                    else do:
                        run sys/ref/convcuom.p(job-mat.sc-uom,
                                               job-mat.qty-uom,
                                               (if job-mat.basis-w  ne 0 then job-mat.basis-w
                                                   else item.basis-w),
                                               job-mat.len,
                                               (if job-mat.wid      ne 0 then job-mat.wid
                                                   else item.r-wid),
                                               item.s-dep, 
                                               job-mat.std-cost,
                                               output v-cost).
                    end. /* else do*/
                    mat.cst-std = job-mat.qty * v-cost * v-pct.
            END. /*IF job-mat.j-no EQ 0*/
        end. /*FOR EACH job-mat NO-LOCK*/
        FOR EACH mat-act NO-LOCK WHERE mat-act.company EQ job.company
                                   AND mat-act.job     EQ job.job
                                   AND mat-act.job-no  EQ job.job-no
                                   AND mat-act.job-no2 EQ job.job-no2:
        
            find first ITEM where item.company eq mat-act.company
                              and item.i-no    eq mat-act.rm-i-no
                              use-index i-no no-lock.
            v-pct = 1.
            find first mat where mat.form-no  eq mat-act.s-num
                             and mat.blank-no eq mat-act.b-num
                             and mat.rm-i-no  eq mat-act.rm-i-no
                              no-error.
            if not avail mat then do:
                create mat.
                assign
                    mat.form-no  = mat-act.s-num
                    mat.blank-no = mat-act.b-num
                    mat.rm-i-no  = mat-act.rm-i-no
                    mat.qty-std  = 0
                    mat.cst-std  = 0
                    v-qty        = 0
                    v-cost       = 0.
                for each rm-rcpth where rm-rcpth.company   eq mat-act.company
                                    and rm-rcpth.job-no    eq mat-act.job-no
                                    and rm-rcpth.job-no2   eq mat-act.job-no2
                                    and rm-rcpth.i-no      eq mat-act.i-no
                                    and rm-rcpth.rita-code eq "I"
                                     no-lock,
                    each rm-rdtlh
                    where rm-rdtlh.r-no eq rm-rcpth.r-no
                    no-lock:
                    assign
                        v-qty  = v-qty  + rm-rdtlh.qty
                        v-cost = v-cost + (rm-rdtlh.qty * rm-rdtlh.cost).
                end.
                v-cost = v-cost / v-qty.
            end.
            ELSE DO:
                v-cost = mat-act.cost.
                IF mat.cst-uom NE mat-act.qty-uom THEN
                    if item.r-wid eq 0 then do:
                        run sys/ref/convcuom.p(mat.cst-uom,mat-act.qty-uom,
                                               (if mat.basis-w   ne 0 then mat.basis-w
                                                   else item.basis-w),
                                               (if mat.len       ne 0 then mat.len
                                                   else item.s-len),
                                               (if mat.wid       ne 0 then mat.wid
                                                   else item.s-wid),
                                               item.s-dep, 
                                               mat-act.cost,
                                               output v-cost).
                    end.
                    else do:
                        run sys/ref/convcuom.p(mat.cst-uom,
                                               mat-act.qty-uom,
                                               (if mat.basis-w   ne 0 then mat.basis-w
                                                   else item.basis-w),
                                               mat.len,
                                               (if mat.wid       ne 0 then mat.wid
                                                   else item.r-wid),
                                               item.s-dep, 
                                               mat-act.cost,
                                               output v-cost).
                    end.
            END.
                
            if v-cost eq ? then mat.updatable = yes.
            
            mat.qty-act = mat.qty-act + (mat-act.qty * v-pct).
            IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
                mat.cst-act = mat.cst-act + (mat-act.qty * v-cost * v-pct).
            ELSE
                mat.cst-act = mat.cst-act + (mat-act.ext-cost * v-pct).
        end. /*FOR EACH mat-act NO-LOCK*/
        for each mat:
            assign
                mat.qty-var = mat.qty-std - mat.qty-act
                mat.cst-var = mat.cst-std - mat.cst-act
                v-std-tot   = v-std-tot + mat.cst-std
                v-act-tot   = v-act-tot + mat.cst-act
                v-var-tot   = v-var-tot + mat.cst-var.
        end.
        FIND FIRST mat
            WHERE mat.form-no  EQ 0
              AND mat.blank-no EQ 0
              AND mat.rm-i-no  EQ "ALL"
               NO-ERROR.
        IF AVAIL mat THEN DELETE mat.
        CREATE mat.
        ASSIGN
            mat.form-no  = 0
            mat.blank-no = 0
            mat.rm-i-no  = "ALL"
            mat.cst-std  = v-std-tot
            mat.cst-act  = v-act-tot
            mat.cst-var  = v-var-tot.
    END. /*FOR EACH job*/
END.  /*   FOR EACH oe-ordl */
