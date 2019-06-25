
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
{MaterialInt.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine as Character no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmAction as Character no-undo.
DEFINE INPUT PARAMETER vFormNo as Character no-undo.
DEFINE INPUT PARAMETER vBlankNo as Character no-undo.
DEFINE INPUT PARAMETER prmItem as Character no-undo.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMaterialInt .

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF vFormNo = "0" AND vBlankNo = "0" AND prmItem = "ALL" THEN DO:
    RETURN.
END.

FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = INT(prmOrderNum) AND oe-ordl.LINE = int(vLine) NO-LOCK,
    EACH job WHERE job.company = oe-ordl.company 
               AND job.job-no = oe-ordl.job-no 
               AND job.job-no2 = oe-ordl.job-no2  NO-LOCK,
    each mat-act WHERE mat-act.company = job.company 
                       AND mat-act.job-no  = job.job-no 
                       AND mat-act.job-no2 = job.job-no2 
                       AND mat-act.s-num   = int(vFormNo )
                       AND mat-act.b-num   = int(vBlankNo )
                       AND mat-act.i-no    = prmItem
                        NO-LOCK:
        
        create tt_mat_tran.
        assign
            tt_mat_tran.job-no     = mat-act.job-no
            tt_mat_tran.job-no2    = mat-act.job-no2
            tt_mat_tran.tran-date  = mat-act.mat-date
            tt_mat_tran.sheet-no   = mat-act.s-num
            tt_mat_tran.blank-no   = mat-act.b-num
            tt_mat_tran.i-no       = mat-act.i-no
            tt_mat_tran.qty-posted = mat-act.qty
            tt_mat_tran.ROWID      = ROWID(mat-act)
            tt_mat_tran.tag        = mat-act.tag
            tt_mat_tran.ext-cost   = mat-act.ext-cost
            tt_mat_tran.qty-uom    = mat-act.qty-uom.
END.  /*   FOR EACH oe-ordl */
