
/*------------------------------------------------------------------------
    File        : MatJob.p
    Purpose     : MatJob

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{MatInfo.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine as CHAR no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMatInfo .

DEF VAR prmComp AS CHAR NO-UNDO.

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
        FOR EACH job-mat OF job NO-LOCK:
            FIND item WHERE item.company = job.company AND item.i-no = job-mat.i-no NO-LOCK NO-ERROR.
            FIND FIRST po-all WHERE po-all.company = job.company 
                                AND po-all.job     = job-mat.job 
                                AND po-all.form-no = job-mat.frm 
                                AND po-all.i-no    = job-mat.rm-i-no
                                 use-index job-idx no-lock no-error.

            IF AVAILABLE po-all THEN
                FIND po-ordl WHERE po-ordl.company = job.company 
                               AND po-ordl.po-no   = po-all.po-no
                               AND po-ordl.line    = po-all.line
                                NO-LOCK NO-ERROR.
                CREATE mat.
                ASSIGN
                    mat.form-no  = job-mat.frm
                    mat.blank-no = job-mat.blank-no
                    mat.rm-i-no  = job-mat.rm-i-no.
                
                IF AVAIL ITEM THEN
                    ASSIGN
                    mat.i-name  = item.i-name
                    mat.q-onh   = item.q-onh
                    mat.q-comm  = item.q-comm
                    mat.q-avail = (item.q-onh - item.q-comm).
                IF AVAIL po-all THEN mat.qty-all = po-all.qty-all.
                IF AVAIL po-ordl THEN mat.due-date = po-ordl.due-date.
        END.
    END.
END.


