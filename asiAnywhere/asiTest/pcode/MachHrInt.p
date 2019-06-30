
/*------------------------------------------------------------------------
    File        : MchHrInt.p
    Purpose     : Machine Hour

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{MachHrInt.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine as Character no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmAction as Character no-undo.
DEFINE INPUT PARAMETER vFormNo as Character no-undo.
DEFINE INPUT PARAMETER vBlankNo as Character no-undo.
DEFINE INPUT PARAMETER prmItem as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMachHrInt .

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmItem = "ALL" THEN
   RETURN.

FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = INT(prmOrderNum) AND oe-ordl.LINE = int(vLine) no-lock: 
    FOR EACH job WHERE job.company = oe-ordl.company 
                   AND job.job-no = oe-ordl.job-no 
                   AND job.job-no2 = oe-ordl.job-no2  NO-LOCK:
        
        for each mch-act WHERE mch-act.company eq job.company 
                           AND mch-act.job-no  eq job.job-no 
                           AND mch-act.job-no2 EQ job.job-no2 
                           AND mch-act.frm     EQ int(vFormNo ) 
                           AND mch-act.blank-no EQ int(vBlankNo ) 
                           AND mch-act.m-code   EQ prmItem
                            no-lock:
            CREATE tt_mch_tran.
            ASSIGN tt_mch_tran.tran-type = "HRS"
                tt_mch_tran.tran-date = mch-act.op-date
                tt_mch_tran.job-no = mch-act.job-no
                tt_mch_tran.job-no2 = mch-act.job-no2
                tt_mch_tran.machine-code = mch-act.m-code
                tt_mch_tran.sheet-no = mch-act.frm
                tt_mch_tran.blank-no = mch-act.blank-no
                tt_mch_tran.i-no = mch-act.i-no
                tt_mch_tran.charge-code = mch-act.CODE
                tt_mch_tran.total-hours = mch-act.hours
                tt_mch_tran.qty-posted = mch-act.qty
                tt_mch_tran.qty-waste = mch-act.waste
                tt_mch_tran.COMPLETE = mch-act.COMPLETE
                tt_mch_tran.ROWID = ROWID(mch-act)
                tt_mch_tran.start-am = IF mch-act.START LT 43140 THEN "AM" ELSE "PM"
                tt_mch_tran.end-am = IF mch-act.stopp LT 43140 THEN "AM" ELSE "PM"
                tt_mch_tran.start-time = IF tt_mch_tran.start-am = "AM" THEN STRING(mch-act.START,"HH:MM")
                                            ELSE STRING(mch-act.START - 43200,"HH:MM")
                tt_mch_tran.end-time = IF tt_mch_tran.end-am = "AM" THEN STRING(mch-act.stopp,"HH:MM")
                                  ELSE STRING(mch-act.stopp - 43200,"HH:MM").
        END.
        for each misc-act WHERE misc-act.company eq job.company 
                            AND misc-act.job-no  eq job.job-no 
                            AND misc-act.job-no2 EQ job.job-no2 
                            AND misc-act.frm     EQ int(vFormNo ) 
                            AND misc-act.blank-no EQ int(vBlankNo ) 
                            AND misc-act.m-code   EQ prmItem
                             no-lock:
            CREATE tt_mch_tran.
            ASSIGN tt_mch_tran.tran-type = IF misc-act.ml THEN "MSC-M" ELSE "MSC-H"
                tt_mch_tran.tran-date = misc-act.misc-date
                tt_mch_tran.job-no = misc-act.job-no
                tt_mch_tran.job-no2 = misc-act.job-no2
                tt_mch_tran.machine-code = misc-act.m-code
                tt_mch_tran.sheet-no = misc-act.frm
                tt_mch_tran.blank-no = misc-act.blank-no
                tt_mch_tran.i-no = misc-act.i-no
                tt_mch_tran.qty-posted = misc-act.qty
                tt_mch_tran.ROWID = ROWID(tt_mch_tran).
        END.
    END. /*FOR EACH job*/
END.  /*   FOR EACH oe-ordl */
