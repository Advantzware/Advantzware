
/*------------------------------------------------------------------------
    File        : jc/GetFeedQty.p
    Purpose     : 

    Syntax      :

    Description : the file will return the qty in msf, ton and LF per each area value

    Author(s)   : Sewa Singh
    Created     : Thu Dec 3 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriMatAct AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER opdQtyInMSF AS DECIMAL NO-UNDO.

DEFINE VARIABLE dLen AS DECIMAL NO-UNDO.
DEFINE VARIABLE dWid AS DECIMAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND FIRST mch-act NO-LOCK 
    WHERE ROWID(mch-act) EQ ipriMatAct 
    NO-ERROR.

IF NOT AVAILABLE mch-act THEN RETURN.
IF mch-act.qty EQ ? THEN RETURN.
    
FIND FIRST mach NO-LOCK 
    WHERE mach.company EQ mch-act.company
    AND mach.m-code EQ mch-act.m-code
    NO-ERROR.  
          
FIND FIRST job NO-LOCK
    WHERE job.company EQ mch-act.company
    AND job.job EQ mch-act.job
    AND job.job-no EQ mch-act.job-no 
    AND job.job-no2 EQ mch-act.job-no2 
    NO-ERROR.
     
IF AVAILABLE job THEN
DO:
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ job.company 
        AND ef.est-no EQ job.est-no 
        AND ef.form-no EQ mch-act.frm NO-ERROR .
    IF AVAILABLE ef THEN DO:  
        IF AVAILABLE mach AND LOOKUP(mach.dept[1],"RC,RS") GT 0 OR LOOKUP(mach.dept[2],"RC,RS") GT 0 OR LOOKUP(mach.dept[3],"RC,RS") GT 0 OR LOOKUP(mach.dept[4],"RC,RS") GT 0 THEN
            ASSIGN
                dLen = ef.gsh-len
                dWid = ef.gsh-wid.
        ELSE IF AVAILABLE mach AND LOOKUP(mach.p-type,"B,A,P") GT 0 THEN DO:
            FIND FIRST eb NO-LOCK 
                WHERE eb.company EQ mch-act.company
                AND eb.est-no EQ job.est-no
                AND eb.form-no EQ mch-act.frm
                AND (eb.blank-no EQ mch-act.blank-no OR mch-act.blank-no EQ 0)
                NO-ERROR.
            IF AVAILABLE eb THEN 
                ASSIGN 
                    dLen = eb.t-len
                    dWid = eb.t-wid
                    .
        END.
        ELSE 
            ASSIGN
                dLen = ef.nsh-len
                dWid = ef.nsh-wid.
    END.
 
    opdQtyInMSF =  (dLen * dWid / 144) * (mch-act.qty / 1000) .       
    IF opdQtyInMSF EQ ? THEN opdQtyInMSF = 0.
                 
END.
          
  
