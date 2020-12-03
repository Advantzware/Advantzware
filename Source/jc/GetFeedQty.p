
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
    
FIND FIRST mach NO-LOCK 
    WHERE mach.company EQ mch-act.company
    AND mach.m-code EQ mch-act.m-code
    NO-ERROR.  
          
FIND FIRST job-hdr NO-LOCK
     where job-hdr.company eq mch-act.company
     and job-hdr.job eq mch-act.job
     and job-hdr.job-no eq mch-act.job-no 
     and job-hdr.job-no2 eq mch-act.job-no2 NO-ERROR.
     
 IF AVAIL job-hdr THEN
 DO:
 FIND FIRST ef NO-LOCK
      WHERE ef.company EQ job-hdr.company 
      AND ef.est-no EQ job-hdr.est-no 
      AND ef.form-no EQ mch-act.frm NO-ERROR .
      
    IF AVAILABLE mach AND lookup(mach.dept[1],"RC,RS") GT 0 OR lookup(mach.dept[2],"RC,RS") GT 0 OR lookup(mach.dept[3],"RC,RS") GT 0 OR lookup(mach.dept[4],"RC,RS") GT 0 THEN
    ASSIGN
      dLen = ef.gsh-len
      dWid = ef.gsh-wid.
    ELSE
    ASSIGN
      dLen = ef.nsh-len
      dWid = ef.nsh-wid.
 
      opdQtyInMSF =  (dLen * dWid / 144) * (mch-act.qty / 1000) .       
             
 END.
          
  
