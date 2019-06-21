
/*------------------------------------------------------------------------
    File        : MatQty.p
    Purpose     : Material Quantity

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{MatQty.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmJobNum as Character no-undo.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMatQty .

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-tspost-val   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE x AS Integer.
  DEFINE VARIABLE i AS Integer.

  DEFINE VARIABLE ct AS Integer.

  DEFINE VARIABLE rate like mach.run-rate no-undo.
  DEFINE VARIABLE mr-rate like mach.mr-rate no-undo.
  DEFINE VARIABLE v-pct as dec init 1.00 no-undo.
  DEFINE VARIABLE v-std-tot AS DEC NO-UNDO EXTENT 2.
  DEFINE VARIABLE v-act-tot AS DEC NO-UNDO EXTENT 2.
  DEFINE VARIABLE v-var-tot AS DEC NO-UNDO EXTENT 2.

IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust = ? THEN ASSIGN prmCust = "".
IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmJobNum = ? THEN ASSIGN prmJobNum = "".

 x = 99.

FOR EACH oe-ordl where oe-ordl.ord-no = int(prmOrderNum) no-lock: 
 FIND job where job.job = int(prmJobNum) NO-LOCK NO-ERROR.    
  for each job-mch where job-mch.company = job.company and
		       job-mch.job = integer(job.job)
		       no-lock:
   find mach where mach.company = job.company and
		   mach.loc     = job.loc and
		   mach.m-code  = job-mch.m-code
		   no-lock no-error.
   if not available mach then next.
    for each mch-act where mch-act.company = job.company and
		       mch-act.job = job.job
		       no-lock:
  Find Job-hdr no-lock no-error.
/**************************************************************************/
  /* {jc/jc-wipmr.i mach.run-crusiz mach.mr-crusiz "job-mch"}*/
rate = 1.
mr-rate = 1.
if ct = 1 THEN DO:
   IF v-tspost-val = "Actual" AND "{3}" = "mch-act" THEN do:
      rate = 0.
      mr-rate = 0.
      DO i = 1 TO rate:
        assign rate    = rate + mch-act.rate[i]
	           mr-rate = mr-rate + mch-act.rate[i].
      END.    
   END.
/*   ELSE assign rate    = (mach.run-rate / mach.run-crusiz) * {1}
               mr-rate = (mach.mr-rate / mach.mr-crusiz) * {2}.*/
END.
else if ct = 2 then
   assign rate    = mach.run-fixoh
	  mr-rate = mach.mr-fixoh.
else if ct = 3 then
   assign rate    = mach.run-varoh
	  mr-rate = mach.mr-varoh.
else if ct = 4 THEN DO:
   IF v-tspost-val = "Actual" AND "{3}" = "mch-act" THEN do:
      rate = 0.
      mr-rate = 0.
      DO i = 1 TO rate :
        assign rate    = rate + mch-act.rate[i]
	           mr-rate = mr-rate + mch-act.rate[i].
      END.    
      assign rate    = mach.run-varoh + mach.run-fixoh + rate
	         mr-rate = mach.mr-varoh  + mach.mr-fixoh  + rate.
   END.
   /*ELSE assign rate    = mach.run-varoh + mach.run-fixoh +
		                 ((mach.run-rate / mach.run-crusiz) * {1})
	           mr-rate = mach.mr-varoh  + mach.mr-fixoh  +
		                 ((mach.mr-rate / mach.mr-crusiz) * {2}).*/
END.  
if rate    = ? then rate    = 0.
if mr-rate = ? then mr-rate = 0.

/*********************************************************/
   if /*hdr-id = ? or*/ (job-mch.frm = job-hdr.frm and
     (job-mch.blank-no = job-hdr.blank-no or job-mch.blank-no = 0)) then
   do:
      v-pct = 1.
      if /*hdr-id <> ? and */  job-mch.blank-no = 0 then
	 v-pct = job-hdr.sq-in * .01.
      create ttMatQty.
      assign ttMatQty.form-no  = job-mch.frm
	     ttMatQty.line     = job-mch.line
	     ttMatQty.blank-no = job-mch.blank-no
	     ttMatQty.m-code   = job-mch.m-code
	     ttMatQty.i-no     = job-mch.i-no
	     ttMatQty.dept     = job-mch.dept
	     ttMatQty.wst-prct = job-mch.wst-prct
	     ttMatQty.est-speed = job-mch.speed
         ttMatQty.std-hrs   = job-mch.run-hr.
      IF job-mch.j-no EQ 0 THEN
        IF ct LT 5 THEN
          ASSIGN
	       ttMatQty.run-std = job-mch.run-hr * rate * v-pct
	       ttMatQty.mr-std  = job-mch.mr-hr * mr-rate * v-pct.
        ELSE
	    IF ct EQ 5 THEN
          ASSIGN
	       ttMatQty.run-std = job-mch.run-qty * v-pct
	       ttMatQty.mr-std  = job-mch.mr-waste * v-pct.
	    ELSE
	      ASSIGN
           ttMatQty.run-std = job-mch.run-qty * (job-mch.wst-prct * .01) * v-pct
	       ttMatQty.mr-std  = job-mch.mr-waste * v-pct.
   end.
   else
      next.
end.
End.
END.  /*   FOR EACH oe-ordl */
