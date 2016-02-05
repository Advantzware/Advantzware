/* --------------------------------------------- custom/kiwiexpt.p 07/00 fwk  */
/*                                                                            */
/* Export Job Data to Kiwi Scheduling Package                                 */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER selected-option AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER jobs-rowid AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER job-rowid AS ROWID NO-UNDO.

def var cocode as char no-undo.
def var locode as char no-undo.

DEFINE VARIABLE text-line AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-joint AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-adders AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.

{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

def var v-exc-d         as   char               format "x(40)"  no-undo.
def var v-use-numup     like sys-ctrl.log-fld                   no-undo.
def var v-not-done      as   log                                no-undo extent 2.
def var v-type          as   char                               no-undo.
def var v-plan          as   char               format "x(320)" no-undo.
def var v-char          as   char                               no-undo.
def var v-date          as   date                               no-undo.
def var v-out           as   char               format "x(40)"  no-undo.
def var v-frm           like job-mch.frm                        no-undo.
def var v-mach-lin      as   char                               no-undo extent 500.
def var hld-frm         like job-mch.frm.
def var v-dec           as   dec                                no-undo.
def var v-i-no          like itemfg.i-no                        no-undo.
def var v-i-name        like itemfg.i-name                      no-undo.
def var v-up            as   int                                no-undo.
def var v-score         as   char               format "x(64)"  no-undo.
def var v-cr-seq        like jobmach.sequence                   no-undo.
def var v-next-mch      like job-mch.line                       no-undo.
def var v-bill-count    as   int                                no-undo.
def var v-brdkey        as   char               format "x(30)"  no-undo.

find first sys-ctrl
    where sys-ctrl.company eq gcompany
      and sys-ctrl.name    eq "LOADPLAN"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = gcompany
   sys-ctrl.name    = "LOADPLAN"
   sys-ctrl.log-fld = no.
  message "Department codes to be excluded from the Planner."
   update sys-ctrl.char-fld.
end.

assign
 v-exc-d          = sys-ctrl.char-fld
 v-use-numup      = sys-ctrl.log-fld.

FIND optconfg WHERE optconfg.description = selected-option NO-LOCK.
IF optconfg.file_location = '' THEN
  RETURN.

find first dept where dept.company eq cocode
                  and dept.code eq "CR"
                no-lock no-error.
if avail dept then
  v-cr-seq = dept.fc.
  
FIND jobs WHERE ROWID(jobs) = jobs-rowid EXCLUSIVE-LOCK.
jobs.cadcam_status = 'Exported'.

OUTPUT TO VALUE(optconfg.file_location + '\' + 'kiwi.tmp').
/*
OUTPUT TO VALUE(optconfg.file_location + '\' + TRIM(jobs.job) + '.tmp').
*/

FIND job WHERE ROWID(job) = job-rowid 
           and job.stat                                   ne "P"
           and job.company                                eq gcompany
         use-index stat-idx NO-LOCK NO-ERROR.

FOR EACH jobitems OF jobs NO-LOCK
    BREAK BY jobitems.form_number BY jobitems.blank_number:

  assign cocode = job.company
         locode = gloc.

  assign
    v-plan     = ""
    v-not-done = yes
    v-type     = if job.stat eq "C" then "DEL" 
                 else if index("RL",job.stat) eq 0 then "CHG" 
                 else "JOB".

  /* Add KIWI TRAN_REQ */
  if v-type eq "DEL" then
    v-plan = "DEL  ".
  else if v-type eq "CHG" then
    v-plan = "CHANG".
  else if v-type eq "JOB" then
    v-plan = "ADD  ".
  else
    leave.

  /* Add KIWI TRAN_ANS */
  v-plan = v-plan + "     ".
  
  find first est where est.e-num                  eq job.e-num
                 no-lock no-error.

  if not avail est then next.

  find first eb where eb.e-num                   eq job.e-num
	           and eb.form-no                 eq jobitems.form_number
                no-lock no-error.

  if not avail eb then next.

  find first job-hdr where job-hdr.company              eq cocode
	                and job-hdr.job                  eq job.job
                       and job-hdr.job-no               eq job.job-no
                       and job-hdr.job-no2              eq job.job-no2
                     no-lock no-error.

  if not avail job-hdr then next.

  find first cust where cust.company             eq cocode
                    and cust.cust-no             eq job-hdr.cust-no
                  no-lock no-error.

  if not avail cust then next.

  find first oe-ord where oe-ord.company           eq cocode
                 	 and oe-ord.ord-no            eq job-hdr.ord-no
                	 and oe-ord.job-no            eq job.job-no
                	 and oe-ord.job-no2           eq job.job-no2
                    use-index job no-lock no-error.

  v-not-done[2] = yes.

  find first job-mch where job-mch.company              eq job.company
                       and job-mch.job                  eq job.job
                       and job-mch.job-no               eq job.job-no
                       and job-mch.job-no2              eq job.job-no2
                       and job-mch.frm                  eq jobitems.form_number
                       and job-mch.dept                 eq "CR"
                     no-lock no-error.
  
  find first ef where ef.e-num   eq job.e-num
                  and ef.form-no eq job-mch.frm
                no-lock no-error.

  if not avail job-mch or not avail ef then next.

  v-next-mch = job-mch.line.
  
  find first jobmach where jobmach.job                  eq jobitems.job
                       and jobmach.form_number          eq jobitems.form_number
                       and jobmach.sequence             gt v-next-mch 
                       and jobmach.department           ne "CR"
                     no-lock no-error.
  
  find first mach where mach.company eq cocode 
                 and mach.loc     eq locode
                 and mach.m-code  eq jobmach.machine
               no-lock no-error.

  /* Add KIWI POKEY and ASI Job# */
  v-plan = v-plan + trim(string(job.job-no + "-" + string(job.job-no2,"99"),"x(10)")) 
                  + fill(" ",10 - length(trim(string(job.job-no + "-" + string(job.job-no2,"99"),"x(10)")))).
  
  /* Add KIWI CUSTNM and ASI Cust# */
  v-plan = v-plan + trim(string(cust.cust-no,"x(10)"))
                  + fill(" ",10 - length(trim(string(cust.cust-no,"x(10)")))).

  /* Add KIWI UDESC and ASI ??????????? */
  v-plan = v-plan + if avail mach then substring(mach.m-dscr,1,3)
                    else fill(" ",3).

  /* Add KIWI DUEDATE and ASI Order Due Date */
  if avail oe-ord then   
    v-plan = v-plan + substr(string(year(oe-ord.due-date)),3,2) 
                    + string(month(oe-ord.due-date),"99")
                    + string(day(oe-ord.due-date),"99").
  else
    v-plan = v-plan + substr(string(year(job.start-date)),3,2) 
                    + string(month(job.start-date),"99")
                    + string(day(job.start-date),"99").

  /* Add KIWI LEADTIME and ASI Warehouse Days */
  v-plan = v-plan + string(cust.ship-days,"9999").

    
  v-plan = substring(v-plan,1,43).
            
  find first oe-ordl where oe-ordl.company eq gcompany
                       and oe-ordl.ord-no  eq job-hdr.ord-no
                       and oe-ordl.i-no    eq jobitems.fg_item
                     no-lock no-error.

  if avail oe-ordl then
    find first itemfg where itemfg.company           eq cocode
  	                 and itemfg.i-no              eq oe-ordl.i-no
	               no-lock no-error.
                                        
  /* Add KIWI ORDQTY and ASI Order Line Qty * qty/set */
  v-plan = v-plan + string(eb.yld-qty * oe-ordl.qty,"99999999").
  
  /* Add KIWI TGTQTY and ASI Job QTY */
  v-plan = v-plan + string(((job-hdr.qty * eb.yld-qty) / 
                           ((ef.n-out * ef.n-out-l) * (eb.num-len * eb.num-wid) * eb.num-up))  
                            ,"99999999").

  /* Add KIWI PCTOVR and ASI Overrun */
  v-plan = v-plan + if avail oe-ordl then string(oe-ordl.over-pct,"999") else 
                    if avail oe-ord  then string(oe-ord.over-pct,"999")  else "  5".
   
  /* Add KIWI PCTUND and ASI Underrun */
  v-plan = v-plan + if avail oe-ordl then string(oe-ordl.under-pct,"999") else
                    if avail oe-ord  then string(oe-ord.under-pct,"999")  else "  5".
   
  /* Add KIWI STACKHEIGHT and ASI Order Line Count */
  v-plan = v-plan + if avail oe-ordl then string(oe-ordl.cas-cnt,"999999") else "     0".

  /* Add KIWI STACKCOUNT and ASI always 1 */
  v-plan = v-plan + "01".

  /* Add KIWI spare */
  v-plan = v-plan + "  ".

  /* Add KIWI SPEC_NUM and ASI Estimate Number */
  v-plan = v-plan + trim(string(job.est-no,"x(10)"))
                  + fill(" ",10 - length(trim(string(job.est-no,"x(10)")))).
     
  /* Add KIWI BLENGTH and ASI Blank Length in total fraction of inches 809 = 50 9/16*/
  v-plan = v-plan + string(((truncate(ef.gsh-len,0) * 16) + 
                            (ef.gsh-len - truncate(ef.gsh-len,0)) * 16),"99999").
     
  /* Add KIWI BWIDTH and ASI Blank Width */
  v-plan = v-plan + string(((truncate(ef.gsh-wid,0) * 16) + 
                            (ef.gsh-wid - truncate(ef.gsh-wid,0)) * 16),"99999").
     
  /* Add KIWI CREASES and ASI Scores */
  v-score = "".
  do i = 1 to extent(eb.k-wid-array2):
    if ef.xgrain eq "N" then
    do:
      if eb.k-wid-array2[i] ne 0 then
        v-score = v-score + string(((truncate(eb.k-wid-array2[i],0) * 16) + 
                                  (eb.k-wid-array2[i] - truncate(eb.k-wid-array2[i],0)) * 16),">>>>9").
      else i = 20.
    end.
    else
    do:
      if eb.k-len-array2[i] ne 0 then
        v-score = v-score + string(((truncate(eb.k-len-array2[i],0) * 16) + 
                              (eb.k-len-array2[i] - truncate(eb.k-len-array2[i],0)) * 16),">>>>9").
      else i = 20.
    end.
  end.

  if string(((truncate(ef.gsh-wid,0) * 16) + 
           (ef.gsh-wid - truncate(ef.gsh-wid,0)) * 16),">>>>9") ne v-score then            
    v-plan = v-plan + string(v-score,"x(64)").
  else
    v-plan = v-plan + string(" ","x(64)").
     
  /* Add KIWI WREDUC and ASI default from KIWI */
  v-plan = v-plan + "**".

  /* Add KIWI MAX_SPLITS and ASI default 0 */
  v-plan = v-plan + "00".

  /* Add KIWI RATE_CD and ASI always 1 */
  v-plan = v-plan + "1".

  /* Add KIWI DESTINATION and ASI always "" */
  v-plan = v-plan + " ".

  /* Add KIWI CMBFLAG and ASI always " " */
  v-plan = v-plan + " ".

  /* Add KIWI CMBTRIM and ASI always "   " */
  v-plan = v-plan + "   ".

  /* Add KIWI CMBNUP and ASI always "   " */
  v-plan = v-plan + "   ".

  /* Add KIWI reserved */
  v-plan = v-plan + fill(" ",60).

  /* Add KIWI MOK1 and ASI default "" */
  v-plan = v-plan + "  ".

  /* Add KIWI MOK2 and ASI default "" */
  v-plan = v-plan + "  ".

  /* Add KIWI MOK3 and ASI default "" */
  v-plan = v-plan + "  ".

  /* Add KIWI ROTATE and ASI always N */
  v-plan = v-plan + "N".

  /* Add KIWI PART_ROTATE and ASI always N */
  v-plan = v-plan + "N".

  /* Add KIWI STATUS and ASI always 1 */
  v-plan = v-plan + "1".

  /* Add KIWI UNPRQTY and ASI Job Qty */
  v-plan = v-plan + string(((job-hdr.qty * eb.yld-qty) / 
                           ((ef.n-out * ef.n-out-l) * (eb.num-len * eb.num-wid) * eb.num-up))  
                            ,"99999999").

  run sys/inc/numup.p (job.e-num, ef.form-no, output v-up).

  v-up = v-up * (if ef.n-out   eq 0 then 1 else ef.n-out) *
	         (if ef.n-out-l eq 0 then 1 else ef.n-out-l).

  find first est-op
	   where est-op.e-num   eq job.e-num
	     and est-op.s-num   eq job-mch.frm
	     and est-op.m-code  eq job-mch.m-code
	     and est-op.op-pass eq job-mch.pass
	     and est-op.dept    eq "CR"
	     and est-op.line    lt 500
	   no-lock no-error.

  v-dec = 1.
  if avail est-op then
    run sys/inc/numout.p (recid(est-op), output v-dec).

  v-dec = v-up / v-dec.

  assign v-up  = v-dec.

  /* Add KIWI NO.UP and ASI Job Material Up */
  v-plan = v-plan + string(v-up,"999").

  /* Add KIWI PIECES TO JOIN and ASI */
  v-plan = v-plan + string(eb.yld-qty,">>9").

  /* Add KIWI CORR and ASI Dept Sequence Number */
  v-plan = v-plan + string(v-cr-seq,"9").

  /* Add KIWI DATETIME and ASI Current Date and Time */
  v-plan = v-plan + string(today,"999999") + substr(string(time,"HH:MM:SS"),1,2)
                                           + substr(string(time,"HH:MM:SS"),4,2)
                                           + substr(string(time,"HH:MM:SS"),7,2).

  /* Add KIWI BRDKEY */
  v-brdkey = trim(string(ef.board)). 
  
  buildloop:
  repeat v-bill-count = 1 to 8:
    find item-bom where item-bom.company  = cocode 
                    and item-bom.parent-i = ef.board 
                    and item-bom.line# = v-bill-count 
                  no-lock no-error.
   if not avail item-bom then next buildloop.
   assign
     v-brdkey = v-brdkey + trim(string(item-bom.i-no)).
  end.
  
  v-plan = v-plan + string(v-brdkey,"x(30)").

  /* Add KIWI CR_TYPE */
  v-plan = v-plan + fill(" ",15).

  /* Add KIWI PLANT_CODE */
  v-plan = v-plan + "1".

  /* Add KIWI UPGRADE LEVEL */
  v-plan = v-plan + "0".

  /* Add KIWI DOWNGRADE LEVEL */
  v-plan = v-plan + "0".

  /* Add KIWI MAX BOARD TYPES ALLOWED */
  v-plan = v-plan + "0".

  /* Add KIWI DISCHARGE DIRECTION */
  v-plan = v-plan + " ".

/*
  /* Add KIWI CR_LF */
  v-plan = v-plan + string(chr(10),"x(1)").
*/

  put v-plan  format "x(320)" skip.


end.

output close.


/*
INPUT FROM VALUE(optconfg.file_location + '\' + TRIM(jobs.job) + '.tmp') NO-ECHO.
OUTPUT TO VALUE(optconfg.file_location + '\' + TRIM(jobs.job) + '.txt').
REPEAT:
  IMPORT UNFORMATTED text-line.
  text-line = REPLACE(text-line,'"','').
  PUT UNFORMATTED text-line SKIP.
END.
OUTPUT CLOSE.
INPUT CLOSE.
OS-DELETE VALUE(optconfg.file_location + '\' + TRIM(jobs.job) + '.tmp').
*/
