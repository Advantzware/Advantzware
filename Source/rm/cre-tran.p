/* -------------------------------------------------- rm/cre-tran.p 02/05 JLF */
/* Create a Transaction History Record for this bin/qty                       */
/* -------------------------------------------------------------------------- */

def input parameter ip-rowid as rowid.
def input parameter ip-rcode like rm-rcpth.rita-code.
def input parameter ip-qty   as dec.
DEFINE INPUT PARAMETER ipcReasonCode AS CHARACTER NO-UNDO .

{sys/inc/var.i shared}


find rm-bin where rowid(rm-bin) eq ip-rowid no-lock.

find first item
    where item.company eq cocode
      and item.i-no    eq rm-bin.i-no
    no-lock.
      
  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT X) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


create rm-rcpth.
assign
 rm-rcpth.r-no       = X
 rm-rcpth.trans-date = today
 rm-rcpth.post-date  = today
 rm-rcpth.company    = cocode
 rm-rcpth.loc        = locode
 rm-rcpth.rita-code  = ip-rcode
 rm-rcpth.i-no       = rm-bin.i-no
 rm-rcpth.i-name     = item.i-name
 rm-rcpth.pur-uom    = item.cons-uom.

create rm-rdtlh.
assign
 rm-rdtlh.r-no      = rm-rcpth.r-no
 rm-rdtlh.company   = rm-rcpth.company
 rm-rdtlh.rita-code = ip-rcode
 rm-rdtlh.loc       = rm-bin.loc
 rm-rdtlh.loc-bin   = rm-bin.loc-bin
 rm-rdtlh.tag       = rm-bin.tag
 rm-rdtlh.qty       = ip-qty
 rm-rdtlh.cost      = IF ip-rcode EQ "C" THEN 0 ELSE rm-bin.cost
 rm-rdtlh.reject-code[1] = ipcReasonCode .
