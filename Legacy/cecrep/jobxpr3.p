/* --------------------------------------------- cec/rep/jobtick3.p 10/98 JLF */
/* cecrep/jobpac2.p from cecrep/jobtik3L.p  factory ticket           Landscape                                        */
/* -------------------------------------------------------------------------- */

define input parameter v-recid  as   recid.
define input parameter v-format as   character.
define input parameter v-terms  like cust.terms.
DEFINE VARIABLE k_frac AS DECIMAL INIT 6.25 NO-UNDO.

{sys/inc/VAR.i SHARED}
{cecrep/jobtick.i "shared"}

define variable v-cas-cnt like eb.cas-cnt.
define variable v-oecount as log.
define variable v-rec-alf as character extent 8.
define variable v-date    as date      init ?.
define variable v-qty     as decimal.
define variable v         as integer.

define workfile w-rec 
    field w-recdate as date 
    field w-rec-qty as decimal.


find job-hdr where recid(job-hdr) eq v-recid no-lock no-error.

find first sys-ctrl
    where sys-ctrl.company eq cocode
    and sys-ctrl.name    eq "OECOUNT"
    no-lock no-error.
v-oecount = available sys-ctrl and sys-ctrl.log-fld.

/*if lookup(v-format,"Brick,TriState,RFC") gt 0 then do:
*/
if available job-hdr and available xeb then
    for each rm-rcpth
        where rm-rcpth.company   eq cocode
        and rm-rcpth.job-no    eq job-hdr.job-no
        and rm-rcpth.job-no2   eq job-hdr.job-no2
        and rm-rcpth.rita-code eq "R"
        and can-find(first item where item.company eq cocode
        and item.i-no    eq rm-rcpth.i-no
        and index("BPR1234",item.mat-type) gt 0)
        no-lock,
      
        first job-mat
        where job-mat.company eq cocode
        and job-mat.rm-i-no eq rm-rcpth.i-no
        and job-mat.job-no  eq job-hdr.job-no
        and job-mat.job-no2 eq job-hdr.job-no2
        and job-mat.frm     eq xeb.form-no
        no-lock,
      
        each rm-rdtlh
        where rm-rdtlh.r-no   eq rm-rcpth.r-no
        and (rm-rdtlh.s-num eq xeb.form-no or rm-rdtlh.s-num eq 0)
        no-lock
      
        by rm-rcpth.trans-date
        by rm-rdtlh.qty descending:
            
        if rm-rcpth.pur-uom eq "EA" then
            v-qty = rm-rdtlh.qty.
        else
            run sys/ref/convquom.p(rm-rcpth.pur-uom, "EA",
                job-mat.basis-w, job-mat.len,
                job-mat.wid, job-mat.dep,
                rm-rdtlh.qty, output v-qty).
                             
        if v-qty le 0 then
            find prev w-rec no-error.
        else
            find first w-rec where w-recdate eq rm-rcpth.trans-date no-error.
    
        if not available w-rec then 
        do:
            create w-rec.
            w-recdate = rm-rcpth.trans-date.
        end.
                             
        w-rec-qty = w-rec-qty + v-qty.
    end.
  
assign
    v     = 0
    v-qty = 0.
   
for each w-rec by w-recdate:
    if w-rec-qty ne 0 then 
    do:
        {sys/inc/roundup.i w-rec-qty}
      
        assign
            v     = v + 1
            v-qty = v-qty + w-rec-qty.
      
        if v le 3 then
            assign
                v-rec-alf[v]     = string(w-recdate,"99/99/9999")
                v-rec-alf[v + 4] = string(w-rec-qty,"->,>>>,>>>,>>>").
    end.  
end.
  
v-rec-alf[8] = string(v-qty,">,>>>,>>>,>>>").
  
do v = 1 to 8:
    v-rec-alf[v] = trim(v-rec-alf[v]) + fill("_",100).
end.
  
display 
    "<=2><R+3><P7><C2>Packing:<C46>Shipping Info:<C64><B>Quantity Completed<c81> SHIP NOTES</b>" SKIP
    "<P10><C2>Pallet ID:" xeb.tr-no 
    when available xeb SKIP

    "<C4># Per Bndl:"
    xoe-ordl.cas-cnt 
    when available xoe-ordl @ xeb.cas-cnt
    xeb.cas-cnt 
    when available xeb SKIP
    "<C4># Per Unit:" xeb.tr-cnt 
    when available xeb SKIP
    "<C3>" v-stackcode format "x(28)" SKIP
    "<C4>Pallet:"
    trim(string({sys/inc/k16v.i xeb.tr-len},">,>>9")) + " x " +
    trim(string({sys/inc/k16v.i xeb.tr-wid},">,>>9"))
    when available xeb format "x(15)"  SKIP
    "<P6><=2><R+4><C46>Ship To #:" xoe-ord.sold-id 
    when available xoe-ord SKIP
    "<C46>" v-shp[1] SKIP
    "<C46>" v-shp[2] SKIP
    "<C46>" v-shp[3] SKIP
    "<C46>" v-shp[4] SKIP
    "<C46>Item PO #:" xoe-ordl.po-no 
    when available xoe-ordl SKIP
    "<P9><=2><R+4><C81>" cShpNote[1] SKIP
    "<C81>" cShpNote[2] SKIP
    "<C81>" cShpNote[3] SKIP
    "<C81>" cShpNote[4] SKIP
    "<P10>"
    with no-box no-labels frame m6 width 200 NO-ATTR-SPACE STREAM-IO.
