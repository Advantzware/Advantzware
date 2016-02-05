/* ---------------------------------------------                  */
/* cecrep/jobhus2.p   Factory ticket  Landscape   for Hughes      */
/* -------------------------------------------------------------------------- */

def input parameter v-recid  as   recid.
def input parameter v-format as   char.
def input parameter v-terms  like cust.terms.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.

{sys/inc/VAR.i SHARED}
{cecrep/jobtick.i "shared"}

def var v-cas-cnt like eb.cas-cnt.
def var v-oecount as   log.
def var v-rec-alf as   char extent 8.
def var v-date    as   date init ?.
def var v-qty     as   dec.
def var v         as   int.

def workfile w-rec field w-recdate as date field w-rec-qty as dec.


find job-hdr where recid(job-hdr) eq v-recid no-lock no-error.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECOUNT"
    no-lock no-error.
v-oecount = avail sys-ctrl and sys-ctrl.log-fld.

/*if lookup(v-format,"Brick,TriState,RFC") gt 0 then do:
*/
  if avail job-hdr and avail xeb then
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
      by rm-rdtlh.qty desc:
            
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
    
    if not avail w-rec then do:
      create w-rec.
      w-recdate = rm-rcpth.trans-date.
    end.
                             
    w-rec-qty = w-rec-qty + v-qty.
  end.
  
  assign
   v     = 0
   v-qty = 0.
   
  for each w-rec by w-recdate:
    if w-rec-qty ne 0 then do:
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
  
  display /*v-line[7]                 at 2 
          "P"                       at 1
          chr(124) format "x"       at 2 not for xprint skip(1)*/ 
          "<P8><U>Packing:</U>" AT 13 
         /* "Sheets Received" AT 57
          "No. of Bundles" AT 87
          "No. of Skids/Bags" AT 115
          "Quantity Completed " AT 140 "Partial" AT 170*/
          "<P10>"SKIP
          "Pallet ID:" AT 2 xeb.tr-no when avail xeb SKIP
          "# Per Bndl:"                 AT 4
          xeb.cas-cnt when avail xeb
          xoe-ordl.cas-cnt when avail xoe-ordl @ xeb.cas-cnt
      /*
          v-rec-alf[1]              at 36     format "x(16)"
          v-rec-alf[5]              at 55     format "x(21)"
          fill("_",13)              at 80    format "x(13)"
          fill("_",14)              at 96    format "x(14)"
          fill("_",12)              at 113    format "x(12)"  */
          SKIP
          "# Per Unit:" AT 4 xeb.tr-cnt when avail xeb
          xoe-ordl.cases-unit when avail xoe-ordl @ xeb.tr-cnt
      /*
          v-rec-alf[2]              at 36     format "x(16)"
          v-rec-alf[6]              at 55     format "x(21)"
          fill("_",13)              at 96    format "x(13)"
       */   
          SKIP
          /* pattern*/
          v-stackcode          AT 4    format "x(28)"  
      /*    v-rec-alf[3]              at 36     format "x(16)"
          v-rec-alf[7]              at 55    format "x(21)"
          fill("_",13)              at 96    format "x(13)"  */
          SKIP
          "Pallet:" AT 4
          trim(string({sys/inc/k16v.i xeb.tr-len},">,>>9")) + " x " +
          trim(string({sys/inc/k16v.i xeb.tr-wid},">,>>9")) when avail xeb format "x(15)"
      /*  "     Totals"             at 36     format "x(16)"
          v-rec-alf[8]              at 55     format "x(21)"
          fill("_",13)              at 96    format "x(13)"
          fill("_",12)              at 113   format "x(12)" */
         with no-box no-labels frame m6 width 200 NO-ATTR-SPACE STREAM-IO.

/* ======
          "P" AT 1
          "UNITIZING" AT 4
          xeb.tr-no when avail xeb
          /*chr(124) format "x"       at 28
          chr(124) format "x"       at 31  */
          "Date"                    at 42
          "Sheets Received"         at 66
          "Units"                   at 99
          "Complete"                at 118
          /*chr(124) format "x"       at 131 */

          "A"                           AT 1
          /*chr(124) format "x"       at 2 */
          "# Per Bndl:"                 AT 4
          xeb.cas-cnt when avail xeb
          xoe-ordl.cas-cnt when avail xoe-ordl and v-oecount @ xeb.cas-cnt
          /*chr(124) format "x"       at 28
          chr(124) format "x"       at 31 */
          v-rec-alf[1]              at 36     format "x(16)"
          v-rec-alf[5]              at 63     format "x(21)"
          fill("_",13)              at 95    format "x(13)"
          fill("_",14)              at 113    format "x(14)"
          /*chr(124) format "x"       at 131     */

          "C"                          AT 1
          /*chr(124) format "x"       at 2    */
          "# Per Unit:" AT 4
          xeb.tr-cnt when avail xeb
          xoe-ordl.cas-cnt when avail xoe-ordl and not v-oecount @ xeb.tr-cnt
          /*r(124) format "x"       at 38
          chr(124) format "x"       at 41 */
          v-rec-alf[2]              at 36     format "x(16)"
          v-rec-alf[6]              at 63     format "x(21)"
          fill("_",13)              at 95    format "x(13)"
        /*  chr(124) format "x"       at 131 */

          "K"                       at 1
          /*chr(124) format "x"       at 2 */ 
          v-stackcode          AT 4    format "x(23)"
          /*chr(124) format "x"       at 38
          chr(124) format "x"       at 41  */
          v-rec-alf[3]              at 36     format "x(16)"
          v-rec-alf[7]              at 63    format "x(21)"
          fill("_",13)              at 95    format "x(13)"
          "   Partial"              at 116    format "x(14)"
          /*chr(124) format "x"       at 131     */

          /*chr(124) format "x"       at 2 */
          "Pallet:" AT 4
          trim(string({sys/inc/k16v.i xeb.tr-len},">,>>9")) + " x " +
          trim(string({sys/inc/k16v.i xeb.tr-wid},">,>>9"))
                                                   when avail xeb format "x(15)"
          /*chr(124) format "x"       at 28
          chr(124) format "x"       at 31 */
          "     Totals"             at 36     format "x(16)"
          v-rec-alf[8]              at 63     format "x(21)"
          fill("_",13)              at 95    format "x(13)"
          fill("_",14)              at 113    format "x(14)"
          /*chr(124) format "x"       at 131
          v-line[8]                 at 2   */

      with no-box no-labels frame m6 width 150 NO-ATTR-SPACE STREAM-IO.
      
/*end.

else

  display /*v-line[3]                 at 2

          "P"                       at 1
          chr(124) format "x"       at 2 */ SKIP(1)
          "UNITIZING" AT 10
          trim(xeb.tr-no) when avail xeb                                       
          /*chr(124) format "x"       at 131 */

          "A"                       at 9
          /*chr(124) format "x"       at 2     */
          "# Per Bndl:" AT 13
          xeb.cas-cnt when avail xeb
          xoe-ordl.cas-cnt when avail xoe-ordl and v-oecount @ xeb.cas-cnt
          "Cores/Legs:"             at 63
          "# of Units:"             at 110
          /*chr(124) format "x"       at 131     */

          "C"                       at 9
          /*chr(124) format "x"       at 2 */
          "# Per Unit:" AT 13
          xeb.tr-cnt when avail xeb
          xoe-ordl.cas-cnt when avail xoe-ordl and not v-oecount @ xeb.tr-cnt
          "Special Unitizing Instructions"      at 63
          "Qty:"                    at 110
          /*chr(124) format "x"       at 131 */

          "K"                       at 9
          /*chr(124) format "x"       at 2 */
          ":" AT 13
          v-stackcode               format "x(40)"
          fill("_",29) format "x(29)"      at 63
          "Date:"                   at 110
          /*chr(124) format "x"       at 131     */

          /*chr(124) format "x"       at 2 */
          "Pallet:" AT 13
          trim(xeb.tr-no) + " " +
          trim(string({sys/inc/k16v.i xeb.tr-len},">,>>9")) + " x " +
          trim(string({sys/inc/k16v.i xeb.tr-wid},">,>>9"))
                                                   when avail xeb format "x(27)"
          fill("_",29) format "x(29)"      at 63
          "CASH IN ADVANCE" at 110 when (v-terms = "CIA" and v-format = "Triad")
         /* chr(124) format "x"       at 131
          v-line[4]                 at 2    */
      with no-box no-labels frame m7 width 150 no-attr-space STREAM-IO.
*/
/* end ---------------------------------- copr. 1998  advanced software, inc. */
===*/
