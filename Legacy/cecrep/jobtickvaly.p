/* --------------------------------------------- cec/rep/jobtick3.p 10/98 JLF */
/*  factory ticket                                                            */
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
DEFINE VARIABLE v-dec    AS DECIMAL NO-UNDO.

def workfile w-rec field w-recdate as date field w-rec-qty as dec.
DEFINE BUFFER bf-xeb FOR eb.

{XMLOutput/XMLOutput.i &XMLOutput=XMLJobTicket} /* rstark 05181205 */

find job-hdr where recid(job-hdr) eq v-recid no-lock no-error.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECOUNT"
    no-lock no-error.
v-oecount = avail sys-ctrl and sys-ctrl.log-fld.

/*if lookup(v-format,"Brick,TriState,RFC") gt 0 then do:
*/
if xeb.est-type eq 6 THEN do:
      find first bf-xeb no-lock
          where bf-xeb.company EQ xeb.company 
            AND bf-xeb.est-no   eq xeb.est-no
            and bf-xeb.form-no eq 0
           no-error.
      IF AVAIL bf-xeb AND NOT bf-xeb.pur-man THEN
          find first bf-xeb no-lock
          where bf-xeb.company EQ xeb.company 
          AND ROWID(bf-xeb) EQ ROWID(xeb)
          no-error.
END.
ELSE 
    find first bf-xeb no-lock
          where bf-xeb.company EQ xeb.company 
        AND ROWID(bf-xeb) EQ ROWID(xeb)
           no-error.

  if avail job-hdr and avail bf-xeb then
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
        and job-mat.frm     eq bf-xeb.form-no
      no-lock,
      
      each rm-rdtlh
      where rm-rdtlh.r-no   eq rm-rcpth.r-no
        and (rm-rdtlh.s-num eq bf-xeb.form-no or rm-rdtlh.s-num eq 0)
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

  if bf-xeb.stack-code ne "" then v-stackcode = bf-xeb.stack-code.
     
  
     if v-stackcode ne "" then v-stackcode = "Pattern:" + trim(v-stackcode).

     if bf-xeb.stacks eq 0 then do:
       if bf-xeb.tr-cas ne 0 then do:
         v-dec = bf-xeb.cas-pal / bf-xeb.tr-cas.
         {sys/inc/roundup.i v-dec}
         v-numstacks = v-dec.
       end.
     end.
     else v-numstacks = bf-xeb.stacks.
  
     if v-numstacks ne 0 and bf-xeb.tr-cas ne 0 then
       v-stackcode = (IF v-stackcode <> "" THEN (v-stackcode + ",") ELSE "") +
                     trim(string(v-numstacks,">>9")) + " Stack/" +
                     trim(string(bf-xeb.tr-cas,">>9")) + " Layer".

  
  display /*v-line[7]                 at 2 
          "P"                       at 1
          chr(124) format "x"       at 2 not for xprint */ SKIP(1) 
          "P" AT 1
          "UNITIZING" AT 3
          bf-xeb.tr-no when avail bf-xeb
          /*chr(124) format "x"       at 28*/
          chr(124) format "x"       at 55
          "Date"                    at 63
          "Sheets Received"         at 79
          "Units"                   at 104
          "Complete"                at 119
          /*chr(124) format "x"       at 131 */

          "A"                       at 1
          /*chr(124) format "x"       at 2 */
          "# Per Bndl:"                 AT 3
          bf-xeb.cas-cnt when avail bf-xeb
          /*xoe-ordl.cas-cnt when avail xoe-ordl and v-oecount @ xeb.cas-cnt : task# 04130507*/
          /*"#/BNDL:" AT 3
          xeb.cas-cnt when avail xeb
          xoe-ordl.cas-cnt when avail xoe-ordl and v-oecount @ xeb.cas-cnt
          "#/Unit:" 
          xeb.tr-cnt when avail xeb
          xoe-ordl.cas-cnt when avail xoe-ordl and not v-oecount @ xeb.tr-cnt 
          */
          /*chr(124) format "x"       at 28 */
          chr(124) format "x"       at 55
          v-rec-alf[1]              at 57     format "x(16)"
          v-rec-alf[5]              at 76     format "x(21)"
          fill("_",13)              at 100    format "x(13)"
          fill("_",14)              at 116    format "x(14)"
          /*chr(124) format "x"       at 131     */

          "C"                       at 1
          /*chr(124) format "x"       at 2    */
          "# Per Unit:" AT 3
          bf-xeb.tr-cnt when avail bf-xeb
          /*xoe-ordl.cas-cnt when avail xoe-ordl and not v-oecount @ xeb.tr-cnt   : task# 04130507*/        
          /*chr(124) format "x"       at 28     */
          chr(124) format "x"       at 55
          v-rec-alf[2]              at 57     format "x(16)"
          v-rec-alf[6]              at 76     format "x(21)"
          fill("_",13)              at 100    format "x(13)"
        /*  chr(124) format "x"       at 131 */

          "K"                       at 1
          /*chr(124) format "x"       at 2 */ 
          v-stackcode          AT 3     format "x(28)"
          /*chr(124) format "x"       at 28*/
          chr(124) format "x"       at 55
          v-rec-alf[3]              at 57     format "x(16)"
          v-rec-alf[7]              at 76     format "x(21)"
          fill("_",13)              at 100    format "x(13)"
          "   Partial"              at 116    format "x(14)"
          /*chr(124) format "x"       at 131     */

          /*chr(124) format "x"       at 2 */
          "Pallet:" AT 3
          trim(string({sys/inc/k16v.i bf-xeb.tr-len},">,>>9")) + " x " +
          trim(string({sys/inc/k16v.i bf-xeb.tr-wid},">,>>9"))
                                                   when avail bf-xeb format "x(15)"
          /*chr(124) format "x"       at 28    */
          chr(124) format "x"       at 55
          "     Totals"             at 57     format "x(16)"
          v-rec-alf[8]              at 76     format "x(21)"
          fill("_",13)              at 100    format "x(13)"
          fill("_",14)              at 116    format "x(14)"
          /*chr(124) format "x"       at 131
          v-line[8]                 at 2   */

      with no-box no-labels frame m6 width 132 NO-ATTR-SPACE STREAM-IO.
        
      IF AVAIL bf-xeb THEN DO:
        /* rstark 05181205 */
        RUN XMLOutput (lXMLOutput,'JobTicketPack','','Row').
        RUN XMLOutput (lXMLOutput,'Unitizing',bf-xeb.tr-no,'Col').
        RUN XMLOutput (lXMLOutput,'No_Per_Bundle',bf-xeb.cas-cnt,'Col').
        RUN XMLOutput (lXMLOutput,'No_Per_Unit',bf-xeb.tr-cnt,'Col').
        RUN XMLOutput (lXMLOutput,'Pattern',v-stackcode,'Col').
        RUN XMLOutput (lXMLOutput,'Pallet',TRIM(STRING({sys/inc/k16v.i bf-xeb.tr-len})) + " x " +
                                           TRIM(STRING({sys/inc/k16v.i bf-xeb.tr-wid})),'Col').
        RUN XMLOutput (lXMLOutput,'/JobTicketPack','','Row').
        /* rstark 05181205 */
      END.

/*end.

else

  display /*v-line[3]                 at 2

          "P"                       at 1
          chr(124) format "x"       at 2 */ SKIP(1)
          "UNITIZING" AT 3
          trim(xeb.tr-no) when avail xeb                                       
          /*chr(124) format "x"       at 131 */

          "A"                       at 1
          /*chr(124) format "x"       at 2     */
          "# Per Bndl:" AT 3
          xeb.cas-cnt when avail xeb
          xoe-ordl.cas-cnt when avail xoe-ordl and v-oecount @ xeb.cas-cnt
          "Cores/Legs:"             at 53
          "# of Units:"             at 100
          /*chr(124) format "x"       at 131     */

          "C"                       at 1
          /*chr(124) format "x"       at 2 */
          "# Per Unit:" AT 3
          xeb.tr-cnt when avail xeb
          xoe-ordl.cas-cnt when avail xoe-ordl and not v-oecount @ xeb.tr-cnt
          "Special Unitizing Instructions"      at 53
          "Qty:"                    at 100
          /*chr(124) format "x"       at 131 */

          "K"                       at 1
          /*chr(124) format "x"       at 2 */
          "Pattern:" AT 3
          v-stackcode               format "x(40)"
          fill("_",29) format "x(29)"      at 53
          "Date:"                   at 100
          /*chr(124) format "x"       at 131     */

          /*chr(124) format "x"       at 2 */
          "Pallet:" AT 3
          trim(xeb.tr-no) + " " +
          trim(string({sys/inc/k16v.i xeb.tr-len},">,>>9")) + " x " +
          trim(string({sys/inc/k16v.i xeb.tr-wid},">,>>9"))
                                                   when avail xeb format "x(27)"
          fill("_",29) format "x(29)"      at 53
          "CASH IN ADVANCE" at 100 when (v-terms = "CIA" and v-format = "Triad")
         /* chr(124) format "x"       at 131
          v-line[4]                 at 2    */
      with no-box no-labels frame m7 width 132 no-attr-space STREAM-IO.
*/
/* end ---------------------------------- copr. 1998  advanced software, inc. */
