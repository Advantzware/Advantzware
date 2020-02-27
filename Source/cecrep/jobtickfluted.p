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
     
  
     if v-stackcode ne "" then v-stackcode = "Pattern:<b>" + trim(v-stackcode) + "</b>"  .

     if bf-xeb.stacks eq 0 then do:
       if bf-xeb.tr-cas ne 0 then do:
         v-dec = bf-xeb.cas-pal / bf-xeb.tr-cas.
         {sys/inc/roundup.i v-dec}
         v-numstacks = v-dec.
       end.
     end.
     else v-numstacks = bf-xeb.stacks.
  
     if v-numstacks ne 0 and bf-xeb.tr-cas ne 0 then
       v-stackcode = (IF v-stackcode <> "" THEN (v-stackcode + ",") ELSE "") + "<b>" +
                     trim(string(v-numstacks,">>9")) + " Stack/" +
                     trim(string(bf-xeb.tr-cas,">>9")) + " Layer" + "</b>"  .

  
  PUT    SKIP(1) 
          "P" AT 1
          "UNITIZING" AT 3
          IF avail bf-xeb THEN bf-xeb.tr-no  ELSE ""
          
         /* "<c32>" chr(124) format "x" 
          "<c36.5>" "Date"                    
          "<c45>" "Sheets Received"         
          "<c60.5>" "Units"                   
          "<c69>" "Complete"    */            
          

          "A"                       at 1
          
          "# Per Bndl:<b>"                 AT 3
          IF avail bf-xeb THEN bf-xeb.cas-cnt ELSE 0  "</b>" 
         
          /*"<c32>" chr(124) format "x" 
          "<c33.5>" v-rec-alf[1] format "x(16)"
          "<c44.5>" v-rec-alf[5] format "x(19)"
          "<c57.6>" fill("_",13) format "x(12)"
          "<c67>" fill("_",14)  FORMAT "x(14)"*/
          
        

          "C"                       at 1
         
          "# Per Unit:<b>" AT 3
          IF avail bf-xeb THEN bf-xeb.tr-cnt ELSE 0 "</b>"
         
          /*"<c32>" chr(124) format "x" 
          "<c33.5>" v-rec-alf[2] format "x(16)"
          "<c44.5>" v-rec-alf[6] format "x(19)"
          "<c57.6>" fill("_",13) format "x(12)"*/
        

          "K"                       at 1
         
           v-stackcode          AT 3     format "x(28)"
          
          /*"<c32>" chr(124) format "x" 
          "<c33.5>" v-rec-alf[3] format "x(16)"
          "<c44.5>" v-rec-alf[7] format "x(19)"
          "<c57.6>" fill("_",13) format "x(12)"           
          "<c69>Partial"    format "x(14)"  */
         
          "Pallet:<b>" AT 3
         IF avail bf-xeb THEN ( trim(string({sys/inc/k16v.i bf-xeb.tr-len},">,>>9")) + " x " +
          trim(string({sys/inc/k16v.i bf-xeb.tr-wid},">,>>9"))) ELSE ""
                                                     format "x(15)" "</b>"
          
          /*"<c32>" chr(124) format "x" 
          "<c36.5>" "Totals" format "x(16)"
          "<c44.5>" v-rec-alf[8] format "x(19)"
          "<c57.6>" fill("_",13) format "x(12)"
           "<c67>" fill("_",14)  FORMAT "x(14)" */
           "# of Units: " AT 3 STRING(IF AVAIL bf-xeb THEN bf-xeb.cas-pal ELSE 0) FORMAT "x(10)" .
          

     /* with no-box no-labels frame m6 width 132 NO-ATTR-SPACE STREAM-IO.*/
        
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


/* end ---------------------------------- copr. 1998  advanced software, inc. */

