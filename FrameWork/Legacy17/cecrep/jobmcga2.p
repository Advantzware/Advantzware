/* --------------------------------------------- cecrep/jobmcga2.p  */
/* cecrep/jobmcga2.p from cecrep/jobxpr2.p  factory ticket           Landscape                                        */
/* -------------------------------------------------------------------------- */

def input parameter v-recid  as   recid.
def input parameter v-format as   char.
def input parameter v-terms  like cust.terms.
DEF INPUT PARAMETER v-notes  LIKE itemfg.prod-notes.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.

{sys/inc/VAR.i SHARED}
{cecrep/jobtick.i "shared"}
{sys/inc/f16to32.i}

def var v-cas-cnt like eb.cas-cnt NO-UNDO.
def var v-oecount as   LOG NO-UNDO.
def var v-rec-alf as   char extent 8 NO-UNDO.
def var v-date    as   date init ? NO-UNDO.
def var v-qty     as   DEC NO-UNDO.
def var v         as   INT NO-UNDO.
DEF VAR v-tied AS LOG NO-UNDO.
DEF VAR v-casetag-param AS CHAR NO-UNDO FORMAT "x(20)" INIT "".

DEF BUFFER b-eb1 FOR eb.
DEF BUFFER b-eb2 FOR eb.
DEF BUFFER bf-set FOR eb.
DEF VAR v-int AS INT NO-UNDO.

DEFINE VARIABLE scr-style-1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE scr-style-2 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE scr-end-cell-l1 AS DECIMAL FORMAT "->>9.99" NO-UNDO.
DEFINE VARIABLE scr-end-cell-l2 AS DECIMAL FORMAT ">9.999999" NO-UNDO.
DEFINE VARIABLE scr-in-cell-length AS DECIMAL FORMAT ">9.999999" NO-UNDO.
DEFINE VARIABLE scr-end-cell-w1 AS DECIMAL FORMAT "->>9.99" NO-UNDO.
DEFINE VARIABLE scr-end-cell-w2 AS DECIMAL FORMAT ">9.999999" NO-UNDO.
DEFINE VARIABLE scr-in-cell-width AS DECIMAL FORMAT ">9.999999" NO-UNDO.
DEF VAR v-len AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF VAR v-qty-set AS DECIMAL FORMAT ">>>9" NO-UNDO.
DEF VAR v-qty-set2 AS DECIMAL FORMAT ">>>9" NO-UNDO.
DEF VAR v-wid AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF VAR v-hight1 AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF VAR v-hight2 AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF VAR v-slt1 AS DECIMAL FORMAT ">>9" NO-UNDO.
DEF VAR v-slt2 AS DECIMAL FORMAT ">>9" NO-UNDO.

def workfile w-rec field w-recdate as date field w-rec-qty as dec.

find job-hdr where recid(job-hdr) eq v-recid no-lock no-error.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECOUNT"
    no-lock no-error.
v-oecount = avail sys-ctrl and sys-ctrl.log-fld.

 IF AVAIL job-hdr THEN DO:

     FIND FIRST sys-ctrl-shipto WHERE
                   sys-ctrl-shipto.company = cocode AND
                   sys-ctrl-shipto.NAME = "CASETAG" AND
                   sys-ctrl-shipto.cust-vend = YES AND
                   sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                   /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                   sys-ctrl-shipto.char-fld > '' NO-LOCK NO-ERROR.
     IF AVAIL sys-ctrl-shipto THEN
         ASSIGN v-casetag-param = IF INDEX(sys-ctrl-shipto.char-fld,"/") > 0 THEN 
             ENTRY(NUM-ENTRIES(sys-ctrl-shipto.char-fld,"/"),sys-ctrl-shipto.char-fld,"/")
             ELSE IF INDEX(sys-ctrl-shipto.char-fld,"\") > 0 THEN 
                 ENTRY(NUM-ENTRIES(sys-ctrl-shipto.char-fld,"\"),sys-ctrl-shipto.char-fld,"\")
                 ELSE sys-ctrl-shipto.char-fld.
 END.

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

  FIND FIRST bf-set WHERE
      bf-set.company = xeb.company AND
      bf-set.est-no = xeb.est-no AND
      bf-set.form-no = 0
      NO-LOCK NO-ERROR.

  FIND FIRST b-eb1 WHERE 
      b-eb1.company EQ xeb.company AND
      b-eb1.est-no  EQ xeb.est-no AND
      b-eb1.form-no NE 0 AND
      b-eb1.blank-no NE 0
      USE-INDEX est-qty
      NO-LOCK NO-ERROR.
  
  IF AVAIL b-eb1 THEN
      FIND FIRST b-eb2 WHERE 
      b-eb2.company EQ xeb.company AND
      b-eb2.est-no  EQ xeb.est-no AND
      b-eb2.form-no NE 0 AND
      b-eb2.blank-no NE 0 AND
      ROWID(b-eb2) NE ROWID(b-eb1)
      USE-INDEX est-qty
      NO-LOCK NO-ERROR.

  IF AVAIL b-eb1 THEN
      DO:
      FIND FIRST style WHERE
          style.company EQ xeb.company AND
          style.style = b-eb1.style
          NO-LOCK.
      IF AVAIL style  THEN
          v-slt1 = style.dim-df .
      ASSIGN
          v-len = b-eb1.len
          v-hight1 = b-eb1.wid
          v-qty-set = b-eb1.yld-qty
          v-int = style.dim-df + 1
          scr-style-1 = b-eb1.style
          scr-end-cell-l1 = {sys/inc/k16.i b-eb1.k-len-array2[1]}
          scr-end-cell-l2 = {sys/inc/k16.i b-eb1.k-len-array2[v-int]}
          scr-in-cell-length = {sys/inc/k16.i b-eb1.k-len-array2[2]}.
      END.
      
      IF AVAIL b-eb2 THEN
      DO:
          FIND FIRST style WHERE
              style.company EQ xeb.company AND
              style.style = b-eb2.style
              NO-LOCK.
          IF AVAIL style  THEN
          v-slt2 = style.dim-df .
          
          ASSIGN
              v-wid = b-eb2.len
              v-hight2 = b-eb2.wid
              v-qty-set2 = b-eb2.yld-qty
              v-int = style.dim-df + 1
              scr-style-2 = b-eb2.style
              scr-end-cell-w1 = {sys/inc/k16.i b-eb2.k-len-array2[1]}
              scr-end-cell-w2 = {sys/inc/k16.i b-eb2.k-len-array2[v-int]}
              scr-in-cell-width = {sys/inc/k16.i b-eb2.k-len-array2[2]}.
      END.

      FIND FIRST eb WHERE eb.company = cocode
              AND eb.est-no = xeb.est-no
              AND eb.form-no = 0 NO-LOCK NO-ERROR.
/*       IF AVAIL eb THEN            */
/*           ASSIGN v-len = eb.len   */
/*               v-wid = eb.wid      */
/*               v-hight1 = eb.dep   */
/*               v-hight2 = eb.dep.  */

  DISPLAY "<C2><P7><B>Prep & Packing:</B>"  /*"Sheets Received"*/ "<P10>Long Strip:"  AT 60  "Short Strip:" AT 114
          /*"No. of Bundles" AT 97
          "No. of Skids/Bags" AT 125
          "Quantity Completed " AT 150 "Partial" AT 180 "<P10>"*/ SKIP
         /* "Pallet ID:" xeb.tr-no when avail xeb SKIP*/
          "<C2>Case/Count:"
          eb.cas-no when avail eb 
          eb.cas-cnt when avail eb
/*           xoe-ordl.cas-cnt when avail xoe-ordl @ xeb.cas-cnt   SAB REMOVED */
          "Length"                 AT 35
          "Height"                 AT 44
          "Strip"                  AT 52
          "Slots"                  AT 58
          "Interior"               AT 64
          "End"                    AT 78
           
          "Length"                 AT 83
          "Height"                 AT 92
          "Strip"                  AT 99
          "Slots"                  AT 105
          "Interior"               AT 111
          "End"                    AT 124
          /*v-rec-alf[1]              at 37     format "x(16)"*/
         /* v-rec-alf[5]              at 57     format "x(21)"*/
          /*fill("_",13)              at 81    format "x(13)"
          fill("_",14)              at 99    format "x(14)"
          fill("_",12)              at 116    format "x(12)"*/
          SKIP
          "<C2>Skid/Cases:" eb.tr-no when avail eb
           eb.cas-pal WHEN AVAIL eb
/*           xoe-ordl.cases-unit when avail xoe-ordl @ xeb.tr-cnt    SAB REMOVED */
/*           "Height" AT 35  */
          /*v-rec-alf[2]              at 37     format "x(16)"
          v-rec-alf[6]              at 57     format "x(21)"
          fill("_",13)              at 99    format "x(13)"*/
          SKIP
          /* pattern*/
          /*"<C2>Tied:" v-tied
          v-rec-alf[3]              at 37     format "x(16)"
          v-rec-alf[7]              at 57    format "x(21)"
          fill("_",13)              at 99    format "x(13)"*/
          SKIP
          "<C2>Label:" v-casetag-param /*v-stackcode SAB REMOVED */
          v-len /* b-eb1.len */  AT 28 /*34 */
          v-hight1 /* b-eb1.wid*/  AT 38 /*43*/
          v-qty-set AT 50 /*53*/
          v-slt1 AT 56 /*59*/
          scr-in-cell-length AT 60 /*63*/
          scr-end-cell-l2 AT 70 /*73*/

          v-wid  /*b-eb2.len*/ AT 79 /*82*/
          v-hight2 /*b-eb2.wid*/ AT 89 /*91*/
          v-qty-set2 AT 99 /* 100*/
          v-slt2 AT 103 /*106*/
          scr-in-cell-width AT 107 /*110*/
          scr-end-cell-w2 AT 117 /*120*/
          /*trim(string({sys/inc/k16v.i xeb.tr-len},">,>>9")) + " x " +
          trim(string({sys/inc/k16v.i xeb.tr-wid},">,>>9"))
                                                   when avail xeb format "x(15)" */
         /*"     Totals"             at 37     format "x(16)"
          v-rec-alf[8]              at 57     format "x(21)"
          fill("_",13)              at 99    format "x(13)"
          fill("_",12)              at 116   format "x(12)"*/

         SKIP
         "<C2>Pack Note:" v-notes
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
