/* --------------------------------------------- cecrep/jobmcpt2.p  */
/* cecrep/jobmcpt2.p from cecrep/jobxpr2.p  factory ticket           Landscape                                        */
/* -------------------------------------------------------------------------- */

def input parameter v-recid  as   recid.
def input parameter v-format as   char.
def input parameter v-terms  like cust.terms.
DEF INPUT PARAMETER v-notes  LIKE itemfg.prod-notes.

DEF OUTPUT PARAM v-cas-no LIKE eb.cas-no NO-UNDO.
DEF OUTPUT PARAM v-cas-cnt like eb.cas-cnt NO-UNDO.
DEF OUTPUT PARAM v-skid LIKE eb.tr-no NO-UNDO.
DEF OUTPUT PARAM v-case-pallet AS CHAR NO-UNDO.
DEF OUTPUT PARAM v-casetag-param AS CHAR NO-UNDO FORMAT "x(20)" INIT "".

DEF OUTPUT PARAM v-len AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF OUTPUT PARAM v-hight1 AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF OUTPUT PARAM v-qty-set AS DECIMAL FORMAT ">>>9" NO-UNDO.
DEF OUTPUT PARAM v-slt1 AS DECIMAL FORMAT ">>9" NO-UNDO.
DEF OUTPUT PARAM scr-in-cell-length AS DECIMAL FORMAT ">9.999999" NO-UNDO.
DEF OUTPUT PARAM scr-end-cell-l2 AS DECIMAL FORMAT ">9.999999" NO-UNDO.

DEF OUTPUT PARAM v-wid AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF OUTPUT PARAM v-hight2 AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF OUTPUT PARAM v-qty-set2 AS DECIMAL FORMAT ">>>9" NO-UNDO.
DEF OUTPUT PARAM v-slt2 AS DECIMAL FORMAT ">>9" NO-UNDO.
DEF OUTPUT PARAM scr-in-cell-width AS DECIMAL FORMAT ">9.999999" NO-UNDO.
DEF OUTPUT PARAM scr-end-cell-w2 AS DECIMAL FORMAT ">9.999999" NO-UNDO.

DEF OUTPUT PARAM v-wid3 AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF OUTPUT PARAM v-hight3 AS DECIMAL FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEF OUTPUT PARAM v-qty-set3 AS DECIMAL FORMAT ">>>9" NO-UNDO.
DEF OUTPUT PARAM v-slt3 AS DECIMAL FORMAT ">>9" NO-UNDO.
DEF OUTPUT PARAM scr-in-cell-width3 AS DECIMAL FORMAT ">9.999999" NO-UNDO.
DEF OUTPUT PARAM scr-end-cell-w3 AS DECIMAL FORMAT ">9.999999" NO-UNDO.

DEF OUTPUT PARAM v-adders1 AS CHAR FORMAT "x(31)" NO-UNDO.
DEF OUTPUT PARAM v-adders2 AS CHAR FORMAT "x(31)" NO-UNDO.
DEF OUTPUT PARAM v-adders3 AS CHAR FORMAT "x(31)" NO-UNDO.

DEF VAR scr-end-cell-w12 AS DECIMAL.
DEF VAR v-int3 AS INT NO-UNDO. /* placeholder */
DEF VAR scr-style-3 AS CHAR NO-UNDO. /* placeholder */
          /*
          v-cas-no
          v-cas-cnt
          /* Skid/Cases */ v-skid
           v-cas-pal 
          /* Label */ v-casetag-param 
          v-len 
          v-hight1
          v-qty-set
          v-slt1 
          scr-in-cell-length 
          scr-end-cell-l2 
          v-wid  
          v-hight2 
          v-qty-set2
          v-slt2 
          scr-in-cell-width 
          scr-end-cell-w2 
          /*Pack Note */ v-notes
          */

DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.

{sys/inc/VAR.i SHARED}
{cecrep/jobtick.i "shared"}
{sys/inc/f16to32.i}

def var v-oecount as   LOG NO-UNDO.
def var v-rec-alf as   char extent 8 NO-UNDO.
def var v-date    as   date init ? NO-UNDO.
def var v-qty     as   DEC NO-UNDO.
def var v         as   INT NO-UNDO.
DEF VAR v-tied AS LOG NO-UNDO.


DEF BUFFER b-eb1 FOR eb.
DEF BUFFER b-eb2 FOR eb.
DEF BUFFER b-eb3 FOR eb.
DEF BUFFER bf-set FOR eb.
DEF VAR v-int AS INT NO-UNDO.

DEFINE VARIABLE scr-style-1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE scr-style-2 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE scr-end-cell-l1 AS DECIMAL FORMAT "->>9.99" NO-UNDO.


DEFINE VARIABLE scr-end-cell-w1 AS DECIMAL FORMAT "->>9.99" NO-UNDO.



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
                   sys-ctrl-shipto.char-fld > '' NO-LOCK NO-ERROR.
     IF AVAIL sys-ctrl-shipto THEN
         ASSIGN v-casetag-param = IF INDEX(sys-ctrl-shipto.char-fld,"/") > 0 THEN 
             ENTRY(NUM-ENTRIES(sys-ctrl-shipto.char-fld,"/"),sys-ctrl-shipto.char-fld,"/")
             ELSE IF INDEX(sys-ctrl-shipto.char-fld,"\") > 0 THEN 
                 ENTRY(NUM-ENTRIES(sys-ctrl-shipto.char-fld,"\"),sys-ctrl-shipto.char-fld,"\")
                 ELSE sys-ctrl-shipto.char-fld.
 END.

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

  IF AVAIL b-eb2 THEN
      FIND FIRST b-eb3 WHERE 
      b-eb3.company EQ xeb.company AND
      b-eb3.est-no  EQ xeb.est-no AND
      b-eb3.form-no NE 0 AND
      b-eb3.blank-no NE 0 AND
      ROWID(b-eb3) NE ROWID(b-eb1) AND
      ROWID(b-eb3) NE ROWID(b-eb2)
      USE-INDEX est-qty
      NO-LOCK NO-ERROR.

  IF AVAIL b-eb1 THEN
      DO:
        v-adders1 = "".
          FIND FIRST xef WHERE xef.company = b-eb1.company
                           AND xef.est-no  = b-eb1.est-no
                           AND xef.form-no = b-eb1.form-no
                         NO-LOCK NO-ERROR.
          IF AVAIL xef THEN
             FIND FIRST est-qty WHERE est-qty.company = xef.company
                                  AND est-qty.est-no  = xef.est-no
                                  AND est-qty.eqty    = xef.eqty
                                NO-LOCK NO-ERROR.

          IF AVAIL est-qty THEN
            FIND FIRST est-prep WHERE est-prep.company = b-eb1.company
                                  AND est-prep.est-no  = b-eb1.est-no                           
                              NO-LOCK NO-ERROR.
          IF AVAIL est-prep THEN
            v-adders1 = est-prep.dscr.

          IF AVAIL est-qty THEN
            FOR EACH est-op WHERE est-op.company = b-eb1.company
                                  AND est-op.est-no  = b-eb1.est-no
                                  AND est-op.s-num   = 1
                                  AND est-op.qty    = est-qty.eqty                            
                              NO-LOCK
                              BREAK BY est-op.est-no 
                                    BY est-op.eqty 
                                    BY est-op.qty
                                    BY est-op.s-num.
              IF FIRST-OF(est-op.s-num) THEN
                v-adders1 = v-adders1 + " " +  est-op.att-type[1]
                                      + " " + est-op.att-type[2]
                                      + " " + est-op.att-type[3] .
            END.
       
          FIND FIRST style WHERE
              style.company EQ xeb.company AND
              style.style = b-eb1.style
              NO-LOCK.
          IF AVAIL style  THEN
              v-slt1 = style.dim-df .
          ASSIGN
              v-len = b-eb1.len
              v-hight1 = b-eb1.wid
              v-qty-set = b-eb1.quantityPerSet
              v-int = style.dim-df + 1
              scr-style-1 = b-eb1.style
              scr-end-cell-l1 = {sys/inc/k16.i b-eb1.k-len-array2[1]}
              scr-end-cell-l2 = {sys/inc/k16.i b-eb1.k-len-array2[v-int]}
              scr-in-cell-length = {sys/inc/k16.i b-eb1.k-len-array2[2]}.
      
      END.
      
      IF AVAIL b-eb2 THEN
      DO:
          v-adders2 = "".
          FIND FIRST xef WHERE xef.company = b-eb2.company
                           AND xef.est-no  = b-eb2.est-no
                           AND xef.form-no = b-eb2.form-no
                         NO-LOCK NO-ERROR.
          IF AVAIL xef THEN
             FIND FIRST est-qty WHERE est-qty.company = xef.company
                                  AND est-qty.est-no  = xef.est-no
                                  AND est-qty.eqty    = xef.eqty
                                NO-LOCK NO-ERROR.
/*           IF AVAIL est-qty THEN                                        */
/*             FIND FIRST est-prep WHERE est-prep.company = b-eb2.company */
/*                                   AND est-prep.est-no  = b-eb2.est-no  */
/*                                   AND est-prep.eqty    = est-qty.eqty  */
/*                               NO-LOCK NO-ERROR.                        */
          IF AVAIL est-qty THEN
            FOR EACH est-op WHERE est-op.company = b-eb2.company
                                  AND est-op.est-no  = b-eb2.est-no
                                  AND est-op.s-num   = 2
                                  AND est-op.qty    = est-qty.eqty                            
                            NO-LOCK
                           BREAK BY est-op.est-no 
                                    BY est-op.eqty 
                                    BY est-op.qty
                                    BY est-op.s-num.
              IF FIRST-OF(est-op.s-num) THEN
                v-adders2 = v-adders2 + " " +  est-op.att-type[1]
                                      + " " + est-op.att-type[2]
                                      + " " + est-op.att-type[3] .
            END.
          FIND FIRST style WHERE
              style.company EQ xeb.company AND
              style.style = b-eb2.style
              NO-LOCK.
          IF AVAIL style  THEN
          v-slt2 = style.dim-df .
          
          ASSIGN
              v-wid = b-eb2.len
              v-hight2 = b-eb2.wid
              v-qty-set2 = b-eb2.quantityPerSet
              v-int = style.dim-df + 1
              scr-style-2 = b-eb2.style
              scr-end-cell-w1 = {sys/inc/k16.i b-eb2.k-len-array2[1]}
              scr-end-cell-w2 = {sys/inc/k16.i b-eb2.k-len-array2[v-int]}
              scr-in-cell-width = {sys/inc/k16.i b-eb2.k-len-array2[2]}.
      END.

      IF AVAIL b-eb3 THEN
      DO:
          v-adders3 = "".
          FIND FIRST xef WHERE xef.company = b-eb1.company
                           AND xef.est-no  = b-eb1.est-no
                           AND xef.form-no = b-eb1.form-no
                         NO-LOCK NO-ERROR.
          IF AVAIL xef THEN
             FIND FIRST est-qty WHERE est-qty.company = xef.company
                                  AND est-qty.est-no  = xef.est-no
                                  AND est-qty.eqty    = xef.eqty
                                NO-LOCK NO-ERROR.
/*           IF AVAIL est-qty THEN                                        */
/*             FIND FIRST est-prep WHERE est-prep.company = b-eb1.company */
/*                                   AND est-prep.est-no  = b-eb1.est-no  */
/*                                   AND est-prep.eqty    = est-qty.eqty  */
/*                               NO-LOCK NO-ERROR.                        */
          IF AVAIL est-qty THEN
            FIND FIRST est-op WHERE est-op.company = b-eb1.company
                                  AND est-op.est-no  = b-eb1.est-no
                                  AND est-op.qty    = est-qty.eqty                            
                              NO-LOCK NO-ERROR.
          IF AVAIL est-op THEN
            v-adders3 = v-adders3 + " " + est-op.att-type[1]
                                  + " " + est-op.att-type[2]
                                  + " " + est-op.att-type[3] .

          FIND FIRST style WHERE
              style.company EQ xeb.company AND
              style.style = b-eb3.style
              NO-LOCK.
          IF AVAIL style  THEN
          v-slt3 = style.dim-df .
          
          ASSIGN
              v-wid3 = b-eb3.len
              v-hight3 = b-eb3.wid
              v-qty-set3 = b-eb3.quantityPerSet
              v-int3 = style.dim-df + 1
              scr-style-3 = b-eb3.style
              scr-end-cell-w12 = {sys/inc/k16.i b-eb3.k-len-array2[1]}
              scr-end-cell-w3 = {sys/inc/k16.i b-eb3.k-len-array2[v-int3]}
              scr-in-cell-width3 = {sys/inc/k16.i b-eb3.k-len-array2[3]}.
      END.

      FIND FIRST eb WHERE eb.company = cocode
              AND eb.est-no = xeb.est-no
              AND eb.form-no = 0 NO-LOCK NO-ERROR.
      IF NOT AVAIL eb THEN
          FIND FIRST eb WHERE eb.company = cocode
                  AND eb.est-no = xeb.est-no
                  AND eb.form-no = 1 NO-LOCK NO-ERROR.

          ASSIGN v-cas-no = ""
                 v-cas-cnt = 0
                 v-skid = ""
                 v-cas-pal = 0.

          IF AVAIL eb THEN
            ASSIGN v-cas-no  = eb.cas-no
                   v-cas-cnt = eb.cas-cnt
                   v-skid    = eb.tr-no
                   v-cas-pal = eb.cas-pal
                   v-case-pallet = substring(eb.tr-no,1, 11) + FILL(" ", 12 - LENGTH(SUBSTRING(eb.tr-no, 1, 11))) + string(v-cas-pal).

  /*
  DISPLAY "<C2><P7><B>Prep & Packing:</B>"  "<P10>Long Strip:"  AT 60  "Short Strip:" AT 114
          SKIP
          "<C2>Case/Count:"
          eb.cas-no when avail eb 
          eb.cas-cnt when avail eb
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
          SKIP
          "<C2>Skid/Cases:" eb.tr-no when avail eb
           eb.cas-pal WHEN AVAIL eb
          SKIP
          SKIP
          "<C2>Label:" v-casetag-param 
          v-len AT 28 /*34 */
          v-hight1  AT 38 /*43*/
          v-qty-set AT 50 /*53*/
          v-slt1 AT 56 /*59*/
          scr-in-cell-length AT 60 /*63*/
          scr-end-cell-l2 AT 70 /*73*/

          v-wid   AT 79 /*82*/
          v-hight2 AT 89 /*91*/
          v-qty-set2 AT 99 /* 100*/
          v-slt2 AT 103 /*106*/
          scr-in-cell-width AT 107 /*110*/
          scr-end-cell-w2 AT 117 /*120*/
         SKIP
         "<C2>Pack Note:" v-notes
         with no-box no-labels frame m6 width 200 NO-ATTR-SPACE STREAM-IO.
    */

      
/* end ---------------------------------- copr. 1998  advanced software, inc. */
