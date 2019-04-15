/* ---------------------------------------------- */
/*  cecrep/jobprogn.p  factory ticket  for Hughes landscape */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.

DEF SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF SHARED VAR revision-no AS CHAR NO-UNDO.
DEF VAR prt-copies AS INT  NO-UNDO.
DEF VAR v-start-compress AS cha NO-UNDO.
DEF VAR v-end-compress AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ink-1 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-2 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-3 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-4 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-5 AS cha FORM "X(30)"NO-UNDO.
DEF var v-dept-note AS cha FORM "x(40)" EXTENT 50 NO-UNDO.
DEF var v-dpr-note AS cha FORM "x(40)" EXTENT 50 NO-UNDO.
DEF var v-spec-note AS cha FORM "x(40)" EXTENT 15 NO-UNDO.
DEF VAR v-deptnote AS cha NO-UNDO.
DEF VAR v-dept-length AS DEC NO-UNDO.
DEF VAR lv-under-run AS cha NO-UNDO.
DEF VAR lv-over-run AS cha NO-UNDO.
DEF VAR lv-part-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-fg-name AS cha NO-UNDO.
DEF VAR lv-dscr1 AS cha NO-UNDO.
DEF VAR lv-dscr2 AS cha NO-UNDO.
DEF VAR lv-mch-dscr AS cha NO-UNDO.
DEF VAR lv-status AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-sts-code AS cha INIT "O,R,C,T,N,X,Q" NO-UNDO.
DEF VAR lv-sts-desc AS cha INIT "O-Original,R-Repeat,C-Change,T-Transfer,N-New Customers,X-Complete Re-run,Q-Quality/Re-work" NO-UNDO.


DEF var v-adders1        as   char format "x(31)"                    no-undo.
DEF VAR v-adder-dscr     LIKE ITEM.est-dscr NO-UNDO.
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image1 = "images\Protagon.jpg"
       ls-image2 = "".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-sman AS cha FORM "x(25)" NO-UNDO.
DEF VAR v-blk-per-frm AS cha FORM "x(15)" NO-UNDO.
DEF VAR ls-fgitem-img AS cha FORM "x(50)" NO-UNDO.
DEF VAR v-qa-text AS cha FORM "x(30)" INIT "6/05 Job Ticket QF-119 Rev.A" NO-UNDO.
DEF VAR v-due-code AS cha FORM "x(4)" NO-UNDO.
DEF VAR v-cad-no AS cha FORM "X(12)" NO-UNDO.
DEF VAR v-rel-date AS DATE NO-UNDO.
DEF VAR v-rel-qty AS INT NO-UNDO.
DEF VAR v-i-code2 AS CHAR FORMAT "X(10)" NO-UNDO.
DEF VAR v-max-qty AS DECI NO-UNDO.
{jcrep/r-ticket.i "shared"}

{cecrep/jobprogn.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}
{sys/inc/notes.i}
def new shared var v-out1-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def new shared var v-out2-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
 
def var laser           as   log    init no     format "Y/N"            no-undo.
def var v-vend-no       like oe-ordl.vend-no                            no-undo.
def var v-po-no         like oe-ordl.po-no                           no-undo.
def var v-po-num        AS CHAR EXTENT 12                                no-undo.
def var v-po-duedate    like po-ordl.due-date EXTENT 12                  no-undo.
def var v-po-vendor     like po-ordl.vend-no  EXTENT 12                  no-undo.
def var v-po-qty        AS CHAR  EXTENT 12 FORMAT "x(14)"                 no-undo.
def var v-qty-or-sup    as   char               format "x(38)"          no-undo.
def var v-i-line        as   char   extent 5    format "x(38)"          no-undo.
def var v-flag          as   log    init no                             no-undo.
def var v-local-copies  as   int                                        no-undo.
def var v-local-loop    as   int    init 1                              no-undo.
def var v-print-score   as   log    init yes                            no-undo.
def var v-pqty          as   dec                                        no-undo.
DEF VAR lv-part-no AS cha FORM "x(15)" NO-UNDO.
DEF VAR lv-rt-num AS INT NO-UNDO.
def stream ctl.
DEF VAR lv-add-entry AS INT NO-UNDO.
DEF VAR v-loop-cnt AS INT NO-UNDO.
DEF VAR v-note-cnt AS INT NO-UNDO.
DEF VAR v-note-length AS INT NO-UNDO.
DEF VAR v-die-loc AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-plate-loc AS cha FORM "x(12)" NO-UNDO.
DEF VAR v-tmp-lines AS dec NO-UNDO.
DEF VAR lv-got-return AS int NO-UNDO.
DEF VAR v-prev-note-rec AS RECID NO-UNDO.
DEF VAR v-prev-extent AS INT NO-UNDO.
DEF VAR v-prev-ext-gap AS INT NO-UNDO.
DEF VAR v-coldscr LIKE eb.i-coldscr NO-UNDO.
DEF SHARED VAR s-prt-ship-split AS LOG NO-UNDO.
DEF SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF SHARED VAR s-sample-required AS LOG NO-UNDO.
DEF BUFFER b-eb FOR eb.
DEF VAR lv-split AS cha FORM "x(60)" EXTENT 4 NO-UNDO.
DEF VAR lv-au AS cha FORM "x(20)" NO-UNDO.
DEF VAR lv-est-type AS cha FORM "x(35)" NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER b-ef FOR ef.
DEF VAR v-tmp-line AS INT NO-UNDO.
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEF WORKFILE tt-wm LIKE w-m.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR lv-dtext AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 100 NO-UNDO.
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR v-sht-qty-string AS cha NO-UNDO.
DEF VAR v-w-l AS cha NO-UNDO.
DEF VAR v-form-code1 AS cha NO-UNDO.
DEF VAR v-sample-required-text  AS CHAR FORMAT "X(35)" NO-UNDO.
DEF VAR v-i AS INT NO-UNDO.
DEF VAR ls-length AS INT NO-UNDO.
DEF VAR ls-start AS INT NO-UNDO.
DEF VAR v-print-po AS CHAR NO-UNDO.
DEF VAR vDesignerName AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR vProjectManager AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-max-note-line AS INT INIT 43 NO-UNDO.
DEF VAR v-last-note-line AS INT NO-UNDO.
DEF VAR iNotePage AS INT NO-UNDO.
DEF VAR iNotePrtLine AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg.

/* gdm - 11030802 */
DEF VAR v-sampreq LIKE reftable.val[2] NO-UNDO.

{cecrep/jc-fibre.i }
DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.
{cecrep/jc-prem.i}
{custom/formtext.i NEW}

assign
v-line[1] = chr(95) + fill(chr(95),40) + chr(95) + "  " +
             chr(95) + fill(chr(95),40) + chr(95) + "  " +
             chr(95) + fill(chr(95),40) + chr(95)  
 v-line[2] = v-line[1]
 v-line[3] = chr(95) + fill(chr(95),128) + chr(95)
 v-line[4] = v-line[3]
 v-line[5] = chr(95) + fill(chr(95),84) + chr(95) + "  " +
                chr(95) + fill(chr(95),40) + chr(95)
 v-line[6] = v-line[5]
 v-line[7] = chr(95) + fill(chr(95),25) + chr(95) + "  " +
             chr(95) + fill(chr(95),99) + chr(95)
 v-line[8] = v-line[7]
 v-qty-or-sup = if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") gt 0
                then ("Supplier:"     + fill("_",28))
                else ("Qty Received: " + fill("_",24))
 v-local-copies = 1
 prt-copies = 1.

do v-local-loop = 1 to v-local-copies:

    for each job-hdr
        where job-hdr.company               eq cocode
          and job-hdr.ftick-prnt            eq reprint

          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no,

        first job
        where job.company                   eq cocode
          and job.job                       eq job-hdr.job
          and job.job-no                    eq job-hdr.job-no
          and job.job-no2                   eq job-hdr.job-no2
          and job.stat                      ne "H"
        no-lock,
        
        first est
        where est.company = job.company
          AND est.est-no                    eq job.est-no
          and est.est-type                  gt 4
        no-lock,

        first cust
        where cust.company                  eq cocode
          and cust.cust-no                  eq job-hdr.cust-no
        no-lock,

        first itemfg
        where itemfg.company                eq cocode
          and itemfg.i-no                   eq job-hdr.i-no
        no-lock

        break by job.job-no
              by job.job-no2:

      v-break = first-of(job.job-no2).

      release xest.
      release xef.
      release xeb.
      release xoe-ord.
      release xoe-ordl.
      release xoe-rel.


      run cecrep/jobtick1.p (recid(job-hdr), v-format,
                              v-local-loop, v-local-copies).

      for each w-ef WHERE (w-ef.frm = job-hdr.frm OR est.est-type <> 8),
          EACH b-eb NO-LOCK WHERE b-eb.company = job-hdr.company
                              AND b-eb.est-no = job-hdr.est-no 
                              AND b-eb.form-no = w-ef.frm
                              AND (b-eb.blank-no = job-hdr.blank-no OR est.est-type NE 8)
          BREAK BY w-ef.frm 
                BY b-eb.blank-no:
          
        release xef.
        release xeb.
        release xstyle.
        release xxprep.
        
        
        run cecrep/jobtickhu.p (recid(w-ef), recid(job-hdr),RECID(b-eb)).
        
        ASSIGN
           v-pqty = 1
           v-cp = ""
           v-sman = "".

        if avail xeb then do:
          if xeb.stock-no ne "" then v-fg = xeb.stock-no.

          ASSIGN
          v-cp = xeb.part-no
          lv-fg-name = itemfg.i-name
          lv-dscr1   = itemfg.part-dscr1    
          lv-dscr2   = itemfg.part-dscr2  .

          {cec/rollfac.i}
          v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                   if xeb.quantityPerSet lt 0 then (-1 / xeb.quantityPerSet)
                                       else xeb.quantityPerSet.
          FIND FIRST sman WHERE sman.company = xeb.company AND
                                sman.sman = xeb.sman NO-LOCK NO-ERROR.
          v-sman = IF AVAIL sman THEN sman.sNAME ELSE xeb.sman.
        end.
        
        assign
         v-loc     = ""
         v-loc-bin = "".
         
        if v-format eq "Brick" or v-format eq "Corrugat" then do: 
          v-iso = "ISO# CS-05-1-F".
          release fg-rdtlh.          
          find first fg-bin
            where fg-bin.company   eq cocode
              and fg-bin.i-no      eq job-hdr.i-no
              and fg-bin.job-no    eq job-hdr.job-no
              and fg-bin.job-no2   eq job-hdr.job-no2
              and fg-bin.loc       eq job-hdr.loc
              and fg-bin.qty       ne 0
          no-lock no-error.
          if avail fg-bin then do:
            assign
              v-loc     = "Whs: " + fg-bin.loc
              v-loc-bin = "Bin: " + fg-bin.loc-bin.
          end.
          else
          if avail itemfg then do:                             
            assign
              v-loc     = "Whs: " + itemfg.def-loc
              v-loc-bin = "Bin: " + itemfg.def-loc-bin.
          end.
        end. /*brick format*/

        ASSIGN lv-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                             IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE ""
               lv-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                              IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE ""
               lv-part-name = xeb.part-dscr1   
               v-blk-per-frm = "  Part " + string(xeb.form-no,"99") + " of " + string(xest.form-qty,"99")     
               v-due-code = IF AVAIL xoe-ord THEN xoe-ord.due-code ELSE "ON"
               lv-au = IF itemfg.alloc THEN "U" ELSE "A".

        IF est.est-type = 6 THEN lv-au = lv-au + "  " + job-hdr.i-no.
        ELSE lv-au = "".
        ASSIGN
        lv-est-type = IF est.est-type = 5 THEN "SINGLE"
                      ELSE IF est.est-type = 6 THEN "SET"
                      ELSE "COMBO"
        lv-est-type = lv-est-type + "  FORM " + string(b-eb.form-no) + " OF " + string(xest.form-qty)
                      + "  BLANK " + STRING(b-eb.blank-no) + " OF " + STRING(xef.blank-qty)
        v-cad-no = IF AVAIL xeb THEN xeb.cad-no ELSE ""
        v-rel-date = IF AVAIL xoe-rel THEN xoe-rel.rel-date ELSE ?
        v-rel-qty = IF AVAIL xoe-rel THEN xoe-rel.tot-qty ELSE 0.                        

       PUT    "</PROGRESS><P12><B>"
       "JOB TICKET" AT 5  " " lv-au  caps(SUBSTRING(v-fg,1,1)) FORM "x" AT 40
       "Production Specification" AT 45 
        "</B><C72>" lv-est-type "</B><P10>"     SKIP
       "<#1><C1><FROM><C105><R+45><RECT><|3>" 
       "<=1><C32><FROM><R+45><C32><LINE><|3>"
       "<=1><C66><FROM><R+45><C66><LINE><|3>"
       "<=1><C90><FROM><R+4><C90><LINE><|3>"
       "<=1><R+4><C1><FROM><C105><LINE><|3>"     /*sold to*/
       "<=#1><R+11><C1><FROM><C105><LINE><|3>"   /*board*/
       "<=1><R+19><C1><FROM><C105><LINE><|3>"    /*shipping info*/
       "<=1><R+19><C33><P8>                 <U>Dept Notes:</U> <C80><U>More Dept Notes:</U>"
/*        "<=1><R+26><C1><FROM><C32><LINE><|3>"   /*packing*/ */
       "<=1><R+35><C1><FROM><C32><LINE><|3>"   /*machine routing*/
       "<=1><R+35><C24><FROM><R+10><LINE><|3>"    
       "<=1><R+37><C66><FROM><C105><LINE><|3>"   /*sheets ordered*/
       /*"<=1><R+37><C71><P8><U>Sheets Ordered:</U>          <U>Due Date:</U>      <U>Supplier:</U>"
       "<=1><R+37><C86><FROM><R+8><LINE><|3>"   
       "<=1><R+37><C96><FROM><R+8><LINE><|3>" */ 
       "<=1><R+37><C67><P10><U><B>Quality Control Approval</B></U>"                 
       "<=1><R+40><C78><P8>OPERATOR  _______________________" 
       "<=1><R+42><C68><P8>Quality-design approval  _______________________"
       .

       view frame head.  /* factory header display  */  

       if v-format eq "RFC" or v-format eq "Boxtech" then
          assign
           v-i-line[1] = itemfg.i-name
           v-i-line[2] = itemfg.part-dscr1
           v-i-line[3] = itemfg.part-dscr2
           v-i-line[4] = itemfg.part-dscr3.
        else DO:
          FIND FIRST bf-eb OF est WHERE bf-eb.form-no = 1 NO-LOCK NO-ERROR.
          assign
           v-i-line[1] = "ITEM DESCRIPTION"
           v-i-line[2] = "Style: " + if avail xstyle then xstyle.dscr else ""
           v-i-line[3] = "Size: "  + if avail xeb    then
                     trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
           v-i-line[4] = "Joint: " + if avail xeb then v-joint-dscr else ""
           vDesignerName = "Designer: " + IF AVAIL bf-eb THEN bf-eb.spare-char-1 ELSE ""   .
        END.
        ASSIGN
        v-i-line[5] = "ORDER TYPE:" + lv-status
        lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                     ELSE itemfg.part-no.

        /*if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") gt 0 then do:*/
        /*RUN sys/ref/getpo#.p (IF AVAIL xoe-ordl AND est.est-type <> 6 THEN ROWID(xoe-ordl) ELSE ROWID(job),
                              w-ef.frm, OUTPUT v-po-no). 
        
        RELEASE po-ord.

        IF v-po-no NE 0 THEN
        FIND FIRST po-ord
            WHERE po-ord.company EQ cocode
              AND po-ord.po-no   EQ v-po-no
            NO-LOCK NO-ERROR.
          
        ASSIGN
         v-vend-no    = IF AVAIL po-ord THEN po-ord.vend-no ELSE ""
         v-qty-or-sup = "Supplier: ".

        if v-vend-no ne "" then do:
           v-qty-or-sup = v-qty-or-sup + trim(v-vend-no).
           IF v-po-no ne 0 THEN v-qty-or-sup = v-qty-or-sup + " PO#:" +
                                               trim(string(v-po-no,">>>>>>>>>>")).
        end.*/

   
     ASSIGN
        v-po-num        = " "      
        v-po-vendor     = " "          
        v-po-duedate    =  ?
        v-po-qty        = "".
     ASSIGN j = 1.

     IF xeb.pur-man = NO THEN DO:
       FOR EACH po-ordl
           WHERE po-ordl.company   EQ job.company
           AND po-ordl.job-no    EQ job.job-no
           AND po-ordl.job-no2   EQ job.job-no2
           AND po-ordl.s-num     EQ b-eb.form-no
           AND po-ordl.item-type EQ YES
/*            AND LOOKUP(po-ordl.stat,"O,P,U") GT 0 */
           USE-INDEX job-no NO-LOCK,

           FIRST po-ord WHERE
           po-ord.company EQ po-ordl.company AND
           po-ord.po-no   EQ po-ordl.po-no /*AND */
/*            LOOKUP(po-ord.stat,"N,O,R,U") GT 0 */
           NO-LOCK:
           ASSIGN
             v-po-num[j]    =  STRING(po-ordl.po-no)     
             v-po-vendor[j]  =  po-ord.vend-no          
             v-po-duedate[j] =  po-ordl.due-date
             v-po-qty[j]     =  string(po-ordl.ord-qty, "->>,>>>,>>9.9<") 
         j = j + 1.
         IF j > 12 THEN
             LEAVE.
       END.
     END.
     ELSE IF AVAIL xeb THEN DO:
         FOR EACH po-ordl
             WHERE po-ordl.company   EQ xeb.company
             AND po-ordl.i-no    EQ xeb.stock-no
             AND po-ordl.ord-no  EQ int(v-ord-no)
/*              AND LOOKUP(po-ordl.stat,"O,P,U") GT 0 */
             NO-LOCK,

             FIRST po-ord WHERE
             po-ord.company EQ po-ordl.company AND
             po-ord.po-no   EQ po-ordl.po-no /*AND*/
/*              LOOKUP(po-ord.stat,"N,O,R,U") GT 0  */
             NO-LOCK:
           ASSIGN
               v-po-num[j]    =  STRING(po-ordl.po-no)     
               v-po-vendor[j]  =  po-ord.vend-no          
               v-po-duedate[j] =  po-ordl.due-date
               v-po-qty[j]     =  string(po-ordl.ord-qty, "->>,>>>,>>9.9<") 
           j = j + 1.
           IF j > 12 THEN
               LEAVE.
         END.
     END.

       /* v-qty-or-sup = v-qty-or-sup + fill("_",38 - length(v-qty-or-sup)).*/
        
        i = 0.
        for each w-i:
          i = i + 1.
        end.
        
        if i le 4 then do i = i + 1 to 5:
           create w-i.
        end.

        ASSIGN
           v-ink-1 = ""
           v-ink-2 = ""
           v-ink-3 = ""
           v-ink-4 = ""
           v-ink-5 = "".             
        
        FOR EACH tt-formtext:
            DELETE tt-formtext.
        END.
        lv-dtext = "".          
        FOR EACH notes WHERE notes.rec_key = job.rec_key and
                    notes.note_code = "PR" and
                    LOOKUP(notes.note_code,v-exc-depts) EQ 0 AND
                    (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0),
            FIRST dept WHERE dept.code = notes.note_code NO-LOCK 
                       BY notes.note_code:  
            lv-dtext = lv-dtext + /*"<B>" + caps(dept.dscr) + ":</B>" + CHR(10) + 
                           "  " +*/ notes.note_text + CHR(10).          
        END.
                
        DO li = 1 TO 42:
          CREATE tt-formtext.
              ASSIGN tt-line-no = li
                     tt-length  = 39. 
        END.
        RUN custom/formtext.p (lv-dtext).
        i = 0.
        v-inst2 = "".
        FOR EACH tt-formtext:
              i = i + 1.
              IF  i <= 42 THEN v-dpr-note[i] = tt-formtext.tt-text.  
        END.                         

        DO v-i = 1 TO 5:
           IF b-eb.i-code[v-i] <> "" THEN DO:
               FIND FIRST w-i WHERE w-i.i-code = b-eb.i-code[v-i] NO-ERROR.
               IF AVAIL w-i THEN DO:
                  IF v-ink-1 = "" THEN
                     v-ink-1 =  w-i.i-dscr +
                           (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                            IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                  ELSE
                     IF v-ink-2 = "" THEN
                        v-ink-2 =  w-i.i-dscr +
                           (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                            IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                     ELSE
                        IF v-ink-3 = "" THEN
                           v-ink-3 =  w-i.i-dscr +
                           (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                            IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                        ELSE
                           IF v-ink-4 = "" THEN
                              v-ink-4 =  w-i.i-dscr +
                              (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                              IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                           ELSE
                              IF v-ink-5 = "" THEN
                                 v-ink-5 =  w-i.i-dscr +
                                 (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                                 IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
               END.
           END.
        END.

        v-form-sqft = round(if v-corr then (v-form-len * v-form-wid * .007)
                                       else (v-form-len * v-form-wid / 144),3).

        FIND FIRST b-itemfg WHERE b-itemfg.company = itemfg.company AND b-itemfg.i-no = xeb.stock-no
                NO-LOCK NO-ERROR.
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.die-no
                            no-lock no-error.
        v-die-loc = IF AVAIL xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.plate-no
                            no-lock no-error.
        ASSIGN
        v-plate-loc = IF AVAIL xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE ""
        lv-status = IF AVAIL xoe-ord THEN ENTRY(lookup(xoe-ord.TYPE,lv-sts-code),lv-sts-desc) ELSE ""
        v-coldscr = IF AVAIL b-eb AND b-eb.i-coldscr <> "" THEN b-eb.i-coldscr ELSE "".
        DEF VAR v-see-1st-blank AS LOG NO-UNDO.
        ASSIGN
        v-see-1st-blank = IF NOT FIRST-OF(w-ef.frm) OR
                             CAN-FIND(FIRST bf-eb WHERE bf-eb.company = b-eb.company
                                                    AND bf-eb.est-no = b-eb.est-no
                                                    AND bf-eb.form-no = b-eb.form-no
                                                    AND bf-eb.blank-no < b-eb.blank-no)
                          THEN YES ELSE NO
        v-max-qty = ( (if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty) * 
                      ( if est.form-qty le 1 AND est.est-type NE 6 then 1 else v-pqty) * 
                      (1 + IF AVAIL xoe-ordl THEN xoe-ordl.over-pct / 100 
                           ELSE IF AVAIL xoe-ord  THEN xoe-ord.over-pct / 100 
                           ELSE 0 )
                    ).
       

            lv-mch-dscr = "". v-adder-dscr = "".
            v-adders1 = xef.adder[1].
            IF xef.adder[1] > "" THEN DO:
              FIND FIRST ITEM WHERE ITEM.i-no = xef.adder[1] NO-LOCK NO-ERROR.
              IF AVAIL ITEM THEN
                  v-adder-dscr = ITEM.est-dscr.
            END.

        FIND FIRST est-op WHERE est-op.est-no = xeb.est-no 
               AND est-op.dept = "DC" 
               AND est-op.company = cocode 
               AND est-op.s-num = w-ef.frm
            NO-LOCK NO-ERROR.

        IF AVAIL est-op THEN
            ASSIGN lv-mch-dscr = est-op.m-dscr.

        DEF VAR ind-j AS INT NO-UNDO.
        ind-j = INDEX(v-len-score," ",26) .
        IF ind-j = 0 THEN ind-j = 30 .
    
        ASSIGN
            v-inst = ""
            v-dept-note[42] = IF AVAIL xoe-ord THEN xoe-ord.user-id ELSE "".
            IF v-dept-note[42] = "" THEN
            v-dept-note[42] = IF AVAIL job THEN job.user-id ELSE "".
        DISP  "<B><p12>" v-cus[1] /*AT 2 */ 
              "<B><P12>Cust Set/Box#:" AT 42 lv-part-no "</B><P10>"   
              "Style:" AT 93 xstyle.dscr WHEN AVAIL xstyle              
              SKIP
              v-cus[2] AT 2  
              "<B><P12>Job Qty:" AT 40 trim(string(job-hdr.qty * v-pqty,">>>,>>9")) format "x(7)"
              "</B><P10>"
              "Order Qty:" trim(string((if avail xoe-ordl then xoe-ordl.qty
                                             else job-hdr.qty) *
                                            if est.form-qty le 1 AND est.est-type NE 6 then 1
                                            else v-pqty,">>>,>>9"))
                                            format "x(7)" 
              
              "Size:" (trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99"))) FORM "x(30)" WHEN AVAIL xeb            
              SKIP
              v-cus[3] AT 2
              "Set Qty:" AT 40
               trim(string(if avail xoe-ordl then xoe-ordl.qty
                                          else job-hdr.qty,">>>,>>9"))
                        when avail xeb and xeb.est-type eq 6    format "x(9)" 
              "Max Qty:" AT 61 STRING(v-max-qty,">>>,>>9")
              "Joint:" AT 80 v-joint-dscr             
              SKIP
              v-cus[4] AT 2            
             /* 
              "Overrun:" AT 40 trim(string(IF AVAIL xoe-ordl THEN xoe-ordl.over-pct ELSE
                                           IF AVAIL xoe-ord  THEN xoe-ord.over-pct  ELSE 0,">>9.99%")) format "x(7)"
              "Underrun:" trim(string(IF AVAIL xoe-ordl THEN xoe-ordl.under-pct ELSE
                                      IF AVAIL xoe-ord  THEN xoe-ord.under-pct  ELSE 0,">>9.99%")) format "x(7)"
              */
              "Item Name: " AT 40 lv-fg-name FORM "x(26)"
              /*"Adders:" AT 80 v-adders FORM "x(23)"*/
              "PO #:" AT 2 xoe-ord.po-no WHEN AVAIL xoe-ord 
            /* "Part item name:" AT 40  */
              "<P12><B>" AT 40 substring(lv-part-name,1,22) FORM "x(25)"  "</B><P10>"
              "Status:" AT 2 lv-status  SUBSTRING(lv-part-name,23,30) AT 57               
              "<P8>"SKIP
              "<U>Board:</U>" AT 15 "   <U>Printing:</U>" AT 70 " <U>Die Cutting, Slit, & Saw</U>" AT 130 "<P10>"SKIP
              "Shts Req'd:" AT 2 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank 
              trim(string(v-sht-qty)) FORMAT "x(9)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank  @ v-sht-qty-string 
              "" WHEN v-see-1st-blank @ v-sht-qty-string
              " Sq Ft:" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank 
               trim(string(v-form-sqft)) WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank  FORM "x(7)"                            
/*               "PRINTING PLATE #:" AT 40 xeb.plate-no when avail xeb */
              /*"PLATE #:" AT 40 xeb.plate-no FORMAT "X(11)" when avail xeb " Loc:" v-plate-loc FORMAT "X(11)"*/
              "Color Desc:" AT 39 v-coldscr              
              "Die Cutter: " AT 80 lv-mch-dscr format "x(20)" SKIP
              "W:" + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))  WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank format "x(22)"  @ v-w-l AT 2
              "SEE 1st BLANK" WHEN v-see-1st-blank @ v-w-l   
              "MSF:"  + trim(string(v-sht-qty * v-form-sqft / 1000,">>>9.9<")) format "x(11)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank           v-dpr-note[1] AT 39 /*"Ink 1:" AT 39 v-ink-1*/              
              "Die #" AT 80 xeb.die-no when avail xeb " Loc:" v-die-loc 

              "<B><P12>"
              SKIP
            WITH FRAME job1 NO-LABEL NO-BOX WIDTH 200 STREAM-IO. 

         DISP
              

/*               "Board:" + v-form-dscr  FORM "x(29)"  @ v-form-code1 AT 2 */
/*               "Board:" + v-form-code  WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank  FORM "x(29)"  @ v-form-code1 AT 2 */

/*               "Board:" + v-board-dscr WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank  FORM "x(29)"  AT 2 */
              "Board:" + v-board-code FORM "x(29)"  @ v-form-code1 AT 2
              " " WHEN v-see-1st-blank @ v-form-code1 
/*               "</B><P10>Ink 2:" AT 32 v-ink-2 "Net   Size:"  AT 83 /*91*/ */
              /*"</B><P10> Ink 2:"  v-ink-2 "Net   Size:"  AT 83*/
              "</B><P10>"  v-dpr-note[2]                              
              "Gross Size:" AT 83  
              "W:" + trim(string({sys/inc/k16v.i xef.gsh-wid},">>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i xef.gsh-len},">>>9.99"))
                                                                format "x(19)"
              "Slit: W:" + string(v-outw) + " L:" + string(v-outl) FORM "x(15)"

              "<P12>" SKIP
              /*v-board-dscr  AT 2 FORM "x(29)"*/
             /*"Adders:" AT 2*/ v-adders1 + " " + v-adder-dscr FORM "x(29)" AT 2
/*               " "  AT 2 FORM "x(29)" */
              /*"</B><P10> Ink 3:"  v-ink-3 "Die   Size:" AT 83*/
              "</B><P10>"  v-dpr-note[3]                

             "Net   Size:"  AT 83
              "W:" + trim(string({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>9.99")) format "x(22)"

              SKIP
              v-dpr-note[4] AT 39
              "Die   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(21)"
              "Up:" "W:" + string(v-upl) + " L:" + string(v-upw) FORM "x(9)"

              SKIP
              /*v-qty-or-sup AT 3 FORM "x(36)" */
              "Score:" AT 2 substring(v-len-score,1,ind-j) WHEN xstyle.TYPE <> "F" format "x(30)" 
              /*"Ink 5:" AT 39 v-ink-5*/ v-dpr-note[5] AT 39
                            
              "Blank Size:" AT 80 
              "W:" + trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)"

              SKIP                            
              SUBSTRING(v-len-score,ind-j + 1,36) AT 3 FORM "x(36)"              
              "PM:" AT 39 v-dept-note[42] FORMAT "x(30)"
              "Impressions:" AT 80 trim(string(v-dc-qty))    format "x(7)" "   Die Inch:" xef.die-in SKIP
              WITH FRAME job1 NO-LABEL NO-BOX WIDTH 200 STREAM-IO.                
        
           /*01111302*/
            FOR EACH tt-formtext:
                DELETE tt-formtext.
            END.
            lv-text = "".          
           
            FOR EACH notes WHERE notes.rec_key = b-itemfg.rec_key AND 
                LOOKUP(notes.note_code,spec-list) GT 0,
                FIRST item-spec WHERE item-spec.code = notes.note_code NO-LOCK 
                BY notes.note_code:  
                lv-text = lv-text + "<B>" + caps(item-spec.notes[1]) + ":</B>" + CHR(10) + 
                             /*notes.note_title +*/  "  " + notes.note_text + CHR(10).         
            END.

            DO li = 1 TO 15:
              CREATE tt-formtext.
                  ASSIGN tt-line-no = li
                         tt-length  = 36. 
            END.
            RUN custom/formtext.p (lv-text).
            i = 0.
            v-inst2 = "".
            FOR EACH tt-formtext:
                  i = i + 1.
                  IF  i <= 15 THEN v-spec-note[i] = tt-formtext.tt-text.  
            END.
       
           DISPLAY "<P8><U>Spec Notes: <P10></U>" AT 12           
               v-spec-note[1] AT 2 SKIP
               v-spec-note[2] AT 2 SKIP
               v-spec-note[3] AT 2 SKIP
               v-spec-note[4] AT 2 SKIP
               v-spec-note[5] AT 2 SKIP
               v-spec-note[6] AT 2 SKIP
               v-spec-note[7] AT 2 SKIP
               v-spec-note[8] AT 2 SKIP
               v-spec-note[9] AT 2 SKIP
               v-spec-note[10] AT 2 SKIP
               v-spec-note[11] AT 2 SKIP
               v-spec-note[12] AT 2 SKIP
               v-spec-note[13] AT 2 SKIP
               v-spec-note[14] AT 2 SKIP
               v-spec-note[15] AT 2 SKIP
             WITH NO-BOX NO-LABEL FRAME shp-info STREAM-IO.
/*           DISPLAY "<P8><U>Shipping Info: <P10></U>" AT 12 SKIP          */
/*               "Ship To #:" AT 2                                         */
/*               xoe-ord.sold-id when avail xoe-ord                        */
/*               xeb.ship-id when avail xeb @ xoe-ord.sold-id              */
/*               xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id SKIP */
/*               v-shp[1] AT 2  SKIP                                       */
/*               v-shp[2] AT 2  SKIP                                       */
/*               v-shp[3] AT 2  SKIP                                       */
/*               v-shp[4] AT 2 SKIP                                        */
/*               "Item PO #:" AT 2  xoe-ordl.po-no when avail xoe-ordl     */
/*              WITH NO-BOX NO-LABEL FRAME shp-info STREAM-IO.             */
/*                                                                         */
/*         run cecrep/jobhus2.p (recid(job-hdr),v-format,cust.terms). */
        i = 0.
        for each w-m:
          i = i + 1.
        end.
        if i lt 3 then do i = i + 1 to 3:
          create w-m.
          w-m.dseq = 999999999.
        end.
        /* box for route */       
        lv-rt-num = i + 3.        
        put /*SKIP(3)*/
            "<P8>        <U>Machine Routing:</U>              <U>Sheets</U>"
            "<P10>"  SKIP.
        i = 0.
        for each w-m by w-m.dseq:
          i = i + 1.
          v-letter = substr("UTE",i,1).
         
          display           
                  w-m.dscr AT 3   "<P8>  <U>Received:</U><P10>" WHEN i = 1 AT 29               
              with no-box no-labels frame oo1 width 150 no-attr-space down STREAM-IO.                 
          v-lines = v-lines + 1.
        end.
/*==
        /* dept notes */
        v-note-length = 39 /*124*/ .
        ASSIGN v-tmp-lines = 0
               j = 0
               K = 0
               lv-got-return = 0
               v-dept-note = "" 
               v-prev-note-rec = ?
               v-prev-extent = 0
               v-prev-ext-gap = 0.

        FOR EACH notes WHERE notes.rec_key = job.rec_key and
                    (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
                 NO-LOCK BY notes.note_code:
            FIND FIRST dept WHERE dept.code = notes.note_code NO-LOCK NO-ERROR.
            IF v-prev-note-rec <> ? AND
               v-prev-note-rec <> RECID(notes) THEN v-prev-extent = v-prev-extent + k - v-prev-ext-gap.

            DO i = 1 TO LENGTH(notes.note_text):
               IF i - j >= v-note-length THEN ASSIGN j = i
                                            lv-got-return = lv-got-return + 1.

               v-tmp-lines = ( i - j ) / v-note-length.
               {SYS/INC/ROUNDUP.I v-tmp-lines}

               k = v-tmp-lines + lv-got-return + 
                   IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.

               IF k < 50 THEN v-dept-note[k] = v-dept-note[k] + 
                                               IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) 
                             ELSE "" .              

               IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
               THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
               END.         
            END.
            v-prev-ext-gap = v-prev-extent.
            ASSIGN v-prev-note-rec = RECID(notes)
                   j = 0
                   lv-got-return = 0.
        END.
*/
        FOR EACH tt-formtext:
            DELETE tt-formtext.
        END.
        lv-text = "".          
        FOR EACH notes WHERE notes.rec_key = job.rec_key and
                     notes.note_code NE "PR" AND notes.note_code NE "BM" AND
                    (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0) AND
                    LOOKUP(notes.note_code,v-exc-depts) EQ 0,
            FIRST dept WHERE dept.code = notes.note_code NO-LOCK 
                       BY notes.note_code:  
            lv-text = lv-text + "<B>" + caps(dept.dscr) + ":</B>" + CHR(10) + 
                         /*notes.note_title +*/  "  " + notes.note_text + CHR(10).          
        END.
        DO li = 1 TO 42:
          CREATE tt-formtext.
              ASSIGN tt-line-no = li
                     tt-length  = 39. 
        END.
        RUN custom/formtext.p (lv-text).
        i = 0.
        v-inst2 = "".
        FOR EACH tt-formtext:
              i = i + 1.
              IF  i <= 42 THEN v-dept-note[i] = tt-formtext.tt-text.  
        END.
        
        IF s-sample-required = TRUE THEN DO:
            FIND FIRST reftable WHERE reftable.reftable = "cecrep/d-hughes.w"
                                  AND reftable.company  = cocode
                                  AND reftable.loc      = TRIM(job-hdr.job-no)
                                  AND reftable.code     = STRING(job-hdr.job-no2,"9999999999")
                                  AND reftable.code2    = string(w-ef.frm,"999")  /*form-no*/
                                  AND reftable.val[1]   = b-eb.blank-no NO-ERROR. /*blank-no*/
            IF AVAILABLE reftable THEN
               v-sample-required-text = "        " + STRING(reftable.val[2]) + " SAMPLE(S) REQUIRED".
            ELSE
               v-sample-required-text = "".
        END.

        IF s-prt-ship-split THEN
           FIND FIRST tt-fibre WHERE tt-fibre.tt-job-no = job-hdr.job-no
                         AND tt-fibre.tt-job-no2 = job-hdr.job-no2
                         AND tt-fibre.tt-frm = w-ef.frm
                         AND tt-fibre.tt-blank = b-eb.blank-no NO-LOCK NO-ERROR.
        IF AVAIL tt-fibre THEN
           ASSIGN lv-split[1] = "LOC1 <U>" + string(tt-fibre.tt-sqty1,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order1,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty1,">>>,>>9") + "</U>"
                  lv-split[2] = "LOC2 <U>" + string(tt-fibre.tt-sqty2,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order2,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty2,">>>,>>9") + "</U>"
                  lv-split[3] = "LOC3 <U>" + string(tt-fibre.tt-sqty3,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order3,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty3,">>>,>>9") + "</U>"
                  lv-split[4] = "LOC4 <U>" + string(tt-fibre.tt-sqty4,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order4,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty4,">>>,>>9") + "</U>".                        

        /*FIND FIRST po-ordl WHERE po-ordl.po-no = v-po-no
                           AND po-ordl.company = cocode NO-LOCK NO-ERROR.

        IF AVAIL po-ordl THEN
            ASSIGN
            v-po-num     = STRING(po-ordl.po-no)
            v-po-duedate = po-ordl.due-date
            v-po-vendor  = po-ordl.vend-no.*/


        PUT "<=1><R+20><C33>" v-dept-note[1]  "  " /*space(7) v-dept-note[26] */
            "<=1><R+21><C33>" v-dept-note[2]  "  " /*"<=1><R+21><C68>" v-dept-note[27]*/ 
            "<=1><R+22><C33>" v-dept-note[3]  "  " /*"<=1><R+22><C68>" v-dept-note[28] */
            "<=1><R+23><C33>" v-dept-note[4]  "  " "<=1><R+21><C68>" "PO#:      Vendor:      PO Qty:   Due Date:"   /*v-dept-note[24] */
            "<=1><R+24><C33>" v-dept-note[5]  "  " "<=1><R+22><C68>"   v-po-num[1] SPACE(2) v-po-vendor[1] v-po-qty[1] SPACE(1) v-po-duedate[1] /*v-dept-note[26] */
            "<=1><R+25><C33>" v-dept-note[6]  "  " "<=1><R+23><C68>"   v-po-num[2] SPACE(2) v-po-vendor[2] v-po-qty[2] space(1) v-po-duedate[2] /*v-dept-note[25] */
            "<=1><R+26><C33>" v-dept-note[7]  "  " "<=1><R+24><C68>"   v-po-num[3] SPACE(2) v-po-vendor[3] v-po-qty[3] space(1) v-po-duedate[3] /*v-dept-note[27] */ 
            "<=1><R+27><C33>" v-dept-note[8]  "  " "<=1><R+25><C68>"   v-po-num[4] SPACE(2) v-po-vendor[4] v-po-qty[4] space(1) v-po-duedate[4] /*v-dept-note[28] */ 
            .

        IF s-sample-required = FALSE THEN
           PUT "<=1><R+28><C33>" v-dept-note[9]  "  " "<=1><R+26><C68>" v-po-num[5] SPACE(2) v-po-vendor[5] v-po-qty[5] space(1) v-po-duedate[5] 
               "<=1><R+29><C33>" v-dept-note[10] "  " "<=1><R+27><C68>" v-po-num[6] SPACE(2) v-po-vendor[6] v-po-qty[6] space(1) v-po-duedate[6] 
               "<=1><R+30><C33>" v-dept-note[11] "  " "<=1><R+28><C68>" v-po-num[7] SPACE(2) v-po-vendor[7] v-po-qty[7] space(1) v-po-duedate[7] 
               "<=1><R+31><C33>" v-dept-note[12] "  " "<=1><R+31><C68>" v-po-num[8] SPACE(2) v-po-vendor[8] v-po-qty[8] space(1) v-po-duedate[8] /* v-dept-note[42] */
               "<=1><R+32><C33>" v-dept-note[13] "  " "<=1><R+31><C68>" v-po-num[9] SPACE(2) v-po-vendor[9] v-po-qty[9] space(1) v-po-duedate[9]
               "<=1><R+33><C33>" v-dept-note[14] "  " "<=1><R+31><C68>" v-po-num[10] SPACE(2) v-po-vendor[10] v-po-qty[10] space(1) v-po-duedate[10]
               "<=1><R+34><C33>" v-dept-note[15] "  " "<=1><R+31><C68>" v-po-num[11] SPACE(2) v-po-vendor[11] v-po-qty[11] space(1) v-po-duedate[11]
               "<=1><R+35><C33>" v-dept-note[16] "  " "<=1><R+31><C68>" v-po-num[12] SPACE(2) v-po-vendor[12] v-po-qty[12] space(1) v-po-duedate[12]
               "<=1><R+36><C33>" v-dept-note[17]  /*v-dept-note[36]*/
                                                     /* upto here*/         
               "<=1><R+37><C33>" v-dept-note[18]  
               "<=1><R+38><C33>" v-dept-note[19] 
               "<=1><R+39><C33>" v-dept-note[20] 
               "<=1><R+40><C33>" v-dept-note[21] 
               "<=1><R+41><C33>" v-dept-note[22] 
               "<=1><R+42><C33>" v-dept-note[23] 
               "<=1><R+43><C33>" v-dept-note[24] 
               "<=1><R+44><C33>" v-dept-note[25]                               
               .
        ELSE
           PUT
               "<=1><R+28><C33>" v-dept-note[9]  "  " "<=1><R+28><C66><FROM><C105><LINE><||3>"
               "<=1><R+29><C33>" v-dept-note[10] "  " "<=1><R+29><C66>" "<B><P12>" v-sample-required-text "</B><P10>" 
               "<=1><R+30><C33>" v-dept-note[11] "  " "<=1><R+30><C66><FROM><C105><LINE><||3>"
               "<=1><R+31><C33>" v-dept-note[12] "  " "<=1><R+31><C66>" v-dept-note[42]
               "<=1><R+32><C33>" v-dept-note[13] "  "/*v-dept-note[32]*/
               "<=1><R+33><C33>" v-dept-note[14] "  " /*v-dept-note[33]*/
               "<=1><R+34><C33>" v-dept-note[15] "  " /*v-dept-note[34]*/
               "<=1><R+35><C33>" v-dept-note[16] "  " /*v-dept-note[35]*/
               "<=1><R+36><C33>" v-dept-note[17] "  " /*v-dept-note[36]*/
                                                     /* upto here*/         
               "<=1><R+37><C33>" v-dept-note[18] 
               "<=1><R+38><C33>" v-dept-note[19] 
               "<=1><R+39><C33>" v-dept-note[20] 
               "<=1><R+40><C33>" v-dept-note[21] 
               "<=1><R+41><C33>" v-dept-note[22] 
               "<=1><R+42><C33>" v-dept-note[23] 
               "<=1><R+43><C33>" v-dept-note[24] 
               "<=1><R+44><C33>" v-dept-note[25]      
              .
        IF s-prt-ship-split THEN
          /*PUT 
            "<=1><R+33><C32><FROM><C65><LINE><||3>"
            "<=1><R+33><C33>" "SPLIT SHIP/QTY   |         SPLIT ORDER" FORM "x(39)" 
            /*"<=1><R+35><C33>" "LOC1 _________|ORD ________ QTY ________" FORM "x(50)" */
            "<=1><R+34><C33>" lv-split[1] FORM "x(70)"
            "<=1><R+35><C33>" lv-split[2] FORM "x(70)"
            "<=1><R+36><C33>" lv-split[3] FORM "x(70)"
            "<=1><R+37><C33>" lv-split[4] FORM "x(70)" .            
          */
            PUT 
            "<=1><R+32><C66><FROM><C105><LINE><||3>"
            "<=1><R+32><C66>" "SPLIT SHIP/QTY   |         SPLIT ORDER" FORM "x(39)" 
            /*"<=1><R+35><C33>" "LOC1 _________|ORD ________ QTY ________" FORM "x(50)" */
            "<=1><R+33><C66>" lv-split[1] FORM "x(70)"
            "<=1><R+34><C66>" lv-split[2] FORM "x(70)"
            "<=1><R+35><C66>" lv-split[3] FORM "x(70)"
            "<=1><R+36><C66>" lv-split[4] FORM "x(70)" .
        PAGE.


        PUT "<#11><C1><FROM><C105><R+47><RECT><|3>"  
            "<=11><C30><FROM><R+4><C30><LINE><|3>"
            "<=11><C60><FROM><R+4><C60><LINE><|3>"
            "<=11><R+4><C1><FROM><C105><LINE><|3>"
            "<=11>Job # <C30> Estimate # " SKIP
            "<P12><C12>" v-job-prt 
            "<C40>" v-est-no
            "<C60>  FG#: " xeb.stock-no SKIP
            "<C60> Desc: " xeb.part-dscr1 SKIP(4)
            .


        if print-box and avail xest then do:
            /*PAGE. */
                        
            FOR EACH tt-formtext:
                DELETE tt-formtext.
            END.
    
            lv-text = "".
            FOR EACH notes WHERE notes.rec_key = job.rec_key
                         AND (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
                         AND notes.note_code = "BN" NO-LOCK:
                  lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
            END.
            DO li = 1 TO 2:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
            END.
            RUN custom/formtext.p (lv-text).
            ASSIGN
               i = 0
               v-dept-note = "".

            FOR EACH tt-formtext:
               i = i + 1.
               IF  i <= 2 THEN v-dept-note[i] = tt-formtext.tt-text.      
            END.

            PUT UNFORMATTED 
                "<UNITS=INCHES><AT=7.5,.54><FROM><AT=+.4,+1.5><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                (job.job-no) + STRING(job.job-no2,"99") + ">" "<AT=,.6>" 
                    (job-hdr.job-no) "-" STRING(job-hdr.job-no2,"99") 
            /*"<AT=,8.8>" trim(job-hdr.job-no) "-" STRING(job-hdr.job-no2,"99") */
                "<R44><C1><FROM><C105><LINE><|3>" skip
                "<R44><C19><FROM><R48><C19><LINE><|3>"
                "<R44><C20><B>BN Notes:</B><C30>" v-dept-note[1] SKIP
                "<C30>" v-dept-note[2] "<=11><R+4>" SKIP                 
                .
              
            /*run cec/desprntL.p (recid(xef),
                               input-output v-lines,
                               recid(xest)).            */
            v-out1-id = RECID(xeb).
            run cec/desprnL2.p (recid(xef),
                               input-output v-lines,
                               recid(xest)).   
            PAGE.
        end.
        ELSE PAGE.
        /* print fgitem's image */


        IF s-prt-fgimage THEN
        DO:
           IF xest.est-type EQ 6 THEN
           DO:               
              IF b-itemfg.box-image NE "" THEN DO:        
                
                 ls-fgitem-img = b-itemfg.box-image .
              
                 PUT UNFORMATTED "<#12><C1><FROM><C106><R+47><RECT><||3><C80>" v-qa-text SKIP
                     "<=12><R+1><C5>FG Item: " b-itemfg.i-no " " b-itemfg.i-name
                     "<=12><R+3><C1><FROM><C106><LINE><||3>"
                     "<=12><R+5><C5><#21><R+45><C+90><IMAGE#21=" ls-fgitem-img ">" SKIP. 
                 PAGE.
              END. 
             
              IF LAST(w-ef.frm) AND itemfg.box-image NE "" THEN DO:        
                
                 ls-fgitem-img = itemfg.box-image .
              
                 PUT UNFORMATTED "<#12><C1><FROM><C106><R+47><RECT><||3><C80>" v-qa-text SKIP
                     "<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
                     "<=12><R+3><C1><FROM><C106><LINE><||3>"
                     "<=12><R+5><C5><#21><R+45><C+90><IMAGE#21=" ls-fgitem-img ">" SKIP. 
                 PAGE.
              END.
           END.
           ELSE
           DO:                
              ls-fgitem-img = itemfg.box-image .              
           
              PUT UNFORMATTED "<#12><C1><FROM><C106><R+47><RECT><||3><C80>" v-qa-text SKIP
                  "<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
                  "<=12><R+3><C1><FROM><C106><LINE><||3>"
                  "<=12><R+5><C5><#21><R+45><C+90><IMAGE#21=" ls-fgitem-img ">" SKIP. 
              PAGE.
           END.
        END.
       end.  /* for each w-ef */   
        
       IF s-prt-set-header AND last-of(job.job-no2) /*AND est.est-type = 6*/ THEN DO: /* print set header */  
          

          i = 0.
          FOR EACH bf-eb WHERE bf-eb.company = est.company
                            AND bf-eb.est-no = est.est-no
                            AND bf-eb.form-no > 0 NO-LOCK:
               i = i + 1.
          END.
          FOR EACH xeb NO-LOCK 
                 WHERE xeb.company = est.company
                   AND xeb.est-no = est.est-no
                   AND xeb.form-no = 0 
                 BY xeb.stock-no:

              FIND first b-itemfg
                where b-itemfg.company                eq xeb.company
                and b-itemfg.i-no                   eq xeb.stock-no
                NO-LOCK NO-ERROR.

            find first xstyle
                where xstyle.company eq xeb.company
                and xstyle.style   eq b-itemfg.style
                no-lock no-error.
            ASSIGN v-i-line[2] = "Style: " + if avail xstyle then xstyle.dscr else ""
                   v-i-line[3] = "Size: "  + if avail xeb    then
                       trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                       trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                       trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
                   /* vDesignerName = "Designer: " + xeb.spare-char-1*/ .

          END.
          /*IF i > 1 THEN DO:              */
          IF i GE 1 THEN DO:
             DEF VAR v-set-qty AS INT NO-UNDO.
             DEF VAR v-ord-qty AS INT NO-UNDO.
             DEF VAR v-over-run AS cha NO-UNDO.
             DEF VAR v-under-run AS cha NO-UNDO.
             DEF VAR v-fg-set AS cha FORM "x(15)" NO-UNDO.
             IF job-hdr.ord-no > 0 THEN
                 FIND FIRST xoe-ordl WHERE xoe-ordl.company = est.company
                                       AND xoe-ordl.i-no    = job-hdr.i-no
                                       AND xoe-ordl.ord-no  = job-hdr.ord-no
                                     NO-LOCK NO-ERROR.

             ASSIGN
             v-fg-set = job-hdr.i-no
             v-set-qty = if /* avail xeb and xeb.*/ est.est-type eq 6 THEN
                           if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty
                         ELSE 0
             v-ord-qty = (if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty) *
                         if est.form-qty le 1 then 1 else v-pqty
             v-over-run = IF avail xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%"))
                          ELSE IF avail xoe-ord THEN trim(string(xoe-ord.over-pct,">>9.99%")) ELSE ""
             v-under-run = IF avail xoe-ordl THEN trim(STRING(xoe-ordl.under-pct,">>9.99%"))
                           ELSE IF avail xoe-ord THEN trim(STRING(xoe-ord.under-pct,">>9.99%")) ELSE "".
             IF AVAIL xoe-ordl THEN 
                 v-po-no = xoe-ordl.po-no. 
                 

             IF v-set-qty = 0 THEN
                 v-set-qty = v-ord-qty.
             v-max-qty = /*v-ord-qty */ v-set-qty  * (1 + IF AVAIL xoe-ordl THEN xoe-ordl.over-pct / 100 
                                           ELSE IF AVAIL xoe-ord  THEN xoe-ord.over-pct / 100 
                                           ELSE 0 ). 
            ASSIGN vProjectManager = "Project Manager: " + IF AVAIL xoe-ord THEN xoe-ord.USER-ID ELSE job.USER-ID.
            PUT "<R1><C1><#1>"   /*SKIP         */
                 "<C1><R+5><C+40>" "<IMAGE#2=" + ls-full-img1 FORM "x(200)"  /*SKIP                  */
                 /*"<P10><=1><R+8>"*/ .
             
             PUT "<R1><C1><#15><C35><P16><B> BILL OF MATERIALS<P7>" SKIP(4)
                 "<P13><B><C2>" v-cus[1] "<C44>Our Order #: " v-ord-no "</B>"
                 "<P12><B><C75>" lv-fg-name FORMAT "x(30)" SKIP "</B>"
                 "<P13><B><C2>Job #: " v-job-prt  "<C47>Our Date: " v-ord-date
                 "<P12><B><C75>" lv-dscr1 FORMAT "x(30)" SKIP "</B>"  
                 /*"<P7></B><C68>Our Date: " v-ord-date SKIP*/
                /* "<P12><B><C68>" lv-dscr2 FORMAT "x(30)" AT 70  "<P9></B>" */
                 "<P13><B><C2>" "Est #: " v-est-no "<C23>FG #: " v-fg-set 
                /* "<P10><B>Our Date: " AT 65 v-ord-date */
                 "<C47>Due Date: " v-due-date "<P11></B>"
                 "<P12><B><C75>" lv-dscr2 FORMAT "x(30)" "<P9></B>" 
                 "<=1><R+9><C2><From><R+5><C108><RECT><||3>" SKIP
                 "<=1><R+9><C2><B><P10>CUSTOMER INFORMATION <C27> ORDER INFORMATION <C77>ITEM DESCRIPTION" SKIP
                 v-cus[1] AT 3 "<C25>" "    PO#:" v-po-no "Set Qty: " STRING(v-set-qty,">>>>>>>")
                 " Max Qty: " STRING(v-max-qty,">>>>,>>9")
                 /*v-i-line[2] AT 102 /* style */ */
                 vDesignerName AT 102
                 SKIP
                 v-cus[2] AT 3 "<C28>"  
                 /*" Job Qty:" trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
                 " Order Qty:" string(v-ord-qty) format "x(7)"*/
                 /*v-i-line[3] AT 98 /*size*/  */
                 vProjectManager AT 98 SKIP
                 v-cus[3] AT 3 "<C28>" " Project #:" lv-part-no
                 /*v-i-line[4] AT 90 SKIP */
                 v-cus[4]  AT 3 "<C32>" " Overrun:"  format "x(9)" lv-over-run
                 " Underrun:" format "x(10)" lv-under-run 
                 v-i-line[5] AT 93 SKIP /* order type orig repeat*/
                 /*SPACE(1) "Adders:" v-adders FORM "x(36)" */
                 "<=1><R+14><C30><P10><B>Set Components <P10><B><C50>Set item: " v-fg-set SKIP
                 "<C2>FINISHED GOOD#  CUSTOMER PART# DESCRIPTION               QTY PER SET       CAD#           STYLE                   MACHINE" SKIP.
             /* each components */                                                    
             
             DEF VAR v-shipto AS cha NO-UNDO.
        
             v-tmp-line = 0.
             FOR EACH xeb NO-LOCK 
                 WHERE xeb.company = est.company
                   AND xeb.est-no = est.est-no
                   AND xeb.form-no > 0 
                 /*BY xeb.stock-no*/
                   BY xeb.form-no                           /*Task 03191408*/
                   BY xeb.blank-no:
                 IF xeb.procat BEGINS "Y" THEN NEXT.

                 /* gdm - 11030802 */
                 IF s-sample-required = TRUE THEN DO:
                   
                   ASSIGN v-sampreq = 0.
                 
                   FIND FIRST b-ef NO-LOCK 
                       WHERE b-ef.company EQ xeb.company
                         AND b-ef.est-no  EQ xeb.est-no 
                         AND b-ef.eqty    EQ xeb.eqty   
                         AND b-ef.form-no EQ xeb.form-no NO-ERROR.
                   IF AVAIL b-ef THEN
                       FIND FIRST reftable NO-LOCK
                          WHERE reftable.reftable = "cecrep/d-hughes.w"
                            AND reftable.company  = xeb.company
                            AND reftable.loc      = TRIM(job-hdr.job-no)
                            AND reftable.code     = STRING(job-hdr.job-no2,"9999999999")
                            AND reftable.code2    = string(b-ef.form-no,"999")  /*form-no*/
                            AND reftable.val[1]   = xeb.blank-no NO-ERROR. /*blank-no*/
                       IF AVAILABLE reftable 
                         THEN
                          ASSIGN v-sampreq = reftable.val[2].
                         ELSE 
                          ASSIGN v-sampreq = 0.
                 END.

                 PUT xeb.stock-no   AT 3  FORMAT "x(15)"
                     xeb.part-no AT 18 FORMAT "x(15)"
                     xeb.part-dscr1 AT 33 FORMAT "x(25)"
                     /*v-sampreq      AT 44*/.

                 IF xeb.quantityPerSet LT 0 THEN
                   PUT -1 / xeb.quantityPerSet FORMAT ">>>>9.9<<<" AT 58.
                 ELSE
                   PUT xeb.quantityPerSet FORMAT ">>>>>>>9.9<<<" AT 58.

                 FIND FIRST xstyle NO-LOCK
                    WHERE xstyle.company  EQ xeb.company
                      AND xstyle.style    EQ xeb.style
                      AND xstyle.industry EQ "2" NO-ERROR.

                 PUT 
                    /*xeb.die-no FORMAT "x(10)" AT 77*/
                    xeb.cad-no FORMAT "x(10)" AT 77
                    if avail xstyle then xstyle.dscr else "" FORMAT "x(24)" AT 92.
                 
                 FIND FIRST est-op WHERE est-op.est-no = xeb.est-no 
                    AND est-op.dept = "DC" 
                    AND est-op.company = cocode 
                    AND est-op.s-num = xeb.form-no
                     NO-LOCK NO-ERROR.
                 IF NOT AVAIL est-op THEN
                     FIND FIRST est-op WHERE est-op.est-no = xeb.est-no 
                        AND est-op.company = cocode 
                        AND est-op.s-num = xeb.form-no
                     NO-LOCK NO-ERROR.
                 IF AVAIL est-op THEN
                    PUT est-op.m-code AT 116.

                 PUT SKIP.
                 v-tmp-line = v-tmp-line + 1. 
                 IF v-tmp-line + 75 > PAGE-SIZE THEN DO:
                   PUT skip(3) "(continued on the next page)" AT 53.
                   PUT "<=1><R+15><C2><FROM><R+" + string(v-tmp-line + 7) + "><C108><RECT><||3>" FORM "x(150)" SKIP.
                   PAGE. 
                   v-tmp-line = 0.
                   
                   PUT "<R1><C1><#1>"   
                        "<C1><R+6><C+40>" "<IMAGE#2=" + ls-full-img1 FORM "x(200)" .
                   PUT "<R1><C1><#15><C35><P16><B> BILL OF MATERIALS<P7>" SKIP(4)
                         "<P13><B><C2>" v-cus[1] "<C44>Our Order #: " v-ord-no "</B>"
                        "<P12><B><C75>" lv-fg-name FORMAT "x(30)" SKIP "</B>"
                        "<P13><B><C2>Job #: " v-job-prt  "<C47>Our Date: " v-ord-date
                        "<P12><B><C75>" lv-dscr1 FORMAT "x(30)" SKIP "</B>"  
                        /*"<P7></B><C68>Our Date: " v-ord-date SKIP*/
                        /* "<P12><B><C68>" lv-dscr2 FORMAT "x(30)" AT 70  "<P9></B>" */
                         "<P13><B><C2>" "Est #: " v-est-no "<C23>FG #: " v-fg-set 
                        /* "<P10><B>Our Date: " AT 65 v-ord-date */
                        "<C47>Due Date: " v-due-date "<P11></B>"
                        "<P12><B><C75>" lv-dscr2 FORMAT "x(30)" "<P9></B>"  
                       "<=1><R+9><C2><From><R+5><C108><RECT><||3>" SKIP
                       "<=1><R+9><C2><B><P10>CUSTOMER INFORMATION <C27> ORDER INFORMATION <C77>ITEM DESCRIPTION" SKIP
                       v-cus[1] AT 3 "<C25>" "    PO#:" v-po-no "Set Qty: " STRING(v-set-qty,">>>>>>>")
                       " Max Qty: " STRING(v-max-qty,">>>>,>>9")
                       /*v-i-line[2] AT 102 /* style */ */
                       vDesignerName AT 102
                       SKIP
                       v-cus[2] AT 3 "<C28>"  
                       /*" Job Qty:" trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
                       " Order Qty:" string(v-ord-qty) format "x(7)"*/
                       /*v-i-line[3] AT 98 /*size*/  */
                       vProjectManager AT 98 SKIP
                       v-cus[3] AT 3 "<C28>" " Project #:" lv-part-no
                       /*v-i-line[4] AT 90 SKIP */
                       v-cus[4]  AT 3 "<C32>" " Overrun:"  format "x(9)" lv-over-run
                       " Underrun:" format "x(10)" lv-under-run 
                       v-i-line[5] AT 93 SKIP /* order type orig repeat*/
                       /*SPACE(1) "Adders:" v-adders FORM "x(36)" */
                       "<=1><R+14><C30><P10><B>Set Components <P10><B><C50>Set item: " v-fg-set SKIP
                       "<C2>FINISHED GOOD#  CUSTOMER PART# DESCRIPTION               QTY PER SET       CAD#           STYLE                   MACHINE" SKIP.
                 END.
                 
             END.
             v-tmp-line = v-tmp-line + 1.
             /*=======
             /* print raw materials from misc/farm of Est */ 
             FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                                AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
             DO i = 1 TO 8:
                IF b-ef.spec-no[i] <> "" THEN DO:
                   RUN custom/extradec.p (.0001, b-ef.spec-qty[i],
                                          OUTPUT lv-spec-qty[i]). 
                   PUT b-ef.spec-dscr[i] AT 32 space(16) lv-spec-qty[i] SKIP.
                   v-tmp-line = v-tmp-line + 1.
                END.
             END.
             ===*/

             PUT "<=1><R+15><C2><FROM><R+" + string(v-tmp-line) + "><C108><RECT><||3>" FORM "x(150)" SKIP.
             v-tmp-line = v-tmp-line + 10.
        
             /*
             i = 0.
             for each tt-wm WHERE lookup(tt-wm.m-code,tspostfg-char) > 0:
                  i = i + 1.
             END.
             i = i + 2.
             */
             /*PUT "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
                 "<=1><R+" + string(v-tmp-line + 1) + "><C2><FROM><R+" + string(i) + "><C78><RECT><||3>" FORM "x(150)" SKIP
                 "<=1><R+" + string(v-tmp-line + 1) + ">" FORM "x(20)".
                 .
        
             i = 0.
             for each tt-wm WHERE lookup(tt-wm.m-code,tspostfg-char) > 0  by tt-wm.dseq:
               i = i + 1.
               display tt-wm.dscr AT 3
                    tt-wm.s-hr when tt-wm.s-hr ne 0
                    fill("_",7)  format "x(7)"    to 38   when tt-wm.dscr ne ""
                    fill("_",7)  format "x(7)"    to 46   when tt-wm.dscr ne ""
                    fill("_",7)  format "x(7)"    to 54   when tt-wm.dscr ne ""
                    space(2)
                    tt-wm.r-sp when tt-wm.r-sp ne 0
                    fill("_",7)  format "x(7)"    to 69   when tt-wm.dscr ne ""
                    fill("_",7)  format "x(7)"    to 77   when tt-wm.dscr ne ""
                    fill("_",7)  format "x(7)"    to 85   when tt-wm.dscr ne ""
                    fill("_",8)  format "x(8)"    to 99   when tt-wm.dscr ne ""
                    fill("_",8)  format "x(8)"    to 108  when tt-wm.dscr ne ""
                    fill("_",8)  format "x(8)"    to 117  when tt-wm.dscr ne ""
                    fill("_",8)  format "x(8)"    to 129  when tt-wm.dscr ne ""
                    /*chr(124) format "x"           at 131   */                  
                    with no-box no-labels frame o21 width 132 no-attr-space down STREAM-IO.
             end.
             FOR EACH tt-wm:
                 DELETE tt-wm.
             END.
             ASSIGN
             v-tmp-line = v-tmp-line + 3 + i /* 4 and add machine routing lines */
        
             v-shipto = IF AVAIL xoe-rel THEN xoe-rel.ship-id 
                        ELSE IF avail xeb THEN xeb.ship-id
                        ELSE IF avail xoe-ord THEN xoe-ord.sold-id 
                        ELSE "".
             FIND FIRST tt-prem WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
                                   AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
             IF NOT AVAIL tt-prem THEN do:
                FIND FIRST bf-eb WHERE bf-eb.company = est.company
                                   AND bf-eb.est-no = est.est-no
                                   AND bf-eb.form-no = 0
                                   AND bf-eb.blank-no = 0 NO-LOCK NO-ERROR.
                CREATE tt-prem.
                IF AVAIL bf-eb THEN 
                   ASSIGN tt-prem.tt-#-bundle = string(bf-eb.cas-cnt)
                          tt-prem.tt-#-unit = string(bf-eb.tr-cnt)
                          tt-prem.tt-pattern = bf-eb.tr-no
                          tt-prem.tt-pallet = bf-eb.cas-no.
             END.
             */
         


             FOR EACH tt-formtext:
                DELETE tt-formtext.
             END.    
             lv-text = "".
             FOR EACH notes NO-LOCK WHERE notes.rec_key = job.rec_key
                                      AND notes.note_code = "BM":
                 lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
             END.
             FOR EACH notes NO-LOCK WHERE notes.rec_key = itemfg.rec_key
                                      AND notes.note_code = "BM":
                 lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
             END.
             DO li = 1 TO 100:
                CREATE tt-formtext.
                ASSIGN
                   tt-line-no = li
                   tt-length  = 72.
             END.
             RUN custom/formtext.p (lv-text).
             i = 0.
             v-dept-inst = "".
             FOR EACH tt-formtext:
                i = i + 1.
                /* was 15 */
                IF  i <= 100 THEN v-dept-inst[i] = tt-formtext.tt-text.      
             END.
             /*IF v-ship <> "" THEN v-dept-inst[15] = v-ship.  /* shipto notes */*/

             DO i = 100 TO 1 BY -1:
                IF v-dept-inst[i] = "" THEN NEXT.
                v-last-note-line = i.
                LEAVE.
             END.
             /*MESSAGE "last note line: " v-last-note-line " max:" v-max-note-line  " tmp:" v-tmp-line
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             */

             PUT "<=1><R+" + string(v-tmp-line) + ">" form "X(20)" .
             v-tmp-line = v-tmp-line + 1.
             
             IF v-max-note-line - v-tmp-line < 4 THEN DO:
                PAGE.
                v-tmp-line = 15.
                /* put header */
                PUT "<R1><C1><#1>"   "<C1><R+6><C+40>" "<IMAGE#2=" + ls-full-img1 FORM "x(200)" .
                PUT "<R1><C1><#15><C35><P16><B> BILL OF MATERIALS<P7>" SKIP(4)
                        "<P13><B><C2>" v-cus[1] "<C44>Our Order #: " v-ord-no "</B>"
                        "<P12><B><C75>" lv-fg-name FORMAT "x(30)" SKIP "</B>"
                        "<P13><B><C2>Job #: " v-job-prt  "<C47>Our Date: " v-ord-date
                        "<P12><B><C75>" lv-dscr1 FORMAT "x(30)" SKIP "</B>"  
                         /*"<P7></B><C68>Our Date: " v-ord-date SKIP*/
                        /* "<P12><B><C68>" lv-dscr2 FORMAT "x(30)" AT 70  "<P9></B>" */
                        "<P13><B><C2>" "Est #: " v-est-no "<C23>FG #: " v-fg-set 
                        /* "<P10><B>Our Date: " AT 65 v-ord-date */
                         "<C47>Due Date: " v-due-date "<P11></B>"
                        "<P12><B><C75>" lv-dscr2 FORMAT "x(30)" "<P9></B>" 
                       "<=1><R+9><C2><From><R+5><C108><RECT><||3>" SKIP
                       "<=1><R+9><C2><B><P10>CUSTOMER INFORMATION <C27> ORDER INFORMATION <C77>ITEM DESCRIPTION" SKIP
                       v-cus[1] AT 3 "<C25>" "    PO#:" v-po-no "Set Qty: " STRING(v-set-qty,">>>>>>>")
                       " Max Qty: " STRING(v-max-qty,">>>>,>>9")
                       /*v-i-line[2] AT 102 /* style */ */
                       vDesignerName AT 102
                       SKIP
                       v-cus[2] AT 3 "<C28>"  
                       /*" Job Qty:" trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
                       " Order Qty:" string(v-ord-qty) format "x(7)"*/
                       /*v-i-line[3] AT 98 /*size*/  */
                       vProjectManager AT 98 SKIP
                       v-cus[3] AT 3 "<C28>" " Project #:" lv-part-no
                       /*v-i-line[4] AT 90 SKIP */
                       v-cus[4]  AT 3 "<C32>" " Overrun:"  format "x(9)" lv-over-run
                       " Underrun:" format "x(10)" lv-under-run 
                       v-i-line[5] AT 93 SKIP /* order type orig repeat*/
                 "<=1><R+" + string(v-tmp-line + 5) + "><C2><FROM><R+" + string(v-max-note-line - v-tmp-line) + "><C108><RECT><||3>" FORM "x(150)" SKIP        
                 "<=1><R+" + string(v-tmp-line + 5) + "><C2>Special instructions  <C73>SHIPPING INFO Ship to: " + v-shipto FORM "x(250)" SKIP
                .
             END.
             ELSE
                 PUT                                                     /* was <R+15> */
                 "<=1><R+" + string(v-tmp-line + 5) + "><C2><FROM><R+" + string(v-max-note-line - v-tmp-line) + "><C108><RECT><||3>" FORM "x(150)" SKIP        
                 "<=1><R+" + string(v-tmp-line + 5) + "><C2>Special instructions  <C73>SHIPPING INFO Ship to: " + v-shipto FORM "x(250)" SKIP
                 .

             ASSIGN iNotePage = 1
                    iNotePrtLine = v-max-note-line - v-tmp-line - 2.
             IF v-last-note-line > v-max-note-line - v-tmp-line - 1 THEN  /* more than one page */
             DO i = 1 TO v-last-note-line:                
                IF i > iNotePrtLine THEN DO:  /* 1st page - 23 2nd - 53, 3rd 83...*/
                   PUT "(continued on the next page)" AT 53 /*i iNotePrtLine*/. 
                   PAGE.
                   iNotePrtLine = 23 + (30 * iNotePage).  /* 30 lines per page + 2 blanks */
                   iNotePage = iNotePage + 1.
                   PUT "<R1><C1><#1>"   "<C1><R+6><C+40>" "<IMAGE#2=" + ls-full-img1 FORM "x(200)" .
                   PUT "<R1><C1><#15><C35><P16><B> BILL OF MATERIALS<P7>" SKIP(4)
                        "<P13><B><C2>" v-cus[1] "<C44>Our Order #: " v-ord-no "</B>"
                        "<P12><B><C75>" lv-fg-name FORMAT "x(30)" SKIP "</B>"
                        "<P13><B><C2>Job #: " v-job-prt  "<C47>Our Date: " v-ord-date
                        "<P12><B><C75>" lv-dscr1 FORMAT "x(30)" SKIP "</B>"  
                        /*"<P7></B><C68>Our Date: " v-ord-date SKIP*/
                        /* "<P12><B><C68>" lv-dscr2 FORMAT "x(30)" AT 70  "<P9></B>" */
                         "<P13><B><C2>" "Est #: " v-est-no "<C23>FG #: " v-fg-set 
                        /* "<P10><B>Our Date: " AT 65 v-ord-date */
                        "<C47>Due Date: " v-due-date "<P11></B>"
                        "<P12><B><C75>" lv-dscr2 FORMAT "x(30)" "<P9></B>" 
                       "<=1><R+9><C2><From><R+5><C108><RECT><||3>" SKIP
                       "<=1><R+9><C2><B><P10>CUSTOMER INFORMATION <C27> ORDER INFORMATION <C77>ITEM DESCRIPTION" SKIP
                       v-cus[1] AT 3 "<C25>" "    PO#:" v-po-no "Set Qty: " STRING(v-set-qty,">>>>>>>")
                       " Max Qty: " STRING(v-max-qty,">>>>,>>9")
                       /*v-i-line[2] AT 102 /* style */ */
                       vDesignerName AT 102
                       SKIP
                       v-cus[2] AT 3 "<C28>"  
                       /*" Job Qty:" trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
                       " Order Qty:" string(v-ord-qty) format "x(7)"*/
                       /*v-i-line[3] AT 98 /*size*/  */
                       vProjectManager AT 98 SKIP
                       v-cus[3] AT 3 "<C28>" " Project #:" lv-part-no
                       /*v-i-line[4] AT 90 SKIP */
                       v-cus[4]  AT 3 "<C32>" " Overrun:"  format "x(9)" lv-over-run
                       " Underrun:" format "x(10)" lv-under-run 
                       v-i-line[5] AT 93 SKIP /* order type orig repeat*/
                      "<=1><R+15><C2><FROM><R+" + string(v-max-note-line - 10) + "><C108><RECT><||3>" FORM "x(150)" SKIP        
                      "<=1><R+15><C2>Special instructions"   SKIP
                      .
                END.
                IF i <= 4 THEN PUT v-dept-inst[i] AT 5 FORM "x(90)" chr(124) format "xx" v-shp[i] SKIP .
                ELSE PUT v-dept-inst[i] AT 5 FORM "x(90)" /*i iNotePrtLine */ SKIP .
             END.
             ELSE IF v-last-note-line >= 4 THEN DO i = 1 TO v-last-note-line:  /* single page note */
                PUT v-dept-inst[i] AT 5 FORM "x(90)" .
                IF i <= 4 THEN PUT chr(124) format "xx" v-shp[i] SKIP.
                ELSE PUT SKIP.                
             END.
             ELSE DO i = 1 TO 4:
                IF i <= v-last-note-line THEN DO:
                    PUT v-dept-inst[i] AT 5 FORM "x(90)" .
                    PUT chr(124) format "xx" v-shp[i] SKIP.
                END.
                ELSE
                    PUT chr(124) format "xx" AT 95 v-shp[i].
             END.
             /*
             PUT 
                 v-dept-inst[1] AT 5 FORM "x(90)" chr(124) format "xx" v-shp[1] SKIP
                 v-dept-inst[2] AT 5 FORM "x(90)" chr(124) format "xx" v-shp[2] SKIP
                 v-dept-inst[3] AT 5 FORM "x(90)" chr(124) format "xx" v-shp[3] SKIP
                 v-dept-inst[4] AT 5 FORM "x(90)" chr(124) format "xx" v-shp[4] SKIP
                 v-dept-inst[5] AT 5 FORM "x(90)" chr(124) format "xx" "Item PO #:" xoe-ordl.po-no SKIP
                 v-dept-inst[6] AT 5 SKIP
                 v-dept-inst[7] AT 5 SKIP
                 v-dept-inst[8] AT 5 SKIP
                 v-dept-inst[9] AT 5 SKIP
                 v-dept-inst[10] AT 5 SKIP
                 v-dept-inst[11] AT 5 SKIP
                 v-dept-inst[12] AT 5 SKIP
                 v-dept-inst[13] AT 5 SKIP
                 v-dept-inst[14] AT 5 SKIP
                 v-dept-inst[15] AT 5 
                 .
             PAGE.
             */
          END. /* i > 1*/
          
       END. /* set header printing  est.est-type = 6 */
       PAGE.
    end.  /* each job */

    end.  /* end v-local-loop  */
 
        hide all no-pause.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
