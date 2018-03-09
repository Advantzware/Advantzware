/* ---------------------------------------------- */
/*  cecrep/jobfibre.p  factory ticket  for Fibre landscape */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF INPUT PARAMETER ip-loop-num AS INT NO-UNDO.
DEF INPUT PARAMETER ip-copies AS INT NO-UNDO.
DEF INPUT PARAMETER ip-unprinted-approved AS LOG NO-UNDO.

DEF SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF VAR v-start-compress AS cha NO-UNDO.
DEF VAR v-end-compress AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ink-1 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-2 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-3 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-4 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-5 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-6 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-7 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-8 AS cha FORM "X(30)" NO-UNDO.
DEF var v-dept-note AS cha FORM "x(48)" EXTENT 50 NO-UNDO.
DEF var v-spec-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF VAR v-deptnote AS cha NO-UNDO.
DEF VAR v-dept-length AS DEC NO-UNDO.
DEF VAR lv-under-run AS cha NO-UNDO.
DEF VAR lv-over-run AS cha NO-UNDO.
DEF VAR lv-part-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-fg-name AS cha NO-UNDO.
DEF VAR lv-status AS cha FORM "x(20)" NO-UNDO.
DEF VAR lv-sts-code AS cha INIT "O,R,C,T,N,X,Q" NO-UNDO.
DEF VAR lv-sts-desc AS cha INIT "O-Original,R-Repeat,C-Change,T-Transfer,N-New Customers,X-Complete Re-run,Q-Quality/Re-work" NO-UNDO.

DEF VAR v-sman AS cha FORM "x(25)" NO-UNDO.
DEF VAR v-blk-per-frm AS cha FORM "x(15)" NO-UNDO.
DEF VAR ls-fgitem-img AS cha FORM "x(50)" NO-UNDO.
DEF VAR lv-ord-po LIKE oe-ord.po-no NO-UNDO.
DEF VAR lv-part-no AS cha FORM "x(15)" NO-UNDO.
DEF VAR lv-cust-set AS cha FORM "x(15)" NO-UNDO.
DEF VAR lv-cas-cnt AS INT FORM ">>>>>>" NO-UNDO.
DEF VAR v-height AS CHAR FORMAT "X(14)" NO-UNDO.
def var v-spec-list as char format "x(20)"init "QA" no-undo.
DEF VAR v-count AS INT NO-UNDO.

DEF VAR v-set-qty AS INT NO-UNDO.
DEF VAR v-ord-qty AS INT NO-UNDO.
DEF VAR v-over-run AS cha NO-UNDO.
DEF VAR v-under-run AS cha NO-UNDO.
DEF VAR v-fg-set AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-see-1st-blank AS LOG NO-UNDO.
DEF VAR v-shipto AS cha NO-UNDO.

DEF NEW SHARED VAR v-adder-1 AS CHAR FORMAT "X(10)" NO-UNDO.
DEF NEW SHARED VAR v-adder-2 AS CHAR FORMAT "X(10)" NO-UNDO.
DEF NEW SHARED VAR v-adder-3 AS CHAR FORMAT "X(10)" NO-UNDO.
DEF NEW SHARED VAR v-adder-4 AS CHAR FORMAT "X(10)" NO-UNDO.
DEF NEW SHARED VAR v-adder-5 AS CHAR FORMAT "X(10)" NO-UNDO.
DEF NEW SHARED VAR v-adder-6 AS CHAR FORMAT "X(10)" NO-UNDO.

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-job-hdr FOR job-hdr.

RUN sys/ref/ordtypes.p (OUTPUT lv-sts-code, OUTPUT lv-sts-desc).

{jcrep/r-ticket.i "shared"}

{cecrep/jobfibre.i "new shared"}
{cecrep/jc-fibre.i }
/*{cecrep/tt-artios.i}*/

DEF WORK-TABLE w-i2 LIKE w-i FIELD i-ext AS INT.
DEF BUFFER b-w-i2 FOR w-i2.
DEF VAR ld-i-qty LIKE w-i2.i-qty NO-UNDO.

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}

def new shared var v-out1-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def new shared var v-out2-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
 
def var laser           as   log    init no     format "Y/N"            no-undo.
def var v-vend-no       like oe-ordl.vend-no                            no-undo.
def var v-po-no         like oe-ordl.po-no-po                           no-undo.
def var v-qty-or-sup    as   char               format "x(38)"          no-undo.
def var v-i-line        as   char   extent 4    format "x(38)"          no-undo.
def var v-flag          as   log    init no                             no-undo.
def var v-print-score   as   log    init yes                            no-undo.
def var v-pqty          as   dec                                        no-undo.

DEF VAR lv-rt-num AS INT NO-UNDO.
def stream ctl.
DEF VAR lv-add-entry AS INT NO-UNDO.
DEF VAR v-loop-cnt AS INT NO-UNDO.
DEF VAR v-note-cnt AS INT NO-UNDO.
DEF VAR v-note-length AS INT NO-UNDO.
DEF VAR v-die-loc AS cha FORM "x(15)" NO-UNDO.

DEF VAR v-prev-ext-gap AS INT NO-UNDO.
DEF VAR v-coldscr LIKE eb.i-coldscr NO-UNDO.

DEF VAR v-oecount AS LOG NO-UNDO.
DEF VAR v-cont-string AS cha NO-UNDO.  
DEF VAR v-prev-k AS INT NO-UNDO.
DEF VAR v-tmp-note-length AS INT NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-cas-desc AS cha NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF SHARED VAR s-prt-ship-split AS LOG NO-UNDO.
DEF BUFFER b-eb FOR eb.
DEF VAR lv-spattern-img AS cha FORM "x(50)" NO-UNDO.
DEF VAR lv-split AS cha FORM "x(60)" EXTENT 4 NO-UNDO.
DEF VAR lv-au AS cha FORM "x(20)" NO-UNDO.
DEF VAR lv-est-type AS cha FORM "x(35)" NO-UNDO.
DEF VAR v-qa-text AS cha FORM "x(30)" INIT "6/05 Job Ticket QF-119 Rev.A" NO-UNDO.
DEF VAR v-start-note AS INT NO-UNDO.
DEF VAR v-end-note AS INT NO-UNDO.

{custom/formtext.i NEW}
{sys/inc/notes.i}
DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.
{cecrep/jc-prem.i}
{custom/notesdef.i}
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 15 NO-UNDO.
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR v-tmp-line AS INT NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER b-ef FOR ef.
DEF workfile tt-wm LIKE w-m.
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEF SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF VAR v-managed-order AS cha FORM "x(30)" NO-UNDO.
DEF VAR ll-tandem AS LOG NO-UNDO.
DEF VAR ll-jqcust AS LOG NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "JOBQTYCUST"
    NO-ERROR.
IF AVAIL sys-ctrl THEN
   ll-jqcust = sys-ctrl.log-fld.

/* gdm - */

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
 v-spec-list = spec-list.

/*do v-local-loop = 1 to 1:*/
   for each job-hdr NO-LOCK
        where job-hdr.company               eq cocode
          and (production OR
               job-hdr.ftick-prnt           eq reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no
        USE-INDEX job-no,

        first job
        where job.company                   eq cocode
          and job.job                       eq job-hdr.job
          and job.job-no                    eq job-hdr.job-no
          and job.job-no2                   eq job-hdr.job-no2
          and job.stat                      ne "H"
          AND (job.pr-printed EQ reprint OR NOT production)
          AND (ip-unprinted-approved EQ NO OR
               (ip-unprinted-approved AND job.pr-printed = NO AND
                job.opened = YES AND job.cs-to-pr = YES)) 
        USE-INDEX job NO-LOCK,
        
        first est
        where est.company = job.company
          AND est.est-no                    eq job.est-no
          and est.est-type                  gt 4
        USE-INDEX est-no2 NO-LOCK,

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
      
       IF s-prt-ship-split THEN
       DO:
          FIND FIRST b-job-hdr where ROWID(b-job-hdr) = ROWID(job-hdr) EXCLUSIVE-LOCK NO-ERROR.
             ASSIGN b-job-hdr.splitShip = YES.
          Release b-job-hdr. 
       END.

       /* get whether warehous item or not */
       find first oe-ordl where oe-ordl.company eq job-hdr.company
           and oe-ordl.ord-no  eq job-hdr.ord-no
           and oe-ordl.job-no  eq job-hdr.job-no
           and oe-ordl.job-no2 eq job-hdr.job-no2
           and oe-ordl.i-no    eq job-hdr.i-no no-lock no-error.
       IF job-hdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
          find first oe-ordl where oe-ordl.company eq job-hdr.company
               and oe-ordl.ord-no  eq job-hdr.ord-no
               and oe-ordl.i-no    eq job-hdr.i-no NO-LOCK no-error.
       IF AVAIL oe-ordl THEN  

       ASSIGN
       v-managed-order = IF oe-ordl.managed = true THEN "MANAGED   WAREHOUSE   ORDER"
                         ELSE ""
       v-break = first-of(job.job-no2).

      release xest.
      release xef.
      release xeb.
      release xoe-ord.
      release xoe-ordl.
      release xoe-rel.

      run cecrep/jobtick1.p (recid(job-hdr), v-format,
                              ip-loop-num, ip-copies).

      for each w-ef WHERE (w-ef.frm = job-hdr.frm OR est.est-type <> 8),
          EACH b-eb NO-LOCK WHERE b-eb.company = job-hdr.company
                              AND b-eb.est-no = job-hdr.est-no 
                              AND b-eb.form-no = w-ef.frm
                              AND (b-eb.blank-no = job-hdr.blank-no OR est.est-type NE 8)
          BREAK BY w-ef.frm BY b-eb.blank-no:
        release xef.
        release xeb.
        release xstyle.
        release xxprep.

        run cecrep/jobtick6.p (recid(w-ef), recid(job-hdr),RECID(b-eb)). /* jobtick2.p*/

        ASSIGN
           v-pqty = 1
           v-cp = ""
           v-sman = "".

        if avail xeb then do:
          RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

          if xeb.stock-no ne "" then v-fg = xeb.stock-no.
          ASSIGN
          v-cp = xeb.part-no
          lv-fg-name = IF AVAIL xoe-ordl AND xoe-ordl.i-no EQ job-hdr.i-no THEN xoe-ordl.i-name ELSE itemfg.i-name.
          {cec/rollfac.i}
          v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                   if xeb.yld-qty lt 0 then (-1 / xeb.yld-qty)
                                       else xeb.yld-qty.
          FIND FIRST sman WHERE sman.company = xeb.company AND
                                sman.sman = xeb.sman NO-LOCK NO-ERROR.
          v-sman = IF AVAIL sman THEN sman.sname ELSE xeb.sman.

          /* Fibre logic to show each ink on estimate - start */
          FOR EACH w-i2:
            DELETE w-i2.
          END.

          DO i = 1 TO EXTENT(xeb.i-code):
            IF xeb.i-code[i] NE "" THEN DO:
              CREATE w-i2.
              ASSIGN
               w-i2.i-code = xeb.i-code[i]
               w-i2.i-dscr = xeb.i-dscr[i]
               w-i2.i-ext  = i.
            END.
          END.

          FOR EACH w-i2,
              FIRST item
              WHERE item.company EQ cocode
                AND item.i-no    EQ w-i2.i-code
              NO-LOCK:
          
            DO i = 1 TO EXTENT(xeb.i-code):
              j = 0.
              IF xeb.i-code[i] EQ w-i2.i-code AND i EQ w-i2.i-ext THEN
              FOR EACH job-mch
                  WHERE job-mch.company EQ job.company
                    AND job-mch.job     EQ job.job
                    AND job-mch.job-no  EQ job.job-no
                    AND job-mch.job-no2 EQ job.job-no2
                    AND job-mch.frm     EQ xef.form-no
                    AND (job-mch.dept   EQ "PR" OR
                         (job-mch.dept  EQ "CT" AND item.mat-type EQ "V"))
                  NO-LOCK,
                  FIRST mach
                  {sys/ref/machW.i}
                    AND mach.m-code EQ job-mch.m-code
                  NO-LOCK
                  BY job-mch.line:
                j = j + 1.
                IF xeb.i-ps[i] EQ j THEN
                  w-i2.i-qty = w-i2.i-qty + mach.col-wastelb +
                               (((xeb.t-sqin - xeb.t-win) *
                                 (job-mch.run-qty * xeb.num-up * v-n-out) *
                                 (xeb.i-%[i] / 100)) / item.yield).
              END.
            END.
          END.

          j = 0.
          wst-ink:
          FOR EACH job-mch
              WHERE job-mch.company EQ job.company
                AND job-mch.job     EQ job.job
                AND job-mch.job-no  EQ job.job-no
                AND job-mch.job-no2 EQ job.job-no2
                AND job-mch.frm     EQ xef.form-no
                AND job-mch.dept    EQ "PR"
              NO-LOCK BY job-mch.line.
            j = j + 1.
            DO i = 1 TO 10:
              IF xeb.i-ps[i] EQ j THEN DO:
                FIND FIRST w-i2 WHERE w-i2.i-code EQ xeb.i-code[i] NO-ERROR.
                IF AVAIL w-i2 THEN DO:
                  FIND FIRST mach
                      {sys/ref/machW.i}
                        AND mach.m-code EQ job-mch.m-code
                      NO-LOCK NO-ERROR.
                  IF AVAIL mach THEN w-i2.i-qty = w-i2.i-qty + mach.ink-waste.
                  NEXT wst-ink.
                END.
              END.
            END.
          END.

          FOR EACH w-i2 BREAK BY w-i2.i-code:
            IF FIRST-OF(w-i2.i-code) THEN ld-i-qty = 0.

            ld-i-qty = ld-i-qty + w-i2.i-qty.

            IF LAST-OF(w-i2.i-code) THEN DO:
              FIND FIRST w-i WHERE w-i.i-code EQ w-i2.i-code NO-ERROR.

              IF AVAIL w-i THEN
              FOR EACH b-w-i2 WHERE b-w-i2.i-code EQ w-i2.i-code:
                b-w-i2.i-qty = w-i.i-qty * (b-w-i2.i-qty / ld-i-qty).
              END.
            END.
          END.
        END.  /* avail xeb*/

        assign
           v-loc     = ""
           v-loc-bin = ""
           lv-over-run =  IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%"))
                          ELSE IF AVAIL xoe-ord THEN trim(string(xoe-ord.over-pct,">>9.99%")) ELSE trim(string(cust.over-pct,">>9.99%"))
           lv-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%"))
                          ELSE IF AVAIL xoe-ord THEN trim(string(xoe-ord.under-pct,">>9.99%")) ELSE trim(string(cust.over-pct,">>9.99%"))
            
           v-blk-per-frm = "  Part " + string(xeb.form-no,"99") + " of " + string(xest.form-qty,"99")     
           lv-ord-po = IF AVAIL xoe-ord THEN xoe-ord.po-no ELSE ""
           lv-status = IF AVAIL xoe-ordl THEN ENTRY(lookup(xoe-ordl.type-code,lv-sts-code),lv-sts-desc) ELSE ""
           lv-part-no = IF AVAIL xoe-ordl AND xoe-ordl.i-no EQ job-hdr.i-no THEN xoe-ordl.part-no 
                        ELSE itemfg.part-no
           lv-cust-set = lv-part-no
           lv-au = IF itemfg.alloc THEN "U" ELSE "A".

       IF est.est-type = 6 THEN
          ASSIGN
            lv-part-name = xeb.part-dscr2
            lv-au = lv-au + "    " + job-hdr.i-no.
       ELSE DO:
         ASSIGN
            lv-part-name = IF AVAIL xoe-ordl THEN xoe-ordl.part-dscr1 ELSE ""  
            lv-au = "".
         RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).
       END.
       ASSIGN
       lv-est-type = IF est.est-type = 5 THEN "SINGLE"
                     ELSE IF est.est-type = 6 THEN "SET"
                     ELSE IF ll-tandem THEN "TANDEM"
                     ELSE "COMBO"
       lv-est-type = lv-est-type + "   FORM " + string(b-eb.form-no) + " OF " + string(xest.form-qty) 
                    + "  BLANK " + STRING(b-eb.blank-no) + " OF " + STRING(xef.blank-qty).


       FIND FIRST stackPattern NO-LOCK WHERE stackPattern.stackCode EQ b-eb.stack-code NO-ERROR.
       IF AVAILABLE stackPattern AND SEARCH(stackPattern.stackImage) NE ? THEN lv-spattern-img =  stackPattern.stackImage.
             
       
       PUT "<P10></PROGRESS>" SKIP "<FCourier New><C2><B>" lv-au "<C33>" lv-est-type "</B>"
       "<P12><B><C90>JOB TICKET" /*AT 140*/  /*caps(SUBSTRING(v-fg,1,1)) FORM "x" AT 40*/       
       SKIP
       "<#1><C1><FROM><C106><R+45><RECT><||3><C80><P10></B>" v-qa-text "<B>"
       "<=1><R-2><C33><B><P12>" v-managed-order "</B>"
       "<=1><C32><FROM><R+45><C32><LINE><||3>"
       "<=1><C55><FROM><R+8><C55><LINE><||3>"
       "<=1><R+20><C66><FROM><R+25><C66><LINE><||3>"
       "<=1><C74><FROM><R+20><C74><LINE><||3>"       
       "<=1><R+5><C74><FROM><C106><LINE><||3>"     /*Order info*/
       "<=1><R+8><C1><FROM><C74><LINE><||3>"     /*sold to*/
       "<=1><R+13><C74><FROM><C106><LINE><||3>"  /* board*/
       "<=1><R+16><C1><FROM><C32><LINE><||3>"   /*Production id*/
       "<=1><R+21><C1><FROM><C32><LINE><||3>"    /*item desc*/
       "<=1><R+20><C66><FROM><C106><LINE><||3>"    /*item desc*/
       "<=1><R+8><C33><P8>                 <U>Dept Notes:</U> "
       "<=1><R+27><C1><FROM><C32><LINE><||3>"   /*packing*/
       "<=1><R+28><C66><FROM><C106><LINE><||3>"   /*Printint*/
       "<=1><R+35><C1><FROM><C32><LINE><||3>"   /*machine routing*/
       "<=1><R+35><C20.8><FROM><R+10><LINE><||3>"
       "<=1><R+40><C66><FROM><C106><LINE><||3>"   /*sheets ordered*/
       "<=1><R+40><C71><P8><U>Sheets Ordered:</U>          <U>Due Date:</U>      <U>Supplier:</U>"
       "<=1><R+40><C86><FROM><R+5><LINE><||3>"   
       "<=1><R+40><C96><FROM><R+5><LINE><||3>"
       "<=1><R+27.2><C23><#22><R+5><C+9.7><IMAGE#22=" lv-spattern-img + ">" FORM "x(70)"  /*Stack pattern image*/ 
       .

       view frame head.  /* factory header display  */  
       
       assign
           v-i-line[1] = "ITEM DESCRIPTION"
           v-i-line[2] = "Style: " + if avail xstyle then xstyle.dscr else ""
           v-i-line[3] = "Size: "  + if avail xeb    then
                     trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
           v-i-line[4] = "Joint: " + if avail xeb then v-joint-dscr else "".
        
       /*===== for xprint */
        RUN sys/ref/getpo#.p (IF AVAIL xoe-ordl AND est.est-type NE 6 THEN ROWID(xoe-ordl) ELSE ROWID(job),
                              w-ef.frm, OUTPUT v-po-no).
        RELEASE po-ord.
        IF v-po-no NE 0 THEN
        FIND FIRST po-ord
            WHERE po-ord.company EQ cocode
              AND po-ord.po-no   EQ v-po-no
            NO-LOCK NO-ERROR.
          
        ASSIGN
         v-vend-no    = IF AVAIL po-ord THEN po-ord.vend-no ELSE ""
         v-qty-or-sup = "Vend: ".

        if v-vend-no ne "" then do:
           v-qty-or-sup = v-qty-or-sup + trim(v-vend-no).
           IF v-po-no ne 0 THEN v-qty-or-sup = v-qty-or-sup + " PO#:" +
                                               trim(string(v-po-no,">>>>>9")).
        end.
        
        i = 0.
        for each w-i:
          i = i + 1.
        end.
        if i le 8 then do i = i + 1 to 8:
          create w-i.
        end.

        v-form-sqft = round(if v-corr then (v-form-len * v-form-wid * .007)
                                       else (v-form-len * v-form-wid / 144),3).
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.die-no
                            no-lock no-error.

        ASSIGN
        v-die-loc = ""
        v-coldscr = IF AVAIL xeb AND xeb.i-coldscr <> "" THEN xeb.i-coldscr ELSE "Plain"
        lv-ord-po = IF AVAIL xoe-ord THEN xoe-ord.po-no ELSE "".

        FIND FIRST b-itemfg WHERE b-itemfg.company = itemfg.company AND b-itemfg.i-no = xeb.stock-no
                NO-LOCK NO-ERROR.
        FIND FIRST notes WHERE notes.rec_key EQ b-itemfg.rec_key 
                           AND notes.note_code = "LOC" NO-LOCK NO-ERROR.
        IF AVAIL notes THEN v-die-loc = REPLACE(SUBSTRING(notes.note_text,1,10),CHR(10),"").

        IF AVAIL b-itemfg THEN lv-fg-name = b-itemfg.i-name.

        v-ord-qty = (if avail xoe-ordl AND xoe-ordl.i-no EQ job-hdr.i-no then xoe-ordl.qty else job-hdr.qty) *
                    (if est.form-qty le 1 then 1 else v-pqty).
        IF ll-jqcust AND NOT AVAIL xoe-ordl AND AVAIL job-hdr THEN
           ASSIGN v-ord-qty = v-ord-qty * ( 1 - (cust.over-pct / 100)).

        v-see-1st-blank = IF NOT FIRST-OF(w-ef.frm) OR
                             CAN-FIND(FIRST bf-eb WHERE bf-eb.company = b-eb.company
                                                    AND bf-eb.est-no = b-eb.est-no
                                                    AND bf-eb.form-no = b-eb.form-no
                                                    AND bf-eb.blank-no < b-eb.blank-no)
                          THEN YES ELSE NO.
                                                
        DISPLAY
            "Order Qty:" + trim(string(v-ord-qty,">>,>>>,>>9")) /*trim(string((if avail xoe-ordl AND xoe-ordl.i-no EQ job-hdr.i-no then xoe-ordl.qty
                                          else job-hdr.qty) *
                                         if est.form-qty le 1 then 1
                                         else v-pqty,">>>,>>9"))*/ FORM "x(20)"
            AT 90 SKIP
            "<P8><U>Shipping Info: <P10></U>" AT 12 
            "<B><P12>Job Qty:" AT 109 trim(string(job-hdr.qty * v-pqty,">>,>>>,>>9")) format "x(10)"
            "</B><P10>"
                SKIP
              "Ship To #:" AT 2
              xoe-ord.sold-id when avail xoe-ord
              xeb.ship-id when avail xeb @ xoe-ord.sold-id
              xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id 
               "Set Qty:" AT 90 
               trim(string(if avail xoe-ordl AND xoe-ordl.i-no EQ job-hdr.i-no then xoe-ordl.qty
                                          else job-hdr.qty,">>,>>>,>>9"))
                        when avail xeb and xeb.est-type eq 6    format "x(12)" 
              SKIP
              v-shp[1] AT 2 
              "Item Name:" AT 90 lv-fg-name FORM "x(26)"
              SKIP
              v-shp[2] AT 2  "Description 1:" AT 90 substring(lv-part-name,1,22) FORM "x(25)" SKIP
              v-shp[3] AT 2  SKIP
              v-shp[4] AT 2  "<U>Board:</U>" AT 106 "<U>Adders:</U>" AT 125  WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              SKIP
              "Cust. PO #:" AT 2  xoe-ordl.po-no when avail xoe-ordl
              "Shts Req'd:" AT 90 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              trim(string(v-sht-qty)) format "x(9)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              "<C97>" v-adder-1 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              SKIP
              "W:" + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))  format "x(22)" AT 90 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              "<C97>" v-adder-2 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              SKIP
               "<P8><U>Production ID:</U><B><P10>" AT 10
               "<C75>Board:" WHEN FIRST-OF(w-ef.frm)
               "<C80>" v-form-code FORM "x(15)" WHEN FIRST-OF(w-ef.frm)
               "SEE 1st BLANK" WHEN v-see-1st-blank @ v-form-code
               "</B><P10><C97>" v-adder-3 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              SKIP
              "Our Order#:"  at 2 v-ord-no   "CAD#:" xeb.cad-no WHEN AVAIL xeb
              "<C74><B>" v-form-dscr WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank FORM "x(30)" 
              "</B><C97><P10>" v-adder-4 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              SKIP
               "<P10>FG#:" AT 2 v-fg FORM "x(15)"  v-blk-per-frm
               "<P10></B>"
               " Sq Ft:" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank AT 103
               trim(string(v-form-sqft)) format "x(7)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
               "MSF:"  + trim(string(v-sht-qty * v-form-sqft / 1000,">>>>9.9<")) format "x(11)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
               "<C97>" v-adder-5 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              SKIP
               "<P12><B>CustPart#:" AT 2 v-cp FORM "x(15)"  "</B>"
               "<P10>" v-qty-or-sup AT 101 FORM "x(26)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank 
               "<C97>" v-adder-6 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              SKIP  
               "<P8>" "<U>Die Cutting, Slit, & Saw</U><P10>" AT 125
              SKIP
                "<P8><U>Item Description:</U><P10>" AT 10               
                "Die #" AT 100 xeb.die-no when avail xeb " Loc:" v-die-loc 
              SKIP
              "Style:" AT 2 xstyle.dscr WHEN AVAIL xstyle              
              "Gross Size:" AT 80  
              "W:" + trim(string({sys/inc/k16v.i xef.gsh-wid},">>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i xef.gsh-len},">>>9.99"))
                                                                format "x(19)"
              "Slit: W:" + string(v-outw) + " L:" + string(v-outl) FORM "x(15)"   
              SKIP
              "Size:" AT 2 
                      (trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99"))) FORM "x(30)" WHEN AVAIL xeb   
              
              "Net   Size:"  AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>9.99")) format "x(22)"
              SKIP
              "Joint:" AT 2 v-joint-dscr             
                            "Die   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(21)"
              "Up:" "W:" + string(v-upl) + " L:" + string(v-upw) FORM "x(9)"
              SKIP            
              "Blank Size:" AT 80 
              "W:" + trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
              " " +
              "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)"
              SKIP
                "Impressions:" AT 80 trim(string(v-dc-qty))    format "x(7)"
              SKIP
              "D/C Style:" AT 80
              SKIP              
              WITH WIDTH 200 NO-BOX NO-LABEL FRAME shp-info STREAM-IO. 

      find first sys-ctrl where sys-ctrl.company eq cocode
                          AND sys-ctrl.name    eq "OECOUNT" no-lock no-error.
      v-oecount = avail sys-ctrl and sys-ctrl.log-fld.

      IF AVAIL xeb THEN FIND FIRST item
              WHERE item.company EQ cocode
                AND item.i-no    EQ xeb.cas-no NO-LOCK NO-ERROR.

      ASSIGN
      v-cas-desc = IF AVAIL ITEM THEN ITEM.i-name 
                   ELSE IF AVAIL xeb THEN xeb.cas-no
                   ELSE ""
      lv-cas-cnt = 0.

      IF AVAIL xeb THEN FIND FIRST item
              WHERE item.company EQ cocode
                AND item.i-no    EQ xeb.tr-no NO-LOCK NO-ERROR.
          
      v-height = "HEIGHT: " + TRIM(STRING(b-eb.tr-dep,"ZZ9.99")).

      display "<P8><U>Packing:</U>" AT 13 
              "<U>Printing:</U>" AT 125
              "<P10>"SKIP
              v-cas-desc AT 3 FORM "x(25)"
              "<B>PRINTING PLATE #:" AT 80 xeb.plate-no when avail xeb "</B>"
              SKIP
              "# Per Bndl:"        AT 3
              xeb.cas-cnt when avail xeb
              "<P10># Per Unit:" AT 3 xeb.tr-cnt FORMAT ">>>>>9" when avail xeb
              "<P10>" v-stackcode AT 3   format "x(28)"  
              SKIP
              "Pallet:" AT 3
              trim(string({sys/inc/k16v.i xeb.tr-len},">,>>9")) + " x " +
              trim(string({sys/inc/k16v.i xeb.tr-wid},">,>>9")) when avail xeb format "x(15)"
              SKIP
              item.i-name AT 3 FORMAT "X(20)" WHEN AVAIL ITEM
              v-height AT 24
              "" AT 80 v-ink-5 SKIP
              "" AT 80 v-ink-6 SKIP
              "" AT 80 v-ink-7 SKIP
              "" AT 80 v-ink-8 SKIP
              "" AT 80
              with no-box no-labels frame m6 width 200 NO-ATTR-SPACE STREAM-IO.

      run cecrep/jobfibr2.p (recid(job-hdr),v-format,cust.terms).
      
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
      
      put SKIP /*(3)*/
          "<R-3><P8>        <U>Machine Routing:</U>              <U>Sheets</U>"
          "<P10>"  SKIP.
      i = 0.
      for each w-m by w-m.dseq:

        i = i + 1.

        v-letter = substr("UTE",i,1).

        display w-m.dscr AT 3   "<P8>  <U>Received:</U><P10>" WHEN i = 1 AT 29
            with no-box no-labels frame oo1 width 150 no-attr-space down STREAM-IO.
                
        v-lines = v-lines + 1.
      end.
      FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                         AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
      
      IF AVAIL b-ef AND b-ef.form-no = w-ef.frm THEN 
         FOR EACH w-m:
             CREATE tt-wm.
             BUFFER-COPY w-m TO tt-wm.
      END.

    EMPTY TEMP-TABLE tt-formtext.

    lv-text = "".
    FOR EACH notes WHERE
        notes.rec_key = job.rec_key AND
        (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0) AND
        LOOKUP(notes.note_code,v-exc-depts) EQ 0
        NO-LOCK,
        FIRST dept NO-LOCK WHERE dept.code = notes.note_code
           BY notes.note_form_no BY dept.fc:

       lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
    END.    

    DO li = 1 TO 11:
      CREATE tt-formtext.
      ASSIGN
       tt-line-no = li
       tt-length  = 48.
    END.

    DO li = 12 TO 30:
      CREATE tt-formtext.
      ASSIGN
       tt-line-no = li
       tt-length  = 39.
    END.

    RUN custom/formtext.p (lv-text).
    ASSIGN
       i = 0
       v-dept-note = "".

    FOR EACH tt-formtext:
        i = i + 1.
        IF i <= 30 THEN v-dept-note[i] = tt-formtext.tt-text.      
    END.

    /*** this is for the spec notes specific to itemfg replacing dept-notes extent 21-25 
         to insert these SPEC NOTES (AH) 05-20-10 */
    lv-text = "".
    EMPTY TEMP-TABLE tt-formtext.
    IF AVAIL itemfg THEN DO:
       FOR EACH notes NO-LOCK WHERE notes.rec_key   EQ itemfg.rec_key
                                AND notes.note_type EQ "S"
                                AND CAN-DO(v-spec-list,notes.note_code),
           FIRST ITEM-spec NO-LOCK WHERE item-spec.company = itemfg.company
                                     AND item-spec.i-no = ""
                                     AND item-spec.code = notes.note_code
           BY item-spec.code BY notes.note_date BY notes.note_time:
           
           lv-text = lv-text + 
                     STRING(job-hdr.frm,">9") + "     " +
                     "<B>" + CAPS(notes.note_code) + ":</B>" + "   " + 
                     notes.note_text + CHR(10) .
       END.
    END.

    ASSIGN
       v-start-note = 30
       v-end-note = 0.

    DO li = 30 TO 1 BY -1:
       IF v-dept-note[li] NE "" THEN
       DO:
          v-start-note = li.
          LEAVE.
       END.
    END.

    ASSIGN
       v-start-note = v-start-note + 1
       v-end-note = v-start-note + 4.

    IF v-start-note LT 31 THEN
    DO:
       DO li = v-start-note TO v-end-note:
          CREATE tt-formtext.
          ASSIGN
             tt-line-no = li
             tt-length  = 39.
       END.
       
       RUN custom/formtext.p (lv-text).
      
       DO li = v-start-note TO v-end-note:
          IF li LE 30 THEN
             v-dept-note[li] = "".
       END.
    END.

    ASSIGN
       i = 0
       v-count = v-start-note.

    FOR EACH tt-formtext:
        IF v-count GT 30 THEN
           LEAVE.
        i = i + 1.
        IF i <= 5 THEN
           v-dept-note[v-count] = tt-formtext.tt-text.
        v-count = v-count + 1.
    END.                       
    /***************************/

        ASSIGN
        v-inst = ""
        v-dept-note[31] = "CSR: " + IF AVAIL xoe-ord THEN xoe-ord.user-id ELSE job.user-id.
        IF s-prt-ship-split THEN
           FIND FIRST tt-fibre WHERE tt-fibre.tt-job-no = job-hdr.job-no
                         AND tt-fibre.tt-job-no2 = job-hdr.job-no2
                         AND tt-fibre.tt-frm = w-ef.frm
                         AND tt-fibre.tt-blank = b-eb.blank-no NO-LOCK NO-ERROR.
        IF AVAIL tt-fibre THEN
           ASSIGN lv-split[1] = "LOC1 <U>" + string(tt-fibre.tt-sqty1,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order1,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty1,">>>,>>9") + "</U>"
                  lv-split[2] = "LOC2 <U>" + string(tt-fibre.tt-sqty2,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order2,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty2,">>>,>>9") + "</U>"
                  lv-split[3] = "LOC3 <U>" + string(tt-fibre.tt-sqty3,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order3,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty3,">>>,>>9") + "</U>"
                  lv-split[4] = "LOC4 <U>" + string(tt-fibre.tt-sqty4,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order4,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty4,">>>,>>9") + "</U>". .

        
        PUT "<=1><R+9><C33>" v-dept-note[1] /*"  " v-dept-note[26]*/
            "<=1><R+10><C33>" v-dept-note[2] /* "  " v-dept-note[27] */
            "<=1><R+11><C33>" v-dept-note[3] /*"  " v-dept-note[28]    */
            "<=1><R+12><C33>" v-dept-note[4] /*"  " v-dept-note[29]    */
            "<=1><R+13><C33>" v-dept-note[5] /*"  " v-dept-note[30]    */
            "<=1><R+14><C33>" v-dept-note[6] /*"  " v-dept-note[31]    */
            "<=1><R+15><C33>" v-dept-note[7] /*"  " v-dept-note[32]    */
            "<=1><R+16><C33>" v-dept-note[8] /*"  " v-dept-note[33]    */
            "<=1><R+17><C33>" v-dept-note[9] /*"  " v-dept-note[34]    */
            "<=1><R+18><C33>" v-dept-note[10] /*"  " v-dept-note[35]   */
            "<=1><R+19><C33>" v-dept-note[11] /*"  " v-dept-note[36]   */
            "<=1><R+20><C33>" v-dept-note[12] FORM "x(39)" /*"  " v-dept-note[37]   */
            "<=1><R+21><C33>" v-dept-note[13] FORM "x(39)" /*"  " v-dept-note[38]   */
            "<=1><R+22><C33>" v-dept-note[14] FORM "x(39)" /*"  " v-dept-note[39]   */
            "<=1><R+23><C33>" v-dept-note[15] FORM "x(39)" /*"  " v-dept-note[40]   */
            "<=1><R+24><C33>" v-dept-note[16] FORM "x(39)" /*"  " v-dept-note[41]   */
            "<=1><R+25><C33>" v-dept-note[17] FORM "x(39)" /*"  " v-dept-note[42]   */
            "<=1><R+26><C33>" v-dept-note[18] FORM "x(39)" /*"  " /* v-dept-note[43]*/ */
            "<=1><R+27><C33>" v-dept-note[19] FORM "x(39)" /*"  " /*v-dept-note[44]*/   */
            "<=1><R+28><C33>" v-dept-note[20] FORM "x(39)" /*"  " /* v-dept-note[45] */  */
            "<=1><R+29><C33>" v-dept-note[21] FORM "x(39)" 
            "<=1><R+30><C33>" v-dept-note[22] FORM "x(39)" 
            "<=1><R+31><C33>" v-dept-note[23] FORM "x(39)" 
            "<=1><R+32><C33>" v-dept-note[24] FORM "x(39)" 
            "<=1><R+33><C33>" v-dept-note[25] FORM "x(39)"
            "<=1><R+34><C33>" v-dept-note[26] FORM "x(39)"
            "<=1><R+35><C33>" v-dept-note[27] FORM "x(39)"
            "<=1><R+36><C33>" v-dept-note[28] FORM "x(39)"
            "<=1><R+37><C33>" v-dept-note[29] FORM "x(39)"
            "<=1><R+38><C33>" v-dept-note[30] FORM "x(39)"
            "<=1><R+39><C33>" v-dept-note[31] FORM "x(39)" 
            "<=1><R+40><C33><FROM><R+5><C65><RECT><||3>"
            "<=1><R+41><C33><P8>  Bundles: ________ @ _______          TOTAL"
            "<=1><R+42><C40> ________ @ _______"
            "<=1><R+43><C33>  Pallets: ________ @ _______"
            "<=1><R+44><C40> ________ @ _______       __________"
            "<=1><R+42><C57><FROM><C64><LINE><||3>"
            "<=1><R+40><C55><FROM><R+5><C55><LINE><||3>"
            .
        IF s-prt-ship-split THEN
            PUT 
            "<=1><R+34><C32><FROM><C65><LINE><||3>"
            "<=1><R+34><C33>" "SPLIT SHIP/QTY   |         SPLIT ORDER" FORM "x(39)" 
            "<=1><R+35><C33>" lv-split[1] FORM "x(70)"
            "<=1><R+36><C33>" lv-split[2] FORM "x(70)"
            "<=1><R+37><C33>" lv-split[3] FORM "x(70)"
            "<=1><R+38><C33>" lv-split[4] FORM "x(70)"
            .

        PUT "<=1><R+30><C66.8><P10>Color: " v-coldscr FORMAT "x(35)" "<FCourier New>"
            "<=1><C66><R+31><FROM><C106><LINE><||3>".

        /*PUT "<=1><R+30><C67><P14>" lv-checkbox-2
            "<=1><R+31.5><C67>" lv-checkbox-3 "<P10><FCourier New>".*/

        PAGE.
        if print-box and avail xest then do:
            PUT "<#11><C1><FROM><C106><R+47><RECT><||3><C80><P10>" v-qa-text  
            "<=11><C30><FROM><R+3><C30><LINE><||3>"
            "<=11><C60><FROM><R+3><C60><LINE><||3>"
            "<=11><R+3><C1><FROM><C106><LINE><||3>"
            "<=11>Job # <C30> Estimate # <C60> Cust Part #"  SKIP
            "<P12><C12>" v-job-prt 
            "<C40>" v-est-no
            "<C70>" lv-part-no SKIP /*(2)*/.

            /*PAGE. */
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

       IF s-prt-set-header AND last-of(job.job-no2) AND est.est-type = 6 THEN DO: /* print set header */
          i = 0.
          FOR EACH bf-eb WHERE bf-eb.company = est.company
                            AND bf-eb.est-no = est.est-no
                            AND bf-eb.form-no > 0 NO-LOCK:
               i = i + 1.
          END.   
          IF i > 1 THEN DO:
             ASSIGN
             v-fg-set = job-hdr.i-no
             v-set-qty = if avail xeb and xeb.est-type eq 6 THEN
                           if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty
                         ELSE 0
             v-ord-qty = (if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty) *
                         (if est.form-qty le 1 then 1 else v-pqty).
                                                            
             IF ll-jqcust AND NOT AVAIL xoe-ordl AND AVAIL job-hdr THEN 
                ASSIGN v-ord-qty = v-ord-qty * ( 1 - (cust.over-pct / 100)).

             ASSIGN
             v-over-run = IF avail xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%"))
                          ELSE IF avail xoe-ord THEN trim(string(xoe-ord.over-pct,">>9.99%")) ELSE ""
             v-under-run = IF avail xoe-ordl THEN trim(STRING(xoe-ordl.under-pct,">>9.99%"))
                           ELSE IF avail xoe-ord THEN trim(STRING(xoe-ord.under-pct,">>9.99%")) ELSE "".

             PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
                 "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
                 "<C60>Our Date: " v-ord-date SKIP
                 "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
                 "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
                 "<=1><R+6><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
                 v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
                 v-i-line[2] AT 90
                 SKIP
                 v-cus[2] AT 3 " Job Qty:" trim(string(job-hdr.qty * v-pqty,">>,>>>,>>9"))    format "x(10)"
                 " Order Qty:" string(v-ord-qty) format "x(10)"
                 v-i-line[3] AT 90 SKIP
                 v-cus[3] AT 3  " Cust Part #:" lv-part-no 
                 v-i-line[4] AT 90 SKIP
                 v-cus[4]  AT 3 " Overrun:"  format "x(7)" 
                 " Underrun:" format "x(7)"  
                 "Adders:" v-adders FORM "x(33)" AT 90 SKIP
                 "<=1><R+11><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
                 "<C2>FINISHED GOOD #                 DESCRIPTION                       RATIO PER SET" SKIP.
             /* each components */
        
             v-tmp-line = 0.
             FOR EACH xeb FIELDS(stock-no part-dscr1 yld-qty) WHERE
                 xeb.company = est.company AND
                 xeb.est-no = est.est-no AND
                 xeb.form-no > 0 NO-LOCK:
                 PUT xeb.stock-no AT 3 space(14) xeb.part-dscr1 space(5) xeb.yld-qty FORMAT "->>>>>9" SKIP.
                 v-tmp-line = v-tmp-line + 1.
             END.
             v-tmp-line = v-tmp-line + 1.
             /* print raw materials from misc/fram of Est */ 
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
             PUT "<=1><R+12><C2><FROM><R+" + string(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)" SKIP.
             v-tmp-line = v-tmp-line + 12 .
        
             i = 0.
             for each tt-wm WHERE lookup(tt-wm.m-code,tspostfg-char) > 0:
                  i = i + 1.
             END.
             i = i + 2.
             PUT "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
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
                    with no-box no-labels frame o21 width 132 no-attr-space down STREAM-IO.
             end.
             FOR EACH tt-wm:
                 DELETE tt-wm.
             END.
             v-tmp-line = v-tmp-line + 3 + i /* 4 and add machine routing lines */.
        
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
                          tt-prem.tt-#-unit = string(bf-eb.cas-pal)
                          tt-prem.tt-pallet = bf-eb.tr-no
                          tt-prem.tt-count  = bf-eb.tr-cnt.
             END.

             FOR EACH tt-formtext:
                DELETE tt-formtext.
             END.    
             lv-text = "".
             FOR EACH notes FIELDS(note_text) NO-LOCK WHERE
                 notes.rec_key = job.rec_key AND
                 lookup(notes.note_code,"SH,UN") GT 0:
                 lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
             END.
             FOR EACH notes FIELDS(note_text) NO-LOCK WHERE
                 notes.rec_key = itemfg.rec_key AND
                 lookup(notes.note_code,"SH,UN") GT 0:
                 lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
             END.
             DO li = 1 TO 15:
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
                IF  i <= 15 THEN v-dept-inst[i] = tt-formtext.tt-text.      
             END.
             IF v-ship <> "" THEN v-dept-inst[15] = v-ship.  /* shipto notes */
             PUT "<=1><R+" + string(v-tmp-line) + ">" form "X(20)".
             v-tmp-line = v-tmp-line + 1.
             PUT "Unitizing Bale <C24>Date <C44>Units <C62>Complete" AT 3 SKIP
                 "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________" skip
                 "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial" skip
                 "Pallet: " AT 3 tt-prem.tt-pallet FORM "x(10)"  "<C20>_____________________ <C40>____________________ " skip
                 "Count:  " AT 3 tt-prem.tt-count FORM ">>>>9" SKIP
                 "<=1><R+" + string(v-tmp-line) + "><C2><FROM><R+6><C78><RECT><||3>" FORM "x(150)" SKIP
                 "<=1><R+" + string(v-tmp-line + 7) + "><C2><FROM><R+16><C78><RECT><||3>" FORM "x(150)" SKIP
        
                 "<=1><R+" + string(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
                 v-dept-inst[1] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[1] SKIP
                 v-dept-inst[2] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[2] SKIP
                 v-dept-inst[3] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[3] SKIP
                 v-dept-inst[4] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[4] SKIP
                 v-dept-inst[5] AT 3 FORM "x(82)" chr(124) format "xx" "Item PO #:" v-po-no SKIP
                 v-dept-inst[6] AT 3 SKIP
                 v-dept-inst[7] AT 3 SKIP
                 v-dept-inst[8] AT 3 SKIP
                 v-dept-inst[9] AT 3 SKIP
                 v-dept-inst[10] AT 3 SKIP
                 v-dept-inst[11] AT 3 SKIP
                 v-dept-inst[12] AT 3 SKIP
                 v-dept-inst[13] AT 3 SKIP
                 v-dept-inst[14] AT 3 SKIP
                 v-dept-inst[15] AT 3 
                 .
             PAGE.
          END. /* i > 1*/
       END. /* set header printing  est.est-type = 6 */
    end.  /* each job */
/*    end.  /* end v-local-loop  */*/
 
        hide all no-pause.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
