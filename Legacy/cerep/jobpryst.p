/* -------------------------------------------------------- cerep/jobpryst.p  */
/*  factory ticket  for folding , Prystup                                      */
/* -------------------------------------------------------------------------- */
def input parameter v-format like sys-ctrl.char-fld.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}

def new shared var save_id as RECID .
def new shared var v-today as date init today.
def new shared var v-job as char format "x(6)" extent 2 init [" ","zzzzzz"].
def new shared var v-job2 as int format "99" extent 2 init [00,99].
def new shared var v-stypart like style.dscr.
def new shared var v-dsc like oe-ordl.part-dscr1 extent 2.
def new shared var v-size as char format "x(26)" extent 2.
def new shared var v-bld-job like oe-ord.job-no.
def new shared var v-bld-job2 like oe-ord.job-no2.
def new shared var v-fill as char format "x(128)".
def new shared var v-frst as log.
def new shared var v-ok as log.
def new shared var v-est-qty as int format "->>,>>>,>>9".
def new shared var v-job-qty as int format "->>,>>>,>>9".
def new shared var v-fac as DEC .
def new shared var v-job-no like oe-ordl.job-no.
def new shared var v-job-no2 like oe-ordl.job-no2.
def new shared var v-due-date like oe-ord.due-date.
def new shared var v-reprint as log.
def new shared var v-up like eb.num-up.
def new shared var v-tandem as log.
def new shared var v-form-no like eb.form-no.
def new shared var v-fup as char.
def new shared var v-layout as char format "x(30)".

def var v-line as int init 1 no-undo.
def var v-gsh-qty as int no-undo.
def var cnt as int init 1 no-undo.
def var v-frm-blk as char format "x(6)" no-undo.
def var v-dec as dec no-undo.
def var v-ovund as char format "x(34)" no-undo.
def var v-mrhr as char format "x(5)".
def var v-cas-dscr like item.est-dscr.
def var v-first as log no-undo.
def var v-spec-list as char format "x(20)"init "QA" no-undo.
DEF VAR lv-form-note AS cha NO-UNDO.
DEF VAR v-itm-printed AS INT NO-UNDO.
DEF VAR v-alloc AS cha NO-UNDO.
DEF VAR v-prep AS cha EXTENT 8 NO-UNDO.
DEF VAR v-misc AS cha EXTENT 6 NO-UNDO.
DEF VAR v-spec-no AS cha EXTENT 8 NO-UNDO.
DEF VAR v-skip AS LOG NO-UNDO.
DEF VAR v-fill2 AS cha INIT "-" FORM "x(125)" NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-cust-name-extent AS CHAR EXTENT 30 NO-UNDO.
DEF VAR v-ship1-extent AS CHAR EXTENT 30 NO-UNDO.
DEF VAR v-ship2-extent AS CHAR EXTENT 30 NO-UNDO.
DEF VAR v-ship4-extent AS CHAR EXTENT 30 NO-UNDO.

def TEMP-TABLE w-lo NO-UNDO
  field layout like v-layout.

def new shared buffer xjob-hdr for job-hdr.

def buffer b-eb for eb.
DEF BUFFER b-est FOR est.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-oe-rel2 FOR oe-rel.
DEF BUFFER b-shipto FOR shipto.

def new shared workfile wrk-op
  field m-dscr like est-op.m-dscr
  field m-code like est-op.m-code
  field d-seq like est-op.d-seq
  field dept like est-op.dept
  field b-num like est-op.b-num
  field s-num like est-op.s-num
  field pass like est-op.op-pass
  field mr like est-op.op-mr extent 100
  field speed like est-op.op-speed extent 100
  field run-hr like job-mch.run-hr extent 100
  field num-sh like est-op.num-sh extent 100
  FIELD spoil LIKE job-mch.wst-prct EXTENT 100
  FIELD mr-waste LIKE job-mch.mr-waste EXTENT 100    .

def new shared workfile wrk-die
  field die-no like eb.die-no
  FIELD cad-no LIKE eb.cad-no
  field form-no like eb.form-no
  field die-size as char format "x(17)".

def new shared workfile wrk-sheet
  field gsh-qty like ef.gsh-qty
  field cal like ef.cal
  FIELD i-no LIKE ef.board
  field brd-dscr like ef.brd-dscr
  field form-no like ef.form-no
  field sh-wid like ef.nsh-len
  field sh-len like ef.nsh-wid.

def new shared workfile wrk-film
  field form-no like ef.form-no
  field snum as int format "99"
  field bnum as int format "99"
  field leaf as char format "x(10)"
  field leaf-l as dec format ">9.9999"
  field leaf-w as dec format ">9.9999".

def new shared workfile wrk-ink
  field i-code as char format "x(10)"
  field form-no like eb.form-no
  field blank-no like eb.blank-no
  field i-dscr as char format "x(20)"
  field i-qty as dec format ">,>>9.9<"
  field i-pass as dec
  FIELD seq AS INT.

def new shared workfile wrk-prep
  field code like est-prep.code
  field dscr like est-prep.dscr
  field s-num as int format "99"
  field b-num as int format "99"
  field ml like est-prep.ml.

def new shared workfile wrk-spec
  field form-no like ef.form-no
  field spec-no as char format "x(10)"
  field dscr as char format "x(20)"
  field qty as dec format ">>>9.9<<<"
  field uom as char format "x(3)".

def new shared workfile wrk-inst
  field d-seq like dept.fc
  field dscr like est-inst.dscr
  field line like est-inst.line-no
  field rec-id as recid.

def new shared workfile wrk-misc
  field form-no like ef.form-no
  field snum as int format "99"
  field bnum as int format "99"
  field cost as char format "x(20)".
  
{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 20 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 20 NO-UNDO.
DEF VAR v-spec-inst AS cha FORM "x(80)" EXTENT 10 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.

DEF VAR v-start-date AS DATE NO-UNDO.
DEF VAR v-req-date AS DATE NO-UNDO.
DEF VAR v-shipto AS cha FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-case-size AS cha NO-UNDO.
DEF VAR v-vend LIKE po-ord.vend-no NO-UNDO.
DEF VAR v-item AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink1 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink2 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-po-no LIKE oe-ordl.po-no NO-UNDO.
DEF VAR lv-mat-dept-list AS cha INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEF VAR v-mat-for-mach AS cha NO-UNDO.
DEF BUFFER xjob-mat FOR job-mat.
DEF VAR v-fgitm AS cha FORM "x(15)" EXTENT 30 NO-UNDO.
DEF VAR v-fgdsc LIKE eb.part-dscr1 EXTENT 30 NO-UNDO.
DEF VAR v-fgqty LIKE job-hdr.qty EXTENT 30 FORM ">>,>>>,>>>" NO-UNDO.
DEF VAR v-pono LIKE oe-ordl.po-no EXTENT 30 NO-UNDO.
DEF VAR v-num-of-fgitm AS INT NO-UNDO.
DEF TEMP-TABLE tt-fgitm FIELD i-no AS cha FORM "x(15)"
                        FIELD seq AS INT
                        FIELD qty AS INT 
                        FIELD i-dscr AS cha
                        FIELD po-no AS cha
                        FIELD cust-name AS cha
                        FIELD shipto1 AS CHAR
                        FIELD shipto2 AS CHAR
                        FIELD shipto4 AS CHAR
                        FIELD due-date AS DATE
                        FIELD pp# AS CHAR
                        FIELD RFQ# AS CHAR
                        FIELD unit AS cha EXTENT 7
                        FIELD form-no AS INT
                        FIELD blank-no AS INT
                        FIELD NumUp AS INT
                        FIELD SheetQty AS INT.

DEF VAR v-board-po LIKE oe-ordl.po-no-po NO-UNDO.
DEF VAR v-plate-printed AS LOG NO-UNDO.
DEF BUFFER xoe-ordl FOR oe-ordl.
DEF BUFFER b-cust FOR cust.
DEF VAR v-cust-name LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name2 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name3 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-last-j AS INT NO-UNDO.
DEF VAR v-po-no2 LIKE v-po-no NO-UNDO.
DEF VAR v-po-no3 LIKE v-po-no NO-UNDO.
v-fill = /*fill("=",128)*/ "<||3><C1><FROM><C108><LINE><||3>".

def new shared frame head.

DEF SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF SHARED VAR s-show-release AS LOG NO-UNDO.
DEF VAR v-po-duedate LIKE po-ordl.due-date NO-UNDO.
DEF VAR v-upc-lbl AS cha FORM "x(10)" NO-UNDO.
DEF VAR v-shipto1 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-shipto2 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF SHARED VAR s-run-speed AS LOG NO-UNDO.
DEF VAR v-pass-count AS INT NO-UNDO.
DEF VAR vPrevJob AS cha NO-UNDO.
DEF VAR vEstno AS cha NO-UNDO.
DEF VAR cOPDscr AS cha FORM "x(20)" NO-UNDO.
DEF VAR cOPRun AS cha FORM "x(20)" NO-UNDO.
DEF VAR dOpMr AS DEC NO-UNDO.
DEF VAR dOpRun AS DEC NO-UNDO.
DEF VAR dSheeterSpeed AS DEC NO-UNDO.
DEF VAR dPrinterSpeed AS DEC NO-UNDO.
DEF VAR dCutterSpeed AS DEC NO-UNDO.
DEF VAR dGluerSpeed AS DEC NO-UNDO.

DEF STREAM st2nd.
DEF VAR cOutput2 AS cha NO-UNDO.
DEF VAR cOpDscr2 AS cha EXTENT 5 NO-UNDO.
DEF VAR cOpRun2 AS cha EXTENT 5 NO-UNDO.
DEF VAR lBoardPrinted AS LOG NO-UNDO.

/* Excel Vars */
DEFINE VARIABLE    chExcel     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE    chWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE    chWorksheet2 AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE    chWorkbook  AS COM-HANDLE NO-UNDO.
DEF VAR cExcelOutput AS cha NO-UNDO.
DEFINE VARIABLE    cFileName   AS CHAR       NO-UNDO.
DEFINE VARIABLE CurrDir AS CHAR NO-UNDO.
DEF VAR v-term AS cha NO-UNDO.
DEF VAR cTotalSheet AS cha NO-UNDO.
DEF VAR iExcelRowCount AS INT NO-UNDO.


RUN CreateExcelSheet.
RUN JobReport.
/*RUN CloseExcel.            */

chWorkbook:sheets(1):activate.

ASSIGN chExcel:VISIBLE = TRUE
       chExcel:ScreenUpdating = YES. 

/*RUN ReOpenExcel.*/
RUN ReleaseExcelObject.
/* end of main */

PROCEDURE JobReport:
{sys/inc/notes.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

ASSIGN
 v-job[1]    = fjob-no
 v-job[2]    = tjob-no
 v-job2[1]   = fjob-no2
 v-job2[2]   = tjob-no2
 v-reprint   = reprint
 v-spec-list = spec-list.

EMPTY TEMP-TABLE tt-fgitm.

for each job-hdr NO-LOCK
        where job-hdr.company               eq cocode
          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no
          and (production OR
               job-hdr.ftick-prnt           eq v-reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          and can-find(first job where job.company eq cocode
                                   and job.job     eq job-hdr.job
                                   and job.job-no  eq job-hdr.job-no
                                   and job.job-no2 eq job-hdr.job-no2
                                   and job.stat    ne "H"
                                   AND (job.pr-printed EQ reprint OR
                                        NOT production))
        use-index job-no,

        first est
        where est.company  eq job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
          and est.est-type le 4  
        no-lock

        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY job-hdr.frm:

      find first job WHERE
           job.company eq cocode AND
           job.job     eq job-hdr.job AND
           job.job-no  eq job-hdr.job-no AND
           job.job-no2 eq job-hdr.job-no2
           no-lock no-error.

      IF production AND
         job.cs-trans-date NE ? THEN DO:
         li = 0.
         DO WHILE li LT 1000:
           li = li + 1.
           FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
           IF AVAIL job THEN
             ASSIGN
              job.pr-printed    = YES
              job.pr-user-id-p  = USERID("nosweat")
              job.pr-print-date = TODAY
              job.pr-print-time = TIME
              li                = 1000.
         END.
      END.
      ELSE DO:
         li = 0.
         IF NOT job-hdr.ftick-prnt THEN DO WHILE TRUE:
            li = li + 1.
            FIND xjob-hdr EXCLUSIVE-LOCK
               WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
               NO-ERROR NO-WAIT.
            IF AVAIL xjob-hdr THEN xjob-hdr.ftick-prnt = YES.
            IF li GE 1000 OR xjob-hdr.ftick-prnt THEN LEAVE.
         END.

         li = 0.
         DO WHILE li LT 1000:
            li = li + 1.
            FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAIL job THEN DO:
              li = 1000.
         
              IF NOT job.cs-printed THEN
                ASSIGN
                 job.cs-printed    = YES
                 job.cs-user-id-p  = USERID("nosweat")
                 job.cs-print-date = TODAY
                 job.cs-print-time = TIME.
         
              IF approve THEN
                ASSIGN
                 job.cs-to-pr      = YES
                 job.cs-user-id-t  = USERID("nosweat")
                 job.cs-trans-date = TODAY
                 job.cs-trans-time = TIME.
            END.
         END.
      END.
      
      FIND CURRENT job NO-LOCK NO-ERROR.
      vEstno = job-hdr.est-no.
      v-est-qty = if avail est then est.est-qty[1] else 0.
      find first oe-ord where oe-ord.company eq job-hdr.company
                          and oe-ord.ord-no  eq job-hdr.ord-no no-lock no-error.

      if FIRST-OF(job-hdr.frm) then v-first = yes.

      /** PRINT JOB HEADER **/
      if v-first then do:
        assign
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2.


        if avail oe-ord then
          if not oe-ctrl.p-fact and oe-ord.stat eq "H" then next.

        ASSIGN
           v-due-date = if avail oe-ord then oe-ord.due-date else ?
           v-start-date = IF AVAIL oe-ord THEN oe-ord.ord-date ELSE job-hdr.start-date.

        v-shipto = "".
        find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq job-hdr.i-no
                no-lock no-error.

        IF AVAIL oe-ordl THEN 
            find first oe-rel
            where oe-rel.company eq cocode
              and oe-rel.ord-no  eq oe-ordl.ord-no
              and oe-rel.i-no    eq oe-ordl.i-no
              and oe-rel.line    eq oe-ordl.line
            no-lock no-error.
        if avail oe-rel then do:
           find first shipto
              where shipto.company eq cocode
                and shipto.cust-no eq oe-rel.cust-no
                and shipto.ship-id eq oe-rel.ship-id
              no-lock no-error.  

          if avail shipto then
              ASSIGN v-shipto[1] = shipto.ship-name
                     v-shipto[2] = shipto.ship-addr[1]
                     v-shipto[3] = shipto.ship-addr[2]
                     v-shipto[4] = trim(oe-rel.ship-city) + ", " +
                                   oe-rel.ship-state + "  " + oe-rel.ship-zip.          
        end.
        FIND FIRST cust WHERE cust.company = job-hdr.company AND
                              cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.
        ASSIGN
        v-req-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?
        v-cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name 
                      ELSE IF AVAIL cust THEN cust.name
                      ELSE job-hdr.cust-no.
        IF AVAIL oe-ord THEN
            ASSIGN v-shipto[1] = oe-ord.sold-name
                   v-shipto[2] = oe-ord.sold-addr[1]
                   v-shipto[4] = oe-ord.sold-city + ", " + oe-ord.sold-state + " " + oe-ord.sold-zip.

        v-line = if avail est                            and
                 est.est-type gt 2 and est.est-type lt 5 then 500 else 50.

        /** SUM UP NUMBER OF SHEETS **/
        find first job
            where job.company eq cocode
              and job.job     eq job-hdr.job
              and job.job-no  eq v-job-no
              and job.job-no2 eq v-job-no2
            no-lock no-error.
            
        if avail job then
        for each job-mch
            where job-mch.company eq cocode
              and job-mch.job     eq job.job
              and job-mch.job-no  eq job.job-no
              and job-mch.job-no2 eq job.job-no2
              AND job-mch.frm = job-hdr.frm
            no-lock,

            first mach
            {sys/ref/machW.i}
              and mach.m-code eq job-mch.m-code
            no-lock

            by mach.d-seq
            by job-mch.frm
            by job-mch.blank-no
            by job-mch.pass
            by job-mch.run-qty desc:

          find first wrk-op
              where wrk-op.m-code eq job-mch.m-code
                and wrk-op.s-num  eq job-mch.frm
               and wrk-op.b-num  eq job-mch.blank-no
                and wrk-op.pass   eq job-mch.pass 
              no-error.
          if not avail wrk-op then do:
            create wrk-op.
            assign
             wrk-op.m-code = job-mch.m-code
             wrk-op.m-dscr = mach.m-dscr
             wrk-op.d-seq  = mach.d-seq
             wrk-op.dept   = job-mch.dept
             wrk-op.s-num  = job-mch.frm
             wrk-op.b-num  = job-mch.blank-no
             wrk-op.pass   = job-mch.pass.
          end.
          assign
           wrk-op.mr[job-mch.frm]     = job-mch.mr-hr
           wrk-op.speed[job-mch.frm]  = job-mch.speed
           wrk-op.num-sh[job-mch.frm] = job-mch.run-qty
           wrk-op.spoil[job-mch.frm] = job-mch.wst-prct   
           wrk-op.mr-waste[job-mch.frm] = job-mch.mr-waste  
           wrk-op.run-hr[job-mch.frm] = job-mch.run-hr    .

          IF wrk-op.dept = "RS" THEN dSheeterSpeed = job-mch.speed.
          ELSE IF wrk-op.dept = "PR" THEN dPrinterSpeed = job-mch.speed.
          ELSE IF wrk-op.dept = "DC" THEN dCutterSpeed = job-mch.speed.
/*           ELSE IF wrk-op.dept = "GL" THEN dGluerSpeed = job-mch.speed. */
        end.
        IF AVAIL est THEN
            FIND FIRST est-op WHERE est-op.company = est.company
                AND est-op.est-no = est.est-no
                AND est-op.dept = "GL" NO-LOCK NO-ERROR.
       IF AVAIL est-op THEN dGluerSpeed = est-op.op-speed.

        /** BUILD PREP WORK FILE **/
        for each job-prep
            where job-prep.company eq cocode
              and job-prep.job     eq job-hdr.job
              and job-prep.job-no  eq job-hdr.job-no
              and job-prep.job-no2 eq job-hdr.job-no2
            no-lock:
          find first prep
              where prep.company eq cocode
                and prep.code    eq job-prep.code
              no-lock no-error.
          create wrk-prep.
          assign
           wrk-prep.code = job-prep.code
           wrk-prep.dscr = if avail prep then prep.dscr else ""
           wrk-prep.s-num = job-prep.frm
           wrk-prep.b-num = job-prep.blank-no
           wrk-prep.ml = job-prep.ml.
        end. /* each job-prep */

        if avail est then
        for each est-prep
            where est-prep.company eq est.company
              AND est-prep.est-no  eq est.est-no
              and index("SON",est-prep.simon) gt 0
            no-lock:
          find first prep
              where prep.company eq cocode
                and prep.code    eq est-prep.code
              no-lock no-error.
          create wrk-prep.
          assign
           wrk-prep.code  = est-prep.code
           wrk-prep.dscr  = if avail prep then prep.dscr else ""
           wrk-prep.s-num = est-prep.s-num
           wrk-prep.b-num = est-prep.b-num
           wrk-prep.ml    = est-prep.ml.
        end.

        if avail oe-ord then
        for each oe-ordm 
            where oe-ordm.company eq cocode
              and oe-ordm.ord-no  eq oe-ord.ord-no
            no-lock:
          find first wrk-prep where wrk-prep.code eq oe-ordm.charge no-error.
          if not avail wrk-prep then do:
            find first prep
                where prep.company eq cocode
                  and prep.code    eq oe-ordm.charge
                no-lock no-error.
            create wrk-prep.
            assign
             wrk-prep.code  = oe-ordm.charge
             wrk-prep.dscr  = if avail prep then prep.dscr else ""
             wrk-prep.s-num = 9
             wrk-prep.b-num = 99
             wrk-prep.ml    = if avail prep then prep.ml else ?.
          end.
        end.
      
      for each ef
          WHERE ef.company EQ job-hdr.company
            AND ef.est-no  EQ job-hdr.est-no
            AND ef.form-no = job-hdr.frm
          break by ef.est-no by ef.form-no:

        v-job-qty = 0.
        for each xjob-hdr FIELDS(qty)
            where xjob-hdr.company eq cocode
              and xjob-hdr.job     eq job-hdr.job
              and xjob-hdr.job-no  eq job-hdr.job-no
              and xjob-hdr.job-no2 eq job-hdr.job-no2
              and xjob-hdr.i-no    eq job-hdr.i-no
            no-lock:
          v-job-qty = v-job-qty + xjob-hdr.qty.
        end.
          
        v-est-qty = 0.
        if est.est-type eq 4 then
        for each eb FIELDS(yld-qty)
            WHERE eb.company  EQ ef.company
              AND eb.est-no   eq ef.est-no
              and eb.stock-no eq job-hdr.i-no
            no-lock:
          v-est-qty = v-est-qty + eb.yld-qty.
        end.

        else v-fac = 1.
        v-itm-printed = 0.

        if ef.form-no eq job-hdr.frm then ebloop:
        for each eb
            WHERE eb.company     EQ ef.company
              AND eb.est-no      eq ef.est-no
              and eb.form-no     eq ef.form-no NO-LOCK
            break by eb.form-no BY eb.blank-no.

      /*    if est.est-type eq 4 and eb.stock-no ne job-hdr.i-no then next ebloop. */

          create w-lo.
          for each b-eb
              WHERE b-eb.company EQ eb.company
                AND b-eb.est-no  eq eb.est-no
                and b-eb.part-no eq eb.part-no
              no-lock break by b-eb.est-no:
            v-fup = "F" + trim(string(b-eb.form-no,">>9")) + "-" +
                    trim(string(b-eb.blank-no,"99")) + "/" +
                    trim(string(b-eb.num-up,">>9")) + "up".
            if length(trim(v-fup)) + length(trim(w-lo.layout)) gt 30 then do:
              substr(w-lo.layout,length(trim(w-lo.layout)),1) = "".
              create w-lo.
            end.
            w-lo.layout = trim(w-lo.layout + " " + trim(v-fup) + ",").
            if last(b-eb.est-no) then
              substr(w-lo.layout,length(trim(w-lo.layout)),1) = "".
          end.
          
          find first wrk-die where wrk-die.die-no eq eb.die-no no-error.
          if not avail wrk-die and eb.die-no gt "" then do:
            create wrk-die.
            assign wrk-die.die-no = eb.die-no
                   wrk-die.cad-no = eb.cad-no
              wrk-die.form-no = eb.form-no
              wrk-die.die-size = string(ef.trim-w) + "x" +
              string(ef.trim-l).
          end.

          /** BUILD INK WORK FILE **/
          for each job-mat
              where job-mat.company eq cocode
                and job-mat.job     eq job-hdr.job
                and job-mat.frm     eq eb.form-no
              no-lock,
              first item
              {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no
              no-lock:

              
            do i = 1 to 17:
              if eb.i-code2[i] eq job-mat.i-no then do:            
                find first wrk-ink
                    where wrk-ink.i-code   eq eb.i-code2[i]
                      and wrk-ink.form-no  eq eb.form-no
                      and wrk-ink.blank-no eq eb.blank-no
                      AND wrk-ink.i-pass = eb.i-ps2[i]
                    no-error.

                if not avail wrk-ink then do:
                  create wrk-ink.
                  assign
                   wrk-ink.i-code   = eb.i-code2[i]
                   wrk-ink.form-no  = eb.form-no
                   wrk-ink.blank-no = eb.blank-no
                   wrk-ink.i-dscr   = eb.i-dscr2[i]
                   wrk-ink.i-pass   = eb.i-ps2[i]
                   wrk-ink.seq = i   .        
                end.                   
              end.
            end. /* loop i */

            find first wrk-ink
                where wrk-ink.i-code    eq job-mat.i-no
                  and wrk-ink.form-no   eq job-mat.frm
                  and (wrk-ink.blank-no eq job-mat.blank-no or
                       est.est-type     eq 4)
                no-error.
                
            if not avail wrk-ink                              and
               (job-mat.blank-no  eq eb.blank-no or
                (job-mat.blank-no eq 0 and eb.blank-no eq 1)) then do:
              create wrk-ink.
              assign
               wrk-ink.i-code   = job-mat.i-no
               wrk-ink.form-no  = eb.form-no
               wrk-ink.blank-no = eb.blank-no
               wrk-ink.i-dscr   = item.est-dscr
               wrk-ink.i-pass   = 1
               wrk-ink.seq = 1   .              
            end.

            if avail wrk-ink then wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.
          end. /* JOB-MAT */

          if eb.est-type eq 4 then v-fac = eb.yld-qty / v-est-qty.
          
        
            find first style
                where style.company eq eb.company
                  and style.style   eq eb.style
                no-lock no-error.
            if avail style then v-stypart = style.dscr.
            assign
             v-dsc[1] = eb.part-dscr1
             v-dsc[2] = eb.part-dscr2
             v-size[1] = string(eb.len) + "x" + string(eb.wid) + "x" +
                         string(eb.dep)
             v-size[2] = eb.i-coldscr.

             IF eb.blank-no > 0 AND eb.blank-no < 11 THEN 
                 ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.
                                                             
            /*if v-first then*/
            v-upc-lbl = IF s-prt-sellprc THEN "Sell Price" ELSE "   UPC#".
          
           v-job-qty = 0.
           for each xjob-hdr FIELDS(qty) where xjob-hdr.company eq cocode
               and xjob-hdr.job     eq job-hdr.job
               and xjob-hdr.job-no  eq job-hdr.job-no
               and xjob-hdr.job-no2 eq job-hdr.job-no2
               and xjob-hdr.i-no    eq eb.stock no-lock:
               v-job-qty = v-job-qty + xjob-hdr.qty.
            end.

            /** PRINT ITEM **/
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq eb.stock-no /*job-hdr.i-no*/
                no-lock no-error.

            if avail oe-ordl then do:
              v-est-qty = oe-ordl.qty.
              find first oe-ord of oe-ordl no-lock.
              v-ovund = string("Overrun/Underrun %:  " +
                               trim(string(oe-ordl.over-pct,">>9.99")) + "/" +
                               trim(string(oe-ordl.under-pct,">>9.99"))).
            end.
            else v-est-qty = v-job-qty.
            
            release w-lo.
            find first w-lo no-error.

            ASSIGN
            v-case-size = string(eb.cas-len) + "x" + string(eb.cas-wid) + "x" +
                         string(eb.cas-dep)
            v-up = eb.num-up
            v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".
            /*
            display trim(string(eb.form-no,">>9")) + "-" +
                    trim(string(eb.blank-no,">>9")) FORM "x(5)" 
                    eb.stock-no @ job-hdr.i-no 
                    v-job-qty /** v-fac*/ format "->>,>>>,>>9"
                    oe-ordl.po-no when avail oe-ordl 
                    v-dsc[1] FORM "x(21)"
                    eb.style /*v-stypart */
                    v-size[1] FORM "x(22)"
                    oe-ordl.cas-cnt when avail oe-ordl
                    eb.cas-cnt when (not avail oe-ordl) or oe-ordl.cas-cnt eq 0 @ oe-ordl.cas-cnt
                    /*PACE(4)*/
                    v-case-size  FORM "x(15)"
                    v-up
                    eb.upc-no 
                    string(oe-ordl.price,">>>,>>9.99<<<<")
                            WHEN AVAIL oe-ordl AND s-prt-sellprc @ eb.upc-no
                    skip
                with stream-io width 175 no-labels no-box frame line-det1.
            */
            v-itm-printed = v-itm-printed + 1.    

            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".
          
          IF LAST-OF(eb.form-no) THEN DO:
               
             /* Number of sheets */
             run oe/rep/ticket1.p (recid(ef), recid(job-hdr)).
             find first wrk-sheet where recid(wrk-sheet) eq save_id.
             IF AVAIL oe-ordl THEN
               FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                            AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.

             ASSIGN
             v-vend = /*IF AVAIL po-ord THEN po-ord.vend-no ELSE "".*/
                      IF AVAIL oe-ordl THEN oe-ordl.vend-no ELSE ""
             v-board-po = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0.
             IF AVAIL po-ord THEN
                FIND FIRST po-ordl WHERE
                     po-ordl.company EQ po-ord.company AND
                     po-ordl.po-no   EQ po-ord.po-no AND
                     po-ordl.i-no = ef.board
                     NO-LOCK NO-ERROR.
                 
             v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.
          END. /* last-of(eb.form-no) */
          
        end. /* each eb */
      end. /* each ef */
      end. /* first job-no */

      
      if last-of(job-hdr.frm) then do:
        
        /* dept notes*/
        lv-line-chars = 128.
        FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       

        {custom/notespr4.i job v-inst2 20 "notes.rec_key = job.rec_key and (notes.note_form_no = job-hdr.frm or notes.note_form_no = 0) and notes.note_code <> '' AND LOOKUP(notes.note_code,v-exc-depts) EQ 0 AND notes.note_type NE 'O' "}
        DO i = 1 TO 20:
           v-dept-inst[i] = v-inst2[i].
        END.
        ASSIGN x = 2
                i = 1
                /*v-ink1 = ""
                v-ink2 = ""*/
                v-pass-count = 0.             
             ASSIGN
                v-skip = NO
                v-plate-printed = NO.

        i = 1.
        v-fgitm = "".
        for each xjob-hdr where xjob-hdr.company eq cocode
                            and xjob-hdr.job     eq job-hdr.job
                            and xjob-hdr.job-no  eq job-hdr.job-no
                            and xjob-hdr.job-no2 eq job-hdr.job-no2
                            and xjob-hdr.frm     eq job-hdr.frm NO-LOCK BY xjob-hdr.blank-no:

           find first xoe-ordl
                where xoe-ordl.company eq xjob-hdr.company
                  and xoe-ordl.ord-no  eq xjob-hdr.ord-no
                  and xoe-ordl.job-no  eq xjob-hdr.job-no
                  and xoe-ordl.job-no2 eq xjob-hdr.job-no2
                  and xoe-ordl.i-no    eq xjob-hdr.i-no
                  no-lock no-error.
           IF AVAIL xoe-ordl THEN v-fgqty[i] = xoe-ordl.cas-cnt.
           /*FIND FIRST itemfg WHERE itemfg.company = xjob-hdr.company AND
                                   itemfg.i-no = xjob-hdr.i-no NO-LOCK NO-ERROR.*/
           FIND FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                             AND b-eb.est-no  eq xjob-hdr.est-no
                             AND b-eb.form-no = xjob-hdr.frm
                             AND b-eb.blank-no = xjob-hdr.blank-no
                             AND b-eb.stock-no = xjob-hdr.i-no
                             NO-LOCK NO-ERROR.

           IF NOT AVAIL b-eb THEN 
               FIND FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                             AND b-eb.est-no  eq xjob-hdr.est-no
                             AND b-eb.form-no = xjob-hdr.frm
                             /*AND b-eb.blank-no = xjob-hdr.blank-no*/
                             AND b-eb.stock-no = xjob-hdr.i-no
                             NO-LOCK NO-ERROR.

           IF NOT AVAIL b-eb THEN 
               FIND FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                             AND b-eb.est-no  eq xjob-hdr.est-no
                             /*AND b-eb.form-no = xjob-hdr.frm*/
                             /*AND b-eb.blank-no = xjob-hdr.blank-no*/
                             AND b-eb.stock-no = xjob-hdr.i-no
                             NO-LOCK NO-ERROR.
.          FIND first wrk-sheet WHERE wrk-sheet.form-no = job-hdr.frm NO-LOCK NO-ERROR.
           IF AVAIL xoe-ordl AND s-show-release THEN DO:
                FOR EACH b-oe-rel2 WHERE b-oe-rel2.company eq cocode AND
                         b-oe-rel2.ord-no  eq xoe-ordl.ord-no AND
                         b-oe-rel2.i-no    eq xoe-ordl.i-no AND
                         b-oe-rel2.line    eq xoe-ordl.LINE
                         NO-LOCK:
                    FIND FIRST reftable
                        WHERE reftable.reftable EQ "oe-rel.lot-no"
                        AND reftable.company  EQ STRING(b-oe-rel2.r-no,"9999999999") NO-LOCK NO-ERROR.

                    FIND FIRST oe-rell
                        WHERE oe-rell.company  EQ b-oe-rel2.company
                        AND oe-rell.r-no     EQ b-oe-rel2.link-no
                        AND oe-rell.ord-no   EQ b-oe-rel2.ord-no
                        AND oe-rell.rel-no   EQ b-oe-rel2.rel-no
                        AND oe-rell.i-no     EQ b-oe-rel2.i-no
                        AND oe-rell.line     EQ b-oe-rel2.line
                        AND oe-rell.po-no    EQ b-oe-rel2.po-no NO-LOCK NO-ERROR.
                        
                    IF AVAIL oe-rell THEN
                            FIND FIRST oe-relh WHERE
                            oe-relh.r-no     EQ oe-rell.r-no NO-LOCK NO-ERROR.

                        
                    RUN AddTTFGItem (b-oe-rel2.tot-qty, b-oe-rel2.po-no, IF AVAIL reftable THEN reftable.CODE ELSE "", 
                        IF AVAIL oe-relh THEN string(oe-relh.rel-date) ELSE string(b-oe-rel2.rel-date) ).
                END.
           END.
           ELSE
                RUN AddTTFGItem (IF AVAIL xoe-ordl THEN xoe-ordl.qty ELSE xjob-hdr.qty, 
                                IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE "", 
                                IF AVAIL xoe-ordl THEN substring(xoe-ordl.part-dscr2,1,8) ELSE "", "").
/*            CREATE tt-fgitm.                                                                          */
/*            ASSIGN tt-fgitm.i-no = xjob-hdr.i-no                                                      */
/*                   tt-fgitm.qty = /*xjob-hdr.qty*/                                                    */
/*                                   IF AVAIL xoe-ordl THEN xoe-ordl.qty ELSE xjob-hdr.qty              */
/*                   tt-fgitm.i-dscr = IF AVAIL b-eb THEN b-eb.part-dscr1 ELSE xjob-hdr.i-no            */
/*                   tt-fgitm.po-no = IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE ""                     */
/*                   tt-fgitm.due-date = IF AVAIL xoe-ordl THEN xoe-ordl.req-date ELSE job-hdr.due-date */
/*                   tt-fgitm.pp# = IF AVAIL b-eb THEN b-eb.spc-no ELSE ""                              */
/*                   tt-fgitm.rfq# = IF AVAIL xoe-ordl THEN substring(xoe-ordl.part-dscr2,1,8) ELSE ""  */
/*                   tt-fgitm.seq = i                                                                   */
/*                   tt-fgitm.form-no = xjob-hdr.frm                                                    */
/*                   tt-fgitm.blank-no = xjob-hdr.blank-no                                              */
/*                   tt-fgitm.NumUp = IF AVAIL b-eb THEN b-eb.num-up ELSE 0                             */
/*                   tt-fgitm.SheetQty = IF AVAIL wrk-sheet THEN wrk-sheet.gsh-qty ELSE 0               */
/*                   .                                                                                  */
/*                                                                                                      */
/*            FIND FIRST b-est WHERE                                                                    */
/*                 b-est.company  eq xjob-hdr.company AND                                               */
/*                 b-est.est-no   EQ xjob-hdr.est-no                                                    */
/*                 NO-LOCK NO-ERROR.                                                                    */
/*                                                                                                      */
/*            IF AVAIL b-est AND b-est.est-type EQ 4 THEN /*combo*/                                     */
/*            DO:                                                                                       */
/*               FIND FIRST b-cust WHERE                                                                */
/*                    b-cust.company EQ xjob-hdr.company AND                                            */
/*                    b-cust.cust-no EQ xjob-hdr.cust-no                                                */
/*                    NO-LOCK NO-ERROR.                                                                 */
/*                                                                                                      */
/*               IF AVAIL b-cust THEN                                                                   */
/*               DO:                                                                                    */
/*                  tt-fgitm.cust-name = b-cust.NAME.                                                   */
/*                                                                                                      */
/*                  FIND FIRST b-oe-ordl WHERE                                                          */
/*                       b-oe-ordl.company eq xjob-hdr.company AND                                      */
/*                       b-oe-ordl.ord-no  eq xjob-hdr.ord-no AND                                       */
/*                       b-oe-ordl.job-no  eq xjob-hdr.job-no AND                                       */
/*                       b-oe-ordl.job-no2 eq xjob-hdr.job-no2 AND                                      */
/*                       b-oe-ordl.i-no    eq xjob-hdr.i-no                                             */
/*                       no-lock no-error.                                                              */
/*                                                                                                      */
/*                  IF AVAIL b-oe-ordl THEN                                                             */
/*                     find first b-oe-rel WHERE                                                        */
/*                          b-oe-rel.company eq cocode AND                                              */
/*                          b-oe-rel.ord-no  eq b-oe-ordl.ord-no AND                                    */
/*                          b-oe-rel.i-no    eq b-oe-ordl.i-no AND                                      */
/*                          b-oe-rel.line    eq b-oe-ordl.LINE                                          */
/*                          no-lock no-error.                                                           */
/*                                                                                                      */
/*                   if avail b-oe-rel then do:                                                         */
/*                      find first b-shipto WHERE                                                       */
/*                           b-shipto.company eq cocode AND                                             */
/*                           b-shipto.cust-no eq b-oe-rel.cust-no AND                                   */
/*                           b-shipto.ship-id eq b-oe-rel.ship-id                                       */
/*                           no-lock no-error.                                                          */
/*                                                                                                      */
/*                     if avail b-shipto then                                                           */
/*                         ASSIGN                                                                       */
/*                           tt-fgitm.shipto1 = b-shipto.ship-name                                      */
/*                           tt-fgitm.shipto2 = b-shipto.ship-addr[1]                                   */
/*                           tt-fgitm.shipto4 = trim(b-oe-rel.ship-city) + ", " +                       */
/*                                              b-oe-rel.ship-state + "  " + b-oe-rel.ship-zip.         */
/*                                                                                                      */
/*                         RELEASE b-cust.                                                              */
/*                         RELEASE b-oe-ordl.                                                           */
/*                         RELEASE b-oe-rel.                                                            */
/*                         RELEASE b-shipto.                                                            */
/*                   END.                                                                               */
/*               END.                                                                                   */
/*            END.                                                                                      */

           i = i + 1.          

        END.
        
        IF s-prt-shipto THEN DO i = 1 TO 4:
            ASSIGN v-shipto1[i] = v-shipto[i]
                   v-shipto2[i] = v-shipto[i].
        END.
        ASSIGN v-cust-name2 = v-cust-name 
               v-cust-name3 = v-cust-name
               .
        /* label prints per item */
        
        i = 0.
        j = 0.                
      end. /* last-of job-hdr.frm */
      
      IF LAST(job-hdr.frm) THEN DO:
          /*PUT UNFORMATTED
                "~t~t~t~tFACTORY TICKET" SKIP(2)
     "JOB #:~t" v-job-no space(0) "-" space(0) v-job-no2 format "99" "~t"
     "~tPREVIOUS JOB#:~t~t" vPrevJob "~t~t~tESTIMATE#:~t" vEstno   SKIP(1).   
            */
          
          RUN BuildExcelDataSheet (1,2,v-job-no + "-" + string(v-job-no2,"99")).
          RUN BuildExcelDataSheet (2,2,vPrevJob).
          RUN BuildExcelDataSheet (3,2,vEstNo).          

/*
          PUT unformatted
       "CUSTOMER NAME:~t~t" v-cust-name  "~t~t~t~tORDER DATE:~t" v-start-date SKIP
       "SOLD TO:~t~t" v-shipto[1] "~t~t~t~tTERMS:~t"
       SKIP
       "~t~t" v-shipto[2] "~t~t~t~t SHIP:~t LTL _______ ~tPREPAID ______" SKIP
       "~t~t" v-shipto[4] "~t~t~t~t~t TL _____ ~tCOLLECT ______" SKIP
       .     
  */
          RUN BuildExcelDataSheet (4,2,v-cust-name).
          RUN BuildExcelDataSheet (5,2,v-shipto[1]).
          RUN BuildExcelDataSheet (6,2,v-shipto[2]).
          RUN BuildExcelDataSheet (7,2,v-shipto[4]).
          RUN BuildExcelDataSheet (8,2,v-start-date).
          RUN BuildExcelDataSheet (9,2,v-term).
          
        
/*
            PUT /*"<P10>" v-fill SKIP                       /*REQ'D*/
                 "<B>BOARD CODE            DUE DATE    SHEETS SHEET SIZE   NET SHEET  BOARD PO# VENDOR#  Die Size     Die#            CAD#</B>"
                 */
                 UNFORMATTED SKIP
                 "BOARD ~t ~t ~tDRAWING ~t ~t ~t~t CASES/" SKIP
                 "DESCRIPTION ~t SHEET SIZE ~t DIE # ~t#~t #UP ~t CORRUGATED/PAD ~t CASE COUNT ~t PALLET ~t PALLETS "
                 SKIP.
       */
            
  
          lBoardPrinted = NO.
          for each wrk-sheet break by wrk-sheet.form-no:               
              
              IF wrk-sheet.brd-dscr = "" THEN NEXT.
              FIND first eb WHERE eb.company     EQ job-hdr.company
                           AND eb.est-no      eq job-hdr.est-no
                           and eb.form-no     eq job-hdr.frm NO-LOCK NO-ERROR.
              IF NOT AVAIL eb THEN
                 FIND FIRST eb WHERE eb.company     EQ job-hdr.company
                           AND eb.est-no      eq job-hdr.est-no NO-LOCK NO-ERROR.
              FIND FIRST ef OF eb NO-LOCK NO-ERROR.
        /*
               put wrk-sheet.brd-dscr "~t"
                    /*(v-po-duedate wrk-sheet.gsh-qty */
                    /*string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len)*/
                    string(ef.gsh-wid) + "x" + string(ef.gsh-len)
                    format "x(13)" "~t"
                    eb.die-no  "~t"
                    eb.cad-no "~t"
                   /* string(ef.nsh-wid) + "x" + string(ef.nsh-len) FORM "x(13)"
                    v-board-po  
                    v-vend  
                    string(ef.trim-w) + "x" + string(ef.trim-l) FORM "x(12)"
                    eb.cad-no */
                    /*"Caliper:" AT 68 wrk-sheet.cal space(5) "Board:"
                    wrk-sheet.brd-dscr "Form:" AT 123 wrk-sheet.form-no*/
                   v-up "~t"
                   eb.cas-no + "/" + eb.layer-pad FORM "x(20)" "~t"
                   eb.tr-cnt "~t"
                   eb.cas-pal "~t"
                   eb.tr-no "~t" SKIP.
                   /* with stream-io width 170 no-labels no-box frame sheet.*/
*/
              
               RUN BuildExcelDataSheet (10,2, wrk-sheet.brd-dscr).
                    RUN BuildExcelDataSheet (11,2,string(ef.gsh-wid) + " x " + string(ef.gsh-len)).
                    RUN BuildExcelDataSheet (12,2,eb.die-no).
                    RUN BuildExcelDataSheet (13,2,eb.cad-no).
                    RUN BuildExcelDataSheet (14,2,string(v-up)).
                    RUN BuildExcelDataSheet (15,2,eb.cas-no + "/" + eb.layer-pad).
                    RUN BuildExcelDataSheet (16,2,string(eb.cas-cnt)).
                    RUN BuildExcelDataSheet (17,2,string(eb.cas-pal)).
                    RUN BuildExcelDataSheet (18,2,eb.tr-no).
              
               LEAVE.
          end. /* each wrk-sheet */


         /*
         PUT UNFORMATTED skip(1)
             "SHEETER(" + string(dSheeterSpeed) + "/HR)  ~t~t  PRINTER(" + string(dPrinterSpeed) + "/HR) ~t~t  CUTTER(" + string(dCutterSpeed) + "/HR)  ~t~t  GLUER ~t~t~tFLAT PACK" SKIP.
         ASSIGN cOPDscr = ""
                cOPRun = ""
                cOpDscr2 = ""
                cOpRun2 = ""
                .
         */
          
         RUN BuildExcelDataSheet (20,2,dSheeterSpeed).
         RUN BuildExcelDataSheet (24,2,dPrinterSpeed).
         RUN BuildExcelDataSheet (28,2,dCutterSpeed).
         RUN BuildExcelDataSheet (32,2,dGluerSpeed).
         
         FOR EACH wrk-op WHERE /*wrk-op.s-num = job-hdr.frm*/ BREAK by wrk-op.d-seq by wrk-op.b-num:             

            IF NOT can-do("RS,PR,DC,GL,FP",wrk-op.dept)  THEN NEXT.            

            IF FIRST-OF(wrk-op.d-seq) THEN DO:
               ASSIGN dOpMr = 0
                      dOpRun = 0
                      .                                     
            END.
            DO i = 1 TO 100:
              ASSIGN dOpMr = dOpMr + wrk-op.mr[i]
                     dOpRun = dOpRun + wrk-op.run-hr[i].
            END.
            IF LAST-OF(wrk-op.d-seq) THEN DO:
               CASE wrk-op.dept:
                   WHEN "RS" THEN ASSIGN cOpDscr2[1] = wrk-op.m-dscr
                                         cOpRun2[1] =  string(dOpMr) + "/" + STRING(dOpRun) .
                   WHEN "PR" THEN ASSIGN cOpDscr2[2] = wrk-op.m-dscr
                                         cOpRun2[2] =  string(dOpMr) + "/" + STRING(dOpRun) .
                   WHEN "DC" THEN ASSIGN cOpDscr2[3] = wrk-op.m-dscr
                                         cOpRun2[3] =  string(dOpMr) + "/" + STRING(dOpRun) .
                   WHEN "GL" THEN ASSIGN cOpDscr2[4] = wrk-op.m-dscr
                                         cOpRun2[4] =  string(dOpMr) + "/" + STRING(dOpRun) .
                   WHEN "FP" THEN ASSIGN cOpDscr2[5] = wrk-op.m-dscr
                                         cOpRun2[5] =  string(dOpMr) + "/" + STRING(dOpRun) .
               END CASE.
            END.
            /*
            IF LAST-OF(wrk-op.d-seq) THEN
               ASSIGN cOPDscr = cOPDscr + IF iOPcnt = 1 AND wrk-op.dept = "RS" THEN wrk-op.m-dscr + "~t~t"
              


              ASSIGN cOPDscr = cOPDscr + (IF wrk-op.dept <> "FP" THEN "" ELSE "~t") + wrk-op.m-dscr + "~t~t"
                     cOPRun = cOPRun + (IF wrk-op.dept <> "FP" THEN "" ELSE "~t") +
                              string(dOpMr) + "/" + STRING(dOpRun) + "~t~t".
            */                    
        end. /* each wrk-op*/
        ASSIGN cOPDscr = cOpDscr2[1] + "~t~t" + cOpDscr2[2] + "~t~t" +
                         cOpDscr2[3] + "~t~t" + cOpDscr2[4] + "~t~t~t" + cOpDscr2[5] 
               cOPRun = cOpRun2[1] + "~t~t" + cOpRun2[2] + "~t~t" +
                         cOpRun2[3] + "~t~t" + cOpRun2[4] + "~t~t~t" + cOpRun2[5] 
               .
/*
        PUT UNFORMATTED /*v-fill AT 1 SKIP.*/
            cOPDscr SKIP
            "MAKE READY/RUN HOURS ~t~t MAKE READY/RUN HOURS ~t~t MAKE READY/RUN HOURS ~t~t MAKE READY/RUN HOURS ~t~t~tMAKE READY/RUN HOURS" SKIP
            cOPRun SKIP(1).
  */                         

        RUN BuildExcelDataSheet (19,2,cOpDscr2[1]).  /*sheeter*/
        RUN BuildExcelDataSheet (23,2,cOpDscr2[2]).  /*Printer*/
        RUN BuildExcelDataSheet (27,2,cOpDscr2[3]).  /*Cutter */
        RUN BuildExcelDataSheet (31,2,cOpDscr2[4]).  /*Gluer*/
        RUN BuildExcelDataSheet (35,2,cOpDscr2[5]).  /*FlatPack*/

       DO i = 1 TO 5:
          IF cOpRun2[i] = "" THEN cOpRun2[i] = "/".
       END.
      /*MESSAGE "p1" SKIP
          "1:" cOpRun2[1] ":"  entry(1,cOpRun2[1],"/") = "" ":" entry(2,cOpRun2[1],"/") = "" SKIP
          "2:" cOpRun2[2] ":"  entry(1,cOpRun2[2],"/") = "" ":" entry(2,cOpRun2[2],"/") = "" SKIP
          "3:" cOpRun2[3] ":"  entry(1,cOpRun2[3],"/") = "" ":" entry(2,cOpRun2[3],"/") = "" SKIP
          "4:" cOpRun2[4] ":"  entry(1,cOpRun2[4],"/") = "" ":" entry(2,cOpRun2[4],"/") = "" SKIP
          "5:" cOpRun2[5] ":"  entry(1,cOpRun2[5],"/") = "" ":" entry(2,cOpRun2[5],"/") = "" SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
       */
        IF ENTRY(1,cOpRun2[1],"/") <> "" THEN
           RUN BuildExcelDataSheet (21,2,entry(1,cOpRun2[1],"/") ). /*sheeter MR*/
        IF ENTRY(2,cOpRun2[1],"/") <> "" THEN
           RUN BuildExcelDataSheet (22,2,entry(2,cOpRun2[1],"/") ). /*sheeter Run*/
        IF ENTRY(1,cOpRun2[2],"/") <> "" THEN
           RUN BuildExcelDataSheet (25,2,entry(1,cOpRun2[2],"/")). /*Printer MR*/
        IF ENTRY(2,cOpRun2[2],"/") <> "" THEN
           RUN BuildExcelDataSheet (26,2,entry(2,cOpRun2[2],"/")). /*Printer Run*/       
        IF ENTRY(1,cOpRun2[3],"/") <> "" THEN
           RUN BuildExcelDataSheet (29,2,entry(1,cOpRun2[3],"/")). /*Cutter  MR*/
        IF ENTRY(2,cOpRun2[3],"/") <> "" THEN
           RUN BuildExcelDataSheet (30,2,entry(2,cOpRun2[3],"/")). /* Cutter Run*/
        IF ENTRY(1,cOpRun2[4],"/") <> "" THEN
           RUN BuildExcelDataSheet (33,2,entry(1,cOpRun2[4],"/")). /* Gluer  MR*/
        IF ENTRY(2,cOpRun2[4],"/") <> "" THEN
           RUN BuildExcelDataSheet (34,2,entry(2,cOpRun2[4],"/")). /* Gluer  Run*/
        IF ENTRY(1,cOpRun2[5],"/") <> "" THEN
           RUN BuildExcelDataSheet (36,2,entry(1,cOpRun2[5],"/")). /* Flat Pack MR*/
        IF ENTRY(2,cOpRun2[5],"/") <> "" THEN
           RUN BuildExcelDataSheet (37,2,entry(2,cOpRun2[5],"/")). /*Flat Pack Run*/
                
        RUN BuildExcelDataSheet (38,2,cTotalSheet). /*Flat Pack Run*/
        
        /*
          PUT UNFORMATTED 
              "COMMENT/SPECIAL INSTRUCTIONS" "~t~t~t~t~t~t~t PRODUCTION RELEASE" SKIP
              v-dept-inst[1] FORM "x(80)"     SKIP
              v-dept-inst[2] FORM "x(80)"    "~t~t~t~t~t~t~t LAYOUT: ____________________" SKIP
              v-dept-inst[3] FORM "x(80)"    "~t~t~t~t~t~t~t SIZE & STYLE: ______________" SKIP
              v-dept-inst[4] FORM "x(80)"      SKIP
              v-dept-inst[5] FORM "x(80)"    "~t~t~t~t~t~t~t COPY: ______________________"  SKIP
              v-dept-inst[6] FORM "x(80)"    "~t~t~t~t~t~t~t COLOR: _____________________" SKIP
              v-dept-inst[7] FORM "x(80)"    SKIP
              v-dept-inst[8] FORM "x(80)"    "~t~t~t~t~t~t~t RELEASED BY: _______________"  SKIP
              v-dept-inst[9] FORM "x(80)"    SKIP
              v-dept-inst[10] FORM "x(80)"   SKIP
              v-dept-inst[11] FORM "x(80)"   SKIP
              v-dept-inst[12] FORM "x(80)"   SKIP
              v-dept-inst[13] FORM "x(80)"   /*SKIP
              v-dept-inst[14] FORM "x(80)"   SKIP
              v-dept-inst[15] FORM "x(80)"   SKIP
              v-dept-inst[16] FORM "x(80)"   SKIP
              v-dept-inst[17] FORM "x(80)"   SKIP
              v-dept-inst[18] FORM "x(80)"   SKIP */
              .
          */
        
           RUN BuildExcelDataSheet (51,2,v-dept-inst[1]). 
           RUN BuildExcelDataSheet (52,2,v-dept-inst[2]). 
           RUN BuildExcelDataSheet (53,2,v-dept-inst[3]). 
           RUN BuildExcelDataSheet (54,2,v-dept-inst[4]). 
           RUN BuildExcelDataSheet (55,2,v-dept-inst[5]). 
           RUN BuildExcelDataSheet (56,2,v-dept-inst[6]). 
           RUN BuildExcelDataSheet (57,2,v-dept-inst[7]). 
           RUN BuildExcelDataSheet (58,2,v-dept-inst[8]). 
           RUN BuildExcelDataSheet (59,2,v-dept-inst[9]). 
           RUN BuildExcelDataSheet (60,2,v-dept-inst[10]). 
           RUN BuildExcelDataSheet (61,2,v-dept-inst[11]). 
           RUN BuildExcelDataSheet (62,2,v-dept-inst[12]). 
           RUN BuildExcelDataSheet (63,2,v-dept-inst[13]). 
           RUN BuildExcelDataSheet (64,2,v-dept-inst[14]). 
           RUN BuildExcelDataSheet (65,2,v-dept-inst[15]). 
           RUN BuildExcelDataSheet (66,2,v-dept-inst[16]). 
           RUN BuildExcelDataSheet (67,2,v-dept-inst[17]). 
           RUN BuildExcelDataSheet (68,2,v-dept-inst[18]).                     
           
         /*            
         PUT STREAM st2nd UNFORMATTED
                   "~t       BREAKOUT-JOB#:  " job-hdr.job-no + "-" + string(job-hdr.job-no2,"99") 
                   "~t~t~t~t~t~t~t~t~t        INK & ROTATION " SKIP
                   "CUSTOMER PART# ~t DESCRIPTION ~t PP# ~t ORDER QTY: ~t#UP~t#PO# ~t RFQ# ~t SHEET QTY. ~t DELIVERY DATE ~t UNIT 1 ~t UNIT 2 ~t UNIT 3 ~t UNIT 4 ~t UNIT 5 ~t UNIT 6 ~t COATING" SKIP
                   .
          */         
         /*===
         i = 1.
         for each wrk-ink /*WHERE wrk-ink.form-no = job-hdr.frm*/
                break /*by wrk-ink.i-pass*/
                   BY wrk-ink.i-code
                      /*BY wrk-ink.blank-no*/: 
             IF LAST-OF(wrk-ink.i-code) THEN DO:                
                FIND first ITEM {sys/look/itemivW.i} and item.i-no eq wrk-ink.i-code NO-LOCK NO-ERROR.
                IF AVAIL ITEM AND ITEM.mat-type = "V" THEN ASSIGN v-ink1[7] = wrk-ink.i-dscr.
                ELSE IF i < 7 THEN ASSIGN v-ink1[i] = wrk-ink.i-dscr
                                          i = i + 1.
             END.             
         END. /* each wrk-ink */
         =====*/
         iExcelRowCount = 2.
         FOR EACH tt-fgitm BY tt-fgitm.seq.
             ASSIGN i = 0
                    v-ink1 = ""
                    .
          
           for each wrk-ink WHERE wrk-ink.form-no = tt-fgitm.form-no
               break BY wrk-ink.blank-no BY wrk-ink.seq
                      /*BY wrk-ink.blank-no*/: 
               IF wrk-ink.seq <= 7 THEN DO:
                  FIND first ITEM {sys/look/itemivW.i} and item.i-no eq wrk-ink.i-code NO-LOCK NO-ERROR.
                  IF AVAIL ITEM AND ITEM.mat-type = "V" THEN ASSIGN v-ink1[7] = wrk-ink.i-dscr.
                  ELSE ASSIGN v-ink1[wrk-ink.seq] = wrk-ink.i-code + "/" + wrk-ink.i-dscr.
               END.    
               i = i + 1.
               IF LAST-OF(wrk-ink.blank-no) AND i <> 1 THEN DO:            
                   LEAVE.
               END.
           END. /* each wrk-ink */
        /* 
           PUT STREAM st2nd unformatted
               tt-fgitm.i-no "~t" tt-fgitm.i-dscr "~t"
               tt-fgitm.pp# "~t"
               tt-fgitm.qty "~t"
               tt-fgitm.NumUp "~t"
               tt-fgitm.po-no   "~t"
               tt-fgitm.RFQ# "~t"
               tt-fgitm.sheetQty "~t"
               tt-fgitm.due-date "~t"
               v-ink1[1] "~t"
               v-ink1[2] "~t"
               v-ink1[3] "~t"
               v-ink1[4] "~t"
               v-ink1[5] "~t"
               v-ink1[6] "~t"
               v-ink1[7] 
               SKIP
               .
           */               
              iExcelRowCount = iExcelRowCount + 1.
    
              RUN BuildExcelDataSheet (iExcelRowCount,4,tt-fgitm.i-no). 
              RUN BuildExcelDataSheet (iExcelRowCount,5,tt-fgitm.i-dscr). 
              RUN BuildExcelDataSheet (iExcelRowCount,6,tt-fgitm.pp#). 
              RUN BuildExcelDataSheet (iExcelRowCount,7,tt-fgitm.qty). 
              RUN BuildExcelDataSheet (iExcelRowCount,8,tt-fgitm.NumUp). 
              RUN BuildExcelDataSheet (iExcelRowCount,9,tt-fgitm.po-no). 
              RUN BuildExcelDataSheet (iExcelRowCount,10,tt-fgitm.RFQ#). 
              RUN BuildExcelDataSheet (iExcelRowCount,11,tt-fgitm.sheetQty). 
              RUN BuildExcelDataSheet (iExcelRowCount,12,string(tt-fgitm.due-date,"99/99/99")). 
              
              IF v-ink1[1] <> "" THEN DO:
              RUN BuildExcelDataSheet (iExcelRowCount,13,entry(1,v-ink1[1],"/")). 
              RUN BuildExcelDataSheet (iExcelRowCount,14,entry(2,v-ink1[1],"/")). 
              END.
              IF v-ink1[2] <> "" THEN DO:
              RUN BuildExcelDataSheet (iExcelRowCount,15,entry(1,v-ink1[2],"/")). 
              RUN BuildExcelDataSheet (iExcelRowCount,16,entry(2,v-ink1[2],"/")). 
              END.
              IF v-ink1[3] <> "" THEN DO:
              RUN BuildExcelDataSheet (iExcelRowCount,17,entry(1,v-ink1[3],"/")). 
              RUN BuildExcelDataSheet (iExcelRowCount,18,entry(2,v-ink1[3],"/")). 
              END.
              IF v-ink1[4] <> "" THEN DO:
              RUN BuildExcelDataSheet (iExcelRowCount,19,entry(1,v-ink1[4],"/")). 
              RUN BuildExcelDataSheet (iExcelRowCount,20,entry(2,v-ink1[4],"/")). 
              END.
              IF v-ink1[5] <> "" THEN DO:
              RUN BuildExcelDataSheet (iExcelRowCount,21,entry(1,v-ink1[5],"/")). 
              RUN BuildExcelDataSheet (iExcelRowCount,22,entry(2,v-ink1[5],"/")). 
              END.
              IF v-ink1[6] <> "" THEN DO:
              RUN BuildExcelDataSheet (iExcelRowCount,23,entry(1,v-ink1[6],"/")). 
              RUN BuildExcelDataSheet (iExcelRowCount,24,entry(2,v-ink1[6],"/")). 
              END.
              
              RUN BuildExcelDataSheet (iExcelRowCount,25,v-ink1[7]). 
              
              

        v-ink1 = "".

        END.


      END.

      /** PRINT MULT COPIES OF TICKETS **/
      save_id = recid(job-hdr).
      if last-of(job-hdr.job-no2) then do:
        for each wrk-op:
          delete wrk-op.
        end.
        for each wrk-prep:
          delete wrk-prep.
        end.
        for each wrk-sheet:
            delete wrk-sheet.
        end.
      end.

      for each wrk-spec:
        delete wrk-spec.
      end.
      for each wrk-film:
        delete wrk-film.
      end.
      for each wrk-die:
        delete wrk-die.
      end.
      /*
      for each wrk-sheet:
        delete wrk-sheet.
      end.
      */
      for each wrk-misc:
        delete wrk-misc.
      end.
      for each wrk-inst:
        delete wrk-inst.
      end.
                  
      v-first = no.
       
    end. /* each job-hdr */

    

  OUTPUT STREAM st2nd CLOSE. 
  OUTPUT CLOSE.
END PROCEDURE.  /* JobReport */

PROCEDURE CreateExcelSheet:

  RUN UTIL/CurrDir.p (output CurrDir).
  ASSIGN
     cFileName = CurrDir + "\Template\PrystupJob.xlt" NO-ERROR.
  /*cFileName = "c:\tmp\PrystupJobTemp.xlsx" NO-ERROR. */

   /* Create excel automation*/
  CREATE "excel.application" chExcel.
  /*chExcel:VISIBLE = TRUE.*/

  /* Open an Excel    */
  chExcel:Workbooks:OPEN(cFileName).

  /* open  sheet*/
  chWorkbook  = chExcel:ActiveWorkbook.
  chWorkSheet = chExcel:Sheets:Item("Data").  /* 3rd Sheet */
  chExcel:ScreenUpdating = no.

  /*============*/

END PROCEDURE. /* CreateExceSheet */

PROCEDURE BuildExcelDataSheet:
  DEF INPUT PARAM  ipRowNum AS INT NO-UNDO.
  DEF INPUT PARAM  ipColNum AS INT NO-UNDO.
  DEF INPUT PARAM cpCellValue AS cha NO-UNDO.
  
  IF cpCellValue = "" THEN RETURN.
  
  chWorkSheet:Cells(ipRowNum,ipColNum):VALUE = cpCellValue.
        
  /*chWorkSheet:Cells(ipRowNum,ipColNum):SELECT().   
  chExcel:SELECTION():VALUE = cpCellValue.
  */
END PROCEDURE. /*BuildExcelSheet */

PROCEDURE CloseExcel:

    chWorkbook:saveAs("c:\tmp\JobPryst.xls", -4143,,,,,,, TRUE).
    /*
    chWorkbook:Close(no) no-error.   
    chExcel:Quit() no-error.
    */
      
END PROCEDURE.

PROCEDURE ReOpenExcel:
      /* Open an Excel    */
  chExcel:Workbooks:OPEN("c:\tmp\JobPryst.xls").
  
END PROCEDURE. /*ReOpenExcel*/

PROCEDURE ReleaseExcelObject:
    RELEASE OBJECT chWorksheet.
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT    chExcel .

END PROCEDURE.  /*ReleaseExcelObject */

PROCEDURE AddTTFGItem:
    DEFINE INPUT PARAMETER ip-qty LIKE xoe-ordl.qty NO-UNDO.
    DEFINE INPUT PARAMETER ip-PO LIKE xoe-ordl.po-no NO-UNDO.
    DEFINE INPUT PARAMETER ip-RFQ AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ip-date AS CHAR NO-UNDO .

    CREATE tt-fgitm.
    ASSIGN tt-fgitm.i-no = xjob-hdr.i-no
          tt-fgitm.qty = ip-qty 
/*         /*xjob-hdr.qty*/                                                        */
/*                           IF AVAIL xoe-ordl THEN xoe-ordl.qty ELSE xjob-hdr.qty */
          tt-fgitm.i-dscr = IF AVAIL b-eb THEN b-eb.part-dscr1 ELSE xjob-hdr.i-no
          tt-fgitm.po-no = ip-PO 
/*               IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE "" */
          tt-fgitm.due-date = IF AVAIL xoe-ordl THEN xoe-ordl.req-date ELSE job-hdr.due-date
          tt-fgitm.pp# = IF AVAIL b-eb THEN b-eb.spc-no ELSE ""
          tt-fgitm.rfq# = ip-RFQ
/*               IF AVAIL xoe-ordl THEN substring(xoe-ordl.part-dscr2,1,8) ELSE "" */
          tt-fgitm.seq = i
          tt-fgitm.form-no = xjob-hdr.frm
          tt-fgitm.blank-no = xjob-hdr.blank-no
          tt-fgitm.NumUp = IF AVAIL b-eb THEN b-eb.num-up ELSE 0    
          tt-fgitm.SheetQty = IF AVAIL wrk-sheet THEN wrk-sheet.gsh-qty ELSE 0    
          .
     IF ip-date NE "" THEN
         ASSIGN tt-fgitm.due-date  = DATE(ip-date) .
 
   FIND FIRST b-est WHERE
        b-est.company  eq xjob-hdr.company AND
        b-est.est-no   EQ xjob-hdr.est-no
        NO-LOCK NO-ERROR.

   IF AVAIL b-est AND b-est.est-type EQ 4 THEN /*combo*/
   DO:
      FIND FIRST b-cust WHERE
           b-cust.company EQ xjob-hdr.company AND
           b-cust.cust-no EQ xjob-hdr.cust-no
           NO-LOCK NO-ERROR.

      IF AVAIL b-cust THEN
      DO:
         tt-fgitm.cust-name = b-cust.NAME.

         FIND FIRST b-oe-ordl WHERE
              b-oe-ordl.company eq xjob-hdr.company AND
              b-oe-ordl.ord-no  eq xjob-hdr.ord-no AND
              b-oe-ordl.job-no  eq xjob-hdr.job-no AND
              b-oe-ordl.job-no2 eq xjob-hdr.job-no2 AND
              b-oe-ordl.i-no    eq xjob-hdr.i-no
              no-lock no-error.

         IF AVAIL b-oe-ordl THEN 
            find first b-oe-rel WHERE
                 b-oe-rel.company eq cocode AND
                 b-oe-rel.ord-no  eq b-oe-ordl.ord-no AND
                 b-oe-rel.i-no    eq b-oe-ordl.i-no AND
                 b-oe-rel.line    eq b-oe-ordl.LINE
                 no-lock no-error.

          if avail b-oe-rel then do:
             find first b-shipto WHERE
                  b-shipto.company eq cocode AND
                  b-shipto.cust-no eq b-oe-rel.cust-no AND
                  b-shipto.ship-id eq b-oe-rel.ship-id
                  no-lock no-error.  
         
            if avail b-shipto then
                ASSIGN
                  tt-fgitm.shipto1 = b-shipto.ship-name
                  tt-fgitm.shipto2 = b-shipto.ship-addr[1]
                  tt-fgitm.shipto4 = trim(b-oe-rel.ship-city) + ", " +
                                     b-oe-rel.ship-state + "  " + b-oe-rel.ship-zip.
         
                RELEASE b-cust.
                RELEASE b-oe-ordl.
                RELEASE b-oe-rel.
                RELEASE b-shipto.
          END.
      END.
   END.    

END PROCEDURE.  /*AddTTFGItem */

/* end ---------------------------------- copr. 1994  advanced software, inc. */
