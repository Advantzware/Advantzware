/* ----------------------------------------------  */
/*  cecrep/jobloy.p  Corrugated factory ticket  for Loy Lang */
/* -------------------------------------------------------------------------- */
/*Mode : 001 Task# 10111316*/

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.

DEF VAR prt-copies AS INT  NO-UNDO.
DEF VAR v-lines-tmp AS INT NO-UNDO.
DEF VAR lv-tmp AS CHAR NO-UNDO.
DEF VAR lv-tmp-val AS CHAR NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ink-1 AS cha FORM "X(34)" NO-UNDO.
DEF VAR v-ink-2 AS cha FORM "X(34)" NO-UNDO.
DEF VAR v-ink-3 AS cha FORM "X(34)" NO-UNDO.
DEF VAR v-ink-4 AS cha FORM "X(34)" NO-UNDO.
DEF VAR v-ink-5 AS cha FORM "X(34)" NO-UNDO.
DEF VAR v-ink-6 AS cha FORM "X(34)" NO-UNDO.
DEF var v-dept-note AS cha FORM "x(110)" EXTENT 8 NO-UNDO.
DEF var v-spec-note AS cha FORM "x(110)" EXTENT 8 NO-UNDO.
DEF VAR lv-under-run AS cha NO-UNDO.
DEF VAR lv-over-run AS cha NO-UNDO.
DEF VAR lv-part-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-fg-name AS cha NO-UNDO.
DEF VAR v-csr AS CHAR FORMAT "X(32)" NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-die-no AS CHAR NO-UNDO.
DEF VAR v-cad-no AS CHAR NO-UNDO.
DEF VAR v-score-string AS CHAR FORMAT "X(25)" NO-UNDO.
DEF VAR v-score-string-2 AS CHAR FORMAT "X(37)" NO-UNDO.
DEF VAR v-score-string-3 AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR score-count AS INT NO-UNDO.
DEF VAR v-see-1st-blank AS LOG NO-UNDO.

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR v-sht-qty-string AS cha NO-UNDO.
DEF VAR v-whs-ship-id AS CHAR NO-UNDO.
def var v-form-sqft-2 as dec decimals 3 format ">>9.9<<" NO-UNDO.
DEF BUFFER oe-rel-whse FOR oe-rel.
DEFINE BUFFER b1-in-house-cust FOR cust.
DEFINE BUFFER b1-shipto FOR shipto.
DEF BUFFER b-itemfg FOR itemfg.

{jcrep/r-ticket.i "shared"}

{cecrep/jobtickloy.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}

{custom/formtext.i NEW}
{sys/inc/notes.i}

v-exc-depts = v-exc-depts + ",BN".

def new shared var v-out1-id       as   recid    no-undo.
def new shared var v-out2-id       as   recid    no-undo.
 
DEF VAR v-po-no AS INT NO-UNDO.
def var v-i-line        as   char   extent 4    format "x(38)"          no-undo.
def var v-local-copies  as   int                                        no-undo.
def var v-local-loop    as   int    init 1                              no-undo.
def var v-pqty          as   dec                                        no-undo.
DEF VAR lv-part-no AS cha FORM "x(15)" NO-UNDO.
DEF VAR lv-rt-num AS INT NO-UNDO.

DEF VAR v-loop-cnt AS INT NO-UNDO.
DEF VAR v-note-length AS INT NO-UNDO.
DEF VAR v-die-loc AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-plate-loc AS CHAR FORM "X(8)" NO-UNDO.

{custom/notesdef.i}
{cecrep/jc-prem.i}
DEF BUFFER b-ef FOR ef.
DEF WORKFILE tt-wm LIKE w-m.

DEF BUFFER bf-eb FOR eb.
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEF SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF BUFFER b-eb FOR eb.

/* gdm - 12040902 */
DEF BUFFER bf-shipto FOR shipto.
DEF BUFFER bf-shipto-notes FOR shipto.
DEF VAR v-shpnote    AS CHAR FORMAT 'x(50)' EXTENT 4 NO-UNDO.
DEF VAR v-crgrain    AS CHAR FORMAT 'x(20)'          NO-UNDO.
DEF VAR v-len-score1 AS CHAR FORMAT 'x(30)'          NO-UNDO.
DEF VAR v-len-score2 AS CHAR FORMAT 'x(30)'          NO-UNDO.
DEF VAR v-len-score3 AS CHAR FORMAT 'x(30)'          NO-UNDO.
DEF VAR v-xcnt       AS INT                          NO-UNDO.
DEF VAR v-ship-id    AS CHAR                         NO-UNDO.

DEF BUFFER bf-reftable1 FOR reftable.
DEF BUFFER bf-reftable2 FOR reftable.

DEF VAR tb_app-unprinted AS LOG NO-UNDO.

ASSIGN
   ls-image1 = "images\loy2.jpg"
   FILE-INFO:FILE-NAME = ls-image1
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.

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
 v-local-copies = 1
 prt-copies = 1.

do v-local-loop = 1 to v-local-copies:
{cecrep/jobprem.i}
      break by job.job-no BY job.job-no2 BY job-hdr.frm:

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
          BREAK BY w-ef.frm BY b-eb.blank-no:

          release xef.
          release xeb.
          release xstyle.
          release xxprep.
          
          run cecrep/jobtickloy.p (recid(w-ef), recid(job-hdr)).
          
          ASSIGN
             v-pqty = 1
             v-cp = "".
          
          if avail xeb then do:
             if xeb.stock-no ne "" then v-fg = xeb.stock-no.
          
             ASSIGN
                v-cp = xeb.part-no
                lv-fg-name = itemfg.i-name.
          
             {cec/rollfac.i}
             v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                      if xeb.yld-qty lt 0 then (-1 / xeb.yld-qty)
                                         else xeb.yld-qty.
          end.
          
          assign
            v-loc     = ""
            v-loc-bin = ""
            lv-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                          IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE ""
            lv-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                           IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE ""
            lv-part-name = xeb.part-dscr1.

       PUT    "</PROGRESS>"
       "<P12><B>JOB TICKET" AT 9
       "Production Specification " AT 50  " Item Name:" lv-part-name SKIP
       "</B><P10><#1><C1><FROM><C105><R+45><RECT><|3>" 
       "<=1><C32><FROM><R+19><C32><LINE><|3>"
       "<=1><C66><FROM><R+19><C66><LINE><|3>"
       "<=1><C90><FROM><R+4><C90><LINE><|3>"
       "<=1><R+4><C1><FROM><C105><LINE><|3>"
       "<=#1><R+10><C1><FROM><C105><LINE><|3>"
       "<=1><R+19><C1><FROM><C105><LINE><|3>".

       view frame head.  /* factory header display  */  

       assign
          v-i-line[1] = itemfg.i-name
          v-i-line[2] = itemfg.part-dscr1
          v-i-line[3] = itemfg.part-dscr2
          v-i-line[4] = itemfg.part-dscr3
          lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                       ELSE itemfg.part-no.
                           
        RUN sys/ref/getpo#.p (IF AVAIL xoe-ordl AND est.est-type NE 6 THEN ROWID(xoe-ordl) ELSE ROWID(job),
                              w-ef.frm, OUTPUT v-po-no).
       
        i = 0.
        for each w-i:
          i = i + 1.
        end.
        if i lt 6 then do i = i + 1 to 6:
          create w-i.
        end.
        find first w-i.
        {sys/inc/roundup.i w-i.i-qty}
        v-ink-1 =   string(w-i.i-dscr,"x(30)") + " " + 
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">9") ELSE "" ) + 
                   (IF w-i.i-dscr <> "" THEN "#" ELSE "") .
        find next w-i.
        {sys/inc/roundup.i w-i.i-qty}
        v-ink-2 =  string(w-i.i-dscr,"x(30)") + " " + 
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">9") ELSE "" ) + 
                   (IF w-i.i-dscr <> "" THEN "#" ELSE "") .
        find next w-i.
        {sys/inc/roundup.i w-i.i-qty}
        v-ink-3 =  string(w-i.i-dscr,"x(30)") + " " + 
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">9") ELSE "" ) + 
                   (IF w-i.i-dscr <> "" THEN "#" ELSE "").
        find NEXT w-i.
        {sys/inc/roundup.i w-i.i-qty}

        ASSIGN
        v-ink-4 =  string(w-i.i-dscr,"x(30)") + " " + 
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">9") ELSE "" ) + 
                   (IF w-i.i-dscr <> "" THEN "#" ELSE "")
        v-form-sqft = round(if v-corr then (xeb.t-len * xeb.t-wid * .007)
                            else (xeb.t-len * xeb.t-wid / 144),3)
        v-form-sqft-2 = round(if v-corr then (v-form-len * v-form-wid * .007)
                                       else (v-form-len * v-form-wid / 144),3).
        FIND NEXT w-i.
        {sys/inc/roundup.i w-i.i-qty}
         v-ink-5 =  string(w-i.i-dscr,"x(30)") + " " + 
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">9") ELSE "" ) + 
                   (IF w-i.i-dscr <> "" THEN "#" ELSE "") .
        find next w-i.
        {sys/inc/roundup.i w-i.i-qty}

         v-ink-6 =  string(w-i.i-dscr,"x(30)") + " " + 
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">9") ELSE "" ) + 
                   (IF w-i.i-dscr <> "" THEN "#" ELSE "") .
       
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.die-no
                            no-lock no-error.
        v-die-loc = IF AVAIL xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

        
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.plate-no
                            no-lock no-error.

        ASSIGN
           v-plate-loc = IF AVAIL xxprep THEN xxprep.loc-bin ELSE ""
           v-csr = "".

        IF AVAIL xoe-ord THEN
        DO:
            IF xoe-ord.entered-id <> "" THEN
                v-csr = xoe-ord.entered-id.
            ELSE
                 v-csr = xoe-ord.USER-ID.

           FIND FIRST users WHERE
                users.user_id = v-csr
                NO-LOCK NO-ERROR.

           IF AVAIL users THEN
              v-csr = v-csr + " - " + users.user_name.
        END.
        ELSE
        DO:
            v-csr = job.USER-ID.

           FIND FIRST users WHERE
                users.user_id = job.USER-ID
                NO-LOCK NO-ERROR.

           IF AVAIL users THEN
              v-csr = v-csr + " - " + users.user_name.
        END.

        /* gdm - 12040902 */
        IF AVAIL xef THEN
            ASSIGN v-crgrain = IF TRIM(xef.xgrain) EQ "N"
                                 THEN "Vertical"
                                 ELSE "Horizontal".                

        FOR EACH po-ordl NO-LOCK
          WHERE po-ordl.company EQ job-hdr.company
            AND po-ordl.job-no  EQ job-hdr.job-no
            AND po-ordl.job-no2 EQ job-hdr.job-no2             
            AND po-ordl.opened,         
         FIRST job-mat NO-LOCK
          WHERE job-mat.company EQ po-ordl.company
            AND job-mat.job     EQ job.job
            AND job-mat.i-no    EQ po-ordl.i-no
            AND job-mat.frm     EQ job-hdr.frm:


           FIND FIRST bf-reftable1 NO-LOCK
              WHERE bf-reftable1.reftable EQ "POLSCORE"
                AND bf-reftable1.company  EQ po-ordl.company
                AND bf-reftable1.loc      EQ "1"
                AND bf-reftable1.code     EQ STRING({1}po-ordl.po-no,"9999999999")
                AND bf-reftable1.code2    EQ STRING({1}po-ordl.line, "9999999999") NO-ERROR.
           
            FIND FIRST bf-reftable2 NO-LOCK
             WHERE bf-reftable2.reftable EQ "POLSCORE"
               AND bf-reftable2.company  EQ {1}po-ordl.company
               AND bf-reftable2.loc      EQ "2"
               AND bf-reftable2.code     EQ STRING({1}po-ordl.po-no,"9999999999")
               AND bf-reftable2.code2    EQ STRING({1}po-ordl.line, "9999999999") NO-ERROR.
           
            IF AVAIL bf-reftable1 OR AVAIL bf-reftable2 THEN DO:
           
              DO v-xcnt = 1 TO 12:
                IF bf-reftable1.val[v-xcnt] NE 0 THEN DO:
                  v-len-score = "".
                  LEAVE.
                END.                        
              END.
           
              DO v-xcnt = 1 TO 12:
                IF STRING(bf-reftable1.val[v-xcnt]) EQ "0" 
                  THEN NEXT.
                ASSIGN v-len-score = v-len-score + 
                                     STRING(bf-reftable1.val[v-xcnt]) + " " +
                                     SUBSTR(bf-reftable1.dscr,v-xcnt,1)
                                     + " ".
              END.
           
              DO v-xcnt = 1 TO 12:
                IF STRING(bf-reftable2.val[v-xcnt]) EQ "0" THEN NEXT.
                ASSIGN v-len-score = v-len-score + 
                                     STRING(bf-reftable2.val[v-xcnt]) + " " +
                                     SUBSTR(bf-reftable2.dscr,v-xcnt,1)
                                     + " ".
              END.            
            END.               
        END.         

        v-len-score = TRIM(v-len-score).

        IF LENGTH(v-len-score) GT 25 THEN DO:

           IF SUBSTR(v-len-score,25,1) NE "" THEN DO:
              IF SUBSTR(v-len-score,26,1) EQ "" THEN
                 ASSIGN v-len-score1 = SUBSTR(v-len-score,1,25)
                        v-len-score2 = SUBSTR(v-len-score,26).
              ELSE 
                 IF SUBSTR(v-len-score,24,1) EQ "" THEN
                    ASSIGN v-len-score1 = SUBSTR(v-len-score,1,23)
                           v-len-score2 = SUBSTR(v-len-score,24).
              ELSE
                 ASSIGN v-len-score1 = SUBSTR(v-len-score,1,
                                              R-INDEX(v-len-score,' ',24))
                        v-len-score2 = SUBSTR(v-len-score,
                                              R-INDEX(v-len-score,' ',24) + 1).        
           END.
           ELSE
              ASSIGN
                 v-len-score1 = SUBSTR(v-len-score,1,25)
                 v-len-score2 = SUBSTR(v-len-score,26).
        END.
        ELSE
           ASSIGN v-len-score1 = v-len-score
                  v-len-score2 = "".

        v-see-1st-blank = IF NOT FIRST-OF(w-ef.frm) OR
                             CAN-FIND(FIRST bf-eb WHERE bf-eb.company = b-eb.company
                                                    AND bf-eb.est-no = b-eb.est-no
                                                    AND bf-eb.form-no = b-eb.form-no
                                                    AND bf-eb.blank-no < b-eb.blank-no)
                          THEN YES ELSE NO.
        
        ASSIGN v-len-score1 = TRIM(v-len-score1)
               v-len-score2 = TRIM(v-len-score2).

        DISP  v-cus[1] AT 2 "PO #:" AT 40 xoe-ord.po-no WHEN AVAIL xoe-ord 
              "Set Qty:" trim(string(if avail xoe-ordl then xoe-ordl.qty
                                          else job-hdr.qty,">>>,>>9"))
                        when avail xeb and xeb.est-type eq 9    format "x(9)"
              "Style:" AT 80 xstyle.dscr WHEN AVAIL xstyle              
              SKIP
              v-cus[2] AT 2  
              "Job Qty:" AT 40 trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
              "Order Qty:" trim(string((if avail xoe-ordl then xoe-ordl.qty
                                             else job-hdr.qty) *
                                            if est.form-qty le 1 then 1
                                            else v-pqty,">>>,>>9"))
                                            format "x(7)"
              "   "
              "Size:" (trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99"))) FORM "x(30)" WHEN AVAIL xeb            
              SKIP
              v-cus[3] AT 2
              "CSR:" AT 40 v-csr
              "Joint:" AT 80 v-joint-dscr             
              SKIP
              v-cus[4] AT 2
              "" AT 40
              "<P9>"SKIP(1)
              "<B>Board:</B>" AT 2
              "<B>Printing:</B>" AT 50
              "<B>Die Cutting, Slit, & Saw</B>" AT 103 "<P10>"SKIP
              /*"Shts Req'd:" AT 2 trim(string(v-sht-qty))   format "x(9)"*/
              "Shts Req'd:" AT 2 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
              trim(string(v-dc-qty)) FORMAT "x(9)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank  @ v-sht-qty-string 
              "" WHEN v-see-1st-blank @ v-sht-qty-string
              " Sq Ft:" trim(string(v-form-sqft)) format "x(7)"
              "PRINT SHEET #:" AT 39 xeb.plate-no FORM "X(15)" when avail xeb
              v-plate-loc WHEN AVAIL xeb
              "Die #" AT 80 xeb.die-no when avail xeb " Loc:" v-die-loc SKIP
              "W:" + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))  format "x(22)" AT 2
              "MSF:" + trim(string(v-dc-qty * v-form-sqft-2 / 1000,">>>9.9<")) format "x(11)"
              "1:" AT 39 v-ink-1
              "Gross Size:" AT 80  
              "W:" + trim(string({sys/inc/k16v.i xef.gsh-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.gsh-len},">>>9.99"))
                                                                format "x(20)"
              "Slit: W:" + string(v-outw) + " L:" + string(v-outl) FORM "x(15)"   
              SKIP
              "Board:" AT 2 v-board-code FORM "x(30)" "2:" AT 39 v-ink-2 "Net   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>9.99")) format "x(22)"
              SKIP
              v-board-dscr AT 4 FORM "x(30)" "3:" AT 39 v-ink-3 "Die   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(22)"
              "Up:" "W:" + string(v-upl) + " L:" + string(v-upw) FORM "x(9)"
              SKIP
               "Score:" AT 2 v-len-score1 WHEN xstyle.TYPE <> "F" format "x(25)" 
              "4:" AT 39 v-ink-4 "Blank Size:" AT 80 
              "W:" + trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)"
              SKIP
              v-len-score2 AT 6 FORM "x(25)"              
              "5:" AT 39 v-ink-5 
              "Impressions: " AT 80 trim(string(v-dc-qty))    format "x(7)"
              SKIP
              "Adders:" AT 2 v-adders FORM "x(23)"
              "6:" AT 39 v-ink-6 
              "D/C Style:" AT 80 SKIP
              "Color Desc:" AT 39 xeb.i-coldscr when avail xeb
              "Corr Dir:" AT 80 v-crgrain
              WITH FRAME job1 NO-LABEL NO-BOX WIDTH 150 STREAM-IO.
        
        i = 0.
        for each w-m:
          i = i + 1.
        end.
        if i lt 3 then do i = i + 1 to 3:
          create w-m.
          w-m.dseq = 999999999.
        end.
        /* box for route */
        
        ASSIGN
           lv-rt-num = i + 3
           i = 0.

        put skip
            "<P9><B>  Machine Routing            SU:    Start    Stop     Total       RUN:    Hours    Start    Stop    Total   QTY:  In     Out      Waste</B><P10>" SKIP.
        
        for each w-m by w-m.dseq:

          ASSIGN
             i = i + 1
             v-letter = substr("UTE",i,1).

          display w-m.dscr AT 3
                  w-m.s-hr                              when w-m.s-hr ne 0
                  fill("_",7)  format "x(7)"    to 38   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 46   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 54   when w-m.dscr ne ""
                  space(4)
                  w-m.r-sp                              when w-m.r-sp ne 0
                  w-m.r-hr                              when w-m.r-hr ne 0
                  fill("_",7)  format "x(7)"    to 80   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 88   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 96    when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 105  when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 114  when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 123  when w-m.dscr ne ""
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

        PUT "<#6><C1><FROM><C105><LINE><|3>" .
            
        RUN cecrep/jobloy2.p (INPUT RECID(job-hdr), INPUT w-ef.frm).

        FOR EACH tt-formtext:
            DELETE tt-formtext.
        END.

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

        FIND FIRST b-itemfg WHERE b-itemfg.company = itemfg.company AND b-itemfg.i-no = xeb.stock-no
            NO-LOCK NO-ERROR.   /*mode 001*/
        FOR EACH notes WHERE
            notes.rec_key = b-itemfg.rec_key AND
            LOOKUP(notes.note_code,spec-list) GT 0,
            FIRST item-spec WHERE item-spec.code = notes.note_code NO-LOCK 
                BY notes.note_code:  
                lv-text = lv-text + "<B>" + caps(item-spec.notes[1]) + ":</B>" + CHR(10) + 
                             /*notes.note_title +*/  "  " + notes.note_text + CHR(10).
        END.

        DO v-count = 1 TO 8:
           CREATE tt-formtext.
           ASSIGN
              tt-line-no = v-count
              tt-length  = 110.
        END.

        RUN custom/formtext.p (lv-text).
        ASSIGN
           v-count = 0
           v-dept-note = "".

        FOR EACH tt-formtext:
            v-count = v-count + 1.
            IF v-count <= 8 THEN v-dept-note[v-count] = tt-formtext.tt-text.      
        END.

        PUT "<=6><C29>" v-dept-note[1]
            "<=6><C29><R+1>" v-dept-note[2]
            "<=6><C29><R+2>" v-dept-note[3]
            "<=6><C29><R+3>" v-dept-note[4]
            "<=6><C29><R+4>" v-dept-note[5]
            "<=6><C29><R+5>" v-dept-note[6]
            "<=6><C29><R+6>" v-dept-note[7]
            "<=6><C29><R+7>" v-dept-note[8]
            "<=6><C28><FROM><R+8><C28><LINE><|3>".
/*

MESSAGE "lv " + STRING(spec-list) VIEW-AS ALERT-BOX ERROR.
        FOR EACH notes WHERE
            notes.rec_key = itemfg.rec_key AND
            (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0) AND
            LOOKUP(notes.note_code,spec-list) GT 0,
            FIRST item-spec WHERE item-spec.code = notes.note_code
            NO-LOCK BY notes.note_code:

            lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).

             MESSAGE "lv-test000 " + STRING(lv-text) VIEW-AS ALERT-BOX ERROR.
        END.
      
            DO v-count = 1 TO 8:
                CREATE tt-formtext.
                ASSIGN
                    tt-line-no = v-count
                    tt-length  = 110.
            END.
MESSAGE "lv-test " + STRING(lv-text) VIEW-AS ALERT-BOX ERROR.
            RUN custom/formtext.p (lv-text).
            ASSIGN
                v-count = 0
                v-spec-note = "".
MESSAGE "lv-test22 " + STRING(lv-text) + "    " + STRING(tt-formtext.tt-text) VIEW-AS ALERT-BOX ERROR.
            FOR EACH tt-formtext:
                v-count = v-count + 1.
                IF v-count <= 8 THEN v-spec-note[v-count] = tt-formtext.tt-text.      
            END.

            PUT "<=6><C29>" v-spec-note[1]
            "<=6><C29><R+1>" v-spec-note[2]
            "<=6><C29><R+2>" v-spec-note[3]
            "<=6><C29><R+3>" v-spec-note[4]
            "<=6><C29><R+4>" v-spec-note[5]
            "<=6><C29><R+5>" v-spec-note[6]
            "<=6><C29><R+6>" v-spec-note[7]
            "<=6><C29><R+7>" v-spec-note[8]
            "<=6><C28><FROM><R+8><C28><LINE><|3>".*/

        
        RUN stackImage.

        PUT "<#7><C1><FROM><C105><LINE><|3>"
            "<=7><C28><FROM><R+14><C28><LINE><|3>"
            "<=7><C75><FROM><R+14><C75><LINE><|3>"
            "<P9>" SKIP.

        RELEASE oe-rel-whse.

        FIND FIRST oe-rel-whse WHERE
             oe-rel-whse.company EQ xoe-ordl.company AND
             oe-rel-whse.ord-no  EQ xoe-ordl.ord-no AND
             oe-rel-whse.i-no    EQ xoe-ordl.i-no AND
             oe-rel-whse.line    EQ xoe-ordl.LINE AND
             oe-rel-whse.ship-id EQ "WHSE"
             NO-LOCK NO-ERROR.

        /* gdm - 12040902 */
        ASSIGN v-shpnote[1] = ''
               v-shpnote[2] = ''
               v-shpnote[3] = ''
               v-shpnote[4] = ''
               v-whs-ship-id = IF AVAIL xoe-ordl AND
                                  AVAIL oe-rel-whse THEN
                                  "WHSE" ELSE ""
               v-ship-id    = IF v-whs-ship-id NE "" THEN "WHSE"
                              ELSE IF AVAIL xoe-rel AND TRIM(xoe-rel.ship-id) NE "" THEN
                                 xoe-rel.ship-id
                              ELSE IF AVAIL xoe-ord AND TRIM(xoe-ord.sold-id) NE "" THEN
                                   xoe-ord.sold-id 
                              ELSE IF AVAIL xeb AND TRIM(xeb.ship-id) NE "" THEN
                                   xeb.ship-id
                              ELSE "".

        FIND FIRST bf-shipto-notes NO-LOCK WHERE
                   bf-shipto-notes.company EQ cust.company AND
                   bf-shipto-notes.cust-no EQ cust.cust-no AND
                   bf-shipto-notes.ship-id EQ v-ship-id NO-ERROR.
        IF AVAIL bf-shipto-notes THEN
           ASSIGN v-shpnote[1] = bf-shipto-notes.notes[1]
                  v-shpnote[2] = bf-shipto-notes.notes[2]
                  v-shpnote[3] = bf-shipto-notes.notes[3] 
                  v-shpnote[4] = bf-shipto-notes.notes[4].
        RELEASE bf-shipto-notes.
                                                                       
        RELEASE b1-in-house-cust.
        RELEASE b1-shipto.        

        IF xoe-ordl.managed = true THEN DO:
            FIND FIRST b1-in-house-cust WHERE b1-in-house-cust.company EQ xoe-ordl.company
                                          AND b1-in-house-cust.active  EQ "X"
                 NO-LOCK NO-ERROR.
            IF AVAIL b1-in-house-cust THEN
               FIND FIRST b1-shipto NO-LOCK WHERE
                          b1-shipto.company EQ xoe-ordl.company AND
                          b1-shipto.cust-no EQ b1-in-house-cust.cust-no AND
                          b1-shipto.ship-id EQ "WHSE" NO-ERROR.

            ASSIGN v-whs-ship-id = IF AVAIL xoe-ordl AND
                                   AVAIL b1-shipto THEN
                                   "WHSE" ELSE ""
                   v-ship-id     = IF v-whs-ship-id NE "" THEN "WHSE"
                                   ELSE IF AVAIL xoe-rel AND TRIM(xoe-rel.ship-id) NE "" THEN
                                   xoe-rel.ship-id
                                   ELSE IF AVAIL xoe-ord AND TRIM(xoe-ord.sold-id) NE "" THEN
                                   xoe-ord.sold-id 
                                   ELSE IF AVAIL xeb AND TRIM(xeb.ship-id) NE "" THEN
                                   xeb.ship-id
                                   ELSE "".

        END.

        IF v-ship-id NE "" AND AVAIL b1-shipto THEN
           FIND bf-shipto WHERE RECID(bf-shipto) = RECID(b1-shipto) 
                NO-LOCK NO-ERROR.
        ELSE IF v-ship-id NE "" THEN
           FIND FIRST bf-shipto WHERE
                      bf-shipto.company EQ cust.company AND
                      bf-shipto.cust-no EQ cust.cust-no AND
                      bf-shipto.ship-id EQ v-ship-id
                      NO-LOCK NO-ERROR.

        /*IF AVAIL bf-shipto THEN
           ASSIGN v-shpnote[1] = bf-shipto.notes[1]
                  v-shpnote[2] = bf-shipto.notes[2]
                  v-shpnote[3] = bf-shipto.notes[3] 
                  v-shpnote[4] = bf-shipto.notes[4].
        */
        /* this is for in house shipto WHSE */
        if AVAIL b1-shipto AND v-whs-ship-id NE ""  then do:
           assign
              i     = 0
              v-shp = ""
              v-ship = "".
              
           if bf-shipto.ship-name ne "" then
             assign
              i        = i + 1
              v-shp[i] = bf-shipto.ship-name.
                
           if bf-shipto.ship-addr[1] ne "" then
             assign
              i        = i + 1
              v-shp[i] = bf-shipto.ship-addr[1].
                
           if bf-shipto.ship-addr[2] ne "" then
             assign
              i        = i + 1
              v-shp[i] = bf-shipto.ship-addr[2].
           
           assign
            i        = i + 1
            v-shp[i] = trim(bf-shipto.ship-city) + ", " +
                       bf-shipto.ship-state + "  " + bf-shipto.ship-zip.
          
           do i = 1 to 4:
              if AVAIL oe-rel-whse AND oe-rel-whse.ship-i[i] ne "" then do:
                 if v-ship eq "" then
                    v-ship = "Shipping:".

                 v-ship = trim(v-ship) + " " + oe-rel-whse.ship-i[i].
              end.
           end.
        end.
        /* this is for oe-rel shipto */
        ELSE
        if v-whs-ship-id NE "" then do:
           assign
              i     = 0
              v-shp = ""
              v-ship = "".
              
           if avail bf-shipto and bf-shipto.ship-name ne "" then
             assign
              i        = i + 1
              v-shp[i] = bf-shipto.ship-name.
                
           if oe-rel-whse.ship-addr[1] ne "" then
             assign
              i        = i + 1
              v-shp[i] = oe-rel-whse.ship-addr[1].
                
           IF oe-rel-whse.ship-addr[2] ne "" then
             assign
              i        = i + 1
              v-shp[i] = oe-rel-whse.ship-addr[2].
           
           assign
            i        = i + 1
            v-shp[i] = trim(oe-rel-whse.ship-city) + ", " +
                       oe-rel-whse.ship-state + "  " + oe-rel-whse.ship-zip.
          
           do i = 1 to 4:
              if oe-rel-whse.ship-i[i] ne "" then do:
                 if v-ship eq "" then
                    v-ship = "Shipping:".

                 v-ship = trim(v-ship) + " " + oe-rel-whse.ship-i[i].
              end.
           end.
        end.
        
        DISPLAY "<=7><C30><B> SHIP NOTES / COMMENTS </B>" AT 2
                "<B>Shipping Info: </B><P10>" AT 79 SKIP
                "Ship To #:" AT 90
                xoe-ord.sold-id when avail xoe-ord
                xeb.ship-id when avail xeb @ xoe-ord.sold-id
                xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id
                v-whs-ship-id WHEN v-whs-ship-id NE "" @ xoe-ord.sold-id SKIP
                v-shpnote[1] FORMAT 'x(50)' AT 35 v-shp[1] AT 90 SKIP
                v-shpnote[2] FORMAT 'x(50)' AT 35 v-shp[2] AT 90 SKIP
                v-shpnote[3] FORMAT 'x(50)' AT 35 v-shp[3] AT 90 SKIP
                v-shpnote[4] FORMAT 'x(50)' AT 35 v-shp[4] AT 90 SKIP
                 "Item PO #:" AT 90 xoe-ordl.po-no when avail xoe-ordl
                "" SKIP   
                "" SKIP                 
                with no-box no-labels frame m8 width 170 no-attr-space STREAM-IO.

        PAGE.

        PUT "<C106><R50><#6><AT=.01,.01>"
            "<IMAGE#6="ls-full-img1.

        PAGE.

        ASSIGN
           v-die-no = IF AVAIL xeb THEN xeb.die-no ELSE ""
           v-cad-no = IF AVAIL xeb THEN xeb.cad-no ELSE "".

        PUT "<#11><C1><FROM><C105><R+47><RECT><|3>"  
            "<=11><C2><B> Customer: </B>" cust.NAME + " - " + cust.cust-no FORMAT "X(40)"
            "<C50><B> Job#: </B>" v-job-prt FORMAT "X(9)"
            "<C65><B> Estimate#: </B>" v-est-no FORMAT "X(6)" SKIP
            "<C2><B> Cust Part#: </B>" v-cp FORMAT "X(15)"
            "<C25><B> FG#: </B>" v-fg FORMAT "X(15)"
            "<C50><B> Order#: </B>" v-ord-no 
            "<C65><B> Die#: </B>" v-die-no FORMAT "X(15)"
            "<C80><B> CAD#: </B>" v-cad-no FORMAT "X(15)" skip
            "<C1><FROM><C105><LINE><|3>".

        if print-box and avail xest then do:

           run cec/desprnloy2.p(recid(xef),
                             input-output v-lines-tmp,
                             recid(xest),
                             OUTPUT v-score-string).

           ASSIGN
              v-score-string = TRIM(v-score-string)
              lv-tmp = ""
              v-score-string-2 = ""
              v-score-string-3 = ""
              score-count = 0.
          
           /*get rid of extra spaces*/
           DO i = 1 TO LENGTH(v-score-string):
              lv-tmp-val = SUBSTRING(v-score-string,i,1).
              IF lv-tmp-val <> " " THEN
                 lv-tmp = lv-tmp + lv-tmp-val.
              ELSE
              IF lv-tmp NE "" THEN
              DO:
                 IF LENGTH(lv-tmp) + 1 LE 37 - LENGTH(v-score-string-2) THEN
                    v-score-string-2 = v-score-string-2 + " " + lv-tmp.
                 
                 score-count = score-count + 1.
          
                 IF score-count LE 12 THEN
                    v-score-string-3 = v-score-string-3 + "         " + lv-tmp.
          
                 lv-tmp = "".
              END.
           END.
          
           IF lv-tmp NE "" THEN
           DO:
              IF LENGTH(lv-tmp) + 1 LE 37 - LENGTH(v-score-string-2) THEN
                 v-score-string-2 = v-score-string-2 + " " + lv-tmp.
          
              score-count = score-count + 1.
          
              IF score-count LE 12 THEN
                 v-score-string-3 = v-score-string-3 + "         " + lv-tmp.
           END.
          
           ASSIGN
              v-score-string-2 = TRIM(v-score-string-2)
              v-score-string-3 = TRIM(v-score-string-3)
              v-out1-id = RECID(xeb).

           run cec/desprnloy.p (recid(xef),
                                input-output v-lines,
                                recid(xest),
                                v-score-string-3).
           PAGE.
        end.
        ELSE
           PAGE.

        PUT "<C106><R50><#6><AT=.01,.01>"
            "<IMAGE#6="ls-full-img1.

        PAGE.
      end.  /* for each w-ef */

      IF s-prt-set-header AND last-of(job.job-no2) AND est.est-type = 6 THEN DO: /* print set header */
         i = 0.
         FOR EACH bf-eb WHERE
             bf-eb.company = est.company AND
             bf-eb.est-no = est.est-no AND
             bf-eb.form-no > 0 NO-LOCK:
             i = i + 1.
         END.   
         IF i > 1 THEN DO:
             DEF VAR v-set-qty AS INT NO-UNDO.
             DEF VAR v-ord-qty AS INT NO-UNDO.
             DEF VAR v-over-run AS cha NO-UNDO.
             DEF VAR v-under-run AS cha NO-UNDO.
             DEF VAR v-fg-set AS cha FORM "x(15)" NO-UNDO.
             ASSIGN
             v-fg-set = job-hdr.i-no
             v-set-qty = if avail xeb and xeb.est-type eq 6 THEN
                           if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty
                         ELSE 0
             v-ord-qty = (if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty) *
                         if est.form-qty le 1 then 1 else v-pqty
             v-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                          IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE ""
             v-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                           IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE "".
             PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
                 "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
                 "<C60>Our Date: " v-ord-date SKIP
                 "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
                 "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
                 "<=1><R+6><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
                 v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
                 v-i-line[2] AT 90
                 SKIP
                 v-cus[2] AT 3 " Job Qty:" trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
                 " Order Qty:" string(v-ord-qty) format "x(7)"
                 v-i-line[3] AT 90 SKIP
                 v-cus[3] AT 3  " Cust Part #:" lv-part-no 
                 v-i-line[4] AT 90 SKIP
                 v-cus[4]  AT 3 " Overrun:"  format "x(7)"  
                 " Underrun:" format "x(7)"  
                 "Adders:" v-adders FORM "x(33)" AT 90 SKIP
                 "<=1><R+11><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
                 "<C2>FINISHED GOOD #                 DESCRIPTION                       RATIO PER SET" SKIP.
             /* each components */
             DEF VAR v-tmp-line AS INT NO-UNDO.
             DEF VAR v-shipto AS cha NO-UNDO.
        
             v-tmp-line = 0.
             FOR EACH xeb FIELDS(stock-no part-dscr1 yld-qty) WHERE
                 xeb.company = est.company AND
                 xeb.est-no = est.est-no AND
                 xeb.form-no > 0
                 NO-LOCK:
                 PUT xeb.stock-no AT 3 space(14) xeb.part-dscr1 space(5) xeb.yld-qty SKIP.
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
             PUT "  <P9>Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date</P9>" SKIP
                 "<=1><R+" + string(v-tmp-line + 1) + "><C2><FROM><R+" + string(i) + "><C78><RECT><||3>" FORM "x(150)" SKIP
                 "<=1><R+" + string(v-tmp-line + 1) + ">" FORM "x(20)".
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
             v-shipto = IF v-whs-ship-id NE "" THEN v-whs-ship-id ELSE
                        IF AVAIL xoe-rel THEN xoe-rel.ship-id 
                        ELSE IF avail xeb THEN xeb.ship-id
                        ELSE IF avail xoe-ord THEN xoe-ord.sold-id 
                        ELSE "".
             FIND FIRST tt-prem WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
                                   AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
             IF NOT AVAIL tt-prem THEN CREATE tt-prem.
        
             ASSIGN v-tmp-lines = 0
                    j = 0
                    K = 0
                    lv-got-return = 0
                    v-dept-inst = "".
        
             {custom/notespr2.i job v-inst2 6 "notes.rec_key = job.rec_key and
                              notes.note_form_no = 0" }
             DO i = 1 TO 6:
                v-dept-inst[i] = v-inst2[i].
             END.
             IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */
             PUT "<=1><R+" + string(v-tmp-line) + ">" form "X(20)".
             v-tmp-line = v-tmp-line + 1.
             PUT "Unitizing Bale <C24>Date <C44>Units <C62>Complete" AT 3 SKIP
                 "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________" skip
                 "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial" skip
                 "Pattern: " AT 3 tt-prem.tt-pattern "<C20>_____________________ <C40>____________________  <C60>________________" skip
                 "Pallet: " AT 3 tt-prem.tt-pallet "<C20>_____________________ <C40>____________________ " skip
                 "<=1><R+" + string(v-tmp-line) + "><C2><FROM><R+6><C78><RECT><||3>" FORM "x(150)" SKIP
                 "<=1><R+" + string(v-tmp-line + 7) + "><C2><FROM><R+7><C78><RECT><||3>" FORM "x(150)" SKIP
        
                 "<=1><R+" + string(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
                 v-dept-inst[1] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[1] SKIP
                 v-dept-inst[2] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[2] SKIP
                 v-dept-inst[3] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[3] SKIP
                 v-dept-inst[4] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[4] SKIP
                 v-dept-inst[5] AT 3 FORM "x(82)" chr(124) format "xx" "Item PO #:" v-po-no SKIP
                 v-dept-inst[6] 
                 .
        
             PAGE.
          END. /* set header printing */
        END. /* est.est-type = 6 */
        /* end of set header printing */

    end.  /* each job */
    end.  /* end v-local-loop  */
 
        hide all no-pause.


PROCEDURE stackImage:
  DEFINE BUFFER pattern FOR reftable.
  DEFINE BUFFER stackPattern FOR stackPattern.
  IF v-stackcode EQ '' THEN RETURN.
  FIND FIRST stackPattern NO-LOCK
       WHERE stackPattern.stackCode EQ SUBSTR(v-stackcode,9,1) NO-ERROR.
  IF AVAILABLE stackPattern AND SEARCH(stackPattern.stackImage) NE ? THEN
  PUT UNFORMATTED
    "<#71><C27><R+1><FROM><C2><R+12>"
    "<IMAGE#71=" stackPattern.stackImage ">"
    "<R-13>".
END PROCEDURE.




/* end ---------------------------------- copr. 1997  advanced software, inc. */
