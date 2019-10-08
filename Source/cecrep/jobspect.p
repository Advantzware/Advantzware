/* ----------------------------------------------  */
/*  cecrep/jobspect.p  Corrugated factory ticket  for Spectrum */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF VAR prt-copies AS INT  NO-UNDO.
DEF VAR v-start-compress AS cha NO-UNDO.
DEF VAR v-end-compress AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ink-1 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-2 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-3 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-4 AS cha FORM "X(30)" NO-UNDO.
DEF var v-dept-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF var v-spec-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF VAR v-deptnote AS cha NO-UNDO.
DEF VAR v-dept-length AS DEC NO-UNDO.
DEF VAR lv-under-run AS cha NO-UNDO.
DEF VAR lv-over-run AS cha NO-UNDO.
DEF VAR lv-part-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-fg-name AS cha NO-UNDO.
DEF VAR lv-status AS cha NO-UNDO.
DEF VAR lv-sts-code AS cha INIT "O,R,C,T,N,X,Q" NO-UNDO.
DEF VAR lv-sts-desc AS cha INIT "O-Original,R-Repeat,C-Change,T-Transfer,N-New Customers,X-Complete Re-run,Q-Quality/Re-work" NO-UNDO.
DEF VAR v-sman AS cha FORM "x(25)" NO-UNDO.
DEF VAR v-set-qty AS INT NO-UNDO.
DEF VAR v-ord-qty AS INT NO-UNDO.
DEF VAR v-over-run AS cha NO-UNDO.
DEF VAR v-under-run AS cha NO-UNDO.
DEF VAR v-fg-set AS cha FORM "x(15)" NO-UNDO.
DEF VAR tb_app-unprinted AS LOG NO-UNDO.

{jcrep/r-ticket.i "shared"}

{cecrep/jobspect.i "new shared"}

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
{custom/notesdef.i}
{cecrep/jc-prem.i}
DEF BUFFER b-ef FOR ef.
DEF workfile tt-wm LIKE w-m.
DEF VAR v-xg-flag AS LOG NO-UNDO.
DEF VAR v-tmp-stype AS cha NO-UNDO.
DEF VAR v-len-score2 AS cha EXTENT 13 NO-UNDO.
DEF VAR v-tmp-score AS cha NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEF SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR lv-text AS cha NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg.
DEF VAR cha-val AS cha NO-UNDO.
DEF VAR w-m-count AS INT NO-UNDO.

DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.
DEF VAR ls-fgitem-img AS cha FORM "x(50)" NO-UNDO.
DEF VAR v-po-due-date AS DATE FORM "99/99/9999" NO-UNDO.

{custom/formtext.i NEW}

FIND sys-ctrl WHERE company = "001" AND NAME = "notes" NO-LOCK NO-ERROR.
 IF AVAILABLE sys-ctrl THEN 
   ASSIGN cha-val = TRIM(sys-ctrl.char-fld).
 
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
 v-qty-or-sup = "Qty Received: " + fill("_",24)
 v-local-copies = 1
 prt-copies = 1.

do v-local-loop = 1 to v-local-copies:
      {cecrep/jobprem.i}
      break by job.job-no BY job.job-no2:

      v-break = first-of(job.job-no2).

      release xest.
      release xef.
      release xeb.
      release xoe-ord.
      release xoe-ordl.
      release xoe-rel.

      run cecrep/jobtick1.p (recid(job-hdr), v-format,
                              v-local-loop, v-local-copies).

      for each w-ef:
        release xef.
        release xeb.
        release xstyle.
        release xxprep.
        
        run cecrep/jobtick2.p (recid(w-ef), recid(job-hdr)).

        v-pqty = 1.
        v-cp = "".
        if avail xeb then do:
          if xeb.stock-no ne "" then v-fg = xeb.stock-no.
          ASSIGN
            v-cp = xeb.part-no
            v-sman = xeb.sman
            lv-fg-name = itemfg.i-name.

          {cec/rollfac.i}
          ASSIGN
            v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                     if xeb.quantityPerSet lt 0 then (-1 / xeb.quantityPerSet)
                                         else xeb.quantityPerSet
            v-form-hdr = "FORM: " + string(xeb.form-no) + " OF " + string(est.form-qty) .
        end.

        assign
         v-loc     = ""
         v-loc-bin = ""
         lv-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                       IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE ""
         lv-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                        IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE ""
         lv-part-name = xeb.part-dscr1.

       PUT "<P12><B>JOB TICKET" AT 9
       "Production Specification <P10>" AT 50
       SKIP
       "<#1><C1><FROM><C105><R+47><RECT><||5>" 
       "<=1><C32><FROM><R+20><C32><LINE><||5>"
       "<=1><C66><FROM><R+20><C66><LINE><||5>"
       "<=1><C90><FROM><R+5><C90><LINE><||5>"
       "<=1><R+5><C1><FROM><C105><LINE><||5>"
       "<=1><R+11><C1><FROM><C105><LINE><||5>"
       "<=1><R+20><C1><FROM><C105><LINE><||5>"       
       "<=1><P14><R-2><C50><B>" cust.NAME space(3) v-fg "</B>".

       view frame head.  /* factory header display  */  

       assign
        v-i-line[1] = "ITEM DESCRIPTION"
        v-i-line[2] = "Style: " + if avail xstyle then xstyle.dscr else ""
        v-i-line[3] = "Size: "  + if avail xeb    then
                  trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                  trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                  trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
        v-i-line[4] = "Joint: " + if avail xeb then v-joint-dscr else ""
        lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                     ELSE itemfg.part-no.
                           
        RUN sys/ref/getpo#.p (IF AVAIL xoe-ordl AND est.est-type NE 6 THEN ROWID(xoe-ordl) ELSE ROWID(job),
                              w-ef.frm, OUTPUT v-po-no).

        RELEASE po-ord.
          
        IF v-po-no NE 0 THEN
        FIND FIRST po-ord
            WHERE po-ord.company EQ cocode
              AND po-ord.po-no   EQ v-po-no
            NO-LOCK NO-ERROR.
        FIND FIRST po-ordl WHERE po-ordl.company = cocode 
                             AND po-ordl.po-no = v-po-no
                             AND po-ordl.i-no = v-form-code NO-LOCK NO-ERROR.
        v-po-due-date =  IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.
        
        ASSIGN
         v-vend-no    = IF AVAIL po-ord THEN po-ord.vend-no ELSE ""
         v-qty-or-sup = "Supplier: ".

        if v-vend-no ne "" then do:
           v-qty-or-sup = v-qty-or-sup + trim(v-vend-no).
           IF v-po-no ne 0 THEN v-qty-or-sup = v-qty-or-sup + " PO#:" +
                                               trim(string(v-po-no,">>>>>>>>>>")).
        end.
        
        i = 0.
        for each w-i:
          i = i + 1.
        end.
        if i lt 4 then do i = i + 1 to 4:
          create w-i.
        end.

        find first w-i.
        v-ink-1 =  w-i.i-dscr +
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find next w-i.
        v-ink-2 =  w-i.i-dscr +
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find next w-i.
        v-ink-3 =  w-i.i-dscr +
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find NEXT w-i.
        v-ink-4 =  w-i.i-dscr +
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".

        v-form-sqft = round(if v-corr then (v-form-len * v-form-wid * .007)
                                       else (v-form-len * v-form-wid / 144),3).
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.die-no
                            no-lock no-error.
        v-die-loc = IF AVAIL xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

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
              v-cus[3] AT 2 "Cust Part#:" AT 40 lv-part-no
              "Joint:" AT 80 v-joint-dscr             
              SKIP
              v-cus[4] AT 2
              "Item Name: " AT 40 lv-fg-name FORM "x(26)"
              SKIP(1)
              "<P8><C13><B><U>Board:</U>" /*AT 12*/ "<C45><U>Printing:</U>" /*AT 65*/ "<C80><U>Die Cutting, Slit, & Saw</B></U>" /*AT 125*/ "<P10>"SKIP
              "Shts Req'd:" AT 2 trim(string(v-sht-qty))   format "x(9)"
                " Sq Ft:" trim(string(v-form-sqft)) format "x(7)"
              "PRINTING PLATE #:" AT 40 xeb.plate-no when avail xeb
              "Die #" AT 80 xeb.die-no when avail xeb " Loc:" v-die-loc SKIP
              "W:" + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))  format "x(22)" AT 2
              "MSF:"  + trim(string(v-sht-qty * v-form-sqft / 1000,">>>9.9<"))
                                                                format "x(11)"
              "Ink 1:" AT 40 v-ink-1
              "Gross Size:" AT 80  
              "W:" + trim(string({sys/inc/k16v.i xef.gsh-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.gsh-len},">>>9.99"))
                                                                format "x(20)"
              "Slit: W:" + string(v-outw) + " L:" + string(v-outl) FORM "x(15)"   
              SKIP
              "Board:<B>" AT 2 v-form-code FORM "x(20)" "</B><C33.4>Ink 2:" /*AT 40*/ v-ink-2 "<C66.8>Net   Size:" /*AT 80*/
              "W:" + trim(string({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>9.99")) format "x(22)"
              SKIP
              "Due In:" AT 2 v-po-due-date "Ink 3:" AT 40 v-ink-3 "Die   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(22)"
              "Up:" "W:" + string(v-upl) + " L:" + string(v-upw) FORM "x(9)"
              SKIP
              v-qty-or-sup AT 2 FORM "x(36)" 
              "Ink 4:" AT 40 v-ink-4 "Blank Size:" AT 80 
              "W:" + trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)"
              SKIP
              "Score:" AT 2 substring(v-len-score,1,30) WHEN xstyle.TYPE <> "F" format "x(30)" 
              "Color Desc:" AT 40 xeb.i-coldscr when avail xeb
              "Impressions: " AT 80 trim(string(v-dc-qty))    format "x(7)"
              SKIP
              "Adders:<B>" AT 2 v-adders FORM "x(23)" "</B>"
              "<C66.8>D/C Style:" /*AT 80*/ SKIP(1)
              WITH FRAME job1 NO-LABEL NO-BOX WIDTH 200 STREAM-IO.
                         
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
        put skip
            "<P7>   Machine Routing         Crew/ #   Shift       SU:        Start       Stop      Total    RUN:        Hours          Start    Stop    Total  QTY: In     Out    Date   Checkout<P10>" SKIP.

        ASSIGN
          i = 0
          w-m-count = 0.

        for each w-m by w-m.dseq:

          i = i + 1.

          IF w-m.dscr NE "" THEN
             w-m-count = w-m-count + 1.

          display w-m.dscr AT 3 FORM "x(14)"
                  fill("_",4)  format "x(4)"    to 22   when w-m.dscr ne ""
                  fill("_",1)  format "x"    to 24   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 30   when w-m.dscr ne ""
                  w-m.s-hr                              when w-m.s-hr ne 0
                  fill("_",5)  format "x(5)"    to 46   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 54   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 62   when w-m.dscr ne ""
                  space(1)
                  w-m.r-sp                              when w-m.r-sp ne 0
                  w-m.r-hr                              when w-m.r-hr ne 0
                  fill("_",5)  format "x(5)"    to 87   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 93   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 99   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 105  when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 111  when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 117  when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 123  when w-m.dscr ne ""
              with no-box no-labels frame oo1 width 150 no-attr-space down STREAM-IO.
                  
          v-lines = v-lines + 1.
        end.
      
        IF w-m-count LE 2 THEN
          display 
               fill("_",121)  format "x(121)"    to 123
               fill("_",121)  format "x(121)"    to 123  
              with no-box no-labels frame oo2 width 150 no-attr-space down STREAM-IO.
        ELSE IF w-m-count EQ 3 THEN
          display 
               fill("_",121)  format "x(121)"    to 123
              with no-box no-labels frame oo3 width 150 no-attr-space down STREAM-IO.

        FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                           AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        
        IF AVAIL b-ef AND b-ef.form-no = w-ef.frm THEN 
           FOR EACH w-m:
               CREATE tt-wm.
               BUFFER-COPY w-m TO tt-wm.
        END.

        PUT SKIP(1).

        PUT "<#6><C1><FROM><C105><LINE><|3>" .
            
        RUN cecrep/jobspect2.p (RECID(job-hdr),v-format,cust.terms).
        PUT "<=6><C28><FROM><R+7><C28><LINE><|3>"
            "<=6><C65><FROM><R+7><C65><LINE><|3>".
        
        RUN stackImage.

        PUT "<#7><C1><FROM><C105><LINE><|3>"
            "<=7><C28><FROM><R+13><C28><LINE><|3>"
            "<=7><C74><FROM><R+13><C74><LINE><|3>"
            "<P8>" SKIP.

        DISPLAY "<=7><C45><U><B>Floor Comments</U>" AT 2   "<U>Shipping Info: </U></B> <P10>" AT 90 SKIP
                "Ship To #:" AT 90 
                xoe-ord.sold-id when avail xoe-ord
                xeb.ship-id when avail xeb @ xoe-ord.sold-id
                xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id  SKIP
                "____________________________________________________" AT 35 v-shp[1] AT 90 SKIP
                "____________________________________________________" AT 35 v-shp[2] AT 90 SKIP
                "____________________________________________________" AT 35 v-shp[3] AT 90 SKIP
                "____________________________________________________" AT 35 v-shp[4] AT 90 SKIP
                "____________________________________________________" AT 35 "Item PO #:" AT 90 xoe-ordl.po-no when avail xoe-ordl
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                /*"____________________________________________________________________________" SKIP */
               with no-box no-labels frame m8 width 170 no-attr-space STREAM-IO.

        v-note-length = 124.
        
        DEF VAR li AS INT NO-UNDO.

        FOR EACH tt-formtext:
            DELETE tt-formtext.
        END.
        lv-text = "".
        FOR EACH notes WHERE notes.rec_key = job.rec_key and
                       (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
                       AND NOT CAN-DO(cha-val,notes.note_code) NO-LOCK:
            lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
        END.
        DO li = 1 TO 6:
           CREATE tt-formtext.
           ASSIGN tt-line-no = li
                  tt-length  = 124.
        END.
        RUN custom/formtext.p (lv-text).
        i = 0.
        v-dept-note = "".
        FOR EACH tt-formtext:
            i = i + 1.
            IF i <= 6 THEN v-dept-note[i] = tt-formtext.tt-text.      
        END.

        ASSIGN
          v-inst = ""
          lv-text = "".
        
        FOR EACH tt-formtext:
          DELETE tt-formtext.
        END.
        
        FOR EACH notes WHERE notes.rec_key = itemfg.rec_key NO-LOCK:
            lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
        END.

        DO li = 1 TO 6:
           CREATE tt-formtext.
           ASSIGN tt-line-no = li
                  tt-length  = 122.
        END.
        RUN custom/formtext.p (lv-text).
        i = 0.
        v-spec-note = "".
        FOR EACH tt-formtext:
            i = i + 1.
            IF  i <= 6 THEN v-spec-note[i] = tt-formtext.tt-text.      
        END.

        PAGE.

        PUT "<#11><C1><FROM><C105><R+47><RECT><|3>"  
            "<=11> Department Notes" SKIP
            v-dept-note[1] AT 2 SKIP
            v-dept-note[2] AT 2 SKIP
            v-dept-note[3] AT 2 SKIP
            v-dept-note[4] AT 2 SKIP
            v-dept-note[5] AT 2 SKIP
            v-dept-note[6] AT 2 SKIP
            "<C1><FROM><C105><LINE><|3>" 
            "<C1> Spec Notes" SKIP
            v-spec-note[1] FORM "x(122)" AT 3 SKIP
            v-spec-note[2] FORM "x(122)" AT 3 SKIP
            v-spec-note[3] FORM "x(122)" AT 3 SKIP
            v-spec-note[4] FORM "x(122)" AT 3 SKIP
            v-spec-note[5] FORM "x(122)" AT 3 SKIP
            v-spec-note[6] FORM "x(122)" AT 3 SKIP.

        if print-box and avail xest THEN
           run cec/desprnsp.p (recid(xef),
                              input-output v-lines,
                              recid(xest)).
        PAGE.

      end.  /* for each w-ef */

      IF s-prt-set-header AND last-of(job.job-no2) AND est.est-type = 6 THEN DO: /* print set header */
         i = 0.
         FOR EACH bf-eb WHERE bf-eb.company = est.company
                    AND bf-eb.est-no = est.est-no
                    AND bf-eb.form-no > 0 NO-LOCK:
             i = i + 1.
         END.   
         IF i > 1 THEN DO:
             
             v-fg-set = job-hdr.i-no.
             v-set-qty = if avail xeb and xeb.est-type eq 6 THEN
                           if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty
                         ELSE 0.
             v-ord-qty = (if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty) *
                         if est.form-qty le 1 then 1 else v-pqty.
             v-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                          IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE "".
             v-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                           IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE "".            
             PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
                 "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
                 "<C67>Our Date: " v-ord-date SKIP
                 "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
                 "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
                 "<=1><R+6><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C58>ITEM DESCRIPTION" SKIP
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
             FOR EACH xeb WHERE xeb.company = est.company
                             AND xeb.est-no = est.est-no
                             AND xeb.form-no > 0 NO-LOCK:
                 PUT xeb.stock-no AT 3 space(14) xeb.part-dscr1 space(5) xeb.quantityPerSet FORMAT ">>>>9.9<<<" SKIP.
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
             v-tmp-line = v-tmp-line + 3 + i.
        
             v-shipto = IF AVAIL xoe-rel THEN xoe-rel.ship-id 
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
    "<#stackImage><C27><R+1><FROM><C2><R+12>"
    "<IMAGE#stackImage=" stackPattern.stackImage ">"
    "<R-13>".
END PROCEDURE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
