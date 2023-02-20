/* ----------------------------------------------  */
/*  cecrep/jobmcga.p  factory ticket  for United <= Xprint landscape */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF SHARED VAR v-dept-codes AS CHAR NO-UNDO.
DEF VAR prt-copies AS INT  NO-UNDO.
DEF VAR v-start-compress AS cha NO-UNDO.
DEF VAR v-end-compress AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ink-1 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-2 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-3 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-4 AS cha FORM "X(30)" NO-UNDO.
DEF var v-dept-note AS cha FORM "x(55)" EXTENT 15 NO-UNDO.
DEF var v-dept-note2 AS cha FORM "x(38)" EXTENT 10 NO-UNDO.
DEF var v-spec-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF VAR v-deptnote AS cha NO-UNDO.
DEF VAR v-dept-length AS DEC NO-UNDO.
DEF VAR lv-under-run AS cha NO-UNDO.
DEF VAR li-under-run AS DECIMAL NO-UNDO.
DEF VAR lv-over-run AS cha NO-UNDO.
DEF VAR li-over-run AS DECIMAL NO-UNDO.
DEF VAR lv-part-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-fg-name AS cha NO-UNDO.
DEF VAR v-cust-po LIKE oe-ordl.po-no NO-UNDO.
DEF VAR v-req-code1 LIKE oe-ordl.req-code NO-UNDO.
DEF VAR v-req-code2 LIKE oe-ordl.req-code NO-UNDO.

DEF VAR v-contact AS cha NO-UNDO.
DEF VAR v-perf AS LOG NO-UNDO.
DEF VAR v-reversed AS LOG NO-UNDO.
DEF VAR v-set-qty AS INT NO-UNDO.
DEF VAR v-tel AS cha NO-UNDO.
DEF VAR v-fax AS cha NO-UNDO.
DEF VAR v-email AS cha NO-UNDO.
DEF VAR v-cas-no AS cha NO-UNDO.
DEF VAR v-qty-1 AS INT NO-UNDO.
DEF VAR v-qty-2 AS INT NO-UNDO.
DEF VAR v-num-tags AS INT NO-UNDO.
DEF VAR v-due-date1 AS DATE NO-UNDO.
DEF VAR v-due-date2 AS DATE NO-UNDO.
DEF VAR v-dock-note AS cha NO-UNDO.

DEF VAR v-board-type-1 AS cha NO-UNDO.
DEF VAR v-board-type-2 AS cha NO-UNDO.
DEF VAR v-board-size-1 AS cha NO-UNDO.
DEF VAR v-board-size-2 AS cha NO-UNDO.
DEF VAR v-board-qty-1 AS INT FORMAT ">>>>>>>" NO-UNDO.
DEF VAR v-board-qty-2 AS INT FORMAT ">>>>>>>" NO-UNDO.
DEF VAR v-slot-height1 AS DEC FORMAT ">9.99999" NO-UNDO.
DEF VAR v-slot-height2 AS DEC FORMAT ">9.99999" NO-UNDO.
DEF VAR v-dc-msetup AS cha EXTENT 4 NO-UNDO.
DEF VAR v-dc-setup AS cha EXTENT 4 NO-UNDO.
DEF VAR v-dies AS cha EXTENT 4 NO-UNDO.
DEF VAR tb_app-unprinted AS LOG NO-UNDO.
DEF VAR fi_per-set AS DEC NO-UNDO.
DEF VAR fi_msf AS DEC FORMAT "99.9999" NO-UNDO.

{jcrep/r-ticket.i "shared"}

{cecrep/jobmcga.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}
{sys/inc/f16to32.i}
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
 v-qty-or-sup = if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") gt 0
                then ("Supplier:"     + fill("_",28))
                else ("Qty Received: " + fill("_",24)).


    assign /*v-local-copies = if lookup(printer.pr-port, "{&PR-PORT}") eq 0 then 
                              prt-copies
                            else 1  */
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

      for each w-ef BREAK BY w-ef.frm:
        release xef.
        release xeb.
        release xstyle.
        release xxprep.

        run cecrep/jobtick2.p (recid(w-ef), recid(job-hdr)).

        FIND FIRST est-prep WHERE est-prep.company = est.company 
            AND est-prep.est-no = est.est-no NO-LOCK NO-ERROR.

        FIND FIRST est-qty WHERE est-qty.company = est.company
          AND est-qty.est-no = est.est-no NO-LOCK NO-ERROR.
        
        FIND FIRST est-op WHERE est-op.company = est-qty.company
          AND est-op.est-no = est-qty.est-no
          AND est-op.qty eq est-qty.eqty 
          AND est-op.line LT 500 
          AND est-op.s-num EQ xeb.form-no
          and est.est-type ne 8 NO-LOCK NO-ERROR.
        
        v-pqty = 1.
        v-cp = "".
        if avail xeb then do:
          if xeb.stock-no ne "" then v-fg = xeb.stock-no.
          v-cp = xeb.part-no.
          /*if xest.est-type eq 6 then v-fg = trim(v-fg) + "  CP#: " +
                                            xeb.part-no . */
            
          ASSIGN lv-fg-name = itemfg.i-name.

          {cec/rollfac.i}
          v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                   if xeb.quantityPerSet lt 0 then (-1 / xeb.quantityPerSet)
                                       else xeb.quantityPerSet.
        end.
        
        assign
         v-loc     = ""
         v-loc-bin = "".
        
        ASSIGN lv-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                             IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE ""
               li-over-run  = IF AVAIL xoe-ordl THEN DECIMAL(xoe-ordl.over-pct) ELSE
                              IF AVAIL xoe-ord  THEN DECIMAL(xoe-ord.over-pct)  ELSE 0
               lv-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                              IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE ""
               li-under-run = IF AVAIL xoe-ordl THEN DECIMAL(xoe-ordl.under-pct) ELSE
                              IF AVAIL xoe-ord  THEN DECIMAL(xoe-ord.under-pct)  ELSE 0
               lv-part-name = IF AVAIL xeb THEN xeb.part-dscr1 ELSE " "   
               lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no ELSE itemfg.part-no
               v-cust-po = IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE "" 
               v-req-code1 = IF AVAIL xoe-ordl THEN xoe-ordl.req-code ELSE ""
               v-req-code2 = IF AVAIL xoe-ordl THEN xoe-ordl.req-code ELSE "".
                                                                          

    ASSIGN v-perf = YES
           v-tel = cust.area-code + "-" + cust.phone
           v-cas-no = ""
           v-fax = substring(cust.fax,1,3) + '-' + substring(cust.fax,4)
           v-email   = cust.email
           v-contact = cust.contact
           v-num-tags = 1 
           .
    IF AVAIL xef THEN 
        ASSIGN v-reversed = (xef.xgrain = "B" OR xef.xgrain = "S").

    ASSIGN v-set-qty = if avail xeb and xeb.est-type eq 6 THEN
                       if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty
                       ELSE 0.

    IF AVAIL est-op THEN DO:
        IF w-ef.frm = 1 THEN DO:
        
            ASSIGN  
            v-dc-msetup[1] = est-op.att-type[1]
            v-dc-msetup[2] = est-op.att-type[2]
            v-dc-msetup[3] = est-op.att-type[3].
            IF AVAIL est-prep THEN
                v-dc-msetup[4] = est-prep.code.
        END.
    END.

    IF AVAIL est-op THEN DO:
        IF w-ef.frm = 2 THEN DO:
        
            ASSIGN  
            v-dc-setup[1] = est-op.att-type[1]
            v-dc-setup[2] = est-op.att-type[2]
            v-dc-setup[3] = est-op.att-type[3].
            IF AVAIL est-prep THEN
                v-dc-setup[4] = est-prep.code.
        END.
    END.
    IF w-ef.frm = 1 THEN DO:
        ASSIGN
            fi_per-set = IF xeb.quantityPerSet LT 0 THEN -1 / xeb.quantityPerSet ELSE xeb.quantityPerSet
                fi_msf  = (est-qty.eqty * fi_per-set) *
                (IF v-corr THEN (xeb.t-sqin * .007)
                    ELSE (xeb.t-sqin / 144)) / 1000.

       ASSIGN v-due-date1 = date(v-due-date)
              v-board-type-1 = v-form-code
              v-board-size-1 = string(fi_msf)
              /*"W:" + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
                               "  " +
                               "L:" + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))*/
              v-dies[1] =      ""
              v-dies[2] =      ""
              v-slot-height1 =  {sys/inc/k16.i xeb.dep } 
              v-qty-1 = v-set-qty - (v-set-qty * li-under-run  / 100 )
              v-board-qty-1 = ( (v-set-qty * li-over-run  / 100 ) + v-set-qty) * xeb.quantityPerSet 
              .
    END.
    
    ELSE IF w-ef.frm = 2 THEN DO:
        ASSIGN
            fi_per-set = IF xeb.quantityPerSet LT 0 THEN -1 / xeb.quantityPerSet ELSE xeb.quantityPerSet
                fi_msf  = (est-qty.eqty * fi_per-set) *
                (IF v-corr THEN (xeb.t-sqin * .007)
                    ELSE (xeb.t-sqin / 144)) / 1000.
    
         ASSIGN v-due-date2 = date(v-due-date)
              v-board-type-2 = v-form-code
              v-board-size-2 = string(fi_msf)
              /*"W:" + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
                               "  " +
                              "L:" + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))*/
             
              v-dies[3] =     ""
              v-dies[4] =     ""
              v-board-qty-2  = ((v-set-qty * li-over-run  / 100) + v-set-qty) * xeb.quantityPerSet
              v-slot-height2 = {sys/inc/k16.i xeb.dep } 
              .
         END.
         
   ASSIGN 
       v-qty-2  = v-set-qty + (v-set-qty * li-over-run  / 100)
       v-note-length = 40.

        ASSIGN v-tmp-lines = 0
               j = 0
               K = 0
               lv-got-return = 0
               v-dept-note2 = "" 
               v-prev-note-rec = ?.

        FOR EACH notes WHERE notes.rec_key = job.rec_key and
                    (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
                     AND notes.note_code = "LI"  NO-LOCK:
            IF v-prev-note-rec <> ? AND
               v-prev-note-rec <> RECID(notes) THEN v-prev-extent = v-prev-extent + k.

            DO i = 1 TO LENGTH(notes.note_text):
               IF i - j >= v-note-length THEN ASSIGN j = i
                                            lv-got-return = lv-got-return + 1.

               v-tmp-lines = ( i - j ) / v-note-length.
               {SYS/INC/ROUNDUP.I v-tmp-lines}

               k = v-tmp-lines + lv-got-return + 
                   IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.
               IF k < 5 THEN v-dept-note2[k] = v-dept-note2[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) 
                             ELSE "" .              

               IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
               THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
               END.         
            END.
            
            ASSIGN v-prev-note-rec = RECID(notes)
                   j = 0
                   lv-got-return = 0.
        END. /* notes */

    IF LAST(w-ef.frm) THEN DO:
       PUT    "</PROGRESS>"
       "<P12><C2><B>CUSTOMER NAME:</B> " v-cus[1]
       "<B><C55>DESCRIPTION/PART #:</B> " lv-part-no SKIP
       "<C2><B>CUSTOMER PO#:</B> "  v-cust-po "<C55><B>Due Date:</B><P10> " v-due-date SKIP
       "<#1><C1><FROM><C105><R+45><RECT><|3>" 
       "<=1><C35><FROM><R+15><C35><LINE><|3>"
       "<=1><C69><FROM><R+15><C69><LINE><|3>"
       /*"<=1><C90><FROM><R+4><C90><LINE><|3>"*/
       "<=1><R+5><C1><FROM><C105><LINE><|3>"
       "<=#1><R+10><C1><FROM><C105><LINE><|3>"
       /*"<=1><R+19><C1><FROM><C105><LINE><|3>" */      
       .

       view frame head.  /* factory header display  */  
    END.
      END. /* w-ef*/
/* ===
      assign
           v-i-line[1] = "ITEM DESCRIPTION"
           v-i-line[2] = "Style: " + if avail xstyle then xstyle.dscr else ""
           v-i-line[3] = "Size: "  + if avail xeb    then
                     trim(string({sys/inc/k16v.i xeb.len},">>>,>>9.99<<<<")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.wid},">>>,>>9.99<<<<")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
           v-i-line[4] = "Joint: " + if avail xeb then v-joint-dscr else "".
        
        lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                     ELSE itemfg.part-no.
                           
        /*if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") gt 0 then do:*/
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
         v-qty-or-sup = "Supplier: ".

        if v-vend-no ne "" then do:
           v-qty-or-sup = v-qty-or-sup + trim(v-vend-no).
           IF v-po-no ne 0 THEN v-qty-or-sup = v-qty-or-sup + " PO#:" +
                                               trim(string(v-po-no,">>>>>>>>>>")).
        end.
       /* v-qty-or-sup = v-qty-or-sup + fill("_",38 - length(v-qty-or-sup)).*/
        
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
/*
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
              "Size:" (trim(string({sys/inc/k16v.i xeb.len},">>>,>>9.99<<<<")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.wid},">>>,>>9.99<<<<")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99"))) FORM "x(30)" WHEN AVAIL xeb            
              SKIP
              v-cus[3] AT 2 "Cust Part#:" AT 40 lv-part-no
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
              "Adders:" AT 80 v-adders FORM "x(23)"
              "<P7>"SKIP(1)
              "Board:" AT 2 "Printing:" AT 55 "Die Cutting, Slit, & Saw" AT 115 "<P10>"SKIP
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
              "Board:" AT 2 v-form-code FORM "x(30)" "Ink 2:" AT 40 v-ink-2 "Net   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.nsh-wid},">>>>>9.99<<<<")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>>>9.99<<<<")) format "x(23)"
              SKIP
              v-form-dscr AT 4 FORM "x(30)" "Ink 3:" AT 40 v-ink-3 "Die   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(22)"
              "Up:" "W:" + string(v-upl) + " L:" + string(v-upw) FORM "x(9)"
              SKIP
           /*   "Score:" AT 2 v-len-score               format "x(30)" */
               v-qty-or-sup AT 2 FORM "x(36)" 
              "Ink 4:" AT 40 v-ink-4 "Blank Size:" AT 80 
              "W:" + trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)"
              SKIP
              /*v-qty-or-sup AT 3 FORM "x(36)" */
              "Score:" AT 2 substring(v-len-score,1,30) WHEN xstyle.TYPE <> "F" format "x(30)" 
              "Color Desc:" AT 40 xeb.i-coldscr when avail xeb
              "Impressions: " AT 80 trim(string(v-dc-qty))    format "x(7)"
              SKIP
              SUBSTRING(v-len-score,31,40) AT 6 FORM "x(40)"
              "D/C Style:" AT 80 SKIP(1)
              WITH FRAME job1 NO-LABEL NO-BOX WIDTH 150 STREAM-IO.

*/         
===*/    
      FIND FIRST w-ef.

      IF AVAILABLE w-ef THEN DO:
      
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

        /*PUT "<=#8><R+1><C+4><from><C+100><Line>" SKIP.*/
        put "<C1><FROM><C105><LINE><|3>"       SKIP
            "<R-1><P7>   Machine Routing                    SU:      Start       Stop        Total          RUN:     Hours      Start       Stop       Total       QTY:  In        Out         Waste <P10>" SKIP.
        i = 0.
        for each w-m by w-m.dseq:
          i = i + 1.
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
        IF i < 7 THEN PUT SKIP(7 - i).

        FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                           AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        
        IF AVAIL b-ef AND b-ef.form-no = w-ef.frm THEN 
           FOR EACH w-m:
               CREATE tt-wm.
               BUFFER-COPY w-m TO tt-wm.
        END.

        PUT "<#6><C1><FROM><C105><LINE><|3>" .

        RUN cecrep/jobmcga2.p (RECID(job-hdr),v-format,cust.terms,itemfg.prod-notes).
        PUT "<=6><C26><FROM><R+8><C26><LINE><|3>"
            /*"<=6><C44><FROM><R+8><C44><LINE><|3>"*/
            "<=6><C66><FROM><R+8><C66><LINE><|3>"
            /*"<=6><C79><FROM><R+8><C79><LINE><|3>"            */
            /*"<=6><C93><FROM><R+8><C93><LINE><|3>"*/.
        
        RUN stackImage.

        PUT "<#7><C1><FROM><C105><LINE><|3>"
            "<=7><C28><FROM><R+14><C28><LINE><|3>"
            "<=7><C75><FROM><R+14><C75><LINE><|3>"
            "<P7>" SKIP.

        DISPLAY "<=7><C77> Shipping Info: <P10>" AT 92 SKIP
                "Ship To #:" AT 91
                xoe-ord.sold-id when avail xoe-ord
                xeb.ship-id when avail xeb @ xoe-ord.sold-id
                xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id SKIP
                /*"____________________________________________________" AT 35*/ v-shp[1] AT 91 SKIP
               /* "____________________________________________________" AT 35*/ v-shp[2] AT 91 SKIP
               /* "____________________________________________________" AT 35*/ v-shp[3] AT 91 SKIP
               /* "____________________________________________________" AT 35*/ v-shp[4] AT 91 SKIP
                /*"____________________________________________________" AT 35 */"Item PO #:" AT 91 xoe-ordl.po-no when avail xoe-ordl
                /*"____________________________________________________" AT 35*/ SKIP
                /*"____________________________________________________" AT 35*/ "Dock Hours:" AT 91 v-dock-note SKIP
                /*"____________________________________________________" AT 35*/ SKIP
                /*"____________________________________________________" AT 35*/ SKIP
                /*"____________________________________________________" AT 35*/ SKIP
                /*"____________________________________________________" AT 35*/ SKIP
                /*"____________________________________________________________________________" SKIP */
               with no-box no-labels frame m8 width 170 no-attr-space STREAM-IO.

        /*PAGE.*/
        /* dept notes */
        v-note-length = 124.
        ASSIGN v-tmp-lines = 0
               j = 0
               K = 0
               lv-got-return = 0
               v-dept-note = "" 
               v-prev-note-rec = ?.

        FOR EACH notes WHERE notes.rec_key = job.rec_key and
                    (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
                     AND LOOKUP(notes.note_code,v-dept-codes) <> 0
                 NO-LOCK:
            IF v-prev-note-rec <> ? AND
               v-prev-note-rec <> RECID(notes) THEN v-prev-extent = v-prev-extent + k.

            DO i = 1 TO LENGTH(notes.note_text):
               IF i - j >= v-note-length THEN ASSIGN j = i
                                            lv-got-return = lv-got-return + 1.

               v-tmp-lines = ( i - j ) / v-note-length.
               {SYS/INC/ROUNDUP.I v-tmp-lines}

               k = v-tmp-lines + lv-got-return + 
                   IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.
               IF k < 12 THEN v-dept-note[k] = v-dept-note[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) 
                             ELSE "" .              

               IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
               THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
               END.         
            END.
            
            ASSIGN v-prev-note-rec = RECID(notes)
                   j = 0
                   lv-got-return = 0.
        END. /* notes */
                
        PUT "<=7><c30> Departments" SKIP
            v-dept-note[1]  AT 34 SKIP
            v-dept-note[2] AT 34 SKIP
            v-dept-note[3] AT 34 SKIP
            v-dept-note[4] AT 34 SKIP
            v-dept-note[5] AT 34 SKIP
            v-dept-note[6]  AT 34 SKIP
            v-dept-note[7] AT 34 SKIP
            v-dept-note[8] AT 34 SKIP
            v-dept-note[9] AT 34 SKIP
            v-dept-note[10] AT 34 SKIP
            v-dept-note[11] AT 34.
        PAGE.
        v-inst = "".

        ASSIGN v-spec-note = ""
           v-tmp-lines = 0
           j = 0
           K = 0
           lv-got-return = 0.

        FOR EACH notes WHERE notes.rec_key = itemfg.rec_key NO-LOCK.
            DO i = 1 TO LENGTH(notes.note_text) :        
               IF i - j >= v-note-length THEN ASSIGN j = i
                                              lv-got-return = lv-got-return + 1.
                   
               v-tmp-lines = ( i - j ) / v-note-length.
               {SYS/INC/ROUNDUP.I v-tmp-lines}

               k = v-tmp-lines + lv-got-return.
               IF k < 9 THEN v-spec-note[k] = v-spec-note[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) 
                                  ELSE "" .              
           
               IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
               THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
               END.         
            END.
        END.
/*
        PUT "<#11><C1><FROM><C105><R+47><RECT><|3>"  
            "<=11> Department Notes" SKIP
            v-dept-note[1]  AT 2 SKIP
            v-dept-note[2] AT 2 SKIP
            v-dept-note[3] AT 2 SKIP
            v-dept-note[4] AT 2 SKIP
            v-dept-note[5] AT 2 SKIP
            v-dept-note[6] AT 2 SKIP
            "<C1><FROM><C105><LINE><|3>" 
            "<C1> Spec Notes" SKIP
           /* entry(1,v-inst) FORM "x(122)" AT 2 SKIP
            entry(2,v-inst) FORM "x(122)" AT 2 SKIP
            entry(3,v-inst) FORM "x(122)" AT 2 SKIP
            entry(4,v-inst) FORM "x(122)" AT 2 SKIP
            entry(5,v-inst) FORM "x(122)" AT 2 SKIP
            entry(6,v-inst) FORM "x(122)" AT 2 SKIP
            */
            v-spec-note[1] FORM "x(122)" AT 2 SKIP
            v-spec-note[2] FORM "x(122)" AT 2 SKIP
            v-spec-note[3] FORM "x(122)" AT 2 SKIP
            v-spec-note[4] FORM "x(122)" AT 2 SKIP
            v-spec-note[5] FORM "x(122)" AT 2 SKIP
            v-spec-note[6] FORM "x(122)" AT 2 SKIP
            .
*/            
        if print-box and avail xest then do:
            /*PAGE. */
            run cec/desprntU.p (recid(xef),
                               input-output v-lines,
                               recid(xest)).
            PAGE.
        end.
        ELSE PAGE.

        RELEASE w-ef.
      end.  /*w-ef */

      s-prt-set-header = NO.

      IF s-prt-set-header AND last-of(job.job-no2) AND est.est-type = 6 THEN DO: /* print set header */
         i = 0.
         FOR EACH bf-eb WHERE bf-eb.company = est.company
                    AND bf-eb.est-no = est.est-no
                    AND bf-eb.form-no > 0 NO-LOCK:
             i = i + 1.
         END.   
         IF i > 1 THEN DO:
             DEF VAR v-set-qty2 AS INT NO-UNDO.
             DEF VAR v-ord-qty AS INT NO-UNDO.
             DEF VAR v-over-run AS cha NO-UNDO.
             DEF VAR v-under-run AS cha NO-UNDO.
             DEF VAR v-fg-set AS cha FORM "x(15)" NO-UNDO.
             v-fg-set = job-hdr.i-no.
             v-set-qty2 = if avail xeb and xeb.est-type eq 6 THEN
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
                 "<C60>Our Date: " v-ord-date SKIP
                 "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
                 "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
                 "<=1><R+6><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
                 v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty2
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
                 PUT xeb.stock-no AT 3 space(14) xeb.part-dscr1 space(5) xeb.quantityPerSet FORMAT ">>>>9.9<<<"  SKIP.
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

             PUT /*"<C2>Machine Routing:  <C15> SU:    Start    Stop     Total    Run:   Start   Stop    total   qty   in   out  waste  date" SKIP*/
                 "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
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
             v-tmp-line = v-tmp-line + 3 + i /* 4 and add machine routing lines */.
        
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
    "<#71><C27><R+1><FROM><C2><R+12>"
    "<IMAGE#71=" stackPattern.stackImage ">"
    "<R-13>".
END PROCEDURE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
