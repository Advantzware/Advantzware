/* ---------------------------------------------- cec/rep/jobtick.p 04/97 JLF */
/*  cecrep/jobpacifi.p  factory ticket  for Pacific landscape */
/* -------------------------------------------------------------------------- */
/*  YSK 06/08/01  change local var v-out1-id, v-out2-id to shared var for despr~nt1.p  */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF VAR prt-copies AS INT  NO-UNDO.
DEF VAR v-start-compress AS cha NO-UNDO.
DEF VAR v-end-compress AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ink-1 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-2 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-3 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-4 AS cha FORM "X(30)" NO-UNDO.
DEF var v-dept-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF VAR v-deptnote AS cha NO-UNDO.
DEF VAR v-dept-length AS DEC NO-UNDO.

{jcrep/r-ticket.i "shared"}

{cecrep/jobpacif.i "new shared"}

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

/*
PUT    "</PROGRESS>"
       "<P12><B>Pacific Packaging" AT 9
       "Production Specification </B><P10>" AT 50 SKIP
       "<#1><C1><FROM><C107><R+47><RECT><|3>" 
       "<=1><C32><FROM><R+19><C32><LINE><|3>"
       "<=1><C66><FROM><R+19><C66><LINE><|3>"
       "<=1><C90><FROM><R+4><C90><LINE><|3>"
       "<=1><R+4><C1><FROM><C107><LINE><|3>"
       "<=#1><R+10><C1><FROM><C107><LINE><|3>"
       "<=1><R+19><C1><FROM><C107><LINE><|3>"       
       .
*/

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

      for each w-ef:
        release xef.
        release xeb.
        release xstyle.
        release xxprep.
        
        run cecrep/jobtick2.p (recid(w-ef), recid(job-hdr)).

        v-pqty = 1.
        if avail xeb then do:
          if xeb.stock-no ne "" then v-fg = xeb.stock-no.
          if xest.est-type eq 6 then v-fg = trim(v-fg) + "  CP#: " +
                                            xeb.part-no.
          
          {cec/rollfac.i}
          v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                   if xeb.quantityPerSet lt 0 then (-1 / xeb.quantityPerSet)
                                       else xeb.quantityPerSet.
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

        PUT    "</PROGRESS>"
       "<P12><B>Pacific Packaging" AT 9
       "Production Specification </B><P10>" AT 50 SKIP
       "<#1><C1><FROM><C105><R+45><RECT><|3>" 
       "<=1><C32><FROM><R+19><C32><LINE><|3>"
       "<=1><C66><FROM><R+19><C66><LINE><|3>"
       "<=1><C90><FROM><R+4><C90><LINE><|3>"
       "<=1><R+4><C1><FROM><C105><LINE><|3>"
       "<=#1><R+10><C1><FROM><C105><LINE><|3>"
       "<=1><R+19><C1><FROM><C105><LINE><|3>"       
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

   
        /* 3 Boxes for customer */
        
            
        /* 3 boxes for board */
              
  /*      PUT "<=#8><R+1><C-1>O" 
            "<=#8><R+2><C-1>U" 
            "<=#8><R+3><C-1>T" 
            "<=#8><R+4><C-1>E" .
  */
        lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                     ELSE itemfg.part-no.
                           
        /*if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") gt 0 then do:*/
        ASSIGN v-vend-no = ""
               v-po-no   = 0.

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

        DISP  v-cus[1] AT 2 "PO #:" AT 40 xoe-ord.po-no WHEN AVAIL xoe-ord 
              "Style:" AT 80 xstyle.dscr WHEN AVAIL xstyle              
              SKIP
              v-cus[2] AT 2
              "Set Qty:" AT 40 trim(string(if avail xoe-ordl then xoe-ordl.qty
                                               else job-hdr.qty,">,>>>,>>9")) format "x(9)"
              "Qty/Set:" trim(string(IF AVAIL xeb THEN xeb.quantityPerSet ELSE 1,"->>,>>9.9<<<")) format "x(12)"
              "Size:" AT 80 (trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                             trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                             trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99"))) FORM "x(30)" WHEN AVAIL xeb            
              SKIP
              v-cus[3] AT 2
              "Job Qty:" AT 40 trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
              "Order Qty:" trim(string((if avail xoe-ordl then xoe-ordl.qty
                                             else job-hdr.qty) *
                                            if est.form-qty le 1 then 1
                                            else v-pqty,">>>,>>9"))
                                            format "x(7)"
              "   "
              "Joint:" AT 80 v-joint-dscr             
              SKIP
              v-cus[4] AT 2
              "Cust Part#:" AT 40 lv-part-no
              "Adders:" AT 80 v-adders FORM "x(23)"
              SKIP
              "Overrun:" AT 40 trim(string(IF AVAIL xoe-ordl THEN xoe-ordl.over-pct ELSE
                                           IF AVAIL xoe-ord  THEN xoe-ord.over-pct  ELSE 0,">>9.99%")) format "x(7)"
              "Underrun:" trim(string(IF AVAIL xoe-ordl THEN xoe-ordl.under-pct ELSE
                                      IF AVAIL xoe-ord  THEN xoe-ord.under-pct  ELSE 0,">>9.99%")) format "x(7)"
              "<P7>"
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
              "W:" + trim(string({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>9.99")) format "x(22)"
              SKIP
              v-form-dscr AT 4 FORM "x(30)" "Ink 3:" AT 40 v-ink-3 "Die   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(22)"
              "Up:" "W:" + string(v-upl) + " L:" + string(v-upw) FORM "x(9)"
              SKIP
              "Score:" AT 2 v-len-score   WHEN xstyle.TYPE <> "F" format "x(30)"
              "Ink 4:" AT 40 v-ink-4 "Blank Size:" AT 80 
              "W:" + trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)"
              SKIP
              v-qty-or-sup AT 3 FORM "x(36)"
              "Color Desc:" AT 40 xeb.i-coldscr when avail xeb
              "Impressions: " AT 80 trim(string(v-dc-qty))    format "x(7)"
              SKIP
              "D/C Style:" AT 80 SKIP(1)
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
        
        lv-rt-num = i + 3.
/*        PUT SKIP " " SKIP "        ".        
        PUT UNFORMATTED "<#8><R+" lv-rt-num "><C+105><RECT#8><|3>" SKIP.                
        /* box for pack */
        PUT "        ".       
        PUT "<#9><R+6><C+105><RECT#9><|3>" SKIP.       
        /* box for notes */
        PUT "        ".        
        PUT "<#10><R+7><C+105><RECT#10><|3>" SKIP.

        if v-format eq "Brick" then 
             put "<=#8>   Machine Routing        SU:    Start    Stop    Total   RUN:  Hours   Start   Stop   Total     QTY: In     Out      Waste" SKIP.
        ELSE PUT "<=#8>   Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP.
        
        PUT "<=#8><R+1><C+4><from><C+100><Line>"
                    SKIP.
*/
        put skip
            "<P7>   Machine Routing                    SU:      Start       Stop        Total          RUN:     Hours      Start       Stop       Total       QTY:  In        Out         Waste <P10>" SKIP.
        i = 0.
        for each w-m by w-m.dseq:

          i = i + 1.

          v-letter = substr("UTE",i,1).

         /* if v-format eq "Brick" then*/
          display /*v-letter                      at 1
                  chr(124) format "x"           at 2 */
                  
                  /*w-m.dscr AT 3*/
                  w-m.m-code AT 3
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
                 /* chr(124) format "x"           at 131 */                  
              with no-box no-labels frame oo1 width 150 no-attr-space down STREAM-IO.
                  
          v-lines = v-lines + 1.
        end.
      /*
        display v-line[4]                 at 2

            with no-box no-labels frame m5 width 132 no-attr-space STREAM-IO.
      */
        PUT "<#6><C1><FROM><C105><LINE><|3>" .
            
        run cecrep/jobpac2.p (recid(job-hdr),v-format,cust.terms).
        PUT "<=6><C28><FROM><R+8><C28><LINE><|3>"
            "<=6><C44><FROM><R+8><C44><LINE><|3>"
            "<=6><C64><FROM><R+8><C64><LINE><|3>"
            "<=6><C79><FROM><R+8><C79><LINE><|3>"            
            "<=6><C93><FROM><R+8><C93><LINE><|3>"            
            .
        
        
        PUT "<#7><C1><FROM><C105><LINE><|3>"
            "<=7><C75><FROM><R+14><C75><LINE><|3> <P7>"
            SKIP.
        DISPLAY "<=7><C2>Floor Comments" AT 2   "Shipping Info: <P10>" AT 137 SKIP
                "Ship To #:" AT 90
                xoe-ord.sold-id when avail xoe-ord
                xeb.ship-id when avail xeb @ xoe-ord.sold-id
                xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id SKIP
                "____________________________________________________________________________" v-shp[1] AT 90 SKIP
                "____________________________________________________________________________" v-shp[2] AT 90 SKIP
                "____________________________________________________________________________" v-shp[3] AT 90 SKIP
                "____________________________________________________________________________" v-shp[4] AT 90 SKIP
                "____________________________________________________________________________" "Item PO #:" AT 90 xoe-ordl.po-no when avail xoe-ordl
                "____________________________________________________________________________" SKIP
                "____________________________________________________________________________" SKIP
                "____________________________________________________________________________" SKIP
                "____________________________________________________________________________" SKIP
                "____________________________________________________________________________" SKIP
                "____________________________________________________________________________" SKIP
                /*"____________________________________________________________________________" SKIP */
               with no-box no-labels frame m8 width 170 no-attr-space STREAM-IO.

        PAGE.
        /* dept notes */
        v-deptnote = "".
        FOR EACH notes WHERE notes.rec_key = job.rec_key NO-LOCK:
            v-deptnote = v-deptnote + 
                           IF notes.note_text <> "" THEN trim(notes.note_text) + ","
                           ELSE "".
        END.
        lv-add-entry = NUM-ENTRIES(v-deptnote).
        IF lv-add-entry < 6 THEN DO i = 1 TO 7 - lv-add-entry:
               v-deptnote = v-deptnote + ", ".
        END.
        
        v-dept-note = "".
        v-loop-cnt = 1.
        j = 0.
        v-note-cnt = v-loop-cnt.
        v-note-length = 124.
        DO i = 1 TO 6:
           IF v-dept-note[i] <> "" THEN NEXT.           

           IF length(entry(v-loop-cnt,v-deptnote)) > v-note-length THEN DO:
              v-dept-length = trunc(LENGTH(entry(v-loop-cnt,v-deptnote)) / v-note-length,0) .
              v-dept-length = IF (LENGTH(entry(v-loop-cnt,v-deptnote)) MOD v-note-length) > 0 THEN v-dept-length + 1
                        ELSE v-dept-length.
              DO j = 1 TO v-dept-length:
                 v-dept-note[v-note-cnt + j - 1] =
                      SUBSTRING(entry(v-loop-cnt,v-deptnote), (1 + (v-note-length * (j - 1))),v-note-length).
              END.              
              v-note-cnt = j.
              v-loop-cnt = v-loop-cnt + 1.
           END.
           ELSE IF i > v-loop-cnt THEN DO:
                v-dept-note[i] = ENTRY(v-loop-cnt,v-deptnote).
                v-loop-cnt = v-loop-cnt + 1.
                v-note-cnt = v-note-cnt + 1.
           END.
           ELSE ASSIGN v-dept-note[i] = ENTRY(i,v-deptnote)
                       v-loop-cnt = v-loop-cnt + 1
                       v-note-cnt = v-note-cnt + 1.
           
           

             
        END.
        

        v-inst = "".
        /*
        IF NOT AVAIL eb  THEN DO:
           FIND FIRST eb WHERE eb.company = est.company AND
                               eb.est-no = est.est-no NO-LOCK NO-ERROR.
           IF AVAIL eb THEN FIND itemfg WHERE itemfg.company = est.company AND
                                 ITEMfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
           IF AVAIL itemfg THEN DO:
              FOR EACH notes WHERE notes.rec_key = itemfg.rec_key NO-LOCK.
                  v-inst = v-inst + 
                           IF notes.note_text <> "" THEN trim(notes.note_text) + ","
                           ELSE "".
              END.
           END.
        END.
        */
        FOR EACH notes WHERE notes.rec_key = itemfg.rec_key NO-LOCK.
            IF LENGTH(notes.note_text) > v-note-length THEN DO:
               v-dept-length = trunc(LENGTH(notes.note_text) / v-note-length,0) .
               v-dept-length = IF (LENGTH(notes.note_text) MOD v-note-length) > 0 THEN v-dept-length + 1
                        ELSE v-dept-length.
               DO j = 1 TO v-dept-length:
                  v-inst = v-inst + 
                           substring(TRIM(notes.note_text), 1 + (v-note-length * (j - 1)), v-note-length) + ",".
               END.
            END.
            ELSE v-inst = v-inst + 
                           IF notes.note_text <> "" THEN trim(notes.note_text) + ","
                           ELSE "".
        END.
        lv-add-entry = NUM-ENTRIES(v-inst).
        IF lv-add-entry < 6 THEN DO i = 1 TO 7 - lv-add-entry:
               v-inst = v-inst + ", ".
        END.
        PUT "<#11><C1><FROM><C105><R+47><RECT><|3>"  
            "<=11> Department Notes" SKIP
            v-dept-note[1]  SKIP
            v-dept-note[2] SKIP
            v-dept-note[3] SKIP
            v-dept-note[4] SKIP
            v-dept-note[5] SKIP
            v-dept-note[6] SKIP
            "<C1><FROM><C105><LINE><|3>" 
            "<C1> Spec Notes" SKIP
            entry(1,v-inst) FORM "x(122)" AT 3 SKIP
            entry(2,v-inst) FORM "x(122)" AT 3 SKIP
            entry(3,v-inst) FORM "x(122)" AT 3 SKIP
            entry(4,v-inst) FORM "x(122)" AT 3 SKIP
            entry(5,v-inst) FORM "x(122)" AT 3 SKIP
            entry(6,v-inst) FORM "x(122)" AT 3 SKIP
            .
        if print-box and avail xest then do:
            /*PAGE. */
            run cec/desprnpa.p (recid(xef),
                               input-output v-lines,
                               recid(xest)).
            PAGE.
        end.
        ELSE PAGE.
            end.  /* for each w-ef */
    end.  /* each job */
    end.  /* end v-local-loop  */
 
        hide all no-pause.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
