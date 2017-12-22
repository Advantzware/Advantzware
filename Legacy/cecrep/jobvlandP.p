/* ---------------------------------------------- cec/rep/jobtick.p 04/97 JLF */
/*  factory ticket                                                            */
/* -------------------------------------------------------------------------- */
/*  YSK 06/08/01  change local var v-out1-id, v-out2-id to shared var for despr~nt1.p  */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR lv-ord-qty LIKE oe-ordl.qty NO-UNDO.
DEF VAR tb_app-unprinted AS LOG NO-UNDO.

{jcrep/r-ticket.i "shared"}

{cecrep/jobtick.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}
{custom/notesdef.i}

DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
def new shared var v-out1-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def new shared var v-out2-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
 
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
DEF VAR lv-m-dscr LIKE w-m.dscr NO-UNDO.
DEF VAR lv-add-entry AS INT NO-UNDO.
DEF VAR v-die-loc AS cha FORM "x(15)" NO-UNDO.
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
DEF BUFFER b-eb FOR eb.
DEF VAR v-job-cust AS LOG NO-UNDO.
DEF VAR lv-under-run AS DECIMAL NO-UNDO.
DEF VAR lv-over-run AS DECIMAL NO-UNDO.
DEF VAR ls-fgitem-img AS cha FORM "x(50)" NO-UNDO.
DEF VAR v-qa-text AS cha FORM "x(30)" INIT "6/05 Job Ticket QF-119 Rev.A" NO-UNDO.

DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.

   FIND FIRST sys-ctrl
       WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "JOBQTYCUST"
       NO-LOCK NO-ERROR.

   IF NOT AVAIL sys-ctrl THEN
       DO TRANSACTION:
       create sys-ctrl.
       assign
           sys-ctrl.company  = cocode
           sys-ctrl.NAME     = "JOBQTYCUST"
           sys-ctrl.module   = "JC"
           sys-ctrl.descrip = "Create Job Quantity with overrun % from customer if no order?"
           sys-ctrl.log-fld = NO .
       end.

  v-job-cust = sys-ctrl.log-fld.

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
 v-qty-or-sup = if CAN-DO("TriState,RFC,Boxtech,Brick,Corrugat,ASI",v-format)
                then ("Supplier: "     + fill("_",28))
                else ("Qty Received: " + fill("_",24))
 v-local-copies = 1.

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
        
        run cecrep/jobtick2csc.p (recid(w-ef), recid(job-hdr), ROWID(b-eb)).

        v-pqty = 1.
        if avail xeb then do:
          if xeb.stock-no ne "" then v-fg = xeb.stock-no.
          if xest.est-type eq 6 then v-fg = trim(v-fg) + "  CP#: " +
                                            xeb.part-no.
          
          {cec/rollfac.i}
          v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                   if xeb.yld-qty lt 0 then (-1 / xeb.yld-qty)
                                       else xeb.yld-qty.
        end.
        
        assign
         v-loc     = ""
         v-loc-bin = "".
         
        if v-format eq "Brick" or v-format eq "Corrugat" OR  v-format eq "ASI"
        then do: 
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

        view frame head.  /* factory header display  */
        
        if v-format eq "RFC" or v-format eq "Boxtech" then
          assign
           v-i-line[1] = itemfg.i-name
           v-i-line[2] = itemfg.part-dscr1
           v-i-line[3] = itemfg.part-dscr2
           v-i-line[4] = itemfg.part-dscr3.
        else
          assign
           v-i-line[1] = "ITEM DESCRIPTION"
           v-i-line[2] = "Style: " + if avail xstyle then xstyle.dscr else ""
           v-i-line[3] = "Size: "  + if avail xeb    then
                     trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
           v-i-line[4] = "Joint: " + if avail xeb then v-joint-dscr else "".
   
       /*===== for xprint */

        PUT "C".
        /* 3 Boxes for customer */
        PUT "<#2><R+6><C+26><RECT#2><|3>"
            "<#3><R-6><C+26><RECT#3><|3>"
            "<#4><R+6><C+26><RECT#4><|3>" SKIP.
        /* 3 boxes for board */
        PUT "B".
        /* 3 Boxes for customer */
        PUT "<#5><R+6><C+26><RECT#5><|3>"
            "<#6><R-6><C+26><RECT#6><|3>"
            "<#7><R+6><C+26><RECT#7><|3>" SKIP.

        PUT "<=#2>" SKIP
            "U" SKIP
            "S" SKIP
            "T" SKIP.
        PUT "<=#5>" SKIP
            "O" SKIP 
            "A" SKIP
            "R" SKIP
            "D" SKIP.
            
        lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                     ELSE itemfg.part-no.
        lv-ord-qty = (if avail xoe-ordl then xoe-ordl.qty
                     else job-hdr.qty) * 
                     (if est.form-qty le 1 then 1 else v-pqty).
         ASSIGN
             lv-over-run  = IF AVAIL xoe-ordl THEN xoe-ordl.over-pct ELSE 
                            IF AVAIL xoe-ord  THEN xoe-ordl.over-pct ELSE 0
             lv-under-run = IF AVAIL xoe-ordl THEN xoe-ordl.under-pct ELSE
                             IF AVAIL xoe-ord  THEN xoe-ordl.under-pct  ELSE 0  .
          IF v-job-cust AND NOT AVAIL xoe-ord AND AVAIL cust THEN
             ASSIGN
             lv-over-run  = cust.over-pct
             lv-under-run = cust.under-pct .


        DISP "<=#2> CUSTOMER INFORMATION" SKIP
              v-cus[1] AT 3 SKIP
              v-cus[2] AT 3 SKIP
              v-cus[3] AT 3 SKIP
              v-cus[4] AT 3 SKIP
            "<=#3><R-6><b> ORDER INFORMATION"
            "<=#3><R-5> PO #:" 
            xoe-ord.po-no WHEN AVAIL xoe-ord
            "Set Qty:"
            trim(string(if avail xoe-ordl then xoe-ordl.qty
                                          else job-hdr.qty,">>>,>>9"))
                        when avail xeb and xeb.est-type eq 6    format "x(9)"
            "<=#3><R-4> Job Qty:"
             trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
            "Order Qty:"
            trim(string( (if avail xoe-ordl then xoe-ordl.qty
                                             else job-hdr.qty) *
                                            if est.form-qty le 1 then 1
                                            else v-pqty,">>>,>>9"))
                                            format "x(7)"
            "<=#3><R-3> Cust Part #:" lv-part-no 
            "<=#3><R-2> Overrun:" 
             trim(string(lv-over-run,">>9.99%")) format "x(7)"                                                         
            "Underrun:"
            trim(string(lv-under-run,">>9.99%")) format "x(7)"
            "<=#3><R-1>        " lv-ord-qty * ( 1 + round((lv-over-run) / 100,2)) FORM ">>>,>>9"
                       "         "     lv-ord-qty * ( 1 - round((lv-under-run) / 100,2)) FORM ">>>,>>9" "</b>"
            WITH NO-BOX NO-LABELS NO-ATTR-SPACE WIDTH 145 STREAM-IO.

     PUT UNFORMATTED
            "<=#4> " v-i-line[1] FORM "x(40)"
            "<=#4><R+1> " v-i-line[2] FORM "x(40)"
            "<=#4><R+2> " v-i-line[3] FORM "x(40)"
            "<=#4><R+3> " v-i-line[4] FORM "x(40)"
            "<=#4><R+4> Adders:" v-adders FORM "x(33)" .
                    
        v-form-sqft = round(if v-corr then (v-form-len * v-form-wid * .007)
                                      else (v-form-len * v-form-wid / 144),3).
        
        find first xxprep where
             xxprep.company eq cocode AND
             xxprep.code eq xeb.plate-no
             no-lock no-error.

        display "<=#5> Shts Req'd:"
                trim(string(v-sht-qty))   format "x(9)"
                " Sq Ft:"
                trim(string(v-form-sqft)) format "x(7)"
                "<=#6><R-6> PLATE #:"
                xeb.plate-no FORMAT "X(15)" when avail xeb
                "Loc: "
                xxprep.loc-bin FORMAT "X(8)" WHEN AVAIL xxprep
                "<=#7> DIE CUTTING, SLIT, & SAW"                
            with no-box no-labels frame m2 width 145 NO-ATTR-SPACE STREAM-IO.

        i = 0.
        for each w-i:
          i = i + 1.
        end.
        if i lt 4 then do i = i + 1 to 4:
          create w-i.
        end.

        find first w-i.
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.die-no
                            no-lock no-error.
        v-die-loc = IF AVAIL xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

        IF v-format EQ "TriLakes" THEN DO:
        display "<=#5><R+1>"
                "W: " + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
                "   " +
                "L: " + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))
                                                                format "x(25)"
                "MSF:"  +
                trim(string(v-sht-qty * v-form-sqft / 1000,">>>9.9<"))
                                                                format "x(11)"
                "<=#6><R-5> 1:"
                w-i.i-code
                w-i.i-qty when w-i.i-qty ne 0
                "5:"
                w-i.i-code2
                w-i.i-qty2 when w-i.i-qty2 ne 0
                "<=#7><R+1> Die #:"
                xeb.die-no when avail xeb
                " Loc:"   /*v-die-loc */ 
                xxprep.loc-bin when avail xeb and avail xxprep      
            with no-box no-labels frame i1 width 150 no-attr-space STREAM-IO.
        find next w-i.        
        display "<=#5><R+2> Board:"
                v-form-code 
                "<=#6><R-4> 2:"
                w-i.i-code
                w-i.i-qty when w-i.i-qty ne 0
                "6:"
                w-i.i-code2
                w-i.i-qty2 when w-i.i-qty2 ne 0
                "<=#7><R+2> Blank Size:"
                "W:" TRIM(string({sys/inc/k16v.i xeb.t-wid},">,>>9.99")) WHEN AVAIL xeb 
                "L:" TRIM(string({sys/inc/k16v.i xeb.t-len},">,>>9.99")) when avail xeb 
            with no-box no-labels frame i2 width 155 no-attr-space STREAM-IO.

        find next w-i.
        display "<=#5><R+3><C+5> " v-form-dscr
                "<=#6><R-3> 3:"
                w-i.i-code
                w-i.i-qty when w-i.i-qty ne 0
                "7:"
                w-i.i-code2
                w-i.i-qty2 when w-i.i-qty2 ne 0
                "<=#7><R+3> Up:"
                "W:" v-upl FORM ">9" 
                "  L:"  v-upw FORM ">9"
                "Slit:  W:"  v-outw FORM ">9"
                "L:"  v-outl FORM ">9"                                          
            with no-box no-labels frame i3 width 155 no-attr-space STREAM-IO.
        
        find next w-i.
        display "<=#5><R+4> Score:"
                 v-len-score     WHEN xstyle.TYPE <> "F"  format "x(32)"
                "<=#6><R-2> 4:"
                w-i.i-code
                w-i.i-qty when w-i.i-qty ne 0
                "8:"
                w-i.i-code2
                w-i.i-qty2 when w-i.i-qty2 ne 0
                "<=#7><R+4> Impressions:"
                trim(string(v-dc-qty))    format "x(7)"
                " To: " +
                trim(string({sys/inc/k16v.i xef.nsh-wid},">>9.99")) +
                "x" +
                trim(string({sys/inc/k16v.i xef.nsh-len},">>9.99"))
                          when avail xef and 
                               (xef.nsh-wid ne xef.gsh-wid or
                                xef.nsh-len ne xef.gsh-len)
                                          format "x(17)"
            with no-box no-labels frame i4 width 155 no-attr-space STREAM-IO.
        END.

        ELSE DO:
        display "<=#5><R+1>"
                "W: " + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
                "   " +
                "L: " + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))
                                                                format "x(25)"
                "MSF:"  +
                trim(string(v-sht-qty * v-form-sqft / 1000,">>>9.9<"))
                                                                format "x(11)"
                "<=#6><R-5> Ink 1:"
                w-i.i-dscr
                w-i.i-qty when w-i.i-qty ne 0
                "LBS" when w-i.i-dscr ne ""
                "<=#7><R+1> Die #:"
                xeb.die-no when avail xeb
                " Loc:"   /*v-die-loc */ 
                xxprep.loc-bin when avail xeb and avail xxprep      
            with no-box no-labels frame i5 width 150 no-attr-space STREAM-IO.
        find next w-i.        
        display "<=#5><R+2> Board:"
                v-form-code 
                "<=#6><R-4> Ink 2:"
                w-i.i-dscr
                w-i.i-qty when w-i.i-qty ne 0
                "LBS" when w-i.i-dscr ne ""
                "<=#7><R+2> Blank Size:"
                "W:" TRIM(string({sys/inc/k16v.i xeb.t-wid},">,>>9.99")) WHEN AVAIL xeb 
                "L:" TRIM(string({sys/inc/k16v.i xeb.t-len},">,>>9.99")) when avail xeb 
            with no-box no-labels frame i6 width 155 no-attr-space STREAM-IO.

        find next w-i.
        display "<=#5><R+3><C+5> " v-form-dscr
                "<=#6><R-3> Ink 3:"
                w-i.i-dscr
                w-i.i-qty when w-i.i-qty ne 0
                "LBS" when w-i.i-dscr ne ""
                "<=#7><R+3> Up:"
                "W:" v-upl FORM ">9" 
                "  L:"  v-upw FORM ">9"
                "Slit:  W:"  v-outw FORM ">9"
                "L:"  v-outl FORM ">9"                                          
            with no-box no-labels frame i7 width 155 no-attr-space STREAM-IO.
        
        find next w-i.
        display "<=#5><R+4> Score:"
                 v-len-score     WHEN xstyle.TYPE <> "F"  format "x(32)"
                "<=#6><R-2> Ink 4:"
                w-i.i-dscr
                w-i.i-qty when w-i.i-qty ne 0
                "LBS" when w-i.i-dscr ne ""
                "<=#7><R+4> Impressions:"
                trim(string(v-dc-qty))    format "x(7)"
                " To: " +
                trim(string({sys/inc/k16v.i xef.nsh-wid},">>9.99")) +
                "x" +
                trim(string({sys/inc/k16v.i xef.nsh-len},">>9.99"))
                          when avail xef and 
                               (xef.nsh-wid ne xef.gsh-wid or
                                xef.nsh-len ne xef.gsh-len)
                                          format "x(17)"
            with no-box no-labels frame i8 width 155 no-attr-space STREAM-IO.
        END.
            
        IF CAN-DO("TriState,RFC,Boxtech,Brick,Corrugat,ASI,Xprint,Pacific,TriLakes,Vineland",v-format) THEN do:
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
          
          IF v-vend-no NE "" THEN DO:
            v-qty-or-sup = v-qty-or-sup + TRIM(v-vend-no).
            
            IF v-po-no NE 0 THEN
              v-qty-or-sup = v-qty-or-sup + "  PO#: " +
                             TRIM(STRING(v-po-no,">>>>>>>>>>")).
          END.
          
          /*v-qty-or-sup = v-qty-or-sup + fill("_",38 - length(v-qty-or-sup)). */
        END.
          
        ELSE v-qty-or-sup = "Qty Received: " + fill("_",24).
          
      /* for xprint */
        display "<=#5><R+5>" v-qty-or-sup
                     "<=#6><R-1> Color Desc:"
                     "<=#6><R-1><C+12> " xeb.i-coldscr when avail xeb
                     "<=#7><R+5> D/C Style:"                             
                         with no-box no-labels frame m3 width 132 no-attr-space STREAM-IO.
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
        PUT SKIP " " SKIP " ".        
        PUT UNFORMATTED "<#8><R+" lv-rt-num "><C+78><RECT#8><|3>" SKIP.                
        /* box for pack */
        PUT " ".        
        PUT "<#9><R+6><C+78><RECT#9><|3>" SKIP.       
        /* box for notes */
        PUT " ".        
        PUT "<#10><R+7><C+78><RECT#10><|3>" SKIP.

        if v-format eq "Brick" OR v-format = "ASI" then 
             put "<=#8> Machine Routing        SU:    Start    Stop    Total   RUN:  Hours   Start   Stop   Total     QTY: In     Out      Waste" SKIP.
        ELSE PUT "<=#8> Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP.
        
        PUT "<=#8><R+1><C+1><from><C+76><Line>"
                    SKIP.

        i = 0.
        for each w-m by w-m.dseq:

          i = i + 1.
          FIND first mach where mach.company eq cocode
                            and mach.m-dscr  eq w-m.dscr NO-LOCK NO-ERROR.
          lv-m-dscr = IF AVAIL mach THEN mach.m-code ELSE w-m.dscr.
          v-letter = substr("UTE",i,1).

          if v-format eq "Brick" OR v-format = "ASI" then
          display lv-m-dscr AT 3
                  w-m.s-hr                              when w-m.s-hr ne 0
                  fill("_",7)  format "x(7)"    to 38   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 46   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 54   when w-m.dscr ne ""
                  space(4)
                  w-m.r-sp                              when w-m.r-sp ne 0
                  w-m.r-hr                              when w-m.r-hr ne 0
                  fill("_",7)  format "x(7)"    to 81   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 89   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 97   when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"        to 111  when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 120  when w-m.dscr ne ""
              fill("_",8)  format "x(8)"    to 129  when w-m.dscr ne ""                
              with no-box no-labels frame o1 width 132 no-attr-space down STREAM-IO.
                  
          else
          display w-m.dscr AT 3
                  w-m.s-hr when w-m.s-hr ne 0
                  fill("_",7)  format "x(7)"    to 38   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 46   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 54   when w-m.dscr ne ""
                  space(2)
                  w-m.r-sp when w-m.r-sp ne 0
                  fill("_",7)  format "x(7)"    to 69   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 77   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 85   when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 99   when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 108  when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 117  when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 129  when w-m.dscr ne ""                 
              with no-box no-labels frame o2 width 132 no-attr-space down STREAM-IO.
                  
          v-lines = v-lines + 1.
        end.

        FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                           AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        
        IF AVAIL b-ef AND b-ef.form-no = w-ef.frm THEN 
           FOR EACH w-m:
               CREATE tt-wm.
               BUFFER-COPY w-m TO tt-wm.
        END.
        PUT "<=#8>"
            SKIP
            "R" SKIP
            "O" SKIP
            "U" SKIP
            "T" SKIP
            "E" SKIP.

        PUT SKIP(1).
        run cecrep/jobtick3.p (recid(job-hdr),v-format,cust.terms).
        RUN stackImage.
        ASSIGN v-tmp-lines = 0
               j = 0
               K = 0
               lv-got-return = 0
               v-dept-inst = "".

        {custom/notespr2.i job v-inst2 6 "notes.rec_key = job.rec_key and
                            (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)" }
        DO i = 1 TO 6:
             v-dept-inst[i] = v-inst2[i].
        END.

        IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */

        PUT "<=#10>" SKIP.

        display "<=#10><R-1>" /* -1*/
                "N"                      AT 1
                "SPECIAL INSTRUCTIONS.." AT 3
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                "SHIPPING INFO"
                "      Ship To #:"
                xoe-ord.sold-id when avail xoe-ord
                xeb.ship-id when avail xeb @ xoe-ord.sold-id
                xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id SKIP
                "O"                       at 1
                v-dept-inst[1] format "x(82)" AT 3
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                v-shp[1]
                "T"                       at 1
                v-dept-inst[2] format "x(82)" AT 3
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                v-shp[2]
                "E"                       at 1
                v-dept-inst[3] format "x(82)" AT 3
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                v-shp[3]
                "S"                       at 1
                v-dept-inst[4] format "x(82)" AT 3
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                v-shp[4]
                v-dept-inst[5] format "x(82)" AT 3
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                "Item PO #:"
                xoe-ordl.po-no when avail xoe-ordl
                v-dept-inst[6] format "x(128)" AT 3
                skip(1)
            with no-box no-labels frame m8 width 132 no-attr-space STREAM-IO.
   
        if print-box and avail xest then do:            
            run cec/desprnt3csc.p (recid(xef),
                                   input-output v-lines,
                                   recid(xest),
                                   IF AVAIL xeb THEN ROWID(xeb) ELSE ?).
        end.
        ELSE PAGE.
       
       IF s-prt-fgimage THEN
        DO:
           IF xest.est-type EQ 6 THEN
           DO:               
              IF itemfg.box-image NE "" THEN DO:        
                
                 ls-fgitem-img = itemfg.box-image .
              
                 PUT UNFORMATTED "<#12><C1><FROM><C80><R+47><RECT><||3><C50>" v-qa-text SKIP
                     "<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
                     "<=12><R+3><C1><FROM><C80><LINE><||3>"
                     "<=12><R+5><C3><#21><R+45><C+75><IMAGE#21=" ls-fgitem-img ">" SKIP. 
                 PAGE.
              END. 
             
              IF LAST(w-ef.frm) AND itemfg.box-image NE "" THEN DO:        
                
                 ls-fgitem-img = itemfg.box-image .
              
                 PUT UNFORMATTED "<#12><C1><FROM><C80><R+47><RECT><||3><C50>" v-qa-text SKIP
                     "<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
                     "<=12><R+3><C1><FROM><C80><LINE><||3>"
                     "<=12><R+5><C3><#21><R+45><C+75><IMAGE#21=" ls-fgitem-img ">" SKIP. 
                 PAGE.
              END.
           END.
           ELSE
           DO:                
              ls-fgitem-img = itemfg.box-image .              
           
              PUT UNFORMATTED "<#12><C1><FROM><C80><R+47><RECT><||3><C50>" v-qa-text SKIP
                  "<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
                  "<=12><R+3><C1><FROM><C80><LINE><||3>"
                  "<=12><R+5><C3><#21><R+45><C+75><IMAGE#21=" ls-fgitem-img ">" SKIP. 
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

                    IF v-job-cust AND NOT AVAIL xoe-ord AND AVAIL cust THEN
               ASSIGN
                    v-over-run  = trim(string(cust.over-pct,">>9.99%"))
                    v-under-run = trim(string(cust.under-pct,">>9.99%")) .

           PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
               "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
               "<C60>Our Date: " v-ord-date SKIP
               "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
               "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
               "<=1><R+6><C2>CUSTOMER INFORMATION <C25><b> ORDER INFORMATION </b><C53>ITEM DESCRIPTION" SKIP
               v-cus[1] AT 3 "<b> PO#: " v-po-no " Set Qty: "  v-set-qty "</b>"
               v-i-line[2] AT 90
               SKIP
               v-cus[2] AT 3 "<b> Job Qty:" trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
               " Order Qty:" string(v-ord-qty) format "x(7)" "</b>"
               v-i-line[3] AT 90 SKIP
               v-cus[3] AT 3  "<b> Cust Part #:" lv-part-no "</b>"
               v-i-line[4] AT 90 SKIP
               v-cus[4]  AT 3 "<b> Overrun:" v-over-run format "x(7)"  
               " Underrun:" v-under-run format "x(7)" SPACE(8) "</b>" 
               "Adders:" v-adders FORM "x(33)" SKIP
               "<=1><R+11><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
               "<C2>FINISHED GOOD #                 DESCRIPTION                       RATIO PER SET" SKIP.
           /* each components */
           DEF VAR v-tmp-line AS INT NO-UNDO.
           DEF VAR v-shipto AS cha NO-UNDO.

           v-tmp-line = 0.
           FOR EACH xeb WHERE xeb.company = est.company
                           AND xeb.est-no = est.est-no
                           AND xeb.form-no > 0 NO-LOCK:
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
           ASSIGN
              v-tmp-line = v-tmp-line + 12
              i = 0.

           for each tt-wm WHERE lookup(tt-wm.m-code,tspostfg-char) > 0:
                i = i + 1.
           END.
           i = i + 2.
           PUT "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
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
           v-tmp-line = v-tmp-line + 3 + i
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
  IF AVAILABLE pattern AND SEARCH(stackPattern.stackImage) NE ? THEN
  PUT UNFORMATTED SKIP(1) "<C60>"
    "<#71><C1><R+8><FROM><C17><R-13>"
    "<IMAGE#71=" stackPattern.stackImage ">"
    "<R-13>".
END PROCEDURE.
/* end ---------------------------------- copr. 1997  advanced software, inc. */
