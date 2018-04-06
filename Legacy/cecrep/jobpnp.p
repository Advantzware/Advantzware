/* ---------------------------------------------- */
/*  factory ticket for Midwest Fibre                                                           */
/* -------------------------------------------------------------------------- */
/*  YSK 06/08/01  change local var v-out1-id, v-out2-id to shared var for despr~nt1.p  */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF VAR prt-copies AS INT  NO-UNDO.
DEF VAR v-start-compress AS cha NO-UNDO.
DEF VAR v-end-compress AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR lv-ord-qty LIKE oe-ordl.qty NO-UNDO.
DEF VAR lv-ord-po LIKE oe-ord.po-no NO-UNDO.
DEF VAR v-wgt AS DEC FORM ">>,>>9.99" NO-UNDO.
DEF VAR v-blk-sqft AS DEC NO-UNDO.
DEF VAR v-unit AS INT NO-UNDO.
DEF VAR v-blk-wgt AS DEC NO-UNDO.

{jcrep/r-ticket.i "shared"}

{cecrep/jobpnp.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}
{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
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
DEF VAR lv-m-dscr LIKE w-m.dscr NO-UNDO.
DEF VAR lv-add-entry AS INT NO-UNDO.
DEF VAR v-die-loc AS cha FORM "x(15)" NO-UNDO.
DEF VAR lv-form-code2 LIKE v-form-code NO-UNDO.
DEF VAR lv-form-dscr2 LIKE v-form-dscr NO-UNDO.
DEF VAR lv-sht-size AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-len-score2 LIKE v-len-score NO-UNDO.
DEF VAR v-set-item AS cha NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF VAR s-form-wid AS cha NO-UNDO.
DEF VAR s-form-len AS cha NO-UNDO.
DEF VAR v-got-dot AS LOG NO-UNDO.
DEF VAR v-len-32 AS cha NO-UNDO.
DEF VAR v-tmp-len AS cha NO-UNDO.
DEF VAR v-tmp-dec AS cha NO-UNDO.
DEF VAR vd-tmp-dec AS DEC NO-UNDO.
DEF VAR lv-sht-size2 LIKE lv-sht-size NO-UNDO.
DEF VAR vs-len AS cha NO-UNDO.
DEF VAR vs-wid AS cha NO-UNDO.
DEF VAR vs-dep AS cha NO-UNDO.

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
 v-qty-or-sup = if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat,ASI") gt 0
                then ("Supplier: "     + fill("_",28))
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
        ELSE DO:
          RUN sys/inc/dec-frac.p (xeb.len,32,OUTPUT vs-len).
          RUN sys/inc/dec-frac.p (xeb.wid,32,OUTPUT vs-wid).
          RUN sys/inc/dec-frac.p (xeb.dep,32,OUTPUT vs-dep).
          assign
           v-i-line[1] = "ITEM DESCRIPTION"
           v-i-line[2] = IF avail xstyle then xstyle.dscr else ""
           v-i-line[3] = IF avail xeb    THEN trim(vs-len) + " x " + TRIM(vs-wid) + " x " + trim(vs-dep) ELSE ""
                     /*trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
                     */

           v-i-line[4] = if avail xeb then v-joint-dscr else "".
        END.
          
   
       /*===== for xprint */


        lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                     ELSE itemfg.part-no.
        lv-ord-qty = (if avail xoe-ordl then xoe-ordl.qty
                     else job-hdr.qty) * 
                     (if est.form-qty le 1 then 1 else v-pqty).
        lv-ord-po = IF AVAIL xoe-ordl THEN xoe-ordl.po-no 
                    ELSE IF AVAIL xoe-ord THEN  xoe-ord.po-no
                    ELSE "".

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
   /*====     
        /* 32th conversion */
                    /*decfrac2.p No "-" */
        RUN sys/inc/dec-frac.p (v-form-wid,32,OUTPUT s-form-wid).
        RUN sys/inc/dec-frac.p (v-form-len,32,OUTPUT s-form-len).               
        v-got-dot = NO.
        v-len-32 = "".
        DO i = 1 TO LENGTH(v-len-score):
           v-tmp-len = SUBSTRING(v-len-score,i,1).
           IF v-tmp-len = " " THEN do:
              IF v-tmp-dec <> "" THEN DO:
                 vd-tmp-dec = DEC("0." + v-tmp-dec) . /* make to decimal*/
                 {sys/inc/k16bb.i vd-tmp-dec}         /* convert to dec from 16th */
                 /*v-tmp-dec = SUBSTRING(string(vd-tmp-dec),INDEX(STRING(vd-tmp-dec),".") + 1). */
                 /* RUN sys/inc/decfrac2.p (vd-tmp-dec,32,OUTPUT v-tmp-dec). No "-" */
                 RUN sys/inc/dec-frac.p (vd-tmp-dec,32,OUTPUT v-tmp-dec).
              END.
              ASSIGN   v-len-32 = v-len-32 + 
                                  (IF v-got-dot THEN v-tmp-dec + v-tmp-len
                                  ELSE v-tmp-len)
                        v-got-dot = NO
                        v-tmp-dec = ""
                        .
           END.
           ELSE do:
               IF v-tmp-len = "." THEN ASSIGN v-got-dot = YES
                                              /*v-len-32 = v-len-32 + " " :with decfrac2.p */
                                              .
               ELSE IF v-got-dot THEN DO: /* decimal*/
                     v-tmp-dec = v-tmp-dec + v-tmp-len.
               END.
               ELSE v-len-32 = v-len-32 + v-tmp-len.
           END.
         /*  MESSAGE "loop:" i v-tmp-len "," v-tmp-dec SKIP
                   v-len-32 length(v-len-32) VIEW-AS ALERT-BOX.  */
        END.
        IF v-tmp-dec <> "" THEN do:
           vd-tmp-dec = DEC("0." + v-tmp-dec) .  /* make to decimal*/
           {sys/inc/k16bb.i vd-tmp-dec}         /* convert to dec from 16th */
           /*v-tmp-dec = SUBSTRING(string(vd-tmp-dec),INDEX(STRING(vd-tmp-dec),".") + 1). */
           /* RUN sys/inc/decfrac2.p (vd-tmp-dec,32,OUTPUT v-tmp-dec). No "-" */
           RUN sys/inc/dec-frac.p (vd-tmp-dec,32,OUTPUT v-tmp-dec).
           v-len-32 = v-len-32 + v-tmp-dec.
         END.

        v-len-score = v-len-32.
 ======  end of 32th conversion  =====================*/
          find first ITEM where item.company eq cocode
                            and item.i-no    eq xef.board
                            no-lock no-error.
          v-form-sqft = round(if v-corr then (v-form-len * v-form-wid * .007)
                                        else (v-form-len * v-form-wid / 144),3).

        ASSIGN lv-form-code2 = v-form-code
               lv-form-dscr2 = v-form-dscr
               lv-len-score2 = v-len-score
               lv-sht-size = "W: " + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
                             "   " +
                             "L: " + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))
               
               /*lv-sht-size = "W:" + s-form-wid + "   " + "L:" + s-form-len  */             
               lv-sht-size2 = lv-sht-size 
               v-wgt = IF AVAIL ITEM THEN (v-form-sqft * ITEM.basis-w / 1000) ELSE 0
               v-blk-sqft = /*round(if v-corr then (xeb.t-len * xeb.t-wid * .007)
                                            else (xeb.t-len * xeb.t-wid / 144),3)
                            */
                           IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0
               v-blk-wgt = /*IF AVAIL ITEM THEN (v-blk-sqft * ITEM.basis-w / 1000) ELSE 0 */
                           IF AVAIL itemfg THEN itemfg.weight-100 / 100 ELSE 0
               v-unit = xeb.tr-cas * xeb.stacks
               .
                       
        IF AVAIL xeb AND xeb.est-type = 6 THEN DO:
           FIND FIRST bf-eb WHERE bf-eb.company = xeb.company
                              AND bf-eb.est-no = xeb.est-no
                              AND bf-eb.form-no = 0 NO-LOCK NO-ERROR.
           v-set-item = IF AVAIL bf-eb THEN bf-eb.stock-no ELSE "".  /*bf-eb.part-dscr1 */
        END.

        DISP "<=#1><B>CUSTOMER" AT 2 "SHIP TO:" AT 35
              xoe-ord.sold-id when avail xoe-ord
              xeb.ship-id when avail xeb @ xoe-ord.sold-id
              xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id               
              "ORDER QTY" AT 70 "CUST P.O." AT 82 "JOB#" AT 97 SKIP
              v-cus[1] AT 2 v-shp[1] FORM "x(27)"             
              trim(string( (if avail xoe-ordl then xoe-ordl.qty
                                           else job-hdr.qty) *
                                          if est.form-qty le 1 then 1
                                          else v-pqty,">>>,>>9"))
                                          format "x(7)" AT 61             
              lv-ord-po AT 72
              v-job-prt AT 87
              SKIP
              v-cus[2] AT 2  v-shp[2] FORM "x(27)"
             "ORDER DATE" AT 61 "DUE DATE" AT 73 "TOTAL SQ FT" AT 83 SKIP
              v-cus[3] AT 2 v-shp[3] FORM "x(28)" v-ord-date  AT 61 v-due-date  AT 73    
              trim(string(v-sht-qty * v-form-sqft,">>>,>>9.9<")) format "x(10)" AT 85
              SKIP
              v-cus[4] AT 2  v-shp[4] SKIP
              "FG#:" AT 2  xeb.stock-no "CUST. PART#:" xeb.part-no "SHEET SIZE:</PROGRESS>" AT 51
               /*"W: " + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
               "   " +
               "L: " + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))
                                                                format "x(25)" */
               lv-sht-size2 FORM "x(25)"
               SKIP
               xeb.part-dscr1 WHEN AVAIL xeb /*not part# lv-part-no */ AT 5 
               "MATERIAL:" AT 51 /* trim(v-form-code) + "," +*/  trim(v-form-dscr) FORM "X(30)" SKIP
               xeb.part-dscr2 WHEN AVAIL xeb AT 5
               /*"SET ITEM:" AT 2  v-set-item  */
               "ADDER:" AT 51 v-adders FORM "x(33)"
               SKIP
               "SCORING:" AT 51  v-len-score  WHEN xstyle.TYPE <> "F"  format "x(32)"
               SKIP
               "SHIPMENT"  "STOCK FROM" AT 49 "QTY ORDERED" AT 68  "BALES RECEIVED" AT 80  SKIP
               "DATE         QTY              BALANCE"
               v-qty-or-sup AT 49  FORM "x(24)" trim(string(v-sht-qty)) format "x(9)" /*AT 99*/                              
               SKIP
               "FINISH BALE COUNT      TOTAL" AT 49 SKIP(3)
                WITH FRAME job1 NO-BOX NO-LABELS NO-ATTR-SPACE WIDTH 145 STREAM-IO.

        i = 0.
        for each w-m:
          i = i + 1.
        end.
        
        if i lt 3 then do i = i + 1 to 3:
          create w-m.
          w-m.dseq = 999999999.
        end.
            /* box for route */
        
        lv-rt-num = i + 3.  /* if i = 4, lv-rt-num = 7*/

        PUT "<#8><C1><FROM><C80><LINE><|3>" SKIP.        
        /*PUT UNFORMATTED "<#8><R+" lv-rt-num ">" SKIP.                */
       
        if v-format eq "Brick" OR v-format = "ASI" then 
             put "<=#8><P7> Machine Routing          SU:    Start    Stop    Total   RUN:  Hours   Start   Stop   Total     QTY: In     Out      Waste" SKIP.
        ELSE PUT "<=#8><P7> Machine Routing          SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP.
        PUT "<=8>".

        i = 0.
        for each w-m by w-m.dseq:

          i = i + 1.
          FIND first mach where mach.company eq cocode
                            and mach.m-dscr  eq w-m.dscr NO-LOCK NO-ERROR.
          lv-m-dscr = IF AVAIL mach THEN mach.m-code ELSE w-m.dscr.
          v-letter = substr("UTE",i,1).

          if v-format eq "Brick" OR v-format = "ASI" then
          display /*v-letter                      at 1
                  chr(124) format "x"           at 2 */                  
                  lv-m-dscr /*w-m.dscr*/  AT 3
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
                 /* chr(124) format "x"           at 131 */                  
              with no-box no-labels frame o1 width 132 no-attr-space down STREAM-IO.
                  
          else
          display /*v-letter                      at 1
                  chr(124) format "x"           at 2 */
                  w-m.dscr AT 3
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
                  /*chr(124) format "x"           at 131   */                  
              with no-box no-labels frame o2 width 132 no-attr-space down STREAM-IO.
                  
          v-lines = v-lines + 1.
        end.
        /*IF lv-rt-num < 8 THEN PUT SKIP(8 - lv-rt-num). */
/*================================*/
        PUT "<P10><#9><C1><FROM><C80><LINE><|3> <=9><R-1>" SKIP.
        DISPLAY 
               "DESCRIPTION" SKIP
               "STYLE:" AT 2 v-i-line[2] "MFG JOINT:" AT 49 v-i-line[4] SKIP
               "ID   :" AT 2 v-i-line[3] "PRINTING:"  AT 49 xeb.i-coldscr when avail xeb
                SKIP
               "SCORING:" AT 2 lv-len-score2 "DIE#:" AT 49         xeb.die-no when avail xeb
                SKIP
                "MATERIAL:" AT 2 /*trim(lv-form-code2) + "," +*/  TRIM(lv-form-dscr2) FORM "x(60)" 
                 /*"<B>Up " AT 60 "W:" v-upl FORM ">9" "  L:"  v-upw FORM ">9"*/
                SKIP
                "SHEET SIZE:" AT 2 lv-sht-size
                "NO.ON:" AT 49 "W:" v-upl FORM ">9" "  L:"  v-upw FORM ">9"
                SKIP
                "SQ FEET: " AT 2  trim(string(v-form-sqft)) format "x(7)"
                "WEIGHT:" AT 35 trim(string(v-wgt,">>,>>9.9<"))
                "BUNDLE:" AT 55 trim(string(xeb.cas-cnt)) when avail xeb
                "BALE:" AT 75 trim(string(xeb.tr-cnt)) when avail xeb  
                SKIP
                "ITEM SQ FT:" AT 2 trim(string(v-blk-sqft)) 
                "ITEM WEIGHT:" AT 35 trim(string(v-blk-wgt,">>,>>9.9<"))
                "UNIT:" AT 75 trim(string(v-unit))
               /*=====================*/
                     WITH FRAME job2 NO-BOX NO-LABELS NO-ATTR-SPACE WIDTH 145 STREAM-IO.

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

        /* PUT SKIP(1). */
        PUT "<#10><C1><FROM><C80><LINE><|3>  <=10><R-1>" SKIP.
        display "SPECIAL INSTRUCTIONS:" AT 1
                v-dept-inst[1] format "x(128)" AT 2
            v-dept-inst[2] format "x(128)" AT 2
            v-dept-inst[3] format "x(128)" AT 2
            v-dept-inst[4] format "x(128)" AT 2
            v-dept-inst[5] format "x(128)" AT 2
            v-dept-inst[6] format "x(128)" AT 2
                "</B>"
                SKIP
            with no-box no-labels frame m8 width 145 no-attr-space STREAM-IO.

        if print-box and avail xest then do:            
           IF LINE-COUNTER < 50 THEN PUT SKIP(50 - LINE-COUNTER).

            /*PUT SKIP(1). */
           /*
            run cec/desprntm.p (recid(xef),
                               input-output v-lines,
                               recid(xest)).     for 32th display */
            run cec/desprnt3.p (recid(xef),
                               input-output v-lines,
                               recid(xest)). 
        end.
        ELSE PAGE.
       
      end.  /* for each w-ef */
    end.  /* each job */
    end.  /* end v-local-loop  */
 
hide all no-pause.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
