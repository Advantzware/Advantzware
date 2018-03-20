/* ---------------------------------------------- cec/rep/jobtick.p 04/97 JLF */
/*  factory ticket                                                            */
/* -------------------------------------------------------------------------- */
/*  YSK 06/08/01  change local var v-out1-id, v-out2-id to shared var for despr~nt1.p  */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

DEFINE INPUT PARAMETER v-format AS CHARACTER.

DEFINE VARIABLE k_frac AS DECIMAL INIT 6.25 NO-UNDO.
DEFINE VARIABLE lv-ord-qty LIKE oe-ordl.qty NO-UNDO.
DEFINE VARIABLE tb_app-unprinted AS LOG NO-UNDO.

{jcrep/r-ticket.i "shared"}

{cecrep/jobtick.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}
{custom/notesdef.i}

DEFINE VARIABLE v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEFINE VARIABLE v-dept-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEFINE VARIABLE v-note-length AS INTEGER INIT 80 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-out1-id       AS   RECID    NO-UNDO.  /* YSK 06/08/01  was~ local var */
DEFINE NEW SHARED VARIABLE v-out2-id       AS   RECID    NO-UNDO.  /* YSK 06/08/01  was~ local var */
 
DEFINE VARIABLE v-vend-no       LIKE oe-ordl.vend-no                            NO-UNDO.
DEFINE VARIABLE v-po-no         LIKE oe-ordl.po-no-po                           NO-UNDO.
DEFINE VARIABLE v-qty-or-sup    AS   CHARACTER               FORMAT "x(38)"          NO-UNDO.
DEFINE VARIABLE v-i-line        AS   CHARACTER   EXTENT 4    FORMAT "x(38)"          NO-UNDO.
DEFINE VARIABLE v-flag          AS   LOG    INIT NO                             NO-UNDO.
DEFINE VARIABLE v-local-copies  AS   INTEGER                                        NO-UNDO.
DEFINE VARIABLE v-local-loop    AS   INTEGER    INIT 1                              NO-UNDO.
DEFINE VARIABLE v-print-score   AS   LOG    INIT YES                            NO-UNDO.
DEFINE VARIABLE v-pqty          AS   DECIMAL                                        NO-UNDO.
DEFINE VARIABLE lv-part-no AS cha FORM "x(15)" NO-UNDO.
DEFINE VARIABLE lv-rt-num AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-m-dscr LIKE w-m.dscr NO-UNDO.
DEFINE VARIABLE lv-add-entry AS INTEGER NO-UNDO.
DEFINE VARIABLE v-die-loc AS cha FORM "x(15)" NO-UNDO.
{cecrep/jc-prem.i}
DEFINE BUFFER b-ef FOR ef.
DEFINE WORKFILE tt-wm LIKE w-m.
DEFINE VARIABLE v-xg-flag AS LOG NO-UNDO.
DEFINE VARIABLE v-tmp-stype AS cha NO-UNDO.
DEFINE VARIABLE v-len-score2 AS cha EXTENT 13 NO-UNDO.
DEFINE VARIABLE v-tmp-score AS cha NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.
DEFINE VARIABLE lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEFINE SHARED VARIABLE s-prt-set-header AS LOG NO-UNDO.
DEFINE BUFFER b-eb FOR eb.
DEFINE VARIABLE v-job-cust AS LOG NO-UNDO.
DEFINE VARIABLE lv-under-run AS DECIMAL NO-UNDO.
DEFINE VARIABLE lv-over-run AS DECIMAL NO-UNDO.
DEFINE VARIABLE cContact AS CHARACTER NO-UNDO .

DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.
FORMAT HEADER
       /*"F a c t o r y   T i c k e t"    at 2 */
       "<P7><FCourier New>JOB  TICKET" AT 2
       SPACE(3)
       v-iso
       SPACE(30)
       "PRINTER'S FINISHING Job Ticket"   SPACE(110)
       /* was 196*/
       /*fill(chr(95),130)               at 2    format "x(130)"         */      
      "<#1><C+80><LINE#1><|3>" 
      SKIP
       "Job #:"                         AT 3
       v-job-prt
       v-form-hdr                       AT 20
       "Our Order #:"                   AT 47
       v-ord-no
       "Ord Date:"                      AT 93
       v-ord-date
       v-loc                            AT 118

       "Est #:"                         AT 3
       v-est-no
       v-set-hdr                        AT 18
       "FG#:"                           AT 47
       v-fg
       "Due Date:"                      AT 93
       v-due-date
       v-loc-bin                        AT 118

    WITH NO-BOX FRAME printhead NO-LABELS WIDTH 146 /*132*/  STREAM-IO.

   FIND FIRST sys-ctrl
       WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "JOBQTYCUST"
       NO-LOCK NO-ERROR.

   IF NOT AVAILABLE sys-ctrl THEN
       DO TRANSACTION:
       CREATE sys-ctrl.
       ASSIGN
           sys-ctrl.company  = cocode
           sys-ctrl.NAME     = "JOBQTYCUST"
           sys-ctrl.module   = "JC"
           sys-ctrl.descrip = "Create Job Quantity with overrun % from customer if no order?"
           sys-ctrl.log-fld = NO .
       END.

  v-job-cust = sys-ctrl.log-fld.

ASSIGN
 v-line[1] = CHR(95) + fill(CHR(95),40) + chr(95) + "  " +
             chr(95) + fill(CHR(95),40) + chr(95) + "  " +
             chr(95) + fill(CHR(95),40) + chr(95)  
 v-line[2] = v-line[1]
 v-line[3] = CHR(95) + fill(CHR(95),128) + chr(95)
 v-line[4] = v-line[3]
 v-line[5] = CHR(95) + fill(CHR(95),84) + chr(95) + "  " +
                chr(95) + fill(CHR(95),40) + chr(95)
 v-line[6] = v-line[5]
 v-line[7] = CHR(95) + fill(CHR(95),25) + chr(95) + "  " +
             chr(95) + fill(CHR(95),99) + chr(95)
 v-line[8] = v-line[7]
 v-qty-or-sup = IF CAN-DO("TriState,RFC,Boxtech,Brick,Corrugat,ASI",v-format)
                THEN ("Supplier: "     + fill("_",28))
                ELSE ("Qty Received: " + fill("_",24))
 v-local-copies = 1.

DO v-local-loop = 1 TO v-local-copies:
    {cecrep/jobprem.i}
      break by job.job-no BY job.job-no2 BY job-hdr.frm:

      v-break = FIRST-OF(job.job-no2).

      RELEASE xest.
      RELEASE xef.
      RELEASE xeb.
      RELEASE xoe-ord.
      RELEASE xoe-ordl.
      RELEASE xoe-rel.

      RUN cecrep/jobtick1.p (RECID(job-hdr), v-format,
                              v-local-loop, v-local-copies).

      FOR EACH w-ef WHERE (w-ef.frm = job-hdr.frm OR est.est-type <> 8),
          EACH b-eb NO-LOCK WHERE b-eb.company = job-hdr.company
                              AND b-eb.est-no = job-hdr.est-no 
                              AND b-eb.form-no = w-ef.frm
                              AND (b-eb.blank-no = job-hdr.blank-no OR est.est-type NE 8)
         BREAK BY w-ef.frm BY b-eb.blank-no:
         
        RELEASE xef.
        RELEASE xeb.
        RELEASE xstyle.
        RELEASE xxprep.
        
        RUN cecrep/jobtick2csc.p (RECID(w-ef), RECID(job-hdr), ROWID(b-eb)).

        v-pqty = 1.
        IF AVAILABLE xeb THEN DO:
          IF xeb.stock-no NE "" THEN v-fg = xeb.stock-no.
          IF xest.est-type EQ 6 THEN v-fg = TRIM(v-fg) + "  CP#: " +
                                            xeb.part-no.
          
          {cec/rollfac.i}
          v-pqty = IF v-rollfac OR xeb.est-type EQ 8 THEN 1 ELSE
                   IF xeb.quantityPerSet LT 0 THEN (-1 / xeb.quantityPerSet)
                                       ELSE xeb.quantityPerSet.
        END.
        
        ASSIGN
         v-loc     = ""
         v-loc-bin = "".
         
        IF v-format EQ "Brick" OR v-format EQ "Corrugat" OR  v-format EQ "ASI"
        THEN DO: 
          v-iso = "ISO# CS-05-1-F".

          RELEASE fg-rdtlh.
          
          FIND FIRST fg-bin
            WHERE fg-bin.company   EQ cocode
              AND fg-bin.i-no      EQ job-hdr.i-no
              AND fg-bin.job-no    EQ job-hdr.job-no
              AND fg-bin.job-no2   EQ job-hdr.job-no2
              AND fg-bin.loc       EQ job-hdr.loc
              AND fg-bin.qty       NE 0
          NO-LOCK NO-ERROR.
          IF AVAILABLE fg-bin THEN DO:
            ASSIGN
              v-loc     = "Whs: " + fg-bin.loc
              v-loc-bin = "Bin: " + fg-bin.loc-bin.
          END.
          ELSE
          IF AVAILABLE itemfg THEN DO:                             
            ASSIGN
              v-loc     = "Whs: " + itemfg.def-loc
              v-loc-bin = "Bin: " + itemfg.def-loc-bin.
          END.

        END. /*brick format*/

        VIEW FRAME printhead.  /* factory header display  */
        
        IF v-format EQ "RFC" OR v-format EQ "Boxtech" THEN
          ASSIGN
           v-i-line[1] = itemfg.i-name
           v-i-line[2] = itemfg.part-dscr1
           v-i-line[3] = itemfg.part-dscr2
           v-i-line[4] = itemfg.part-dscr3.
        ELSE
          ASSIGN
           v-i-line[1] = "ITEM DESCRIPTION"
           v-i-line[2] = "Style: " + IF AVAILABLE xstyle THEN xstyle.dscr ELSE ""
           v-i-line[3] = "Size: "  + IF AVAILABLE xeb    THEN
                     TRIM(STRING({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(STRING({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(STRING({sys/inc/k16v.i xeb.dep},">,>>9.99")) ELSE ""
           v-i-line[4] = "Joint: " + IF AVAILABLE xeb THEN v-joint-dscr ELSE "".
   
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
            
        lv-part-no = IF AVAILABLE xoe-ordl THEN xoe-ordl.part-no 
                     ELSE itemfg.part-no.
        lv-ord-qty = (IF AVAILABLE xoe-ordl THEN xoe-ordl.qty
                     ELSE job-hdr.qty) * 
                     (IF est.form-qty LE 1 THEN 1 ELSE v-pqty).
         ASSIGN
             lv-over-run  = IF AVAILABLE xoe-ordl THEN xoe-ordl.over-pct ELSE 
                            IF AVAILABLE xoe-ord  THEN xoe-ordl.over-pct ELSE 0
             lv-under-run = IF AVAILABLE xoe-ordl THEN xoe-ordl.under-pct ELSE
                             IF AVAILABLE xoe-ord  THEN xoe-ordl.under-pct  ELSE 0  .
          IF v-job-cust AND NOT AVAILABLE xoe-ord AND AVAILABLE cust THEN
             ASSIGN
             lv-over-run  = cust.over-pct
             lv-under-run = cust.under-pct .

          IF AVAILABLE xoe-ord THEN
              ASSIGN cContact = xoe-ord.contact .
          ELSE IF AVAILABLE cust  THEN
              ASSIGN cContact = cust.contact .
          ELSE
              ASSIGN cContact = "".


        DISPLAY "<=#2> CUSTOMER INFORMATION" SKIP
              v-cus[1] AT 3 SKIP
              v-cus[2] AT 3 SKIP
              v-cus[3] AT 3 SKIP
              v-cus[4] AT 3 SKIP
            "Contact:" AT 3  cContact FORMAT "x(25)" SKIP
            "<=#3><R-6><b> ORDER INFORMATION"
            "<=#3><R-5> PO #:" 
            xoe-ord.po-no WHEN AVAILABLE xoe-ord
            "Set Qty:"
            TRIM(STRING(IF AVAILABLE xoe-ordl THEN xoe-ordl.qty
                                          ELSE job-hdr.qty,">>>,>>9"))
                        WHEN AVAILABLE xeb AND xeb.est-type EQ 6    FORMAT "x(9)"
            "<=#3><R-4> Job Qty:"
             TRIM(STRING(job-hdr.qty * v-pqty,">>>,>>9"))    FORMAT "x(7)"
            "Order Qty:"
            TRIM(STRING( (IF AVAILABLE xoe-ordl THEN xoe-ordl.qty
                                             ELSE job-hdr.qty) *
                                            IF est.form-qty LE 1 THEN 1
                                            ELSE v-pqty,">>>,>>9"))
                                            FORMAT "x(7)"
            "<=#3><R-3> Cust Part #:" lv-part-no 
            "<=#3><R-2> Overrun:" 
             TRIM(STRING(lv-over-run,">>9.99%")) FORMAT "x(7)"                                                         
            "Underrun:"
            TRIM(STRING(lv-under-run,">>9.99%")) FORMAT "x(7)"
            "<=#3><R-1>        " lv-ord-qty * ( 1 + round((lv-over-run) / 100,2)) FORM ">>>,>>9"
                       "         "     lv-ord-qty * ( 1 - round((lv-under-run) / 100,2)) FORM ">>>,>>9" "</b>"
            WITH NO-BOX NO-LABELS NO-ATTR-SPACE WIDTH 145 STREAM-IO.

     PUT UNFORMATTED
            "<=#4> " v-i-line[1] FORM "x(40)"
            "<=#4><R+1> " v-i-line[2] FORM "x(40)"
            "<=#4><R+2> " v-i-line[3] FORM "x(40)"
            "<=#4><R+3> " v-i-line[4] FORM "x(40)"
            "<=#4><R+4> Adders:" v-adders FORM "x(33)" .
                    
        v-form-sqft = ROUND(IF v-corr THEN (v-form-len * v-form-wid * .007)
                                      ELSE (v-form-len * v-form-wid / 144),3).
        
        FIND FIRST xxprep WHERE
             xxprep.company EQ cocode AND
             xxprep.code EQ xeb.plate-no
             NO-LOCK NO-ERROR.

       /* display "<=#5> Shts Req'd:"
                trim(string(v-sht-qty))   format "x(9)"
                " Sq Ft:"
                trim(string(v-form-sqft)) format "x(7)"
                "<=#6><R-6> PLATE #:"
                xeb.plate-no FORMAT "X(15)" when avail xeb
                "Loc: "
                xxprep.loc-bin FORMAT "X(8)" WHEN AVAIL xxprep
                "<=#7> DIE CUTTING, SLIT, & SAW"                
            with no-box no-labels frame m2 width 145 NO-ATTR-SPACE STREAM-IO.*/

        i = 0.
        FOR EACH w-i:
          i = i + 1.
        END.
        IF i LT 4 THEN DO i = i + 1 TO 4:
          CREATE w-i.
        END.

        FIND FIRST w-i.
        FIND FIRST xxprep WHERE xxprep.company EQ cocode
                            AND xxprep.code EQ xeb.die-no
                            NO-LOCK NO-ERROR.
        v-die-loc = IF AVAILABLE xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

        IF v-format EQ "TriLakes" THEN DO:
        DISPLAY "<=#5><R+1>"
                "W: " + trim(STRING({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
                "   " +
                "L: " + trim(STRING({sys/inc/k16v.i v-form-len},">,>>9.99"))
                                                                FORMAT "x(25)"
                "MSF:"  +
                trim(STRING(v-sht-qty * v-form-sqft / 1000,">>>9.9<"))
                                                                FORMAT "x(11)"
                "<=#6><R-5> 1:"
                w-i.i-code
                w-i.i-qty WHEN w-i.i-qty NE 0
                "5:"
                w-i.i-code2
                w-i.i-qty2 WHEN w-i.i-qty2 NE 0
                "<=#7><R+1> Die #:"
                xeb.die-no WHEN AVAILABLE xeb
                " Loc:"   /*v-die-loc */ 
                xxprep.loc-bin WHEN AVAILABLE xeb AND AVAILABLE xxprep      
            WITH NO-BOX NO-LABELS FRAME i1 WIDTH 150 NO-ATTR-SPACE STREAM-IO.
        FIND NEXT w-i.        
        DISPLAY "<=#5><R+2> Board:"
                v-form-code 
                "<=#6><R-4> 2:"
                w-i.i-code
                w-i.i-qty WHEN w-i.i-qty NE 0
                "6:"
                w-i.i-code2
                w-i.i-qty2 WHEN w-i.i-qty2 NE 0
                "<=#7><R+2> Blank Size:"
                "W:" TRIM(STRING({sys/inc/k16v.i xeb.t-wid},">,>>9.99")) WHEN AVAILABLE xeb 
                "L:" TRIM(STRING({sys/inc/k16v.i xeb.t-len},">,>>9.99")) WHEN AVAILABLE xeb 
            WITH NO-BOX NO-LABELS FRAME i2 WIDTH 155 NO-ATTR-SPACE STREAM-IO.

        FIND NEXT w-i.
        DISPLAY "<=#5><R+3><C+5> " v-form-dscr
                "<=#6><R-3> 3:"
                w-i.i-code
                w-i.i-qty WHEN w-i.i-qty NE 0
                "7:"
                w-i.i-code2
                w-i.i-qty2 WHEN w-i.i-qty2 NE 0
                "<=#7><R+3> Up:"
                "W:" v-upl FORM ">9" 
                "  L:"  v-upw FORM ">9"
                "Slit:  W:"  v-outw FORM ">9"
                "L:"  v-outl FORM ">9"                                          
            WITH NO-BOX NO-LABELS FRAME i3 WIDTH 155 NO-ATTR-SPACE STREAM-IO.
        
        FIND NEXT w-i.
        DISPLAY "<=#5><R+4> Score:"
                 v-len-score     WHEN xstyle.TYPE <> "F"  FORMAT "x(32)"
                "<=#6><R-2> 4:"
                w-i.i-code
                w-i.i-qty WHEN w-i.i-qty NE 0
                "8:"
                w-i.i-code2
                w-i.i-qty2 WHEN w-i.i-qty2 NE 0
                "<=#7><R+4> Impressions:"
                TRIM(STRING(v-dc-qty))    FORMAT "x(7)"
                " To: " +
                trim(STRING({sys/inc/k16v.i xef.nsh-wid},">>9.99")) +
                "x" +
                trim(STRING({sys/inc/k16v.i xef.nsh-len},">>9.99"))
                          WHEN AVAILABLE xef AND 
                               (xef.nsh-wid NE xef.gsh-wid OR
                                xef.nsh-len NE xef.gsh-len)
                                          FORMAT "x(17)"
            WITH NO-BOX NO-LABELS FRAME i4 WIDTH 155 NO-ATTR-SPACE STREAM-IO.
        END.

        ELSE DO:
        DISPLAY "<=#5><R+1>"
                "W: " + trim(STRING({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
                "   " +
                "L: " + trim(STRING({sys/inc/k16v.i v-form-len},">,>>9.99"))
                                                                FORMAT "x(25)"
                "MSF:"  +
                trim(STRING(v-sht-qty * v-form-sqft / 1000,">>>9.9<"))
                                                                FORMAT "x(11)"
                "<=#6><R-5>       "
                w-i.i-dscr
                w-i.i-qty WHEN w-i.i-qty NE 0
                "LBS" WHEN w-i.i-dscr NE ""
                "<=#7><R+1> Die #:"
                xeb.die-no WHEN AVAILABLE xeb
                " Loc:"   /*v-die-loc */ 
                xxprep.loc-bin WHEN AVAILABLE xeb AND AVAILABLE xxprep      
            WITH NO-BOX NO-LABELS FRAME i5 WIDTH 150 NO-ATTR-SPACE STREAM-IO.
        FIND NEXT w-i.        
        DISPLAY "<=#5><R+2> Board:"
                v-form-code 
                "<=#6><R-4>       "
                w-i.i-dscr
                w-i.i-qty WHEN w-i.i-qty NE 0
                "LBS" WHEN w-i.i-dscr NE ""
                "<=#7><R+2> Blank Size:"
                "W:" TRIM(STRING({sys/inc/k16v.i xeb.t-wid},">,>>9.99")) WHEN AVAILABLE xeb 
                "L:" TRIM(STRING({sys/inc/k16v.i xeb.t-len},">,>>9.99")) WHEN AVAILABLE xeb 
            WITH NO-BOX NO-LABELS FRAME i6 WIDTH 155 NO-ATTR-SPACE STREAM-IO.

        FIND NEXT w-i.
        DISPLAY "<=#5><R+3><C+5> " v-form-dscr
                "<=#6><R-3>       "
                w-i.i-dscr
                w-i.i-qty WHEN w-i.i-qty NE 0
                "LBS" WHEN w-i.i-dscr NE ""
                "<=#7><R+3> Up:"
                "W:" v-upl FORM ">9" 
                "  L:"  v-upw FORM ">9"
                "Slit:  W:"  v-outw FORM ">9"
                "L:"  v-outl FORM ">9"                                          
            WITH NO-BOX NO-LABELS FRAME i7 WIDTH 155 NO-ATTR-SPACE STREAM-IO.
        
        FIND NEXT w-i.
        DISPLAY "<=#5><R+4> Score:"
                 v-len-score     WHEN xstyle.TYPE <> "F"  FORMAT "x(32)"
                "<=#6><R-2>       "
                w-i.i-dscr
                w-i.i-qty WHEN w-i.i-qty NE 0
                "LBS" WHEN w-i.i-dscr NE ""
                "<=#7><R+4> Impressions:"
                TRIM(STRING(v-dc-qty))    FORMAT "x(7)"
                " To: " +
                trim(STRING({sys/inc/k16v.i xef.nsh-wid},">>9.99")) +
                "x" +
                trim(STRING({sys/inc/k16v.i xef.nsh-len},">>9.99"))
                          WHEN AVAILABLE xef AND 
                               (xef.nsh-wid NE xef.gsh-wid OR
                                xef.nsh-len NE xef.gsh-len)
                                          FORMAT "x(17)"
            WITH NO-BOX NO-LABELS FRAME i8 WIDTH 155 NO-ATTR-SPACE STREAM-IO.
        END.
            
        IF CAN-DO("TriState,RFC,Boxtech,Brick,Corrugat,ASI,Xprint,Pacific,TriLakes",v-format) THEN DO:
          RUN sys/ref/getpo#.p (IF AVAILABLE xoe-ordl AND est.est-type NE 6 THEN ROWID(xoe-ordl) ELSE ROWID(job),
                                w-ef.frm, OUTPUT v-po-no).

          RELEASE po-ord.
          
          IF v-po-no NE 0 THEN
          FIND FIRST po-ord
              WHERE po-ord.company EQ cocode
                AND po-ord.po-no   EQ v-po-no
              NO-LOCK NO-ERROR.
          
          ASSIGN
           v-vend-no    = IF AVAILABLE po-ord THEN po-ord.vend-no ELSE ""
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
        DISPLAY "<=#5><R+5>" v-qty-or-sup
                     "<=#6><R-1>            "
                     "<=#6><R-1><C+12> " xeb.i-coldscr WHEN AVAILABLE xeb
                     "<=#7><R+5> D/C Style:"                             
                         WITH NO-BOX NO-LABELS FRAME m3 WIDTH 132 NO-ATTR-SPACE STREAM-IO.
        i = 0.
        FOR EACH w-m:
          i = i + 1.
        END.
        IF i LT 3 THEN DO i = i + 1 TO 3:
          CREATE w-m.
          w-m.dseq = 999999999.
        END.
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

        IF v-format EQ "Brick" OR v-format = "ASI" THEN 
             PUT "<=#8> Machine Routing        SU:    Start    Stop    Total   RUN:  Hours   Start   Stop   Total     QTY: In     Out      Waste" SKIP.
        ELSE PUT "<=#8> Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP.
        
        PUT "<=#8><R+1><C+1><from><C+76><Line>"
                    SKIP.

        i = 0.
        FOR EACH w-m BY w-m.dseq:

          i = i + 1.
          FIND FIRST mach WHERE mach.company EQ cocode
                            AND mach.m-dscr  EQ w-m.dscr NO-LOCK NO-ERROR.
          lv-m-dscr = IF AVAILABLE mach THEN mach.m-code ELSE w-m.dscr.
          v-letter = substr("UTE",i,1).

          IF v-format EQ "Brick" OR v-format = "ASI" THEN
          DISPLAY lv-m-dscr AT 3
                  w-m.s-hr                              WHEN w-m.s-hr NE 0
                  FILL("_",7)  FORMAT "x(7)"    TO 38   WHEN w-m.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 46   WHEN w-m.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 54   WHEN w-m.dscr NE ""
                  SPACE(4)
                  w-m.r-sp                              WHEN w-m.r-sp NE 0
                  w-m.r-hr                              WHEN w-m.r-hr NE 0
                  FILL("_",7)  FORMAT "x(7)"    TO 81   WHEN w-m.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 89   WHEN w-m.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 97   WHEN w-m.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"        TO 111  WHEN w-m.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"    TO 120  WHEN w-m.dscr NE ""
              FILL("_",8)  FORMAT "x(8)"    TO 129  WHEN w-m.dscr NE ""                
              WITH NO-BOX NO-LABELS FRAME o1 WIDTH 132 NO-ATTR-SPACE DOWN STREAM-IO.
                  
          ELSE
          DISPLAY w-m.dscr AT 3
                  w-m.s-hr WHEN w-m.s-hr NE 0
                  FILL("_",7)  FORMAT "x(7)"    TO 38   WHEN w-m.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 46   WHEN w-m.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 54   WHEN w-m.dscr NE ""
                  SPACE(2)
                  w-m.r-sp WHEN w-m.r-sp NE 0
                  FILL("_",7)  FORMAT "x(7)"    TO 69   WHEN w-m.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 77   WHEN w-m.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 85   WHEN w-m.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"    TO 99   WHEN w-m.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"    TO 108  WHEN w-m.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"    TO 117  WHEN w-m.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"    TO 129  WHEN w-m.dscr NE ""                 
              WITH NO-BOX NO-LABELS FRAME o2 WIDTH 132 NO-ATTR-SPACE DOWN STREAM-IO.
                  
          v-lines = v-lines + 1.
        END.

        FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                           AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        
        IF AVAILABLE b-ef AND b-ef.form-no = w-ef.frm THEN 
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
        RUN cecrep/jobtick3.p (RECID(job-hdr),v-format,cust.terms).
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

        DISPLAY "<=#10><R-1>" /* -1*/
                "N"                      AT 1
                "SPECIAL INSTRUCTIONS.." AT 3
                CHR(124) FORMAT "x"       AT 87
                CHR(124) FORMAT "x"       AT 90
                "SHIPPING INFO"
                "      Ship To #:"
                xoe-ord.sold-id WHEN AVAILABLE xoe-ord
                xeb.ship-id WHEN AVAILABLE xeb @ xoe-ord.sold-id
                xoe-rel.ship-id WHEN AVAILABLE xoe-rel @ xoe-ord.sold-id SKIP
                "O"                       AT 1
                v-dept-inst[1] FORMAT "x(82)" AT 3
                CHR(124) FORMAT "x"       AT 87
                CHR(124) FORMAT "x"       AT 90
                v-shp[1]
                "T"                       AT 1
                v-dept-inst[2] FORMAT "x(82)" AT 3
                CHR(124) FORMAT "x"       AT 87
                CHR(124) FORMAT "x"       AT 90
                v-shp[2]
                "E"                       AT 1
                v-dept-inst[3] FORMAT "x(82)" AT 3
                CHR(124) FORMAT "x"       AT 87
                CHR(124) FORMAT "x"       AT 90
                v-shp[3]
                "S"                       AT 1
                v-dept-inst[4] FORMAT "x(82)" AT 3
                CHR(124) FORMAT "x"       AT 87
                CHR(124) FORMAT "x"       AT 90
                v-shp[4]
                v-dept-inst[5] FORMAT "x(82)" AT 3
                CHR(124) FORMAT "x"       AT 87
                CHR(124) FORMAT "x"       AT 90
                "Item PO #:"
                xoe-ordl.po-no WHEN AVAILABLE xoe-ordl
                v-dept-inst[6] FORMAT "x(128)" AT 3
                SKIP(1)
            WITH NO-BOX NO-LABELS FRAME m8 WIDTH 132 NO-ATTR-SPACE STREAM-IO.
   
        IF print-box AND AVAILABLE xest THEN DO:            
            RUN cec/desprnt3print.p (RECID(xef),
                                   INPUT-OUTPUT v-lines,
                                   RECID(xest),
                                   IF AVAILABLE xeb THEN ROWID(xeb) ELSE ?).
        END.
        ELSE PAGE.
       
      END.  /* for each w-ef */
            
      IF s-prt-set-header AND last-of(job.job-no2) AND est.est-type = 6 THEN DO: /* print set header */
        i = 0.
        FOR EACH bf-eb WHERE bf-eb.company = est.company
                          AND bf-eb.est-no = est.est-no
                          AND bf-eb.form-no > 0 NO-LOCK:
             i = i + 1.
        END.   

        IF i > 1 THEN DO:
        
           DEFINE VARIABLE v-set-qty AS INTEGER NO-UNDO.
           DEFINE VARIABLE v-ord-qty AS INTEGER NO-UNDO.
           DEFINE VARIABLE v-over-run AS cha NO-UNDO.
           DEFINE VARIABLE v-under-run AS cha NO-UNDO.
           DEFINE VARIABLE v-fg-set AS cha FORM "x(15)" NO-UNDO.
           ASSIGN
           v-fg-set = job-hdr.i-no
           v-set-qty = IF AVAILABLE xeb AND xeb.est-type EQ 6 THEN
                         IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty
                       ELSE 0
           v-ord-qty = (IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty) *
                       IF est.form-qty LE 1 THEN 1 ELSE v-pqty
           v-over-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.over-pct,">>9.99%")) ELSE
                        IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.over-pct,">>9.99%"))  ELSE ""
           v-under-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.under-pct,">>9.99%")) ELSE
                         IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.under-pct,">>9.99%"))  ELSE "".

                    IF v-job-cust AND NOT AVAILABLE xoe-ord AND AVAILABLE cust THEN
               ASSIGN
                    v-over-run  = TRIM(STRING(cust.over-pct,">>9.99%"))
                    v-under-run = TRIM(STRING(cust.under-pct,">>9.99%")) .

           PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
               "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
               "<C60>Our Date: " v-ord-date SKIP
               "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
               "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
               "<=1><R+6><C2>CUSTOMER INFORMATION <C25><b> ORDER INFORMATION </b><C53>ITEM DESCRIPTION" SKIP
               v-cus[1] AT 3 "<b> PO#: " v-po-no " Set Qty: "  v-set-qty "</b>"
               v-i-line[2] AT 90
               SKIP
               v-cus[2] AT 3 "<b> Job Qty:" TRIM(STRING(job-hdr.qty * v-pqty,">>>,>>9"))    FORMAT "x(7)"
               " Order Qty:" STRING(v-ord-qty) FORMAT "x(7)" "</b>"
               v-i-line[3] AT 90 SKIP
               v-cus[3] AT 3  "<b> Cust Part #:" lv-part-no "</b>"
               v-i-line[4] AT 90 SKIP
               v-cus[4]  AT 3 "<b> Overrun:" v-over-run FORMAT "x(7)"  
               " Underrun:" v-under-run FORMAT "x(7)" SPACE(8) "</b>" 
               "Adders:" v-adders FORM "x(33)" SKIP
               "<=1><R+11><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
               "<C2>FINISHED GOOD #                 DESCRIPTION                       RATIO PER SET" SKIP.
           /* each components */
           DEFINE VARIABLE v-tmp-line AS INTEGER NO-UNDO.
           DEFINE VARIABLE v-shipto AS cha NO-UNDO.

           v-tmp-line = 0.
           FOR EACH xeb WHERE xeb.company = est.company
                           AND xeb.est-no = est.est-no
                           AND xeb.form-no > 0 NO-LOCK:
               PUT xeb.stock-no AT 3 SPACE(14) xeb.part-dscr1 SPACE(5) xeb.quantityPerSet SKIP.
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
                 PUT b-ef.spec-dscr[i] AT 32 SPACE(16) lv-spec-qty[i] SKIP.
                 v-tmp-line = v-tmp-line + 1.
              END.
           END.
           PUT "<=1><R+12><C2><FROM><R+" + string(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)" SKIP.
           ASSIGN
              v-tmp-line = v-tmp-line + 12
              i = 0.

           FOR EACH tt-wm WHERE LOOKUP(tt-wm.m-code,tspostfg-char) > 0:
                i = i + 1.
           END.
           i = i + 2.
           PUT "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
               "<=1><R+" + string(v-tmp-line + 1) + "><C2><FROM><R+" + string(i) + "><C78><RECT><||3>" FORM "x(150)" SKIP
               "<=1><R+" + string(v-tmp-line + 1) + ">" FORM "x(20)".

           i = 0.
           FOR EACH tt-wm WHERE LOOKUP(tt-wm.m-code,tspostfg-char) > 0  BY tt-wm.dseq:
             i = i + 1.
             DISPLAY tt-wm.dscr AT 3
                  tt-wm.s-hr WHEN tt-wm.s-hr NE 0
                  FILL("_",7)  FORMAT "x(7)"    TO 38   WHEN tt-wm.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 46   WHEN tt-wm.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 54   WHEN tt-wm.dscr NE ""
                  SPACE(2)
                  tt-wm.r-sp WHEN tt-wm.r-sp NE 0
                  FILL("_",7)  FORMAT "x(7)"    TO 69   WHEN tt-wm.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 77   WHEN tt-wm.dscr NE ""
                  FILL("_",7)  FORMAT "x(7)"    TO 85   WHEN tt-wm.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"    TO 99   WHEN tt-wm.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"    TO 108  WHEN tt-wm.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"    TO 117  WHEN tt-wm.dscr NE ""
                  FILL("_",8)  FORMAT "x(8)"    TO 129  WHEN tt-wm.dscr NE ""
                  /*chr(124) format "x"           at 131   */                  
                  WITH NO-BOX NO-LABELS FRAME o21 WIDTH 132 NO-ATTR-SPACE DOWN STREAM-IO.

           END.
           FOR EACH tt-wm:
               DELETE tt-wm.
           END.
           ASSIGN
           v-tmp-line = v-tmp-line + 3 + i
           v-shipto = IF AVAILABLE xoe-rel THEN xoe-rel.ship-id 
                      ELSE IF AVAILABLE xeb THEN xeb.ship-id
                      ELSE IF AVAILABLE xoe-ord THEN xoe-ord.sold-id 
                      ELSE "".
           FIND FIRST tt-prem WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
                                 AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
           IF NOT AVAILABLE tt-prem THEN CREATE tt-prem.

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
           PUT "<=1><R+" + string(v-tmp-line) + ">" FORM "X(20)".
           v-tmp-line = v-tmp-line + 1.
           
           PUT "Unitizing Bale <C24>Date <C44>Units <C62>Complete" AT 3 SKIP
               "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________" SKIP
               "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial" SKIP
               "Pattern: " AT 3 tt-prem.tt-pattern "<C20>_____________________ <C40>____________________  <C60>________________" SKIP
               "Pallet: " AT 3 tt-prem.tt-pallet "<C20>_____________________ <C40>____________________ " SKIP
               "<=1><R+" + string(v-tmp-line) + "><C2><FROM><R+6><C78><RECT><||3>" FORM "x(150)" SKIP
               "<=1><R+" + string(v-tmp-line + 7) + "><C2><FROM><R+7><C78><RECT><||3>" FORM "x(150)" SKIP

               "<=1><R+" + string(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
               v-dept-inst[1] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[1] SKIP
               v-dept-inst[2] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[2] SKIP
               v-dept-inst[3] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[3] SKIP
               v-dept-inst[4] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[4] SKIP
               v-dept-inst[5] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" "Item PO #:" v-po-no SKIP
               v-dept-inst[6] 
               .
                             
           PAGE.
        END. /* set header printing */
      END. /* est.est-type = 6 */
     /* end of set header printing */

    END.  /* each job */
    END.  /* end v-local-loop  */
 
HIDE ALL NO-PAUSE.

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
