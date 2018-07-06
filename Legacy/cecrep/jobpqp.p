/* ********************************************************* cecrep/jobpqp.i  */
/*  N-K = JOBCARDC - FACTORY TICKET FOR PQP                                    */
/* ************************************************************************** */

{jcrep/r-ticket.i "shared"}

{cecrep/jobpqpH1.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}
{custom/notesdef.i}
{cecrep/jc-prem.i}

DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.

&SCOPED-DEFINE PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

DEF STREAM ctl.

DEF BUFFER b-ef FOR ef.
DEF BUFFER bf-eb FOR eb.

DEF WORK-TABLE tt-wm LIKE w-m. /* CANT CHANGE BECAUSE OF jobtick.i */


DEF INPUT PARAM v-FORMAT AS CHAR NO-UNDO.

DEF NEW SHARED VAR v-out1-id        AS RECID NO-UNDO.
DEF NEW SHARED VAR v-out2-id        AS RECID NO-UNDO.
DEF     SHARED VAR s-prt-set-header AS LOG   NO-UNDO.

DEF VAR v-mach-var AS INT NO-UNDO.
DEF VAR v-mach AS CHAR FORMAT "X(8)" EXTENT 4 NO-UNDO.
DEF VAR prt-copies     AS INT         NO-UNDO.
DEF VAR v-note-length  AS INT INIT 80 NO-UNDO.
DEF VAR v-local-copies AS INT         NO-UNDO.
DEF VAR v-local-loop   AS INT INIT 1  NO-UNDO.
DEF VAR lv-rt-num      AS INT         NO-UNDO.
DEF VAR lv-add-entry   AS INT         NO-UNDO.

DEF VAR v-start-compress AS CHAR                          NO-UNDO.
DEF VAR v-end-compress   AS CHAR                          NO-UNDO.
DEF VAR v-inst2          AS CHAR                EXTENT 6  NO-UNDO.    
DEF VAR v-dept-inst      AS CHAR FORMAT "x(47)" EXTENT 6  NO-UNDO.
DEF VAR v-qty-or-sup     AS CHAR FORMAT "x(38)"           NO-UNDO.
DEF VAR v-i-line         AS CHAR /*FORMAT "X(38)"*/ EXTENT 4  NO-UNDO.
DEF VAR v-i-line-set     AS CHAR /*FORMAT "X(38)"*/ EXTENT 4  NO-UNDO.
DEF VAR lv-part-no       AS CHAR FORMAT "x(15)"           NO-UNDO.
DEF VAR v-tmp-stype      AS CHAR                          NO-UNDO.
DEF VAR v-len-score2     AS CHAR                EXTENT 13 NO-UNDO.
DEF VAR v-tmp-score      AS CHAR                          NO-UNDO.

DEF VAR k_frac      AS DEC INIT 6.25                      NO-UNDO.
DEF VAR v-pqty      AS DEC                                NO-UNDO.
DEF VAR v-leaf-W    AS DEC FORMAT ">>9.99"                NO-UNDO.
DEF VAR v-leaf-L    AS DEC FORMAT ">>9.99"                NO-UNDO.

DEF VAR lv-ord-qty  LIKE oe-ordl.qty                        NO-UNDO.
DEF VAR v-vend-no   LIKE oe-ordl.vend-no                    NO-UNDO.
DEF VAR v-po-no     LIKE oe-ordl.po-no-po                   NO-UNDO.
DEF VAR lv-m-dscr   LIKE w-m.dscr                           NO-UNDO.
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEF VAR v-die-W     LIKE prep.die-w                         NO-UNDO.
DEF VAR v-die-L     LIKE prep.die-l                         NO-UNDO.

DEF VAR laser         AS LOG INIT NO  FORMAT "Y/N" NO-UNDO.
DEF VAR v-flag        AS LOG INIT NO               NO-UNDO.
DEF VAR v-print-score AS LOG INIT YES              NO-UNDO.
DEF VAR v-xg-flag     AS LOG                       NO-UNDO.
DEF VAR v-line-1 AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-line-2 AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-line-3 AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-line-4 AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-line-5 AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-line-6 AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-line-7 AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-line-8 AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-line-9 AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-line-10 AS CHAR FORMAT "X(200)" NO-UNDO.

ASSIGN v-line[1] = CHR(95) + FILL(CHR(95),40) + CHR(95) + "  " +
                   CHR(95) + FILL(CHR(95),40) + CHR(95) + "  " +
                   CHR(95) + FILL(CHR(95),40) + CHR(95)  
       v-line[2] = v-line[1]
       v-line[3] = CHR(95) + FILL(CHR(95),128) + CHR(95)
       v-line[4] = v-line[3]
       v-line[5] = CHR(95) + FILL(CHR(95),84) + CHR(95) + "  " +
                   CHR(95) + FILL(CHR(95),40) + CHR(95)
       v-line[6] = v-line[5]
       v-line[7] = CHR(95) + FILL(CHR(95),25) + CHR(95) + "  " +
                   CHR(95) + FILL(CHR(95),99) + CHR(95)
       v-line[8] = v-line[7]
       v-qty-or-sup = IF LOOKUP(v-FORMAT,"TriState,RFC,Boxtech,Brick,Corrugat,ASI") GT 0
                        THEN ("Supplier: "     + FILL("_",28))
                        ELSE ("Qty Received: " + FILL("_",24)).

ASSIGN v-local-copies = 1
       prt-copies     = 1.

DO v-local-loop = 1 TO v-local-copies:
  {cecrep/jobpqp.i}
    BREAK BY job.job-no 
          BY job.job-no2
          BY job-hdr.job-no2:
    
    ASSIGN v-break = FIRST-OF(job.job-no2).
    
    RELEASE xest.
    RELEASE xef.
    RELEASE xeb.
    RELEASE xoe-ord.
    RELEASE xoe-ordl.
    RELEASE xoe-rel.

    RUN cecrep/jobtick1.p (RECID(job-hdr), v-FORMAT,
                           v-local-loop, v-local-copies).

    FOR EACH w-ef WHERE w-ef.frm EQ job-hdr.frm OR 
                        est.est-type NE 8 BREAK BY w-ef.frm :
     
     RELEASE xef.
     RELEASE xeb.
     RELEASE xstyle.
     RELEASE xxprep.
        
     RUN cecrep/jobtick2.p (RECID(w-ef), RECID(job-hdr)).

     ASSIGN v-pqty = 1
            v-form1 = TRIM(IF AVAIL xeb THEN STRING(xeb.form-no) ELSE "")
            v-form2 = TRIM(STRING(est.form-qty)).

     IF AVAIL xeb THEN DO:

       IF xeb.stock-no NE "" THEN v-fg = xeb.stock-no.
       IF xest.est-type EQ 6 THEN v-fg = TRIM(v-fg) + "  CP#: " + xeb.part-no.


       {cec/rollfac.i}
       ASSIGN v-pqty = IF v-rollfac OR xeb.est-type EQ 8 
                         THEN 1 
                         ELSE IF xeb.quantityPerSet LT 0 
                                THEN (-1 / xeb.quantityPerSet)
                                ELSE xeb.quantityPerSet.
     END.

     ASSIGN v-loc     = ""
            v-loc-bin = "".

     PUT "<P13>".

     VIEW FRAME head.  /* factory header display  */

     PUT "<C3><FGCOLOR=BLUE>Est #:"
         "<FGCOLOR=GREEN>" v-est-no
         "<C32><FGCOLOR=BLUE> J O B C A R D"
         "<C60><FGCOLOR=BLUE>Ord Date:<FGCOLOR=GREEN><C70>"
         v-ord-date
         SKIP
         "<C3><FGCOLOR=BLUE>Job #:"
         "<FGCOLOR=GREEN>" v-job-prt
         "<C31><FGCOLOR=BLUE>Form #:"
         "<FGCOLOR=GREEN>" v-form1 " OF " v-form2 
         "<C60><FGCOLOR=BLUE>Due Date:"
         "<C70><FGCOLOR=GREEN>" v-due-date
         SKIP
         "<P12>".

     ASSIGN v-i-line[1] = "ITEM DESCRIPTION"
            v-i-line[2] = "<FGCOLOR=BLUE>Style: <FGCOLOR=GREEN>" + 
                          (IF AVAIL xstyle THEN xstyle.dscr ELSE "")
            v-i-line[3] = "<FGCOLOR=BLUE>Size:  <FGCOLOR=GREEN>"  + 
                          (IF AVAIL xeb 
                           THEN TRIM(STRING({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                                TRIM(STRING({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                                TRIM(STRING({sys/inc/k16v.i xeb.dep},">,>>9.99")) 
                           ELSE "")
            v-i-line[4] = "<FGCOLOR=BLUE>Joint: <FGCOLOR=GREEN>" + 
                          (IF AVAIL xeb THEN v-joint-dscr ELSE "")
            v-i-line-set[2] = "Style: " + 
                          (IF AVAIL xstyle THEN xstyle.style ELSE "")
            v-i-line-set[3] = "Size:  "  + 
                          (IF AVAIL xeb THEN
                              TRIM(STRING({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                              TRIM(STRING({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                              TRIM(STRING({sys/inc/k16v.i xeb.dep},">,>>9.99")) 
                           ELSE "")
            v-i-line-set[4] = "Joint: " + 
                          (IF AVAIL xeb THEN v-joint-dscr ELSE "").

     /* BUILD ALL THE BOXES  */
     PUT "<B>" 
         "<#2><R5.5><C1><FROM><R11><C80><RECT>"
         "<C49.5><R5.5><FROM><R11><C49.5><LINE>"
        SKIP
         "<#4><R12><C1><FROM><R17><C80><RECT>" 
         "<C49.5><R12><FROM><R17><C49.5><LINE>"
        SKIP
         "<#6><R18><C1><FROM><R25><C80><RECT>" 
         "<C49.5><R18><FROM><R25><C49.5><LINE>"
        SKIP
         "<#8><R26><C1><FROM><R33.5><C80><RECT>" 
        SKIP
         "<#9><R34.5><C1><FROM><R41.5><C80><RECT>" 
         "</B>".

     ASSIGN lv-part-no = IF AVAIL xest AND xest.est-type = 6 THEN xeb.part-no
                         ELSE IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                         ELSE itemfg.part-no
            lv-ord-qty = (IF AVAIL xoe-ordl THEN xoe-ordl.qty
                          ELSE job-hdr.qty) * 
                         (IF est.form-qty LE 1 THEN 1
                          ELSE v-pqty)
            i = 0.

     FOR EACH w-i:
         i = i + 1.
     END.

     IF i LT 4 THEN DO i = i + 1 TO 4:
       CREATE w-i.
     END.

     FIND FIRST w-i.

     ASSIGN
        v-line-1 = "<=#2><P12><R6><C2><FGCOLOR=BLUE>Customer:<FGCOLOR=GREEN><C14>" +
                   v-cus[1] + "<C50>" + v-i-line[2]
        v-line-2 = "<C2><FGCOLOR=BLUE>Cust Part #:<FGCOLOR=GREEN>" + lv-part-no +
                   "<C50><FGCOLOR=GREEN>" + v-i-line[3]
        v-line-10 = "<C2><FGCOLOR=BLUE>Item Name:<FGCOLOR=GREEN><C14>" +
                   (IF AVAIL xest AND xest.est-type = 6 THEN STRING(xeb.part-dscr1,"X(30)")
                    ELSE IF AVAIL xoe-ordl THEN STRING(xoe-ordl.i-name,"X(30)")
                    ELSE STRING(itemfg.i-name,"X(30)")) +
                   "<C50><FGCOLOR=BLUE>Qty:<FGCOLOR=GREEN><C53>" +
                   TRIM(STRING(job-hdr.qty * v-pqty,">>>,>>9"),"X(7)") +
                   "<C65><FGCOLOR=BLUE>Shts: <FGCOLOR=GREEN>" + trim(string(v-sht-qty,">>>,>>9"))
        v-line-4 = "<C2><FGCOLOR=BLUE>Description:<FGCOLOR=GREEN>" +
                   (IF AVAIL xest AND xest.est-type = 6 THEN STRING(xeb.part-dscr2,"X(30)")
                    ELSE IF AVAIL xoe-ordl THEN STRING(xoe-ordl.part-dscr1,"X(30)")
                    ELSE STRING(itemfg.part-dscr1,"x(30)"))
        v-line-5 = "<C50><FGCOLOR=BLUE>Under:<FGCOLOR=GREEN> " +
                   TRIM(STRING((lv-ord-qty * ( 1 - ROUND((IF AVAIL xoe-ordl 
                                                  THEN xoe-ordl.under-pct 
                                                  ELSE IF AVAIL xoe-ord  
                                                         THEN xoe-ord.under-pct  
                                                        ELSE 0) / 100,2))),">>>,>>9"))
        v-line-6 = "<C65><FGCOLOR=BLUE>Over:<FGCOLOR=GREEN> " +
                   TRIM(STRING((lv-ord-qty * ( 1 + ROUND((IF AVAIL xoe-ordl THEN xoe-ordl.over-pct 
                                               ELSE IF AVAIL xoe-ord THEN xoe-ord.over-pct  
                                               ELSE 0) / 100,2))),">>>,>>9"))
        v-form-sqft = ROUND(IF v-corr THEN (v-form-len * v-form-wid * .007)
                            ELSE (v-form-len * v-form-wid / 144),3).

     PUT v-line-1 SKIP v-line-2 SKIP v-line-10 SKIP v-line-4 v-line-5 v-line-6.
     
     ASSIGN v-die-W   = xef.trim-w
            v-die-L   = xef.trim-l
            v-leaf-W  = xef.leaf-w[1]
            v-leaf-L  = xef.leaf-l[1]
            v-line-1 = "<#4><R12.5><C2><FGCOLOR=BLUE>Board: <FGCOLOR=GREEN>" + v-form-dscr
                     + "<C50><FGCOLOR=BLUE>Die #:  <FGCOLOR=GREEN>"
                     + (IF AVAIL xeb THEN xeb.die-no ELSE "")
            v-line-2 = "<C2><FGCOLOR=BLUE>W: <FGCOLOR=GREEN>" + TRIM(STRING({sys/inc/k16v.i v-form-wid},">,>>9.99"))
                     + "<C12><FGCOLOR=BLUE>L: <FGCOLOR=GREEN>" + TRIM(STRING({sys/inc/k16v.i v-form-len},">,>>9.99"))
            v-line-3 = "<C28><FGCOLOR=BLUE>Sq Ft: <FGCOLOR=GREEN>" + TRIM(STRING(v-form-sqft))
                     + "<C50><FGCOLOR=BLUE>Die Sz: W: <FGCOLOR=GREEN>" + TRIM(STRING({sys/inc/k16v.i v-die-W},">>9.99"))
                     + "<C68><FGCOLOR=BLUE>L: <FGCOLOR=GREEN>" + TRIM(STRING({sys/inc/k16v.i v-die-L},">>9.99"))
            v-line-4 = "<C2><FGCOLOR=BLUE>Adders:<FGCOLOR=GREEN>" + STRING(v-adders,"X(33)")
            v-line-5 = "<C50><FGCOLOR=BLUE>LablSz: W: <FGCOLOR=GREEN>" + TRIM(STRING({sys/inc/k16v.i v-leaf-W},">>9.99"))
                     + "<C68><FGCOLOR=BLUE>L: <FGCOLOR=GREEN>" + TRIM(STRING({sys/inc/k16v.i v-leaf-L},">>9.99"))
            v-line-6 = "<C50><FGCOLOR=BLUE>Up: W: <FGCOLOR=GREEN>" + STRING(v-upl,">>9")
                     + "<C60><FGCOLOR=BLUE> L: <FGCOLOR=GREEN>" + STRING(v-upw,">>9")
            v-line-7 = "<#6><R18><C2><FGCOLOR=BLUE>SPECIAL INSTRUCTION<FGCOLOR=GREEN>"
            v-line-8 = "<C50><FGCOLOR=BLUE>PLATE #: <FGCOLOR=GREEN>" + (IF AVAIL xeb THEN xeb.plate-no ELSE "")
            v-line-9 = "<C50><R19><FGCOLOR=BLUE>Ink 1: <FGCOLOR=GREEN>" + w-i.i-dscr.

     PUT v-line-1 SKIP v-line-2 v-line-3 SKIP v-line-4 v-line-5 SKIP v-line-6 SKIP v-line-7 v-line-8 SKIP
         v-line-9 SKIP.

     ASSIGN v-tmp-lines   = 0
            j             = 0
            K             = 0
            lv-got-return = 0
            v-dept-inst   = "".

     {custom/notespr2.i job v-inst2 5 "notes.rec_key = job.rec_key and
      (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)" }

     DO i = 1 TO 5:
        ASSIGN v-dept-inst[i] = SUBSTRING(v-inst2[i],1,47).
     END.

     ASSIGN
        v-line-1 = "<R19><C2>" + v-dept-inst[1]
        v-line-2 = "<R20><C2>" + v-dept-inst[2]
        v-line-3 = "<R21><C2>" + v-dept-inst[3]
        v-line-4 = "<R22><C2>" + v-dept-inst[4]
        v-line-5 = "<R23><C2>" + v-dept-inst[5].

     PUT v-line-1 SKIP v-line-2 SKIP v-line-3 SKIP v-line-4 SKIP v-line-5 SKIP.

     /*IF v-ship <> "" THEN v-dept-inst[6] = v-ship. /* shipto notes */ */

     FIND NEXT w-i.

     v-line-1 = "<#6><R20><C50><FGCOLOR=BLUE>Ink 2: <FGCOLOR=GREEN>" + w-i.i-dscr.

     FIND NEXT w-i.

     ASSIGN
        v-line-2 = "<#6><R21><C50><FGCOLOR=BLUE>Ink 3: <FGCOLOR=GREEN>" + w-i.i-dscr
        v-line-3 = "<C50><R22><FGCOLOR=BLUE>Color Desc: <FGCOLOR=GREEN>" + (IF AVAIL xeb THEN xeb.i-coldscr ELSE "")
        v-line-4 = "<C50><R23>" + v-i-line[4].

     PUT v-line-1 SKIP v-line-2 SKIP v-line-3 SKIP v-line-4 SKIP.
     
     ASSIGN
        v-line-1 = "<#8><FGCOLOR=BLUE><R26><C2>Vendor<C15>Date<C28>Sheets Received<C54>Units<C65>Complete"
        v-line-2 = "<#8><R27><C2>" + FILL('_',10) + "<C13>" + FILL('_',10)
                 + "<C24>" + FILL('_',25) + "<C50>" + FILL('_',13)
                 + "<C64>" + FILL('_',15)
        v-line-3 = "<#8><R28.5><C2>" + FILL('_',10) + "<C13>" + FILL('_',10)
                 + "<C24>" + FILL('_',25) + "<C50>" + FILL('_',13)
                 + "<C64>" + FILL('_',15)
        v-line-4 = "<#8><R30><C2>" + FILL('_',61) + "<C65>Partial "
        v-line-5 = "<#8><C14><R31>" + "Totals" + "<C24>" + FILL('_',25)
                 + "<C50>" + FILL('_',13) + "<C64>" + FILL('_',15)
        v-line-6 = "<#9><R34.5><C2>Machine<C12>Pallets<C25>Qty/Pallet<C38>Partial<C50>Total<C61>Date<C70>Operator".

     PUT v-line-1 SKIP v-line-2 SKIP v-line-3 SKIP v-line-4 SKIP v-line-5 SKIP v-line-6 SKIP.

     ASSIGN
        v-mach-var = 0
        v-mach = "".

     FOR EACH w-m,
         FIRST mach FIELDS(m-code) WHERE
               mach.company EQ cocode AND
               mach.loc     EQ locode AND
               mach.m-dscr  EQ w-m.dscr
               NO-LOCK:

         IF v-mach-var GT 4 THEN
            LEAVE.

         ASSIGN
            v-mach-var = v-mach-var + 1
            v-mach[v-mach-var] = mach.m-code.
     END.

     ASSIGN
        v-line-1 = "<#9><R36><C2>" + v-mach[1] + "<C11>" + FILL('_',10) + "<C22>" + FILL('_',15)
                 + "<C38>" + FILL('_',8) + "<C47>" + FILL('_',10)
                 + "<C58>" + FILL('_',10) + "<C69>" + FILL('_',10)
        v-line-2 = "<#9><R37><C2>" + v-mach[2] + "<C11>" + FILL('_',10) + "<C22>" + FILL('_',15)
                 + "<C38>" + FILL('_',8) + "<C47>" + FILL('_',10)
                 + "<C58>" + FILL('_',10) + "<C69>" + FILL('_',10)
        v-line-3 = "<#9><R38><C2>" + v-mach[3] + "<C11>" + FILL('_',10) + "<C22>" + FILL('_',15)
                 + "<C38>" + FILL('_',8) + "<C47>" + FILL('_',10)
                 + "<C58>" + FILL('_',10) + "<C69>" + FILL('_',10)
        v-line-4 = "<#9><R39><C2>" + v-mach[4] + "<C11>" + FILL('_',10) + "<C22>" + FILL('_',15)
                 + "<C38>" + FILL('_',8) + "<C47>" + FILL('_',10)
                 + "<C58>" + FILL('_',10) + "<C69>" + FILL('_',10).

     PUT v-line-1 SKIP v-line-2 SKIP v-line-3 SKIP v-line-4 SKIP.

     DEF VAR v-score-type AS cha NO-UNDO.

     ASSIGN
        j         = 0
        v-xg-flag = (xef.xgrain EQ "S" AND xef.est-type GE 5) OR
                     xef.xgrain EQ "B".
        v-len-score2 = "".

     IF v-len-score <> "" THEN DO:

        v-tmp-score = " ".
       
        DO i = 2 TO LENGTH(v-len-score):
       
          IF (SUBSTR(v-len-score,i,1) = "" AND
              SUBSTR(v-len-score,i - 1,1) <> "") OR
              i = LENGTH(v-len-score) THEN
       
             ASSIGN j               = j + 1
                    v-tmp-stype     = IF v-xg-flag 
                                        THEN xeb.k-len-scr-type2[j]
                                        ELSE xeb.k-wid-scr-type2[j]
                    v-len-score2[j] = v-tmp-score + v-tmp-stype
                    v-tmp-score     = "".
          ELSE 
           ASSIGN v-tmp-score = v-tmp-score + " ".
        END.
       
        ASSIGN v-score-type = "Type :".  
       
        DO i = 1 TO 13:
           IF v-len-score2[i] <> "" THEN
              v-score-type = v-score-type + v-len-score2[i] .
        END.
     END.

     ASSIGN v-qty-or-sup = "Qty Received: " + FILL("_",24)
            i = 0.

     FOR EACH w-m: i = i + 1. END.

     IF i LT 3 THEN DO i = i + 1 TO 3:
        CREATE w-m.
        ASSIGN w-m.dseq = 999999999.
     END.

     /* box for route */
     ASSIGN lv-rt-num = i + 3
            i = 0.

     FOR EACH w-m by w-m.dseq: 

       ASSIGN i = i + 1
              v-lines = v-lines + 1.

       FIND FIRST mach WHERE
            mach.company EQ cocode AND
            mach.loc     EQ locode AND
            mach.m-dscr  EQ w-m.dscr
            NO-LOCK NO-ERROR.
     END.

     FIND LAST b-ef USE-INDEX est-qty WHERE
          b-ef.company = est.company AND
          b-ef.est-no = est.est-no
          NO-LOCK NO-ERROR.

     IF AVAIL b-ef AND b-ef.form-no = w-ef.frm THEN  
        FOR EACH w-m:
            CREATE tt-wm.
            BUFFER-COPY w-m TO tt-wm.
        END.

     PUT SKIP(1).

     IF print-box AND AVAIL xest THEN DO:            
        PUT "<C1><R41.5>".
        RUN cec/desprnt3.p (recid(xef),INPUT-OUTPUT v-lines,RECID(xest)).
     END.
     IF NOT LAST(w-ef.frm) THEN do:
         PAGE.     
         PUT SKIP(1).
     END.
     ELSE IF NOT LAST(job-hdr.job-no2) THEN DO:
         PAGE.     
         PUT SKIP(1).
     END.
    END.  /* FOR EACH w-ef */
 
    IF s-prt-set-header AND LAST-OF(job.job-no2) AND est.est-type = 6 
      THEN DO: /* print set header */
        
      
      ASSIGN i = 0.

      FOR EACH bf-eb WHERE bf-eb.company = est.company
                       AND bf-eb.est-no = est.est-no
                       AND bf-eb.form-no > 0 NO-LOCK:
        ASSIGN  i = i + 1.
      END.

      IF i > 1 THEN DO:
        PAGE.
        PUT SKIP(1).
        DEF VAR v-set-qty AS INT NO-UNDO.
           DEF VAR v-ord-qty AS INT NO-UNDO.
           DEF VAR v-over-run AS cha NO-UNDO.
           DEF VAR v-under-run AS cha NO-UNDO.
           DEF VAR v-fg-set AS cha FORM "x(15)" NO-UNDO.
           v-fg-set = job-hdr.i-no.
           v-set-qty = if avail xeb and xeb.est-type eq 6 THEN
                         if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty
                       ELSE 0.
           v-ord-qty = (if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty) /**
                       if est.form-qty le 1 then 1 else v-pqty*/.
           v-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                        IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE "".
           v-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                         IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE "".
           PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
               "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
               "<C60>Our Date: " v-ord-date SKIP
               "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
               "<=1><R+7><C2><From><R+5><C78><RECT><||3>" SKIP
               "<=1><R+7><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
               v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
               v-i-line-set[2] AT 90
               SKIP
               v-cus[2] AT 3 " Job Qty:" trim(string(job-hdr.qty,">>>,>>9"))    format "x(7)"
               " Order Qty:" string(v-ord-qty) format "x(7)"
               v-i-line-set[3] AT 90 SKIP
               v-cus[3] AT 3  " Cust Part #:" lv-part-no 
               v-i-line-set[4] AT 90 SKIP
               v-cus[4]  AT 3 " Overrun:"  format "x(7)"  
               " Underrun:" format "x(7)"  
               "Adders:" v-adders FORM "x(33)" AT 90 SKIP
               "<=1><R+12><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
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


           PUT "<=1><R+13><C2><FROM><R+" + string(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)" SKIP.
           v-tmp-line = v-tmp-line + 13 .

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
                             v-dept-inst[6].
      END.  /* set header printing */
    END. /* est.est-type = 6 */
END.  /* each job */
END.  /* end v-local-loop  */

HIDE ALL NO-PAUSE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
