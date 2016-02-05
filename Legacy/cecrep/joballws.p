/* ------------------------------------------- cecrep/joballws.p 04200903 GDM */
/* Job Ticket Xprint form for Allwest                                         */
/* cecrep/joballws.p  factory ticket  for Allwest - landscape                 */
/* -------------------------------------------------------------------------- */

&SCOPED-DEFINE PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

DEF INPUT PARAM v-FORMAT AS CHAR.

DEF VAR prt-copies       AS INT                            NO-UNDO.
DEF VAR v-start-compress AS CHAR                           NO-UNDO.
DEF VAR v-end-compress   AS CHAR                           NO-UNDO.
DEF VAR k_frac           AS DEC  INIT 6.25                 NO-UNDO.
DEF VAR v-ink-1          AS CHAR FORMAT "X(30)"            NO-UNDO.
DEF VAR v-ink-2          AS CHAR FORMAT "X(30)"            NO-UNDO.
DEF VAR v-ink-3          AS CHAR FORMAT "X(30)"            NO-UNDO.
DEF VAR v-ink-4          AS CHAR FORMAT "X(30)"            NO-UNDO.
DEF VAR v-ink-5          AS CHAR FORMAT "X(30)"            NO-UNDO.
DEF VAR v-dept-note      AS CHAR FORMAT "x(40)"  EXTENT 50 NO-UNDO.
DEF VAR v-spec-note      AS CHAR FORMAT "x(124)" EXTENT 10 NO-UNDO.
DEF VAR v-deptnote       AS CHAR                           NO-UNDO.
DEF VAR v-dept-length    AS DEC                            NO-UNDO.
DEF VAR lv-under-run     AS CHAR                           NO-UNDO.
DEF VAR lv-over-run      AS CHAR                           NO-UNDO.
DEF VAR lv-part-name     AS CHAR FORMAT "x(30)"            NO-UNDO.
DEF VAR lv-fg-name       AS CHAR                           NO-UNDO.
DEF VAR lv-status        AS CHAR FORMAT "x(30)"            NO-UNDO.
DEF VAR lv-sts-code      AS CHAR INIT "O,R,C,T,N,X,Q"      NO-UNDO.
DEF VAR lv-sts-desc      AS CHAR 
 INIT "O-Original,R-Repeat,C-Change,T-Transfer,N-New Customers,X-Complete Re-run,Q-Quality/Re-work" NO-UNDO.

DEF VAR v-sman        AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-blk-per-frm AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-due-code    AS CHAR FORMAT "x(4)"  NO-UNDO.
DEF VAR v-cad-no      AS CHAR                NO-UNDO. 
DEF VAR v-rel-date    AS DATE                NO-UNDO.
DEF VAR v-rel-qty     AS INT                 NO-UNDO.
DEF VAR v-i-code2     AS CHAR FORMAT "X(10)" NO-UNDO.
DEF VAR lv-spattern-img AS cha FORM "x(50)" NO-UNDO.

{jcrep/r-ticket.i "shared"}
{cecrep/joballws.i "new shared"}
{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}

DEF NEW SHARED VAR v-out1-id AS RECID NO-UNDO.
DEF NEW SHARED VAR v-out2-id AS RECID NO-UNDO.
 
DEF VAR laser          AS LOG INIT NO   FORMAT "Y/N"   NO-UNDO.
DEF VAR v-vend-no      LIKE oe-ordl.vend-no            NO-UNDO.
DEF VAR v-po-no        LIKE oe-ordl.po-no-po           NO-UNDO.
DEF VAR v-qty-or-sup   AS char          FORMAT "x(38)" NO-UNDO.
DEF VAR v-i-line       AS char EXTENT 5 FORMAT "x(38)" NO-UNDO.
DEF VAR v-flag         AS log  INIT NO                 NO-UNDO.
DEF VAR v-local-copies AS int                          NO-UNDO.
DEF VAR v-local-loop   AS int  INIT 1                  NO-UNDO.
DEF VAR v-print-score  AS log  INIT YES                NO-UNDO.
DEF VAR v-pqty         AS DEC                          NO-UNDO.
DEF VAR lv-part-no     AS CHAR FORMAT "x(15)"          NO-UNDO.
DEF VAR lv-rt-num      AS INT                          NO-UNDO.

DEF STREAM ctl.

DEF VAR lv-add-entry    AS INT                 NO-UNDO.
DEF VAR v-loop-cnt      AS INT                 NO-UNDO.
DEF VAR v-note-cnt      AS INT                 NO-UNDO.
DEF VAR v-note-length   AS INT                 NO-UNDO.
DEF VAR v-die-loc       AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-plate-loc     AS CHAR FORMAT "x(12)" NO-UNDO.
DEF VAR v-tmp-lines     AS DEC                 NO-UNDO. 
DEF VAR lv-got-return   AS INT                 NO-UNDO. 
DEF VAR v-prev-note-rec AS RECID               NO-UNDO.
DEF VAR v-prev-extent   AS INT                 NO-UNDO.
DEF VAR v-prev-ext-gap  AS INT                 NO-UNDO.
DEF VAR v-coldscr       LIKE eb.i-coldscr      NO-UNDO.

DEF SHARED VAR s-prt-ship-split  AS LOG NO-UNDO.
DEF SHARED VAR s-prt-set-header  AS LOG NO-UNDO.
DEF SHARED VAR s-sample-rEQuired AS LOG NO-UNDO.

DEF BUFFER b-eb FOR eb.

DEF VAR lv-split    AS cha FORMAT "x(60)" EXTENT 4 NO-UNDO.
DEF VAR lv-au       AS cha FORMAT "x(20)"          NO-UNDO.
DEF VAR lv-est-type AS cha FORMAT "x(35)"          NO-UNDO.

DEF BUFFER bf-eb FOR eb.
DEF BUFFER b-ef FOR ef.

DEF VAR v-tmp-line  AS INT                                  NO-UNDO.
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.

DEF WORK-TABLE tt-wm NO-UNDO LIKE w-m.

DEF VAR lv-text                 AS CHAR                        NO-UNDO.
DEF VAR li                      AS INT                         NO-UNDO.
DEF VAR v-dept-inst             AS CHAR FORM "x(80)" EXTENT 15 NO-UNDO.
DEF VAR v-inst2                 AS CHAR EXTENT 6               NO-UNDO.
DEF VAR v-sht-qty-STRING        AS CHAR                        NO-UNDO.
DEF VAR v-w-l                   AS CHAR                        NO-UNDO.
DEF VAR v-form-code1            AS CHAR                        NO-UNDO.
DEF VAR v-sample-rEQuired-text  AS CHAR FORMAT "X(35)"         NO-UNDO.
DEF VAR v-i                     AS INT                         NO-UNDO.

DEF VAR v-samprEQ LIKE reftable.val[2] NO-UNDO.

DEF VAR v-embaflag AS LOG NO-UNDO.

{cecrep/jc-fibre.i }

DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.

{cecrep/jc-prem.i}
{custom/formtext.i NEW}

ASSIGN
 v-line[1] = CHR(95) + FILL(CHR(95),40) + CHR(95) + "  " +
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
 v-qty-or-sup = IF LOOKUP(v-FORMAT,"TriState,RFC,Boxtech,Brick,Corrugat") GT 0
                  THEN ("Supplier:"     + FILL("_",28))
                  ELSE ("Qty Received: " + FILL("_",24)).

ASSIGN v-local-copies = 1
       prt-copies = 1.

DO v-local-loop = 1 TO v-local-copies:
  
  FOR EACH job-hdr 
    WHERE job-hdr.company               EQ cocode
      AND job-hdr.ftick-prnt            EQ reprint
      AND job-hdr.job-no                GE SUBSTR(fjob-no,1,6)
      AND job-hdr.job-no                LE SUBSTR(tjob-no,1,6)
      AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) + TRIM(job-hdr.job-no) +
          STRING(job-hdr.job-no2,"99")  GE fjob-no
      AND FILL(" ",6 - length(trim(job-hdr.job-no))) + TRIM(job-hdr.job-no) +
          STRING(job-hdr.job-no2,"99")  LE tjob-no,

    FIRST job NO-LOCK
    WHERE job.company EQ cocode
      AND job.job     EQ job-hdr.job
      AND job.job-no  EQ job-hdr.job-no
      AND job.job-no2 EQ job-hdr.job-no2
      AND job.stat    NE "H" ,

    FIRST est NO-LOCK
    WHERE est.company  EQ job.company
      AND est.est-no   EQ job.est-no
      AND est.est-type GT 4,

    FIRST cust NO-LOCK
     WHERE cust.company EQ cocode
      AND cust.cust-no  EQ job-hdr.cust-no,

    FIRST itemfg NO-LOCK
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ job-hdr.i-no
    BREAK BY job.job-no
          BY job.job-no2:


     ASSIGN v-break = FIRST-OF(job.job-no2).
      
     RELEASE xest.
     RELEASE xef.
     RELEASE xeb.
     RELEASE xoe-ord.
     RELEASE xoe-ordl.
     RELEASE xoe-rel.

     RUN cecrep/jobtick1.p (RECID(job-hdr), v-FORMAT,
                            v-local-loop, v-local-copies).

     FOR EACH w-ef 
       WHERE (w-ef.frm = job-hdr.frm OR est.est-type <> 8),
        EACH b-eb NO-LOCK 
       WHERE b-eb.company = job-hdr.company
         AND b-eb.est-no = job-hdr.est-no 
         AND b-eb.form-no = w-ef.frm
         AND (b-eb.blank-no = job-hdr.blank-no OR est.est-type NE 8)
        BREAK BY w-ef.frm 
              BY b-eb.blank-no:

         RELEASE xef.
         RELEASE xeb.
         RELEASE xstyle.
         RELEASE xxprep.
         
         RUN cecrep/jobtick4.p (RECID(w-ef), 
                                RECID(job-hdr),
                                RECID(b-eb)). /* jobtick2.p*/

         ASSIGN 
           v-pqty = 1 v-cp = "" v-sman = "".

         IF AVAIL xeb THEN DO:

           IF xeb.stock-no NE "" THEN ASSIGN v-fg = xeb.stock-no.

           ASSIGN v-cp = xeb.part-no.

           ASSIGN lv-fg-name = itemfg.i-name.

           {cec/rollfac.i}

           ASSIGN 
             v-pqty = IF v-rollfac OR 
                         xeb.est-type EQ 8 THEN
                         1 
                      ELSE IF xeb.yld-qty LT 0 THEN
                           (-1 / xeb.yld-qty)
                      ELSE xeb.yld-qty.

           FIND FIRST sman 
             WHERE sman.company = xeb.company 
               AND sman.sman = xeb.sman NO-LOCK NO-ERROR.

           ASSIGN v-sman = IF AVAIL sman 
                             THEN sman.sNAME ELSE xeb.sman.

         END. /* IF avail xeb*/ 
         
         ASSIGN
            v-loc     = ""
            v-loc-bin = "".

         IF v-FORMAT EQ "Brick" OR 
            v-FORMAT EQ "Corrugat" THEN
           DO: 

           ASSIGN v-iso = "ISO# CS-05-1-F".

           RELEASE fg-rdtlh.          

           FIND FIRST fg-bin
             WHERE fg-bin.company EQ cocode
               AND fg-bin.i-no    EQ job-hdr.i-no
               AND fg-bin.job-no  EQ job-hdr.job-no
               AND fg-bin.job-no2 EQ job-hdr.job-no2
               AND fg-bin.loc     EQ job-hdr.loc
               AND fg-bin.qty     NE 0 NO-LOCK NO-ERROR.
           IF AVAIL fg-bin THEN DO:
              ASSIGN
                 v-loc     = "Whs: " + fg-bin.loc
                 v-loc-bin = "Bin: " + fg-bin.loc-bin.
           END.
           ELSE
           IF AVAIL itemfg THEN DO:
            ASSIGN
              v-loc     = "Whs: " + itemfg.def-loc
              v-loc-bin = "Bin: " + itemfg.def-loc-bin.
           END.
         END. /*brick FORMAT*/

         ASSIGN 
          lv-over-run   = IF AVAIL xoe-ordl THEN
                             TRIM(STRING(xoe-ordl.over-pct,">>9.99%")) 
                          ELSE IF AVAIL xoe-ord THEN
                               TRIM(STRING(xoe-ord.over-pct,">>9.99%"))
                          ELSE ""
          lv-under-run  = IF AVAIL xoe-ordl THEN
                             TRIM(STRING(xoe-ordl.under-pct,">>9.99%")) 
                          ELSE IF AVAIL xoe-ord THEN
                             TRIM(STRING(xoe-ord.under-pct,">>9.99%")) 
                          ELSE ""
          lv-part-name  = xeb.part-dscr1   
          v-blk-per-frm = "  Part " + 
                          STRING(xeb.form-no,"99") + " of " + 
                          STRING(xest.form-qty,"99")     
          v-due-code    = IF AVAIL xoe-ord 
                            THEN xoe-ord.due-code ELSE "ON"
          lv-au         = IF itemfg.alloc THEN "U" ELSE "A".

          IF est.est-type = 6 THEN
             lv-au = lv-au + "  " + job-hdr.i-no.
          ELSE
             lv-au = "".

         ASSIGN 
           lv-est-type = IF est.est-type = 5 THEN
                            "SINGLE"
                         ELSE IF est.est-type = 6 THEN
                            "SET"
                         ELSE
                            "COMBO"
           lv-est-type = lv-est-type + "  FORM " + 
                          STRING(b-eb.form-no) + " OF " + STRING(xest.form-qty)
                          + "  BLANK " + STRING(b-eb.blank-no) + " OF " + 
                          STRING(xef.blank-qty). 

         ASSIGN 
           v-cad-no = IF AVAIL xeb THEN xeb.cad-no ELSE ""
           v-rel-date = IF AVAIL xoe-rel THEN xoe-rel.rel-date ELSE ?
           v-rel-qty = IF AVAIL xoe-rel THEN xoe-rel.tot-qty ELSE 0.

         FIND FIRST reftable NO-LOCK WHERE reftable.reftable EQ 'stackpat'
                                       AND reftable.company EQ ''
                                       AND reftable.loc EQ ''
                                       AND reftable.code EQ b-eb.stack-code NO-ERROR.
         IF AVAILABLE reftable AND SEARCH(reftable.dscr) NE ? THEN lv-spattern-img =  reftable.dscr.

         PUT    
            "</PROGRESS><P12><B>"
            "JOB TICKET" AT 5  " " 
            lv-au  CAPS(SUBSTR(v-fg,1,1)) FORMAT "x" AT 40
            "Production Specification" AT 45 
            "</B><C72>" lv-est-type "</B><P10>"     SKIP

            "<#1><C1><FROM><C105><R+45><RECT><|3>" 
            "<=1><C32><FROM><R+45><C32><LINE><|3>"
            "<=1><C66><FROM><R+45><C66><LINE><|3>"
            "<=1><C90><FROM><R+4><C90><LINE><|3>"
            "<=1><R+4><C1><FROM><C105><LINE><|3>"     /*sold to*/
            "<=#1><R+11><C1><FROM><C105><LINE><|3>"   /*board*/
            "<=1><R+19><C1><FROM><C105><LINE><|3>"    /*shipping info*/
            "<=1><R+19><C33><P8>                 <U>Dept Notes:</U> <C80><U>More Dept Notes:</U>"
            "<=1><R+26><C1><FROM><C32><LINE><|3>"   /*packing*/
            "<=1><R+35><C1><FROM><C32><LINE><|3>"   /*machine routing*/
            "<=1><R+35><C24><FROM><R+10><LINE><|3>"    
            "<=1><R+37><C66><FROM><C105><LINE><|3>"   /*sheets ordered*/
            "<=1><R+37><C71><P8><U>Sheets Ordered:</U>          <U>Due Date:</U>      <U>Supplier:</U>"
            "<=1><R+37><C86><FROM><R+8><LINE><|3>"   
            "<=1><R+37><C96><FROM><R+8><LINE><|3>"   
            "<=1><R+26.2><C23><#22><R+5><C+9.7><IMAGE#22=" lv-spattern-img + ">" FORM "x(70)"  /*Stack pattern image*/ 
            .

         VIEW FRAME head.  /* factory header display  */  

          
         IF v-FORMAT EQ "RFC" OR 
            v-FORMAT EQ "Boxtech" THEN
            ASSIGN
              v-i-line[1] = itemfg.i-name     
              v-i-line[2] = itemfg.part-dscr1 
              v-i-line[3] = itemfg.part-dscr2 
              v-i-line[4] = itemfg.part-dscr3.
           ELSE
            ASSIGN
              v-i-line[1] = "ITEM DESCRIPTION"
              v-i-line[2] = "Style: " + IF AVAIL xstyle 
                                          THEN xstyle.dscr ELSE ""
              v-i-line[3] = "Size: "  + IF AVAIL xeb    
                                          THEN
                                           TRIM(STRING({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                                           TRIM(STRING({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                                           TRIM(STRING({sys/inc/k16v.i xeb.dep},">,>>9.99")) 
                                          ELSE ""
              v-i-line[4] = "Joint: " + IF AVAIL xeb THEN v-joint-dscr ELSE ""
              v-i-line[5] = "ORDER TYPE:" + lv-status.


         ASSIGN lv-part-no = IF AVAIL xoe-ordl 
                              THEN xoe-ordl.part-no  ELSE itemfg.part-no.

         RUN sys/ref/getpo#.p (IF AVAIL xoe-ordl AND 
                                 est.est-type <> 6 
                                THEN 
                                 ROWID(xoe-ordl) ELSE ROWID(job),
                               w-ef.frm, OUTPUT v-po-no).

         RELEASE po-ord.

         IF v-po-no NE 0 THEN
            FIND FIRST po-ord WHERE
                 po-ord.company EQ cocode AND
                 po-ord.po-no   EQ v-po-no
                 NO-LOCK NO-ERROR.

         ASSIGN
           v-vend-no    = IF AVAIL po-ord THEN po-ord.vend-no ELSE ""
           v-qty-or-sup = "Supplier: ".

         IF v-vend-no NE "" THEN DO:            
            ASSIGN v-qty-or-sup = v-qty-or-sup + TRIM(v-vend-no).
            IF v-po-no ne 0 THEN
               v-qty-or-sup = v-qty-or-sup + " PO#:" +
                              TRIM(STRING(v-po-no,">>>>>>>>>>")).
         END.
         
         ASSIGN i = 0.
         FOR EACH w-i: 
             i = i + 1.
         END.

         IF i LE 4 THEN 
            DO i = i + 1 to 5:
               CREATE w-i.
            END.

         ASSIGN
           v-ink-1 = ""
           v-ink-2 = ""
           v-ink-3 = ""
           v-ink-4 = ""
           v-ink-5 = "".

         DO v-i = 1 TO 5:
           IF b-eb.i-code[v-i] <> "" THEN DO:

             FIND FIRST w-i WHERE w-i.i-code = b-eb.i-code[v-i] NO-ERROR.

             IF AVAIL w-i THEN DO:
                IF v-ink-1 = "" THEN 
                   v-ink-1 = w-i.i-dscr +
                             (IF w-i.i-qty <> 0 THEN
                                 STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                             IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                ELSE IF v-ink-2 = "" THEN
                   v-ink-2 = w-i.i-dscr +
                            (IF w-i.i-qty <> 0 THEN
                                STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                             IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                ELSE IF v-ink-3 = "" THEN
                   v-ink-3 = w-i.i-dscr +
                             (IF w-i.i-qty <> 0 THEN
                                 STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                             IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                ELSE IF v-ink-4 = "" THEN
                   v-ink-4 = w-i.i-dscr +
                           (IF w-i.i-qty <> 0 THEN
                               STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                            IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                ELSE IF v-ink-5 = "" THEN 
                   v-ink-5 = w-i.i-dscr +
                             (IF w-i.i-qty <> 0 THEN
                                 STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                             IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
             END. /* IF AVAIL w-i */
             ELSE
             DO:
                FIND first ITEM WHERE
                     item.company  eq cocode AND
                     item.i-no     eq b-eb.i-code[v-i] AND
                     item.mat-type eq "I"
                     NO-LOCK NO-ERROR.

                IF AVAIL ITEM THEN
                DO:
                   IF v-ink-1 = "" THEN 
                      v-ink-1 = ITEM.i-name.
                   ELSE IF v-ink-2 = "" THEN
                      v-ink-2 = ITEM.i-name.
                   ELSE IF v-ink-3 = "" THEN
                      v-ink-3 = ITEM.i-name.
                   ELSE IF v-ink-4 = "" THEN
                      v-ink-4 = ITEM.i-name.
                   ELSE IF v-ink-5 = "" THEN 
                      v-ink-5 = ITEM.i-name.
                END.
             END.
           END. /* IF b-eb.i-code[v-i] */
         END. /* DO v-i */

         ASSIGN
           v-form-sqft = ROUND(IF v-corr 
                                 THEN (v-form-len * v-form-wid * .007)
                                 ELSE (v-form-len * v-form-wid / 144),3).

         FIND FIRST xxprep 
            WHERE xxprep.company EQ cocode
              AND xxprep.code EQ xeb.die-no NO-LOCK NO-ERROR.

         ASSIGN
            v-die-loc = IF AVAIL xxprep 
                         THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

         FIND FIRST xxprep 
           WHERE xxprep.company EQ cocode
             AND xxprep.code EQ xeb.plate-no NO-LOCK NO-ERROR.

         ASSIGN
           v-plate-loc = IF AVAIL xxprep 
                           THEN xxprep.loc + " " + xxprep.loc-bin ELSE ""
           lv-status   = IF AVAIL xoe-ord 
                           THEN ENTRY(LOOKUP(xoe-ord.TYPE,lv-sts-code),lv-sts-desc) 
                           ELSE ""
           v-coldscr = IF AVAIL xeb AND 
                          xeb.i-coldscr <> "" 
                         THEN xeb.i-coldscr ELSE "Plain".

         DEF VAR v-see-1st-blank AS LOG NO-UNDO.
         
         ASSIGN
           v-see-1st-blank = IF NOT FIRST-OF(w-ef.frm) OR
                                CAN-FIND(FIRST bf-eb 
                                          WHERE bf-eb.company = b-eb.company
                                            AND bf-eb.est-no = b-eb.est-no
                                            AND bf-eb.form-no = b-eb.form-no
                                            AND bf-eb.blank-no < b-eb.blank-no)
                               THEN YES ELSE NO.

         DISP  
              "<B><p12>" v-cus[1] /*AT 2 */ 
              "<B><P12>Cust Set/Box#:"          AT 42 
              lv-part-no "</B><P10>"   
              "Style:"                          AT 93 
              xstyle.dscr WHEN AVAIL xstyle              
            SKIP
              v-cus[2]                          AT 2  
              "<B><P12>Job Qty:"                AT 40 
              TRIM(STRING(job-hdr.qty * v-pqty,">>>,>>9")) FORMAT "x(7)"
              "</B><P10>"
              "Order Qty:" 
              TRIM(STRING((IF AVAIL xoe-ordl THEN xoe-ordl.qty
                             ELSE job-hdr.qty) *
                           IF est.form-qty LE 1 
                             THEN 1
                             ELSE v-pqty,">>>,>>9"))       FORMAT "x(7)" 
              "Size:" 
              (TRIM(STRING({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
               TRIM(STRING({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
               TRIM(STRING({sys/inc/k16v.i xeb.dep},">,>>9.99"))) 
                                                           FORMAT "x(30)" WHEN AVAIL xeb
            SKIP
              v-cus[3]   AT 2
              "Set Qty:" AT 40 
              TRIM(STRING(IF AVAIL xoe-ordl 
                            THEN xoe-ordl.qty
                            ELSE job-hdr.qty,">>>,>>9")) 
                 WHEN AVAIL xeb AND xeb.est-type EQ 6      FORMAT "x(9)" 
              "Joint:" AT 80 v-joint-dscr             
            SKIP
              v-cus[4]          AT 2
              "Item Name: "     AT 40 lv-fg-name    FORMAT "x(26)"
              "PO #:"           AT 2  xoe-ord.po-no WHEN AVAIL xoe-ord 
              "Part item name:" AT 40 SUBSTR(lv-part-name,1,22) FORMAT "x(25)"
              "Status:"         AT 2  lv-status SUBSTR(lv-part-name,23,30) AT 57
              "<P8>"
            SKIP
              "<U>Board:</U>"   AT 15 "   <U>Printing:</U>" AT 70 " <U>Die Cutting, Slit, & Saw</U>" AT 130 "<P10>"SKIP
              "Shts REQ'd:"     AT 2 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank 
              TRIM(STRING(v-sht-qty)) FORMAT "x(9)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank  @ v-sht-qty-STRING 
              "" WHEN v-see-1st-blank @ v-sht-qty-STRING
              " Sq Ft:" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank 
               TRIM(STRING(v-form-sqft)) WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank  FORMAT "x(7)"
              "PLATE #:" AT 40 xeb.plate-no FORMAT "X(11)" WHEN AVAIL xeb " Loc:" v-plate-loc FORMAT "X(11)"
              "Die #" AT 80 xeb.die-no WHEN AVAIL xeb " Loc:" v-die-loc 
            SKIP
              "W:" + TRIM(STRING({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
              " " +
              "L:" + TRIM(STRING({sys/inc/k16v.i v-form-len},">,>>9.99"))  WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank FORMAT "x(22)"  @ v-w-l AT 2
              "SEE 1st BLANK" WHEN v-see-1st-blank @ v-w-l   
              "MSF:"  + TRIM(STRING(v-sht-qty * v-form-sqft / 1000,">>>9.9<")) FORMAT "x(11)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank              "Ink 1:" AT 39 v-ink-1
              "Gross Size:" AT 80  
              "W:" + TRIM(STRING({sys/inc/k16v.i xef.gsh-wid},">>>9.99")) +
              " " +
              "L:" + TRIM(STRING({sys/inc/k16v.i xef.gsh-len},">>>9.99"))
                                                                FORMAT "x(19)"
              "Slit: W:" + STRING(v-outw) + " L:" + STRING(v-outl) FORM "x(15)"   
              "<B><P12>"
              SKIP
            WITH FRAME job1 NO-LABEL NO-BOX WIDTH 200 STREAM-IO.                 

         DISP
              "Board:" + v-form-code  FORM "x(29)"  @ v-form-code1 AT 2
              " " WHEN v-see-1st-blank @ v-form-code1 
              "</B><P10> Ink 2:"  v-ink-2 "Net   Size:"  AT 83
              "W:" + TRIM(STRING({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) +
              " " +
              "L:" + TRIM(STRING({sys/inc/k16v.i xef.nsh-len},">>>9.99")) FORMAT "x(22)"
              "<B><P12>" SKIP
              v-form-dscr  AT 2 FORM "x(29)" 
              "</B><P10> Ink 3:"  v-ink-3 "Die   Size:" AT 83
              "W:" + TRIM(STRING({sys/inc/k16v.i xef.TRIM-w},">>>9.99")) +
              " " +
              "L:" + TRIM(STRING({sys/inc/k16v.i xef.TRIM-l},">>>9.99")) FORMAT "x(21)"
              "Up:" "W:" + STRING(v-upl) + " L:" + STRING(v-upw) FORM "x(9)"
              SKIP
               v-qty-or-sup AT 2 FORM "x(36)" 
              "Ink 4:" AT 39 v-ink-4 "Blank Size:" AT 80 
              "W:" + TRIM(STRING({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
              " " +
              "L:" + TRIM(STRING({sys/inc/k16v.i xeb.t-len},">>>9.99")) FORMAT "x(22)"
              SKIP
              "Score:" AT 2 SUBSTR(v-len-score,1,30) WHEN xstyle.TYPE <> "F" FORMAT "x(30)" 
              "Ink 5:" AT 39 v-ink-5
              "Impressions:" AT 80 TRIM(STRING(v-dc-qty))    FORMAT "x(7)"
              SKIP
              "Adders:"         AT 2 v-adders      FORMAT "x(23)"
              "Color Desc:" AT 39 v-coldscr
              "D/C Style:" AT 80 SKIP
              WITH FRAME job1 NO-LABEL NO-BOX WIDTH 200 STREAM-IO.                
        
          DISPLAY "<P8><U>Shipping Info: <P10></U>" AT 12 SKIP
              "Ship To #:" AT 2
              xoe-ord.sold-id WHEN AVAIL xoe-ord
              xeb.ship-id WHEN AVAIL xeb @ xoe-ord.sold-id
              xoe-rel.ship-id WHEN AVAIL xoe-rel @ xoe-ord.sold-id SKIP
              v-shp[1] AT 2  SKIP
              v-shp[2] AT 2  SKIP
              v-shp[3] AT 2  SKIP
              v-shp[4] AT 2 SKIP
              "Item PO #:" AT 2  xoe-ordl.po-no WHEN AVAIL xoe-ordl
             WITH NO-BOX NO-LABEL FRAME shp-info STREAM-IO. 

         RUN cecrep/jobalws2.p (RECID(job-hdr),v-FORMAT,cust.terms).
         
         ASSIGN i = 0.
         FOR EACH w-m:
          ASSIGN i = i + 1.
         END.

         IF i LT 3 THEN 
           DO i = i + 1 to 3:

           CREATE w-m.
           w-m.dsEQ = 999999999.
         END.


        /* box for route */       
        ASSIGN lv-rt-num = i + 3.        
        PUT SKIP(3)
          "<P8>        <U>Machine Routing:</U>              <U>Sheets</U>"
/*          SU:      Start       Stop        Total          RUN:     Hours      Start       Stop       Total       QTY:  In        Out         Waste */ 
          "<P10>"  
         SKIP.

        ASSIGN v-embaflag = NO
               i = 0.
        FOR EACH w-m BY w-m.dsEQ:
          ASSIGN i = i + 1
                 v-letter = SUBSTR("UTE",i,1).

          DISPLAY 
            w-m.dscr AT 3   "<P8>  <U>Received:</U><P10>" WHEN i = 1 AT 29
           WITH NO-BOX NO-LABELS FRAME oo1 
              WIDTH 150 NO-ATTR-SPACE DOWN STREAM-IO.                 

          ASSIGN v-lines = v-lines + 1.

          /* gdm - 05270908 */
          IF (w-m.m-code = "EMBA"  OR
              w-m.m-code = "14800" OR
              w-m.m-code = "14801" OR
              w-m.m-code = "14802") 
            THEN ASSIGN v-embaflag = YES.          

        END.

        FOR EACH tt-formtext:
            DELETE tt-formtext.
        END.
        

        ASSIGN lv-text = "".          
        FOR EACH notes 
          WHERE notes.rec_key = job.rec_key 
            AND (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0),
         FIRST dept NO-LOCK 
          WHERE dept.code = notes.note_code BY notes.note_code:

         ASSIGN lv-text = lv-text + "<B>" + 
                          CAPS(dept.dscr) + ":</B>" + CHR(10) + "  " + 
                          notes.note_text + CHR(10).          
        END.


        DO li = 1 TO 42:
          CREATE tt-formtext.
          ASSIGN tt-line-no = li
                 tt-length  = 39. 
        END.

        RUN custom/formtext.p (lv-text).

        ASSIGN i = 0 v-inst2 = "".

        FOR EACH tt-formtext:
          i = i + 1.
          IF  i <= 42 THEN v-dept-note[i] = tt-formtext.tt-text.      
        END.

        ASSIGN 
          v-inst = "".        
          v-dept-note[42] = "CSR: " + IF AVAIL xoe-ord 
                                        THEN xoe-ord.user-id ELSE "".
        
        IF s-sample-rEQuired = TRUE THEN DO:
            FIND FIRST reftable WHERE reftable.reftable = "cecrep/d-allwest.w"
                                  AND reftable.company  = cocode
                                  AND reftable.loc      = TRIM(job-hdr.job-no)
                                  AND reftable.code     = STRING(job-hdr.job-no2,"9999999999")
                                  AND reftable.code2    = STRING(w-ef.frm,"999")  /*form-no*/
                                  AND reftable.val[1]   = b-eb.blank-no NO-ERROR. /*blank-no*/
            IF AVAILABLE reftable THEN
               v-sample-rEQuired-text = "        " + STRING(reftable.val[2]) + " SAMPLE(S) REQUIRED".
            ELSE
               v-sample-rEQuired-text = "".
        END.

        IF s-prt-ship-split 
          THEN
           FIND FIRST tt-fibre NO-LOCK 
             WHERE tt-fibre.tt-job-no  EQ job-hdr.job-no
               AND tt-fibre.tt-job-no2 EQ job-hdr.job-no2
               AND tt-fibre.tt-frm     EQ w-ef.frm
               AND tt-fibre.tt-blank   EQ b-eb.blank-no NO-ERROR.
           IF AVAIL tt-fibre 
             THEN
              ASSIGN 
                lv-split[1] = "LOC1 <U>" + STRING(tt-fibre.tt-sqty1,">>>,>>9") + "</U>     |" + "   ORDER <U>" + STRING(tt-fibre.tt-order1,">>>>>9") + "</U>   QTY <U>" + STRING(tt-fibre.tt-oqty1,">>>,>>9") + "</U>"
                lv-split[2] = "LOC2 <U>" + STRING(tt-fibre.tt-sqty2,">>>,>>9") + "</U>     |" + "   ORDER <U>" + STRING(tt-fibre.tt-order2,">>>>>9") + "</U>   QTY <U>" + STRING(tt-fibre.tt-oqty2,">>>,>>9") + "</U>"
                lv-split[3] = "LOC3 <U>" + STRING(tt-fibre.tt-sqty3,">>>,>>9") + "</U>     |" + "   ORDER <U>" + STRING(tt-fibre.tt-order3,">>>>>9") + "</U>   QTY <U>" + STRING(tt-fibre.tt-oqty3,">>>,>>9") + "</U>"
                lv-split[4] = "LOC4 <U>" + STRING(tt-fibre.tt-sqty4,">>>,>>9") + "</U>     |" + "   ORDER <U>" + STRING(tt-fibre.tt-order4,">>>>>9") + "</U>   QTY <U>" + STRING(tt-fibre.tt-oqty4,">>>,>>9") + "</U>". 

        PUT "<=1><R+20><C33>" v-dept-note[1] "  " v-dept-note[21]
            "<=1><R+21><C33>" v-dept-note[2] "  " v-dept-note[22]
            "<=1><R+22><C33>" v-dept-note[3] "  " v-dept-note[23]
            "<=1><R+23><C33>" v-dept-note[4] "  " v-dept-note[24]
            "<=1><R+24><C33>" v-dept-note[5] "  " v-dept-note[25]
            "<=1><R+25><C33>" v-dept-note[6] "  " v-dept-note[26]
            "<=1><R+26><C33>" v-dept-note[7] "  " v-dept-note[27]
            "<=1><R+27><C33>" v-dept-note[8] "  " v-dept-note[28]
            .
        IF s-sample-rEQuired EQ FALSE 
          THEN
           PUT "<=1><R+28><C33>" v-dept-note[9] "  " v-dept-note[29]
               "<=1><R+29><C33>" v-dept-note[10] "  " v-dept-note[30]
               "<=1><R+30><C33>" v-dept-note[11] "  " v-dept-note[31]
               "<=1><R+31><C33>" v-dept-note[12] "  " v-dept-note[42]
               "<=1><R+32><C33>" v-dept-note[13] "  "/*v-dept-note[32]*/
               "<=1><R+33><C33>" v-dept-note[14] "  " /*v-dept-note[33]*/
               "<=1><R+34><C33>" v-dept-note[15] "  " /*v-dept-note[34]*/
               "<=1><R+35><C33>" v-dept-note[16] "  " /*v-dept-note[35]*/
               "<=1><R+36><C33>" v-dept-note[17] "  " /*v-dept-note[36]*/
                                                     /* upto here*/         
               "<=1><R+37><C33>" v-dept-note[18] "  " 
               "<=1><R+38><C33>" v-dept-note[19] "  "
               "<=1><R+39><C33>" v-dept-note[20] "  "
               "<=1><R+40><C33>" v-dept-note[21] 
               "<=1><R+41><C33>" v-dept-note[22] 
               "<=1><R+42><C33>" v-dept-note[23] 
               "<=1><R+43><C33>" v-dept-note[24] 
               "<=1><R+44><C33>" v-dept-note[25]
               .
          ELSE
           PUT
               "<=1><R+28><C33>" v-dept-note[9] "  " "<=1><R+28><C66><FROM><C105><LINE><||3>"
               "<=1><R+29><C33>" v-dept-note[10] "  " "<B><P12>" v-sample-rEQuired-text "</B><P10>" 
               "<=1><R+30><C33>" v-dept-note[11] "  " "<=1><R+30><C66><FROM><C105><LINE><||3>"
               "<=1><R+31><C33>" v-dept-note[12] "  " v-dept-note[42]
               "<=1><R+32><C33>" v-dept-note[13] "  "/*v-dept-note[32]*/
               "<=1><R+33><C33>" v-dept-note[14] "  " /*v-dept-note[33]*/
               "<=1><R+34><C33>" v-dept-note[15] "  " /*v-dept-note[34]*/
               "<=1><R+35><C33>" v-dept-note[16] "  " /*v-dept-note[35]*/
               "<=1><R+36><C33>" v-dept-note[17] "  " /*v-dept-note[36]*/
                                                     /* upto here*/         
               "<=1><R+37><C33>" v-dept-note[18] "  " 
               "<=1><R+38><C33>" v-dept-note[19] "  "
               "<=1><R+39><C33>" v-dept-note[20] "  "
               "<=1><R+40><C33>" v-dept-note[21] 
               "<=1><R+41><C33>" v-dept-note[22] 
               "<=1><R+42><C33>" v-dept-note[23] 
               "<=1><R+43><C33>" v-dept-note[24] 
               "<=1><R+44><C33>" v-dept-note[25]      
              .

        IF s-prt-ship-split 
          THEN
           PUT 
            "<=1><R+32><C66><FROM><C105><LINE><||3>"
            "<=1><R+32><C66>" "SPLIT SHIP/QTY   |         SPLIT ORDER" FORM "x(39)"            
            "<=1><R+33><C66>" lv-split[1] FORM "x(70)"
            "<=1><R+34><C66>" lv-split[2] FORM "x(70)"
            "<=1><R+35><C66>" lv-split[3] FORM "x(70)"
            "<=1><R+36><C66>" lv-split[4] FORM "x(70)" .
        PAGE.


        PUT "<#11><C1><FROM><C105><R+47><RECT><|3>"  
            "<=11><C30><FROM><R+4><C30><LINE><|3>"
            "<=11><C60><FROM><R+4><C60><LINE><|3>"
            "<=11><R+4><C1><FROM><C105><LINE><|3>"
            "<=11>Job # <C30> Estimate # <C60> Cust Part #"  SKIP
            "<P12><C12>" v-job-prt 
            "<C40>" v-est-no
            "<C70>" lv-part-no SKIP(4)
            .

        IF print-box AND AVAIL xest THEN DO:            
          
            FOR EACH tt-formtext:
                DELETE tt-formtext.
            END.

            ASSIGN lv-text = "".

            FOR EACH notes 
              WHERE notes.rec_key = job.rec_key
                AND (notes.note_form_no = w-ef.frm OR 
                     notes.note_form_no = 0)
                AND notes.note_code = "BN" NO-LOCK:

              ASSIGN lv-text = lv-text + " " + 
                               TRIM(notes.note_text) + CHR(10).
            END.            

            IF v-embaflag THEN DO:

              DO li = 1 TO 2:
                CREATE tt-formtext.
                ASSIGN tt-line-no = li
                       tt-length  = 55.
              END.
              
              RUN custom/formtext.p (lv-text).

              ASSIGN i = 0 v-dept-note = "".

              FOR EACH tt-formtext:
                ASSIGN i = i + 1.
                IF  i <= 2 THEN v-dept-note[i] = tt-formtext.tt-text.      
              END.

              PUT UNFORMATTED 
                "<UNITS=INCHES><AT=7.5,.54><FROM><AT=+.4,+1.5><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                (job.job-no) + STRING(job.job-no2,"99") + ">" "<AT=,.6>" 
                (job-hdr.job-no) "-" STRING(job-hdr.job-no2,"99") 
                "<R44><C1><FROM><C85><LINE><|3>" skip
                "<R44><C19><FROM><R48><C19><LINE><|3>"
                "<R44><C85><FROM><R48><C85><LINE><|3>"
                "<R44><C20><B>BN Notes:</B><C30>" v-dept-note[1] SKIP
                "<C30>" v-dept-note[2] "<=11><R+4>" 
                SKIP.

              ASSIGN v-out1-id = RECID(xeb).

              RUN cec/desprncap.p (RECID(xef),
                                   INPUT-OUTPUT v-lines,
                                   RECID(xest), "").
              
            END. /* v-embaflg */
            ELSE DO:

              DO li = 1 TO 2:
                CREATE tt-formtext.
                ASSIGN tt-line-no = li
                       tt-length  = 80.
              END.

              RUN custom/formtext.p (lv-text).

              ASSIGN i = 0 v-dept-note = "".

              FOR EACH tt-formtext:

                ASSIGN i = i + 1.

                IF  i <= 2 THEN v-dept-note[i] = tt-formtext.tt-text.      
              END.
           
              PUT UNFORMATTED 
                "<UNITS=INCHES><AT=7.5,.54><FROM><AT=+.4,+1.5><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                (job.job-no) + STRING(job.job-no2,"99") + ">" "<AT=,.6>" 
                    (job-hdr.job-no) "-" STRING(job-hdr.job-no2,"99")            
                "<R44><C1><FROM><C105><LINE><|3>" skip
                "<R44><C19><FROM><R48><C19><LINE><|3>"
                "<R44><C20><B>BN Notes:</B><C30>" v-dept-note[1] SKIP
                "<C30>" v-dept-note[2] "<=11><R+4>" SKIP                 
                .
                         
              ASSIGN v-out1-id = RECID(xeb).
              RUN cec/desprnL2.p (RECID(xef),
                                  INPUT-OUTPUT v-lines,
                                  RECID(xest)).   
            END.

            PAGE.
        END.
        ELSE PAGE.
     END. /* for each w-ef */

     IF s-prt-set-header     AND 
        LAST-OF(job.job-no2) AND 
        est.est-type = 6 
       THEN DO: /* print set header */

         ASSIGN i = 0.
         FOR EACH bf-eb NO-LOCK
           WHERE bf-eb.company = est.company
             AND bf-eb.est-no = est.est-no
             AND bf-eb.form-no > 0:

           ASSIGN i = i + 1.
         END.

         IF i > 1 THEN DO:

          DEF VAR v-set-qty   AS INT               NO-UNDO.
          DEF VAR v-ord-qty   AS INT               NO-UNDO.
          DEF VAR v-over-run  AS CHAR              NO-UNDO.
          DEF VAR v-under-run AS CHAR              NO-UNDO.
          DEF VAR v-fg-set    AS CHAR FORM "x(15)" NO-UNDO.

           ASSIGN
             v-fg-set    = job-hdr.i-no
             v-set-qty   = IF avail xeb AND xeb.est-type EQ 6 
                           THEN
                            IF AVAIL xoe-ordl 
                              THEN xoe-ordl.qty ELSE job-hdr.qty
                           ELSE 0
             v-ord-qty   = (IF AVAIL xoe-ordl 
                              THEN xoe-ordl.qty ELSE job-hdr.qty) *
                            IF est.form-qty LE 1 
                              THEN 1 ELSE v-pqty
             v-over-run  = IF AVAIL xoe-ordl 
                             THEN TRIM(STRING(xoe-ordl.over-pct,">>9.99%"))
                             ELSE 
                              IF AVAIL xoe-ord 
                                THEN 
                                  TRIM(STRING(xoe-ord.over-pct,">>9.99%")) 
                                  ELSE ""
             v-under-run = IF AVAIL xoe-ordl 
                             THEN TRIM(STRING(xoe-ordl.under-pct,">>9.99%"))
                             ELSE 
                              IF AVAIL xoe-ord 
                               THEN TRIM(STRING(xoe-ord.under-pct,">>9.99%")) 
                               ELSE "".


           PUT 
               "<R1><C1><#15><C30><P16><B> SET HEADER<P7>" SKIP
               "<P13><B>" v-cus[1] AT 2 "<P7></B>" 
               "<P12><B>" lv-fg-name FORMAT "x(30)" AT 70 "<P7></B>"
               "<P8><B>Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
               "<P7></B><C60>Our Date: " v-ord-date SKIP
               "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
               "<=1><R+4><C2><From><R+5><C78><RECT><||3>" SKIP
               "<=1><R+4><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
               v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
               v-i-line[2] AT 90
             SKIP
               v-cus[2] AT 3 " Job Qty:" TRIM(STRING(job-hdr.qty * v-pqty,">>>,>>9"))    FORMAT "x(7)"
               " Order Qty:" STRING(v-ord-qty) FORMAT "x(7)"
               v-i-line[3] AT 90 SKIP
               v-cus[3] AT 3  " Cust Part #:" lv-part-no 
               v-i-line[4] AT 90 SKIP
               v-cus[4]  AT 3 " Overrun:"  FORMAT "x(7)" 
               " Underrun:" FORMAT "x(7)"  
               "Adders:" v-adders FORM "x(36)" v-i-line[5] AT 90 SKIP
               "<=1><R+9><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
               "<C2>FINISHED GOOD #  DESCRIPTION                     SAMPLES REQ'D  RATIO PER SET     DIE #      CAD#        STYLE" SKIP.
             
             
           DEF VAR v-shipto AS cha NO-UNDO.
        
           ASSIGN  v-tmp-line = 0.
           FOR EACH xeb NO-LOCK 
             WHERE xeb.company = est.company
               AND xeb.est-no = est.est-no
               AND xeb.form-no > 0  BY xeb.stock-no:

              /* gdm - 11030802 */
              IF s-sample-rEQuired = TRUE THEN DO:

                ASSIGN v-samprEQ = 0.
                
                FIND FIRST b-ef NO-LOCK 
                  WHERE b-ef.company EQ xeb.company
                    AND b-ef.est-no  EQ xeb.est-no 
                    AND b-ef.EQty    EQ xeb.EQty   
                    AND b-ef.form-no EQ xeb.form-no NO-ERROR.
                IF AVAIL b-ef 
                  THEN
                   FIND FIRST reftable NO-LOCK
                     WHERE reftable.reftable EQ "cecrep/allwest.w"
                       AND reftable.company  EQ xeb.company
                       AND reftable.loc      EQ TRIM(job-hdr.job-no)
                       AND reftable.code     EQ STRING(job-hdr.job-no2,"9999999999")
                       AND reftable.code2    EQ STRING(b-ef.form-no,"999")  /*form-no*/
                       AND reftable.val[1]   EQ xeb.blank-no NO-ERROR. /*blank-no*/
                   IF AVAILABLE reftable 
                     THEN
                       ASSIGN v-samprEQ = reftable.val[2].
                     ELSE 
                       ASSIGN v-samprEQ = 0.
              END. /* IF s-sample-rEQuired */

              
              PUT xeb.stock-no   AT 3  
                  xeb.part-dscr1 AT 20 
                  v-samprEQ      AT 52.

              IF xeb.yld-qty LT 0 
                THEN PUT -1 / xeb.yld-qty FORMAT ">>>>>>9.9<<<<<<" AT 67.
                ELSE PUT xeb.yld-qty FORMAT ">>>>>>9.9<<<<<<" AT 67.

              FIND FIRST xstyle NO-LOCK
                WHERE xstyle.company  EQ xeb.company
                  AND xstyle.style    EQ xeb.style
                  AND xstyle.industry EQ "2" NO-ERROR.
              
              PUT 
                 xeb.die-no FORMAT "x(10)" AT 85
                 xeb.cad-no FORMAT "x(10)" AT 96
                 IF AVAIL xstyle 
                  THEN xstyle.dscr ELSE "" FORMAT "x(30)" AT 108. 

              PUT SKIP.

              ASSIGN v-tmp-line = v-tmp-line + 1.

              IF v-tmp-line + 65 > PAGE-SIZE THEN DO:
                 PUT "<=1><R+10><C2><FROM><R+" + STRING(v-tmp-line + 1) + "><C78><RECT><||3>" FORM "x(150)" SKIP.
                 PAGE. 
                 v-tmp-line = 0.

                 PUT "<R1><C30><P16><B> SET HEADER<P7>" SKIP
                     "<P13><B>" v-cus[1] AT 2 "<P7></B>" 
                     "<P12><B>" lv-fg-name FORMAT "x(30)" AT 70 "<P7></B>"
                     "<P8><B>Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
                     "<P7></B><C60>Our Date: " v-ord-date SKIP
                     "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
                     "<=1><R+4><C2><From><R+5><C78><RECT><||3>" SKIP
                     "<=1><R+4><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
                     v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
                     v-i-line[2] AT 90
                   SKIP
                     v-cus[2] AT 3 " Job Qty:" TRIM(STRING(job-hdr.qty * v-pqty,">>>,>>9"))    FORMAT "x(7)"
                     " Order Qty:" STRING(v-ord-qty) FORMAT "x(7)"
                     v-i-line[3] AT 90 SKIP
                     v-cus[3] AT 3  " Cust Part #:" lv-part-no 
                     v-i-line[4] AT 90 SKIP
                     v-cus[4]  AT 3 " Overrun:"  FORMAT "x(7)" 
                     " Underrun:" FORMAT "x(7)"  
                     "Adders:" v-adders FORM "x(36)" v-i-line[5] AT 90 SKIP
                     "<=1><R+9><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
                     "<C2>FINISHED GOOD #  DESCRIPTION                     SAMPLES REQ'D  RATIO PER SET     DIE #      CAD#        STYLE" SKIP.             
              END.
           END.

           ASSIGN v-tmp-line = v-tmp-line + 1.

           FIND LAST b-ef USE-INDEX est-qty 
             WHERE b-ef.company = est.company
               AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.

           DO i = 1 TO 8:
             IF b-ef.spec-no[i] <> "" THEN DO:
                   RUN custom/extradec.p (.0001, b-ef.spec-qty[i],
                                          OUTPUT lv-spec-qty[i]).
                   PUT b-ef.spec-dscr[i] AT 32 space(16) lv-spec-qty[i] SKIP.
                   v-tmp-line = v-tmp-line + 1.
                END.
             END.
             
             PUT 
                 "<=1><R+10><C2><FROM><R+" + STRING(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)" SKIP.

             ASSIGN v-tmp-line = v-tmp-line + 10
                    i = 0.

             FOR EACH tt-wm WHERE LOOKUP(tt-wm.m-code,tspostfg-char) > 0:
                ASSIGN i = i + 1.
             END.


             ASSIGN i = i + 2.
             PUT 
                 "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
                 "<=1><R+" + STRING(v-tmp-line + 1) + "><C2><FROM><R+" + STRING(i) + "><C78><RECT><||3>" FORM "x(150)" SKIP
                 "<=1><R+" + STRING(v-tmp-line + 1) + ">" FORM "x(20)".
                 .

            
             ASSIGN i = 0.
             FOR EACH tt-wm WHERE LOOKUP(tt-wm.m-code,tspostfg-char) > 0  
               BY tt-wm.dsEQ:

               ASSIGN i = i + 1.


               DISPLAY 
                    tt-wm.dscr AT 3
                    tt-wm.s-hr when tt-wm.s-hr ne 0
                    FILL("_",7)  FORMAT "x(7)"    to 38   when tt-wm.dscr ne ""
                    FILL("_",7)  FORMAT "x(7)"    to 46   when tt-wm.dscr ne ""
                    FILL("_",7)  FORMAT "x(7)"    to 54   when tt-wm.dscr ne ""
                    space(2)
                    tt-wm.r-sp when tt-wm.r-sp ne 0
                    FILL("_",7)  FORMAT "x(7)"    to 69   when tt-wm.dscr ne ""
                    FILL("_",7)  FORMAT "x(7)"    to 77   when tt-wm.dscr ne ""
                    FILL("_",7)  FORMAT "x(7)"    to 85   when tt-wm.dscr ne ""
                    FILL("_",8)  FORMAT "x(8)"    to 99   when tt-wm.dscr ne ""
                    FILL("_",8)  FORMAT "x(8)"    to 108  when tt-wm.dscr ne ""
                    FILL("_",8)  FORMAT "x(8)"    to 117  when tt-wm.dscr ne ""
                    FILL("_",8)  FORMAT "x(8)"    to 129  when tt-wm.dscr ne ""
                    /*CHR(124) FORMAT "x"           at 131   */                  
                    WITH NO-BOX NO-LABELS FRAME o21 WIDTH 132 
                     NO-ATTR-SPACE DOWN STREAM-IO.
             END.


             FOR EACH tt-wm:
                 DELETE tt-wm.
             END.

             
             ASSIGN 
               v-tmp-line = v-tmp-line + 3 + i /* 4 and add machine routing lines */.
               v-shipto = IF AVAIL xoe-rel 
                          THEN xoe-rel.ship-id 
                          ELSE 
                           IF avail xeb 
                             THEN xeb.ship-id
                             ELSE 
                              IF avail xoe-ord 
                               THEN xoe-ord.sold-id 
                               ELSE "".


             FIND FIRST tt-prem 
               WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
                 AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
             IF NOT AVAIL tt-prem THEN DO:
               FIND FIRST bf-eb 
                 WHERE bf-eb.company = est.company
                   AND bf-eb.est-no = est.est-no
                   AND bf-eb.form-no = 0
                   AND bf-eb.blank-no = 0 NO-LOCK NO-ERROR.

               CREATE tt-prem.
               IF AVAIL bf-eb 
                 THEN 
                   ASSIGN tt-prem.tt-#-bundle = STRING(bf-eb.cas-cnt)
                          tt-prem.tt-#-unit = STRING(bf-eb.tr-cnt)
                          tt-prem.tt-pattern = bf-eb.tr-no
                          tt-prem.tt-pallet = bf-eb.cas-no.
             END.

             FOR EACH tt-formtext:
                DELETE tt-formtext.
             END.    

             ASSIGN lv-text = "".
             FOR EACH notes NO-LOCK 
               WHERE notes.rec_key = job.rec_key
                 AND notes.note_code = "SH":

               ASSIGN lv-text = lv-text + " " + 
                                TRIM(notes.note_text) + CHR(10).

             END.

             FOR EACH notes NO-LOCK 
               WHERE notes.rec_key = itemfg.rec_key
                 AND notes.note_code = "SH":
                ASSIGN lv-text = lv-text + " " + 
                                 TRIM(notes.note_text) + CHR(10).
             END.


             DO li = 1 TO 15:
                CREATE tt-formtext.
                ASSIGN
                   tt-line-no = li
                   tt-length  = 72.
             END.

             RUN custom/formtext.p (lv-text).

             ASSIGN i = 0 
                    v-dept-inst = "".


             FOR EACH tt-formtext:
                i = i + 1.
                IF  i <= 15 THEN v-dept-inst[i] = tt-formtext.tt-text.      
             END.

             IF v-ship <> "" THEN v-dept-inst[15] = v-ship.  /* shipto notes */

             PUT "<=1><R+" + STRING(v-tmp-line) + ">" FORMAT "X(20)".

             ASSIGN v-tmp-line = v-tmp-line + 1.
             
             PUT 
                 "Unitizing Bale <C24>Date <C44>Units <C62>Complete" AT 3 SKIP
                  "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________" skip
                  "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial" skip
                  "Pattern: " AT 3 tt-prem.tt-pattern FORM "x(10)" "<C20>_____________________ <C40>____________________  <C60>________________" skip
                  "Pallet: " AT 3 tt-prem.tt-pallet FORM "x(10)"  "<C20>_____________________ <C40>____________________ " skip
                  "<=1><R+" + STRING(v-tmp-line) + "><C2><FROM><R+6><C78><RECT><||3>" FORM "x(150)" SKIP
                  "<=1><R+" + STRING(v-tmp-line + 7) + "><C2><FROM><R+15><C78><RECT><||3>" FORM "x(150)" SKIP
                  "<=1><R+" + STRING(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
                  v-dept-inst[1] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[1] SKIP
                  v-dept-inst[2] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[2] SKIP
                  v-dept-inst[3] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[3] SKIP
                  v-dept-inst[4] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[4] SKIP
                  v-dept-inst[5] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" "Item PO #:" v-po-no SKIP
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
  END.  /* each job */
END.  /* end v-local-loop  */
 
HIDE ALL NO-PAUSE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
