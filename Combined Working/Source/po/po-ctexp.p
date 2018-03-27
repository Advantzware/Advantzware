/* -------------------------------------------------- po/po-ctexp.p 02/02 JLF */
/*                                                                            */
/* Corr-Trim export PO                                                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-format as char no-undo.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xjob-mat for job-mat.
def buffer xitem for item.
def buffer b-ref1  for reftable.
def buffer b-ref2  for reftable.

{po/po-print.i}

def var v-sname like shipto.ship-name.
def var v-saddr like shipto.ship-addr.
def var v-scity like shipto.ship-city.
def var v-sstate like shipto.ship-state.
def var v-szip like shipto.ship-zip.
def var v-adder like item.i-no extent 6 no-undo.
def var v-add-cst as dec extent 6 no-undo.
def var v-add-set as dec extent 6 no-undo.
def var xg-flag as log init no no-undo.
def var v-instr as char no-undo.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR v-tmp-lines AS dec NO-UNDO.
DEF VAR lv-got-return AS int NO-UNDO.
def var v-ord-qty like po-ordl.ord-qty no-undo.
def var v-ord-cst like po-ordl.cost no-undo.
def var v-ord-set like po-ordl.setup no-undo.
def var v-brd-cst as dec no-undo.
def var v-brd-set as dec no-undo.
def var v-fac-cst as dec no-undo.
def var v-fac-set as dec no-undo.
def var v-outfile as char extent 4 no-undo.
def var v-mach as char extent 4 no-undo.
def var v-line as char no-undo.
DEF VAR ld AS DEC DECIMALS 10 NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-uom LIKE job-mat.sc-uom NO-UNDO.


DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20.

{sys/inc/corrtrim.i}

find first po-ctrl where po-ctrl.company eq cocode no-lock.

find first company where company.company eq cocode no-lock.

find first cust
    where cust.company eq cocode
      and cust.active  eq "X"
    no-lock no-error.

if avail cust and corrtrim-log and corrtrim-dir ne "" then
print-po-blok:
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST po-ord
    WHERE RECID(po-ord) EQ report.rec-id
      AND CAN-FIND(FIRST po-ordl
                   WHERE po-ordl.company   EQ po-ord.company
                     AND po-ordl.po-no     EQ po-ord.po-no
                     AND po-ordl.item-type EQ YES
                     AND (v-printde-po OR NOT po-ordl.deleted)),

    FIRST vend
    WHERE vend.company    EQ po-ord.company
      AND vend.vend-no    EQ po-ord.vend-no
      AND (vend.po-export EQ "CorrTrim" OR
           (poexport-cha  EQ "CorrTrim" AND vend.an-edi-vend))
    NO-LOCK
                   
    BREAK BY po-ord.po-no.

  if first(po-ord.po-no) then do:
    if opsys eq "UNIX" and substr(corrtrim-dir,1,1) ne v-slash then
      corrtrim-dir = v-slash + corrtrim-dir.

    if substr(corrtrim-dir,length(corrtrim-dir),1) eq v-slash then
      substr(corrtrim-dir,length(corrtrim-dir),1) = "".
    
    assign
     v-outfile[1] = trim(corrtrim-dir) + v-slash + "dataxfer" +
                    v-slash + "in" + v-slash
     v-outfile[2] = v-outfile[1] + string(time,"99999999")
     v-outfile[3] =  (IF corrtrim-char = "Welsh" THEN "socorrWelsh_po_" ELSE "po_")
                + trim(REPLACE(v-format, " ","")) + "lacor" +
                    substr(string(year(today),"9999"),3,2) +
                    string(month(today),"99") +
                    string(day(today),"99") +
                    substr(string(time,"HH:MM:SS"),1,2) +
                    substr(string(time,"HH:MM:SS"),4,2) +
                    substr(string(time,"HH:MM:SS"),7,2) + ".dat"
     v-outfile[4] = v-outfile[1] + v-outfile[3].
    
    output to value(v-outfile[2]).
    
    /* Order Download Specification - BEGIN */
    put "$BEGIN$"
        space(2)
        "9"
        space(244)
        skip.
  end.

  if po-ord.stat eq "N" then po-ord.stat = "O".

  assign
   v-sname    = company.name
   v-saddr[1] = company.addr[1]
   v-saddr[2] = company.addr[2]
   v-scity    = company.city
   v-sstate   = company.state
   v-szip     = company.zip.
 
  if po-ord.type eq "D" then
    assign
     v-sname    = po-ord.ship-name
     v-saddr[1] = po-ord.ship-addr[1]
     v-saddr[2] = po-ord.ship-addr[2]
     v-scity    = po-ord.ship-city
     v-sstate   = po-ord.ship-state
     v-szip     = po-ord.ship-zip.
  
  FOR EACH po-ordl NO-LOCK
      WHERE po-ordl.company   EQ po-ord.company
        AND po-ordl.po-no     EQ po-ord.po-no
        AND po-ordl.item-type EQ YES
        AND (v-printde-po OR NOT po-ordl.deleted),
      
      FIRST ITEM NO-LOCK
      WHERE item.company  EQ po-ordl.company
        AND item.i-no     EQ po-ordl.i-no
        AND item.mat-type EQ "B"
      
      BY po-ordl.line WITH FRAME po-line:
      
    ASSIGN
     xg-flag   = NO
     v-adder   = ""
     v-brd-cst = 0
     v-brd-set = 0
     v-add-cst = 0
     v-add-set = 0
     lv-uom    = "".
    
    FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
          AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no))) +
                                  TRIM(po-ordl.job-no)
          AND job.job-no2 EQ po-ordl.job-no2
        NO-ERROR.
        
    IF AVAIL job THEN DO:
      FIND FIRST est NO-LOCK
          WHERE est.company EQ job.company
            AND est.est-no  EQ job.est-no
          NO-ERROR.
      
      FOR EACH job-mat NO-LOCK
          WHERE job-mat.company  EQ cocode
            AND job-mat.job      EQ job.job
            AND job-mat.job-no   EQ job.job-no
            AND job-mat.job-no2  EQ job.job-no2
            AND job-mat.i-no     EQ po-ordl.i-no
            AND job-mat.frm      EQ po-ordl.s-num
          USE-INDEX job
          BREAK BY job-mat.blank-no DESC:

        IF LAST(job-mat.blank-no)            OR
           job-mat.blank-no EQ po-ordl.b-num THEN LEAVE.
      END.

      IF AVAIL job-mat THEN DO:
        FIND FIRST ef NO-LOCK
            WHERE ef.e-num   EQ job.e-num
              AND ef.form-no EQ job-mat.frm
            NO-ERROR.
   
        ASSIGN
         xg-flag = AVAIL ef AND (ef.xgrain EQ "S" OR ef.xgrain EQ "B")
         i       = 0.
         
        FOR EACH xjob-mat NO-LOCK
            WHERE xjob-mat.company  EQ job-mat.company
              AND xjob-mat.job      EQ job-mat.job
              AND xjob-mat.job-no   EQ job-mat.job-no
              AND xjob-mat.job-no2  EQ job-mat.job-no2
              AND xjob-mat.frm      EQ job-mat.frm
              AND xjob-mat.blank-no EQ job-mat.blank-no
              AND xjob-mat.i-no     NE job-mat.i-no,
              
            FIRST xitem NO-LOCK
            WHERE xitem.company  EQ xjob-mat.company 
              AND xitem.i-no     EQ xjob-mat.i-no
              AND xitem.mat-type EQ "A":
              
          ASSIGN
           i          = i + 1
           v-adder[i] = xitem.i-no.

          FIND FIRST e-item NO-LOCK
              WHERE e-item.company EQ xitem.company
                AND e-item.i-no    EQ xitem.i-no
              NO-ERROR.
    
          RELEASE e-item-vend.
          IF AVAIL e-item THEN
          FIND FIRST e-item-vend NO-LOCK
              WHERE e-item-vend.company EQ e-item.company
                AND e-item-vend.i-no    EQ e-item.i-no
                AND e-item-vend.vend-no EQ po-ord.vend-no
              NO-ERROR.

          IF AVAIL e-item-vend THEN DO:
            ld = po-ordl.ord-qty.

            IF po-ordl.pr-qty-uom NE e-item.std-uom THEN
              RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, e-item.std-uom,
                                      xitem.basis-w, xjob-mat.len,
                                      xjob-mat.wid, xitem.s-dep,
                                      ld, OUTPUT ld).

            EMPTY TEMP-TABLE tt-eiv.
            CREATE tt-eiv.
            DO li = 1 TO 10:
               ASSIGN
                  tt-eiv.run-qty[li] = e-item-vend.run-qty[li]
                  tt-eiv.run-cost[li] = e-item-vend.run-cost[li]
                  tt-eiv.setups[li] = e-item-vend.setups[li].
            END.
            
            
            
            IF AVAIL e-item-vend THEN
            DO:
               
            
               DO li = 1 TO 10:
                  ASSIGN
                     tt-eiv.run-qty[li + 10] = e-item-vend.runQtyXtra[li]
                     tt-eiv.run-cost[li + 10] = e-item-vend.runCostXtra[li]
                     tt-eiv.setups[li + 10] = e-item-vend.setupsXtra[li].
               END.
            END.
        
            DO li = 1 TO 20:
               IF ld LE tt-eiv.run-qty[li] THEN LEAVE.
            END.

            ASSIGN
             v-add-set[i] = tt-eiv.setups[li]
             v-add-cst[i] = tt-eiv.run-cost[li]
             lv-uom       = e-item.std-uom.
          END.

          ELSE
            ASSIGN
             v-add-cst[i] = xjob-mat.std-cost
             lv-uom       = xjob-mat.sc-uom.

          IF lv-uom NE "MSF" THEN
            RUN sys/ref/convcuom.p (lv-uom, "MSF", xjob-mat.basis-w,
                                    xjob-mat.len, xjob-mat.wid, xitem.s-dep,
                                    v-add-cst[i], OUTPUT v-add-cst[i]).
             
          IF i GE 6 THEN LEAVE.
        END.

        ASSIGN
         v-brd-cst = job-mat.std-cost
         lv-uom    = job-mat.sc-uom.
      END.
    END.

    FIND FIRST e-item NO-LOCK
        WHERE e-item.company EQ item.company
          AND e-item.i-no    EQ item.i-no
        NO-ERROR.
    
    RELEASE e-item-vend.
    IF AVAIL e-item THEN
    FIND FIRST e-item-vend NO-LOCK
        WHERE e-item-vend.company EQ e-item.company
          AND e-item-vend.i-no    EQ e-item.i-no
          AND e-item-vend.vend-no EQ po-ord.vend-no
        NO-ERROR.

    IF AVAIL e-item-vend THEN DO:
      ld = po-ordl.ord-qty.

      IF po-ordl.pr-qty-uom NE e-item.std-uom THEN
        RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, e-item.std-uom,
                                item.basis-w, po-ordl.s-len,
                                po-ordl.s-wid, item.s-dep,
                                ld, OUTPUT ld).

      EMPTY TEMP-TABLE tt-eiv.
      CREATE tt-eiv.
      DO li = 1 TO 10:
         ASSIGN
            tt-eiv.run-qty[li] = e-item-vend.run-qty[li]
            tt-eiv.run-cost[li] = e-item-vend.run-cost[li]
            tt-eiv.setups[li] = e-item-vend.setups[li].
      END.
      
            
      IF AVAIL e-item-vend THEN
      DO:
         
      
         DO li = 1 TO 10:
            ASSIGN
               tt-eiv.run-qty[li + 10] = e-item-vend.runQtyXtra[li]
               tt-eiv.run-cost[li + 10] = e-item-vend.runCostXtra[li]
               tt-eiv.setups[li + 10] = e-item-vend.setupsXtra[li].
         END.
      END.
        
      DO li = 1 TO 10:
         IF ld LE tt-eiv.run-qty[li] THEN LEAVE.
      END.

      ASSIGN
       v-brd-set = tt-eiv.setups[li]
       v-brd-cst = tt-eiv.run-cost[li]
       lv-uom    = e-item.std-uom.
    END.

    IF lv-uom NE "" AND lv-uom NE "MSF" THEN
      RUN sys/ref/convcuom.p (lv-uom, "MSF",
                              item.basis-w, po-ordl.s-len,
                              po-ordl.s-wid, item.s-dep,
                              v-brd-cst, OUTPUT v-brd-cst).
    
    /* Order Record - Type 9 */
    
    /* TYPE */
    put string(po-ord.printed,"C/A") format "x".
    
    /* CORR CODE */
    put "NO  ".
    
    /* GRADE */
    put po-ordl.i-name format "x(25)".
    
    /* CORR DATE */
    put substr(string(year(po-ord.due-date),"9999"),3,2) format "99"
        month(po-ord.due-date)                           format "99"
        day(po-ord.due-date)                             format "99".
    
    /* DUE DATE */
    put substr(string(year(po-ordl.due-date),"9999"),3,2)   format "99"
        month(po-ordl.due-date)                             format "99"
        day(po-ordl.due-date)                               format "99".
        
    /* ORDER ID */
    put po-ord.company  format "xxx"
        po-ord.po-no    format "999999999"
        po-ordl.line    format "999".
        
    /* WIDTH & LENGTH */
    put trunc(po-ordl.s-wid,0)                          format "9999"
        (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16   format "99".
        
    put trunc(po-ordl.s-len,0)                          format "9999"
        (po-ordl.s-len - trunc(po-ordl.s-len,0)) * 16   format "99".
        
    /* QUANTITY */
    v-ord-qty = po-ordl.ord-qty.
    
    if po-ordl.pr-qty-uom ne "EA" then
      run sys/ref/convquom.p (po-ordl.pr-qty-uom, "EA",
                              item.basis-w, po-ordl.s-len,
                              po-ordl.s-wid, item.s-dep,
                              v-ord-qty, output v-ord-qty).
                           
    if v-ord-qty - trunc(v-ord-qty,0) gt 0 then
      v-ord-qty = trunc(v-ord-qty,0) + 1.
    
    put v-ord-qty   format "999999".
    
    /* MAX OVER % */
    put 100 + po-ord.over-pct   format "999".
    
    /* STD OVER % */
    put 100 + po-ord.over-pct   format "999".
    
    /* UNDER % */
    put 100 - po-ord.under-pct  format "999".
    
    /* SHYABLE */
    put "N".
    
    /* MAX SHY */
    put "  ".
    
    /* FLUTE */
    put item.flute  format "x(10)".
    
    /* PER UNIT */
    put "    ".
    
    /* NUM SCORES */
    put "00".
    
    /* PRIORITY */
    put " ".
    
    /* MIN OUT */
    put "  ".
    
    /* MAX OUT */
    put "  ".
    
    /* MIN RUN */
    put "      ".
    
    /* SPLIT CODE */
    put "0 ".
    
    /* KNIFE */
    put " ".
    
    /* CLOSE */
    put " ".
    
    /* SCORE TYPE */
    put "  ".
    
    /* CUST NAME */
    put company.name    format "x(25)".
    
    /* CUST ID */
    put po-ord.company  format "x(25)".
    
    /* CUST PO */
    put string(po-ord.po-no,"999999999")    format "x(25)".
    
    /* CUST ITEM */
    put po-ordl.i-no    format "x(25)".
    
    /* SPECIAL INSTR */
    ASSIGN
     v-tmp-lines   = 0
     j             = 0
     k             = 0
     lv-got-return = 0
     v-inst        = ""
     v-instr       = "".
    
    /*do i = 1 to 4:
      if po-ord.spec-i[i] ne "" then
        v-instr = v-instr + trim(po-ord.spec-i[i]) + " ".
    end.*/
    FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
       DO i = 1 TO LENGTH(notes.note_text) :        
           IF i - j >= 80 THEN ASSIGN j = i
                                      lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / 80.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return.

           IF k < 5 THEN v-inst[k] = v-inst[k] +
                                     IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1)
                                     ELSE "" .              
           
           IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
           THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
           END.
       END.
    END.

    DO i = 1 TO 4:
      IF v-inst[i] NE "" THEN
        v-instr = v-instr + TRIM(v-inst[i]) + " ".
    END.

    put v-instr     format "x(240)".
    
    /* SHIP TO CUST ADDRESS NAME */
    put v-sname     format "x(30)".
    
    /* SHIP TO CUST ADDRESS1 */
    put v-saddr[1]  format "x(30)".
    
    /* SHIP TO CUST ADDRESS2 */
    put v-saddr[2]  format "x(30)".
    
    /* SHIP TO CUST CITY */
    put v-scity     format "x(35)".
    
    /* SHIP TO CUST STATE */
    put v-sstate    format "x(20)".
    
    /* SHIP TO CUST COUNTRY */
    put space(25).
    
    /* SHIP TO CUST ZIP */
    put v-szip      format "x(10)".
    
    /* SHIP TO CUST PHONE */
    put space(20).
    
    /* SHIP TO CUST FAX */
    put space(20).
    
    /* HOLD REASON CODE */
    put space(20).
    
    /* BILL TO CUST ADDRESS NAME */
    put company.name    format "x(30)".
    
    /* BILL TO CUST ADDRESS1 */
    put company.addr[1] format "x(30)".
    
    /* BILL TO CUST ADDRESS2 */
    put company.addr[2] format "x(30)".
    
    /* BILL TO CUST CITY */
    put company.city    format "x(35)".
    
    /* BILL TO CUST STATE */
    put company.state   format "x(20)".
    
    /* BILL TO CUST COUNTRY */
    put space(25).
    
    /* BILL TO CUST ZIP */
    put company.zip     format "x(25)".
    
    /* BILL TO CUST PHONE */
    put space(10).
    
    /* BILL TO CUST FAX */
    put space(20).
    
    put skip.
    
    run po/po-ordls.p (recid(po-ordl)).
    
    {po/po-ordls.i}
            
    /* Order Extension Record */
    
    /* TYPE */
    put "X".

    do i = 1 to 12:
      if avail b-ref1 then do:
        /* STYPE 1 to extent(b-ref1.val) */
        put substr(b-ref1.dscr,i,1) format "xx".
    
        /* SCORE 1 to extent(b-ref1.val) */
        put trunc(b-ref1.val[i],0)                          format "9999"
            (b-ref1.val[i] - trunc(b-ref1.val[i],0)) * 100  format "99".
      end.
            
      else put "  000000".
    end.

    do i = 1 to 5:
      if avail b-ref2 then do:
        /* STYPE 1 to extent(b-ref2.val) */
        put substr(b-ref2.dscr,i,1) format "xx".
    
        /* SCORE 1 to extent(b-ref2.val) */
        put trunc(b-ref2.val[i],0)                          format "9999"
            (b-ref2.val[i] - trunc(b-ref2.val[i],0)) * 100  format "99".
      end.
            
      else put "  000000".
    end.
    
    put space(6).
    
    /* TEST */
    put item.reg-no format "x(10)".
    
    /* BOARD PRICE / MSF */
    v-ord-cst = po-ordl.cost.
    
    if po-ordl.pr-uom ne "MSF" then
      run sys/ref/convcuom.p (po-ordl.pr-uom, "MSF",
                              item.basis-w, po-ordl.s-len,
                              po-ordl.s-wid, item.s-dep,
                              v-ord-cst, output v-ord-cst).
                           
    put v-ord-cst format "9999999.99".
    
    /* ADDERS */
    do i = 1 to 6:
      put v-adder[i] format "x(10)".
    end.
    
    put space(40).
    
    /* SET UP CHARGES */
    put "0000000.00".
    
    /* SET UP CHARGES UOM */
    put space(10).
    
    /* PO DATE */
    put substr(string(year(po-ord.due-date),"9999"),3,2) format "99"
        month(po-ord.due-date)                           format "99"
        day(po-ord.due-date)                             format "99".
    
    /* PO INSTR */
    ASSIGN
     v-tmp-lines   = 0
     j             = 0
     k             = 0
     lv-got-return = 0
     v-inst        = ""
     v-instr       = "".
    
    /*do i = 1 to 4:
      if po-ordl.spec-i[i] ne "" then
        v-instr = v-instr + trim(po-ordl.spec-i[i]) + " ".
    end.*/
    FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
       DO i = 1 TO LENGTH(notes.note_text) :        
           IF i - j >= 80 THEN ASSIGN j = i
                                      lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / 80.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return.

           IF k < 5 THEN v-inst[k] = v-inst[k] +
                                     IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1)
                                     ELSE "" .              
           
           IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
           THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
           END.
       END.
    END.

    DO i = 1 TO 4:
      IF v-inst[i] NE "" THEN
        v-instr = v-instr + TRIM(v-inst[i]) + " ".
    END.

    put v-instr     format "x(240)".
    
    /* JOB NO */
    put trim(po-ordl.job-no) + "-" +
        string(po-ordl.job-no2,"99") format "x(9)".
        
    /* FIRST MACHINE CODE & INITIAL */
    {po/po-fibr1.i v-mach[1] v-mach[2] v-mach[3] v-mach[4]}
    
    put v-mach[1]   format "x(5)"
        v-mach[2]   format "x(1)".

    /* PO LINE DESCRIPTION */
    put po-ordl.dscr[1] format "x(30)"
        po-ordl.dscr[2] format "x(30)".

    /* PO LINE LINE# */
    put po-ordl.line format "999".

    /* PO LINE FORM# */
    put po-ordl.s-num format "99".

    /* PO LINE BOARD/MSF */
    v-ord-qty = po-ordl.ord-qty.
    
    if po-ordl.pr-qty-uom ne "MSF" then
      run sys/ref/convquom.p (po-ordl.pr-qty-uom, "MSF",
                              item.basis-w, po-ordl.s-len,
                              po-ordl.s-wid, item.s-dep,
                              v-ord-qty, output v-ord-qty).

    ASSIGN
     v-ord-cst = (po-ordl.t-cost /*- po-ordl.setup*/) / v-ord-qty
     v-fac-cst = v-brd-cst + v-add-cst[1] + v-add-cst[2] + v-add-cst[3] +
                             v-add-cst[4] + v-add-cst[5] + v-add-cst[6]
     v-fac-set = v-brd-set + v-add-set[1] + v-add-set[2] + v-add-set[3] +
                             v-add-set[4] + v-add-set[5] + v-add-set[6].

    IF v-fac-cst EQ 0 THEN v-brd-cst = v-ord-cst.
    IF v-fac-set EQ 0 THEN v-brd-set = po-ordl.setup.

    ASSIGN
     v-fac-cst = v-ord-cst / v-fac-cst
     v-fac-set = po-ordl.setup / v-fac-set.

    IF v-fac-cst EQ ? THEN v-fac-cst = 1.
    IF v-fac-set EQ ? THEN v-fac-set = 1.

    /* PO LINE BOARD/MSF */
    PUT v-brd-cst * v-fac-cst FORMAT "9999999.99".

    /* PO LINE BOARD SETUP */
    PUT v-brd-set * v-fac-set FORMAT "9999999.99".

    /* ADDERS */
    DO i = 1 TO 6:
      /* PO LINE ADDER/MSF */
      PUT v-add-cst[i] * v-fac-cst FORMAT "9999999.99".

      /* PO LINE ADDER SETUP */
      PUT v-add-set[i] * v-fac-set FORMAT "9999999.99".
    END.

    put skip.
  end. /* for each po-ordl record */

  po-ord.printed = yes.
  
  if last(po-ord.po-no)        and
     search(v-outfile[2]) ne ? then do:
    /* Order Download Specification - END */
    put "$END$"
        space(249)
        skip.

    output close.
    
    if opsys eq "unix" then
      unix silent quoter -c 1-3000 value(v-outfile[2]) >
                                   value(v-outfile[2] + ".quo").
    else
      dos  silent quoter -c 1-3000 value(v-outfile[2]) >
                                   value(v-outfile[2] + ".quo").
                                   
    input from value(v-outfile[2] + ".quo").
    
    output to value(v-outfile[4]).
    
    repeat:
      v-line = "".
      import v-line.
      if v-line eq "" then put skip(1).
      else put unformatted v-line skip.
    end.
    
    output close.
    
    input close.
    
    if opsys eq "unix" then
      unix silent rm value(v-outfile[2] + "*.*").
    else
      dos silent del value(v-outfile[2] + "*.*").
      

     IF corrtrim-char = "Welsh" THEN 
          RUN po/ftppo.p (v-outfile[4],"Welsh"). 
    message "Corr-Trim file:" trim(v-outfile[3]) "has been created"
            view-as alert-box.
  end.
end. /* for each po-ord record */

/* end ----------------------------------- Copr. 2002  Advanced Software Inc. */
