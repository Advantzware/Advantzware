/* -------------------------------------------------- po/po-kwexp.p 03/07 JLF */
/*                                                                            */
/* Kiwi export PO                                                              */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-FORMAT AS CHAR NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF BUFFER xjob-mat FOR job-mat.
DEF BUFFER xitem FOR item.
DEF BUFFER b-ref1  FOR reftable.
DEF BUFFER b-ref2  FOR reftable.

{po/po-print.i}

DEF VAR v-sname LIKE shipto.ship-name.
DEF VAR v-saddr LIKE shipto.ship-addr.
DEF VAR v-scity LIKE  shipto.ship-city.
DEF VAR v-sstate LIKE shipto.ship-state.
DEF VAR v-szip LIKE shipto.ship-zip.
DEF VAR v-instr AS CHAR NO-UNDO.
DEF VAR v-ord-qty LIKE po-ordl.ord-qty EXTENT 4 NO-UNDO.
DEF VAR v-ord-cst LIKE po-ordl.cost NO-UNDO.
DEF VAR v-setup LIKE e-item-vend.setup NO-UNDO.
DEF VAR v-outfile AS CHAR EXTENT 4 NO-UNDO.
DEF VAR v-mach AS CHAR EXTENT 4 NO-UNDO.
DEF VAR v-line AS CHAR NO-UNDO.
DEF VAR v-job-no AS CHAR NO-UNDO.
DEF VAR li-style AS INT NO-UNDO.
DEF VAR lv-brdadd AS CHAR EXTENT 7 NO-UNDO.
DEF VAR li-brdadd AS INT EXTENT 7 NO-UNDO.
DEF VAR lv-board-vend-rm AS INT NO-UNDO.
DEF VAR lv-board-adder-vend-rm AS INT EXTENT 6 NO-UNDO.
DEF VAR li AS INT NO-UNDO.


DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20.

{sys/inc/kiwi.i}

IF kiwi-char EQ "Trilakes" THEN
   v-format = "TL".

FIND FIRST po-ctrl WHERE po-ctrl.company EQ cocode NO-LOCK.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK.

FIND FIRST cust
    WHERE cust.company EQ cocode
      and cust.active  EQ "X"
    NO-LOCK NO-ERROR.

IF AVAIL cust AND kiwi-log AND kiwi-dir NE "" THEN
print-po-blok:
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST po-ord
    WHERE RECID(po-ord) EQ report.rec-id
      AND CAN-FIND(FIRST po-ordl
                   WHERE po-ordl.company   EQ po-ord.company
                     AND po-ordl.po-no     EQ po-ord.po-no
                     AND po-ordl.item-type EQ YES
                     AND (v-printde-po OR NOT po-ordl.deleted)),

    FIRST vend NO-LOCK
    WHERE vend.company EQ po-ord.company
      AND vend.vend-no EQ po-ord.vend-no
      AND (vend.po-export EQ "Kiwi" OR
           (poexport-cha  EQ "Kiwi" AND vend.an-edi-vend))
                   
    BY po-ord.po-no.

  IF OPSYS EQ "UNIX" AND SUBSTR(kiwi-dir,1,1) NE v-slash THEN
    kiwi-dir = v-slash + kiwi-dir.

  if SUBSTR(kiwi-dir,LENGTH(kiwi-dir),1) EQ v-slash THEN
    SUBSTR(kiwi-dir,LENGTH(kiwi-dir),1) = "".
    
  ASSIGN
   v-outfile[1] = TRIM(kiwi-dir) + v-slash + "dataxfer" +
                  v-slash + "in" + v-slash
   v-outfile[2] = v-outfile[1] + STRING(TIME,"99999999").

  IF v-format = "TL" THEN
     v-outfile[3] = "TL_" + "kiwi" +
                  SUBSTR(STRING(YEAR(TODAY),"9999"),3,2) +
                  STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") +
                  SUBSTR(STRING(TIME,"HH:MM:SS"),1,2) +
                  SUBSTR(STRING(TIME,"HH:MM:SS"),4,2) +
                  SUBSTR(STRING(TIME,"HH:MM:SS"),7,2) + ".dat".
  ELSE
     v-outfile[3] = "po_" + TRIM(REPLACE(v-format, " ","")) + "kiwi" +
                    SUBSTR(STRING(YEAR(TODAY),"9999"),3,2) +
                    STRING(MONTH(TODAY),"99") +
                    STRING(DAY(TODAY),"99") +
                    SUBSTR(STRING(TIME,"HH:MM:SS"),1,2) +
                    SUBSTR(STRING(TIME,"HH:MM:SS"),4,2) +
                    SUBSTR(STRING(TIME,"HH:MM:SS"),7,2) + ".dat".

  v-outfile[4] = v-outfile[1] + v-outfile[3].
    
  OUTPUT TO VALUE(v-outfile[2]) BINARY.

  IF po-ord.stat EQ "N" THEN po-ord.stat = "O".

  ASSIGN
   v-sname    = cust.name
   v-saddr[1] = cust.addr[1]
   v-saddr[2] = cust.addr[2]
   v-scity    = cust.city
   v-sstate   = cust.state
   v-szip     = cust.zip.
 
  IF po-ord.type EQ "D" THEN
    ASSIGN
     v-sname    = po-ord.ship-name
     v-saddr[1] = po-ord.ship-addr[1]
     v-saddr[2] = po-ord.ship-addr[2]
     v-scity    = po-ord.ship-city
     v-sstate   = po-ord.ship-state
     v-szip     = po-ord.ship-zip.

  FIND FIRST carrier NO-LOCK
      WHERE carrier.company EQ po-ord.company
        AND carrier.carrier EQ po-ord.carrier
      NO-ERROR.

  /* Order Download Specification */

  /* H1 */

  /* CUSTOMER # */
  IF kiwi-char EQ "TRILAKES" THEN
     PUT "20211"                                  FORMAT "x(5)".
  ELSE /*CSC*/
     PUT "20500"                                  FORMAT "x(5)".

  /* 01 */
  PUT "01"                                        FORMAT "x(2)".

  /* 000000 */
  PUT "000000"                                    FORMAT "x(6)".

  /* CUSTOMER PHONE # */
  PUT cust.area-code                              FORMAT "999"
      "-"                                         FORMAT "x"
      cust.phone                                  FORMAT "999-9999".

  /* CUSTOMER BILLING ZIP */
  PUT cust.zip                                    FORMAT "x(12)".

  /* SHIP VIA */
  PUT STRING(IF AVAIL carrier THEN carrier.dscr ELSE po-ord.carrier)
                                                  FORMAT "x(15)".

  /* FREIGHT */
  PUT IF po-ord.fob-code EQ "DEST" THEN "DESTINATION" ELSE "ORIGIN"
                                                  FORMAT "x(15)".

  /* P.O. DATE */
  PUT po-ord.po-date                              FORMAT "99/99/99".

  /* 000 */
  PUT FILL(" ",3)                                 FORMAT "x(3)".

  /* CUST # (LEAVE blank) */
  PUT FILL(" ",5)                                 FORMAT "x(5)".

  /* 45 blank spaces */
  PUT FILL(" ",45)                                FORMAT "x(45)"      skip.

  /* H2 */
    
  /* CUST BILLING NAME */
  PUT cust.name                                   FORMAT "x(30)".
    
  /* CUST BILLING ADDRESS */
  PUT cust.addr[1]                                FORMAT "x(30)".
    
  /* CUST BILLING ADDRESS */
  PUT cust.addr[2]                                FORMAT "x(30)".
    
  /* CUST BILLING CITY, ST */
  PUT TRIM(cust.city) + " " + TRIM(cust.state)    FORMAT "x(30)".

  /* 8 blank spaces */
  PUT FILL(" ",8)                                 FORMAT "x(8)"       skip.

  /* H3 */
    
  /* CUST SHIP TO NAME */
  PUT v-sname                                     FORMAT "x(30)".
    
  /* CUST SHIP TO ADDRESS */
  PUT v-saddr[1]                                  FORMAT "x(30)".
    
  /* CUST SHIP TO ADDRESS */
  PUT v-saddr[2]                                  FORMAT "x(30)".
    
  /* CUST SHIP TO CITY, ST */
  PUT TRIM(v-scity) + " " + TRIM(v-sstate)        FORMAT "x(30)".

  /* 8 blank spaces */
  PUT FILL(" ",8)                                 FORMAT "x(8)"       skip.

  FOR EACH po-ordl
      WHERE po-ordl.company   EQ po-ord.company
        AND po-ordl.po-no     EQ po-ord.po-no
        AND po-ordl.item-type EQ YES
        AND (v-printde-po OR NOT po-ordl.deleted),
      
      FIRST item NO-LOCK
      WHERE item.company  EQ cocode
        AND item.i-no     EQ po-ordl.i-no
        AND item.mat-type EQ "B"
      
      BY po-ordl.line:

    /* Order Download Specification */
    
    /* D1 */

    /* CUSTOMER # */
    IF kiwi-char EQ "TRILAKES" THEN
       PUT "20211"                                  FORMAT "x(5)".
    ELSE /*CSC*/
       PUT "20500"                                  FORMAT "x(5)".

    /* 01 */
    PUT "01"                                        FORMAT "x(2)".
    
    /* PURCHASE ORDER # */
    PUT po-ord.po-no                                FORMAT "9999999".
    
    /* PURCHASE ORDER LINE # */
    PUT po-ordl.line                                FORMAT "99".
    
    /* PURCHASE ORDER # */
    PUT po-ord.po-no                                FORMAT "9999999".
    
    /* PURCHASE ORDER LINE # */
    PUT po-ordl.line                                FORMAT "99".
    
    /* UNUSED (LEAVE blank) */
    PUT FILL(" ",13)                                FORMAT "x(13)".
    
    /* BY */
    PUT "BY"                                        FORMAT "x(5)".
    
    /* DUE DATE */
    PUT SUBSTR(STRING(year(po-ordl.due-date),"9999"),3,2)  +
        STRING(month(po-ordl.due-date),"99") +
        STRING(day(po-ordl.due-date),"99")          FORMAT "x(6)".
    
    /* OVERRUN PERCENTAGE */
    PUT po-ord.over-pct                             FORMAT "99".
    
    /* UNDERRUN PERCENTAGE */
    PUT po-ord.under-pct                            FORMAT "99".
        
    /* INTEGER OF WIDTH */
    PUT TRUNC(po-ordl.s-wid,0)                      FORMAT "999999".

    /* NUMERATOR OF WIDTH */
    PUT (po-ordl.s-wid - TRUNC(po-ordl.s-wid,0)) * 16
                                                    FORMAT "99".
    /* DENOMINATOR OF WIDTH */
    PUT 16                                          FORMAT "99".
        
    /* WIDTH */
    PUT TRUNC(po-ordl.s-wid,0)                      FORMAT "999"
        ":"                                         FORMAT "x"
        (po-ordl.s-wid - TRUNC(po-ordl.s-wid,0)) * 16
                                                    FORMAT "99".
        
    /* INTEGER OF LENGTH */
    PUT TRUNC(po-ordl.s-len,0)                      FORMAT "999999".

    /* NUMERATOR OF LENGTH */
    PUT (po-ordl.s-len - TRUNC(po-ordl.s-len,0)) * 16
                                                    FORMAT "99".
    /* DENOMINATOR OF LENGTH */
    PUT 16                                          FORMAT "99".
        
    /* LENGTH */
    PUT TRUNC(po-ordl.s-len,0)                      FORMAT "999"
        ":"                                         FORMAT "x"
        (po-ordl.s-len - TRUNC(po-ordl.s-len,0)) * 16
                                                    FORMAT "99".
    
    /* STYLE NUMBER */
    RUN po/po-ordls.p (RECID(po-ordl)).
    
    {po/po-ordls.i}
 
    li-style = IF AVAIL b-ref1 OR AVAIL b-ref2 THEN 1 ELSE 2.

    PUT li-style                                    FORMAT "9999".
    
    /* STYLE DESCRIPTION */
    PUT (IF li-style EQ 1 THEN "SCORED" ELSE "TRIMMED") + " SHEET"
                                                    FORMAT "x(14)".
    
    /* WEIGHT OF BOARD */
    PUT item.basis-w                                FORMAT "9999".
        
    /* QUANTITY SHEETS */
    v-ord-qty[1] = po-ordl.ord-qty.
    
    IF po-ordl.pr-qty-uom NE "EA" THEN
      RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-qty[1], OUTPUT v-ord-qty[1]).
                           
    IF v-ord-qty[1] - TRUNC(v-ord-qty[1],0) GT 0 THEN
      v-ord-qty[1] = TRUNC(v-ord-qty[1],0) + 1.

    v-ord-qty[2] = v-ord-qty[1].

    IF v-ord-qty[1] GT 99999999 THEN v-ord-qty[1] = 99999999.
    
    PUT v-ord-qty[1]                                FORMAT "99999999".

    /* 13 blank spaces */
    PUT FILL(" ",13)                                FORMAT "x(13)"      skip.
    
    /* D2 */
    
    /* FLUTE */
    PUT item.flute                                  FORMAT "x(3)".
    
    /* PRICE PER MSF */
    v-ord-cst = po-ordl.cost.
    
    IF po-ordl.pr-uom NE "MSF" THEN
      RUN sys/ref/convcuom.p(po-ordl.pr-uom, "MSF",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-cst, OUTPUT v-ord-cst).

    IF v-ord-cst GT 9999.99 THEN v-ord-cst = 9999.99.
                           
    PUT v-ord-cst                                   FORMAT "9999.99".
        
    /* SETUP CHARGE */
    v-setup = 0.

    RELEASE e-item.
    RELEASE e-item-vend.

    FIND FIRST e-item OF item NO-LOCK NO-ERROR.

    IF AVAIL e-item THEN
    FIND FIRST e-item-vend OF e-item NO-LOCK
        WHERE e-item-vend.vend-no   EQ po-ord.vend-no
          AND e-item-vend.item-type EQ YES
        NO-ERROR.
    
    IF AVAIL e-item-vend THEN DO:
      v-ord-qty[3] = po-ordl.ord-qty.

      IF po-ordl.pr-qty-uom NE e-item.std-uom THEN
        RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, e-item.std-uom,
                               item.basis-w, po-ordl.s-len,
                               po-ordl.s-wid, item.s-dep,
                               v-ord-qty[3], OUTPUT v-ord-qty[3]).
                           
      EMPTY TEMP-TABLE tt-eiv.
      CREATE tt-eiv.
      DO i = 1 TO 10:
         ASSIGN
            tt-eiv.run-qty[i] = e-item-vend.run-qty[i]
            tt-eiv.setups[i] = e-item-vend.setups[i].
      END.

            
      IF AVAIL e-item-vend THEN
      DO:
         
      
         DO i = 1 TO 10:
            ASSIGN
               tt-eiv.run-qty[i + 10] = e-item-vend.runQtyXtra[i]
               tt-eiv.setups[i + 10] = e-item-vend.setupsXtra[i].
         END.
      END.

      DO i = 1 TO 20:
         IF v-ord-qty[3] LE tt-eiv.run-qty[i] THEN DO:
            v-setup = tt-eiv.setups[i].
            LEAVE.
         END.
      END.
    END.

    IF v-setup GT 999.99 THEN v-setup = 999.99.
    
    PUT v-setup                                     FORMAT "999.99".

    /* "001.00" */
    PUT 1                                           FORMAT "999.99".

    /* "00000000.0000" */
    PUT 0                                           FORMAT "99999999.9999".
        
    /* ORDERED SF */
    v-ord-qty[3] = po-ordl.ord-qty.
    
    IF po-ordl.pr-qty-uom NE "SF" THEN
      RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "SF",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-qty[3], OUTPUT v-ord-qty[3]).
                           
    IF v-ord-qty[3] - TRUNC(v-ord-qty[3],0) GT 0 THEN
      v-ord-qty[3] = TRUNC(v-ord-qty[3],0) + 1.

    v-ord-qty[4] = v-ord-qty[3].

    IF v-ord-qty[3] GT 9999999 THEN v-ord-qty[3] = 9999999.
    
    PUT v-ord-qty[3]                                FORMAT "9999999".

    /* 46 blank spaces */
    PUT FILL(" ",46)                                FORMAT "x(46)".

    /* "X" */
    PUT "X"                                         FORMAT "x".

    /* "X" */
    PUT "X"                                         FORMAT "x".

    /* 8 blank spaces */
    PUT FILL(" ",38)                                FORMAT "x(38)"       skip.
    
    /* D3 */

    /* DESCRIPTION TEXT */
    PUT po-ordl.i-name                              FORMAT "x(30)".

    /* 128 blank spaces */
    PUT FILL(" ",98)                                FORMAT "x(98)"     skip.
    
    /* D4 */
    
    /* SCORE */
    DO i = 1 TO 9:
      IF AVAIL b-ref1 AND b-ref1.val[i] ne 0 THEN 
        PUT TRUNC(b-ref1.val[i],0)                  FORMAT ">>>"
            ":"                                     FORMAT "x"
            (b-ref1.val[i] - TRUNC(b-ref1.val[i],0)) * 100
                                                    FORMAT "99"
            SUBSTR(b-ref1.dscr,i,1)                 FORMAT "x".
            
      ELSE PUT "000:00 "                            FORMAT "x(7)".
    END.

    /* NUMBER UP */
    PUT 1                                           FORMAT "999.99".

    /* TRIM */
    PUT "00"                                        FORMAT "xx".

    /* 1 OUT WIDTH, NO TRIM */
    PUT TRUNC(po-ordl.s-wid,0)                      FORMAT "999"
        ":"                                         FORMAT "x"
        (po-ordl.s-wid - TRUNC(po-ordl.s-wid,0)) * 16
                                                    FORMAT "99".

    /* BLANK WIDTH (MULT OUT) */
    PUT TRUNC(po-ordl.s-wid,0)                      FORMAT "999"
        ":"                                         FORMAT "x"
        (po-ordl.s-wid - TRUNC(po-ordl.s-wid,0)) * 16
                                                    FORMAT "99".

    /* (SF OR SM) PER M */
    v-ord-qty[2] = (v-ord-qty[4] / v-ord-qty[2]) * 1000.

    IF v-ord-qty[2] GT 99999999 THEN v-ord-qty[2] = 99999999.

    PUT v-ord-qty[2]                                FORMAT "99999999".

    /* EXT'D (SF OR SM) ORDERED */
    IF v-ord-qty[4] GT 99999999 THEN v-ord-qty[4] = 99999999.

    PUT v-ord-qty[4]                                FORMAT "99999999".

    /* Board and Adders */

    IF v-format NE "TL" THEN
    DO:
       ASSIGN
        lv-brdadd = ""
        li-brdadd = 0
        li        = 1.
      
       FIND FIRST reftable NO-LOCK
           WHERE reftable.reftable EQ "util/b-hrms-x.w"
             AND reftable.company  EQ po-ordl.company
             AND reftable.code2    EQ po-ordl.i-no
           NO-ERROR.
       IF AVAIL reftable THEN
       DO i = 1 TO LENGTH(reftable.code):
         IF SUBSTR(reftable.code,i,1) EQ "." THEN li = li + 1.
         ELSE lv-brdadd[li] = lv-brdadd[li] + SUBSTR(reftable.code,i,1).
       END.
      
       DO li = 1 TO 7:
         li-brdadd[li] = INT(lv-brdadd[li]) NO-ERROR.
       END.
      
       /* BASE BOARD GRADE CODE */
       PUT li-brdadd[1]                                FORMAT "9999".
       
       /* ADDERS */
       DO i = 2 TO 7:
         PUT li-brdadd[i]                              FORMAT "9999".
       END.
    END.
    ELSE /*Trilakes*/
    DO:
       ASSIGN
          lv-board-vend-rm = 0
          lv-board-adder-vend-rm = 0.

       FIND FIRST reftable
            WHERE reftable.reftable EQ "util/b-hrms-x.w"
              AND reftable.company  EQ po-ordl.company
              AND reftable.code2    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.

       IF AVAIL reftable THEN
       DO:
          lv-board-vend-rm = INT(reftable.CODE) NO-ERROR.
          RELEASE reftable.
       END.

       /* BASE BOARD GRADE CODE */
       PUT lv-board-vend-rm                            FORMAT "9999".

       FIND FIRST job
            WHERE job.company EQ cocode
              AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no))) +
                                 TRIM(po-ordl.job-no)
              AND job.job-no2 EQ po-ordl.job-no2
            NO-LOCK NO-ERROR.
        
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
            IF last(job-mat.blank-no)            OR
               job-mat.blank-no EQ po-ordl.b-num THEN LEAVE.
          END.
         
          IF AVAIL job-mat THEN do:
            
             i = 0.
              
             FOR EACH xjob-mat NO-LOCK
                 WHERE xjob-mat.company  EQ cocode
                   AND xjob-mat.job      EQ job-mat.job
                   AND xjob-mat.job-no   EQ job-mat.job-no
                   AND xjob-mat.job-no2  EQ job-mat.job-no2
                   AND xjob-mat.frm      EQ job-mat.frm
                   AND xjob-mat.blank-no EQ job-mat.blank-no
                   AND xjob-mat.i-no     ne job-mat.i-no,
                   
                 FIRST xitem NO-LOCK
                 WHERE xitem.company  EQ cocode 
                   AND xitem.i-no     EQ xjob-mat.i-no
                   AND xitem.mat-type EQ "A":
                   
                 i = i + 1.

                 FIND FIRST reftable WHERE
                      reftable.reftable EQ "util/b-hrms-x.w" AND
                      reftable.company  EQ cocode AND
                      reftable.code2    EQ xitem.i-no
                      NO-LOCK NO-ERROR.

                 IF AVAIL reftable THEN
                 DO:
                    lv-board-adder-vend-rm[i] = INT(reftable.CODE) NO-ERROR.
                    RELEASE reftable.
                 END.
                  
                 IF i GE 6 THEN LEAVE.
             END. /*each xjob-mat*/
          END. /*avail job-mat*/
       END. /*avail job*/

       /* ADDERS */
       DO i = 1 TO 6:
          PUT lv-board-adder-vend-rm[i]                 FORMAT "9999".
       END. 
          
    END. /*Trilakes*/

    /* 1 blank space */
    PUT FILL(" ",1)                                 FORMAT "x(1)"       skip.
    
    /* D5 */

    /* SPECIAL INSTRUCTIONS */
    v-instr = "".

    FOR EACH notes WHERE notes.rec_key EQ po-ordl.rec_key NO-LOCK:
      v-instr = v-instr + " " + TRIM(notes.note_text).
    END.

    FOR EACH notes WHERE notes.rec_key EQ po-ord.rec_key NO-LOCK:
      v-instr = v-instr + " " + TRIM(notes.note_text).
    END.

    /* Task 09011501 for CSC / 5 Star */
    IF kiwi-char EQ "CSC" THEN DO:
      v-instr = trim(po-ordl.dscr[1]) + " " + trim(po-ordl.dscr[2]) + " " + v-instr.
    END.
    
    /*replace line feed and returns with a space*/
    IF kiwi-char EQ "TRILAKES" THEN
       ASSIGN
          v-instr = REPLACE(v-instr,CHR(10)," ")
          v-instr = REPLACE(v-instr,CHR(13)," ").

    PUT v-instr                                     FORMAT "x(64)".

    /* CSC Job# */
    v-job-no = "".
        
    RELEASE job.
    IF po-ordl.job-no NE "" THEN
    FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
          AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no))) +
                             TRIM(po-ordl.job-no)
          AND job.job-no2 EQ po-ordl.job-no2
        NO-ERROR.

    IF AVAIL job THEN DO:
      FOR EACH job-mch NO-LOCK
          WHERE job-mch.company  EQ cocode
            AND job-mch.job      EQ job.job
            AND job-mch.job-no   EQ job.job-no
            AND job-mch.job-no2  EQ job.job-no2
            AND job-mch.frm      EQ po-ordl.s-num
          BY job-mch.line:
        v-job-no = SUBSTR(job-mch.m-code,1,1).
        LEAVE.
      END.
      IF v-job-no EQ "" THEN
      FOR EACH job-mch NO-LOCK
          WHERE job-mch.company  eq cocode
            AND job-mch.job      eq job.job
            AND job-mch.job-no   eq job.job-no
            AND job-mch.job-no2  eq job.job-no2
          BY job-mch.line:
        v-job-no = SUBSTRING(job-mch.m-code,1,1).
        LEAVE.
      END.

      v-job-no = (IF v-job-no NE "" THEN v-job-no + "-" ELSE "") +
                 TRIM(po-ordl.job-no) + "-" + STRING(po-ordl.job-no2,"99").

      IF po-ordl.job-no EQ "" THEN v-job-no = "".

      IF v-job-no EQ "-" THEN v-job-no = "".
    END.

    PUT v-job-no                                    FORMAT "x(11)".

    /* 53 blank spaces */
    PUT FILL(" ",53)                                FORMAT "x(53)"       SKIP.
  END. /* FOR EACH po-ordl record */

  po-ord.printed = YES.

  IF SEARCH(v-outfile[2]) NE ? THEN DO:
    OUTPUT CLOSE.
    
    IF opsys EQ "unix" THEN
      UNIX SILENT QUOTER -c 1-3000 VALUE(v-outfile[2]) >
                                   VALUE(v-outfile[2] + ".quo").
    ELSE
      DOS  SILENT QUOTER -c 1-3000 VALUE(v-outfile[2]) >
                                   VALUE(v-outfile[2] + ".quo").
                                   
    INPUT FROM VALUE(v-outfile[2] + ".quo").
    
    OUTPUT TO VALUE(v-outfile[4]) BINARY.
    
    REPEAT:
      v-line = FILL(" ",128).
      IMPORT v-line.
      PUT v-line FORMAT "x(128)" SKIP.
    END.
    
    OUTPUT CLOSE.
    
    INPUT CLOSE.
    
    IF OPSYS EQ "unix" THEN
      UNIX SILENT rm VALUE(v-outfile[2] + "*.*").
    ELSE
      DOS SILENT DEL VALUE(v-outfile[2] + "*.*").

    RUN po/ftppo.p (v-outfile[4], "kiwi").

    MESSAGE "Kiwi file:" TRIM(v-outfile[3]) "has been created" 
        VIEW-AS ALERT-BOX.
  END.

  PAUSE 1 NO-MESSAGE.
END. /* FOR EACH po-ord record */

/* end ----------------------------------- Copr. 2004  Advanced Software Inc. */
