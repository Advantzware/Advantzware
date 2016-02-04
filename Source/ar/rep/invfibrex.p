/* ---------------------------------------------- ar/rep/invxprnt.p   */
/* PRINT INVOICE   Xprint form for Pacific PKG             */
/* -------------------------------------------------------------------------- */
/*lines per page make 69?*/

{sys/inc/var.i shared}

{ar/rep/invoice.i}
{custom/notesdef.i}

DEF VAR v-inst         AS CHAR FORMAT "x(80)" EXTENT 4        NO-UNDO.
DEF VAR v-salesman     AS CHAR FORMAT "x(14)"                 NO-UNDO.
DEF VAR v-fob          AS CHAR FORMAT "x(27)"                 NO-UNDO.
DEF VAR v-shipvia      LIKE carrier.dscr                      NO-UNDO.
DEF VAR v-addr3        AS CHAR FORMAT "x(30)"                 NO-UNDO.
DEF VAR v-sold-addr3   AS CHAR FORMAT "x(30)"                 NO-UNDO.
DEF VAR v-shipto-name  AS CHAR FORMAT "x(30)"                 NO-UNDO.
DEF VAR v-shipto-addr  AS CHAR FORMAT "x(30)" EXTENT 2        NO-UNDO.
DEF VAR v-shipto-city  AS CHAR FORMAT "x(15)"                 NO-UNDO.
DEF VAR v-shipto-state AS CHAR FORMAT "x(2)"                  NO-UNDO.
DEF VAR v-shipto-zip   AS CHAR FORMAT "x(10)"                 NO-UNDO.
DEF VAR v-printline    AS INT                                 NO-UNDO.
DEF VAR v-inv-date     AS DATE INIT TODAY FORMAT "99/99/9999" NO-UNDO.
DEF VAR v-date-ship    AS DATE INIT TODAY                     NO-UNDO.
DEF VAR v-p-c          AS CHAR                                NO-UNDO.
DEF VAR lv-bol-no      LIKE oe-bolh.bol-no                    NO-UNDO.
DEF VAR v-lot-no       AS CHAR                                NO-UNDO.
DEF VAR v-terms        AS CHAR FORMAT "X(15)"                 NO-UNDO.

DEF BUFFER xar-inv FOR ar-inv.
DEF BUFFER xar-invl FOR ar-invl.

DEF VAR v          AS INT                         NO-UNDO.
DEF VAR v-ship-qty AS INT FORMAT "99999"          NO-UNDO.
DEF VAR v-price    AS DEC FORMAT ">>>>9.9999"     NO-UNDO.
DEF VAR v-t-price  AS DEC FORMAT ">>>>>>9.99"     NO-UNDO.
DEF VAR v-po-no    LIKE ar-invl.po-no             NO-UNDO.
                                                          
DEF VAR v-price-head   AS CHAR FORMAT "x(5)"      NO-UNDO.
DEF VAR v-subtot-lines AS DEC                     NO-UNDO.

DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.

/* gdm - 04160924 */
DEF BUFFER b-rh FOR rm-rcpth.
DEF BUFFER b-rd FOR rm-rdtlh.

DEF VAR v-addr3a       AS CHAR FORMAT "x(30)"                 NO-UNDO.
DEF VAR v-sold-addr3a  AS CHAR FORMAT "x(30)"                 NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR v-addr-2 AS LOG NO-UNDO.

ASSIGN
  ls-image1 = "images\FWFlogoBW.jpg"
  FILE-INFO:FILE-NAME = ls-image1
  ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FOR EACH report NO-LOCK 
   WHERE report.term-id EQ v-term-id ,
  FIRST ar-inv NO-LOCK
   WHERE recid(ar-inv) EQ report.rec-id ,
  FIRST cust NO-LOCK
   WHERE cust.company EQ ar-inv.company
     AND cust.cust-no EQ ar-inv.cust-no
    BREAK BY ar-inv.cust-no
          BY ar-inv.inv-no:


    FIND FIRST carrier NO-LOCK
      WHERE carrier.company EQ cocode 
        AND carrier.carrier EQ ar-inv.carrier NO-ERROR.
    IF AVAIL carrier 
      THEN 
       ASSIGN v-shipvia = FILL(" ",10 - INT(LENGTH(carrier.dscr) / 2)) 
                        + carrier.dscr.
      ELSE ASSIGN v-shipvia = "".

      FIND FIRST terms WHERE terms.t-code EQ ar-inv.terms NO-LOCK NO-ERROR.
      IF AVAIL terms THEN
        v-terms = FILL(" ",9 - INT(LENGTH(terms.dscr) / 2)) + terms.dscr .
      ELSE v-terms = "" .

    FIND FIRST shipto NO-LOCK 
      WHERE shipto.company eq cocode 
        AND shipto.cust-no eq ar-inv.cust-no 
        AND shipto.ship-id eq ar-inv.ship-id NO-ERROR.
    IF AVAIL shipto 
     THEN 
      ASSIGN  
        v-shipto-name = shipto.ship-name
        v-shipto-addr[1] = shipto.ship-addr[1]
        v-shipto-addr[2] = shipto.ship-addr[2]
        v-shipto-city = shipto.ship-city
        v-shipto-state = shipto.ship-state
        v-shipto-zip = shipto.ship-zip.
    
    ASSIGN
      v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
      v-sold-addr3 = v-shipto-city + ", " + v-shipto-state
                   + "  " + v-shipto-zip
      v-printline = 0
      v-po-no = ""
      v-date-ship = ar-inv.inv-date
      v-salesman = "".

    IF ar-inv.fob-code BEGINS "ORIG" 
      THEN ASSIGN v-fob = "     Origin".
      ELSE ASSIGN v-fob = "  Destination".

    FOR EACH ar-invl FIELDS(sman b-no ord-no) NO-LOCK
      WHERE ar-invl.x-no  EQ ar-inv.x-no 
        BREAK BY ar-invl.i-no:

        DO i = 1 TO 3:

         IF ar-invl.sman[i] NE "" THEN DO:

           FIND FIRST sman NO-LOCK 
             WHERE sman.company EQ cocode 
               AND sman.sman EQ ar-invl.sman[i] NO-ERROR.
           IF AVAIL sman THEN DO:

            ASSIGN v-salesman = FILL(" ",10 - INT(LENGTH(sman.sname) / 2)) 
                              + sman.sname.
            RELEASE sman.
           END.

           LEAVE.
         END. /* IF ar-invl.sman[i] NE */
        END. /* DO i */
        

        FOR EACH oe-bolh NO-LOCK
          WHERE oe-bolh.b-no = ar-invl.b-no 
            AND oe-bolh.ord-no = ar-invl.ord-no :

            ASSIGN v-date-ship = oe-bolh.bol-date.
        END. /* each oe-bolh */

        FIND FIRST oe-bolh 
            WHERE oe-bolh.b-no = ar-invl.b-no NO-LOCK NO-ERROR.
        IF AVAIL oe-bolh 
          THEN ASSIGN v-date-ship = oe-bolh.bol-date.
    END.  /* each ar-invl */

    ASSIGN lv-bol-no = 0.

    FIND FIRST ar-invl NO-LOCK 
        WHERE ar-invl.x-no EQ ar-inv.x-no NO-ERROR.
    IF AVAIL ar-invl 
      THEN 
       ASSIGN
         v-price-head = ar-invl.pr-uom
         lv-bol-no = ar-invl.bol-no.
    
    FOR EACH ar-invl FIELDS(po-no misc i-no) NO-LOCK
      WHERE ar-invl.x-no  EQ ar-inv.x-no
       BY ar-invl.misc
       BY ar-invl.i-no:

       ASSIGN v-po-no = FILL(" ",8 - INT(LENGTH(ar-invl.po-no) / 2)) 
                      + ar-invl.po-no.
       LEAVE.
    END. /* FOR EACH ar-invl */

    ASSIGN v-inv-date = ar-inv.inv-date.

    {ar/rep/invfibrex.i}

    ASSIGN v-subtot-lines = 0.

    FOR EACH ar-invl NO-LOCK
      WHERE ar-invl.x-no  EQ ar-inv.x-no
        AND NOT ar-invl.misc
        BY ar-invl.i-no:
        
        IF v-printline > 50 THEN DO:
           PAGE.
           v-printline = 0.
           {ar/rep/invfibrex.i}
        END.

        ASSIGN
          v-ship-qty  = IF ar-invl.ord-no EQ 0 
                         THEN ar-invl.qty
                         ELSE ar-invl.ship-qty
          v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
          v-t-price = ar-invl.amt
          v-subtot-lines = v-subtot-lines + ar-invl.amt
          v-p-c = "P"
          v-lot-no = "".

        RUN get_lot_no.

        IF v-lot-no EQ "" THEN
           v-lot-no = ar-invl.lot-no.

        FOR EACH oe-boll NO-LOCK
          WHERE oe-boll.company EQ ar-invl.company 
            AND oe-boll.bol-no eq ar-invl.bol-no 
            AND oe-boll.i-no eq ar-invl.i-no 
            AND oe-boll.ord-no EQ ar-invl.ord-no :

            IF oe-boll.p-c THEN ASSIGN v-p-c = "C".
        END. /* FOR EACH oe-boll */

        PUT
          "<P9><C0.2>" ar-invl.qty FORMAT "->,>>>,>>9"
          "<C8>"       v-ship-qty  FORMAT "->,>>>,>>9"
          "<C17>"      ar-invl.ord-no FORMAT ">>>>>>9"
          "<C24>"      STRING(ar-invl.part-no) FORMAT "x(15)"
          "<C37>"      STRING(ar-invl.i-name) FORMAT "x(25)"
          "<C57>"      STRING(v-p-c,"x")
          "<C60.5>"     v-price FORMAT "$>>>,>>9.99<<"
          "<C70>"      STRING(v-price-head) FORMAT "x(4)"
          "<C73>"       ar-invl.amt FORMAT "$->>>,>>9.99"
         SKIP.

        ASSIGN v-printline = v-printline + 1.
        
        PUT "<C8>"     STRING(ar-invl.inv-qty,"->,>>>,>>9") FORMAT "X(10)"
            "<C17.75>" TRIM(STRING(INT(ar-invl.est-no),">>>>>>>"))
            "<C23.75>" STRING(ar-invl.i-no) FORMAT "x(15)".
        
        IF TRIM(ar-invl.part-dscr1) NE "" THEN
           PUT "<C36.8>" ar-invl.part-dscr1 FORMAT "x(25)".
        ELSE
           IF v-lot-no NE "" THEN DO:
              PUT "<C36.8>" v-lot-no FORMAT "X(25)".
              ASSIGN v-lot-no = "".
           END.

        PUT SKIP.
        ASSIGN v-printline = v-printline + 1.
       
        PUT "<C23.75>" ar-invl.po-no.

        IF TRIM(ar-invl.part-dscr2) NE "" 
          THEN PUT "<C36.8>" ar-invl.part-dscr2 FORMAT "X(25)".
          ELSE
           IF v-lot-no NE "" THEN DO:
             PUT "<C36.8>" v-lot-no FORMAT "X(25)".
             ASSIGN v-lot-no = "".
           END.

        PUT SKIP.
        ASSIGN v-printline = v-printline + 1.

        IF v-lot-no NE "" THEN DO:
           PUT "<C36.8>"  v-lot-no FORMAT "X(25)" SKIP.
           ASSIGN v-printline = v-printline + 1.
        END.

        PUT SKIP(1).
     
        ASSIGN v-printline = v-printline + 1.
            
        IF v-printline > 50 THEN DO:
          PAGE.
          v-printline = 0.
          {ar/rep/invfibrex.i}
        END.

    END.  /* each ar-invl */

    IF CAN-FIND(FIRST ar-invl NO-LOCK
                 WHERE ar-invl.x-no  EQ ar-inv.x-no
                 AND ar-invl.misc)
      THEN DO:
        PUT CHR(10)
           "** Miscellaneous Items **" AT 23 SKIP(1).
           v-printline = v-printline + 1.
    END.

    FOR EACH ar-invl NO-LOCK
      WHERE ar-invl.x-no  EQ ar-inv.x-no
        AND ar-invl.misc
        BY ar-invl.i-no:

        IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
               {ar/rep/invfibrex.i}
        END.

        PUT "<C17.4>"  TRIM(STRING(ar-invl.ord-no,">>>>>>>")) 
            "<C23.75>" ar-invl.prep-charge FORMAT "x(20)"
            "<C38.8>"  ar-invl.prep-dscr   FORMAT "x(30)"
            "<C71>"    ar-invl.prep-amt    FORMAT "$->>>,>>9.99"
            SKIP.
        
        ASSIGN
           v-printline = v-printline + 1
           v-subtot-lines = v-subtot-lines + ar-invl.prep-amt.

        IF ar-invl.est-no NE "" OR 
           ar-invl.po-no NE "" 
          THEN DO:

           PUT "<C17.4>" TRIM(STRING(INT(ar-invl.est-no),">>>>>>>"))
               "<C23.75>" "PO#: " ar-invl.po-no 
               SKIP.
           ASSIGN
              v-printline = v-printline + 1.
        END.  
    END.

    PUT "<P10>".

    {custom/notesprtA.i ar-inv v-inst 4}

    DO i = 1 TO 4:
     IF v-inst[i] <> "" THEN DO:                


       IF v-printline > 50 THEN DO:
         PAGE.
         v-printline = 0.
         {ar/rep/invfibrex.i}
       END.

       PUT v-inst[i] SKIP.
       ASSIGN v-printline = v-printline + 1.
     END. /* IF v-inst[i] <> "" */
    END. /* DO i = */

    IF v-printline > 50 THEN DO:
      PAGE.
      v-printline = 0.
      {ar/rep/invfibrex.i}
    END.


   ASSIGN
      v-inv-freight = IF NOT(ar-inv.freight eq 0 or not ar-inv.f-bill) THEN
                      ar-inv.freight 
                      ELSE 0
      v-printline = v-printline + 6.

   PUT "<P6><R59><C1>INTEREST AT THE RATE OF 1 1/2% PER MONTH WILL BE CHARGED ON ALL PAST DUE"
       "<R60><C1>ACCOUNTS. IN THE EVENT OF FAILURE TO PAY ANY OF THE AMOUNT DUE ON THIS"
       "<R61><C1>INVOICE, ALL COLLECTION COSTS AND/OR ATTORNEY FEES IN THE COLLECTION OF"
       "<R62><C1>ANY SUCH AMOUNT WILL BE PAID BY CUSTOMER."
       "<P12><R58><C56.5><#8><FROM><R+4><C81><RECT>"
       "<=8><C57>SUB TOTAL $<C68>" v-subtot-lines FORMAT "-ZZZZZZZZ9.99"
       "<=8><R+1><C57>FREIGHT   $<C68>" v-inv-freight FORMAT "-ZZZZZZZZ9.99"
       "<=8><R+2><C57>SALES TAX $<C68>" ar-inv.tax-amt FORMAT "-ZZZZZZZZ9.99"
       "<=8><R+3><C57>TOTAL DUE $<C68>" ar-inv.gross FORMAT "-ZZZZZZZZ9.99"
       "<=8><R58><C66.7><FROM><C66.7><R62><RECT>"
       "<=8><R59><C56.5><FROM><C81><R59><RECT>"
       "<=8><R60><C56.5><FROM><C81><R60><RECT>"
       "<=8><R61><C56.5><FROM><C81><R61><RECT>"
       "<=8><R62><C56.5><FROM><C81><R62><RECT><P10>".

   PAGE.

   DO TRANSACTION:
      FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
      ASSIGN xar-inv.printed = yes.
      FIND CURRENT xar-inv NO-LOCK.

   END. /* DO TRANSACTION avail ar-inv */ 
 
end. /* each report, ar-inv */


/* gdm - 04160924 */
PROCEDURE get_lot_no:
    
    IF ar-invl.job-no NE "" THEN DO:

        RELEASE reftable.

        FIND FIRST job NO-LOCK
            WHERE job.company EQ ar-invl.company
              AND job.job-no  EQ ar-invl.job-no
              AND job.job-no2 EQ ar-invl.job-no2 NO-ERROR.
        IF AVAIL job THEN
            FIND FIRST reftable NO-LOCK
               WHERE reftable.reftable EQ "jc/jc-calc.p"
                 AND reftable.company  EQ job.company
                 AND reftable.loc      EQ ""
                 AND reftable.code     EQ STRING(job.job,"999999999")
                 AND reftable.code2    EQ ar-invl.i-no
                 USE-INDEX reftable NO-ERROR.
            IF NOT AVAIL reftable THEN
               FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ ar-invl.company
                  AND job-hdr.job-no  EQ ar-invl.job-no
                  AND job-hdr.job-no2 EQ ar-invl.job-no2
                  AND job-hdr.i-no    EQ ar-invl.i-no NO-ERROR.

            IF AVAIL reftable OR AVAIL job-hdr THEN
                FOR EACH rm-rcpth NO-LOCK
                  WHERE rm-rcpth.company   EQ ar-invl.company
                    AND rm-rcpth.job-no    EQ ar-invl.job-no
                    AND rm-rcpth.job-no2   EQ ar-invl.job-no2
                    AND rm-rcpth.rita-code EQ "I" USE-INDEX job,
                 EACH rm-rdtlh NO-LOCK
                  WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                    AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                    AND rm-rdtlh.s-num     EQ (IF AVAIL reftable 
                                               THEN reftable.val[12]
                                               ELSE job-hdr.frm)
                    AND rm-rdtlh.tag       NE "",
                 EACH b-rd NO-LOCK
                  WHERE b-rd.company   EQ rm-rdtlh.company
                    AND b-rd.tag       EQ rm-rdtlh.tag
                    AND b-rd.loc       EQ rm-rdtlh.loc
                    AND b-rd.loc-bin   EQ rm-rdtlh.loc-bin
                    AND b-rd.rita-code EQ "R"
                    AND b-rd.tag2      NE "" USE-INDEX tag,
                 FIRST b-rh NO-LOCK
                  WHERE b-rh.r-no      EQ b-rd.r-no
                    AND b-rh.rita-code EQ b-rd.rita-code
                    AND b-rh.i-no      EQ rm-rcpth.i-no:

                    v-lot-no = b-rd.tag2.                
                    
                END. /* for each */

        END. /* get lot # */ 
END PROCEDURE.

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
