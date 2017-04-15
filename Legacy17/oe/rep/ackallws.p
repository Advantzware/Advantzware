/* ------------------------------------------- oe/rep/ackallws.p GDM 04200907*/
/* ORDER ACKNOLEDGEMENT for N-K-1-ACKHEAD = Allwest                          */
/* ------------------------------------------------------------------------- */

DEF INPUT PARAM ip-prt-revised AS LOG NO-UNDO.

{sys/inc/var.i shared}

{oe/rep/acknowl.i}

DEF VAR v-salesman as char format "x(3)" NO-UNDO.
DEF VAR v-fob as char format "x(27)" NO-UNDO.
DEF VAR v-shipvia like carrier.dscr NO-UNDO.
DEF VAR v-addr3 as char format "x(30)" NO-UNDO.
DEF VAR v-addr4 as char format "x(30)" NO-UNDO.
DEF VAR v-sold-addr3 as char format "x(30)" NO-UNDO.
DEF VAR v-line as INT NO-UNDO.
DEF VAR v-printline as int NO-UNDO.
DEF VAR v-ackhead as char format "x(32)" init
  "A C K N O W L E D G E M E N T" NO-UNDO.
DEF VAR v-len as int NO-UNDO.
DEF VAR v-totord as dec format "->>,>>>,>>9.99" NO-UNDO.
DEF VAR v-totlin as dec format "->>,>>>,>>9.99".
DEF VAR v-ans as log init no NO-UNDO.
DEF VAR lcnt as int init 1 NO-UNDO.
DEF VAR pagebreak as int init 28 NO-UNDO.
DEF VAR v-cust-phone as char format "(999)999-9999" no-undo.
DEF VAR v-part like oe-ordl.part-no no-undo.
DEF VAR v-tax-rate     as dec format ">,>>9.99<<<".
DEF VAR v-frt-tax-rate like v-tax-rate.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORMAT "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORMAT "x(50)" NO-UNDO.

IF cocode = "002" THEN ls-image1 = "images\allwestl.jpg".
ELSE ASSIGN ls-image1 = "images\allwestl.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORMAT "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
DEF VAR v-t-tax      as   dec extent 3 NO-UNDO.
DEF VAR v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-billinst AS cha FORMAT "x(70)" EXTENT 4 NO-UNDO.
DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR lv-first-note AS LOG NO-UNDO.
DEF VAR v-comp-add5 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-inst AS cha FORMAT "x(80)" EXTENT 4 NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-line-print AS INT INIT 45 NO-UNDO.
DEF VAR v-ext-price AS DEC NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT INIT 1 NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEF VAR lv-is-fgnote AS LOG NO-UNDO.
DEF VAR v-pdscr1 AS cha NO-UNDO.
DEF VAR v-pdscr2 AS cha NO-UNDO.
DEF VAR v-pdscr3 AS cha NO-UNDO.
DEF VAR lv-prt-sts AS cha NO-UNDO.
DEF VAR lv-prt-date AS DATE FORMAT "99/99/9999" NO-UNDO.
DEF VAR lv-prt-time AS cha NO-UNDO.
DEF VAR v-prod-code AS cha NO-UNDO.
DEF VAR v-whse-txt AS cha FORMAT "x(75)" NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "ACKHEAD" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company eq cocode no-lock no-error.
{custom/notesdef.i}

 ASSIGN v-comp-add1 = ""
        v-comp-add2 = ""
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = ""
        lv-email = ""
        lv-comp-name = "".

 IF lv-display-comp THEN DO:
        FIND FIRST cust WHERE cust.company = cocode AND
                              cust.active = "X" NO-LOCK NO-ERROR.
      /*  ASSIGN v-comp-add1 = company.addr[1]
               v-comp-add2 = company.addr[2]
               v-comp-add3 = company.city + ", " + company.st + "  " + company.zip
               v-comp-add4 = "Phone:  " + IF AVAIL cust THEN string(cust.area-code,"(999)") + string(cust.phone,"999-9999") ELSE "" 
               v-comp-add5 = "Fax     :  " + IF AVAIL cust THEN string(cust.fax,"(999)999-9999") ELSE ""
               lv-email    = "Email:  " + IF AVAIL cust THEN cust.email ELSE ""
               lv-comp-name = company.NAME   
               .
      */ 
       IF AVAIL cust THEN
          ASSIGN v-comp-add1 = cust.addr[1]
                 v-comp-add2 = cust.addr[2]
                 v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
                 v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
                 v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
                 lv-email    = "Email:  " + cust.email 
                 lv-comp-name = cust.NAME   
                 .
 END.

ASSIGN ll-calc-disc-first = NO.
FOR EACH sys-ctrl
  WHERE sys-ctrl.company  EQ cocode
    AND sys-ctrl.name     EQ "INVPRINT"
    AND sys-ctrl.char-fld EQ "Dayton" NO-LOCK:
    ASSIGN ll-calc-disc-first = YES.
    LEAVE.
END.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

FOR EACH report 
  WHERE report.term-id EQ v-term-id NO-LOCK,
 FIRST oe-ord
  WHERE RECID(oe-ord) EQ report.rec-id 
    BREAK BY oe-ord.ord-no:


    IF FIRST-OF(oe-ord.ord-no) THEN lv-tot-pg = 1.
    /*
     if oe-ord.sman[2] eq "" and oe-ord.sman[3] eq "" 
       then v-salesman = oe-ord.sman[1].
       else v-salesman = oe-ord.sman[1] + oe-ord.sman[2] + oe-ord.sman[3].
    */
    IF oe-ord.sman[1] <> "" THEN DO:
      FIND FIRST sman 
        WHERE sman.company EQ oe-ord.company
          AND sman.sman    EQ oe-ord.sman[1] NO-LOCK NO-ERROR.

        ASSIGN  v-salesman = IF AVAIL sman THEN sman.sname ELSE oe-ord.sman[1].

    END.

    IF oe-ord.fob-code EQ "ORIG" 
      THEN ASSIGN v-fob = "Origin".
      ELSE ASSIGN v-fob = "Destination".

    FIND FIRST carrier
      WHERE carrier.company EQ oe-ord.company
        AND carrier.carrier EQ oe-ord.carrier no-lock no-error.
    IF AVAIL carrier 
      THEN ASSIGN v-shipvia = carrier.dscr.
      ELSE ASSIGN v-shipvia = "".

    ASSIGN 
      v-printline  = 0
      v-addr3      = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
      v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
                       "  " + oe-ord.sold-zip
        v-line = 1.

    IF oe-ord.sold-city = ""  AND 
       oe-ord.sold-state = "" AND 
       oe-ord.sold-zip = "" 
      THEN ASSIGN v-sold-addr3 = v-addr3.

    FIND FIRST cust
      WHERE cust.company EQ cocode
        AND cust.cust-no EQ oe-ord.cust-no NO-LOCK NO-ERROR.
    IF AVAIL cust THEN ASSIGN v-cust-phone = cust.area-code + cust.phone.

    /* get q-no for first item */
    ASSIGN v-q-no = 0.

    FIND FIRST oe-ordl 
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  eq oe-ord.ord-no NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl THEN DO:

     FIND FIRST reftable NO-LOCK
       WHERE reftable.reftable EQ "oe-ordl.q-no"
         AND reftable.company EQ oe-ordl.company
         AND reftable.loc EQ STRING(oe-ordl.ord-no,"9999999999")
         AND reftable.CODE EQ oe-ordl.i-no
         AND reftable.code2 EQ STRING(oe-ordl.LINE,"9999999999") NO-ERROR.

     IF AVAIL reftable THEN v-q-no = reftable.val[1].

    END.

    /* get total page-num */
    ASSIGN ln-cnt = 0.

    FOR EACH oe-ordl 
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no 
        AND oe-ordl.LINE > 0 NO-LOCK:

        ASSIGN ln-cnt = ln-cnt + 3.
        IF oe-ordl.part-dscr1 NE "" THEN ASSIGN ln-cnt = ln-cnt + 1.

        IF oe-ordl.part-dscr2 NE "" THEN ASSIGN ln-cnt = ln-cnt + 1.

        FOR EACH oe-rel NO-LOCK
          WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
            AND ((oe-rel.link-no EQ 0 AND v-schrel) OR
                  (oe-rel.link-no NE 0 AND v-actrel))
           BREAK BY oe-rel.link-no DESC:

            IF FIRST-OF(oe-rel.link-no) THEN DO:
              IF oe-rel.link-no EQ 0 
                THEN ASSIGN ln-cnt = ln-cnt + 1.
                ELSE 
                 IF FIRST(oe-rel.link-no) THEN ASSIGN ln-cnt = ln-cnt + 1.

              ASSIGN ln-cnt = ln-cnt + 1.              
            END.

            ASSIGN v-addr4 = "".

            IF v-shipto THEN DO:
              FIND FIRST shipto 
                WHERE shipto.company EQ cocode
                  AND shipto.cust-no EQ oe-rel.cust-no
                  AND shipto.ship-id EQ oe-rel.ship-id NO-LOCK NO-ERROR.
              IF AVAIL shipto THEN DO:
                ASSIGN 
                  ln-cnt  = ln-cnt + 1
                  v-addr4 = shipto.ship-city + ", " +
                            shipto.ship-state + "  " + shipto.ship-zip.
              END.

              IF shipto.ship-addr[1] <> "" THEN ASSIGN ln-cnt = ln-cnt + 1.

              IF shipto.ship-addr[2] <> "" THEN ASSIGN ln-cnt = ln-cnt + 1.

              IF v-addr4 <> ""             THEN ASSIGN ln-cnt = ln-cnt + 1.

            END.
        END.

        IF v-prntinst THEN DO:
          FIND FIRST itemfg 
            {sys/look/itemfgrlW.i}
            AND itemfg.i-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.

            {custom/notesprt.i itemfg v-inst 4}

            ASSIGN lv-is-fgnote = NO.

            DO i = 1 TO 4:
              IF v-inst[i] <> "" 
                THEN 
                 ASSIGN 
                   ln-cnt = ln-cnt + 1
                   lv-is-fgnote = YES.
            END.

            IF lv-is-fgnote THEN ASSIGN ln-cnt = ln-cnt + 2
                                        lv-is-fgnote = NO.
        END.
    END.

    FOR EACH oe-ordm NO-LOCK 
      WHERE oe-ordm.company eq oe-ord.company 
        AND oe-ordm.ord-no eq oe-ord.ord-no 
       BREAK BY ord-no:

        IF FIRST(oe-ordm.ord-no) 
          THEN ASSIGN ln-cnt = ln-cnt + 2.

        ASSIGN ln-cnt = ln-cnt + 1.
    END.

    IF oe-ord.f-bill 
      THEN ASSIGN ln-cnt = ln-cnt + 1.

    ASSIGN lv-tot-pg = IF (ln-cnt MOD 27) = 0
                         THEN TRUNC( ln-cnt / 27,0)
                         ELSE lv-tot-pg + TRUNC( ln-cnt / 27,0) .

    ASSIGN lv-prt-sts = IF ip-prt-revised 
                          THEN "<FGCOLOR=RED>REVISED<FGCOLOR=BLACK>" 
                          ELSE "<FGCOLOR=BLACK>ORIGINAL<FGCOLOR=BLACK>" 
           lv-prt-date = TODAY
           lv-prt-time = STRING(TIME,"hh:mm am").

   {oe/rep/ackallws.i}


   FOR EACH oe-ordl
     WHERE oe-ordl.company eq oe-ord.company
       AND oe-ordl.ord-no  eq oe-ord.ord-no
       AND oe-ordl.LINE > 0 NO-LOCK:

       IF v-printline ge lv-line-print THEN DO:

         PUT 
             "<R59><C65>Page " 
               STRING(PAGE-NUM - lv-pg-num,">>9") + " of " + 
               string(lv-tot-pg) FORMAT "x(20)" .
         PAGE .
         {oe/rep/ackallws.i}
         ASSIGN v-printline = 20.          
       END.



       ASSIGN
         v-ext-price = oe-ordl.t-price.
         v-part      = IF oe-ordl.i-no NE oe-ordl.part-no 
                         THEN oe-ordl.part-no
                         ELSE oe-ordl.part-no.
         v-prod-code = IF INDEX("ON",oe-ordl.TYPE-code) > 0 
                         THEN "NEW"
                         ELSE 
                          IF oe-ordl.type-code = "R" 
                            THEN "REPEAT"
                            ELSE "".


       PUT            
           v-part         AT 2                       SPACE(1)             
           v-prod-code    FORMAT "x(6)"              SPACE(1)
           oe-ordl.i-name FORMAT "x(30)"             SPACE(1)
           STRING(oe-ord.ord-no)                     SPACE(1)  
           oe-ordl.qty    FORMAT "->>>,>>>,>>9" 
           oe-ordl.price  FORMAT "$->>>>,>>9.99<<<<" SPACE(4)
           oe-ordl.pr-uom                            SPACE(2) 
           v-ext-price    FORMAT "$->>>>,>>9.99"    
           SKIP.

       ASSIGN v-printline = v-printline + 1.     
        
       ASSIGN 
         v-pdscr1 = oe-ordl.part-dscr1
         v-pdscr2 = oe-ordl.part-dscr2
         v-pdscr3 = oe-ordl.i-no.
       
       IF v-pdscr1 = "" 
         THEN
          ASSIGN 
           v-pdscr1 = v-pdscr2
           v-pdscr2 = v-pdscr3
           v-pdscr3 = "".

       IF v-pdscr1 = ""
         THEN 
          ASSIGN 
           v-pdscr1 = v-pdscr2
           v-pdscr2 = v-pdscr3
           v-pdscr3 = "".

       IF v-printline GE lv-line-print THEN DO:
         
         PUT 
             "<R59><C65>Page " 
              STRING(PAGE-NUM - lv-pg-num,">>9") + " of " + 
              STRING(lv-tot-pg) FORMAT "x(20)" .
         PAGE .
         {oe/rep/ackallws.i}

         ASSIGN v-printline = 20.          
       END.

       PUT oe-ordl.po-no AT 2
           v-pdscr1 FORMAT "x(30)" AT 25  
           SPACE(1)
           v-q-no
           SKIP.

       ASSIGN v-printline = v-printline + 1.

       IF v-pdscr2 <> "" THEN DO:
         IF v-printline GE lv-line-print THEN DO:

           PUT 
               "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORMAT "x(20)" .
               PAGE .
           {oe/rep/ackallws.i}
           ASSIGN v-printline = 20.          
         END.

         PUT v-pdscr2 FORMAT "x(30)" AT 25  
             SKIP.
         ASSIGN v-printline = v-printline + 1.
       END.
       
       IF v-schrel 
         THEN
          FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND oe-rel.link-no EQ 0
            BREAK by oe-rel.rel-date:
            
            RUN print-rels (FIRST(oe-rel.rel-date)).
          END.  /* for each oe-rel  */

       IF v-actrel 
         THEN
          FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND oe-rel.link-no NE 0
            BREAK by oe-rel.rel-date:

            RUN print-rels (FIRST(oe-rel.rel-date)).
          END.   /* for each oe-rel  */

       ASSIGN v-line = v-line + 1.

       IF oe-ordl.pr-uom BEGINS "L" AND 
          oe-ordl.pr-uom NE "LB" 
         THEN ASSIGN v-totlin = oe-ordl.price * IF oe-ordl.qty LT 0 
                                                  THEN -1 ELSE 1.
         ELSE
          IF oe-ordl.pr-uom eq "CS" THEN DO:
            FIND FIRST itemfg 
              {sys/look/itemfgrlW.i}
               AND itemfg.i-no eq oe-ordl.i-no NO-LOCK NO-ERROR.
            
            ASSIGN 
             v-totlin = oe-ordl.qty /
                       (IF oe-ordl.cas-cnt ne 0 
                          THEN oe-ordl.cas-cnt 
                          ELSE
                           IF AVAIL itemfg AND 
                              itemfg.case-count NE 0
                             THEN itemfg.case-count ELSE 1) * oe-ordl.price.
          END.
          ELSE
           IF oe-ordl.pr-uom EQ "C" 
             THEN ASSIGN v-totlin = oe-ordl.qty / 100 * oe-ordl.price.
             ELSE
              IF oe-ordl.pr-uom EQ "M" 
                THEN ASSIGN v-totlin = oe-ordl.qty / 1000 * oe-ordl.price.
                ELSE /** DEFAULT TO EACH **/
                    ASSIGN v-totlin = oe-ordl.qty * oe-ordl.price.


       ASSIGN v-totlin = ROUND(v-totlin,2).

       IF oe-ordl.disc NE 0 
         THEN 
          ASSIGN v-totlin = IF ll-calc-disc-first 
                              THEN (v-totlin - ROUND(v-totlin * oe-ordl.disc / 
                                    100,2))
                              ELSE ROUND(v-totlin * (1 - 
                                        (oe-ordl.disc / 100)),2).

       ASSIGN v-totord = v-totord + v-totlin.

/*  
        if v-printline ge lv-line-print then
        do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORMAT "x(20)" .
            PAGE .
            {oe/rep/ackallws.i}
            assign v-printline = 20.          
        end.
*/
       IF v-prntinst THEN DO:
           
           /* print spec notes */
           FIND FIRST itemfg 
             {sys/look/itemfgrlW.i}
             AND itemfg.i-no eq oe-ordl.i-no NO-LOCK NO-ERROR.
           IF AVAIL itemfg THEN DO:
             
             ASSIGN lv-first-note = yes.

             {custom/notesprt.i itemfg v-inst 4}

             DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:
               IF v-printline GE lv-line-print THEN DO:

                 PUT 
                     "<R59><C65>Page " 
                     STRING(PAGE-NUM - lv-pg-num,">>9") + " of " + 
                     STRING(lv-tot-pg) FORMAT "x(20)" .
                     PAGE .

                     {oe/rep/ackallws.i}

                     ASSIGN v-printline = 20.          
               END.

               IF lv-first-note THEN DO:
                 PUT SKIP(1).

                 ASSIGN 
                  v-printline = v-printline + 1
                  lv-first-note = NO.
               END.

               PUT v-inst[i] SKIP.

               ASSIGN v-printline = v-printline + 1.
              END.
             END.

             IF NOT lv-first-note THEN DO:
              PUT SKIP(1).
              ASSIGN v-printline = v-printline + 1.           
             END.
           END.
       END.  /* v-prntinst */

       PUT SKIP(1).

       ASSIGN v-printline = v-printline + 1.           

   END. /* each oe-ordl */

   FOR EACH oe-ordm NO-LOCK 
     WHERE oe-ordm.company EQ oe-ord.company 
       AND oe-ordm.ord-no  EQ oe-ord.ord-no 
     BREAK BY ord-no:


       IF v-printline ge lv-line-print THEN DO:

         PUT 
             "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORMAT "x(20)" .
             PAGE .
             {oe/rep/ackallws.i}
             
             assign v-printline = 20.          
       END.

       IF FIRST(oe-ordm.ord-no) THEN DO:

         PUT "** Miscellaneous Items **" at 25 .

         IF v-print-fmt EQ "HOP" THEN PUT "Taxable" AT 62.

         PUT SKIP(1).

         ASSIGN v-printline = v-printline + 2.
       END.

       IF v-printline GE lv-line-print THEN DO:

         PUT 
             "<R59><C65>Page " STRING(PAGE-NUM - lv-pg-num,">>9") + " of " + 
                               STRING(lv-tot-pg) FORMAT "x(20)" .

         PAGE .
         {oe/rep/ackallws.i}

         ASSIGN v-printline = 20.          
       END.

       IF oe-ordm.bill EQ "N" 
         THEN 
           PUT /*v-line FORMAT ">>>9" AT 25 SPACE(3)*/
               oe-ordm.charge AT 25 oe-ordm.dscr FORMAT "x(30)" "      N/C" .          
         ELSE
           PUT /*v-line FORMAT ">>>9" AT 25 space(3) */
               oe-ordm.charge AT 25 
               oe-ordm.dscr FORMAT "x(30)" /*SPACE(11)*/             
               oe-ordm.amt FORMAT "$->>>>,>>9.99" AT 101.

       PUT SKIP.

       ASSIGN 
         v-line = v-line + 1
         v-printline = v-printline + 1.

       IF oe-ordm.bill NE "N" THEN ASSIGN v-totord = v-totord + oe-ordm.amt.

   END.  /* each oe-ordm */

   /* print freight*/
   IF oe-ord.f-bill THEN DO:
     
     ASSIGN v-totord = v-totord + oe-ord.t-freight.

     PUT "Freight:" AT 25 oe-ord.t-freight FORMAT "$->>>>,>>9.99" AT 101.

     ASSIGN v-printline = v-printline + 1.
   END.

   /* print billing notes */
   ASSIGN v-billinst = "".

   IF v-prntinst THEN DO i = 1 TO 4:
     ASSIGN v-billinst[i] = oe-ord.bill-i[i].
   END.

   IF cust.ship-day <> 0 
     THEN 
       ASSIGN v-whse-txt = "Maximum warehouse hold term are " + 
                            STRING(cust.ship-day) + " days.".
     ELSE 
       ASSIGN v-whse-txt = "This order will be manufactured and shipped immediately as make and ship.".

   PUT 
/*        "<R54><C1>" v-whse-txt */
        "<R53><C1><#10><P12><B> Comments </B> <P10> " 
        "<R54><C1>" v-billinst[1] 
        "<R55><C1>" v-billinst[2] 
        "<R56><C1>" v-billinst[3] 
        "<R57><C1>" v-billinst[4]   "<P10>Order Total:" AT 90 "<U>"  
        v-totord FORMAT "$>>>>,>>9.99"  "</U><P9>"               
        "<R59><C1>" " ______________________________________(Please sign and fax or email back) " 
        "<R60><C1>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.                   <P16> Thank You! <P10></B>" 
        "<R62><C1>" /*v-whse-txt*/ "<C65>Page "  string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORMAT "x(20)" 
        " " SKIP
         "<P8>          30 Tanforan Avenue, South San Francisco, CA 94080-6608" "    (650) 615-8970    Fax (650) 615-8974     www.allwestcontainer.com".

   ASSIGN
     v-totord        = 0
     oe-ord.ack-prnt = YES
     v-printline = v-printline + 11
     lv-pg-num = PAGE-NUM.

   
   IF v-printline <= 66 THEN PAGE. /*PUT SKIP(60 - v-printline). */

END. /* each oe-ord */

RETURN.

PROCEDURE print-rels:
    DEF INPUT PARAM ip-first AS LOG NO-UNDO.

    DO WITH with frame sched-rel DOWN:
      
      IF ip-first THEN DO:

        ASSIGN lcnt = 1.

        IF oe-rel.link-no eq 0 THEN DO:          
          IF v-printline ge lv-line-print THEN DO:
            PUT 
                "<R59><C65>Page " STRING(PAGE-NUM - lv-pg-num,">>9") + " of " + 
                                  STRING(lv-tot-pg) FORMAT "x(20)" .
            PAGE .
            {oe/rep/ackallws.i}
            ASSIGN v-printline = 20.          
          END.

          PUT 
              SKIP
              "Scheduled Releases:" at 25 SKIP .
          ASSIGN v-printline = v-printline + 2.
        END.
        ELSE DO:
         IF v-printline GE lv-line-print THEN DO:
             PUT "<R59><C65>Page " STRING(PAGE-NUM - lv-pg-num,">>9") + " of " +
                                   STRING(lv-tot-pg) FORMAT "x(20)" .
             PAGE .
             {oe/rep/ackallws.i}
             ASSIGN v-printline = 20.          
         END.

         PUT "Actual Releases:" at 25 SKIP.

         ASSIGN v-printline = v-printline + 1.  
        END.
      END.

      {oe/rel-stat.i lv-stat}

      IF AVAIL oe-rell 
        THEN
         FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
         
         ASSIGN ld-date = IF AVAIL oe-relh
                            THEN oe-relh.rel-date 
                            ELSE oe-rel.rel-date.
          
         ASSIGN lcnt = lcnt + 1.

         IF v-shipto THEN DO:

           FIND FIRST shipto
             WHERE shipto.company EQ cocode
               AND shipto.cust-no EQ oe-rel.cust-no
               AND shipto.ship-id EQ oe-rel.ship-id NO-LOCK NO-ERROR.
           IF AVAIL shipto 
             THEN 
              ASSIGN v-addr4 = shipto.ship-city + ", " +
                               shipto.ship-state + "  " + shipto.ship-zip.

           IF v-printline GE lv-line-print THEN DO:

             PUT 
                 "<R59><C65>Page " STRING(PAGE-NUM - lv-pg-num,">>9") + " of " +
                                   STRING(lv-tot-pg) FORMAT "x(20)" .
             PAGE .
             {oe/rep/ackallws.i}
             ASSIGN v-printline = 20.          
           END.

           IF AVAIL shipto THEN DO:
             PUT 
                 oe-rel.po-no AT 2 
                 shipto.ship-name AT 25 FORMAT "x(28)" 
                 /*lcnt AT 55 FORMAT ">>9" SPACE(1)*/  
                 ld-date AT 59 (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty) 
                 SKIP .
             ASSIGN v-printline = v-printline + 1.

             IF shipto.ship-addr[1] <> "" THEN DO:
               IF v-printline GE lv-line-print THEN DO:

                 PUT "<R59><C65>Page " STRING(PAGE-NUM - lv-pg-num,">>9") + " of " +
                                       STRING(lv-tot-pg) FORMAT "x(20)" .
                 PAGE .
                 {oe/rep/ackallws.i}
                 assign v-printline = 20.          
               END.
               
               PUT shipto.ship-addr[1] AT 25  SKIP.
               ASSIGN v-printline = v-printline + 1.
             END.

             IF shipto.ship-addr[2] <> "" THEN DO:
               IF v-printline ge lv-line-print THEN DO:

                 PUT "<R59><C65>Page " STRING(PAGE-NUM - lv-pg-num,">>9") + " of " +
                                       STRING(lv-tot-pg) FORMAT "x(20)" .
                 PAGE .
                 {oe/rep/ackallws.i}
                 assign v-printline = 20.          
               END.

               PUT shipto.ship-addr[2] AT 25 SKIP.
               ASSIGN v-printline = v-printline + 1.

             END.

             IF v-addr4 <> "" THEN DO:
               IF v-printline ge lv-line-print THEN DO:
                 PUT "<R59><C65>Page " STRING(PAGE-NUM - lv-pg-num,">>9") + " of " +
                                       STRING(lv-tot-pg) FORMAT "x(20)" .
                 PAGE .
                 {oe/rep/ackallws.i}
                 ASSIGN v-printline = 20.          
               END.

               PUT v-addr4 AT 25  SKIP.
               ASSIGN v-printline = v-printline + 1.
             END.
           END.
         END.

         PUT SKIP(1).
         ASSIGN v-printline = v-printline + 1.
    END.

END PROCEDURE.

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
