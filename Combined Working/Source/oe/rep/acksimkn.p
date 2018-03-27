/* ------------------------------------------- oe/rep/acksimkn.p GDM 04200907*/
/* ORDER ACKNOLEDGEMENT for N-K-1-ACKHEAD = SIMKINS                          */
/* ------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/acknowl.i}

{custom/formtext.i NEW} 

DEF VAR v-salesman   AS CHAR FORMAT "x(3)"  NO-UNDO.
DEF VAR v-fob        AS CHAR FORMAT "x(27)" NO-UNDO.
DEF VAR v-shipvia    LIKE carrier.dscr      NO-UNDO.
DEF VAR v-addr3      AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-addr4      AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-sold-addr3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-line       AS INT NO-UNDO.
DEF VAR v-printline  AS INT NO-UNDO.
DEF VAR v-ackhead    AS CHAR FORMAT "x(32)" INIT
  "A C K N O W L E D G E M E N T" NO-UNDO.

DEF VAR v-len              AS INT                         NO-UNDO.
DEF VAR v-totord           AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR v-totlin           AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR v-ans              AS LOG INIT NO                 NO-UNDO.
DEF VAR lcnt               AS INT INIT 1                  NO-UNDO.
DEF VAR pagebreak          AS INT INIT 28                 NO-UNDO.
DEF VAR v-cust-phone       AS CHAR FORMAT "(999)999-9999" NO-UNDO.
DEF VAR v-part             LIKE oe-ordl.part-no           NO-UNDO.
DEF VAR v-tax-rate         AS DEC FORMAT ">,>>9.99<<<".
DEF VAR v-frt-tax-rate     LIKE v-tax-rate.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1    AS CHAR                NO-UNDO.
DEF VAR ls-image2    AS CHAR                NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS CHAR FORMAT "x(200)" NO-UNDO.

/*
ASSIGN ls-image1 = "images\pacific1.bmp"
       ls-image2 = "images\pacific2.bmp".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".
*/

DEF VAR v-tel     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fax     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact AS CHAR FORMAT "x(20)" NO-UNDO .

DEF VAR v-comp-add1  AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2  AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3  AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4  AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC                 NO-UNDO.
DEF VAR v-quo-total  AS DEC                 NO-UNDO.
DEF VAR v-t-tax      AS DEC EXTENT 3        NO-UNDO.
DEF VAR v-bot-lab    AS CHAR FORMAT "x(63)" EXTENT 3 NO-UNDO.

DEF VAR v-q-no          LIKE oe-ord.q-no                NO-UNDO.
DEF VAR v-billinst      AS CHAR FORMAT "x(70)" EXTENT 4 NO-UNDO.
DEF VAR v-inst-lines    AS INT                          NO-UNDO.
DEF VAR lv-first-note   AS LOG                          NO-UNDO.
DEF VAR v-comp-add5     AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR lv-display-comp AS LOG                          NO-UNDO.
DEF VAR lv-email        AS CHAR FORM "x(40)"            NO-UNDO.
DEF VAR lv-comp-name    AS CHAR FORM "x(30)"            NO-UNDO.
DEF VAR v-inst          AS CHAR FORM "x(80)" EXTENT 4   NO-UNDO.
DEF VAR lv-comp-color   AS CHAR                         NO-UNDO.
DEF VAR lv-other-color  AS CHAR INIT "BLACK"            NO-UNDO.
DEF VAR lv-line-print   AS INT INIT 44                  NO-UNDO.
DEF VAR lv-due-date     AS DATE                         NO-UNDO.
DEF VAR v-text          AS CHAR                         NO-UNDO.
DEF VAR v-licnt         AS INT                          NO-UNDO.
DEF VAR v-notes         AS CHAR FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEF VAR note-count      AS INT                          NO-UNDO. 
DEF VAR v-text1         AS CHAR FORMAT "x(170)" EXTENT 10 NO-UNDO.


FIND FIRST sys-ctrl 
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "ACKHEAD" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl AND 
   sys-ctrl.log-fld 
  THEN ASSIGN lv-display-comp = YES.
  ELSE ASSIGN lv-display-comp = NO.

FIND FIRST sys-ctrl 
  WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl 
  THEN ASSIGN lv-comp-color = sys-ctrl.char-fld.
  ELSE ASSIGN lv-comp-color = "BLACK".

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
{custom/notesdef.i}

ASSIGN 
  v-comp-add1 = ""
  v-comp-add2 = ""
  v-comp-add3 = ""
  v-comp-add4 = ""
  v-comp-add5 = ""
  lv-email = ""
  lv-comp-name = "".

IF lv-display-comp THEN DO:

  FIND FIRST cust 
    WHERE cust.company = cocode 
      AND cust.active = "X" NO-LOCK NO-ERROR.
  IF AVAIL cust 
    THEN
     ASSIGN 
      v-comp-add1 = cust.addr[1]
      v-comp-add2 = cust.addr[2]
      v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
      v-comp-add4 = "Phone:  " + STRING(cust.area-code,"(999)") + STRING(cust.phone,"999-9999")
      v-comp-add5 = "Fax     :  " + STRING(cust.fax,"(999)999-9999") 
      lv-email    = "Email:  " + cust.email 
      lv-comp-name = cust.NAME   .
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
  FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id:

   IF oe-ord.sman[2] EQ "" AND 
      oe-ord.sman[3] EQ "" 
     THEN ASSIGN v-salesman = oe-ord.sman[1].
     ELSE ASSIGN v-salesman = oe-ord.sman[1] + oe-ord.sman[2] + oe-ord.sman[3].

   IF oe-ord.fob-code EQ "ORIG" 
     THEN ASSIGN v-fob = "Origin".
     ELSE ASSIGN v-fob = "Destination".

   FIND FIRST carrier
     WHERE carrier.company EQ oe-ord.company
       AND carrier.carrier EQ oe-ord.carrier NO-LOCK NO-ERROR.
   IF AVAIL carrier 
     THEN ASSIGN v-shipvia = carrier.dscr.
     ELSE ASSIGN v-shipvia = "".

   ASSIGN 
     v-printline = 0
     v-addr3 = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
     v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
                    "  " + oe-ord.sold-zip
     v-line = 1.

   IF oe-ord.sold-city = "" AND 
      oe-ord.sold-state = "" AND 
      oe-ord.sold-zip = "" 
     THEN ASSIGN v-sold-addr3 = v-addr3.

   FIND FIRST cust
     WHERE cust.company EQ cocode
       AND cust.cust-no EQ oe-ord.cust-no NO-LOCK NO-ERROR.
   IF AVAIL cust 
     THEN ASSIGN v-cust-phone = cust.area-code + cust.phone.

   ASSIGN v-q-no = 0.
   FIND FIRST oe-ordl 
     WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl AND 
       oe-ordl.est-no <> "" 
      THEN DO:

      FIND LAST quotehd 
        WHERE quotehd.company = oe-ordl.company
          AND quotehd.est-no = oe-ordl.est-no NO-LOCK NO-ERROR.
      IF AVAIL quotehd THEN DO:
        
        FIND FIRST quoteitm OF quotehd 
          WHERE quoteitm.part-no = oe-ordl.part-no NO-LOCK NO-ERROR.
        IF AVAIL quoteitm THEN DO:
          ASSIGN v-q-no = quoteitm.q-no.

          FIND FIRST quotEQty 
            WHERE quotEQty.company = oe-ordl.company
              AND quotEQty.loc = quoteitm.loc
              AND quotEQty.q-no = quoteitm.q-no
              AND quotEQty.LINE = quoteitm.LINE
              AND quotEQty.qty = oe-ordl.qty NO-LOCK NO-ERROR.
          IF AVAIL quotEQty THEN ASSIGN v-q-no = quotEQty.q-no.
        END.
      END.
    END.

    ASSIGN lv-due-date = IF AVAIL oe-ordl 
                           THEN oe-ordl.rEQ-date ELSE oe-ord.due-date.

    {oe/rep/acksimkn.i}

    FOR EACH oe-ordl
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no NO-LOCK:

      IF v-printline GE lv-line-print THEN DO:

        PAGE .
        {oe/rep/ackxprnt.i}
        ASSIGN v-printline = 20.          
      END.

      ASSIGN v-part = IF oe-ordl.i-no NE oe-ordl.part-no 
                          THEN oe-ordl.part-no ELSE "".

      PUT 
/*           v-line FORM ">>>9" SPACE(3) */
          oe-ordl.po-no       FORMAT "x(15)"             SPACE(2)
          v-part             FORMAT "x(15)"             SPACE(3) 
          oe-ordl.part-dscr1 FORMAT "x(25)"             SPACE(2)
          oe-ordl.qty        FORMAT "->,>>>,>>9"        SPACE(2)
          oe-ordl.price      FORMAT "->,>>>,>>9.99<<"   SPACE(4)
          oe-ordl.pr-uom  
          SKIP
          .

      ASSIGN v-printline = v-printline + 1.

      IF v-printline GE lv-line-print THEN DO:
        PAGE .
        {oe/rep/ackxprnt.i}
        ASSIGN v-printline = 20.          
      END.


      IF oe-ordl.i-no NE oe-ordl.part-no OR
         oe-ordl.part-dscr1 NE ""        
        THEN DO:

        
        PUT 
           oe-ordl.i-no   AT 18
           oe-ordl.i-name AT 36  SKIP.

       ASSIGN v-printline = v-printline + 1.
      END.

      IF v-printline GE lv-line-print THEN DO: 

        PAGE .
        {oe/rep/ackxprnt.i}
        ASSIGN v-printline = 20.          
      END.

      IF oe-ordl.part-dscr2 NE "" THEN DO:

        PUT 
           oe-ordl.part-dscr2 at 36  SKIP.

        ASSIGN v-printline = v-printline + 1.
      END.

      IF v-printline GE lv-line-print THEN DO:

        PAGE .
        {oe/rep/ackxprnt.i}
        ASSIGN v-printline = 20.          
      END.

      IF v-schrel 
        THEN
         FOR EACH oe-rel
           WHERE oe-rel.company EQ oe-ordl.company
             AND oe-rel.ord-no  EQ oe-ordl.ord-no
             AND oe-rel.i-no    EQ oe-ordl.i-no
             AND oe-rel.line    EQ oe-ordl.line
             AND oe-rel.link-no EQ 0
            NO-LOCK break by oe-rel.rel-date:

           RUN print-rels (FIRST(oe-rel.rel-date)).
         END.  /* FOR EACH oe-rel  */

      IF v-actrel 
        THEN
         FOR EACH oe-rel
           WHERE oe-rel.company EQ oe-ordl.company
             AND oe-rel.ord-no  EQ oe-ordl.ord-no
             AND oe-rel.i-no    EQ oe-ordl.i-no
             AND oe-rel.line    EQ oe-ordl.line
             AND oe-rel.link-no NE 0
             NO-LOCK break by oe-rel.rel-date:

            RUN print-rels (FIRST(oe-rel.rel-date)).
      END.   /* FOR EACH oe-rel  */

      ASSIGN v-line = v-line + 1.

      IF v-printline GE lv-line-print THEN DO:
        PAGE .
        {oe/rep/ackxprnt.i}
        ASSIGN v-printline = 20.          
      END.

      
      IF oe-ordl.pr-uom BEGINS "L" AND 
         oe-ordl.pr-uom NE "LB" 
        THEN ASSIGN v-totlin = oe-ordl.price * IF oe-ordl.qty LT 0 
                                                 THEN -1 ELSE 1.
        ELSE
         IF oe-ordl.pr-uom EQ "CS" THEN DO:
           FIND FIRST itemfg 
             {sys/look/itemfgrlW.i}
             AND itemfg.i-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.

           ASSIGN 
             v-totlin = oe-ordl.qty /
                       (IF oe-ordl.cas-cnt NE 0 
                          THEN oe-ordl.cas-cnt 
                          ELSE
                           IF AVAIL itemfg AND 
                              itemfg.case-count ne 0
                             THEN itemfg.case-count 
                             ELSE 1) * oe-ordl.price.
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
         ASSIGN 
           v-totlin = IF ll-calc-disc-first 
                        THEN (v-totlin - 
                              ROUND(v-totlin * oe-ordl.disc / 100,2))
                        ELSE ROUND(v-totlin * (1 - (oe-ordl.disc / 100)),2).

      ASSIGN v-totord = v-totord + v-totlin.
  
      IF v-printline GE lv-line-print THEN DO:
        PAGE .
        {oe/rep/ackxprnt.i}
        ASSIGN v-printline = 20.          
      END.

      /* print spec notes */
      IF v-prntinst THEN DO:
        FIND FIRST itemfg 
          {sys/look/itemfgrlW.i}
            AND itemfg.i-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL itemfg THEN DO:
          ASSIGN lv-first-note = yes.

          {custom/notesprt.i itemfg v-inst 4}

          DO i = 1 TO 4:

            IF v-inst[i] <> "" THEN DO:
              IF lv-first-note THEN DO:

                PUT SKIP(1).
                ASSIGN v-printline = v-printline + 1.
                lv-first-note = NO.
              END.

              PUT 
                 v-inst[i] SKIP.

              ASSIGN v-printline = v-printline + 1.
            END.

            IF v-printline GE lv-line-print THEN DO:
               PAGE .
               {oe/rep/ackxprnt.i}
               ASSIGN v-printline = 20.          
            END.
          END.

          IF NOT lv-first-note THEN DO:

             PUT SKIP(1).
             ASSIGN v-printline = v-printline + 1.           
          END.

          IF v-printline GE lv-line-print THEN DO:
            PAGE .
            {oe/rep/ackxprnt.i}
            ASSIGN v-printline = 20.          
          END.
        END.
      END. /* if v-prntinst*/      

    END.  /* each oe-ordl */

    /* PRINT PEN NOTES */
    IF v-print-pen-notes = YES THEN DO: 
       FIND FIRST oe-ordl
         WHERE oe-ordl.company EQ oe-ord.company
           AND oe-ordl.ord-no  EQ oe-ord.ord-no NO-LOCK NO-ERROR.
       IF AVAIL oe-ordl THEN
       FIND FIRST  notes NO-LOCK
         WHERE notes.rec_key = oe-ordl.rec_key
           AND TRIM(notes.note_text) NE "" NO-ERROR.
       IF AVAIL notes THEN DO:
   
         PUT "<B>Notes: </B>" SKIP.
   
         ASSIGN  v-printline = v-printline + 1.
   
         FOR EACH notes NO-LOCK
           WHERE notes.rec_key = oe-ordl.rec_key
              BY note_date BY note_time:
   
             FOR EACH tt-formtext: DELETE tt-formtext. END.
   
             ASSIGN 
               v-text = ""
               v-text = v-text + " " + notes.note_text.
   
             DO v-licnt = 1 TO 5:
                CREATE tt-formtext.
                ASSIGN tt-line-no = v-licnt
                       tt-length  = 100. 
             END.
   
             RUN custom/formtext.p (v-text).
   
             ASSIGN 
                  i = 0 v-notes = "" note-count = 0.
   
             FOR EACH tt-formtext:
                ASSIGN i = i + 1.
                IF i <= 5 
                  THEN ASSIGN v-notes[i] = tt-formtext.tt-text.
   
                IF v-notes[i] <> "" THEN note-count = i.
   
             END.
   
             DO i = 1 TO note-count:
                      
               IF v-notes[i] NE "" 
                 THEN PUT  v-notes[i] FORM "x(80)" SKIP(1).
   
               ASSIGN v-printline = v-printline + 2.
               
               IF v-printline GE lv-line-print THEN DO:
                 PAGE .
                 {oe/rep/ackxprnt.i}
                 ASSIGN v-printline = 20.          
               END.
             END.
         END. /* For each notes */
       END. /* FIND FIRST */
    END.

    FOR EACH oe-ordm NO-LOCK 
      WHERE oe-ordm.company EQ oe-ord.company 
        AND oe-ordm.ord-no EQ oe-ord.ord-no 
       BREAK BY ord-no:

        IF v-printline ge lv-line-print THEN DO:
           PAGE .
           {oe/rep/ackxprnt.i}
           ASSIGN v-printline = 20.          
        END.

        IF FIRST(oe-ordm.ord-no) THEN DO:

          PUT 
              "** Miscellaneous Items **" AT 23.

          IF v-print-fmt EQ "HOP" 
            THEN 
             PUT "Taxable" AT 62.

          PUT SKIP(1).

          ASSIGN v-printline = v-printline + 2.
        END.

        IF v-printline ge lv-line-print THEN DO:
            PAGE .
            {oe/rep/ackxprnt.i}
            assign v-printline = 20.          
        END.


        IF oe-ordm.bill EQ "N" 
          THEN
            PUT 
               v-line FORMAT ">>>9" SPACE(3)
               oe-ordm.charge oe-ordm.dscr "      N/C" .          
          ELSE
            PUT 
                v-line FORMAT ">>>9" SPACE(3)
                oe-ordm.charge oe-ordm.dscr SPACE(11)
                oe-ordm.amt.

        PUT SKIP.

        ASSIGN 
            v-line = v-line + 1
            v-printline = v-printline + 2.

        IF v-printline ge lv-line-print THEN DO:
            PAGE .
            {oe/rep/ackxprnt.i}
            assign v-printline = 20.          
        END.

        IF oe-ordm.bill ne "N" 
          THEN ASSIGN v-totord = v-totord + oe-ordm.amt.
    END.  /* each oe-ordm */

    /* print billing notes */
    ASSIGN v-billinst = "".

    IF v-prntinst THEN  
      DO i = 1 TO 4:
      
          v-billinst[i] = oe-ord.bill-i[i].
    END.

    IF v-printline ge lv-line-print THEN DO:
       PAGE .
       {oe/rep/ackxprnt.i}
       ASSIGN v-printline = 20.          
    END.

    ASSIGN
      v-totord        = 0
      oe-ord.ack-prnt = yes.

    PUT 
        "<FArial><R55><C1><#10><P12><B> Comments </B> <P8> " 
        "<R56><C1>" v-billinst[1] 
        "<R57><C1>" v-billinst[2] 
        "<R58><C1>" v-billinst[3] 
        "<R59><C1>" v-billinst[4] 
        "<R61><C1>" " ______________________________________(Please sign and fax back) " 
        "<=10><R-2>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>" .
      ASSIGN v-printline = v-printline + 6.

      IF v-printline <= 66 THEN PAGE. /*PUT SKIP(60 - v-printline). */
END.  /* each oe-ord */

IF v-terms  AND
   TRIM(v-termfile) NE ""
  THEN DO:

   PAGE.

   DEF VAR v-char AS CHAR NO-UNDO.
   DEF VAR v-linecnt AS INT  NO-UNDO.

   INPUT FROM VALUE(TRIM(v-termfile)).
   REPEAT:
    IMPORT UNFORMATTED v-char.
    
    IF TRIM(v-char) NE "" AND LENGTH(TRIM(v-char)) GT 145 THEN DO:

      FOR EACH tt-formtext: DELETE tt-formtext. END.
            
      DO v-licnt = 1 TO 10:
       CREATE tt-formtext.
       ASSIGN tt-line-no = v-licnt
              tt-length  = 145. 
      END.

      RUN custom/formtext.p (v-char).

      ASSIGN  i = 0 v-text1 = "" v-linecnt = 0.

      FOR EACH tt-formtext:
        ASSIGN i = i + 1.

        IF i <= 5 
          THEN ASSIGN v-text1[i] = tt-formtext.tt-text.

        IF v-text1[i] <> "" THEN v-linecnt = i.
      END.

      DO i = 1 TO v-linecnt:                   
        IF v-text1[i] NE "" 
          THEN  PUT  "<FTimes New (W1)><#11><P8>" 
                     v-text1[i] FORMAT "x(145)" SKIP.
      END.
    END.
    ELSE PUT "<FTimes New (W1)><#11><P8>"  v-char FORMAT "x(145)" SKIP. 
  
   END.
   INPUT CLOSE.
   
END.
RETURN.

PROCEDURE print-rels:
    
    DEF INPUT PARAM ip-first AS LOG NO-UNDO.
    
    DO WITH FRAME sched-rel DOWN:

      if v-printline ge lv-line-print THEN DO:
            PAGE .
            {oe/rep/ackxprnt.i}
            assign v-printline = 20.          
      END.

      IF ip-first THEN DO:
        ASSIGN lcnt = 1.
        IF oe-rel.link-no EQ 0 THEN DO:
           put "Scheduled Releases:" at 10  SKIP.
           ASSIGN v-printline = v-printline + 1.
        END.
        ELSE DO:

            PUT "Actual Releases:" at 10 SKIP.
            ASSIGN v-printline = v-printline + 1.
        END.
      END.

      IF v-printline ge lv-line-print THEN DO:
          PAGE .
          {oe/rep/ackxprnt.i}
          assign v-printline = 20.          
      END.

      {oe/rel-stat.i lv-stat}
      IF AVAIL oe-rell 
        THEN
         FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.

      ASSIGN 
        ld-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.

      PUT lcnt AT 10 
          (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty) SPACE(5) 
          ld-date  SKIP.

      ASSIGN
        v-printline = v-printline + 1
        lcnt        = lcnt + 1.

      IF v-shipto THEN DO:
        FIND FIRST shipto
          WHERE shipto.company EQ cocode
           AND shipto.cust-no EQ oe-rel.cust-no
           AND shipto.ship-id EQ oe-rel.ship-id NO-LOCK NO-ERROR.
        IF AVAIL shipto 
          THEN
           ASSIGN 
              v-addr4 = shipto.ship-city + ", " +
                        shipto.ship-state + "  " + shipto.ship-zip.

        IF v-printline GE lv-line-print THEN DO:
           PAGE .
           {oe/rep/ackxprnt.i}
           ASSIGN v-printline = 20.          
        END.

        IF AVAIL shipto THEN DO:

          PUT shipto.ship-name AT 10 SKIP .

          ASSIGN v-printline = v-printline + 1.

          IF v-printline ge lv-line-print THEN DO:
            PAGE .
            {oe/rep/ackxprnt.i}
            ASSIGN v-printline = 20.          
          END.

          IF shipto.ship-addr[1] <> "" THEN DO:

            PUT shipto.ship-addr[1] AT 10  SKIP.

            ASSIGN v-printline = v-printline + 1.

            IF v-printline ge lv-line-print THEN DO:
              PAGE .
              {oe/rep/ackxprnt.i}
              ASSIGN v-printline = 20.          
            END.
          END.

          IF shipto.ship-addr[2] <> "" THEN DO:
           PUT shipto.ship-addr[2] AT 10  SKIP.
           ASSIGN v-printline = v-printline + 1.

           IF v-printline GE lv-line-print THEN DO:
             PAGE .
             {oe/rep/ackxprnt.i}
             ASSIGN v-printline = 20.          
           END.
          END.

          IF v-addr4 <> "" THEN DO:
            PUT v-addr4 AT 10 SKIP.
            ASSIGN v-printline = v-printline + 1.
          END.
        END.
      END.
    END.
END PROCEDURE

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
