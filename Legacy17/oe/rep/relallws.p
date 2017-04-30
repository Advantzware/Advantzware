/* ------------------------------------------- oe/rep/relallws.p GDM 04200906*/
/* REALSE TICKET PRINT for N-K-1-RELPRINT = Allwest                          */
/* ------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

DEF VAR v-units-hdr    AS CHAR FORMAT "x(5)" EXTENT 2 NO-UNDO.
DEF VAR v-zone-hdr     AS CHAR FORMAT "x(10)"         NO-UNDO.
DEF VAR v-zone         LIKE shipto.dest-code          NO-UNDO.
DEF VAR v-part-dscr    LIKE oe-ordl.i-name            NO-UNDO.
DEF VAR v-qty          LIKE oe-rell.qty               NO-UNDO.
DEF VAR v-frt-pay-dscr AS CHAR FORMAT "x(11)" NO-UNDO.

/* === with xprint ====*/
DEF VAR v-term       AS CHAR                NO-UNDO.
DEF VAR ls-image1    AS CHAR                NO-UNDO.
DEF VAR ls-image2    AS CHAR                NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS CHAR FORMAT "x(50)" NO-UNDO.

ASSIGN 
 ls-image1 = "images/allwest.jpg"
 ls-image2 = "images/allwest.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fax     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact AS CHAR FORMAT "x(20)" NO-UNDO.

DEF VAR v-comp-add1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC                          NO-UNDO.
DEF VAR v-quo-total  AS DEC                          NO-UNDO.
DEF VAR v-t-tax      AS DEC EXTENT 3                 NO-UNDO.
DEF VAR v-bot-lab    AS CHAR FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEF VAR v-q-no       LIKE oe-ord.q-no                NO-UNDO.
DEF VAR v-printline  AS INT                          NO-UNDO.
DEF VAR v-ship-i     AS CHAR FORMAT "x(60)" EXTENT 4 NO-UNDO.

DEF VAR ll-display-comp AS LOG                 NO-UNDO.  /* display company address */
DEF VAR ll-consol-rells AS LOG                 NO-UNDO.
DEF VAR lv-comp-name    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR lv-email        AS CHAR FORMAT "x(40)" NO-UNDO.

DEF VAR v-UntCnt        AS CHAR FORMAT "x(10)" NO-UNDO.

DEF VAR lv-comp-color  AS CHAR              NO-UNDO.
DEF VAR lv-other-color AS CHAR INIT "BLACK" NO-UNDO.
DEF VAR lv-comp-unit   AS INT               NO-UNDO.

DEF BUFFER xitemfg FOR itemfg.

DEF SHARED VAR v-print-components AS LOG NO-UNDO.
DEF SHARED VAR s-print-part-no    AS LOG NO-UNDO.

ASSIGN tmpstore = FILL("-",130).

FIND FIRST sys-ctrl NO-LOCK
  WHERE sys-ctrl.company EQ cocode
    and sys-ctrl.name    EQ "RELPRINT" NO-ERROR.
ASSIGN
 ll-display-comp = AVAIL sys-ctrl AND sys-ctrl.log-fld
 ll-consol-rells = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0.

FIND FIRST sys-ctrl 
  WHERE sys-ctrl.company EQ cocode
    and sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl 
  THEN lv-comp-color = sys-ctrl.char-fld.
  ELSE lv-comp-color = "BLACK".

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

ASSIGN v-comp-add1 = ""
       v-comp-add2 = ""
       v-comp-add3 = ""
       v-comp-add4 = ""
       v-comp-add5 = ""
       lv-email = ""
       lv-comp-name = "".

IF ll-display-comp THEN DO:

 FIND FIRST cust NO-LOCK
   WHERE cust.company = cocode 
     AND cust.active = "X"  NO-ERROR.
 IF AVAIL cust 
   THEN
    ASSIGN 
      v-comp-add1 = cust.addr[1]
      v-comp-add2 = cust.addr[2]
      v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
      v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
      v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
      lv-email    = "Email:  " + cust.email 
      lv-comp-name = cust.NAME.
END.

FORMAT
  tt-rell.ord-no
  tt-rell.po-no  AT 8
  locbin[1]      AT 23
  tt-rell.i-no   AT 29  
  oe-ordl.i-name AT 44 FORMAT "x(22)"
  v-UntCnt       AT 75
  tt-rell.qty    AT 85 FORMAT "->>>>>>>9"  
 SKIP
  locbin[2]       AT 23     
  oe-ordl.part-no
  locbin[3]       AT 23
  oe-ordl.part-dscr1 AT 44 FORMAT "x(30)" 
 SKIP
  locbin[4]          AT 23
  oe-ordl.part-dscr2 AT 44 FORMAT "x(30)"
  with down frame relprint no-box no-label STREAM-IO width 110.


FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN v-printline = 0.

IF LOOKUP(v-relprint,"Argrov,Sonoco") GT 0 
  THEN
   ASSIGN
    v-units-hdr[1] = "Units"
    v-units-hdr[2] = "-----".

IF v-zone-p THEN ASSIGN v-zone-hdr = "Route No.:".

{oe/rep/foreachr.i},
  FIRST cust
    WHERE cust.company EQ cocode
      AND cust.cust-no EQ oe-relh.cust-no
    NO-LOCK
    BREAK BY oe-relh.release#:

  RUN oe/custxship.p (oe-relh.company,
                      oe-relh.cust-no,
                      oe-relh.ship-id,
                      BUFFER shipto).
  ASSIGN
    v-tot-qty = 0
    v-weight  = 0
    v-pallets = 0
    v-zone    = IF v-zone-p THEN shipto.dest-code ELSE "".

  FIND FIRST carrier NO-LOCK
    WHERE carrier.company EQ cocode
    AND carrier.carrier EQ oe-relh.carrier NO-ERROR.

  ASSIGN
    v-carrier   = IF AVAIL carrier THEN carrier.dscr ELSE ""
    v-frt-terms = "".

  FOR EACH xoe-rell
    WHERE xoe-rell.company EQ oe-relh.company
      AND xoe-rell.r-no    EQ oe-relh.r-no
    USE-INDEX r-no NO-LOCK,
    FIRST oe-ord NO-LOCK
      WHERE oe-ord.company EQ xoe-rell.company
        AND oe-ord.ord-no  EQ xoe-rell.ord-no:


      CASE oe-ord.frt-pay:
        when "P" THEN v-frt-terms = "Prepaid".
        when "C" THEN v-frt-terms = "Collect".
        when "B" THEN v-frt-terms = "Bill".
        when "T" THEN v-frt-terms = "Third Party".
      END CASE.

      LEAVE.
  END.

  FOR EACH tt-rell: DELETE tt-rell. END.

  /** Calculate the total weight of the released items. **/
  FOR EACH xoe-rell
    WHERE xoe-rell.company EQ cocode
      and xoe-rell.r-no    EQ oe-relh.r-no
    USE-INDEX r-no:

    FIND FIRST xoe-ordl
      WHERE xoe-ordl.company EQ cocode
        AND xoe-ordl.ord-no  EQ xoe-rell.ord-no
        AND xoe-ordl.line    EQ xoe-rell.line
        AND xoe-ordl.i-no    EQ xoe-rell.i-no
      USE-INDEX ord-no NO-LOCK NO-ERROR.

    IF AVAIL xoe-ordl 
      THEN
       ASSIGN
        v-tot-qty = v-tot-qty + xoe-rell.qty
        v-weight = v-weight + (IF xoe-ordl.t-weight ne ? 
                                THEN (ROUND(xoe-ordl.t-weight /
                                   xoe-ordl.qty, 2) * xoe-rell.qty) 
                                ELSE 0).

    IF AVAIL xoe-ordl AND 
       xoe-ordl.est-no NE "" 
      THEN DO:

       FIND FIRST eb NO-LOCK
         WHERE eb.company  EQ xoe-ordl.company
           AND eb.est-no   EQ xoe-ordl.est-no
           AND eb.form-no  EQ xoe-ordl.form-no
           AND eb.blank-no EQ xoe-ordl.blank-no  NO-ERROR.

       IF xoe-ordl.form-no EQ 0 AND
         (xoe-ordl.est-type EQ 2 OR 
          xoe-ordl.est-type EQ 6) 
         THEN DO:

          FOR EACH fg-set
            WHERE fg-set.company EQ xoe-ordl.company
              AND fg-set.set-no  EQ xoe-ordl.i-no NO-LOCK:

              ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
          END.

          IF v-set-qty EQ 0 THEN ASSIGN v-set-qty = 1.

          FOR EACH eb
            WHERE eb.company EQ xoe-ordl.company
              AND eb.est-no  EQ xoe-ordl.est-no
              AND eb.form-no NE 0 NO-LOCK:

            FIND fg-set NO-LOCK
              WHERE fg-set.company EQ xoe-ordl.company
                AND fg-set.set-no  EQ xoe-ordl.i-no
                AND fg-set.part-no EQ eb.stock-no NO-ERROR.

            ASSIGN
              v-part-qty = (IF AVAIL fg-set AND 
                               fg-set.part-qty NE 0 
                              THEN fg-set.part-qty 
                              ELSE 1) / v-set-qty
              v-pallets = v-pallets +
                         (IF xoe-rell.qty-case NE 0 
                           THEN ROUND((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                           ELSE IF eb.cas-cnt NE 0 
                                  THEN ROUND((ROUND((v-tot-qty * v-part-qty) /
                                              eb.cas-cnt, 2) 
                                              / eb.cas-pal) + .49, 0)
                                  ELSE ROUND((ROUND((v-weight * v-part-qty) /
                                             eb.cas-wt, 2) 
                                             / eb.cas-pal) + .49, 0)).
          END. /* each eb */
       END.  /* do */
       ELSE
       IF AVAIL eb THEN DO:
        ASSIGN
          v-pallets = v-pallets +
                     (if xoe-rell.qty-case ne 0 
                        THEN ROUND((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                        ELSE 
                         IF eb.cas-cnt ne 0 
                          THEN ROUND((ROUND(v-tot-qty / eb.cas-cnt, 2) /
                                      eb.cas-pal) + .49, 0)
                          ELSE ROUND((ROUND(v-weight / eb.cas-wt, 2) /
                                      eb.cas-pal) + .49, 0)).
       END. /* do */
    END. /* est-no ne "" */

    IF ll-consol-rells THEN DO:

     IF (xoe-rell.qty-case * xoe-rell.cases) NE 0 
      THEN RUN create-tt-rell (xoe-rell.qty-case, xoe-rell.cases).

     IF xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases) NE 0 
      THEN RUN create-tt-rell (xoe-rell.qty - 
                               (xoe-rell.qty-case * xoe-rell.cases), 1).

    END.
    ELSE DO:

     CREATE tt-rell.
     BUFFER-COPY xoe-rell EXCEPT rec_key TO tt-rell.
    END.

    ASSIGN xoe-rell.printed = YES.
  END. /* each xoe-rell */

  {oe/rep/relallws.i}

  FOR EACH tt-rell
    BY tt-rell.i-no
    BY tt-rell.po-no
    BY tt-rell.ord-no
    BY tt-rell.line
    BY tt-rell.cases DESC:

    ASSIGN         
        v-UntCnt = STRING(tt-rell.cases) + " @ " + 
                   STRING(tt-rell.qty-case) .

    FIND FIRST oe-rel NO-LOCK
      WHERE oe-rel.company  EQ tt-rell.company
        AND oe-rel.ord-no   EQ tt-rell.ord-no
        AND oe-rel.line     EQ tt-rell.line
        AND oe-rel.link-no  EQ tt-rell.r-no
        AND oe-rel.ship-id  EQ oe-relh.ship-id
        AND oe-rel.po-no    EQ tt-rell.po-no
        AND oe-rel.i-no     EQ tt-rell.i-no NO-ERROR.
    IF NOT AVAIL oe-rel 
      THEN
       FIND FIRST oe-rel NO-LOCK
         WHERE oe-rel.company  EQ tt-rell.company
           AND oe-rel.ord-no   EQ tt-rell.ord-no
		   AND oe-rel.line     EQ tt-rell.line
	   	   AND oe-rel.rel-date EQ oe-relh.rel-date
    	   AND oe-rel.ship-id  EQ oe-relh.ship-id
		   AND oe-rel.po-no    EQ tt-rell.po-no
		   AND oe-rel.i-no     EQ tt-rell.i-no  NO-ERROR.

    FIND FIRST oe-ordl NO-LOCK
      WHERE oe-ordl.company EQ cocode
        AND oe-ordl.ord-no  EQ tt-rell.ord-no
        AND oe-ordl.i-no    EQ tt-rell.i-no
        AND oe-ordl.line    EQ tt-rell.LINE NO-ERROR.
    
    IF v-headers THEN DO:        

     FIND itemfg OF tt-rell NO-LOCK NO-ERROR.

     ASSIGN locbin = "".

     IF v-p-bin THEN DO:

      IF AVAIL itemfg THEN DO:

       ASSIGN xx = 0.

       FOR EACH fg-bin NO-LOCK
         WHERE fg-bin.company  EQ cocode
           AND fg-bin.i-no     EQ itemfg.i-no
           AND fg-bin.qty      GT 0 
         BREAK BY fg-bin.loc-bin:

         IF FIRST-OF(fg-bin.loc-bin) THEN DO:
           ASSIGN xx = xx + 1.

           IF xx LE EXTENT(locbin) THEN locbin[xx] = fg-bin.loc-bin.

         END. /* IF FIRST-OF(fg-bin.loc-bin) */
       END. /* FOR EACH fg-bin */
      END. /* IF AVAIL itemfg */

      DISPLAY
          tt-rell.ord-no
          tt-rell.po-no  
          locbin[1]      
          tt-rell.i-no   
          oe-ordl.i-name 
          v-UntCnt       
          tt-rell.qty format "->>>>>>>9" 
        SKIP
          locbin[2] 
          oe-ordl.part-no
          locbin[3] 
          oe-ordl.part-dscr1 
        SKIP
          locbin[4] 
          oe-ordl.part-dscr2 
      WITH FRAME relprint STREAM-IO NO-BOX NO-LABELS WIDTH 120.
      DOWN WITH FRAME relprint.    

      ASSIGN v-printline = v-printline + 4 + IF xx >= 4 THEN 1 ELSE 0 .

     END.
     ELSE DO:
       
      DISPLAY
        tt-rell.ord-no                                  
        tt-rell.po-no  AT 8                             
        tt-rell.i-no   AT 29
        v-UntCnt       TO 83
        tt-rell.qty                                     
      WITH FRAME ln-s-comp STREAM-IO NO-BOX NO-LABELS WIDTH 120.
  
      ASSIGN v-printline = v-printline + 2.

      DOWN WITH FRAME ln-s-comp.

      IF AVAIL oe-ordl 
        THEN DO i = 1 TO 3:

         ASSIGN
            v-part-dscr = IF i EQ 1 
                            THEN oe-ordl.i-name
                            ELSE
                              IF i EQ 2 
                                THEN oe-ordl.part-dscr1
                                ELSE oe-ordl.part-dscr2.


         IF i = 1 AND s-print-part-no THEN PUT oe-ordl.part-no AT 29.

         IF v-part-dscr NE "" THEN DO:
           PUT v-part-dscr at 44 SKIP.
           ASSIGN v-printline = v-printline + 1.
         END.
         ELSE 
          IF s-print-part-no THEN PUT SKIP.
      END. /* THEN DO */
     END. /* ELSE DO: */
    END. /* IF v-headers */
    ELSE DO:        

      DISPLAY
            tt-rell.ord-no
            tt-rell.po-no       AT 8
            tt-rell.loc-bin     AT 23 FORMAT "x(5)"   WHEN v-p-bin
            tt-rell.i-no        AT 29
            v-UntCnt       TO 83
            tt-rell.qty
      WITH FRAME ln-s.
      DOWN WITH FRAME ln-s STREAM-IO NO-BOX NO-LABELS WIDTH 120.
      
      ASSIGN v-printline = v-printline + 1.

      IF AVAIL oe-ordl 
        THEN DO i = 1 TO 3:

         ASSIGN v-part-dscr = IF i EQ 1 
                                THEN oe-ordl.i-name
                                ELSE  
                                 IF i EQ 2 
                                  THEN oe-ordl.part-dscr1
                                  ELSE oe-ordl.part-dscr2.

         IF i = 1 AND s-print-part-no THEN PUT oe-ordl.part-no AT 13.

         IF v-part-dscr NE "" THEN DO:
             PUT v-part-dscr AT 28 SKIP.
             ASSIGN v-printline = v-printline + 1.
         END.
         ELSE IF s-print-part-no THEN PUT SKIP.

      END. /* THEN DO */
    END. /* ELSE DO:*/

    PUT SKIP(1).

    ASSIGN v-printline = v-printline + 1.

    IF LINE-COUNTER > 63 THEN DO:
      PAGE .
      ASSIGN v-printline = 0.
      {oe/rep/relallws.i}.
    END.

    IF v-print-components THEN DO: /* display componets of set */

      IF NOT AVAIL itemfg 
        THEN 
         FIND itemfg OF tt-rell NO-LOCK NO-ERROR.

        
      IF AVAIL itemfg AND 
         itemfg.isaset 
        THEN
         FOR EACH fg-set 
           WHERE fg-set.company EQ cocode
             AND fg-set.set-no  EQ itemfg.i-no NO-LOCK:

           FIND FIRST xitemfg NO-LOCK 
             WHERE xitemfg.company EQ cocode
               AND xitemfg.i-no    EQ fg-set.part-no NO-ERROR.

           FIND FIRST fg-bin NO-LOCK 
             WHERE fg-bin.company EQ cocode
               AND fg-bin.i-no    EQ xitemfg.i-no
               AND fg-bin.job-no  EQ tt-rell.job-no
               AND fg-bin.job-no2 EQ tt-rell.job-no2 NO-ERROR.
           IF AVAIL fg-bin 
             THEN
              ASSIGN lv-comp-unit = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                                          fg-bin.case-count,0).
             ELSE ASSIGN lv-comp-unit = 0.


           ASSIGN v-part-dscr = STRING(fg-set.part-no,"x(15)") +
		                        (IF AVAIL xitemfg 
                                  THEN xitemfg.i-name 
                                  ELSE "").

           {sys/inc/part-qty.i v-part-qty fg-set}
           IF AVAIL fg-bin THEN DO:

             PUT lv-comp-unit      AT 8  FORMAT "->>9" " "
                 fg-bin.case-count AT 23  FORMAT ">>>>9"
                 v-part-dscr       AT 29  FORMAT "x(40)"
                 fg-bin.qty        TO 85  FORMAT "->>>>>>>9"
               SKIP.

             ASSIGN v-printline = v-printline + 1.

             IF fg-bin.partial-count <> 0 THEN DO:
              PUT "  1" AT 16 "@" fg-bin.partial-count FORMAT ">>>>9" SKIP.          
                  v-printline = v-printline + 1.
               END.
           END.
           ELSE do:
             PUT v-part-dscr AT 29 FORMAT "x(40)" SKIP.

             ASSIGN v-printline = v-printline + 1.
           END.
           
           IF LINE-COUNTER > 63 THEN DO:
             PAGE .
             ASSIGN v-printline = 0.
             {oe/rep/relallws.i}.
           END.
      END. /* for each fg-set */
    END. /* end of components display */

    ASSIGN tt-rell.printed = true.        
  END. /* for each tt-rell */

  ASSIGN v-ship-i[1] = IF AVAIL oe-rel THEN oe-rel.ship-i[1] ELSE ""
         v-ship-i[2] = IF AVAIL oe-rel THEN oe-rel.ship-i[2] ELSE ""
         v-ship-i[3] = IF AVAIL oe-rel THEN oe-rel.ship-i[3] ELSE ""
         v-ship-i[4] = IF AVAIL oe-rel THEN oe-rel.ship-i[4] ELSE "".


  PUT 
    "<FArial><R50><C1><P12><B>     Shipping Instructions: </B> <P9> "SKIP(1)
    "<R51><C1>" v-ship-i[1] AT 7 
    "<R52><C1>" v-ship-i[2] AT 7 
    "<R53><C1>" v-ship-i[3] AT 7 
    "<R54><C1>" v-ship-i[4] AT 7
    "<R56><C1>"
    "__________________________________________________________________________________________________________________"  SKIP 

    "<|10><C1><R58><#8><FROM><C80><R60><RECT> " 
    "<=8> Pulled By                                         Checked By                                        # of Units                                         Total Weight/Cube" SKIP
    "<R58><C20><FROM><R60><C20><Line>" 
    "<R58><C40><FROM><R60><C40><Line>" 
    "<R58><C60><FROM><R60><C60><Line>" 
    .

  ASSIGN v-printline = v-printline + 14.

  PAGE. 

  ASSIGN v-printline = 0.
  RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).

END. /* for each oe-relh */

RETURN.

PROCEDURE create-tt-rell.
  
  DEF INPUT PARAM ip-qty-case LIKE oe-rell.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-rell.cases    NO-UNDO.

  IF ip-qty-case LT 0 
   THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-rell NO-LOCK
    WHERE tt-rell.i-no     EQ xoe-rell.i-no
      AND tt-rell.po-no    EQ xoe-rell.po-no
      AND tt-rell.ord-no   EQ xoe-rell.ord-no
      AND tt-rell.line     EQ xoe-rell.line
      AND tt-rell.qty-case EQ ip-qty-case NO-ERROR.
  IF NOT AVAIL tt-rell THEN DO:
    CREATE tt-rell.
    BUFFER-COPY xoe-rell EXCEPT rec_key TO tt-rell
    ASSIGN
     tt-rell.qty-case = ip-qty-case
     tt-rell.cases    = 0
     tt-rell.qty      = 0
     tt-rell.partial  = 0.
  END.

  ASSIGN
   tt-rell.cases    = tt-rell.cases + ip-cases
   tt-rell.qty      = tt-rell.qty + (ip-qty-case * ip-cases).

  IF xoe-rell.p-c THEN tt-rell.p-c = YES.

END PROCEDURE.
