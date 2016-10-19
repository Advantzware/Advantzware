/* ----------------------------------------------- oe/rep/relindc.i  01/05 YSK*/
/* Print OE Release/Picking tickets    for Indian Carton                      */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

DEFINE VARIABLE v-units-hdr AS CHARACTER FORMAT "x(5)" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-zone-hdr AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-zone LIKE shipto.dest-code NO-UNDO.
DEFINE VARIABLE v-part-dscr LIKE oe-ordl.i-name NO-UNDO.
DEFINE VARIABLE v-qty LIKE oe-rell.qty NO-UNDO.
DEFINE VARIABLE v-frt-pay-dscr AS CHARACTER FORMAT "x(11)" NO-UNDO.
/* === with xprint ====*/
DEFINE VARIABLE v-term AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-image1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS CHARACTER FORMAT "x(150)" NO-UNDO.
DEFINE SHARED VARIABLE s-print-pricing AS LOGICAL NO-UNDO.

ASSIGN ls-image1 = "images\icc.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEFINE VARIABLE v-tel AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact AS CHARACTER FORMAT "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-line-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-quo-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-t-tax      AS   DECIMAL EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bot-lab    AS   CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-q-no LIKE oe-ord.q-no NO-UNDO.
DEFINE VARIABLE v-printline AS INTEGER NO-UNDO.
DEFINE VARIABLE v-ship-i AS CHARACTER FORMAT "x(60)" EXTENT 4 NO-UNDO.

DEFINE VARIABLE ll-display-comp AS LOGICAL NO-UNDO.  /* display company address */
DEFINE VARIABLE ll-consol-rells AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-comp-name AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE lv-email AS CHARACTER FORMAT "x(30)" NO-UNDO.

DEFINE VARIABLE lv-comp-color AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-other-color AS CHARACTER INITIAL "BLACK" NO-UNDO.
DEFINE BUFFER xitemfg FOR itemfg.
DEFINE VARIABLE lv-comp-unit AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE v-print-components AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE s-print-part-no AS LOGICAL NO-UNDO.

DEFINE VARIABLE lv-qty-case AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-cases AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-partial AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-tot-cases LIKE v-pallets NO-UNDO.
DEFINE VARIABLE v-tag AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-price AS DECIMAL DECIMALS 4 NO-UNDO.
DEFINE VARIABLE lv-ext-price AS DECIMAL NO-UNDO.
DEFINE VARIABLE lv-price-string AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-job-no AS CHARACTER NO-UNDO .
DEFINE VARIABLE d-bar-line AS DECIMAL NO-UNDO .
DEFINE VARIABLE iCountLine AS INTEGER NO-UNDO .
DEFINE VARIABLE iv-comp-unit AS INTEGER NO-UNDO .

ASSIGN tmpstore = FILL("-",130).

FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "RELPRINT"  NO-ERROR.
ASSIGN
 ll-display-comp = AVAILABLE sys-ctrl AND sys-ctrl.log-fld
 ll-consol-rells = AVAILABLE sys-ctrl AND sys-ctrl.int-fld NE 0.

FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "LOGOCOLR"  NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

FIND FIRST company NO-LOCK WHERE company.company EQ cocode  NO-ERROR.
ASSIGN v-comp-add1 = ""
       v-comp-add2 = ""
       v-comp-add3 = ""
       v-comp-add4 = ""
       v-comp-add5 = ""
       lv-email = ""
       lv-comp-name = ""
       d-bar-line   = 4.3 .

IF ll-display-comp THEN DO:
   FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode AND
                         cust.active EQ "X"  NO-ERROR.
 
  IF AVAILABLE cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = cust.addr[2]
            v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4 = "Phone:  " + STRING(cust.area-code,"(999)") + STRING(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + STRING(cust.fax,"(999)999-9999") 
            lv-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME   
            .

 END.

FORMAT
  tt-rell.ord-no
  tt-rell.po-no AT 12
  /*tt-rell.loc-bin  AT 23  FORM "x(5)"*/
  tt-rell.i-no AT 29  oe-ordl.i-name AT 44
  oe-ordl.qty FORMAT "->>>>>>>9" TO 83
  tt-rell.qty FORMAT "->>>>>>>9" SKIP
  WITH DOWN FRAME relprint NO-BOX NO-LABEL STREAM-IO WIDTH 110.

FORMAT
  tt-rell.loc-bin  AT 12  FORMAT "x(9)"
  v-tag AT 29 FORMAT "x(15)"
  /*oe-ordl.part-dscr1 at 46 FORMAT "x(30)" */
  tt-rell.partial  TO 83 
  lv-partial FORMAT ">>>" TO 93  skip
   /* oe-ordl.i-name AT 29*/
    /*oe-ordl.part-dscr2 AT 46 FORMAT "x(30)"*/
  
  WITH DOWN FRAME relprint-2 NO-BOX NO-LABEL STREAM-IO WIDTH 110.

FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ cocode  NO-ERROR.
v-printline = 0.

IF v-zone-p THEN v-zone-hdr = "Route No.:".

    {oe/rep/foreachr.i},

        FIRST cust NO-LOCK
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ oe-relh.cust-no
        BREAK BY {1} BY oe-relh.release#:

      RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).

      ASSIGN
       v-tot-qty = 0
       v-weight  = 0
       v-pallets = 0
       v-total   = 0
       v-zone    = IF v-zone-p THEN shipto.dest-code ELSE "".

      FIND FIRST carrier NO-LOCK
          WHERE carrier.company EQ cocode
            AND carrier.carrier EQ oe-relh.carrier
           NO-ERROR.
      
      assign
       v-carrier   = IF AVAILABLE carrier THEN carrier.dscr ELSE ""
       v-frt-terms = "".

      FOR EACH xoe-rell NO-LOCK
          WHERE xoe-rell.company EQ oe-relh.company
            AND xoe-rell.r-no    EQ oe-relh.r-no
          USE-INDEX r-no ,
          FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ xoe-rell.company
            AND oe-ord.ord-no  EQ xoe-rell.ord-no:

        CASE oe-ord.frt-pay:
             WHEN "P" THEN v-frt-terms = "Prepaid".
             WHEN "C" THEN v-frt-terms = "Collect".
             WHEN "B" THEN v-frt-terms = "Bill".
             WHEN "T" THEN v-frt-terms = "Third Party".
        END CASE.

        LEAVE.
      END.

      FOR EACH tt-rell:
        DELETE tt-rell.
      END.

      /** Calculate the total weight of the released items. **/
      FOR EACH xoe-rell
          WHERE xoe-rell.company EQ cocode
            AND xoe-rell.r-no    EQ oe-relh.r-no:
        FIND FIRST xoe-ordl NO-LOCK
            WHERE xoe-ordl.company EQ cocode
              AND xoe-ordl.ord-no  EQ xoe-rell.ord-no
              AND xoe-ordl.line    EQ xoe-rell.line
              AND xoe-ordl.i-no    EQ xoe-rell.i-no
            USE-INDEX ord-no  NO-ERROR.
        IF AVAILABLE xoe-ordl THEN DO:
           FIND itemfg NO-LOCK WHERE itemfg.company EQ cocode
                         AND itemfg.i-no EQ xoe-ordl.i-no  NO-ERROR.
          ASSIGN
           v-tot-qty = v-tot-qty + xoe-rell.qty
           v-weight = v-weight + /*(if xoe-ordl.t-weight ne ? then
                                  /*(round(xoe-ordl.t-weight /
                                   xoe-ordl.qty, 2) * xoe-rell.qty) else 0)*/
                                 ((xoe-ordl.t-weight / xoe-ordl.qty) * xoe-rell.qty) else 0)*/
                                (xoe-rell.qty / 100) * itemfg.weight-100.
        END.
        IF AVAILABLE xoe-ordl AND xoe-ordl.est-no NE "" THEN DO:
          FIND FIRST eb NO-LOCK
              WHERE eb.company  EQ xoe-ordl.company
                AND eb.est-no   EQ xoe-ordl.est-no
                AND eb.form-no  EQ xoe-ordl.form-no
                AND eb.blank-no EQ xoe-ordl.blank-no
               NO-ERROR.

          IF xoe-ordl.form-no EQ 0                             AND
             (xoe-ordl.est-type EQ 2 or xoe-ordl.est-type EQ 6) THEN DO:
            FOR EACH fg-set NO-LOCK
                WHERE fg-set.company EQ xoe-ordl.company
                  AND fg-set.set-no  EQ xoe-ordl.i-no:
              v-set-qty = v-set-qty + fg-set.part-qty.
            END.
            IF v-set-qty EQ 0 THEN v-set-qty = 1.
            FOR EACH eb NO-LOCK
                WHERE eb.company EQ xoe-ordl.company
                  AND eb.est-no  EQ xoe-ordl.est-no
                  AND eb.form-no NE 0:
              FIND fg-set NO-LOCK
                  WHERE fg-set.company EQ xoe-ordl.company
                    AND fg-set.set-no  EQ xoe-ordl.i-no
                    AND fg-set.part-no EQ eb.stock-no
                   NO-ERROR.

              ASSIGN
               v-part-qty = (IF AVAILABLE fg-set AND fg-set.part-qty NE 0 THEN
                             fg-set.part-qty ELSE 1) / v-set-qty
               v-pallets = v-pallets +
                           (IF xoe-rell.qty-case NE 0 THEN
                              ROUND((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                            ELSE
                            IF eb.cas-cnt NE 0 THEN
                              ROUND((ROUND((v-tot-qty * v-part-qty) /
                                         eb.cas-cnt, 2) / eb.cas-pal) + .49, 0)
                            ELSE
                              ROUND((ROUND((v-weight * v-part-qty) /
                                         eb.cas-wt, 2) / eb.cas-pal) + .49, 0)).
            END. /* each eb */
          END. /* do */
          ELSE
          IF AVAILABLE eb THEN DO:
            ASSIGN
             v-pallets = v-pallets +
                         (IF xoe-rell.qty-case NE 0 THEN
                            ROUND((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                          ELSE
                          IF eb.cas-cnt NE 0 THEN
                            ROUND((ROUND(v-tot-qty / eb.cas-cnt, 2) /
                                                       eb.cas-pal) + .49, 0)
                          ELSE
                            ROUND((ROUND(v-weight / eb.cas-wt, 2) /
                                                       eb.cas-pal) + .49, 0)).
          END. /* do */
        END. /* est-no ne "" */

        IF ll-consol-rells THEN DO:
          IF (xoe-rell.qty-case * xoe-rell.cases) NE 0 THEN
            RUN create-tt-rell (xoe-rell.qty-case, xoe-rell.cases, xoe-rell.qty).

          IF xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases) NE 0 THEN
            RUN create-tt-rell (xoe-rell.qty - (xoe-rell.qty-case * xoe-rell.cases), 1, 0).
        END.

        ELSE DO:
          CREATE tt-rell.
          BUFFER-COPY xoe-rell TO tt-rell.

          FIND FIRST xitemfg OF tt-rell NO-LOCK NO-ERROR.

          IF AVAILABLE xitemfg THEN
          DO:
            IF xitemfg.q-onh EQ 0 THEN
               tt-rell.loc-bin = "FLOOR".
            RELEASE xitemfg.
          END.
        END.

        lv-tot-cases = lv-tot-cases + xoe-rell.cases.
        IF xoe-rell.partial GT 0 THEN lv-tot-cases = lv-tot-cases + 1.
        xoe-rell.printed = YES.
      END. /* each xoe-rell */
      FIND FIRST tt-rell NO-LOCK
           WHERE tt-rell.po-no NE "" NO-ERROR.

      {oe/rep/relindc2.i}
         
      FOR EACH tt-rell
        BREAK  BY tt-rell.i-no
          BY tt-rell.po-no
          BY tt-rell.ord-no
          BY tt-rell.line
          BY tt-rell.cases DESCENDING:

	    FIND FIRST oe-rel NO-LOCK
	        WHERE oe-rel.company  EQ tt-rell.company
	          AND oe-rel.ord-no   EQ tt-rell.ord-no
	          AND oe-rel.line     EQ tt-rell.line
	          AND oe-rel.link-no  EQ tt-rell.r-no
	          AND oe-rel.ship-id  EQ oe-relh.ship-id
	          AND oe-rel.po-no    EQ tt-rell.po-no
	          AND oe-rel.i-no     EQ tt-rell.i-no
	         NO-ERROR.

	    IF NOT AVAILABLE oe-rel THEN
	      FIND FIRST oe-rel NO-LOCK
    	      WHERE oe-rel.company  EQ tt-rell.company
	    	    AND oe-rel.ord-no   EQ tt-rell.ord-no
		        AND oe-rel.line     EQ tt-rell.line
	   	        AND oe-rel.rel-date EQ oe-relh.rel-date
    		    AND oe-rel.ship-id  EQ oe-relh.ship-id
		        AND oe-rel.po-no    EQ tt-rell.po-no
		        AND oe-rel.i-no     EQ tt-rell.i-no
	           NO-ERROR.

        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ cocode
              AND oe-ordl.ord-no  EQ tt-rell.ord-no
              AND oe-ordl.i-no    EQ tt-rell.i-no
              AND oe-ordl.line    EQ tt-rell.line
             NO-ERROR.
   
        IF v-headers THEN DO:
          FIND itemfg OF tt-rell NO-LOCK NO-ERROR.
          locbin = "".
          v-tag = oe-ordl.part-no /*tt-rell.tag */.
          IF v-p-bin THEN DO:
             IF AVAILABLE itemfg THEN DO:
                xx = 0.
                FOR EACH fg-bin NO-LOCK
                    WHERE fg-bin.company  EQ cocode
                      AND fg-bin.i-no     EQ itemfg.i-no
                      AND fg-bin.qty      GT 0 
                    
                    BREAK BY fg-bin.loc-bin:
                   IF FIRST-of(fg-bin.loc-bin) THEN DO:
                      xx = xx + 1.
                      IF xx LE EXTENT(locbin) THEN locbin[xx] = fg-bin.loc-bin.
                   END.
                END.

                FIND FIRST fg-bin where fg-bin.company eq cocode
                              and fg-bin.i-no    eq itemfg.i-no
                              and fg-bin.job-no = tt-rell.job-no
                              AND fg-bin.job-no2 = tt-rell.job-no2 NO-LOCK NO-ERROR.
                IF AVAIL fg-bin THEN
                    ASSIGN iv-comp-unit = trunc((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) .
                    ELSE iv-comp-unit = 0 .


             END.
             lv-partial = IF tt-rell.partial GT 0 THEN 1 ELSE 0.

            /* DISPLAY
              tt-rell.ord-no
              tt-rell.po-no
              tt-rell.i-no
              tt-rell.qty-case @ oe-ordl.qty
              tt-rell.cases @ tt-rell.qty
              oe-ordl.i-name   WHEN AVAILABLE oe-ordl  SKIP
              WITH FRAME relprint STREAM-IO NO-BOX NO-LABELS WIDTH 120.
              DOWN WITH FRAME relprint.*/

              
              ASSIGN iCountLine = 0 .
              PUT  oe-ordl.part-no FORMAT "x(15)" SPACE(1)
                  "<UNITS=INCHES><AT=" + STRING(d-bar-line)  + ",1.5><FROM><AT=+.22,+2><BARCODE,TYPE=128A,CHECKSUM=NONE,VALUE=" +
                   STRING(tt-rell.i-no) + ">" FORM "x(100)" "<P10>" 
                   "<AT=,2.3>" tt-rell.i-no FORM "x(15)"  SPACE(1) .
              PUT "<R-2> "
                   IF AVAIL oe-ordl THEN oe-ordl.i-name ELSE "" FORMAT "x(30)" SPACE(1)
                   tt-rell.qty-case SPACE(4)
                   tt-rell.cases FORMAT "->>>>" SKIP .

                ASSIGN iCountLine = 1 . 

                IF AVAILABLE oe-ordl THEN
                    DO i = 1 to 3:
                    lv-partial = IF i EQ 1 AND tt-rell.partial GT 0 THEN 1 ELSE 0.
                    v-part-dscr = IF i EQ 1 THEN oe-ordl.part-dscr1
                    ELSE IF i EQ 2 THEN oe-ordl.part-dscr2
                    ELSE   "" .
                           IF v-part-dscr NE "" THEN PUT v-part-dscr AT 46 FORMAT "x(25)" .
                           IF lv-partial GT 0 THEN PUT tt-rell.partial TO 83 lv-partial TO 93.              
                           IF s-print-part-no OR lv-partial GT 0 OR v-part-dscr NE "" THEN DO: 
                               PUT SKIP.
                               v-printline = v-printline + 1.
                               ASSIGN iCountLine = iCountLine + 1 .
                           END.
                END.

                  v-job-no = TRIM(oe-ordl.job-no) + "-" + STRING(INTEGER(oe-ordl.job-no2), "99" ) .
              IF s-print-pricing THEN
              DO:
                 RUN calc-ext-cost(OUTPUT lv-price, OUTPUT lv-ext-price).
                 ASSIGN
                 lv-price-string = TRIM(STRING(lv-price,"-ZZ,ZZZ,ZZ9.9999") + "/" + oe-ordl.pr-uom)
                 v-total = v-total + lv-ext-price
                 v-printline = v-printline + 1.
                 PUT lv-price-string AT 46 FORMAT "X(20)" lv-ext-price FORMAT "->>,>>>,>>9.99" TO 93.
                 ASSIGN iCountLine = iCountLine + 1 .
              END.
              


            /*  DISPLAY
               tt-rell.loc-bin  
               v-tag
               /*oe-ordl.i-name WHEN s-print-part-no*/
               /*oe-ordl.part-dscr1 WHEN AVAILABLE oe-ordl   /* CTS */*/
               /*oe-ordl.part-dscr2 WHEN AVAILABLE oe-ordl /*bsm*/    */
               tt-rell.partial WHEN tt-rell.partial GT 0
               lv-partial WHEN lv-partial NE 0
               WITH FRAME relprint-2 STREAM-IO NO-BOX NO-LABELS WIDTH 120.
               DOWN WITH FRAME relprint-2.*/

               PUT SKIP v-job-no FORMAT "X(11)" 
                    tt-rell.loc FORMAT "x(6)" AT 46 SPACE(1)
                    tt-rell.loc-bin FORMAT "x(8)" space(1)
                    iv-comp-unit FORMAT "->>>>" .

               v-printline = v-printline + 1.
               
               ASSIGN iCountLine = iCountLine + 1 .
               IF NOT LAST(tt-rell.i-no) THEN
                 PUT  SKIP(2).

              v-printline = v-printline + 4 + IF xx GE 4 THEN 1 ELSE 0 .
             IF iCountLine = 1 THEN
                   ASSIGN d-bar-line = d-bar-line + 0.5 .
               ELSE IF iCountLine = 2 THEN
                   ASSIGN d-bar-line = d-bar-line + 0.7 .
               ELSE IF iCountLine = 3 THEN
                   ASSIGN d-bar-line = d-bar-line + 0.8 .
               ELSE
                   ASSIGN d-bar-line = d-bar-line + 0.9 .

          END.
          ELSE DO:

          /*  DISPLAY
              tt-rell.ord-no
              tt-rell.po-no  AT 12
              tt-rell.i-no   AT 29
              tt-rell.qty-case @ oe-ordl.qty TO 83
              tt-rell.cases @ tt-rell.qty 
              WITH FRAME ln-s-comp STREAM-IO NO-BOX NO-LABELS WIDTH 120.
             v-printline = v-printline + 2.
            DOWN WITH FRAME ln-s-comp.*/

               ASSIGN iCountLine = 0 .
               PUT  oe-ordl.part-no FORMAT "x(15)" SPACE(1)
                  "<UNITS=INCHES><AT=" + STRING(d-bar-line)  + ",1.5><FROM><AT=+.22,+2><BARCODE,TYPE=128A,CHECKSUM=NONE,VALUE=" +
                   STRING(tt-rell.i-no) + ">" FORM "x(100)" "<P10>" 
                   "<AT=,2.3>" tt-rell.i-no FORM "x(15)"  SPACE(1) .
              PUT "<R-2> "
                   IF AVAIL oe-ordl THEN oe-ordl.i-name ELSE "" FORMAT "x(30)" SPACE(1)
                   tt-rell.qty-case SPACE(4)
                   tt-rell.cases SPACE(1) SKIP .
               
               ASSIGN iCountLine = 1 .

             v-job-no = TRIM(oe-ordl.job-no) + "-" + STRING(INTEGER(oe-ordl.job-no2), "99" ) .

              PUT v-job-no FORMAT "X(11)"  .

            IF AVAILABLE oe-ordl THEN
            DO i = 1 to 3:
              lv-partial = IF i EQ 1 AND tt-rell.partial GT 0 THEN 1 ELSE 0.
              v-part-dscr = IF i EQ 1 THEN oe-ordl.part-dscr1
                            ELSE IF i EQ 2 THEN oe-ordl.part-dscr2
                            ELSE   "" .
              IF v-part-dscr NE "" THEN PUT v-part-dscr AT 46 FORMAT "x(25)" .
              IF lv-partial GT 0 THEN PUT tt-rell.partial TO 83 lv-partial TO 93.              
              IF s-print-part-no OR lv-partial GT 0 OR v-part-dscr NE "" THEN DO: 
                 PUT SKIP.
                 v-printline = v-printline + 1.
                 ASSIGN iCountLine = iCountLine + 1 .
              END.
              
            END.
             IF s-print-pricing THEN
               DO:
                  RUN calc-ext-cost(OUTPUT lv-price, OUTPUT lv-ext-price).
                  ASSIGN
                  lv-price-string = TRIM(STRING(lv-price,"-ZZ,ZZZ,ZZ9.9999") + "/" + oe-ordl.pr-uom)
                  v-total = v-total + lv-ext-price
                  v-printline = v-printline + 1.
                  PUT lv-price-string AT 46 FORMAT "X(20)" lv-ext-price FORMAT "->>,>>>,>>9.99" TO 93.
                  ASSIGN iCountLine = iCountLine + 1 .
               END.
               IF NOT LAST(tt-rell.i-no) THEN
                 PUT  SKIP(2).
               IF iCountLine = 1 THEN
                   ASSIGN d-bar-line = d-bar-line + 0.5 .
               ELSE IF iCountLine = 2 THEN
                   ASSIGN d-bar-line = d-bar-line + 0.7 .
               ELSE IF iCountLine = 3 THEN
                   ASSIGN d-bar-line = d-bar-line + 0.8 .
               ELSE
                   ASSIGN d-bar-line = d-bar-line + 0.9 .


          END.
        END.  /* v-header */
        ELSE DO:

         /* DISPLAY
            tt-rell.ord-no
            tt-rell.po-no       AT 12
            /*tt-rell.loc-bin     AT 23   FORMAT "x(5)"   WHEN v-p-bin*/
            tt-rell.i-no        AT 29
            tt-rell.qty-case @ oe-ordl.qty TO 83
            tt-rell.cases @ tt-rell.qty
            WITH FRAME ln-s.
          DOWN WITH FRAME ln-s STREAM-IO NO-BOX NO-LABELS WIDTH 120.*/
           
          ASSIGN iCountLine = 0 .
           PUT  oe-ordl.part-no FORMAT "x(15)" SPACE(1)
                  "<UNITS=INCHES><AT=" + STRING(d-bar-line)  + ",1.5><FROM><AT=+.22,+2><BARCODE,TYPE=128A,CHECKSUM=NONE,VALUE=" +
                   STRING(tt-rell.i-no) + ">" FORM "x(100)" "<P10>" 
                   "<AT=,2.3>" tt-rell.i-no FORM "x(15)"  SPACE(1) .
              PUT "<R-2> "
                   IF AVAIL oe-ordl THEN oe-ordl.i-name ELSE "" FORMAT "x(30)" SPACE(1)
                   tt-rell.qty-case SPACE(4)
                   tt-rell.cases SPACE(1) SKIP .
               ASSIGN d-bar-line = d-bar-line + 0.7 .

          v-printline = v-printline + 1.
          ASSIGN iCountLine = 1 .

           v-job-no = TRIM(oe-ordl.job-no) + "-" + STRING(int(oe-ordl.job-no2), "99" ) .
            IF NOT v-p-bin THEN
              PUT v-job-no FORMAT "X(11)"  .

          IF AVAILABLE oe-ordl THEN
          DO:
             DO i = 1 to 3:
                 lv-partial = IF i EQ 1 AND tt-rell.partial GT 0 THEN 1 ELSE 0.
                 v-part-dscr = IF i EQ 1 THEN oe-ordl.part-dscr1
                               ELSE IF i EQ 2 THEN oe-ordl.part-dscr2
                               ELSE   "".
                 IF v-part-dscr NE "" THEN PUT v-part-dscr AT 46 FORMAT "x(25)" .
                 IF lv-partial GT 0 THEN PUT tt-rell.partial TO 83 lv-partial TO 93.              
                 IF s-print-part-no OR lv-partial GT 0 OR v-part-dscr NE "" THEN DO: 
                    PUT SKIP.
                    v-printline = v-printline + 1.
                    ASSIGN iCountLine = iCountLine + 1 .
                 END.
             END.
          END.

           IF s-print-pricing AND AVAILABLE oe-ordl THEN
          DO:
             RUN calc-ext-cost(OUTPUT lv-price, OUTPUT lv-ext-price).
             ASSIGN
             lv-price-string = TRIM(STRING(lv-price,"-ZZ,ZZZ,ZZ9.9999") + "/" + oe-ordl.pr-uom)
             v-total = v-total + lv-ext-price
             v-printline = v-printline + 1.
             PUT lv-price-string AT 46 FORMAT "X(20)" lv-ext-price FORMAT "->>,>>>,>>9.99" TO 93.
             ASSIGN iCountLine = iCountLine + 1 .
          END.
              IF v-p-bin THEN do:
                  PUT SKIP v-job-no FORMAT "X(11)" 
                    tt-rell.loc FORMAT "x(8)" AT 46 SPACE(1)
                    tt-rell.loc-bin FORMAT "x(10)" space(1) .
                  ASSIGN iCountLine = iCountLine + 1 .
                  v-printline = v-printline + 1.
              END.
              IF NOT LAST(tt-rell.i-no) THEN
                 PUT  SKIP(2).

              IF iCountLine = 1 THEN
                   ASSIGN d-bar-line = d-bar-line + 0.5 .
               ELSE IF iCountLine = 2 THEN
                   ASSIGN d-bar-line = d-bar-line + 0.7 .
               ELSE IF iCountLine = 3 THEN
                   ASSIGN d-bar-line = d-bar-line + 0.8 .
               ELSE
                   ASSIGN d-bar-line = d-bar-line + 0.9 .

        END.
          

        /*put skip(1).
        v-printline = v-printline + 1.*/
        IF LINE-COUNTER GT 55 THEN DO:
               PAGE .
               v-printline = 0.
               {oe/rep/relindc2.i}.
               d-bar-line   = 4.3  .
        END.

        IF v-print-components THEN DO: /* display componets of set */

          IF NOT AVAILABLE itemfg THEN FIND itemfg of tt-rell NO-LOCK NO-ERROR.
          IF AVAILABLE itemfg AND itemfg.isaset THEN
          FOR EACH fg-set NO-LOCK WHERE fg-set.company EQ cocode
	                    AND fg-set.set-no  EQ itemfg.i-no :

            FIND FIRST xitemfg NO-LOCK WHERE xitemfg.company EQ cocode
	                           AND xitemfg.i-no    EQ fg-set.part-no  NO-ERROR.
            FIND FIRST fg-bin NO-LOCK WHERE fg-bin.company EQ cocode
                              AND fg-bin.i-no    EQ xitemfg.i-no
                              AND fg-bin.job-no EQ tt-rell.job-no
                              AND fg-bin.job-no2 EQ tt-rell.job-no2  NO-ERROR.
            IF AVAILABLE fg-bin THEN
               ASSIGN lv-comp-unit = TRUNCATE((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) 
                 /*lv-comp-partial = fg-bin.qty - (lv-comp-unit * fg-bin.case-count)*/
                 .
            ELSE lv-comp-unit = 0.
            v-part-dscr = STRING(fg-set.part-no,"x(16)") +
		                  (IF AVAILABLE xitemfg THEN xitemfg.i-name ELSE "").

            {sys/inc/part-qty.i v-part-qty fg-set}
            IF AVAILABLE fg-bin THEN DO:
               PUT lv-comp-unit AT 15  FORMAT "->>9" " "
                   fg-bin.case-count FORMAT ">>>>9"
                   v-part-dscr              AT 40 FORMAT "x(40)"
                   /*lv-relqty /*tt-rell.qty*/ * v-part-qty*/
                   fg-bin.qty TO 94 FORMAT "->>>>>>>9"
	               SKIP.
               v-printline = v-printline + 1.
               IF fg-bin.partial-count NE 0 THEN DO:
                  PUT "  1" AT 16 "@" fg-bin.partial-count FORMAT ">>>>9" SKIP.          
                  v-printline = v-printline + 1.
               END.
            END.
            ELSE DO:
                PUT v-part-dscr AT 40 FORMAT "x(40)" SKIP.
                v-printline = v-printline + 1.
            END.
            IF LINE-COUNTER GT 55 THEN DO:
               PAGE .
               v-printline = 0.
               {oe/rep/relindc2.i}.
            END.
          END. /* FOR EACH fg-set */
       END.  /* END of components display */

        tt-rell.printed = TRUE.        
      END. /* FOR EACH tt-rell */

IF s-print-pricing THEN
   PUT "Total:" AT 74 v-total FORMAT "->>,>>>,>>9.99" TO 93.

ASSIGN v-ship-i[1] = IF AVAILABLE oe-rel THEN oe-rel.ship-i[1] ELSE ""
       v-ship-i[2] = IF AVAILABLE oe-rel THEN oe-rel.ship-i[2] ELSE ""
       v-ship-i[3] = IF AVAILABLE oe-rel THEN oe-rel.ship-i[3] ELSE ""
       v-ship-i[4] = IF AVAILABLE oe-rel THEN oe-rel.ship-i[4] ELSE "".

PUT "<FArial><R50><C1><P12><B>     Shipping Instructions: </B> <P9> "SKIP(1)
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
             
  v-printline = v-printline + 14.
 
 /* IF v-printline < 45 THEN*/  PAGE. /* PUT SKIP(60 - v-printline). */
      v-printline = 0.
      RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
    
    END. /* FOR EACH oe-relh */

RETURN.

PROCEDURE create-tt-rell.
  DEFINE INPUT PARAM ip-qty-case LIKE oe-rell.qty-case NO-UNDO.
  DEFINE INPUT PARAM ip-cases    LIKE oe-rell.cases NO-UNDO.
  DEFINE INPUT PARAM ip-qty      LIKE oe-rell.qty NO-UNDO.

  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1
     ip-qty      = ip-qty * -1.

  FIND FIRST tt-rell NO-LOCK
      WHERE tt-rell.i-no     EQ xoe-rell.i-no
        AND tt-rell.po-no    EQ xoe-rell.po-no
        AND tt-rell.ord-no   EQ xoe-rell.ord-no
        AND tt-rell.line     EQ xoe-rell.line
        AND tt-rell.qty-case EQ ip-qty-case
       NO-ERROR.

  IF NOT AVAILABLE tt-rell THEN DO:
    
    CREATE tt-rell.
    BUFFER-COPY xoe-rell TO tt-rell
    ASSIGN
     tt-rell.qty-case = ip-qty-case
     tt-rell.cases    = 0
     tt-rell.qty      = 0
     tt-rell.partial  = 0.

    FIND FIRST xitemfg of tt-rell NO-LOCK NO-ERROR.

    IF AVAILABLE xitemfg THEN
    DO:
       IF xitemfg.q-onh EQ 0 THEN
          tt-rell.loc-bin = "FLOOR".
       RELEASE xitemfg.
    END.
  END.

  ASSIGN
   tt-rell.cases = tt-rell.cases + ip-cases
   tt-rell.qty   = tt-rell.qty + ip-qty.

  IF xoe-rell.p-c THEN tt-rell.p-c = YES.

END PROCEDURE.

PROCEDURE calc-ext-cost:

   DEFINE OUTPUT PARAMETER op-tmp-price AS DECIMAL DECIMALS 4 NO-UNDO.
   DEFINE OUTPUT PARAMETER op-t-price AS DECIMAL NO-UNDO.

   DEFINE VARIABLE lv-tmp-price AS DECIMAL DECIMALS 4 NO-UNDO.
   DEFINE VARIABLE lv-price LIKE oe-ordl.price NO-UNDO.

   IF (tt-rell.qty NE 0 AND oe-ordl.pr-uom NE "") THEN
   DO:
      FIND FIRST itemfg
           {sys/look/itemfgrlW.i} AND
           itemfg.i-no EQ oe-ordl.i-no
           NO-LOCK NO-ERROR.
           
      FIND FIRST reftable NO-LOCK WHERE
           reftable.reftable EQ "oe-rell.sell-price" AND
           reftable.rec_key  EQ tt-rell.rec_key
           USE-INDEX rec_key
            NO-ERROR.

      IF AVAILABLE reftable AND reftable.val[1] NE 0 THEN
         lv-price = reftable.val[1].
      ELSE
         lv-price = oe-ordl.price.

      RELEASE reftable.

      ASSIGN
       lv-tmp-price = IF oe-ordl.pr-uom BEGINS "L" AND oe-ordl.pr-uom NE "LB" THEN
                       IF tt-rell.qty LT 0 THEN -1 ELSE 1
                       ELSE
                       IF oe-ordl.pr-uom EQ "CS" THEN
                          tt-rell.qty / (IF oe-ordl.cas-cnt NE 0 THEN oe-ordl.cas-cnt ELSE
                                         IF AVAILABLE itemfg AND itemfg.case-count NE 0
                                                         THEN itemfg.case-count ELSE
                                                              1)
                     ELSE
                     IF oe-ordl.pr-uom EQ "C" THEN
                       tt-rell.qty / 100
                     ELSE
                     IF oe-ordl.pr-uom EQ "M" THEN
                       tt-rell.qty / 1000
                     ELSE
                       tt-rell.qty
                                
        op-t-price = lv-tmp-price * lv-price
        op-tmp-price = lv-price
        op-t-price = ROUND(op-t-price * (1 - (oe-ordl.disc / 100)),2).

   END.
END PROCEDURE.
