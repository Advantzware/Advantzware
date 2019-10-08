/* ----------------------------------------------- po/po-sultn.p GDM 06030906  */
/* Purchase Order Print Program for N-K-1-POPRINT = ACPI                      */
/* -------------------------------------------------------------------------- */

/* fax multiple recipents or single */
DEF INPUT PARAMETER ip-multi-faxout AS LOG NO-UNDO. 
DEF INPUT PARAM  ip-lines-per-page  AS INT NO-UNDO.

DEF STREAM st-fax.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEF BUFFER b-ref1 FOR reftable.
DEF BUFFER b-ref2 FOR reftable.

{po/po-print.i}
{custom/notesdef.i}
{custom/formtext.i NEW}

DEF VAR v-tmp-note-length AS INT NO-UNDO.
DEF VAR lv-text           AS CHAR NO-UNDO.
DEF VAR li                AS INT NO-UNDO.
DEF VAR v-wid             like po-ordl.s-wid format ">>>9.99" no-undo.
DEF VAR v-len             like po-ordl.s-len format ">>>9.99" no-undo.
DEF VAR pol-counter       as int no-undo.
DEF VAR save_id           as recid.
DEF VAR time_stamp        as char.
DEF VAR v-exp-limit       as int no-undo init 10.
DEF VAR v-line-number     as int.
DEF VAR v-page-counter    as int format ">>9".
DEF VAR v-lines-to-skip   as int.
DEF VAR v-sname           like shipto.ship-name.
DEF VAR v-saddr           like shipto.ship-addr.
DEF VAR v-scity           like shipto.ship-city.
DEF VAR v-sstate          like shipto.ship-state.
DEF VAR v-szip            like shipto.ship-zip.
DEF VAR v-po-type         as char format "x(10)".
DEF VAR v-freight-dscr    as char format "x(7)".
DEF VAR v-change-dscr     as char format "x(7)".
DEF VAR v-dash-line       as char format "x(80)" extent 3.
DEF VAR v-adders          as log.
DEF VAR xg-flag           as log init no no-undo.
DEF VAR v-space           as log init YES NO-UNDO.
DEF VAR len-score         as CHAR NO-UNDO.
DEF VAR same-score        as CHAR no-undo.
DEF VAR v-test-scr        as log no-undo.
DEF VAR v-hdr             as char format "x(15)" initial "" no-undo.
DEF VAR v-ino-job         as char format "x(15)" initial "" no-undo.
DEF VAR v-change-ord      as char format "x(35)" initial "" no-undo.
DEF VAR v-inst-lines      AS INT NO-UNDO.
DEF VAR v-inst            AS CHAR FORM "x(80)" EXTENT 4 NO-UNDO.

DEF BUFFER xjob-mat FOR job-mat.
DEF BUFFER xitem    FOR item.

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha EXTENT 50 NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(60)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(60)" EXTENT 50 NO-UNDO.

ASSIGN ls-image1 = "images\sultana.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel     AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-fax     AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS CHAR FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add6 AS CHAR FORM "x(40)" NO-UNDO.

DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total  AS DEC NO-UNDO.
DEF VAR v-t-tax      as   dec extent 3 NO-UNDO.
DEF VAR v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no       LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline  AS INT NO-UNDO.
DEF VAR v-basis-w    AS DEC NO-UNDO.
DEF VAR v-dep        AS DEC NO-UNDO.
DEF VAR v-qty        LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR v-tot-sqft   AS DEC  NO-UNDO.
DEF VAR v-vend-item  AS CHAR NO-UNDO.
DEF VAR v-adder      AS CHAR FORM "x(15)" extent 5 no-undo.
DEF VAR v-num-add    as int initial 0 no-undo.
DEF VAR v-job-no     AS CHAR NO-UNDO.
DEF VAR v-mch-cod     AS CHAR NO-UNDO.
DEF VAR v-cost       AS DEC NO-UNDO.
DEF VAR v-setup      AS DEC NO-UNDO.
DEF VAR lv-item-rec  AS CHAR NO-UNDO.

DEF VAR v-fgitm      AS LOG  NO-UNDO.
DEF VAR v-fglist     AS CHAR NO-UNDO.
DEF VAR v-inolist    AS CHAR NO-UNDO.
DEF VAR v-cnt        AS INT  NO-UNDO.
DEF VAR v-totflg     AS LOG  NO-UNDO.

v-dash-line = fill ("_",80).

/*==============*/
DEF VAR lv-file-name AS cha FORM "x(60)" NO-UNDO.

OS-CREATE-DIR VALUE("c:\temp\fax") NO-ERROR.
IF ip-multi-faxout THEN DO:

  INPUT FROM OS-DIR ("C:\temp\fax") NO-ECHO.
  REPEAT:
      SET lv-file-name.  
      IF lv-file-name <> "." AND lv-file-name <> ".." THEN DO:     
         OS-DELETE VALUE("C:\temp\fax\" + lv-file-name) .       
      END.
  END.
END.
/*==================*/


{po/po-print.f}

ASSIGN v-hdr = "VEND ITEM".
       
FIND FIRST company WHERE company.company EQ cocode NO-LOCK. 

FIND FIRST cust 
  WHERE cust.company = cocode 
    AND cust.active = "X" NO-LOCK NO-ERROR.

ASSIGN v-comp-add1 = ""
       v-comp-add2 = ""
       v-comp-add3 = ""
       v-comp-add4 = "".

/*IF AVAIL cust 
  THEN
   ASSIGN v-comp-add1 = cust.addr[1]
          v-comp-add2 = TRIM(cust.addr[2])
          v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
          v-comp-add4 = "Phone:  " + STRING(cust.area-code,"(999)") + 
                                     STRING(cust.phone,"999-9999") 
          v-comp-add5 = "Fax  :  " + STRING(cust.fax,"(999)999-9999")
          v-comp-add6 = "Email:  " + cust.email . */

v-tot-sqft = 0.

print-po-blok:
 FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
     FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
     BREAK BY po-ord.vend-no BY PO-ORD.PO-NO:

     IF NOT CAN-FIND(FIRST po-ordl WHERE
        po-ordl.company EQ po-ord.company AND
        po-ordl.po-no EQ po-ord.po-no) THEN NEXT.

     FIND FIRST vend 
       WHERE vend.company EQ po-ord.company 
         AND vend.vend-no EQ po-ord.vend-no NO-LOCK NO-ERROR.

     IF ip-multi-faxout AND 
        FIRST-OF(po-ord.vend-no) 
       THEN DO:

        OUTPUT CLOSE.

        OUTPUT STREAM st-fax CLOSE.

        OUTPUT TO value("c:\temp\fax\fx" + po-ord.vend-no + ".xpr") 
            PAGE-SIZE value(ip-lines-per-page).

        OUTPUT STREAM st-fax TO 
            VALUE("c:\temp\fax\fx" + po-ord.vend-no + ".txt").

        PUT STREAM st-fax UNFORMATTED "FAX#:" 
            TRIM(STRING(vend.fax-prefix)) + 
            STRING(vend.fax-area,"x(3)")  + 
            STRING(vend.fax,"xxxxxxx") SKIP.

        PUT CONTROL "<PRINT=NO>".       
        PUT UNFORMATTED "<EXPORT=c:\temp\fax\fx" TRIM(vend.vend-no) ".tif,BW>" .

        /* PUT "FAX#:" cust.fax SKIP. */

     END. /* IF ip-multi-faxout */

     IF AVAIL company 
       THEN ASSIGN v-sname     = company.name
                   v-saddr [1] = company.addr [1]
                   v-saddr [2] = company.addr [2]
                   v-scity     = company.city
                   v-sstate    = company.state
                   v-szip      = company.zip.

     IF po-ord.type EQ "D"
       THEN ASSIGN v-sname     = po-ord.ship-name
                   v-saddr[1]  = po-ord.ship-addr[1]
                   v-saddr[2]  = po-ord.ship-addr[2]
                   v-scity     = po-ord.ship-city
                   v-sstate    = po-ord.ship-state
                   v-szip      = po-ord.ship-zip.

     {po/exportpo.i}

     ASSIGN v-page-counter  = 1
            v-change-ord    = "".

     IF po-ord.stat EQ "N" 
       THEN ASSIGN po-ord.stat = "O".
       ELSE 
        IF po-ord.stat EQ "U" 
          THEN v-change-ord = "(CHANGED ORDER ONLY)".

     FIND FIRST terms WHERE terms.t-code EQ po-ord.terms NO-LOCK NO-ERROR.

     FIND FIRST carrier 
       WHERE carrier.company EQ po-ord.company 
         AND carrier.carrier EQ po-ord.carrier NO-LOCK NO-ERROR.

     IF po-ord.type EQ "R" 
       THEN ASSIGN v-po-type = "Regular".
       ELSE ASSIGN v-po-type = "Drop Ship".

     IF po-ord.frt-pay EQ "P" 
       THEN ASSIGN v-freight-dscr = "Prepaid".
       ELSE 
        IF po-ord.frt-pay EQ "C" 
          THEN ASSIGN v-freight-dscr = "Collect".
          ELSE ASSIGN v-freight-dscr = "Bill".

     ASSIGN v-fgitm = NO
            v-fglist = "".

     /*FORM HEADER SKIP*/
      v-printline = 0.
      
      {po/po-sultn.i}
        
     /*========*/

     FOR EACH po-ordl WHERE
         po-ordl.company EQ po-ord.company AND
         po-ordl.po-no EQ po-ord.po-no BY po-ordl.line:

      ASSIGN xg-flag = no.

      IF NOT v-printde-po AND 
         po-ordl.deleted 
        THEN NEXT.

      ASSIGN v-change-dscr = "".

      IF po-ordl.stat EQ "A" 
        THEN ASSIGN v-change-dscr = "Added".
        ELSE 
         IF po-ordl.stat EQ "U" 
           THEN ASSIGN v-change-dscr = "Updated".
           ELSE 
            IF po-ordl.stat EQ "O" 
              THEN ASSIGN v-change-dscr = "Open".
              ELSE 
               IF po-ordl.stat EQ "P" 
                 THEN ASSIGN v-change-dscr = "Partial".
                 ELSE 
                  IF po-ordl.stat EQ "C" 
                    THEN ASSIGN v-change-dscr = "Closed".
                    

      IF po-ordl.deleted EQ YES THEN ASSIGN v-change-dscr = "Deleted".

      ASSIGN v-ino-job = po-ordl.vend-i-no.

      FIND item WHERE item.company EQ po-ordl.company
                  AND item.i-no    EQ po-ordl.i-no
                  AND po-ordl.item-type NO-LOCK NO-ERROR.

      v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) 
                       THEN ITEM.vend-item ELSE "") +
                    (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) 
                       THEN (" " + ITEM.vend2-item) ELSE "").

      ASSIGN v-wid = po-ordl.s-wid
             v-len = po-ordl.s-len
             v-vend-item = po-ordl.vend-i-no.

      IF AVAIL item AND 
         item.mat-type EQ "B" 
        THEN DO:

        IF v-shtsiz THEN DO:

          ASSIGN v-wid = po-ordl.s-wid - TRUNCATE(po-ordl.s-wid,0).
          ASSIGN v-wid = ( v-wid * 16 ) / 100.
          ASSIGN v-wid = TRUNCATE(po-ordl.s-wid,0) + v-wid.
          ASSIGN v-len = po-ordl.s-len - TRUNCATE(po-ordl.s-len,0).
          ASSIGN v-len = ( v-len * 16 ) / 100.
          ASSIGN v-len = TRUNCATE(po-ordl.s-len,0) + v-len.

          ASSIGN v-num-add = 0.

          FIND FIRST job WHERE job.company EQ cocode 
                           and job.job-no EQ STRING(FILL(" ",6 - 
                                                    LENGTH(TRIM(po-ordl.job-no)))) +
                                                    TRIM(po-ordl.job-no) 
                           AND job.job-no2 EQ po-ordl.job-no2 NO-LOCK NO-ERROR.
          IF AVAIL job THEN DO:

            FOR EACH job-mat
              WHERE job-mat.company  EQ cocode
                AND job-mat.job      EQ job.job
                AND job-mat.job-no   EQ job.job-no
                AND job-mat.job-no2  EQ job.job-no2
                AND job-mat.i-no     EQ po-ordl.i-no
                AND job-mat.frm      EQ po-ordl.s-num
               USE-INDEX job NO-LOCK
               BREAK BY job-mat.blank-no DESC:


               IF LAST(job-mat.blank-no) OR
                  job-mat.blank-no EQ po-ordl.b-num 
                 THEN LEAVE.
            END. /* FOR EACH JOB-MAT*/

            ASSIGN v-adder = "" .


            IF AVAIL job-mat THEN DO:

              /* Adder i-no and i-name to po of exist */
              FOR EACH xjob-mat NO-LOCK 
                WHERE xjob-mat.company  EQ cocode
                  AND xjob-mat.job      EQ job-mat.job
                  AND xjob-mat.job-no   EQ job-mat.job-no
                  AND xjob-mat.job-no2  EQ job-mat.job-no2
                  AND xjob-mat.frm      EQ job-mat.frm
                  AND xjob-mat.blank-no EQ job-mat.blank-no
                  AND xjob-mat.i-no     NE job-mat.i-no:

                FIND FIRST xitem 
                  WHERE xitem.company  EQ cocode
                    AND xitem.i-no     EQ xjob-mat.i-no
                    AND xitem.mat-type EQ "A" NO-LOCK NO-ERROR.
                IF AVAIL xitem THEN DO:
                  /*
                  
                  put xitem.i-no at 25 xitem.i-name at 38.
                  ASSIGN v-line-number = v-line-number + 1.
                  
                  */

                  ASSIGN v-num-add = v-num-add + 1.

                  IF v-num-add EQ 1 
                    THEN ASSIGN v-adder[1] = xitem.i-name.
                    ELSE 
                     IF v-num-add EQ 2 
                       THEN ASSIGN v-adder[2] = xitem.i-name.
                       ELSE 
                        if v-num-add EQ 3
                          THEN ASSIGN v-adder[3] = xitem.i-name.
                          ELSE 
                           IF v-num-add EQ 4 
                             THEN ASSIGN v-adder[4] = xitem.i-name.
                             ELSE 
                              IF v-num-add EQ 5 
                                THEN ASSIGN v-adder[5] = xitem.i-name.

                END. /* IF AVAIL xitem */
              END. /* FOR EACH xjob-mat */

              FIND FIRST ef WHERE EF.COMPANY EQ JOB.COMPANY
                              AND ef.est-no  EQ job.est-no
                              and ef.form-no EQ job-mat.frm NO-LOCK NO-ERROR.
              IF AVAIL ef AND 
                 (ef.xgrain EQ "S" OR 
                  ef.xgrain EQ "B") 
                 THEN ASSIGN xg-flag = yes.
            END.  /* avail job-mat */

          END.  /* avail job */

        END.  /* v-shtsiz */        
      END. /* avail item and item.mat-type EQ "B" */

      IF v-printline > 40 THEN DO:         
           PAGE.
           v-printline = 0.
           {po/po-sultn.i}
      END.
      v-mch-cod = "" .
      FOR EACH job-mch WHERE job-mch.company EQ cocode
          AND job-mch.job-no EQ po-ordl.job-no
          AND job-mch.job-no2 EQ po-ordl.job-no2
          AND job-mch.frm EQ po-ordl.s-num use-index line-idx NO-LOCK:

          ASSIGN v-mch-cod = job-mch.m-code .
          LEAVE.
      END.

      v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99").
      
      IF v-job-no = "-00" THEN v-job-no = "".

      PUT po-ordl.LINE       FORMAT ">>9"
          po-ordl.ord-qty                   SPACE(2)
          po-ordl.pr-qty-uom                SPACE(1)
          po-ordl.i-no       FORMAT "x(23)" SPACE(2)
          v-adder[1]  
          v-job-no           FORMAT "x(9)" 
          po-ordl.cost       FORMAT "->>>9.99<<"
          po-ordl.pr-uom
          po-ordl.t-cost     FORMAT "->>,>>9.99" 
         SKIP.

       FIND FIRST itemfg 
        WHERE itemfg.company = po-ordl.company
          AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.

      FIND FIRST  oe-ordl 
        WHERE oe-ordl.company = po-ordl.company
          AND oe-ordl.i-no = po-ordl.i-no 
          AND oe-ordl.ord-no = po-ordl.ord-no NO-LOCK NO-ERROR.

      IF AVAIL oe-ordl AND oe-ordl.part-no NE " "  THEN DO: 
         PUT oe-ordl.part-no FORMAT "x(25)" AT 25 SPACE(1)
                 v-mch-cod FORMAT "x(6)" AT 65 SKIP.
          v-printline = v-printline + 1.
          ASSIGN v-line-number = v-line-number + 1
                         v-mch-cod = "".
      END.
      ELSE IF AVAIL itemfg THEN DO:
         PUT itemfg.part-no FORMAT "x(25)" AT 25 SPACE(1) 
                 v-mch-cod FORMAT "x(6)" AT 65 SKIP.
          v-printline = v-printline + 1.
          ASSIGN v-line-number = v-line-number + 1
                         v-mch-cod = "".
      END. 

    IF v-mch-cod EQ "" THEN
      PUT STRING(po-ordl.over-pct,">>9.99%") + " / " +
          STRING(po-ordl.under-pct,">>9.99%")  FORMAT "x(17)" AT 4 
          po-ordl.i-name    AT 25 SPACE(1) 
/*           v-vend-item FORM "x(15)" space(1) */
              /*  v-mch-cod FORMAT "x(6)"    */             SPACE(16)
          v-adder[2]              SPACE(1)
          v-change-dscr  
         SKIP.
    ELSE
        PUT STRING(po-ordl.over-pct,">>9.99%") + " / " +
          STRING(po-ordl.under-pct,">>9.99%")  FORMAT "x(17)" AT 4 
          po-ordl.i-name    AT 25 SPACE(10) 
/*           v-vend-item FORM "x(15)" space(1) */
               v-mch-cod FORMAT "x(6)"                 SPACE(1)
          v-adder[2]              SPACE(1)
          v-change-dscr  
         SKIP.
    
      v-printline = v-printline + 2.

      ASSIGN v-line-number = v-line-number + 3.

      IF po-ordl.dscr[1] NE "" OR 
         v-adder[3] <> "" 
        THEN DO:

          PUT po-ordl.dscr[1] FORMAT "x(52)" AT 25 
              " "
              v-adder[3]  
             SKIP.

          v-line-number = v-line-number + 1.

          v-printline = v-printline + 1.
      END.

      IF po-ordl.dscr[2] NE "" THEN DO:

        PUT po-ordl.dscr[2] FORMAT "x(52)" AT 25              
            " "
            v-adder[4] 
           SKIP.

        v-line-number = v-line-number + 1.
        
        v-printline = v-printline + 1.
      END.
      
      IF AVAIL itemfg THEN DO:                        
        /* gdm - 07070901 */
        IF TRIM(itemfg.part-dscr3) <> "" THEN DO:

          PUT itemfg.part-dscr3  AT 25 SKIP.

          v-line-number = v-line-number + 1.

          v-printline = v-printline + 1.
        END.
        IF TRIM(itemfg.box-image) NE "" THEN ASSIGN v-fgitm  = YES.

        IF TRIM(itemfg.box-image) NE "" THEN
          IF SUBSTR(itemfg.box-image,LENGTH(itemfg.box-image) - 3, 4) EQ ".jpg"  OR 
           SUBSTR(itemfg.box-image,LENGTH(itemfg.box-image) - 3, 4) EQ ".bmt"
          THEN ASSIGN v-fglist  = v-fglist + "," +  itemfg.box-image
                      v-inolist = v-inolist + "," + itemfg.i-no.

        IF TRIM(v-fglist) EQ "," THEN ASSIGN v-fglist = "".
      END.
     
      IF TRIM(po-ordl.vend-i-no) <> "" THEN DO:            

          PUT  "Item#: "   AT 25 
              po-ordl.vend-i-no FORMAT  "x(15)" SKIP.
          v-line-number = v-line-number + 1.

          v-printline = v-printline + 1.

        END.

      v-setup = po-ordl.setup.

      IF po-ordl.item-type THEN DO:

        IF  v-wid   GT 0 OR
            v-len   GT 0 OR
            v-cost  GT 0 OR
            v-setup GT 0
          THEN DO:
        
           PUT 
            "W: " AT 25 
            v-wid SPACE(2) 
            "L: " v-len  
            "                        "
            STRING(v-cost) + " " + po-ordl.pr-uom + " $" +
            STRING(v-setup) + "SETUP" FORM "x(25)" SPACE(1)
            v-adder[5]  /*AT 78 */ 
            /* space(2) v-vend-item FORM "x(20)" */ 
            .

           ASSIGN v-line-number = v-line-number + 1
                  v-printline = v-printline + 1
                  v-totflg = YES.
        END. /* IF NOT ZERO */

      END.

      RUN po/po-ordls.p (RECID(po-ordl)).

      {po/poprints.i}

          IF NOT v-test-scr THEN DO:
            PUT SKIP
                "Score: " AT 8
                len-score format "x(70)" .

            v-line-number = v-line-number + 1.

            v-printline = v-printline + 1.

          END.
          ELSE 
           IF DEC(TRIM(len-score)) NE v-wid THEN DO:
             
             PUT SKIP 
                "Score: " AT 8
                 len-score format "x(70)" .

             v-line-number = v-line-number + 1.

             v-printline = v-printline + 1.
           END.
        END. /* if v-lscore-c ne "" from poprints.i */
       END.
      END. /* avail reftable from poprints.i*/

      PUT SKIP(1).

      ASSIGN v-line-number = v-line-number + 1. 
           
      v-printline = v-printline + 1.

      /* calc total sq feet */
      ASSIGN v-basis-w = 0
             v-dep     = 0.

      IF po-ordl.item-type 
        THEN
         FIND FIRST ITEM
           WHERE ITEM.company EQ po-ord.company
             AND ITEM.i-no    EQ po-ordl.i-no NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN ASSIGN v-basis-w = item.basis-w
                                   v-dep     = item.s-dep.

      IF po-ordl.pr-qty-uom EQ "MSF" 
        THEN v-qty = po-ordl.ord-qty.
        ELSE RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                                    v-basis-w, po-ordl.s-len, 
                                    po-ordl.s-wid, v-dep,
                                    po-ordl.ord-qty, OUTPUT v-qty).

      v-tot-sqft = v-tot-sqft + (v-qty * 1000).

      /* SPEC NOTE FROM PO LINE  */
      RUN get-notes (po-ordl.rec_key, "",2).

      /* SPEC NOTE FROM ITEM */
      IF v-print-sn THEN DO:          

        ASSIGN v-tmp-lines = 0
               v-inst-lines = 0
               lv-item-rec = "".
        
        lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                      ELSE IF AVAIL itemfg THEN itemfg.rec_key
                      ELSE "".
  
        IF lv-item-rec <> "" 
          THEN RUN get-notes(lv-item-rec, "PO",2).
       
      END. /* IF v-print-sn === end of specnote print */
     END.  /* FOR EACH po-ordl record */

     /* gdm - 07010906 */
     IF v-totflg
       THEN 
         PUT "Total Sq. Ft: "  AT 50 v-tot-sqft FORM ">,>>>,>>9" SKIP.
       

     v-tot-sqft = 0.

     v-bot-lab[1] = "GST        :" 
                    /*vend.tax-gr + "        :       " */ + 
                    STRING(po-ord.tax,"->>,>>9.99").
              
              RUN get-notes (po-ord.rec_key, "",1).
 
     IF v-printline > 45 THEN DO: 
           PAGE.
           v-printline = 0.
           {po/po-sultn.i}
     END.

     PUT "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
         "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "->>,>>9.99"
         "<=8><R+2> "  v-bot-lab[1] 
         "<=8><R+3> "  " " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
         /*v-bot-lab[2] */
         "<=8><R+4> Grand Total:" po-ord.t-cost FORM "->>,>>9.99" .

     PUT "<FArial><R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
         "<R59.5>"
         " Please acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
         " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
         "<R62>"
         ' ____________________Please initial & Fax to "Buyer" ' SKIP
        SKIP.


     v-printline = v-printline + 6.
     
     IF v-printline <= page-size THEN PUT SKIP(74 - v-printline).

     IF v-fgitm AND 
        TRIM(v-fglist) NE "" 
       THEN DO:
        
        DO v-cnt = 1 TO NUM-ENTRIES(v-fglist):

            IF ENTRY(v-cnt,v-fglist) EQ "" THEN NEXT.

            IF ENTRY(v-cnt,v-fglist) NE "" THEN DO:

              ASSIGN ls-image2[v-cnt] = ENTRY(v-cnt,v-fglist).

              FILE-INFO:FILE-NAME = ls-image2[v-cnt].

              ls-full-img2[v-cnt] = FILE-INFO:FULL-PATHNAME + ">".

            END.

            IF ls-full-img2[v-cnt] NE ? THEN PAGE.

            PUT
                "<FArial>"   SKIP
                "<C3><R2><#11><R+40><C+70>" "<IMAGE#2=" ls-full-img2[v-cnt] SKIP
                "<P15><C35><B>" ENTRY(v-cnt,v-inolist) "</B>".

        END.
     END.

END. /* FOR EACH po-ord record */.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */


PROCEDURE get-notes:

    DEF INPUT PARAM ip-rec-key AS CHAR NO-UNDO.
    DEF INPUT PARAM ip-codes   AS CHAR NO-UNDO.
    DEF INPUT PARAM ip-skip    AS INT  NO-UNDO.

    DEF VAR v-licnt AS INT  NO-UNDO.
    DEF VAR v-icnt  AS INT  NO-UNDO.
    DEF VAR v-text  AS CHAR NO-UNDO.

    FOR EACH tt-formtext: DELETE tt-formtext. END.

    ASSIGN v-text = "".    
    
    IF TRIM(ip-codes) EQ ""
      THEN
       FOR EACH notes NO-LOCK 
         WHERE notes.rec_key EQ ip-rec-key:

         ASSIGN v-text = v-text + " " + 
                          TRIM(notes.note_text) + CHR(10).
       END.
      ELSE 
       FOR EACH notes NO-LOCK 
         WHERE notes.rec_key EQ ip-rec-key
           AND notes.note_code = TRIM(ip-codes):

         ASSIGN v-text = v-text + " " + 
                          TRIM(notes.note_text) + CHR(10).
       END.

    IF v-text <> "" 
      THEN
       DO v-licnt = 1 TO 10:

         CREATE tt-formtext.
         ASSIGN tt-line-no = v-licnt
                tt-length  = 80.
    END.

    RUN custom/formtext.p (v-text).

    ASSIGN v-icnt = 0.

    IF v-printline > 45 THEN DO:   
           PAGE.
           v-printline = 0.
           {po/po-sultn.i}
      END.
          
   FOR EACH tt-formtext:
       
       ASSIGN v-icnt = v-icnt + 1.
       IF v-icnt <= 10 THEN DO:
           
           /* IF ip-codes NE "" 
           THEN PUT {1}.*/
           IF v-printline > 45 THEN DO:
               PAGE.
               v-printline = 0.
               {po/po-sultn.i}
           END.

           IF tt-formtext.tt-text <> "" THEN do:
               PUT tt-formtext.tt-text FORM "x(80)" 
                   SKIP.      
               v-printline = v-printline + 1.
           END.
         END.
    END.
     IF CAN-FIND(FIRST tt-formtext WHERE TRIM(tt-formtext.tt-text) NE "") 
             THEN
             PUT SKIP(ip-skip).
    
END PROCEDURE.
