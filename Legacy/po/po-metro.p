/* --------------------------------------------- po/po-metro.p  */
/* Purchase Order XPrint Program for N-K-POPRINT = metro */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ip-multi-faxout AS LOG NO-UNDO. /* fax multiple recipents or single */
DEFINE INPUT PARAMETER  ip-lines-per-page AS INTEGER NO-UNDO.
DEFINE STREAM st-fax.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEFINE BUFFER b-ref1  FOR reftable.
DEFINE BUFFER b-ref2  FOR reftable.

{po/po-print.i}

DEFINE VARIABLE v-wid LIKE po-ordl.s-wid FORMAT ">>9.9999" NO-UNDO.
DEFINE VARIABLE v-len LIKE po-ordl.s-len FORMAT ">>9.9999" NO-UNDO.
DEFINE VARIABLE pol-counter AS INTEGER NO-UNDO.
DEFINE VARIABLE save_id AS RECID.
DEFINE VARIABLE time_stamp AS CHARACTER.
DEFINE VARIABLE v-exp-limit AS INTEGER NO-UNDO INIT 10.
DEFINE VARIABLE v-line-number AS INTEGER.
DEFINE VARIABLE v-page-counter AS INTEGER FORMAT ">>9".
DEFINE VARIABLE v-lines-to-skip AS INTEGER.
DEFINE VARIABLE v-sname LIKE shipto.ship-name.
DEFINE VARIABLE v-saddr LIKE shipto.ship-addr.
DEFINE VARIABLE v-scity LIKE shipto.ship-city.
DEFINE VARIABLE v-sstate LIKE shipto.ship-state.
DEFINE VARIABLE v-szip LIKE shipto.ship-zip.
DEFINE VARIABLE v-po-type AS CHARACTER FORMAT "x(10)".
DEFINE VARIABLE v-freight-dscr AS CHARACTER FORMAT "x(7)".
DEFINE VARIABLE v-change-dscr AS CHARACTER FORMAT "x(7)".
DEFINE VARIABLE v-dash-line AS CHARACTER FORMAT "x(80)" EXTENT 3.
DEFINE VARIABLE v-adders AS LOG.
DEFINE VARIABLE xg-flag AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE v-space AS LOG INIT YES.
DEFINE VARIABLE len-score AS CHARACTER.
DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE BUFFER xitem FOR item.
DEFINE VARIABLE same-score AS ch NO-UNDO.
DEFINE VARIABLE v-test-scr AS LOG NO-UNDO.
DEFINE VARIABLE v-hdr AS CHARACTER FORMAT "x(15)" INITIAL "" NO-UNDO.
DEFINE VARIABLE v-ino-job AS CHARACTER FORMAT "x(15)" INITIAL "" NO-UNDO.
DEFINE VARIABLE v-change-ord AS CHARACTER FORMAT "x(35)" INITIAL "" NO-UNDO.
DEFINE VARIABLE v-tmp-lines AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-inst-lines AS INTEGER NO-UNDO.
DEFINE VARIABLE v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE lv-tot-pg AS INTEGER NO-UNDO.
DEFINE VARIABLE ln-cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-pg-num AS INTEGER NO-UNDO.

DEFINE BUFFER b-cost FOR reftable.
DEFINE BUFFER b-qty FOR reftable.
DEFINE BUFFER b-setup FOR reftable.

DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20.


/* === with xprint ====*/
DEFINE VARIABLE ls-image1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS CHARACTER FORM "x(60)" NO-UNDO.    
DEFINE VARIABLE ls-image2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img2 AS CHARACTER FORM "x(150)" NO-UNDO.

ASSIGN ls-image1 = "images\metro.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
ASSIGN ls-image2 = "images\GPI Terms and Conditions 8-6-2014-page-001.jpg"
       FILE-INFO:FILE-NAME = ls-image2
       ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEFINE VARIABLE v-tel AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact AS cha FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-line-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-quo-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-t-tax      AS   DECIMAL EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bot-lab    AS   CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-q-no LIKE oe-ord.q-no NO-UNDO.
DEFINE VARIABLE v-printline AS INTEGER NO-UNDO.
DEFINE VARIABLE v-basis-w AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-dep AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-qty LIKE po-ordl.ord-qty NO-UNDO.
DEFINE VARIABLE v-tot-sqft AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-vend-item AS cha NO-UNDO.
DEFINE VARIABLE v-adder AS cha FORM "x(15)" EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-num-add AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-job-no AS cha NO-UNDO.
DEFINE VARIABLE v-cost AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-setup AS DECIMAL NO-UNDO.
DEFINE VARIABLE lv-item-rec AS cha NO-UNDO.
DEFINE VARIABLE lv-got-return AS INTEGER NO-UNDO.
v-dash-line = FILL ("_",80).

/*==============*/
DEFINE VARIABLE lv-file-name AS cha FORM "x(60)" NO-UNDO.
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
IF AVAILABLE company THEN
ASSIGN
 v-sname     = company.name
 v-saddr [1] = company.addr [1]
 v-saddr [2] = company.addr [2]
 v-scity     = company.city
 v-sstate    = company.state
 v-szip      = company.zip
 v-comp-add1 = company.addr[1]
 v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
 v-comp-add3 = "Phone: 604.533.2545" 
 v-comp-add4 = "Fax  : 604.533.2633".
 .
 v-tot-sqft = 0.
    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
        BREAK BY po-ord.vend-no BY PO-ORD.PO-NO:

        IF NOT CAN-FIND(FIRST po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no) THEN NEXT.        

        FIND FIRST vend WHERE vend.company EQ po-ord.company 
                  AND vend.vend-no EQ po-ord.vend-no NO-LOCK NO-ERROR.

        IF ip-multi-faxout AND FIRST-OF(po-ord.vend-no) THEN DO:
           OUTPUT CLOSE.
           OUTPUT STREAM st-fax CLOSE.
           OUTPUT TO value("c:\temp\fax\fx" + po-ord.vend-no + ".xpr") PAGE-SIZE value(ip-lines-per-page).
           OUTPUT STREAM st-fax TO value("c:\temp\fax\fx" + po-ord.vend-no + ".txt").
           PUT STREAM st-fax UNFORMATTED "FAX#:" STRING(vend.fax-area,"x(3)") + STRING(vend.fax,"xxxxxxx") SKIP.
           PUT CONTROL "<PRINT=NO>".       
           PUT UNFORMATTED "<EXPORT=c:\temp\fax\fx" TRIM(vend.vend-no) ".tif,BW>" .
           /*
           PUT "FAX#:" cust.fax SKIP.*/
        END.

      IF po-ord.type EQ "D" THEN
        ASSIGN v-sname     = po-ord.ship-name
               v-saddr[1]  = po-ord.ship-addr[1]
               v-saddr[2]  = po-ord.ship-addr[2]
               v-scity     = po-ord.ship-city
               v-sstate    = po-ord.ship-state
               v-szip      = IF LENGTH(po-ord.ship-zip) > 5 THEN STRING(po-ord.ship-zip,"xxxxx-xxxx")
                             ELSE po-ord.ship-zip.

      {po/exportpo.i}

      ASSIGN v-page-counter  = 1
             v-change-ord    = "".

      IF po-ord.stat EQ "N" THEN
        ASSIGN po-ord.stat = "O".
/*        
      else
      if po-ord.stat eq "U" then
        v-change-ord = "(CHANGED ORDER ONLY)".
*/
      FIND FIRST terms WHERE terms.t-code EQ po-ord.terms NO-LOCK NO-ERROR.
      FIND FIRST carrier WHERE carrier.company EQ po-ord.company 
                           AND carrier.carrier EQ po-ord.carrier NO-LOCK NO-ERROR.

      IF po-ord.type EQ "R" THEN
        ASSIGN v-po-type = "Regular".
      ELSE
        ASSIGN v-po-type = "Drop Ship".

      IF po-ord.frt-pay EQ "P" THEN
        ASSIGN v-freight-dscr = "Prepaid".
      ELSE IF po-ord.frt-pay EQ "C" THEN
        ASSIGN v-freight-dscr = "Collect".
      ELSE
        ASSIGN v-freight-dscr = "Bill".

/*FORM HEADER SKIP*/
      v-printline = 0.
      {po/po-metro.i}
        
      /*========*/
      IF v-print-terms THEN
          lv-tot-pg = 2.
      ELSE
          lv-tot-pg = 1.

      ln-cnt = 0.
      FOR EACH po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no NO-LOCK :
          FIND item WHERE item.company EQ po-ordl.company
                    AND item.i-no    EQ po-ordl.i-no
                    AND po-ordl.item-type
                  NO-LOCK NO-ERROR.

          FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.

        v-vend-item = /*(IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                        +
                      (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "").
                      */
                      po-ordl.vend-i-no.
          ln-cnt = ln-cnt + 4.
          IF v-vend-item <> "" THEN ln-cnt = ln-cnt + 1.
       
          v-tmp-lines = 0.
          v-inst-lines = 0.
          FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
            IF notes.note_text <> "" THEN DO:
               v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
               {SYS/INC/ROUNDUP.I v-tmp-lines}
               v-inst-lines = v-inst-lines + v-tmp-lines. 
            END.
          END.
          IF v-inst-lines GT 0 THEN v-inst-lines = v-inst-lines + 1.
          ln-cnt = ln-cnt + v-inst-lines.
       
          /* === spec note print */
          IF v-print-sn THEN DO:
             ASSIGN v-tmp-lines = 0
                   v-inst-lines = 0
                   lv-item-rec = "".

            lv-item-rec = IF po-ordl.item-type AND AVAILABLE ITEM THEN ITEM.rec_key
                      ELSE IF AVAILABLE itemfg THEN itemfg.rec_key
                      ELSE "".
            IF lv-item-rec <> "" THEN DO:
               FOR EACH notes WHERE notes.rec_key = lv-item-rec AND 
                   /*notes.note_type = "S" */ notes.note_code = "PO" NO-LOCK:
                   IF notes.note_text <> "" THEN DO:
                     v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                     {SYS/INC/ROUNDUP.I v-tmp-lines}
                     v-inst-lines = v-inst-lines + v-tmp-lines. 
                   END.
               END.
               IF v-inst-lines GT 0 THEN v-inst-lines = v-inst-lines + 1.
               ln-cnt = ln-cnt + v-inst-lines .
            END.
          END.
       
      END.  /* each po-ordl */
      v-inst-lines = 0.
      v-tmp-lines = 0.
      FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
           IF notes.note_text <> "" THEN DO:
              v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
              {SYS/INC/ROUNDUP.I v-tmp-lines}
              v-inst-lines = v-inst-lines + v-tmp-lines. 
           END.
      END.
      IF v-inst-lines GT 0 THEN v-inst-lines = v-inst-lines + 1.
      ln-cnt = ln-cnt + v-inst-lines.     
               
      lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0) .  /* 23->47 25 po detail lines */
      /*  end of getting total page per po */

      FOR EACH po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no BY po-ordl.line:
        ASSIGN xg-flag = NO.
        IF NOT v-printde-po AND po-ordl.deleted THEN NEXT.
        ASSIGN v-change-dscr = "".

        IF po-ordl.stat EQ "A" THEN ASSIGN v-change-dscr = "Added".
        ELSE IF po-ordl.stat EQ "U" THEN ASSIGN v-change-dscr = "Updated".
        ELSE IF po-ordl.stat EQ "O" THEN ASSIGN v-change-dscr = "Open".
        ELSE IF po-ordl.stat EQ "P" THEN ASSIGN v-change-dscr = "Partial".
        ELSE IF po-ordl.stat EQ "C" THEN ASSIGN v-change-dscr = "Closed".

        IF po-ordl.deleted EQ YES THEN   ASSIGN v-change-dscr = "Deleted".

        ASSIGN v-ino-job = po-ordl.vend-i-no.
        
        FIND item WHERE item.company EQ po-ordl.company
                    AND item.i-no    EQ po-ordl.i-no
                    AND po-ordl.item-type
                  NO-LOCK NO-ERROR.
        v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                        +
                      (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "").
        ASSIGN v-wid = po-ordl.s-wid
               v-len = po-ordl.s-len
               v-vend-item = po-ordl.vend-i-no.


        IF v-printline > 47 THEN DO:         
           PUT "<R63><C70>Page " STRING(PAGE-NUMBER - lv-pg-num,">>9") + " of " + string(lv-tot-pg ) FORM "x(20)" .
           PAGE.
           v-printline = 0.
           {po/po-metro.i}
        END.

        v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99").
        IF v-job-no = "-00" THEN v-job-no = "".
        v-adder = "".

        PUT po-ordl.LINE FORM ">>9"
            po-ordl.ord-qty SPACE(2)
            po-ordl.pr-qty-uom SPACE(1)
            po-ordl.i-no FORM "x(25)" SPACE(1)
            v-job-no FORM "x(9)" AT 58 SPACE(3)
            po-ordl.cost FORM "->>>,>>9.99<<"
            po-ordl.pr-uom
            po-ordl.t-cost FORM "->>>,>>9.99"  SKIP.

        PUT po-ordl.i-name AT 25 SPACE(1)
            v-change-dscr AT 77  SKIP.
            
        IF po-ordl.dscr[1] <> "" THEN          
           PUT po-ordl.dscr[1] AT 25 SKIP.    
                
        v-printline = v-printline + 3.
        ASSIGN v-line-number = v-line-number + 4.
  
        FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.

        IF v-vend-item <> "" THEN DO:
           PUT TRIM(po-ord.vend) AT 25 "Item#: " v-vend-item FORMAT "x(15)" SKIP.
           v-line-number = v-line-number + 1.
           v-printline = v-printline + 1.
        END.

        RUN calc-cost (RECID(po-ordl),OUTPUT v-cost,OUTPUT v-setup).            

        IF AVAILABLE ITEM AND ITEM.mat-type = "C" THEN
            PUT "W: " AT 25 v-wid SPACE(2) "L: " v-len  SPACE(2)
                "D: " ITEM.case-d FORM ">>>9.99<<".
        ELSE
        PUT "W: " AT 25 v-wid SPACE(2) "L: " v-len  
                 "                          " 
          /*      STRING(v-cost) + " " + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)"   
                 v-adder[5] AT 78  */
               /* space(2) v-vend-item FORM "x(20)" */  .
            
            ASSIGN v-line-number = v-line-number + 1
                   v-printline = v-printline + 1.       

        PUT SKIP(1).
        ASSIGN v-line-number = v-line-number + 1.
        v-printline = v-printline + 1.
      /* calc total sq feet */
    
        ASSIGN v-basis-w = 0
               v-dep     = 0.

    IF po-ordl.item-type THEN
    FIND FIRST ITEM
        WHERE ITEM.company EQ po-ord.company
          AND ITEM.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE ITEM THEN
      ASSIGN
       v-basis-w = item.basis-w
       v-dep     = item.s-dep.

    IF po-ordl.pr-qty-uom EQ "MSF" THEN
      v-qty = po-ordl.ord-qty.
    ELSE
      RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                             v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                             po-ordl.ord-qty, OUTPUT v-qty).

     v-tot-sqft = v-tot-sqft + (v-qty * 1000).

   ASSIGN v-inst = ""
           v-tmp-lines = 0
           j = 0
           K = 0
           lv-got-return = 0.

   
    FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
       DO i = 1 TO LENGTH(notes.note_text) :        
           IF i - j >= 82 THEN ASSIGN j = i
                                      lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / 82.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return.

           IF k < 5 THEN v-inst[k] = v-inst[k] +
                                     IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1)
                                     ELSE "" .              
           
           IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
           THEN DO:
                  lv-got-return = lv-got-return + 1.
                  j = i.        
           END.
       END.
    END.
    v-printline = v-printline + /* IF k < 5 THEN 4 ELSE*/  k.

    IF v-printline > 47 THEN DO:         
           PUT "<R63><C70>Page " STRING(PAGE-NUMBER - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PUT "<FBook Antiqua>"
              /*    "<R56><C1><From><R56><C35><LINE>"
                  "<R55><C1>X"
                  "<R56><C5>Authorized Signature" */
                  "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP .
               IF v-print-terms THEN
                  PUT
                  "   – see attached page." SKIP
                  "" SKIP
                  SKIP.    
           PAGE.
           v-printline = 0.
           {po/po-metro.i}
    END.

    DO i = 1 TO 4:
       IF v-inst[i] <> "" THEN DO: 
          PUT v-inst[i] FORM "x(82)" SKIP.          
       END.                           
    END.    
     
     /* === spec note print */
     IF v-print-sn THEN DO:
        ASSIGN v-tmp-lines = 0
               v-inst-lines = 0
               lv-item-rec = "".

        lv-item-rec = IF po-ordl.item-type AND AVAILABLE ITEM THEN ITEM.rec_key
                      ELSE IF AVAILABLE itemfg THEN itemfg.rec_key
                      ELSE "".
        IF lv-item-rec <> "" THEN DO:
           FOR EACH notes WHERE notes.rec_key = lv-item-rec AND 
               /*notes.note_type = "S" */ notes.note_code = "PO" NO-LOCK:
              IF notes.note_text <> "" THEN DO:
                 v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                v-inst-lines = v-inst-lines + v-tmp-lines. 
              END.
           END.
           IF v-inst-lines GT 0 THEN v-inst-lines = v-inst-lines + 1.
           v-printline = v-printline + v-inst-lines .
           IF v-printline > 47 THEN DO:         
              PUT "<R63><C70>Page " STRING(PAGE-NUMBER - lv-pg-num,">>9") + " of " + string(lv-tot-pg ) FORM "x(20)" .
              PUT "<FBook Antiqua>"
                 /* "<R56><C1><From><R56><C35><LINE>"
                  "<R55><C1>X"
                  "<R56><C5>Authorized Signature" */
                  "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP.
              IF v-print-terms THEN
                  PUT
                  "   – see attached page." SKIP
                  "" SKIP
                  SKIP.     
            
              PAGE.
              v-printline = 0.
              {po/po-metro.i}
           END.     
           ASSIGN v-inst = ""
                  v-tmp-lines = 0
                  j = 0
                  K = 0
                  lv-got-return = 0.
           FOR EACH notes WHERE notes.rec_key = lv-item-rec AND
               /* notes.note_type = "S" */  notes.note_code = "PO" NO-LOCK:
               DO i = 1 TO LENGTH(notes.note_text) :        
                  IF i - j >= 82 THEN ASSIGN j = i
                                      lv-got-return = lv-got-return + 1.
                  v-tmp-lines = ( i - j ) / 82.
                  {SYS/INC/ROUNDUP.I v-tmp-lines}
                  k = v-tmp-lines + lv-got-return.

                  IF k < 5 THEN v-inst[k] = v-inst[k] +
                                     IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1)
                                     ELSE "" .              
           
                  IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
                  THEN DO:
                     lv-got-return = lv-got-return + 1.
                     j = i.        
                  END.
               END.
               /* v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
               {SYS/INC/ROUNDUP.I v-tmp-lines}
               IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                  PUT {1} substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" v-printline v-tmp-lines i SKIP.              
                  v-printline = v-printline + 1.
               END.*/
           END.
           DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN PUT {1} v-inst[i] FORM "x(82)"  SKIP.              
           END.
        END. /* lv-item-spec <> "" */
     END.
     /* === end of specnote print */

  END. /* for each po-ordl record */
  
       IF v-printline > 47 THEN DO:                  
          PUT "<R63><C70>Page " STRING(PAGE-NUMBER - lv-pg-num,">>9") + " of " + string(lv-tot-pg ) FORM "x(20)" .
              PUT "<FBook Antiqua>"
              /*    "<R56><C1><From><R56><C35><LINE>"
                  "<R55><C1>X"
                  "<R56><C5>Authorized Signature" */
                  "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP .
              IF v-print-terms THEN
                  PUT
                  "   – see attached page." SKIP
                  " " SKIP
                  SKIP.     
          PAGE.
          v-printline = 0.
          {po/po-metro.i}
       END.

  v-inst = "".
  FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
                v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                   /*PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                   v-printline = v-printline + 1.                                           */
                   IF i < 5  THEN  /* display upto 4 lines */
                       ASSIGN v-inst[i] =  SUBSTRING(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80)
                              v-printline = v-printline + 1.
                   ELSE LEAVE.
                END.

   END.
  
       IF v-printline > 47 THEN DO:                  
           PUT "<R63><C70>Page " STRING(PAGE-NUMBER - lv-pg-num,">>9") + " of " + string(lv-tot-pg ) FORM "x(20)" .
           PUT "<FBook Antiqua>"
                  "<R56><C1><From><R56><C35><LINE>"
                  "<R55><C1>X"
                  "<R56><C5>Authorized Signature" .
          PAGE.
          v-printline = 0.
          {po/po-metro.i}

       END.

   /*   PUT "Total Sq. Ft: "  AT 50 v-tot-sqft FORM ">>>,>>9" SKIP. */

      v-tot-sqft = 0.
/*      
      v-bot-lab[1] = "nge        :"
                     /*vend.tax-gr + "        :       " */ + STRING(po-ord.tax,"$->>,>>9.99").
*/
      PUT /*"<R53><C1>" */ v-inst[1] 
          /*"<R54><C1>" */ v-inst[2] 
          /*"<R55><C1>"*/  v-inst[3] 
          /*"<R56><C1>" */ v-inst[4] 
          "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
    "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "->>>,>>9.99"
    "<=8><R+2> "  v-bot-lab[1] 
    "<=8><R+3> "  " " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
                /*v-bot-lab[2] */
    "<=8><R+4> Grand Total:" po-ord.t-cost FORM "->>>,>>9.99" 
    /*"<=8><R+5><C+10>Page " string(PAGE-NUM,">>9") + " of <#Pages>"  FORM "x(20)"   */
    "<=8><R+5><C+10>Page " STRING(PAGE-NUMBER - lv-pg-num,">>9") + " of " + string(lv-tot-pg ) FORM "x(20)" .

PUT "<FBook Antiqua>"
    "<R56><C1><From><R56><C35><LINE>"
    "<R55><C1>X"
    "<R56><C5>Authorized Signature"
    "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP .
  IF v-print-terms THEN DO:
    PUT "   – see attached page." SKIP
        " " SKIP
        SKIP.     
   PAGE.
   PUT "<FCourier New>"   SKIP
       "<C4><R1><#1><R+110><C+77>"    /* larger */
       "<IMAGE#1=" ls-full-img2  SKIP  . 
  END.

   

     
IF LAST-OF(po-ord.po-no) THEN lv-pg-num = PAGE-NUMBER.
v-printline = v-printline + 6.
IF v-printline <= PAGE-SIZE THEN PUT SKIP(74 - v-printline).

END. /* for each po-ord record */.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */


PROCEDURE calc-cost:

 DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
 DEFINE OUTPUT PARAMETER op-cost AS DECIMAL NO-UNDO.
 DEFINE OUTPUT PARAMETER op-setup AS DECIMAL NO-UNDO.
 DEFINE VARIABLE vv-qty  AS DECIMAL NO-UNDO.
 DEFINE VARIABLE vv-cost AS DECIMAL NO-UNDO.
 
 DEFINE VARIABLE vv-setup AS DECIMAL NO-UNDO.
 DEFINE VARIABLE li AS INTEGER NO-UNDO.

 DEFINE VARIABLE vv-basis-w AS DECIMAL NO-UNDO.
 /*DEF VAR v-len AS DEC NO-UNDO.
 DEF VAR v-wid AS DEC NO-UNDO. */
 DEFINE VARIABLE vv-dep AS DECIMAL NO-UNDO.
 
 DEFINE VARIABLE v-ord-qty AS DECIMAL NO-UNDO.

 DEFINE BUFFER b-po-ordl FOR po-ordl.
 DEFINE BUFFER b-po-ord FOR po-ord.
 
 FIND b-po-ordl WHERE RECID(b-po-ordl) = ip-recid NO-LOCK .
 FIND FIRST b-po-ord WHERE
      b-po-ord.company EQ b-po-ordl.company AND
      b-po-ord.po-no   EQ b-po-ordl.po-no
      NO-LOCK.

   ASSIGN
  /*  v-len = (po-ordl.s-len)
    v-wid = (po-ordl.s-wid) */
    v-ord-qty = (b-po-ordl.ord-qty).
    /*{po/calc10.i v-len}
    {po/calc10.i v-wid}.
    */
   FIND FIRST e-item
       WHERE e-item.company EQ cocode
         AND e-item.i-no    EQ b-po-ordl.i-no
       NO-LOCK NO-ERROR.

   IF AVAILABLE e-item THEN
   FIND FIRST e-item-vend OF e-item
       WHERE e-item-vend.vend-no EQ b-po-ord.vend-no
       NO-LOCK NO-ERROR.

   IF AVAILABLE e-item-vend THEN DO:
     FIND FIRST ITEM
         WHERE ITEM.company EQ cocode
           AND ITEM.i-no    EQ b-po-ordl.i-no
         NO-LOCK NO-ERROR.

     ASSIGN
      vv-basis-w = IF AVAILABLE ITEM THEN ITEM.basis-w ELSE vv-basis-w
      vv-dep     = IF AVAILABLE ITEM THEN ITEM.s-dep ELSE vv-dep
      vv-cost    = (b-po-ordl.cost)
      vv-qty     = (b-po-ordl.ord-qty).

     IF e-item.std-uom NE b-po-ordl.pr-qty-uom THEN
       RUN sys/ref/convquom.p(b-po-ordl.pr-qty-uom,
                              e-item.std-uom, vv-basis-w,
                              v-len, v-wid, vv-dep,
                              vv-qty, OUTPUT vv-qty).

     vv-setup = 0.

     EMPTY TEMP-TABLE tt-eiv.
     CREATE tt-eiv.
     DO li = 1 TO 10:
        ASSIGN
           tt-eiv.run-qty[li] = e-item-vend.run-qty[li]
           tt-eiv.run-cost[li] = e-item-vend.run-cost[li]
           tt-eiv.setups[li] = e-item-vend.setups[li].
     END.

     FIND FIRST b-qty WHERE
          b-qty.reftable = "vend-qty" AND
          b-qty.company = e-item-vend.company AND
	      b-qty.CODE    = e-item-vend.i-no AND
          b-qty.code2   = e-item-vend.vend-no
          NO-LOCK NO-ERROR.
     
     IF AVAILABLE b-qty THEN
     DO:
        FIND FIRST b-cost WHERE
             b-cost.reftable = "vend-cost" AND
             b-cost.company = e-item-vend.company AND
	         b-cost.CODE    = e-item-vend.i-no AND
             b-cost.code2   = e-item-vend.vend-no
             NO-LOCK NO-ERROR.

        FIND FIRST b-setup WHERE
             b-setup.reftable = "vend-setup" AND
             b-setup.company = e-item-vend.company AND
             b-setup.CODE    = e-item-vend.i-no AND
             b-setup.code2   = e-item-vend.vend-no
             NO-LOCK NO-ERROR.
     
        DO li = 1 TO 10:
           ASSIGN
              tt-eiv.run-qty[li + 10] = b-qty.val[li]
              tt-eiv.run-cost[li + 10] = b-cost.val[li]
              tt-eiv.setups[li + 10] = b-setup.val[li].
        END.
     END.


     DO li = 1 TO 20:
       IF tt-eiv.run-qty[li] LT vv-qty THEN NEXT.
       ASSIGN
          vv-cost = tt-eiv.run-cost[li] * vv-qty
          vv-setup = tt-eiv.setups[li].
       LEAVE.
     END.
     
     IF vv-qty <> 0 THEN vv-cost = vv-cost / vv-qty.  
     ELSE vv-cost = vv-cost.

     IF e-item.std-uom NE b-po-ordl.pr-uom THEN
         RUN sys/ref/convcuom.p(e-item.std-uom,
                                b-po-ordl.pr-uom, vv-basis-w,
                                v-len, v-wid, vv-dep,
                                vv-cost, OUTPUT vv-cost).     
  END.
  ELSE vv-cost = b-po-ordl.cost.

  ASSIGN op-cost = vv-cost
         op-setup = vv-setup.

END PROCEDURE.

