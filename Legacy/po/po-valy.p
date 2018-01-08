/* --------------------------------------------- po/po-valy.p                 */
/* Purchase Order XPrint Program for N-K-POPRINT = Valley                     */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER ip-multi-faxout AS LOG NO-UNDO. /* fax multiple recipents or single */
DEF INPUT PARAM  ip-lines-per-page AS INT NO-UNDO.
DEF STREAM st-fax.
DEF BUFFER bf-poordl FOR po-ordl.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-ref1  for reftable.
def buffer b-ref2  for reftable.

{po/po-print.i}

def var v-wid like po-ordl.s-wid format ">>9.99" no-undo.
def var v-len like po-ordl.s-len format ">>9.99" no-undo.
def var pol-counter as int no-undo.
def var save_id as RECID NO-UNDO.
def var time_stamp as CHAR NO-UNDO.
def var v-exp-limit as int no-undo init 10 .
def var v-page-counter as int format ">>9" NO-UNDO.
def var v-lines-to-skip as INT NO-UNDO.
def var v-sname like shipto.ship-name NO-UNDO.
def var v-saddr like shipto.ship-addr NO-UNDO.
def var v-scity like shipto.ship-city NO-UNDO.
def var v-sstate like shipto.ship-state NO-UNDO.
def var v-szip like shipto.ship-zip NO-UNDO.
def var v-po-type as char format "x(10)" NO-UNDO.
def var v-freight-dscr as char format "x(7)" NO-UNDO.
def var v-change-dscr as char format "x(7)" NO-UNDO.
def var v-dash-line as char format "x(80)" extent 3 NO-UNDO.
def var v-adders as log NO-UNDO.
def var xg-flag as log init no no-undo.
def var v-space as log init yes NO-UNDO.
def var len-score as char NO-UNDO.
def buffer xjob-mat for job-mat .
def buffer xitem for item .
def var same-score as ch no-undo.
def var v-test-scr as log no-undo.
def var v-hdr as char format "x(15)" initial "" no-undo.
def var v-ino-job as char format "x(15)" initial "" no-undo.
def var v-change-ord as char format "x(35)" initial "" no-undo.
DEF VAR v-tmp-lines AS DEC NO-UNDO.
DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 12 NO-UNDO.
DEF VAR lv-tot-pg AS INT NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR v-ord-qty LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR v-t-cost LIKE po-ordl.t-cost NO-UNDO.
DEF VAR v-po-cost LIKE po-ordl.cost NO-UNDO.


DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(60)" NO-UNDO.
DEF VAR v-print-blank AS LOG NO-UNDO.
DEF VAR v-line AS INT NO-UNDO.
DEF VAR vs-ord-qty AS cha NO-UNDO.

ASSIGN ls-image1 = "images\valley.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-basis-w AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-qty LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR v-tot-sqft AS DEC NO-UNDO.
DEF VAR v-vend-item AS cha NO-UNDO.
def var v-adder AS cha FORM "x(15)" extent 5 no-undo.
def var v-num-add as int initial 0 no-undo.
DEF VAR v-job-no AS cha NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-setup AS DEC NO-UNDO.
DEF VAR v-msf AS DEC NO-UNDO.

DEF VAR lv-item-rec AS cha NO-UNDO.
DEF VAR lv-got-return AS int NO-UNDO.
DEF VAR lv-text AS cha NO-UNDO.
DEF VAR lv-text-line AS INT NO-UNDO.
DEF VAR lv-text-line-length AS INT NO-UNDO.
DEF VAR lv-char AS cha NO-UNDO.
DEF VAR lv-char-list AS cha NO-UNDO.
DEF TEMP-TABLE tt-text NO-UNDO
         FIELD TYPE AS cha
         FIELD tt-line AS INT
         FIELD tt-text AS cha 
         FIELD tt-recid AS RECID
         INDEX tt-text IS PRIMARY TYPE tt-line.

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

assign v-hdr = "VEND ITEM".
       
find first company where company.company eq cocode NO-LOCK. 
if avail company then
assign
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
 FOR EACH tt-text:
     DELETE tt-text.
 END.
 v-tot-sqft = 0.
    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id,
        first vend where vend.company eq po-ord.company 
                     and vend.vend-no eq po-ord.vend-no no-lock
        BREAK BY po-ord.vend-no BY PO-ORD.PO-NO:

        IF NOT CAN-FIND(FIRST po-ordl WHERE
           po-ordl.company EQ po-ord.company AND
           po-ordl.po-no EQ po-ord.po-no) THEN NEXT.        

        /*find first vend where vend.company eq po-ord.company 
                  and vend.vend-no eq po-ord.vend-no no-lock no-error.*/

        IF ip-multi-faxout AND FIRST-OF(po-ord.vend-no) THEN DO:
           OUTPUT CLOSE.
           OUTPUT STREAM st-fax CLOSE.
           OUTPUT TO value("c:\temp\fax\fx" + po-ord.vend-no + ".xpr") PAGE-SIZE value(ip-lines-per-page).
           OUTPUT STREAM st-fax TO value("c:\temp\fax\fx" + po-ord.vend-no + ".txt").
           PUT STREAM st-fax UNFORMATTED "FAX#:" STRING(vend.fax-area,"x(3)") + STRING(vend.fax,"xxxxxxx") SKIP.
           PUT CONTROL "<PRINT=NO>".       
           PUT UNFORMATTED "<EXPORT=c:\temp\fax\fx" trim(vend.vend-no) ".tif,BW>" .
           /*
           PUT "FAX#:" cust.fax SKIP.*/
        END.

      if po-ord.type eq "D" then
        assign v-sname     = po-ord.ship-name
               v-saddr[1]  = po-ord.ship-addr[1]
               v-saddr[2]  = po-ord.ship-addr[2]
               v-scity     = po-ord.ship-city
               v-sstate    = po-ord.ship-state
               v-szip      = IF length(po-ord.ship-zip) > 5 THEN string(po-ord.ship-zip,"xxxxx-xxxx")
                             ELSE po-ord.ship-zip.

      {po/exportpo.i}

      assign v-page-counter  = 1
             v-change-ord    = "".

      if po-ord.stat eq "N" then
        assign po-ord.stat = "O".
      else
      if po-ord.stat eq "U" then
        v-change-ord = "(CHANGED ORDER ONLY)".

      find first terms where terms.t-code eq po-ord.terms no-lock no-error.
      find first carrier where carrier.company eq po-ord.company 
                           and carrier.carrier eq po-ord.carrier no-lock no-error.

      if po-ord.type eq "R" then
        assign v-po-type = "Regular".
      else
        assign v-po-type = "Drop Ship".

      if po-ord.frt-pay eq "P" then
        assign v-freight-dscr = "Prepaid".
      else if po-ord.frt-pay eq "C" then
        assign v-freight-dscr = "Collect".
      else
        assign v-freight-dscr = "Bill".

/*FORM HEADER SKIP*/
      v-printline = 0.
      {po/po-valy.i}
        
      /*===get total page # per po#  =====*/
      lv-tot-pg = 1.
      ln-cnt = 0.
      for each po-ordl WHERE
           po-ordl.company EQ po-ord.company AND
           po-ordl.po-no EQ po-ord.po-no NO-LOCK BREAK BY po-ordl.i-no BY po-ordl.s-wid BY po-ordl.s-len
                       BY po-ordl.job-no BY po-ordl.job-no2:
        IF v-summarize-by-item AND /*LAST-OF(po-ordl.s-len)*/ last-of(po-ordl.job-no2) THEN DO:
            ln-cnt = ln-cnt + 5.          
            ASSIGN lv-text = ""
                   lv-text-line = 0
                   lv-text-line-length = 0
                   lv-char = ""
                   lv-char-list = "".
            FOR EACH bf-poordl WHERE
                bf-poordl.company EQ po-ord.company AND
                bf-poordl.po-no EQ po-ord.po-no AND
                bf-poordl.i-no = po-ordl.i-no
                    AND bf-poordl.s-wid = po-ordl.s-wid
                    AND bf-poordl.s-len = po-ordl.s-len 
                    AND bf-poordl.job-no = po-ordl.job-no
                    AND bf-poordl.job-no2 = po-ordl.job-no2
                   NO-LOCK :
                /*ln-cnt = ln-cnt + 5.          */
                FOR EACH notes WHERE notes.rec_key = bf-poordl.rec_key NO-LOCK:
                    lv-text = lv-text + notes.note_text + CHR(10).
                END.
            END.
            lv-text-line = 0.
            lv-text-line-length = 80.
            DO i = 1 TO LENGTH(lv-text):
               ASSIGN lv-char = SUBSTR(lv-text,i,1).
               IF lv-char EQ CHR(10) OR lv-char EQ CHR(13) THEN DO:
                  
               END.
               ELSE DO:
                    lv-char-list = lv-char-list + lv-char.
               END.

               IF  lv-char EQ CHR(10) OR lv-char EQ CHR(13) OR 
                   length(lv-char-list) >= lv-text-line-length THEN DO:
                   lv-text-line = lv-text-line + 1.
                   CREATE tt-text.
                   ASSIGN tt-text.TYPE = "LineNote"
                          tt-text.tt-line = lv-text-line
                          tt-text.tt-text = lv-char-list
                          tt-text.tt-recid = RECID(po-ordl)
                          lv-char-list = "".
               END.
            END.
              /* end of notes */
            ln-cnt = ln-cnt + lv-text-line.           

            IF v-print-sn THEN DO:
               IF po-ordl.item-type THEN
                  FIND FIRST ITEM WHERE ITEM.company EQ po-ord.company
                                    AND ITEM.i-no    EQ po-ordl.i-no NO-LOCK NO-ERROR.
               ELSE FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                                        AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
               lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                              ELSE IF AVAIL itemfg THEN itemfg.rec_key
                              ELSE "".
               ASSIGN lv-text = ""
                      lv-text-line = 0
                      lv-text-line-length = 0
                      lv-char = ""
                      lv-char-list = "".
               FOR EACH notes WHERE notes.rec_key = lv-item-rec AND 
                       /*notes.note_type = "S" */ notes.note_code = "PO" NO-LOCK:
                   lv-text = lv-text + notes.note_text + CHR(10).
               END.
               lv-text-line = 0.
               lv-text-line-length = 80.
               DO i = 1 TO LENGTH(lv-text):
                  ASSIGN lv-char = SUBSTR(lv-text,i,1).
                  IF lv-char EQ CHR(10) OR lv-char EQ CHR(13) THEN DO:

                  END.
                  ELSE DO:
                      lv-char-list = lv-char-list + lv-char.
                  END.
                  IF  lv-char EQ CHR(10) OR lv-char EQ CHR(13) OR 
                     length(lv-char-list) >= lv-text-line-length THEN DO:
                     lv-text-line = lv-text-line + 1.
                     CREATE tt-text.
                     ASSIGN tt-text.TYPE = "SPECNote"
                            tt-text.tt-line = lv-text-line
                            tt-text.tt-text = lv-char-list
                            tt-text.tt-recid = RECID(po-ordl)
                            lv-char-list = "".
                  END.
               END.
               ln-cnt = ln-cnt + lv-text-line.           
            END.  /* v-print-sn */
        END. /* last-of(po-ordl.job-no2)*/
        ELSE IF NOT v-summarize-by-item THEN DO:
            ln-cnt = ln-cnt + 4.          
            ASSIGN lv-text = ""
                   lv-text-line = 0
                   lv-text-line-length = 0
                   lv-char = ""
                   lv-char-list = "".
            FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
                lv-text = lv-text + notes.note_text + CHR(10).
            END.
            lv-text-line = 0.
            lv-text-line-length = 80.
            DO i = 1 TO LENGTH(lv-text):
               ASSIGN lv-char = SUBSTR(lv-text,i,1).
               IF lv-char EQ CHR(10) OR lv-char EQ CHR(13) THEN DO: END.
               ELSE DO:
                    lv-char-list = lv-char-list + lv-char.
               END.
               IF  lv-char EQ CHR(10) OR lv-char EQ CHR(13) OR 
                   length(lv-char-list) >= lv-text-line-length THEN DO:
                   lv-text-line = lv-text-line + 1.
                   CREATE tt-text.
                   ASSIGN tt-text.TYPE = "LineNote"
                          tt-text.tt-line = lv-text-line
                          tt-text.tt-text = lv-char-list
                          tt-text.tt-recid = RECID(po-ordl)
                          lv-char-list = "".
               END.
            END.
              /* end of notes */
            ln-cnt = ln-cnt + lv-text-line.           

            IF v-print-sn THEN DO:
               IF po-ordl.item-type THEN
                  FIND FIRST ITEM WHERE ITEM.company EQ po-ord.company
                                    AND ITEM.i-no    EQ po-ordl.i-no NO-LOCK NO-ERROR.
               ELSE FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                                        AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
               lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                              ELSE IF AVAIL itemfg THEN itemfg.rec_key
                              ELSE "".
               ASSIGN lv-text = ""
                      lv-text-line = 0
                      lv-text-line-length = 0
                      lv-char = ""
                      lv-char-list = "".
               FOR EACH notes WHERE notes.rec_key = lv-item-rec AND 
                       /*notes.note_type = "S" */ notes.note_code = "PO" NO-LOCK:
                   lv-text = lv-text + notes.note_text + CHR(10).
               END.
               lv-text-line = 0.
               lv-text-line-length = 80.
               DO i = 1 TO LENGTH(lv-text):
                  ASSIGN lv-char = SUBSTR(lv-text,i,1).
                  IF lv-char EQ CHR(10) OR lv-char EQ CHR(13) THEN DO:

                  END.
                  ELSE DO:
                      lv-char-list = lv-char-list + lv-char.
                  END.
                  IF  lv-char EQ CHR(10) OR lv-char EQ CHR(13) OR 
                     length(lv-char-list) >= lv-text-line-length THEN DO:
                     lv-text-line = lv-text-line + 1.
                     CREATE tt-text.
                     ASSIGN tt-text.TYPE = "SPECNote"
                            tt-text.tt-line = lv-text-line
                            tt-text.tt-text = lv-char-list
                            tt-text.tt-recid = RECID(po-ordl)
                            lv-char-list = "".
                  END.
               END.
               ln-cnt = ln-cnt + lv-text-line.           
            END.  /* v-print-sn */
        END.
      END. /* not summarized */
      /* header notes */
      ASSIGN lv-text = ""
             lv-text-line = 0
             lv-text-line-length = 0
             lv-char = ""
             lv-char-list = "".
      FOR EACH notes WHERE  notes.rec_key = po-ord.rec_key NO-LOCK:
          lv-text = lv-text + notes.note_text + CHR(10).
      END.
      lv-text-line = 0.
      lv-text-line-length = 80.
      DO i = 1 TO LENGTH(lv-text):
         ASSIGN lv-char = SUBSTR(lv-text,i,1).
         IF lv-char EQ CHR(10) OR lv-char EQ CHR(13) THEN DO: END.
         ELSE DO:
              lv-char-list = lv-char-list + lv-char.
         END.
         IF  lv-char EQ CHR(10) OR lv-char EQ CHR(13) OR 
             length(lv-char-list) >= lv-text-line-length THEN DO:
             lv-text-line = lv-text-line + 1.
             CREATE tt-text.
             ASSIGN tt-text.TYPE = "HeadNote"
                    tt-text.tt-line = lv-text-line
                    tt-text.tt-text = lv-char-list
                    tt-text.tt-recid = RECID(po-ord)
                    lv-char-list = "".
          END.
      END.
      ASSIGN
      ln-cnt = ln-cnt + lv-text-line
      lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0).  /* 23->47 25 po detail lines */
      /*  end of getting total page per po */
      for each po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no /*by po-ordl.line*/ 
          BREAK BY po-ordl.i-no BY po-ordl.s-wid BY po-ordl.s-len BY po-ordl.job-no BY po-ordl.job-no2:

        IF FIRST-OF(po-ordl.job-no2) THEN ASSIGN v-ord-qty = 0
                                                 v-t-cost = 0
                                                 v-po-cost = 0.

        assign xg-flag = no.
        if not v-printde-po and po-ordl.deleted then next.
        assign v-change-dscr = "".

        /*if po-ordl.stat eq "A" THEN ASSIGN v-change-dscr = "Added".
        else if po-ordl.stat eq "U" THEN assign v-change-dscr = "Updated".
        else if po-ordl.stat eq "O" THEN assign v-change-dscr = "Open".
        else if po-ordl.stat eq "P" then assign v-change-dscr = "Partial".
        else if po-ordl.stat eq "C" then assign v-change-dscr = "Closed".

        if po-ordl.deleted eq yes then   assign v-change-dscr = "Deleted".*/

        assign v-ino-job = po-ordl.vend-i-no.
        
        find item where item.company eq po-ordl.company
                    and item.i-no    eq po-ordl.i-no
                    and po-ordl.item-type
                  no-lock no-error.
        v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                        +
                      (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "").
        ASSIGN v-wid = po-ordl.s-wid
               v-len = po-ordl.s-len
               v-vend-item = po-ordl.vend-i-no.

        v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99").
        IF v-job-no = "-00" THEN v-job-no = "".
        v-adder = "".
        ASSIGN v-basis-w = 0
               v-dep     = 0.
        IF AVAIL ITEM THEN
           ASSIGN v-basis-w = item.basis-w
                  v-dep     = item.s-dep.

        IF po-ordl.pr-qty-uom EQ "MSF" THEN v-msf = po-ordl.ord-qty.
        ELSE
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                             v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                             po-ordl.ord-qty, OUTPUT v-msf).

        v-tot-sqft = v-tot-sqft + (v-msf * 1000).
        v-ord-qty = v-ord-qty + po-ordl.ord-qty . /* YSK: 05/17/06 
                                                     for conversion, what's a base uom
                                                     - Joe said do later*/

        IF v-printline > 43 THEN DO:         
           PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
           /*   PUT "<FBook Antiqua>"
                  "<R56><C1><From><R56><C35><LINE>"
                  "<R55><C1>X"
                  "<R56><C5>Authorized Signature"
                  "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
                  " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
                  " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
                  SKIP.     */
           PAGE.
           v-printline = 0.
           {po/po-valy.i}
        END.

        /*IF po-ordl.setup EQ 0 THEN*/ v-cost = po-ordl.cost.

        /*ELSE DO:
          v-qty = po-ordl.ord-qty.

          IF po-ordl.pr-qty-uom NE po-ordl.pr-uom THEN
            RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, po-ordl.pr-uom,
                                    v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                                    v-qty, OUTPUT v-qty).

          v-cost = po-ordl.cost * v-qty.

          v-cost = (v-cost - po-ordl.setup) /
                   (v-cost / po-ordl.cost).
        END.*/

        v-t-cost = v-t-cost + po-ordl.t-cost.
        v-po-cost = v-po-cost + v-cost.

        ASSIGN 
         v-wid   = po-ordl.s-wid - TRUNC(po-ordl.s-wid,0)
         v-wid   = (v-wid * 16) / 100
         v-wid   = TRUNC(po-ordl.s-wid,0) + v-wid
         v-len   = po-ordl.s-len - TRUNC(po-ordl.s-len,0)
         v-len   = (v-len * 16) / 100
         v-len   = TRUNC(po-ordl.s-len,0) + v-len
         v-adder = ""
         xg-flag = NO.

        release job-mat. 

        if avail item and item.mat-type eq "B" then do:
          find first job
              where job.company eq cocode
                and job.job-no  eq fill(" ",6 - length(trim(po-ordl.job-no))) +
                                   trim(po-ordl.job-no)
                and job.job-no2 eq po-ordl.job-no2
              no-lock no-error.
        
          if avail job then do:
            find first est
                where est.company eq job.company
                  and est.est-no  eq job.est-no
                no-lock no-error.
      
            for each job-mat
                where job-mat.company  eq cocode
                  and job-mat.job      eq job.job
                  and job-mat.job-no   eq job.job-no
                  and job-mat.job-no2  eq job.job-no2
                  and job-mat.i-no     eq po-ordl.i-no
                  and job-mat.frm      eq po-ordl.s-num
                use-index job no-lock
                break by job-mat.blank-no desc:
              if last(job-mat.blank-no)            or
                 job-mat.blank-no eq po-ordl.b-num then leave.
            end.

            if avail job-mat then do:
              find first ef
                  where ef.e-num   eq job.e-num
                    and ef.form-no eq job-mat.frm
                  no-lock no-error.
   
              assign
               xg-flag   = avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B")
               v-num-add = 0.
         
              for each xjob-mat
                  where xjob-mat.company  eq cocode
                    and xjob-mat.job      eq job-mat.job
                    and xjob-mat.job-no   eq job-mat.job-no
                    and xjob-mat.job-no2  eq job-mat.job-no2
                    and xjob-mat.frm      eq job-mat.frm
                    and xjob-mat.blank-no eq job-mat.blank-no
                    and xjob-mat.i-no     ne job-mat.i-no
                  no-lock,
              
                  first xitem
                  where xitem.company  eq cocode 
                    and xitem.i-no     eq xjob-mat.i-no
                    and xitem.mat-type eq "A"
                  no-lock:
                
                assign
                 v-num-add          = v-num-add + 1
                 v-adder[v-num-add] = xitem.i-no.
             
                if v-num-add ge 3 then leave.
              end.
            end.
          end.    
        end.
               
        run po/po-ordls.p (recid(po-ordl)).

        IF v-summarize-by-item AND /*LAST-OF(po-ordl.s-len)*/ LAST-OF(po-ordl.job-no2)  THEN DO:
           /*IF LAST-OF(po-ordl.i-no) THEN
              PUT v-LINE /*po-ordl.line*/ FORM ">>>"
               /*po-ordl.ord-qty*/ v-ord-qty FORM "->>>>,>>>,>>9" SPACE(3)
               po-ordl.pr-qty-uom SPACE(1)
               po-ordl.i-no FORM "x(30)" SPACE(1)
               /*v-adder[1] */
               v-job-no FORM "x(9)" AT 80 SPACE(1)
               po-ordl.cost FORM "->>>9.99<<"
               po-ordl.pr-uom
               v-t-cost /*po-ordl.t-cost*/ FORM "$>>>,>>9.99" SKIP.
           ELSE */
           
           vs-ord-qty = IF INDEX(STRING(v-ord-qty),".") > 0 THEN
                           SUBSTRING(STRING(v-ord-qty),1,INDEX(STRING(v-ord-qty),".") - 1) + "." +
                           SUBSTRING(STRING(v-ord-qty),INDEX(STRING(v-ord-qty),".") + 1)
                        ELSE STRING(v-ord-qty).
           vs-ord-qty = IF length(trim(vs-ord-qty)) < 13 THEN 
                        FILL(" ",13 - length(TRIM(vs-ord-qty))) + TRIM(vs-ord-qty)
               ELSE vs-ord-qty.
           PUT v-LINE /*po-ordl.line*/ FORM ">>>"
               /*v-ord-qty FORM "->>>>,>>>,>>9"*/
               vs-ord-qty FORM "x(13)" /*"->>>>>>,>>9.9<<"*/ SPACE(3) /* no qty display space(16) */                               
               po-ordl.pr-qty-uom SPACE(1)
               po-ordl.i-no FORM "x(30)" SPACE(1)
               v-adder[1]
               v-job-no FORM "x(9)" AT 79 SPACE(1)
               v-po-cost FORM "->>>9.99<<" SPACE(1)
               po-ordl.pr-uom
               v-t-cost /*po-ordl.t-cost*/ FORM "$>>>,>>9.99"  SKIP. 

           PUT po-ordl.i-name AT 25 SPACE(1) /*v-vend-item FORM "x(15)" space(1)*/
               v-adder[2] SPACE(1)
               "Setup:" AT 82 po-ordl.setup
               v-change-dscr AT 107  SKIP.
           v-printline = v-printline + 2.
           
           FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
     
           IF v-vend-item <> "" THEN DO:
              PUT trim(po-ord.vend) AT 25 "Item#: " v-vend-item SKIP.
              v-printline = v-printline + 1.
           END.
           /* check whether i-no is roll */
           FIND FIRST job-hdr WHERE job-hdr.company = po-ordl.company
                                AND job-hdr.job-no = po-ordl.job-no
                                AND job-hdr.job-no2 = po-ordl.job-no2 NO-LOCK NO-ERROR.
           IF AVAIL job-hdr THEN 
              FIND FIRST ef WHERE ef.company = po-ordl.company
                              AND ef.est-no = job-hdr.est-no
                              AND ef.form-no = po-ordl.s-num NO-LOCK NO-ERROR.
    
           RUN calc-cost (recid(po-ordl),OUTPUT v-cost,OUTPUT v-setup). 

           IF AVAIL ITEM AND ITEM.mat-type = "C" THEN
              put "W: " at 25 v-wid space(2) "L: " v-len SPACE(2) "D: "  ITEM.case-d FORM ">>>9.99<<" .
           ELSE DO:
              IF (AVAIL ITEM AND ITEM.r-wid > 0) or
                   (AVAIL ef AND ef.roll) THEN  /* no length for Roll*/
                   put "W: " AT 25 v-wid SPACE(22).
              ELSE put "W: " AT 25 v-wid SPACE(2) "L: " v-len SPACE(11).

              PUT v-adder[3].
           END.

           PUT " MSF: " + TRIM(STRING(v-msf,">>9.999")) FORMAT "x(13)".
            
           {po/poprints.i}

               v-printline = v-printline + 1.

               if not v-test-scr then
                 put skip
                     space(24)
                     "Score: "
                     len-score format "x(80)".
          
               else
               if dec(trim(len-score)) ne v-wid then
                 put skip
                     space(24)
                     "Score: "
                     len-score format "x(80)".

               ELSE v-printline = v-printline - 1.
             end.
             END. 
           end.
            
            assign v-printline = v-printline + 1.
            put skip(1).
            assign v-printline = v-printline + 1.
            /* calc total sq feet */
        /*= notes
            ASSIGN v-inst = ""
                   v-tmp-lines = 0
                   j = 0
                   K = 0
                   lv-got-return = 0
                   .
   
            FOR EACH bf-poordl OF po-ord NO-LOCK WHERE bf-poordl.i-no = po-ordl.i-no
                    AND bf-poordl.s-wid = po-ordl.s-wid
                    AND bf-poordl.s-len = po-ordl.s-len ,
                EACH notes WHERE notes.rec_key = bf-poordl.rec_key NO-LOCK BREAK BY notes.rec_key:

                DO i = 1 TO LENGTH(notes.note_text) :        
                   IF i - j >= 82 THEN ASSIGN j = i
                                      lv-got-return = lv-got-return + 1.
                  
                   v-tmp-lines = ( i - j ) / 82.
                   {SYS/INC/ROUNDUP.I v-tmp-lines}
                   k = v-tmp-lines + lv-got-return .

                   IF k < 13 THEN v-inst[k] = v-inst[k] +
                                     IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1)
                                     ELSE "" .              
           
                   IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
                   THEN do:
                      lv-got-return = lv-got-return + 1.
                      j = i.        
                   END.
                END.
                IF last-of(notes.rec_key) THEN ASSIGN lv-got-return = lv-got-return + 1
                                                      .
            END.
            v-printline = v-printline + /* IF k < 5 THEN 4 ELSE*/  k.
        */
            
            IF v-printline > 47 THEN DO:         
               PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
               PUT "<FBook Antiqua>"
               /*    "<R56><C1><From><R56><C35><LINE>"
                  "<R55><C1>X"
                  "<R56><C5>Authorized Signature" */
                  "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
                  " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
                  " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
                  SKIP.    
               PAGE.
               v-printline = 0.
               {po/po-valy.i}
            END.
            FOR EACH tt-text WHERE tt-text.TYPE = "Linenote" AND tt-text.tt-recid = recid(po-ordl) BY tt-text.tt-line:
                
                IF v-printline > 47 THEN DO:         
                   PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                   PUT "<FBook Antiqua>"
                   /*    "<R56><C1><From><R56><C35><LINE>"
                       "<R55><C1>X"
                       "<R56><C5>Authorized Signature" */
                       "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
                       " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
                       " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
                       SKIP.    
                    PAGE.
                    v-printline = 0.
                    {po/po-valy.i}
                END.
                PUT tt-text.tt-text FORM "x(80)"   SKIP.
                v-printline = v-printline + 1.
            END.
            /* === spec note print */
            IF v-print-sn THEN DO:
                ASSIGN v-tmp-lines = 0
                       v-inst-lines = 0
                       lv-item-rec = "".
              
               FOR EACH tt-text WHERE tt-text.TYPE = "specnote" AND tt-text.tt-recid = recid(po-ordl) BY tt-text.tt-line:
                  IF v-printline > 47 THEN DO:         
                     PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                     PUT "<FBook Antiqua>"
                     /*    "<R56><C1><From><R56><C35><LINE>"
                       "<R55><C1>X"
                       "<R56><C5>Authorized Signature" */
                        "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
                        " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
                        " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
                        SKIP.    
                      PAGE.
                      v-printline = 0.
                      {po/po-valy.i}
                  END.
                  PUT tt-text.tt-text FORM "x(80)"  SKIP.
                  v-printline = v-printline + 1.
               END.
             END.
             /* === end of specnote print */
        END. /*last-of(po-ordl.s-len) po-ordl.job-no2*/
        ELSE IF NOT v-summarize-by-item THEN DO:  
            v-line = po-ordl.LINE.
            {po/po-valy2.i}
        END.
  end. /* for each po-ordl record */
  
       IF v-printline > 47 THEN DO:                  
          PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PUT "<FBook Antiqua>"
              /*    "<R56><C1><From><R56><C35><LINE>"
                  "<R55><C1>X"
                  "<R56><C5>Authorized Signature" */
                  "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
                  " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
                  " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
                  SKIP.     
          PAGE.
          v-printline = 0.
          {po/po-valy.i}
       END.
  
  v-inst = "".
/*  FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
                v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                   /*PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                   v-printline = v-printline + 1.                                           */
                   IF i < 5  THEN  /* display upto 4 lines */
                       ASSIGN v-inst[i] =  substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80)
                          /*    v-printline = v-printline + 1*/ .
                   ELSE LEAVE.
                END.

   end.
*/ i = 0.
   FOR EACH tt-text WHERE tt-text.TYPE = "headcnote" AND tt-text.tt-recid = recid(po-ord) BY tt-text.tt-line:
       i = i + 1.
       IF i < 5 THEN ASSIGN v-inst[i] = tt-text.tt-text
                            v-printline = v-printline + 1.
   END.             
       IF v-printline > 47 THEN DO:                  
           PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
           PUT "<FBook Antiqua>"
                  "<R56><C1><From><R56><C35><LINE>"
                  "<R55><C1>X"
                  "<R56><C5>Authorized Signature" .
          PAGE.
          v-printline = 0.
          {po/po-valy.i}

       END.   

      v-tot-sqft = 0.
      v-bot-lab[1] = "TAX        :"
                     /*vend.tax-gr + "        :       " */ + STRING(po-ord.tax,"$->>>,>>9.99").

      PUT /*"<R53><C1>" */ v-inst[1] 
          /*"<R54><C1>" */ v-inst[2] 
          /*"<R55><C1>"*/  v-inst[3] 
          /*"<R56><C1>" */ v-inst[4] 
          "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
    "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "$->>>,>>9.99"
    "<=8><R+2> "  v-bot-lab[1] 
    "<=8><R+3> "  " " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
                /*v-bot-lab[2] */
    "<=8><R+4> Grand Total:" po-ord.t-cost FORM "$->>>,>>9.99" 
    /*"<=8><R+5><C+10>Page " string(PAGE-NUM,">>9") + " of <#Pages>"  FORM "x(20)"   */
    "<=8><R+5><C+10>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .

PUT "<FBook Antiqua>"
    "<R56><C1><From><R56><C35><LINE>"
    "<R55><C1>X"
    "<R56><C5>Authorized Signature"
    "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
     " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
     " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
     SKIP.     
     
IF last-of(po-ord.po-no) THEN lv-pg-num = PAGE-NUM.
v-printline = v-printline + 6.
IF v-printline <= page-size THEN PUT SKIP(74 - v-printline).
/*
IF v-printline <= page-size THEN do:     
   page {1}. /*PUT SKIP(74 - v-printline). */
   v-printline = 0.
   {po/po-valy.i}
END.
*/
end. /* for each po-ord record */.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */


PROCEDURE calc-cost:

 DEF INPUT PARAM ip-recid AS recid NO-UNDO.
 DEF OUTPUT PARAM op-cost AS DEC NO-UNDO.
 DEF OUTPUT PARAM op-setup AS DEC NO-UNDO.
 DEF VAR vv-qty  AS DEC NO-UNDO.
 DEF VAR vv-cost AS DEC NO-UNDO.
 
 DEF VAR vv-setup AS dec NO-UNDO.
 DEF VAR li AS INT NO-UNDO.

 DEF VAR vv-basis-w AS DEC NO-UNDO.
 /*DEF VAR v-len AS DEC NO-UNDO.
 DEF VAR v-wid AS DEC NO-UNDO. */
 DEF VAR vv-dep AS DEC NO-UNDO.
 
 DEF VAR v-ord-qty AS dec NO-UNDO.

 DEF BUFFER b-po-ordl FOR po-ordl.
 DEF BUFFER b-po-ord FOR po-ord.
 
 FIND b-po-ordl WHERE RECID(b-po-ordl) = ip-recid NO-LOCK .
 FIND FIRST b-po-ord WHERE
      b-po-ord.company EQ b-po-ordl.company AND
      b-po-ord.po-no   EQ b-po-ordl.po-no
      NO-LOCK.

   ASSIGN
    v-ord-qty = (b-po-ordl.ord-qty).
    
   FIND FIRST e-item
       WHERE e-item.company EQ cocode
         AND e-item.i-no    EQ b-po-ordl.i-no
       NO-LOCK NO-ERROR.

   IF AVAIL e-item THEN
   FIND FIRST e-item-vend OF e-item
       WHERE e-item-vend.vend-no EQ b-po-ord.vend-no
       NO-LOCK NO-ERROR.

   IF AVAIL e-item-vend THEN DO:
     FIND FIRST ITEM
         WHERE ITEM.company EQ cocode
           AND ITEM.i-no    EQ b-po-ordl.i-no
         NO-LOCK NO-ERROR.

     ASSIGN
      vv-basis-w = IF AVAIL ITEM THEN ITEM.basis-w ELSE vv-basis-w
      vv-dep     = IF AVAIL ITEM THEN ITEM.s-dep ELSE vv-dep
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

  ELSE
    ASSIGN
     vv-cost  = b-po-ordl.cost
     vv-setup = b-po-ordl.setup.

  ASSIGN
   op-cost  = vv-cost
   op-setup = vv-setup.

END PROCEDURE.

