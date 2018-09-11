/* -------------------------------------------------------------------------- */ 
/* po/po-ppi.p                                                                */
/* Purchase Order Print Program for Preferred                                 */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}


{po/po-print.i}
DEF SHARED VAR s-group-notes AS LOG NO-UNDO.

DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.    
DEF VAR v-basis-w AS DEC NO-UNDO.
DEF VAR v-len AS DEC FORM "->,>>9.99" NO-UNDO.
DEF var v-wid2 like po-ordl.s-wid format ">>9.99999" no-undo. /* for recalc extened cost */
def var v-len2 like po-ordl.s-len format ">>9.99999" no-undo. /* for recalc extened cost */
def var pol-counter as int no-undo.
def var save_id as recid.
def var time_stamp as char.
def var v-exp-limit as int no-undo init 10.
def var v-line-number as int.
def var v-page-counter as int format ">>9".
def var v-lines-to-skip as int.
def var v-sname like shipto.ship-name.
def var v-saddr like shipto.ship-addr.
def var v-scity like shipto.ship-city.
def var v-sstate like shipto.ship-state.
def var v-szip like shipto.ship-zip.
def var v-po-type as char format "x(10)".
def var v-freight-dscr as char format "x(7)".
def var v-change-dscr as char format "x(7)".
def var v-dash-line as char format "x(80)" extent 3.
def var v-dec-fld as decimal no-undo.
def var v-adders as log.
def var xg-flag as log init no no-undo.
def var v-space as log init yes.
def var len-score as char.
def buffer xjob-mat for job-mat.
def buffer xitem for item.
def var same-score as ch no-undo.
def var v-test-scr as log no-undo.
def var v-hdr as char format "x(15)" initial "" no-undo.
def var v-ino-job as char format "x(15)" initial "" no-undo.
def var v-change-ord as char format "x(35)" initial "" no-undo.
DEF VAR lv-got-return AS int NO-UNDO.
DEF VAR lv-dep AS DEC NO-UNDO.
DEF VAR lv-dep2 AS DEC NO-UNDO.
DEF VAR lv-print-wid AS DEC NO-UNDO.
DEF VAR lv-print-len AS DEC NO-UNDO.
DEF VAR lv-print-dep AS DEC NO-UNDO.

DEFINE VARIABLE lv-Format AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lv-Ord-Qty LIKE po-ordl.ord-qty NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

ASSIGN ls-image1 = "images\ppitop.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.

DEF VAR v-qty LIKE po-ordl.ord-qty NO-UNDO.
/*DEF VAR v-tot-sqft AS DEC NO-UNDO.*/
DEF VAR v-vend-item AS cha NO-UNDO.
def var v-num-add as int initial 0 no-undo.
DEF VAR v-job-no AS cha NO-UNDO.
/*DEF VAR v-cost AS DEC NO-UNDO.*/
DEF VAR v-tmp-lines AS dec NO-UNDO.
DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(40)" NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-flute LIKE ITEM.flute NO-UNDO.
DEF VAR lv-reg-no LIKE ITEM.reg-no NO-UNDO.
DEF VAR lv-item-rec AS cha NO-UNDO.
{custom/formtext.i NEW}
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF var v-dept-note AS cha FORM "x(80)" EXTENT 50 NO-UNDO.
DEF VAR lv-text-line AS INT NO-UNDO.
DEF VAR lv-text-line-length AS INT NO-UNDO.
DEF VAR lv-char AS cha NO-UNDO.
DEF VAR lv-char-list AS cha NO-UNDO.
DEF VAR v-sig-image AS CHAR NO-UNDO.
DEF VAR v-signature AS CHAR NO-UNDO.
DEF TEMP-TABLE tt-text NO-UNDO
         FIELD TYPE AS cha
         FIELD tt-line AS INT
         FIELD tt-text AS cha 
         FIELD tt-recid AS RECID
         INDEX tt-text IS PRIMARY TYPE tt-line.

v-dash-line = fill ("_",80).

{po/po-print.f}

assign v-hdr = "VEND ITEM".
       
find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "POPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.
v-dec-fld = sys-ctrl.dec-fld.
FIND first sys-ctrl where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".


FUNCTION FNmetric RETURNS DEC (INPUT aa AS DECIMAL, input bb as decimal).
 IF v-metric = TRUE THEN 
    RETURN ROUND(bb * 25.4,0).
 ELSE   
   RETURN aa.
END FUNCTION.

find first company where company.company eq cocode NO-LOCK. 
 
 /*v-tot-sqft = 0.*/
    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
        BREAK BY PO-ORD.PO-NO:

/*      if po-ord.type eq "D" then  */
        assign v-sname     = po-ord.ship-name
               v-saddr[1]  = po-ord.ship-addr[1]
               v-saddr[2]  = po-ord.ship-addr[2]
               v-scity     = po-ord.ship-city
               v-sstate    = po-ord.ship-state
               v-szip      = po-ord.ship-zip.

      {po/exportpo.i}

      assign v-page-counter  = 1
             v-change-ord    = "".

      if po-ord.stat eq "N" then
        assign po-ord.stat = "O".
      else
      if po-ord.stat eq "U" then
        v-change-ord = "(CHANGED ORDER ONLY)".

      find first vend where vend.company eq po-ord.company 
                        and vend.vend-no eq po-ord.vend-no no-lock no-error.
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

v-printline = 0.
{po/po-ppi.i}

      for each po-ordl WHERE 
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no by po-ordl.line:
        assign xg-flag = no.
        if not v-printde-po and po-ordl.deleted then next.
        assign v-change-dscr = "".

        if po-ordl.stat eq "A" THEN ASSIGN v-change-dscr = "Added".
        else if po-ordl.stat eq "U" THEN assign v-change-dscr = "Updated".
        else if po-ordl.stat eq "O" THEN assign v-change-dscr = "Open".
        else if po-ordl.stat eq "P" then assign v-change-dscr = "Partial".
        else if po-ordl.stat eq "C" then assign v-change-dscr = "Closed".

        if po-ordl.deleted eq yes then   assign v-change-dscr = "Deleted".

        assign v-ino-job = po-ordl.vend-i-no.
        /*V-ADDER = "".*/
        v-vend-item = "".

        find item where item.company eq po-ordl.company
                    and item.i-no    eq po-ordl.i-no
                    and po-ordl.item-type
                  no-lock no-error.
        v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                        +
                      (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "").


        ASSIGN v-wid = po-ordl.s-wid
               v-len = po-ordl.s-len
               lv-dep = IF po-ordl.s-dep GT 0 THEN po-ordl.s-dep
                        ELSE IF AVAIL ITEM AND ITEM.mat-type = "C" THEN item.case-d
                        ELSE IF AVAIL ITEM THEN ITEM.s-dep
                        ELSE 0
               v-wid2 = po-ordl.s-wid
               v-len2 = po-ordl.s-len
               lv-dep2 = lv-dep.
        

        if avail item and item.mat-type eq "B" then do:
          if v-shtsiz then do:
           if v-dec-fld = 0.08 then
            assign v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
                   v-wid = ( v-wid * 16 ) / 100
                   v-wid = truncate(po-ordl.s-wid,0) + v-wid
                   v-len = po-ordl.s-len - truncate(po-ordl.s-len,0)
                   v-len = ( v-len * 16 ) / 100
                   v-len = truncate(po-ordl.s-len,0) + v-len
                   lv-dep = lv-dep2 - truncate(lv-dep2,0)
                   lv-dep = ( lv-dep * 16 ) / 100
                   lv-dep = TRUNCATE(lv-dep2,0) + lv-dep.
           else
              assign v-wid = po-ordl.s-wid 
                    v-len = po-ordl.s-len.
            
            assign v-num-add = 0.

            find first job where job.company eq cocode 
                             and job.job-no eq string(fill(" ",6 - length(
                                                trim(po-ordl.job-no)))) +
                                                trim(po-ordl.job-no) 
                             and job.job-no2 eq po-ordl.job-no2
                           no-lock no-error.
            if avail job then
            do:
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

              if avail job-mat then
              do:
                /*
                /* Adder i-no and i-name to po of exist */
                for each xjob-mat where xjob-mat.company  eq cocode
                                    and xjob-mat.job      eq job-mat.job
                                    and xjob-mat.job-no   eq job-mat.job-no
                                    and xjob-mat.job-no2  eq job-mat.job-no2
                                    and xjob-mat.frm      eq job-mat.frm
                                    and xjob-mat.blank-no eq job-mat.blank-no
                                    and xjob-mat.i-no     ne job-mat.i-no
                                  no-lock:
                  find first xitem where xitem.company        eq cocode
                                     and xitem.i-no      eq xjob-mat.i-no
                                     and xitem.mat-type  eq "A" no-lock no-error.
                  if avail xitem then
                  do:
                      /*
                    put xitem.i-no at 25 xitem.i-name at 38.
                    assign v-line-number = v-line-number + 1.
                    */
                      assign v-num-add = v-num-add + 1.
                      if v-num-add eq 1 THEN assign v-adder[1] = xitem.i-name.
                      else if v-num-add eq 2 THEN assign v-adder[2] = xitem.i-name.
                      else if v-num-add eq 3 THEN assign v-adder[3] = xitem.i-name.
                      else if v-num-add eq 4 THEN assign v-adder[4] = xitem.i-name.
                      else if v-num-add eq 5 THEN assign v-adder[5] = xitem.i-name.
                  end.

                end.
                */
                find first ef where EF.COMPANY EQ JOB.COMPANY
                                AND ef.est-no  EQ job.est-no
                                and ef.form-no eq job-mat.frm
                              no-lock no-error.
                if avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B") THEN ASSIGN xg-flag = yes.
              end. /* avail job-mat */
            end. /* avail job */
          end. /* v-shtsiz */        
          /*
          IF AVAIL ITEM AND ITEM.industry = "2" THEN DO:
             assign v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0).
             assign v-wid = ( v-wid * 16 ) / 100.
             assign v-wid = truncate(po-ordl.s-wid,0) + v-wid.
             assign v-len = po-ordl.s-len - truncate(po-ordl.s-len,0).
             assign v-len = ( v-len * 16 ) / 100.
             assign v-len = truncate(po-ordl.s-len,0) + v-len.
          END.
          */
        end. /* avail item and item.mat-type eq "B" */
       /* v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,">>").*/
        v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99") +
                   "-" + string(po-ordl.s-num,"99").

        IF po-ordl.job-no = "" THEN v-job-no = "".

        IF v-job-no = "-" THEN v-job-no = "".
       
        IF v-printline + 4 > 46 THEN DO:

           PAGE.
           v-printline = 0.
           {po/po-ppi.i}
        END.

        lv-ord-qty = po-ordl.ord-qty.
        lv-format = IF CAN-DO("MSF,TON", po-ordl.pr-qty-uom) 
                      THEN "->>,>>>,>>9.99"
                      ELSE
                      IF CAN-DO("LF,EA", po-ordl.pr-qty-uom)
                      THEN "->>>,>>>,>>9"
                      ELSE "->>>,>>>,>>9.9<<<<<".

        IF po-ordl.pr-qty-uom EQ "LF" THEN DO:
          {sys/inc/roundup.i lv-ord-qty}
        END.

        PUT po-ordl.LINE FORM ">>9"
            STRING(lv-ord-qty, lv-format) FORMAT "x(14)" SPACE(2)
         /*  po-ordl.ord-qty  01/25/07 01250709 */
            po-ordl.pr-qty-uom SPACE(1)
            po-ordl.i-no FORM "x(20)" SPACE(16)
            /*v-adder[1] */
            v-job-no FORM "x(12)" SPACE(1)
            po-ordl.cost FORM "->>>9.99<<"
            po-ordl.pr-uom
            po-ordl.t-cost FORM "->>,>>9.99"              
            SKIP.

        v-printline = v-printline + 1.

        PUT int(po-ordl.over-pct) FORM ">>9" AT 7 " / "
            int(po-ordl.under-pct) FORM ">>9" 
            po-ordl.i-name AT 25 FORM "x(30)" SPACE(16) /*v-vend-item FORM "x(15)" space(1) */
            /*v-adder[2]*/ SPACE(12)
            v-change-dscr  SKIP.
        ASSIGN v-printline = v-printline + 1
               v-line-number = v-line-number + 3.
        
        if po-ordl.dscr[1] ne "" /*OR v-adder[3] <> ""*/ then do:
          put po-ordl.dscr[1] format "x(30)"  at 25 " "             
              /*v-adder[3] */ SPACE(15)
              skip.
          ASSIGN
             v-line-number = v-line-number + 1
             v-printline = v-printline + 1.
        end.
        
    
        if po-ordl.dscr[2] ne "" /*OR v-adder[4] <> ""*/ then do:
          put po-ordl.dscr[2] format "x(30)" at 25              
              /*" " v-adder[4]*/ skip.
          ASSIGN
             v-line-number = v-line-number + 1
             v-printline = v-printline + 1.
        end.
        
        IF /*v-adder[5] <> "" OR*/ v-vend-item <> "" THEN DO:
            put v-vend-item  FORM "x(30)" AT 25              
                /*" " v-adder[5]*/ skip.
            ASSIGN
               v-line-number = v-line-number + 1
               v-printline = v-printline + 1.
        END.
        /* calc total sq feet */
    
        IF v-itemDescription AND NOT po-ordl.item-type THEN /* fg item */ DO:
          FIND FIRST itemfg NO-LOCK
               WHERE itemfg.company = po-ordl.company
                 AND itemfg.i-no = po-ordl.i-no NO-ERROR.
          IF AVAILABLE itemfg THEN DO:
            IF itemfg.part-dscr3 NE '' THEN DO:
              PUT itemfg.part-dscr3 AT 25.
              ASSIGN
                v-line-number = v-line-number + 1
                v-printline = v-printline + 1.
            END. /* if part-dscr3 */
          END. /* avail itemfg */
        END. /* if v-itemdescription */
        
        ASSIGN v-basis-w = 0
               v-dep     = 0.

        RELEASE ITEM.

        IF po-ordl.item-type THEN
           FIND FIRST ITEM WHERE ITEM.company EQ po-ord.company
                            AND ITEM.i-no    EQ po-ordl.i-no
                     NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
           ASSIGN v-basis-w = item.basis-w
                  v-dep     = ITEM.s-dep.

        IF po-ordl.pr-qty-uom EQ "MSF" THEN v-qty = po-ordl.ord-qty.
        ELSE RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                             v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                             po-ordl.ord-qty, OUTPUT v-qty).
        /*ASSIGN
        v-tot-sqft = v-tot-sqft + (v-qty * 1000)
        v-cost = po-ordl.cost.*/

        IF AVAIL ITEM AND ITEM.mat-type EQ "B" AND ITEM.industry EQ "2" THEN
          ASSIGN lv-flute = "  Flute: " + ITEM.flute
                 lv-reg-no = "Test: " + ITEM.reg-no.
        ELSE
          ASSIGN lv-flute = ""
                 lv-reg-no = "".

        ASSIGN
           lv-print-wid = FNmetric(v-wid, v-wid2)
           lv-print-len = FNmetric(v-len, v-len2)
           lv-print-dep = FNmetric(lv-dep, lv-dep2).

        IF v-wid GT 0 THEN
           DO:
             IF NOT v-metric THEN
                PUT  "W: " at 25 lv-print-wid FORM ">>9.99999" space(2).
             ELSE
                PUT  "W: " at 25 lv-print-wid FORM ">>9" space(2).
           END.
        IF v-len GT 0 THEN
           DO:
              IF NOT v-metric THEN
                 PUT "L: " lv-print-len FORM ">>9.99999" SPACE(2).
              ELSE
                 PUT "L: " lv-print-len FORM ">>9" SPACE(2).
           END.
        IF lv-dep GT 0 THEN
           DO:
              IF NOT v-metric THEN
                 PUT "D: "  lv-print-dep FORM ">>9.99999" SPACE(9).
              ELSE
                 PUT "D: "  lv-print-dep FORM ">>9" SPACE(9).
           END.

        IF AVAIL ITEM AND ITEM.mat-type EQ "B" AND ITEM.industry = "2" THEN
           PUT lv-flute FORM "x(13)" /*"Test:" */ lv-reg-no FORM "x(10)".

        assign v-line-number = v-line-number + 1
               v-printline = v-printline + 1.

         len-score = "".   
         run po/po-ordls.p (recid(po-ordl)).
         {po/poprints.i}       
            
           end.
           END.
         end.
        
         FOR EACH tt-formtext:
            DELETE tt-formtext.
         END.
         lv-text = "".
         FOR EACH notes NO-LOCK WHERE notes.rec_key = po-ordl.rec_key :
            lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
         END.
         DO li = 1 TO 20:
           CREATE tt-formtext.
           ASSIGN tt-line-no = li
                  tt-length  = 80.
         END.
         RUN custom/formtext.p (lv-text).
         i = 0.
         v-dept-note = "".
         FOR EACH tt-formtext:
            i = i + 1.
            IF  i <= 20 THEN v-dept-note[i] = tt-formtext.tt-text.      
         END.
         li = 0.
         DO i = 20 TO 1 BY -1:
           li = i.
           IF v-dept-note[i] <> "" THEN LEAVE.
         END.
         IF s-group-notes AND v-printline + li > 46 THEN DO:

           PAGE.
           v-printline = 0.
           {po/po-ppi.i}
         END.

         DO i = 1 TO li:

            IF i = 1 THEN
               PUT SKIP.

            PUT v-dept-note[i] SKIP.
            v-printline = v-printline + 1.
            IF v-printline > 46 THEN DO:
               PAGE.
               v-printline = 0.
               {po/po-ppi.i}
            END.
         END.

     PUT skip(1).
     assign v-line-number = v-line-number + 1.
     v-printline = v-printline + 1.
  
     IF v-printline > 46 THEN DO:
          PAGE.
          v-printline = 0.
          {po/po-ppi.i}
     END.
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
           IF lv-char EQ CHR(10) OR lv-char EQ CHR(13) THEN DO: END.
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
        FOR EACH tt-text WHERE tt-text.TYPE = "specnote" AND tt-text.tt-recid = recid(po-ordl) BY tt-text.tt-line:
            IF v-printline > 46 THEN DO:
              PAGE.
              v-printline = 0.
              {po/po-ppi.i}
            END.     
            PUT tt-text.tt-text FORM "x(80)"  SKIP.
                v-printline = v-printline + 1.
        END.
     END.  /* v-print-sn */

  end. /* for each po-ordl record */

  ASSIGN v-inst = ""
         v-tmp-lines = 0
         j = 0
         K = 0
         lv-got-return = 0.
  
FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
          
    DO i = 1 TO LENGTH(notes.note_text) :        
       IF i - j >= 80 THEN ASSIGN j = i
                                  lv-got-return = lv-got-return + 1.
             
       v-tmp-lines = ( i - j ) / 80.
       {SYS/INC/ROUNDUP.I v-tmp-lines}
       k = v-tmp-lines + lv-got-return.

       IF k < 7 THEN v-inst[k] = v-inst[k] + 
            IF SUBSTRING(notes.note_text,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                           THEN SUBSTRING(notes.note_text,i,1)
                           ELSE "".

       IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
       THEN do:
           lv-got-return = lv-got-return + 1.
           j = i.
       END.

     END.
  end.

  IF v-printline > 46 THEN DO:

     PAGE.
     v-printline = 0.
     {po/po-ppi.i}
  END.

  /*PUT "Grand Total MSF: " +
      TRIM(STRING(v-tot-sqft / 1000,">>>,>>9.9<<")) AT 50 FORMAT "x(30)"
      SKIP. */

  /*v-tot-sqft = 0.*/
  v-bot-lab[1] = "Tax        :" + STRING(po-ord.tax,"->>,>>9.99").

  PUT "<R48><C1>" v-inst[1] 
      "<R49><C1>" v-inst[2]
      "<R50><C1>" v-inst[3]
      "<R51><C1>" v-inst[4]
      "<R52><C1>" v-inst[5]
      "<R53><C1>" v-inst[6]
      "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
      "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "->>,>>9.99"
      "<=8><R+2> "  v-bot-lab[1] 
      "<=8><R+3> "  " "
      "<=8><R+4> Grand Total:" po-ord.t-cost FORM "->>,>>9.99" .

   v-sig-image = "signature\" + po-ord.buyer + ".jpg".
   FILE-INFO:FILE-NAME = v-sig-image.
   IF SEARCH(FILE-INFO:FULL-PATHNAME) = ? THEN DO:
      v-sig-image = "signature\" + po-ord.buyer + ".bmp".     
      FILE-INFO:FILE-NAME = v-sig-image.
   END.
   v-sig-image = IF FILE-INFO:FULL-PATHNAME NE ? THEN FILE-INFO:FULL-PATHNAME + ">" ELSE "".

   v-signature = IF v-sig-image NE "" THEN
                    "<C15><#2><R54><C+40><Image#2=" + v-sig-image        
                 ELSE "".

   PUT v-signature FORM "x(100)" SKIP.

   PUT "<FArial><R58><C3>Authorized Signature" SKIP
       "<R60><C1><P12><B>Terms and Conditions </B><P9>" SKIP
       "<C1>Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
       "<C1>INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP.

   v-printline = v-printline + 8.

   IF v-printline < 60 THEN PUT SKIP(80 - v-printline).

    end. /* for each po-ord record */.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */

