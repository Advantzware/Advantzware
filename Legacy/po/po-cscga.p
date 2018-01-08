/* --------------------------------------------- po/po-cscga.p  */
/* Purchase Order XPrint Program for N-K-POPRINT = CSC-GA                     */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER ip-multi-faxout AS LOG NO-UNDO. /* fax multiple recipents or single */
DEF INPUT PARAM  ip-lines-per-page AS INT NO-UNDO.
DEF STREAM st-fax.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-ref1  for reftable.
def buffer b-ref2  for reftable.

{po/po-print.i}
{custom/notesdef.i}
{custom/formtext.i NEW}
DEF VAR v-tmp-note-length AS INT NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
def var v-wid like po-ordl.s-wid format ">>9.99" no-undo.
def var v-len like po-ordl.s-len format ">>9.99" no-undo.
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

DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(60)" NO-UNDO.
ASSIGN ls-image1 = "images\cscga.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".



DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20.

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
DEF VAR lv-item-rec AS cha NO-UNDO.
DEF VAR lv-over-under AS CHAR FORMAT "X(15)" NO-UNDO.

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
 v-comp-add1 = company.addr[1]
 v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
 v-comp-add3 = "Phone: 604.533.2545" 
 v-comp-add4 = "Fax  : 604.533.2633"
 v-tot-sqft = 0.

    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
        BREAK BY po-ord.vend-no BY PO-ORD.PO-NO:

        IF NOT CAN-FIND(FIRST po-ordl WHERE
           po-ordl.company EQ  po-ord.company AND
           po-ordl.po-no EQ po-ord.po-no) THEN NEXT.
        find first vend where vend.company eq po-ord.company 
                  and vend.vend-no eq po-ord.vend-no no-lock no-error.

        IF ip-multi-faxout AND FIRST-OF(po-ord.vend-no) THEN DO:
           OUTPUT CLOSE.
           OUTPUT STREAM st-fax CLOSE.
           OUTPUT TO value("c:\temp\fax\fx" + po-ord.vend-no + ".xpr") PAGE-SIZE value(ip-lines-per-page).
           OUTPUT STREAM st-fax TO value("c:\temp\fax\fx" + po-ord.vend-no + ".txt").
           PUT STREAM st-fax UNFORMATTED "FAX#:" trim(string(vend.fax-prefix)) + STRING(vend.fax-area,"x(3)") + STRING(vend.fax,"xxxxxxx") SKIP.
           PUT CONTROL "<PRINT=NO>".       
           PUT UNFORMATTED "<EXPORT=c:\temp\fax\fx" trim(vend.vend-no) ".tif,BW>" .
           /*
           PUT "FAX#:" cust.fax SKIP.*/
        END.

      if avail company then
        assign
         v-sname     = company.name
         v-saddr [1] = company.addr [1]
         v-saddr [2] = company.addr [2]
         v-scity     = company.city
         v-sstate    = company.state
         v-szip      = company.zip.

      if po-ord.type eq "D" then
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
      {po/po-cscga.i}
        
/*========*/
      for each po-ordl WHERE
           po-ordl.company EQ  po-ord.company AND
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

        if avail item and item.mat-type eq "B" then do:
          if v-shtsiz then do:
            assign v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
             v-wid = ( v-wid * 16 ) / 100
             v-wid = truncate(po-ordl.s-wid,0) + v-wid
             v-len = po-ordl.s-len - truncate(po-ordl.s-len,0)
             v-len = ( v-len * 16 ) / 100
             v-len = truncate(po-ordl.s-len,0) + v-len.
            
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

                find first ef where EF.COMPANY EQ JOB.COMPANY
                                AND ef.est-no  EQ job.est-no
                                and ef.form-no eq job-mat.frm
                              no-lock no-error.
                if avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B") THEN ASSIGN xg-flag = yes.
              end. /* avail job-mat */
            end. /* avail job */
          end. /* v-shtsiz */        
        end. /* avail item and item.mat-type eq "B" */

        IF v-printline > 42 THEN DO:         
           PAGE.
           v-printline = 0.
           {po/po-cscga.i}
        END.

        v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99").
        IF v-job-no = "-00" THEN v-job-no = "".
        PUT po-ordl.LINE FORM ">>9"
            po-ordl.ord-qty SPACE(2)
            po-ordl.pr-qty-uom SPACE(1)
            po-ordl.i-no FORM "x(23)" SPACE(1)
            v-adder[1] 
            v-job-no FORM "x(9)" SPACE(1)
            po-ordl.cost FORM "->>>9.99<<"
            po-ordl.pr-uom
            po-ordl.t-cost FORM "->>,>>9.99" 
            SKIP.

        lv-over-under = FILL(" ",3) + (STRING(po-ordl.over-pct,"ZZ9%") + " - "
                      + TRIM(STRING(po-ordl.under-pct,"ZZ9%"))).

        PUT lv-over-under AT 4
            po-ordl.i-name FORM "X(23)" AT 25 SPACE(1)
            v-adder[2] FORM "X(30)" SPACE(1)
            v-change-dscr  SKIP.
        v-printline = v-printline + 2.
        assign v-line-number = v-line-number + 3.
        
        if po-ordl.dscr[1] ne "" OR v-adder[3] <> "" then do:
          put po-ordl.dscr[1] format "x(23)" at 25 " "             
              v-adder[3] FORM "X(30)" 
              skip.
          v-line-number = v-line-number + 1.
          v-printline = v-printline + 1.
        end.
        
    
        if po-ordl.dscr[2] ne "" then do:
          put po-ordl.dscr[2] format "x(23)" at 25              
              " " v-adder[4] FORM "X(30)"
              skip.
          v-line-number = v-line-number + 1.
          v-printline = v-printline + 1.
        end.
        
        FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL itemfg AND itemfg.part-dscr3 <> "" THEN DO:
            put itemfg.part-dscr3  at 25 skip.
          v-line-number = v-line-number + 1.
          v-printline = v-printline + 1.
        END.
        IF v-vend-item <> "" THEN DO:
           PUT /*trim(po-ord.vend) AT 25 */ "Item#: " at 25  v-vend-item  FORM "x(15)" SKIP.
           v-line-number = v-line-number + 1.
           v-printline = v-printline + 1.
        END.

        RUN calc-cost (recid(po-ordl),OUTPUT v-cost,OUTPUT v-setup).
        v-setup = po-ordl.setup.

        IF po-ordl.item-type THEN DO:
           put "W: " at 25 v-wid space(2) "L: " v-len SPACE(4) 
                 v-adder[5] FORM "X(30)"
                STRING(v-cost) + " " + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)" SPACE(1).
            
            assign v-line-number = v-line-number + 1
                   v-printline = v-printline + 1.
        END.
        run po/po-ordls.p (recid(po-ordl)).
            
        {po/poprints.i}
            
            if not v-test-scr then do:
                  put skip                      
                      "Score: " AT 25
                      len-score format "x(50)" .
                      
                  v-line-number = v-line-number + 1.
                  v-printline = v-printline + 1.
            end.
            ELSE if dec(trim(len-score)) ne v-wid then do:
                  put skip
                      "Score: " AT 25
                      len-score format "x(50)" .
                      
                  v-line-number = v-line-number + 1.
                  v-printline = v-printline + 1.
            end.
          end.  /* if v-lscore-c ne "" from poprints.i */
          END.
        end.  /* avail reftable from poprints.i*/
   

        put skip(1).
        assign v-line-number = v-line-number + 1.
        v-printline = v-printline + 1.
        

      /* calc total sq feet */
    
        ASSIGN v-basis-w = 0
               v-dep     = 0.

    IF po-ordl.item-type THEN
    FIND FIRST ITEM
        WHERE ITEM.company EQ po-ord.company
          AND ITEM.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
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
     
     FOR EACH tt-formtext:
         DELETE tt-formtext.
     END.
     lv-text = "".
     FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
             lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
     END.
     IF lv-text <> "" THEN
     DO li = 1 TO 4:
            CREATE tt-formtext.
            ASSIGN
             tt-line-no = li
             tt-length  = 78.
     END.
     RUN custom/formtext.p (lv-text).
     i = 0.
     FOR EACH tt-formtext:
        i = i + 1.
        IF  i <= 4 THEN do:
            PUT tt-formtext.tt-text FORM "x(78)" 
                SKIP.      
            v-printline = v-printline + 1.
        END.
     END.
    
     /* === spec note print */
     IF v-print-sn THEN DO:
        ASSIGN v-tmp-lines = 0
               v-inst-lines = 0
               lv-item-rec = "".

        lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                      ELSE IF AVAIL itemfg THEN itemfg.rec_key
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
           if v-inst-lines gt 0 then v-inst-lines = v-inst-lines + 1.
           v-printline = v-printline + v-inst-lines + 1.
           IF v-printline > 46 AND v-inst-lines > 0 THEN DO:         
              PAGE.
              v-printline = 0.
              {po/po-cscga.i}
           END.     
    
           FOR EACH notes WHERE notes.rec_key = lv-item-rec AND
               /* notes.note_type = "S" */  notes.note_code = "PO" NO-LOCK:
               v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
               {SYS/INC/ROUNDUP.I v-tmp-lines}
               IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                  PUT {1} substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)"                      
                      lv-item-rec notes.note_code SKIP.              
                  v-printline = v-printline + 1.
               END.
           end.
        END. /* lv-item-spec <> "" */
     END.
     /* === end of specnote print */

  end. /* for each po-ordl record */
  
  v-inst = "".
  FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
                v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                   /*PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                   v-printline = v-printline + 1.                                           */
                   IF i < 5  THEN  /* display upto 4 lines */
                       ASSIGN v-inst[i] =  substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80)
                              v-printline = v-printline + 1.
                   ELSE LEAVE.
                END.

   end.
  
      PUT "Total Sq. Ft: "  AT 50 v-tot-sqft FORM ">>>,>>9" SKIP.
      v-tot-sqft = 0.
      v-bot-lab[1] = "GST        :"
                     /*vend.tax-gr + "        :       " */ + STRING(po-ord.tax,"->>,>>9.99").

      PUT "<R53><C1>" v-inst[1] 
          "<R54><C1>" v-inst[2] 
          "<R55><C1>" v-inst[3] 
          "<R56><C1>" v-inst[4] 
          "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
    "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "->>,>>9.99"
    "<=8><R+2> "  v-bot-lab[1] 
    "<=8><R+3> "  " " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
                /*v-bot-lab[2] */
    "<=8><R+4> Grand Total:" po-ord.t-cost FORM "->>,>>9.99" .

v-printline = v-printline + 6.
IF v-printline <= page-size THEN PUT SKIP(74 - v-printline).

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

