/* --------------------------------------------- po/po-indiana.p  */
/* Purchase Order XPrint Program for N-K-POPRINT = INDIANA                    */
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
def var time_stamp as CHAR NO-UNDO.
def var v-exp-limit as int no-undo init 10.
def var v-line-number as INT NO-UNDO.
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
def var xg-flag as log init no no-undo.
def var v-space as log init YES NO-UNDO.
def var len-score as CHAR NO-UNDO.
def buffer xjob-mat for job-mat.
def buffer xitem for item.
def var same-score as ch no-undo.
def var v-test-scr as log no-undo.
def var v-hdr as char format "x(15)" initial "" no-undo.
def var v-ino-job as char format "x(15)" initial "" no-undo.
def var v-change-ord as char format "x(35)" initial "" no-undo.

DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF SHARED VAR s-print-prices AS LOG NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(60)" NO-UNDO.
ASSIGN ls-image1 = "images\icc.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">"
       v-dash-line = fill ("_",80).

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-vend-item AS cha FORMAT "X(45)" NO-UNDO.
DEF VAR lv-item-rec AS cha NO-UNDO.
DEF VAR v-dec-fld AS DEC no-undo.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "POPRINT" no-lock no-error.

v-dec-fld = IF AVAIL sys-ctrl THEN sys-ctrl.dec-fld ELSE 0.

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

find first company where company.company eq cocode NO-LOCK.

assign v-hdr = "VEND ITEM".
       
 print-po-blok:
 FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
     FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
     BREAK BY po-ord.vend-no BY PO-ORD.PO-NO:

     IF NOT CAN-FIND(FIRST po-ordl WHERE
        po-ordl.company EQ po-ord.company AND
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
      {po/po-indiana.i}
        
/*========*/
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
        
        find item where item.company eq po-ordl.company
                    and item.i-no    eq po-ordl.i-no
                    and po-ordl.item-type
                  no-lock no-error.
        
        v-vend-item = "Vendor ID: " + po-ord.vend + " Item#: " + po-ordl.vend-i-no.

        if avail item and item.mat-type eq "B" then do:
          if v-shtsiz then do:
            IF v-dec-fld EQ 0.08 THEN
               assign v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
                      v-wid = ( v-wid * 16 ) / 100
                      v-wid = truncate(po-ordl.s-wid,0) + v-wid
                      v-len = po-ordl.s-len - truncate(po-ordl.s-len,0)
                      v-len = ( v-len * 16 ) / 100
                      v-len = truncate(po-ordl.s-len,0) + v-len.
            ELSE
                ASSIGN
                   v-wid = po-ordl.s-wid
                   v-len = po-ordl.s-len.

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
           {po/po-indiana.i}
        END.

        PUT po-ordl.LINE FORM ">>9"
            po-ordl.ord-qty SPACE(2)
            po-ordl.pr-qty-uom SPACE(1)
            po-ordl.i-no FORM "x(23)" SPACE(20).

        IF s-print-prices THEN
           PUT po-ordl.cost FORM "->>>9.99<<" SPACE(1)
               po-ordl.pr-uom SPACE(5) 
               po-ordl.t-cost FORM "->>,>>9.99".
        
        PUT SKIP po-ordl.i-name AT 25 SPACE(1)
            space(13) v-change-dscr  SKIP.
        v-printline = v-printline + 2.
        assign v-line-number = v-line-number + 3.
        
        if po-ordl.dscr[1] ne "" then do:
          put po-ordl.dscr[1] format "x(52)" at 25 " "             
              skip.
          v-line-number = v-line-number + 1.
          v-printline = v-printline + 1.
        end.
        
        if po-ordl.dscr[2] ne "" then do:
          put po-ordl.dscr[2] format "x(52)" at 25              
              SKIP.
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
           PUT v-vend-item AT 25 FORMAT "X(45)" SKIP.
           v-line-number = v-line-number + 1.
           v-printline = v-printline + 1.
        END.

        IF po-ordl.item-type THEN DO:
           IF v-wid GT 0 THEN
              put "W: " at 25 v-wid.
            
           IF v-len GT 0 THEN
             PUT space(2) "L: " v-len.  
            
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
               lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                             ELSE IF AVAIL itemfg THEN itemfg.rec_key
                             ELSE "".

        IF lv-item-rec <> "" THEN DO:
           FOR EACH notes WHERE notes.rec_key = lv-item-rec AND 
               notes.note_code = "PO" NO-LOCK:

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
              {po/po-indiana.i}
           END.     
    
           FOR EACH notes WHERE notes.rec_key = lv-item-rec AND
               notes.note_code = "PO" NO-LOCK:
               v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
               {SYS/INC/ROUNDUP.I v-tmp-lines}

               IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                  PUT {1} substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)"
                      SKIP.

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
         IF i < 5  THEN  /* display upto 4 lines */
             ASSIGN v-inst[i] =  substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80)
                    v-printline = v-printline + 1.
         ELSE LEAVE.
      END.
   end.
  
   v-bot-lab[1] = "Tax        :"
                + STRING(po-ord.tax,"->>,>>9.99").

   PUT "<R53><C1>" v-inst[1] 
       "<R54><C1>" v-inst[2] 
       "<R55><C1>" v-inst[3] 
       "<R56><C1>" v-inst[4].

   IF s-print-prices THEN
     PUT "<R58><C60><#8><FROM><R+5><C+20><RECT> "
         "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "->>,>>9.99"
         "<=8><R+2> "  v-bot-lab[1] 
         "<=8><R+3> "  " "
         "<=8><R+4> Grand Total:" po-ord.t-cost FORM "->>,>>9.99" .

PUT "<FArial><R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
     " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
     " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
     SKIP.     
     

v-printline = v-printline + 6.
IF v-printline <= page-size THEN PUT SKIP(74 - v-printline).

end. /* for each po-ord record */.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */
