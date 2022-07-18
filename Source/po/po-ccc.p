/* --------------------------------------------- po/po-ccc.p  */
/* Purchase Order XPrint Program for N-K-POPRINT = CCC                        */
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
DEF VAR v-sig-image AS CHAR NO-UNDO.
DEF var v-dept-note   AS CHARACTER FORM "x(80)" EXTENT 50 NO-UNDO.

DEF VAR v-inst-lines  AS INTEGER   NO-UNDO.
DEF VAR v-inst        AS CHARACTER FORM "x(80)" EXTENT 4 NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1     AS CHARACTER NO-UNDO.
DEF VAR ls-full-img1  AS CHARACTER FORM "x(200)" NO-UNDO.
DEF VAR cTermsImage1  AS CHARACTER FORM "x(200)" NO-UNDO.
DEF VAR cTermsImage2  AS CHARACTER FORM "x(200)" NO-UNDO.
DEF VAR v-signature   AS CHARACTER FORM "x(100)" NO-UNDO.

 ASSIGN
    FILE-INFO:FILE-NAME = ".\CustFiles\Images\3cTerms1.jpg" .
    cTermsImage1 = FILE-INFO:FULL-PATHNAME + ">" .
    
 ASSIGN
    FILE-INFO:FILE-NAME = ".\CustFiles\Images\3cTerms2.jpg" .
    cTermsImage2 = FILE-INFO:FULL-PATHNAME + ">" .

DEFINE VARIABLE cRtnChar     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValid       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).

IF lRecFound AND cRtnChar NE "" THEN 
DO:
    cRtnChar = DYNAMIC-FUNCTION (
        "fFormatFilePath",
        cRtnChar
        ).
                   
    /* Validate the N-K-1 BusinessFormLogo image file */
    RUN FileSys_ValidateFile(
        INPUT  cRtnChar,
        OUTPUT lValid,
        OUTPUT cMessage
        ) NO-ERROR.

    IF NOT lValid THEN 
    DO:
        MESSAGE "Unable to find image file '" + cRtnChar + "' in N-K-1 setting for BusinessFormLogo"
            VIEW-AS ALERT-BOX ERROR.
    END.
END.

ASSIGN 
    ls-full-img1 = cRtnChar + ">" .

DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO.
DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
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
DEF VAR v-overrun AS DECI NO-UNDO.
DEF VAR v-underrun AS DECI NO-UNDO.
DEF VAR v-inst2 AS cha EXTENT 25 NO-UNDO. 
DEF VAR v-wld AS CHAR  FORM "x(30)"  NO-UNDO.
DEFINE VARIABLE dCoreDia AS DECIMAL FORMAT ">,>>9.99<<" NO-UNDO.
DEFINE VARIABLE cFlueTest AS CHARACTER  NO-UNDO.

v-dash-line = fill ("_",80).

/*==============*/
DEF VAR lv-file-name AS cha FORM "x(60)" NO-UNDO.
OS-CREATE-DIR VALUE("c:\temp\fax").
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
if avail company then
assign
 v-hdr       = "VEND ITEM"
 v-sname     = company.name
 v-saddr [1] = company.addr [1]
 v-saddr [2] = company.addr [2]
 v-scity     = company.city
 v-sstate    = company.state
 v-szip      = company.zip
 v-comp-add1 = company.addr[1]
 v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
 v-tot-sqft = 0.

    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
        EXCLUSIVE-LOCK
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

      v-printline = 0.
      {po/po-ccc.i}

      for each po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no by po-ordl.line:
        
        if not v-printde-po and po-ordl.deleted then next.
        assign xg-flag = NO v-change-dscr = "".

        if po-ordl.stat eq "A" THEN ASSIGN v-change-dscr = "Added".
        else if po-ordl.stat eq "U" THEN assign v-change-dscr = "Updated".
        else if po-ordl.stat eq "O" THEN assign v-change-dscr = "Open".
        else if po-ordl.stat eq "P" then assign v-change-dscr = "Partial".
        else if po-ordl.stat eq "C" then assign v-change-dscr = "Closed".

        if po-ordl.deleted eq yes then assign v-change-dscr = "Deleted".

        assign v-ino-job = po-ordl.vend-i-no.
        
        find item where item.company eq po-ordl.company
                    and item.i-no    eq po-ordl.i-no
                    and po-ordl.item-type
                  no-lock no-error.
        ASSIGN
           v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                          +
                         (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "")
           v-wid = po-ordl.s-wid
           v-len = po-ordl.s-len
           v-vend-item = po-ordl.vend-i-no.

        if avail item and item.mat-type eq "B" then do:
          if v-shtsiz then do:
            assign
               v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
               v-wid = ( v-wid * 16 ) / 100
               v-wid = truncate(po-ordl.s-wid,0) + v-wid
               v-len = po-ordl.s-len - truncate(po-ordl.s-len,0)
               v-len = ( v-len * 16 ) / 100
               v-len = truncate(po-ordl.s-len,0) + v-len
               v-num-add = 0.

            find first job where job.company eq cocode 
                             and job.job-no eq po-ordl.job-no 
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
                if avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B") THEN
                   ASSIGN xg-flag = yes.
              end. /* avail job-mat */
            end. /* avail job */
          end. /* v-shtsiz */        
        end. /* avail item and item.mat-type eq "B" */
        ELSE if avail item and (item.mat-type eq "D" OR ITEM.mat-type = "5" OR ITEM.mat-type = "6" OR ITEM.mat-type = "C")
                           AND po-ordl.s-wid = 0 
                           AND po-ordl.s-len = 0 then do:
            ASSIGN v-wid = ITEM.case-w
                   v-len = ITEM.case-l.
        END.

        IF v-printline > 42 THEN DO:         
           PAGE.
           v-printline = 0.
           {po/po-ccc.i}
        END.
       
        v-job-no = TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', po-ordl.job-no, po-ordl.job-no2))).
        IF v-job-no BEGINS "-" THEN v-job-no = "".
        PUT po-ordl.LINE FORM ">>9"
            po-ordl.ord-qty SPACE(2)
            po-ordl.pr-qty-uom SPACE(1)
            po-ordl.i-no FORM "x(23)" SPACE(2)
            po-ordl.due-date  FORM "99/99/9999" SPACE(1)
            /*v-adder[1] */
            v-job-no FORM "x(13)" SPACE(1) .

        IF po-ordl.cost > 99999 THEN
            PUT
            /*po-ordl.cost FORM "->>>9.99<<"*/ SPACE(8)
            po-ordl.pr-uom
            po-ordl.t-cost FORM "->>,>>9.99" 
            SKIP.
        ELSE 
            PUT
            po-ordl.cost FORM "->>>9.99<<" 
            po-ordl.pr-uom
            po-ordl.t-cost FORM "->>,>>9.99" 
            SKIP.

        PUT po-ordl.i-name AT 25 SPACE(1)
            space(31) /*v-adder[2] SPACE(1)*/
            v-change-dscr  SKIP.

        ASSIGN
           v-printline = v-printline + 2
           v-line-number = v-line-number + 3.
        
        if po-ordl.dscr[1] ne "" /*OR v-adder[3] <> ""*/ then do:
          put po-ordl.dscr[1] format "x(52)" at 25 " "             
              /*v-adder[3]  */
              skip.
          ASSIGN
             v-line-number = v-line-number + 1.
             v-printline = v-printline + 1.
        end.
    
        if po-ordl.dscr[2] ne "" then do:
          put po-ordl.dscr[2] format "x(52)" at 25              
              /*" " v-adder[4] */
              skip.
          ASSIGN
             v-line-number = v-line-number + 1
             v-printline = v-printline + 1.
        end.
        
        FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL itemfg AND itemfg.part-dscr3 <> "" THEN DO:
            put itemfg.part-dscr3  at 25 skip.
          ASSIGN
             v-line-number = v-line-number + 1
             v-printline = v-printline + 1.
        END.
        IF v-vend-item <> "" THEN DO:
           PUT "Item#: " at 25  v-vend-item  FORM "x(15)" SKIP.
           ASSIGN
              v-line-number = v-line-number + 1
              v-printline = v-printline + 1.
        END.

        v-setup = po-ordl.setup.

        dCoreDia = 0.
        IF AVAIL ITEM AND ITEM.industry EQ "2" THEN
            ASSIGN dCoreDia =  IF item.mat-type EQ "P" THEN (item.ect / 10000) ELSE item.ect.
        ELSE dCoreDia =  IF item.mat-type NE "A" THEN (item.ect / 10000) ELSE item.ect.

        IF po-ordl.item-type THEN DO:
           ASSIGN v-wld = "".
           IF ITEM.mat-type = "C" OR ITEM.mat-type = "5"  OR item.mat-type = "6" THEN
             ASSIGN v-wld = "W: " + STRING(v-wid,">>9.99") + " L: " + string(v-len,">>9.99") +
                            " D: " + string(item.case-d,">>9.99").
           ELSE
           IF ITEM.mat-type = "P" THEN
             ASSIGN v-wld = "W:" + STRING(v-wid,">>9.99").
           ELSE
           IF ITEM.mat-type = "B" OR item.mat-type = "D" THEN
             ASSIGN v-wld = "W: " + STRING(v-wid,">>9.99") + " L: " + string(v-len,">>9.99").

           IF LENGTH(TRIM(v-wld)) < 30 THEN DO:
               ASSIGN v-wld = v-wld + FILL(" ":U, 50 - LENGTH(TRIM(v-wld))).
           END.

           PUT v-wld AT 25
               "Core Dia: " dCoreDia FORMAT ">,>>9.9<<<" SPACE(3)
               /*"W: " at 25 v-wid space(2) "L: " v-len  
               "                          "*/
                STRING(v-cost) + " " + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)" SPACE(1)
                 /*v-adder[5]*/.
            
            assign v-line-number = v-line-number + 1
                   v-printline = v-printline + 1.
        END.

       /* IF dCoreDia GT 0 THEN DO:
            put "Core Dia: " AT 25 dCoreDia FORMAT ">,>>9.9<<<" SKIP.
            ASSIGN
                v-line-number = v-line-number + 1
                v-printline = v-printline + 1.
        END.
        ELSE
           PUT SKIP.*/

        /* added this over/underrun row */
        IF po-ordl.item-type THEN DO:
           put "Overrun%: " at 25 po-ordl.over-pct FORM "->>9.99" space(2) 
               "Underrun%: " po-ordl.under-pct FORM "->>9.99"
                 "                          "
                /*STRING(v-cost) + " " + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)" SPACE(1)
                 v-adder[5]*/.
            
            assign v-line-number = v-line-number + 1
                   v-printline = v-printline + 1.
        END.

        
            
        {po/poprints.i}
            
        if not v-test-scr then do:
           put skip
               "Score: " AT 25
               len-score format "x(50)".

           ASSIGN    
              v-line-number = v-line-number + 1
              v-printline = v-printline + 1.
        end.
        ELSE if dec(trim(len-score)) ne v-wid then do:
              put skip
                  "Score: " AT 25
                  len-score format "x(50)" .
                  
              ASSIGN
                 v-line-number = v-line-number + 1
                 v-printline = v-printline + 1.
        end.
        end.  /* if v-lscore-c ne "" from poprints.i */
        END.
        end.  /* avail reftable from poprints.i*/

        put skip(1).
        assign
           v-line-number = v-line-number + 1
           v-printline = v-printline + 1
           /* calc total sq feet */
           v-basis-w = 0
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

    ASSIGN
       v-tot-sqft = v-tot-sqft + (v-qty * 1000)
       v-tmp-lines = 0
       v-inst-lines = 0.

     FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
         IF notes.note_text <> "" THEN DO:
            v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
            {SYS/INC/ROUNDUP.I v-tmp-lines}
            v-inst-lines = v-inst-lines + v-tmp-lines. 
         END.
     END.
     if v-inst-lines gt 0 then v-inst-lines = v-inst-lines + 1.
    
     FOR EACH tt-formtext:
         DELETE tt-formtext.
     END.

     lv-text = "".
     FOR EACH notes FIELDS(note_text) WHERE
         notes.rec_key = po-ordl.rec_key
         NO-LOCK:
         lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
     END.

     IF lv-text <> "" THEN
        DO li = 1 TO 20:
           CREATE tt-formtext.
           ASSIGN
            tt-line-no = li
            tt-length  = 80.
        END.
     RUN custom/formtext.p (lv-text).
     ASSIGN
         i = 0
         v-dept-note = "".

     FOR EACH tt-formtext:
         i = i + 1.
         IF i <= 20 THEN
                v-dept-note[i] = tt-formtext.tt-text.
         /*IF i <= 20 THEN do:
            PUT tt-formtext.tt-text FORM "x(78)" SKIP.      
            v-printline = v-printline + 1.
        END.*/
     END.
     li = 0.
         DO i = 20 TO 1 BY -1:
           li = i.
           IF v-dept-note[i] <> "" THEN LEAVE.
         END.
     DO i = 1 TO li:
            PUT v-dept-note[i] SKIP.
            v-printline = v-printline + 1.
         END.
    
     /* === spec note print */
     IF v-print-sn THEN DO:
        ASSIGN v-tmp-lines = 0
               v-inst-lines = 0
               lv-item-rec = ""
               v-inst2 = ""
               lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                             ELSE IF AVAIL itemfg THEN itemfg.rec_key
                             ELSE "".

        IF lv-item-rec <> "" THEN DO:
           {custom/notespr9.i itemfg v-inst2 25 
               "notes.rec_key EQ lv-item-rec AND notes.note_code = 'PO' "}
 
           DO i = 1 TO 25:
              IF v-inst2[i] NE "" THEN DO:  
                 PUT {1} v-inst2[i] FORMAT "X(128)" SKIP.
                 v-printline = v-printline + 1.
              END.
           END.
        END. /* lv-item-rec <> "" */
     END.
     /* === end of specnote print */

  end. /* for each po-ordl record */
   
  v-inst = "".
  
  {custom/notesprt.i po-ord v-inst 4}
           DO i = 1 TO 4:
                IF v-inst[i] <> "" THEN DO:                
                   v-printline = v-printline + 1.
                END.
           END.
  
   ASSIGN  
      v-tot-sqft = 0
      v-bot-lab[1] = "Tax        :"
                   + STRING(po-ord.tax,"->>,>>9.99").

   PUT "<R53><C1>" v-inst[1] 
       "<R54><C1>" v-inst[2] 
       "<R55><C1>" v-inst[3] 
       "<R56><C1>" v-inst[4] 
       "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
       "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "->>,>>9.99"
       "<=8><R+2> "  v-bot-lab[1] 
       "<=8><R+3> "  " "
       "<=8><R+4> Grand Total:" po-ord.t-cost FORM "->>,>>9.99".

   PUT "<FArial><R54><C1><P12>  <P9> " SKIP
       " " SKIP
       " " SKIP
       "<R60><C1>AUTHORIZED BY: ________________________________"
       v-signature
       SKIP.
       
   PUT "<FArial><R63.2><C2.5><P6>" "All sales/purchases described herein are subject to Essentra Packaging's terms and conditions attached hereto and made a part hereof (""Terms and Conditions"").  Essentra Packaging disclaims and rejects any" SKIP
    "<C2.5><R-0.3>" "additional terms and conditions proposed by Supplier and the same shall not be binding upon Essentra Packaging, regardless of when submitted.  Supplier�s acceptance of the sales/purchases described herein" SKIP
    "<C2.5><R-0.3>" "and said Terms and Conditions may be confirmed in writing (via letter, email, or fax) or any verbal or physical manifestation of acceptance including, but not limited to, Supplier's delivery of goods or" SKIP
    "<C2.5><R-0.3>" "acceptance of payment made by Essentra Packaging for the sales or purchases described herein."
    "<P9>".

v-printline = v-printline + 6.
IF v-printline <= page-size THEN PUT SKIP(74 - v-printline).

PUT UNFORMATTED "<R1><C1><R65><C110><IMAGE#1=" cTermsImage1 SKIP .
                        PAGE.

PUT UNFORMATTED "<R1><C1><R65><C110><IMAGE#1=" cTermsImage2 SKIP .
                        PAGE. 

end. /* for each po-ord record */.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */
