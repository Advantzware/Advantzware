/* --------------------------------------------- po/po-pnp.p 12/00 FWK */
/* Purchase Order Print Program for S-8-POPRINT = P&P                         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-ref1  for reftable.
def buffer b-ref2  for reftable.

{po/po-print.i}
{custom/notesdef.i}
def var v-wid like po-ordl.s-wid format ">>9.99<<" no-undo.
def var v-len like po-ordl.s-len format ">>9.99<<" no-undo.
def var v-wid2 like po-ordl.s-wid format ">>9.99<<" no-undo. /* for recalc extened cost */
def var v-len2 like po-ordl.s-len format ">>9.99<<" no-undo. /* for recalc extened cost */
DEF var pol-counter as int no-undo.
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
DEF VAR lv-item-rec AS cha NO-UNDO.



DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(60)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(60)" NO-UNDO.
ASSIGN ls-image1 = "images\action.jpg"
       ls-image2 = "images\pacific2.bmp"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">"
       FILE-INFO:FILE-NAME = ls-image2
       ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

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
DEF VAR v-dep AS dec FORM ">>9.99<<" NO-UNDO.
DEF VAR v-qty LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR v-tot-sqft AS DEC NO-UNDO.
DEF VAR v-vend-item AS cha NO-UNDO.
def var v-adder AS cha FORM "x(15)" extent 5 no-undo.
def var v-num-add as int initial 0 no-undo.
DEF VAR v-job-no AS cha NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-sub-cost AS DEC NO-UNDO.
DEF VAR v-tot-cost AS DEC NO-UNDO.
DEF VAR v-totsetup AS DEC NO-UNDO.
DEF VAR v-setup AS DEC NO-UNDO.
DEF VAR v-tax-tot AS DEC NO-UNDO.
DEF VAR v-tax-rate AS DEC NO-UNDO.
DEF VAR v-frt-tax-rate AS DEC NO-UNDO.
DEF VAR vt-cost LIKE po-ordl.t-cost NO-UNDO.
DEF VAR v-ord-qty LIKE po-ordl.ord-qty NO-UNDO.

DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.

v-dash-line = fill ("_",80).

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
 v-comp-add3 = "Phone: 604.857.1660" 
 v-comp-add4 = "Fax     : 604.857.1665".
 .
 v-tot-sqft = 0.
    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
        BREAK BY PO-ORD.PO-NO: 
      
      if po-ord.type eq "D" then
        assign v-sname     = po-ord.ship-name
               v-saddr[1]  = po-ord.ship-addr[1]
               v-saddr[2]  = po-ord.ship-addr[2]
               v-scity     = po-ord.ship-city
               v-sstate    = po-ord.ship-state
               v-szip      = po-ord.ship-zip.

      {po/exportpo.i}

      find first vend where vend.company eq po-ord.company 
                        and vend.vend-no eq po-ord.vend-no no-lock no-error.
      find first terms where terms.t-code eq po-ord.terms no-lock no-error.
      find first carrier where carrier.company eq po-ord.company 
                           and carrier.carrier eq po-ord.carrier no-lock no-error.

      ASSIGN
       v-page-counter = 1
       v-change-ord   = ""
       v-tax-rate     = 0
       v-frt-tax-rate = 0
       v-po-type      = IF po-ord.type EQ "R" THEN "Regular" ELSE "Drop Ship"
       v-freight-dscr = IF po-ord.frt-pay EQ "P" THEN "Prepaid" ELSE
                        IF po-ord.frt-pay EQ "C" THEN "Collect" ELSE "Bill".

      IF po-ord.stat EQ "N" THEN po-ord.stat = "O".
      ELSE
      IF po-ord.stat EQ "U" THEN v-change-ord = "(CHANGED ORDER ONLY)".

      IF po-ord.tax-gr NE "" THEN
        RUN ar/cctaxrt.p (po-ord.company, po-ord.tax-gr,
                          OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

      v-printline = 0.
      {po/po-pacif.i}  /* xprint form */

      ASSIGN v-sub-cost = 0
             v-tot-cost = 0
             v-totsetup = 0
             v-tax-tot  = 0.

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
        v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                        +
                      (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "").
        ASSIGN v-wid = po-ordl.s-wid
               v-len = po-ordl.s-len
               v-wid2 = po-ordl.s-wid
               v-len2 = po-ordl.s-len.

        if avail item and item.mat-type eq "B" then do:
          if v-shtsiz then do:
            assign v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0).
            assign v-wid = ( v-wid * 16 ) / 100.
            assign v-wid = truncate(po-ordl.s-wid,0) + v-wid.
            assign v-len = po-ordl.s-len - truncate(po-ordl.s-len,0).
            assign v-len = ( v-len * 16 ) / 100.
            assign v-len = truncate(po-ordl.s-len,0) + v-len.
            
            assign v-num-add = 0.
            v-adder = "".
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
        v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,">>").

        IF po-ordl.setup NE 0 THEN v-setup = po-ordl.setup.
        ELSE RUN calc-cost (RECID(po-ordl), OUTPUT v-cost, OUTPUT v-setup).
/* calc t-cost */

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


        if lookup(INPUT po-ordl.pr-uom,"L,LOT") > 0 then
           assign
               vt-cost =  v-cost - round((v-cost * po-ordl.disc) / 100, 2).
        ELSE if po-ordl.pr-qty-uom ne po-ordl.pr-uom then do:
              v-ord-qty = 0.
              run sys/ref/convquom.p(po-ordl.pr-qty-uom, po-ordl.pr-uom,
                                     v-basis-w, v-len2, v-wid2, v-dep,
                                     po-ordl.ord-qty, output v-ord-qty).
              vt-cost = (v-ord-qty * v-cost) -
                       round( (v-ord-qty * v-cost * po-ordl.disc) / 100, 2).
        end.
        ELSE vt-cost = (po-ordl.ord-qty * v-cost) -
                       round( (po-ordl.ord-qty * v-cost * po-ordl.disc) / 100, 2).

        vt-cost = vt-cost + v-setup.

/* end of t-cost calc */
        ASSIGN v-sub-cost = v-sub-cost + po-ordl.t-cost
               v-tot-cost = v-tot-cost + po-ordl.t-cost
               v-totsetup = v-totsetup + v-setup.

        /* calc tax without setup */
        IF po-ordl.tax THEN
          v-tax-tot = v-tax-tot + ROUND((vt-cost * v-tax-rate / 100),2).
        
        IF v-job-no = "-" THEN v-job-no = "".

        DO i = 1 TO 5:
           IF v-adder[i] <> "" AND LENGTH(v-adder[i]) < 15 THEN
              v-adder[i] = FILL(" ", 15 - LENGTH(v-adder[i])) + v-adder[i].
        END.

    IF v-printline > 46 THEN DO:         
        PAGE.
        v-printline = 0.
        {po/po-pacif.i} 
    END.

        PUT po-ordl.LINE FORM ">>9"
            po-ordl.ord-qty SPACE(2)
            po-ordl.pr-qty-uom SPACE(1)
            po-ordl.i-no FORM "x(23)" SPACE(1)
            v-adder[1] 
            v-job-no FORM "x(9)" SPACE(1)
            po-ordl.cost FORM "->>>9.99<<"
            po-ordl.pr-uom
            po-ordl.t-cost - v-setup  FORM "->>,>>9.99"             
            SKIP.

        v-vend-item = po-ordl.vend-i-no.

        PUT po-ordl.i-name AT 25 FORM "x(26)" SPACE(1) /*v-vend-item FORM "x(15)" space(1) */
            v-adder[2] SPACE(16)
            v-change-dscr  SKIP.
        v-printline = v-printline + 2.
        assign v-line-number = v-line-number + 3.
        
        if po-ordl.dscr[1] ne "" OR v-adder[3] <> "" then do:
          put po-ordl.dscr[1] format "x(26)" at 25 " "             
              v-adder[3] 
              skip.
          v-line-number = v-line-number + 1.
          v-printline = v-printline + 1.
        end.
            
        if po-ordl.dscr[2] ne "" OR v-adder[4] <> "" then do:
          put po-ordl.dscr[2] format "x(26)" at 25              
              " " v-adder[4] skip.
          v-line-number = v-line-number + 1.
          v-printline = v-printline + 1.
        end.
        IF v-adder[5] <> "" OR v-vend-item <> "" THEN DO:
            put v-vend-item  FORM "x(26)" AT 25              
                " " v-adder[5] skip.
            v-line-number = v-line-number + 1.
            v-printline = v-printline + 1.
        END.
        IF (AVAIL ITEM AND index("1,2,3,4",ITEM.mat-type) > 0 ) THEN
           put "W: " at 25 v-wid space(2) "L: " v-len                   
                 "  D: " v-dep 
            /*   STRING(v-cost) + " " + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)"   */

                /* v-adder[5] AT 72 */ SKIP.
        ELSE put "W: " at 25 v-wid space(2) "L: " v-len  
                 "                          "
            /*   STRING(v-cost) + " " + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)"   */

                /* v-adder[5] AT 78 */ SKIP
                 .
            
        assign v-line-number = v-line-number + 1
                   v-printline = v-printline + 1.

        run po/po-ordls.p (recid(po-ordl)).            
        {po/poprints.i}            
            IF AVAIL ITEM AND lookup(ITEM.mat-type,"1,2,3,4") > 0 THEN DO: 
            END.
            ELSE DO:
                if not v-test-scr then do:
                   put "Score: " AT 25
                       len-score format "x(50)" SKIP.
                      
                   v-line-number = v-line-number + 1.
                   v-printline = v-printline + 1.
                end.
          
                else
                if dec(trim(len-score)) ne v-wid then do:
                  put "Score: " AT 25
                      len-score format "x(50)" SKIP.
                      
                  v-line-number = v-line-number + 1.
                  v-printline = v-printline + 1.
                end.
            END.
          end.
          END.
        end.

       /*  old 
        repeat pol-counter = 1 to 4:
          if po-ordl.spec-i[pol-counter] ne "" then
          do:
            IF v-printline > 46 THEN DO:         
               PAGE.
               v-printline = 0.
               {po/po-pacif.i}
            END. 
            put po-ordl.spec-i[pol-counter] format "x(65)" at 10 skip.
            assign v-line-number = v-line-number + 1.
            v-printline = v-printline + 1.
          end.
        end.
       */
        put "GL# " AT 25 po-ordl.actnum skip.
        assign v-line-number = v-line-number + 1.
        v-printline = v-printline + 1.

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
 /*
     ASSIGN v-tmp-lines = 0
            v-inst-lines = 0.
     FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
            IF notes.note_text <> "" THEN DO:
               v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
               {SYS/INC/ROUNDUP.I v-tmp-lines}
               v-inst-lines = v-inst-lines + v-tmp-lines. 
            END.
     END.
 
  /*   if v-inst-lines gt 0 then v-inst-lines = v-inst-lines + 1. */
     v-printline = v-printline + v-inst-lines.
     
     IF v-printline > 46 THEN DO:                  
        PAGE.
        v-printline = 0.
        {po/po-pacif.i}
     END.

     FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
             v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
             {SYS/INC/ROUNDUP.I v-tmp-lines}
             IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                v-printline = v-printline + 1.
             
             END.
            
     end.
 */
     FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                         AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
     lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                   ELSE IF AVAIL itemfg THEN itemfg.rec_key
                   ELSE "".
     {custom/notespr2.i "lv-item-rec" v-inst 4 "notes.rec_key = lv-item-rec and notes.note_code = 'PO'"}
     DO i = 1 TO 4:
       IF v-inst[i] <> "" THEN DO:
          IF v-printline gt 46 then DO:
             page .
             v-printline = 0.
             {po/po-pacif.i}
          END.
          PUT v-inst[i] SKIP.
          v-printline = v-printline + 1.
       END.
     END.

     IF v-printline > 46 THEN DO:                  
        PAGE.
        v-printline = 0.
        {po/po-pacif.i}
     END.


  end. /* for each po-ordl record */
  /*
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
  */
  {custom/notesprt.i po-ord v-inst 4}
  IF v-printline > 46 THEN DO:                  
     PAGE.
     v-printline = 0.
     {po/po-pacif.i}
  END.

  /*v-printline 46*/
      v-tax-tot = ROUND(v-tax-tot,2).
      v-tot-cost = v-sub-cost + v-tax-tot.

      PUT "Total Sq. Ft: "  AT 50 v-tot-sqft FORM ">>>,>>9" SKIP.

      IF v-totsetup NE 0 THEN
        PUT "Setup Charges: " AT 50 v-totsetup FORM "->>,>>9.99" TO 95 SKIP.

      v-tot-sqft = 0.
      v-bot-lab[1] = "GST        :"
                     /*vend.tax-gr + "        :       " */ 
                     + STRING(v-tax-tot,"->>,>>9.99") /*STRING(po-ord.tax,"->>,>>9.99")*/.
   
      PUT "<R52><C1>" v-inst[1] 
          "<R53><C1>" v-inst[2] 
          "<R54><C1>" v-inst[3] 
          "<R55><C1>" v-inst[4] 
          "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
          "<=8><R+1> Sub Total  :" /*po-ord.t-cost - po-ord.tax */ v-sub-cost FORM "->>,>>9.99"
          "<=8><R+2> " v-bot-lab[1] 
          "<=8><R+3> " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
                v-bot-lab[2]
          "<=8><R+4> Grand Total:" v-tot-cost /*not to include setup  po-ord.t-cost*/  FORM "->>,>>9.99" 
          .

PUT  "<R57><C2><FROM><R57><C25><LINE> <R57><C33><FROM><R57><C57><LINE>" SKIP
     "<FArial><R57><C2><P9>Authorized  By <R57><C33>Approved  By" skip
     "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
     "  Acknowledge order verifying price and delivery date. No partial or back orders without our permission."SKIP
     "  Prices on this order are firm unless notified otherwise. If no prices are on this order," SKIP
     "  please provide current pricing.  Pst # 808980. " SKIP(1)     
     "  I acknowledge the pricing on this P.O. are correct. _________________________(please sign and fax)" SKIP
     .

v-printline = v-printline + 6.

IF v-printline < 60 THEN PAGE. /* PUT SKIP(80 - v-printline).*/
       ASSIGN v-sub-cost = 0
              v-tot-cost = 0
              v-totsetup = 0.

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

   RELEASE e-item-vend.
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
                              v-len2, v-wid2, vv-dep,
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
              tt-eiv.setups[li + 10] = e-item-vend.runQtyXtra[li].
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
                                v-len2, v-wid2, vv-dep,
                                vv-cost, OUTPUT vv-cost).     
  END.
  ELSE vv-cost = b-po-ordl.cost.

  ASSIGN op-cost = vv-cost
         op-setup = vv-setup.

END PROCEDURE.

