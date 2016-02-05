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
/*==
/* for po/po-sysct.p */
def  NEW shared var factor# as decimal no-undo.
def  NEW shared var v-default-gl-log as log no-undo.
def  NEW shared var v-default-gl-cha as cha no-undo.
def  NEW shared var v-po-qty as log initial true no-undo.
def  NEW shared var v-po-msf like sys-ctrl.int-fld no-undo.
====*/

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(60)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(60)" NO-UNDO.
ASSIGN ls-image1 = "images\elitelogo.jpg"
       ls-image2 = "images\pacific2.bmp".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
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
DEF VAR v-setup AS DEC NO-UNDO.
DEF VAR v-tax-tot AS DEC NO-UNDO.
DEF VAR v-tax-rate AS DEC NO-UNDO.
DEF VAR v-frt-tax-rate AS DEC NO-UNDO.
DEF VAR vt-cost LIKE po-ordl.t-cost NO-UNDO.
DEF VAR v-ord-qty LIKE po-ordl.ord-qty NO-UNDO.

DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR lv-item-rec AS cha NO-UNDO.
{custom/formtext.i NEW}
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF var v-dept-note AS cha FORM "x(80)" EXTENT 50 NO-UNDO.
DEF VAR lv-text-line AS INT NO-UNDO.
DEF VAR lv-text-line-length AS INT NO-UNDO.
DEF VAR lv-char AS cha NO-UNDO.
DEF VAR lv-char-list AS cha NO-UNDO.
DEF TEMP-TABLE tt-text 
         FIELD TYPE AS cha
         FIELD tt-line AS INT
         FIELD tt-text AS cha 
         FIELD tt-recid AS RECID
         INDEX tt-text IS PRIMARY TYPE tt-line.

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
 v-comp-add3 = "Phone: 404.344.3699" 
 v-comp-add4 = "Fax     : 404.344.3422".
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
      {po/po-elite.i}  /* xprint form */

      ASSIGN v-sub-cost = 0
             v-tot-cost = 0
             v-tax-tot  = 0
             v-setup    = 0.

      for each po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no by po-ordl.line:
        assign xg-flag = no.
        if not v-printde-po and po-ordl.deleted then next.
        
        assign v-change-dscr = ""
               v-adder = "".
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
        ASSIGN
        v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,">>")
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

        ASSIGN
        v-setup = v-setup + po-ordl.setup
        v-cost = po-ordl.cost.

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

        vt-cost = vt-cost + po-ordl.setup.

/* end of t-cost calc */
        ASSIGN v-sub-cost = v-sub-cost + vt-cost
               v-tot-cost = v-tot-cost + vt-cost.

        /* calc tax without setup */
        IF po-ordl.tax THEN
          v-tax-tot = v-tax-tot + ROUND((vt-cost * v-tax-rate / 100),2).

        vt-cost = vt-cost - po-ordl.setup.
        
        IF v-job-no = "-" THEN v-job-no = "".

        DO i = 1 TO 5:
           IF v-adder[i] <> "" AND LENGTH(v-adder[i]) < 15 THEN
              v-adder[i] = FILL(" ", 15 - LENGTH(v-adder[i])) + v-adder[i].
        END.

    IF v-printline > 46 THEN DO:         
        PAGE.
        v-printline = 0.
        {po/po-elite.i} 
    END.

        PUT po-ordl.LINE FORM ">>9"
            po-ordl.ord-qty SPACE(2)
            po-ordl.pr-qty-uom SPACE(1)
            po-ordl.i-no FORM "x(23)" SPACE(1)
            v-adder[1] 
            v-job-no FORM "x(9)" SPACE(1)
            v-cost  /*po-ordl.cost*/ FORM "->>>9.99<<"
            po-ordl.pr-uom
            vt-cost /* po--ordl.t-cost */ FORM "->>,>>9.99"             
            SKIP.

        v-vend-item = po-ordl.vend-i-no.

        PUT po-ordl.i-name AT 25 FORM "x(30)" SPACE(1) /*v-vend-item FORM "x(15)" space(1) */
            v-adder[2] SPACE(12)
            v-change-dscr  SKIP.
        v-printline = v-printline + 2.
        assign v-line-number = v-line-number + 3.
        
        if po-ordl.dscr[1] ne "" OR v-adder[3] <> "" then do:
          put po-ordl.dscr[1] format "x(30)" at 25 " "             
              v-adder[3] 
              skip.
          v-line-number = v-line-number + 1.
          v-printline = v-printline + 1.
        end.
            
        if po-ordl.dscr[2] ne "" OR v-adder[4] <> "" then do:
          put po-ordl.dscr[2] format "x(30)" at 25              
              " " v-adder[4] skip.
          v-line-number = v-line-number + 1.
          v-printline = v-printline + 1.
        end.
        IF v-adder[5] <> "" OR v-vend-item <> "" THEN DO:
            put v-vend-item  FORM "x(30)" AT 25              
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
 
     {custom/notesprt.i po-ordl v-inst 4}
     DO i = 1 TO 4:
       IF v-inst[i] <> "" THEN DO:
          IF v-printline gt 46 then DO:
             page .
             v-printline = 0.
             {po/po-elite.i}
          END.
          PUT v-inst[i] SKIP.
          v-printline = v-printline + 1.
       END.
     END.
     IF v-printline > 46 THEN DO:                  
        PAGE.
        v-printline = 0.
        {po/po-elite.i}
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
              {po/po-elite.i}
            END.     
            PUT tt-text.tt-text FORM "x(80)"  SKIP.
                v-printline = v-printline + 1.
        END.
     END.  /* v-print-sn */

  end. /* for each po-ordl record */
  
  {custom/notesprt.i po-ord v-inst 4}
  IF v-printline > 46 THEN DO:                  
     PAGE.
     v-printline = 0.
     {po/po-elite.i}
  END.

  /*v-printline 46*/
      v-tax-tot = ROUND(v-tax-tot,2).
      v-tot-cost = v-sub-cost + v-tax-tot.

      PUT "Total Sq. Ft: "  AT 50 v-tot-sqft FORM ">>>,>>9" SKIP.

      IF v-setup NE 0 THEN
        PUT "Setup Charges: " AT 50 v-setup FORM "->>,>>9.99" TO 95 SKIP.

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
              v-tot-cost = 0.

    end. /* for each po-ord record */.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */



