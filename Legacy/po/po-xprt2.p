/* --------------------------------------------- po/po-xprt2.p */
/* PO Xprint form for Folding */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-ref1  for reftable.
def buffer b-ref2  for reftable.

{po/po-print.i}

DEF VAR v-wid AS DEC FORM ">>9.99<<" NO-UNDO.
DEF VAR v-dep AS DEC FORM ">>9.99<<" NO-UNDO.    
DEF VAR v-basis-w AS DEC NO-UNDO.
DEF VAR v-len AS DEC FORM "->,>>9.99" NO-UNDO.
DEF var v-wid2 like po-ordl.s-wid format ">>9.99<<" no-undo. /* for recalc extened cost */
def var v-len2 like po-ordl.s-len format ">>9.99<<" no-undo. /* for recalc extened cost */
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
DEF VAR lv-got-return AS int NO-UNDO.

DEF SHARED VAR s-group-notes AS LOG NO-UNDO.
{custom/formtext.i NEW}
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF var v-dept-note AS cha FORM "x(80)" EXTENT 50 NO-UNDO.
DEF var v-spec-note AS cha FORM "x(80)" EXTENT 50 NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image1 = "images\pacific1.bmp"
       ls-image2 = "images\pacific2.bmp"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">"
       FILE-INFO:FILE-NAME = ls-image2
       ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".



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
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.

DEF VAR v-qty LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR v-tot-sqft AS DEC NO-UNDO.
DEF VAR v-vend-item AS cha NO-UNDO.
def var v-adder AS cha FORM "x(15)" extent 5 no-undo.
def var v-num-add as int initial 0 no-undo.
DEF VAR v-job-no AS cha NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-setup AS DEC NO-UNDO.
DEF VAR v-tmp-lines AS dec NO-UNDO.
DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(40)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-flute LIKE ITEM.flute NO-UNDO.
DEF VAR lv-reg-no LIKE ITEM.reg-no NO-UNDO.
DEF VAR lv-item-rec AS cha NO-UNDO.
def var v-dec-fld as decimal no-undo.
DEF VAR lv-dep AS DEC NO-UNDO.
DEF VAR lv-dep2 AS DEC NO-UNDO.
DEFINE VARIABLE dCoreDia AS DECIMAL FORMAT ">,>>9.99<<" NO-UNDO.
DEFINE VARIABLE cFlueTest AS CHARACTER  NO-UNDO.


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

find first company where company.company eq cocode NO-LOCK. 
/*if avail company then
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
 */
 ASSIGN v-comp-add1 = ""
        v-comp-add2 = "" 
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = ""
        .
 IF lv-display-comp THEN DO:
    FIND FIRST cust WHERE cust.company = cocode AND
                       cust.active = "X" NO-LOCK NO-ERROR.
    IF AVAIL cust THEN
       ASSIGN v-comp-add1 = cust.addr[1]
           v-comp-add2 = cust.addr[2]
           v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
           v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
           v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
           lv-email    = "Email:  " + cust.email 
           lv-comp-name = cust.NAME   
           .
 END.

 v-tot-sqft = 0.
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
/*
      if v-pre-printed-forms eq yes then do:
        display po-ord.po-no v-page-counter po-ord.po-date v-po-type
                po-ord.po-change-date vend.name vend.add1 vend.add2 vend.city
                vend.state vend.zip v-sname v-saddr [1] v-saddr [2] v-scity
                v-sstate v-szip po-ord.buyer po-ord.contact
                terms.dscr when avail terms po-ord.acknowledge
                po-ord.fob-code carrier.dscr po-ord.frt-pay
                with frame po-head.

        assign v-line-number = 24.
      end.
      else
      do:
        if v-company eq yes then
        do:
          display v-change-ord company.name company.addr [1] company.addr [2] po-ord.po-no
                  v-page-counter company.city company.state company.zip
                  po-ord.po-date v-po-type po-ord.po-change-date vend.name
                  vend.add1 vend.add2 vend.city vend.state vend.zip v-sname
                  v-saddr [1] v-saddr [2] v-scity v-sstate v-szip
                  v-dash-line [1]
                  po-ord.buyer po-ord.contact
                  terms.dscr when avail terms
                  po-ord.acknowledge po-ord.fob-code
                  carrier.dscr v-freight-dscr v-dash-line [2] v-dash-line [3]
                  v-hdr with frame po-head-2.
          assign v-line-number = 26.
        end.
        else
        do:
          display v-change-ord po-ord.po-no v-page-counter
                  po-ord.po-date v-po-type po-ord.po-change-date vend.name
                  vend.add1 vend.add2 vend.city vend.state vend.zip v-sname
                  v-saddr [1] v-saddr [2] v-scity v-sstate v-szip
                  v-dash-line [1]
                  po-ord.buyer po-ord.contact
                  terms.dscr when avail terms
                  po-ord.acknowledge po-ord.fob-code
                  carrier.dscr v-freight-dscr v-dash-line [2] v-dash-line [3]
                  v-hdr with frame po-head-3.
          assign v-line-number = 26.
        end.
      end.
 =========*/
v-printline = 0.
{po/po-xprt2.i}

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
        V-ADDER = "".
        v-vend-item = "".

        find item where item.company eq po-ordl.company
                    and item.i-no    eq po-ordl.i-no
                    and po-ordl.item-type
                  no-lock no-error.
        v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                        +
                      (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "").
       
         if v-dec-fld = 0.08 then
           ASSIGN v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
                   v-wid = ( v-wid * 16 ) / 100
                   v-wid = truncate(po-ordl.s-wid,0) + v-wid
                   v-len = po-ordl.s-len - truncate(po-ordl.s-len,0)
                   v-len = ( v-len * 16 ) / 100
                   v-len = truncate(po-ordl.s-len,0) + v-len
                   v-wid2 = po-ordl.s-wid
                   v-len2 = po-ordl.s-len 
                   lv-dep = IF AVAIL ITEM AND ITEM.mat-type = "C" THEN item.case-d
                        ELSE IF AVAIL ITEM THEN ITEM.s-dep
                        ELSE 0
                   lv-dep2 = lv-dep
                   lv-dep = lv-dep2 - truncate(lv-dep2,0)
                   lv-dep = ( lv-dep * 16 ) / 100
                   lv-dep = TRUNCATE(lv-dep2,0) + lv-dep
                             .
               ELSE
                   ASSIGN v-wid = po-ordl.s-wid
                       v-len = po-ordl.s-len
                       v-wid2 = po-ordl.s-wid
                       v-len2 = po-ordl.s-len
                       lv-dep = IF AVAIL ITEM AND ITEM.mat-type = "C" THEN item.case-d
                        ELSE IF AVAIL ITEM THEN ITEM.s-dep
                        ELSE 0
                       lv-dep2 = lv-dep
                            .
                   
        if avail item and item.mat-type eq "B" then do:
          if v-shtsiz then do:
             if v-dec-fld = 0.08 then
            assign v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
                   v-wid = ( v-wid * 16 ) / 100
                   v-wid = truncate(po-ordl.s-wid,0) + v-wid
                   v-len = po-ordl.s-len - truncate(po-ordl.s-len,0)
                   v-len = ( v-len * 16 )  / 100
                   v-len = truncate(po-ordl.s-len,0) + v-len
                   lv-dep = lv-dep2 - truncate(lv-dep2,0)
                   lv-dep = ( lv-dep * 16 ) / 100
                   lv-dep = TRUNCATE(lv-dep2,0) + lv-dep  . 
            
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
       
        DO i = 1 TO 5:
           IF v-adder[i] <> "" AND LENGTH(v-adder[i]) < 15 THEN
              v-adder[i] = FILL(" ", 15 - LENGTH(v-adder[i])) + v-adder[i].
        END.

        IF v-printline + 4 > 46 THEN DO:         
           PAGE.
           v-printline = 0.
           {po/po-xprt2.i}
        END.
       
        PUT po-ordl.LINE FORM ">>9"
            po-ordl.ord-qty SPACE(2)
            po-ordl.pr-qty-uom SPACE(1)
            po-ordl.i-no FORM "x(20)" SPACE(1)
            v-adder[1] 
            v-job-no FORM "x(12)" SPACE(1)
            po-ordl.cost FORM "->>>9.99<<"
            po-ordl.pr-uom
            po-ordl.t-cost FORM "->>>,>>9.99"              
            SKIP.

        v-printline = v-printline + 1.

        PUT po-ordl.i-name AT 25 FORM "x(26)" SPACE(1) /*v-vend-item FORM "x(15)" space(1) */
            v-adder[2] SPACE(16)
            v-change-dscr  SKIP.
        v-printline = v-printline + 1.
        assign v-line-number = v-line-number + 3.
        
        if po-ordl.dscr[1] ne "" OR v-setup <> 0 OR v-adder[3] <> "" then do:
          put po-ordl.dscr[1] format "x(26)"  at 25 " "             
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

       ELSE IF v-itemDescription THEN /* fg item */ DO:
           IF po-ordl.dscr[2] NE '' THEN DO:
           PUT po-ordl.dscr[2] AT 25.
              ASSIGN
                v-line-number = v-line-number + 1
                v-printline = v-printline + 1.
           END.

       END.
       ELSE DO:
           IF po-ordl.vend-i-no NE '' THEN DO:
           PUT po-ordl.vend-i-no AT 25.
              ASSIGN
                v-line-number = v-line-number + 1
                v-printline = v-printline + 1.
           END.
       END.
    
        ASSIGN v-basis-w = 0
               v-dep     = 0.

       IF po-ordl.item-type THEN
          FIND FIRST ITEM WHERE ITEM.company EQ po-ord.company
                            AND ITEM.i-no    EQ po-ordl.i-no
                     NO-LOCK NO-ERROR.
       IF AVAIL ITEM THEN
          ASSIGN v-basis-w = item.basis-w
                 v-dep     = item.s-dep.

       IF po-ordl.pr-qty-uom EQ "MSF" THEN v-qty = po-ordl.ord-qty.
       ELSE RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                             v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                             po-ordl.ord-qty, OUTPUT v-qty).

       v-tot-sqft = v-tot-sqft + (v-qty * 1000).
       dCoreDia = 0.
       IF AVAIL ITEM THEN DO:
        IF ITEM.industry EQ "2" THEN
            ASSIGN dCoreDia =  IF item.mat-type EQ "P" THEN (item.ect / 10000) ELSE item.ect.
        ELSE dCoreDia =  IF item.mat-type NE "A" THEN (item.ect / 10000) ELSE item.ect.
       END.

       RUN calc-cost (recid(po-ordl),OUTPUT v-cost,OUTPUT v-setup).                    

       v-cost = po-ordl.cost - (ROUND(v-setup /  v-qty,4)). /* reclac cost from setup */
       IF AVAIL ITEM AND ITEM.mat-type NE "C" AND ITEM.flute NE "" THEN ASSIGN lv-flute = " Flute: " + ITEM.flute
                                  lv-reg-no = " Test: " + ITEM.reg-no.
       ELSE assign lv-flute = ""
                   lv-reg-no = "".
        ASSIGN cFlueTest = string(lv-flute,"x(11)") + string(lv-reg-no,"x(10)").
       IF lv-flute EQ "" AND lv-reg-no EQ "" THEN
              ASSIGN cFlueTest = IF dCoreDia GT 0 THEN " Core Dia: " + string(dCoreDia,">,>>9.99<<") ELSE ""
                     dCoreDia = 0.
       
       /* check whether i-no is roll */
       FIND FIRST job-hdr WHERE job-hdr.company = po-ordl.company
                            AND job-hdr.job-no = po-ordl.job-no
                            AND job-hdr.job-no2 = po-ordl.job-no2 NO-LOCK NO-ERROR.
       IF AVAIL job-hdr THEN 
          FIND FIRST ef WHERE ef.company = po-ordl.company
                          AND ef.est-no = job-hdr.est-no
                          AND ef.form-no = po-ordl.s-num NO-LOCK NO-ERROR.

       IF AVAIL ITEM AND INDEX("1,2,3,4,A,G,J,L,R,T,V,W,Z,9,M,I,O,X,Y,7,8",ITEM.mat-type) > 0 THEN  DO:
          /* PUT  STRING(v-cost,">>,>>9.99<<") + po-ordl.pr-uom + " $" + 
                STRING(v-setup) + "SETUP" FORM "x(25)"   AT 77
                SKIP.
           assign v-line-number = v-line-number + 1
                   v-printline = v-printline + 1.     
           */     
        END.
        ELSE IF AVAIL ITEM AND ITEM.mat-type = "C" THEN DO:
             if v-dec-fld = 0.08 then
                 PUT   "W: " at 25 v-wid FORM ">>>9.99"  space(1) " L:" v-len FORM "->,>>9.99"  
                  SPACE(1) " D:" lv-dep FORM ">>>9.99<<" SPACE(1)
                  cFlueTest FORMAT "x(20)"
               /* lv-flute FORM "x(13)"  lv-reg-no FORM "x(10)"*/
                STRING(v-cost,">>,>>9.99<<") + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)"   
                SKIP.
             ELSE
                 PUT   "W: " at 25 v-wid FORM ">>>9.99<<<"  space(1) " L:" v-len FORM "->,>>9.99<<<"  
                  SPACE(1) " D:" lv-dep FORM ">>>9.99<<" SPACE(1)
                  cFlueTest FORMAT "x(20)"
               /* lv-flute FORM "x(13)"  lv-reg-no FORM "x(10)"*/
                STRING(v-cost,">>,>>9.99<<") + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)"   
                SKIP.
             assign v-line-number = v-line-number + 1
                   v-printline = v-printline + 1.

        END.
        ELSE DO:
           IF AVAIL ITEM AND index("b,d,p",ITEM.mat-type) > 0 THEN ASSIGN lv-flute = ""
                                                                         lv-reg-no = "".
           
           IF (AVAIL ITEM AND ITEM.r-wid > 0) or
              (AVAIL ef AND ef.roll) THEN do:  /* no length for Roll*/
                if v-dec-fld = 0.08 then
              PUT    "W:   " at 25 v-wid FORM ">>>9.99"  space(10) 
                    /* lv-flute FORM "x(13)" lv-reg-no FORM "x(10)"*/
                    cFlueTest FORMAT "x(27)"
                STRING(v-cost,">>,>>9.99<<") + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)"   
                SKIP.
                ELSE
                PUT    "W:   " at 25 v-wid FORM ">>>9.99<<<"  space(10) 
                    /* lv-flute FORM "x(13)" lv-reg-no FORM "x(10)"*/
                    cFlueTest FORMAT "x(27)"
                STRING(v-cost,">>,>>9.99<<") + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)"   
                SKIP.
           END.

           ELSE do:
               if v-dec-fld = 0.08 then
                PUT    "W: " at 25 v-wid FORM ">>>9.99" space(1) "L: " v-len FORM "->,>>9.99"
                 /*"                   "*/
            /*  /*  "  Flute:"*/  lv-flute FORM "x(13)" /*"Test:" */ lv-reg-no FORM "x(10)"*/
                   cFlueTest FORMAT "x(27)"
                STRING(v-cost,">>,>>9.99<<") + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)"   
                SKIP
               /* space(2) v-vend-item FORM "x(20)" */  .
               ELSE
                   PUT    "W: " at 25 v-wid FORM ">>>9.99<<<" space(1) "L: " v-len FORM "->,>>9.99<<<"
                 /*"                   "*/
             /* /*  "  Flute:"*/  lv-flute FORM "x(13)" /*"Test:" */ lv-reg-no FORM "x(10)"*/
                       cFlueTest FORMAT "x(27)"
                STRING(v-cost,">>,>>9.99<<") + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)"   
                SKIP
               /* space(2) v-vend-item FORM "x(20)" */  .
          END.
        
          assign v-line-number = v-line-number + 1
                 v-printline = v-printline + 1.
       END.

        IF dCoreDia GT 0 THEN DO:
            put "Core Dia: " AT 25 dCoreDia FORMAT ">,>>9.9<<<" SKIP.
            ASSIGN
                v-line-number = v-line-number + 1
                v-printline = v-printline + 1.
        END.
        ELSE
           PUT SKIP.

        len-score = "".   
        run po/po-ordls.p (recid(po-ordl)).
        {po/poprints.i}       
            IF AVAIL ITEM AND lookup("1,2,3,4",ITEM.mat-type) > 0 THEN DO: 
            END.
            ELSE DO:
         /*      if not v-test-scr then do:
                  put 
                      "Score: " AT 3
                      len-score format "x(80)" SKIP .
                      
                  v-line-number = v-line-number + 1.
                  v-printline = v-printline + 1.
               end.
          
               else
               if dec(trim(len-score)) ne v-wid then do:
                  put "Score: " AT 3
                      len-score format "x(80)"  SKIP.
                      
                  v-line-number = v-line-number + 1.
                  v-printline = v-printline + 1.                  
               END.
            */   
            END.
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
      ASSIGN
       tt-line-no = li
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
       {po/po-xprnt.i}
    END.
    DO i = 1 TO li:
         PUT v-dept-note[i] SKIP.
         v-printline = v-printline + 1.
         IF v-printline > 46 THEN DO:                  
            PAGE.
            v-printline = 0.
            {po/po-xprt2.i}
         END.
    END.

    PUT skip(1).
    assign v-line-number = v-line-number + 1.
    v-printline = v-printline + 1.
  
     IF v-printline > 46 THEN DO:
          PAGE.
          v-printline = 0.
          {po/po-xprt2.i}
     END.
          /* === spec note print */
  
     IF v-print-sn THEN DO:
        ASSIGN v-tmp-lines = 0
               v-inst-lines = 0
               lv-item-rec = "".

        FOR EACH tt-formtext:
            DELETE tt-formtext.
        END.
        lv-text = "".

        FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                      ELSE IF AVAIL itemfg THEN itemfg.rec_key
                      ELSE "".
        IF lv-item-rec <> "" THEN 
        FOR EACH notes WHERE notes.rec_key = lv-item-rec 
               AND /*notes.note_type = "S" */  notes.note_code = "PO" NO-LOCK:
              IF notes.note_text <> "" THEN 
                 lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
        END.
               
        DO li = 1 TO 20:
           CREATE tt-formtext.
           ASSIGN tt-line-no = li
                  tt-length  = 80.
        END.
        RUN custom/formtext.p (lv-text).
        i = 0.
        v-spec-note = "".
        FOR EACH tt-formtext:
            i = i + 1.
            IF  i <= 20 THEN v-spec-note[i] = tt-formtext.tt-text.      
        END.
        li = 0.
        DO i = 20 TO 1 BY -1:
           li = i.
           IF v-spec-note[i] <> "" THEN LEAVE.
        END.
        IF s-group-notes AND v-printline + li > 46 THEN DO:
           PAGE.
           v-printline = 0.
           {po/po-xprt2.i}
        END.
        DO i = 1 TO li:
             PUT v-spec-note[i] SKIP.
             v-printline = v-printline + 1.
             IF v-printline > 46 THEN DO:                  
                PAGE.
                v-printline = 0.
                {po/po-xprt2.i}
             END.
        END.
    /*
        FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                      ELSE IF AVAIL itemfg THEN itemfg.rec_key
                      ELSE "".
        IF lv-item-rec <> "" THEN DO:
           FOR EACH notes WHERE notes.rec_key = lv-item-rec 
               AND /*notes.note_type = "S" */  notes.note_code = "PO" NO-LOCK:
              IF notes.note_text <> "" THEN DO:
                 v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                v-inst-lines = v-inst-lines + v-tmp-lines. 
              END.
           END.
           if v-inst-lines gt 0 then v-inst-lines = v-inst-lines + 1.
           v-printline = v-printline + v-inst-lines.
           IF v-printline > 46 THEN DO:         
              PAGE.
              v-printline = 0.
              {po/po-xprt2.i}
           END.     
    
           FOR EACH notes WHERE notes.rec_key = lv-item-rec AND 
               /*notes.note_type = "S" */  notes.note_code = "PO"  NO-LOCK:
               v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
               {SYS/INC/ROUNDUP.I v-tmp-lines}
               IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                  PUT {1} substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)"  SKIP.              
              /*    v-printline = v-printline + 1. */
               END.
           end.
        END. /* lv-item-spec <> "" */
        */
     END.
     /* === end of specnote print */

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

              IF k < 5 THEN v-inst[k] = v-inst[k] + SUBSTRING(notes.note_text,i,1).              

              IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
              THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
              END.

           END.
  end.
/*
  IF v-printline > 46 THEN DO:                  
     PAGE.
     v-printline = 0.
     {po/po-xprt2.i}
  END.
*/
  /*v-printline 46*/

      PUT "Grand Total MSF: " +
          TRIM(STRING(v-tot-sqft / 1000,">>>,>>9.9<<")) AT 50 FORMAT "x(30)"
          SKIP.

      v-tot-sqft = 0.
      v-bot-lab[1] = "Tax        :"
                     /*vend.tax-gr + "        :       " */ + STRING(po-ord.tax,"->>>,>>9.99").

      PUT "<R53><C1>" v-inst[1] 
          "<R54><C1>" v-inst[2]
          "<R55><C1>" v-inst[3]
          "<R56><C1>" v-inst[4]
          "<R58><C60><#8><FROM><R+5><C+21><RECT> " 
    "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "->>>,>>9.99"
    "<=8><R+2> "  v-bot-lab[1] 
    "<=8><R+3> "  " " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
                /*v-bot-lab[2] */
    "<=8><R+4> Grand Total:" po-ord.t-cost FORM "->>>,>>9.99" .

PUT "<FArial><R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
     " " SKIP
     " " SKIP
     " " SKIP(1)     
     "  I acknowledge the pricing on this P.O. is correct. _________________________(please sign and fax)" SKIP
     .

v-printline = v-printline + 6.

IF v-printline < 60 THEN PUT SKIP(80 - v-printline).

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
 DEF VAR lv-added-cons-cost AS DEC NO-UNDO.
 DEF VAR lv-adder-setup AS DEC NO-UNDO.
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
                             IF AVAIL e-item THEN e-item.std-uom ELSE "", vv-basis-w,
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

     IF AVAIL e-item AND e-item.std-uom NE b-po-ordl.pr-uom THEN
         RUN sys/ref/convcuom.p(e-item.std-uom,
                                b-po-ordl.pr-uom, vv-basis-w,
                                v-len, v-wid, vv-dep,
                                vv-cost, OUTPUT vv-cost).     
  END.
  ELSE vv-cost = b-po-ordl.cost.

  /* for adders */
  FIND first job-mat where job-mat.company eq po-ordl.company
                         and job-mat.job-no  eq po-ordl.job-no
                         and job-mat.job-no2 eq po-ordl.job-no2
                         AND job-mat.frm = po-ordl.s-num
                         AND job-mat.blank-no = po-ordl.b-num
                         NO-LOCK NO-ERROR.
  IF AVAIL job-mat THEN
     run po/po-adder2.p (recid(po-ordl), recid(job-mat),OUTPUT vv-cost, OUTPUT lv-added-cons-cost,OUTPUT lv-adder-setup).

  ASSIGN op-cost = vv-cost
         op-setup = vv-setup + lv-adder-setup.

END PROCEDURE.

