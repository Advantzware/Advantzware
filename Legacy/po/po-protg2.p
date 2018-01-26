/* --------------------------------------------- po/po-protg2.p */
/* Purchase Order Print Program for S-8-POPRINT = PremierX                        */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-ref1  for reftable.
def buffer b-ref2  for reftable.

{po/po-print.i}

DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.  
DEF VAR vc-wid AS CHAR NO-UNDO INIT "".
DEF VAR vc-len AS CHAR NO-UNDO INIT "".
DEF VAR v-basis-w AS DEC NO-UNDO.
DEF VAR v-len AS DEC FORM "->,>>9.99" NO-UNDO.
DEF var v-wid2 like po-ordl.s-wid format ">>9.99" no-undo. /* for recalc extened cost */
def var v-len2 like po-ordl.s-len format ">>9.99" no-undo. /* for recalc extened cost */
def var pol-counter as int no-undo.
def var v-exp-limit as int no-undo init 10.
def var v-line-number as INT NO-UNDO.
def var v-page-counter as int format ">>9".
def var v-sname like shipto.ship-name NO-UNDO.
def var v-saddr like shipto.ship-addr NO-UNDO.
def var v-scity like shipto.ship-city NO-UNDO.
def var v-sstate like shipto.ship-state NO-UNDO.
def var v-szip like shipto.ship-zip NO-UNDO.
def var v-po-type as char format "x(10)" NO-UNDO.
def var v-freight-dscr as char format "x(7)" NO-UNDO.
def var v-change-dscr as char format "x(7)" NO-UNDO.
def var v-dash-line as char format "x(80)" extent 3 NO-UNDO.
def var v-adders as LOG NO-UNDO.
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
DEF VAR lv-got-return AS int NO-UNDO.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.
DEF VAR v-signature AS cha NO-UNDO.
DEF VAR v-sig-image AS cha NO-UNDO.
DEF VAR v-username AS cha NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR lv-text-line AS INT NO-UNDO.
DEF VAR lv-text-line-length AS INT NO-UNDO.
DEF VAR lv-char AS cha NO-UNDO.
DEF VAR lv-char-list AS cha NO-UNDO.
DEF VAR v-curr-dscr AS CHAR NO-UNDO.
DEF TEMP-TABLE tt-text NO-UNDO
    FIELD TYPE AS cha
    FIELD tt-line AS INT
    FIELD tt-text AS cha 
    FIELD tt-recid AS RECID
    INDEX tt-text IS PRIMARY TYPE tt-line.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(60)" NO-UNDO.

ASSIGN ls-image1 = "images\Protagonpo.jpg"
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
def var v-bot-lab as char format "x(63)" extent 3 NO-UNDO.
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
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-flute LIKE ITEM.flute NO-UNDO.
DEF VAR lv-reg-no LIKE ITEM.reg-no NO-UNDO.
DEF VAR lv-item-rec AS cha NO-UNDO.
DEF VAR v-tot-msf AS DEC FORM ">>>>,>>9.999" NO-UNDO.
DEF VAR v-out-qty AS DEC NO-UNDO.
DEFINE VAR v-ord-no AS CHAR NO-UNDO.
DEF VAR v-vend-no AS CHAR NO-UNDO.

{custom/formtext.i NEW}

DEF VAR v-lstloc AS CHAR FORM "x(20)" NO-UNDO.

v-dash-line = fill ("_",80).

{po/po-print.f}
{ce/msfcalc.i}

assign v-hdr = "VEND ITEM".
       
       
find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "POPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company eq cocode NO-LOCK. 

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
           lv-comp-name = cust.NAME.
 END.

 v-tot-sqft = 0.
    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
        BREAK BY PO-ORD.PO-NO:

        assign v-sname     = po-ord.ship-name
               v-saddr[1]  = po-ord.ship-addr[1]
               v-saddr[2]  = po-ord.ship-addr[2]
               v-scity     = po-ord.ship-city
               v-sstate    = po-ord.ship-state
               v-szip      = po-ord.ship-zip.

      {po/exportpo.i}

      assign v-page-counter  = 1.

      if po-ord.stat eq "N" then
         po-ord.stat = "O".

      find first vend where vend.company eq po-ord.company 
                        and vend.vend-no eq po-ord.vend-no no-lock no-error.
      find first terms where terms.t-code eq po-ord.terms no-lock no-error.
      find first carrier where carrier.company eq po-ord.company 
                           and carrier.carrier eq po-ord.carrier no-lock no-error.

      FIND FIRST currency WHERE currency.company EQ cocode
                            AND currency.c-code EQ vend.curr-code NO-LOCK NO-ERROR .
      IF AVAIL currency THEN 
          ASSIGN v-curr-dscr = currency.c-desc .

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
   {po/po-protg2.i}

      for each po-ordl WHERE
           po-ordl.company EQ po-ord.company AND
           po-ordl.po-no EQ po-ord.po-no by po-ordl.line:
        assign xg-flag = no.
        if not v-printde-po and po-ordl.deleted then next.
        assign v-change-dscr = "".

        if po-ordl.stat eq "A" THEN ASSIGN v-change-dscr = "Added".
        else if po-ordl.stat eq "U" THEN assign v-change-dscr = "Updated".
        else if po-ordl.stat eq "O" THEN assign v-change-dscr = "Open ".
        else if po-ordl.stat eq "P" then assign v-change-dscr = "Partial".
        else if po-ordl.stat eq "C" then assign v-change-dscr = "Closed ".

        if po-ordl.deleted eq yes then   assign v-change-dscr = "Deleted".
        assign
           v-ino-job = po-ordl.vend-i-no
           V-ADDER = ""
           v-vend-item = "".

        FIND FIRST e-item
            WHERE e-item.company EQ cocode
            AND e-item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.

       IF AVAIL e-item THEN
           FIND FIRST e-item-vend OF e-item
           WHERE e-item-vend.vend-no EQ po-ord.vend-no
           NO-LOCK NO-ERROR.

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
        v-wid2 = po-ordl.s-wid
        v-len2 = po-ordl.s-len.

        if avail item and item.mat-type eq "B" then do:
          if v-shtsiz then do:
            ASSIGN
            v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
            v-wid = ( v-wid * 16 ) / 100
            v-wid = truncate(po-ordl.s-wid,0) + v-wid
            v-len = po-ordl.s-len - truncate(po-ordl.s-len,0)
            v-len = ( v-len * 16 ) / 100
            v-len = truncate(po-ordl.s-len,0) + v-len
            v-num-add = 0.

        /*IF v-adder[2] <> "" OR po-ordl.vend-i-no <> "" THEN DO:
                ASSIGN v-vend-no = po-ordl.vend-i-no.  
            ELSE  
                v-vend-no = ITEM.ven-item.
        END.*/

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
                    and (job-mat.frm      eq po-ordl.s-num OR
                         po-ordl.s-num EQ ?)
                  use-index job no-lock
                  break by job-mat.blank-no desc:
                if last(job-mat.blank-no)            or
                   job-mat.blank-no eq po-ordl.b-num then leave.
              end.

              if avail job-mat and 
                job-mat.i-no     eq po-ordl.i-no  /* gmd - 06190903*/
               then do:
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
                  /*if avail xitem then
                  do:
                      assign v-num-add = v-num-add + 1.
                      if v-num-add eq 1 THEN assign v-adder[1] = xitem.i-name.
                      else if v-num-add eq 2 THEN assign v-adder[2] = xitem.i-name.
                      else if v-num-add eq 3 THEN assign v-adder[3] = xitem.i-name.
                      else if v-num-add eq 4 THEN assign v-adder[4] = xitem.i-name.
                      else if v-num-add eq 5 THEN assign v-adder[5] = xitem.i-name.
                  end.*/

                end.
                
                find first ef where EF.COMPANY EQ JOB.COMPANY
                                AND ef.est-no  EQ job.est-no
                                and ef.form-no eq job-mat.frm
                              no-lock no-error.

                if avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B") THEN ASSIGN xg-flag = yes.
                IF AVAIL ef THEN do:


                    ASSIGN v-adder[1] = ef.adder[7].


                    FIND FIRST ITEM WHERE ITEM.company = ef.company AND
                                         ITEM.i-no = ef.adder[1] NO-LOCK NO-ERROR.

                    IF AVAIL ITEM /*AND ITEM.vend-item <> ""*/ THEN DO:

                        FIND FIRST e-item
                            WHERE e-item.company EQ cocode
                            AND e-item.i-no    EQ ITEM.i-no
                            NO-LOCK NO-ERROR.

                       IF AVAIL e-item THEN
                           FIND FIRST e-item-vend OF e-item
                           WHERE e-item-vend.vend-no EQ po-ord.vend-no
                           NO-LOCK NO-ERROR.

                        ASSIGN v-vend-no = (IF AVAIL e-item-vend THEN e-item-vend.vend-item ELSE "").
                    END.
                        
                END.
              end. /* avail job-mat */
            end. /* avail job */
          end. /* v-shtsiz */        
          
        end. /* avail item and item.mat-type eq "B" */
       
        v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99") +
                   (IF po-ordl.s-num NE ? THEN "-" + string(po-ordl.s-num,"99")
                    ELSE "").

        IF po-ordl.job-no = "" THEN v-job-no = "".

        IF v-job-no = "-" THEN v-job-no = "".
       
        DO i = 1 TO 5:
           IF v-adder[i] <> "" AND LENGTH(v-adder[i]) < 15 THEN
              v-adder[i] = FILL(" ", 15 - LENGTH(v-adder[i])) + v-adder[i].
        END.
        IF v-vend-no EQ "" THEN
            ASSIGN v-vend-no =  po-ordl.vend-i-no.
             
        IF v-printline > 48 THEN DO:         
           PAGE.
           v-printline = 0.
           {po/po-protg2.i} 
        END.

           /* stacey */
/*            MESSAGE po-ordl.dscr[1] SKIP            */
/*                    po-ordl.dscr[2]                 */
/*                VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
           

       /* IF v-adder[1] = "" THEN*/
        PUT po-ordl.LINE FORM ">>9"
            po-ordl.ord-qty SPACE(2)
            po-ordl.pr-qty-uom SPACE(1)
            po-ordl.i-name FORMAT "x(30)"
            SPACE(5)
            v-job-no FORM "x(12)"  /*AT 75*/ SPACE(1)
            po-ordl.cost FORM "->>>9.99<<" SPACE(1)
            po-ordl.pr-uom FORM "x(3)"
            po-ordl.t-cost FORM "->>>>,>>9.99"  
            SKIP.
       /* ELSE
            PUT po-ordl.LINE FORM ">>9"
                po-ordl.ord-qty SPACE(2)
                po-ordl.pr-qty-uom SPACE(1)
                po-ordl.i-name FORMAT "x(20)"
                SPACE(1)
                v-adder[1] SPACE(1)
                v-job-no FORM "x(12)" /*SPACE(1)*/
                po-ordl.cost FORM "->>>9.99<<" SPACE(1)
                po-ordl.pr-uom
                po-ordl.t-cost FORM "->>,>>9.99"  
                SKIP.*/

           
        v-printline = v-printline + 1.
        
        /* gdm - 05210907 */
        IF po-ordl.pr-qty-uom EQ "EA" THEN DO:
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                                 v-basis-w,po-ordl.s-len,po-ordl.s-wid,v-dep,
                                 po-ordl.ord-qty, OUTPUT v-qty).
             
          ASSIGN v-tot-msf = v-qty.
        END.
        ELSE DO:
          ASSIGN v-tot-msf = 0.
          IF po-ordl.pr-qty-uom NE "EA" THEN DO:
            ASSIGN v-tot-msf = 0.
            RUN sys/ref/convquom.p(po-ordl.pr-qty-uom:SCREEN-VALUE,
                                   "MSF", v-basis-w, v-len, v-wid,v-dep,
                                   po-ordl.ord-qty, OUTPUT v-out-qty).
            ASSIGN v-tot-msf = v-tot-msf.
          END.
        END.

        /* gdm - 05210907 end */

        /* gdm - 11040905 */
        ASSIGN v-lstloc = "".
        FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ po-ordl.company
            AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.
        IF AVAIL itemfg THEN
          FIND LAST fg-rcpth NO-LOCK
            WHERE fg-rcpth.company   EQ itemfg.company 
              AND fg-rcpth.i-no      EQ itemfg.i-no
              AND fg-rcpth.rita-code EQ "R" NO-ERROR.
          IF AVAIL fg-rcpth THEN 
            FIND LAST ASI.fg-rdtlh NO-LOCK
              WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-ERROR.
            IF AVAIL fg-rdtlh THEN ASSIGN v-lstloc = fg-rdtlh.loc + "/" +
                                                     TRIM(fg-rdtlh.loc-bin).   
            IF AVAIL itemfg THEN 
               ASSIGN v-ord-no = string(po-ordl.ord-no) .
             ELSE
                v-ord-no = "".
        
/*         IF TRIM(v-lstloc) NE "" THEN do:      */
/*           PUT "WHS/BIN" AT 7.                 */
/*            ASSIGN                             */
/*             v-line-number = v-line-number + 1 */
/*             v-printline = v-printline + 1.    */
/*         END.                                  */
            
        /* gdm - 11040905 end */
       
         /*IF v-adder[2] <> "" OR po-ordl.vend-i-no <> "" THEN DO:
            put po-ordl.vend-i-no  FORM "x(30)" AT 25              
                " " v-adder[2] skip.
            ASSIGN
            v-line-number = v-line-number + 1
            v-printline = v-printline + 1.
        END.*/


        /* output v-vend-no */
        IF v-vend-no <> "" OR v-vend-no <> "" THEN do:
        PUT v-vend-no FORM "x(30)" AT 25
            v-ord-no AT 61
            SPACE(2)
            /*v-change-dscr*/ SKIP.
        ASSIGN
            v-line-number = v-line-number + 1
            v-printline = v-printline + 1.
        END.

/*         /* gdm - 11040905 */               */
/*         IF TRIM(v-lstloc) NE "" THEN       */
/*            PUT v-lstloc AT 7 FORM "x(15)". */

        PUT po-ordl.dscr[1] AT 25  FORM "x(30)" SKIP
            po-ordl.dscr[2] AT 25  FORM "x(30)".
            /*v-adder[2] FORM "x(8)" SPACE(1)*/
            
        ASSIGN
           v-printline = v-printline + 1
           v-line-number = v-line-number + 3.
        
        if po-ordl.dscr[1] ne "" OR v-setup <> 0 /*OR v-adder[3] <> ""*/ OR  po-ordl.due-date <> ? then do:
          PUT /* v-adder[3] FORM "x(8)"  AT 56  SPACE(1)*/
              po-ordl.due-date  AT 61
              SKIP.
              
          ASSIGN
          v-line-number = v-line-number + 1
          v-printline = v-printline + 1.
        end.
    
        /*if po-ordl.dscr[2] ne "" OR v-adder[4] <> "" then do:
          put " " v-adder[4] AT 56 skip.
          ASSIGN
          v-line-number = v-line-number + 1
          v-printline = v-printline + 1.
        end.*/
        
       
        /* calc total sq feet */
    
        ASSIGN v-basis-w = 0
               v-dep     = 0
               v-vend-no = " ".

       RELEASE ITEM.

       IF po-ordl.item-type THEN
          FIND FIRST ITEM WHERE
               ITEM.company EQ po-ord.company AND
               ITEM.i-no    EQ po-ordl.i-no
               NO-LOCK NO-ERROR.

       IF AVAIL ITEM THEN
          ASSIGN v-basis-w = item.basis-w
                 v-dep     = item.s-dep.

       IF po-ordl.pr-qty-uom EQ "MSF" THEN
          v-qty = po-ordl.ord-qty.
       ELSE
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                                 v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                                 po-ordl.ord-qty, OUTPUT v-qty).

       v-tot-sqft = v-tot-sqft + (v-qty * 1000).
       /*RUN calc-cost(recid(po-ordl),OUTPUT v-cost,OUTPUT v-setup).*/
          
       IF v-printline > 48 THEN DO:                  
        PAGE.
        v-printline = 0.
        {po/po-protg2.i}
       END.

       ASSIGN
          v-cost = po-ordl.cost /*- (ROUND(v-setup / v-qty,4))*/ . /* reclac cost from setup */
          v-setup = po-ordl.setup.

       /* If item material type is B or S, then do this logic. */
       IF AVAIL ITEM AND lookup(ITEM.mat-type,"B,S") GT 0 THEN
       DO:
          ASSIGN lv-flute = "  Flute: " + ITEM.flute
                 lv-reg-no = "Test: " + ITEM.reg-no.

          /* Convert decimal to fraction. */
          RUN sys\inc\decfrac2.p(INPUT po-ordl.s-wid, INPUT 32, OUTPUT vc-wid).
          RUN sys\inc\decfrac2.p(INPUT po-ordl.s-len, INPUT 32, OUTPUT vc-len).

          /* Display width and length */
          PUT "W: " at 25 vc-wid space(2) "L: " vc-len.

          /* If material type not B, then display flute and test. */
          IF ITEM.mat-type <> "B" THEN
              PUT lv-flute FORM "x(13)" lv-reg-no FORM "x(10)".

          /* Display cost and setup. */
          PUT STRING(v-cost,"->>>,>>9.99<<") + po-ordl.pr-uom + " $" +
              STRING(v-setup) + "SETUP" FORM "x(25)"   
              SKIP.
              
       END. /* END: If item material type is B or S, then do this logic. */
       ELSE
       DO:
          assign lv-flute = ""
                 lv-reg-no = "".

          put SPACE(74)
              STRING(v-cost,"->>>,>>9.99<<") + po-ordl.pr-uom + " $" +
              STRING(v-setup) + "SETUP" FORM "x(25)"   
              SKIP.
         END.

       assign
          v-line-number = v-line-number + 1
          v-printline = v-printline + 1
          len-score = "".

       run po/po-ordls.p (recid(po-ordl)).
       {po/poprints.i}       
       IF AVAIL ITEM AND lookup(ITEM.mat-type,"1,2,3,4") > 0 THEN DO: 
       END.
       ELSE DO:
          if not v-test-scr then do:
             IF po-ordl.spare-char-1 = "LENGTH" THEN
             put 
                 "HOR Score: " AT 3
                 len-score format "x(80)" SKIP .
             ELSE
                 put 
                     "Score: " AT 3
                     len-score format "x(80)" SKIP .
                 
             ASSIGN
             v-line-number = v-line-number + 1
             v-printline = v-printline + 1.
          end.
       
          else
          if dec(trim(len-score)) ne v-wid then do:
              IF po-ordl.spare-char-1 = "LENGTH" THEN
                  put "HOR Score: " AT 3
                      len-score format "x(80)"  SKIP.
              ELSE
                  put "Score: " AT 3
                      len-score format "x(80)"  SKIP.
                 
             ASSIGN
             v-line-number = v-line-number + 1
             v-printline = v-printline + 1.
          END.
       END.
         end.
         END.
       end.
  
    ASSIGN v-inst = ""
           v-tmp-lines = 0
           j = 0
           K = 0
           lv-got-return = 0.
  
    FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
       DO i = 1 TO LENGTH(notes.note_text) :        
           IF i - j >= 80 THEN ASSIGN j = i
                                      lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / 80.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return.

           IF k < 7 THEN v-inst[k] = v-inst[k] +
                                     IF SUBSTRING(trim(notes.note_text),i,1) <> CHR(10) THEN SUBSTRING(trim(notes.note_text),i,1)
                                     ELSE "" .              
           
           IF SUBSTRING(trim(note_text),i,1) = CHR(10) OR SUBSTRING(trim(note_text),i,1) = CHR(13)                 
           THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
           END.
        END.
        PUT SKIP(1).
        v-printline = v-printline + 1.
     END.
     
     
    /*v-printline = v-printline + k.*/
     
    IF v-printline > 48 THEN DO:                  
       PAGE.
       v-printline = 0.
       {po/po-protg2.i}
    END.
    
    DO i = 1 TO 6:
       IF v-inst[i] <> "" THEN DO:
          PUT "<C2>" v-inst[i]  SKIP.         
          v-printline = v-printline + 1.
       END.
    END.    
    
   assign
       v-line-number = v-line-number + 1
       v-printline = v-printline + 1.
     IF v-printline > 48 THEN DO:
        PAGE.
        v-printline = 0.
        {po/po-protg2.i}
     END.
         
     /* === spec note print */
     IF v-print-sn THEN DO:
        IF po-ordl.item-type THEN
           FIND FIRST ITEM WHERE ITEM.company EQ po-ord.company
                             AND ITEM.i-no    EQ po-ordl.i-no NO-LOCK NO-ERROR.
        ELSE FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                                 AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        ASSIGN
        lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                      ELSE IF AVAIL itemfg THEN itemfg.rec_key
                      ELSE ""
        lv-text = ""
        lv-text-line = 0
        lv-text-line-length = 0
        lv-char = ""
        lv-char-list = "".
       
        FOR EACH notes FIELDS(note_text) WHERE
            notes.rec_key = lv-item-rec AND 
            notes.note_code = "PO" NO-LOCK:
            lv-text = lv-text + notes.note_text + CHR(10).
        END.
        
        ASSIGN
        lv-text-line = 0
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
           
        FIND FIRST tt-text WHERE tt-text.TYPE = "specnote" AND tt-text.tt-recid = recid(po-ordl) NO-LOCK NO-ERROR.
        IF AVAIL tt-text  THEN
             PUT SKIP(1).

        FOR EACH tt-text WHERE tt-text.TYPE = "specnote" AND tt-text.tt-recid = recid(po-ordl) BY tt-text.tt-line:
            IF v-printline > 48 THEN DO:         
              PAGE.
              v-printline = 0.
              {po/po-protg2.i}
            END. 
             
            PUT tt-text.tt-text FORM "x(80)"  SKIP.
                v-printline = v-printline + 1.
        END.
      
     END. 
    
     
      /* v-print-sn */
     
     /* === end of specnote print */
     
     EMPTY TEMP-TABLE tt-formtext.

     lv-text = "".
     FOR EACH notes FIELDS(note_text) WHERE notes.rec_key EQ vend.rec_key
         AND notes.note_group = "Vendor Notes" NO-LOCK:
         lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
     END.

     DO i = 1 TO EXTENT(v-inst):
        CREATE tt-formtext.
        ASSIGN
            tt-line-no = i
            tt-length  = 80.
        END.

        RUN custom/formtext.p (lv-text).
        ASSIGN
            i      = 0
            v-inst = "".
        
        FOR EACH tt-formtext:
            i = i + 1.
            IF i LE EXTENT(v-inst) THEN v-inst[i] = tt-formtext.tt-text.      
        END.

        v-inst-lines = 0.
        DO i = 1 TO EXTENT(v-inst):
            if v-inst[i] ne "" then v-inst-lines = v-inst-lines + 1.
        end.

        if v-inst-lines gt 0 then do:
          
           IF v-printline >= 48 THEN DO:         
              PAGE.
              v-printline = 0.
              {po/po-protg2.i}
            END.
            v-inst-lines = v-inst-lines + 1.
            put {1} SKIP(1).
            do i = 1 to EXTENT(v-inst):
                if v-inst[i] ne "" THEN DO:
                    put {1} v-inst[i]  SKIP.
                    v-printline = v-printline + 1.
                END.
             end.
           end.
           
           PUT "<R-1>________________________________________________________________________________________________"
           SKIP(1).
           v-printline = v-printline + 1.
     end. /* for each po-ordl record */
     
  ASSIGN v-inst = ""
         v-tmp-lines = 0
         j = 0
         K = 0
         lv-got-return = 0.
 
FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
          
    DO i = 1 TO LENGTH(notes.note_text):        
       IF i - j >= 80 THEN ASSIGN j = i
                                  lv-got-return = lv-got-return + 1.
             
       v-tmp-lines = ( i - j ) / 80.
       {SYS/INC/ROUNDUP.I v-tmp-lines}
       k = v-tmp-lines + lv-got-return.

       IF k < 5 THEN v-inst[k] = v-inst[k] + 
            IF SUBSTRING(notes.note_text,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                           THEN SUBSTRING(notes.note_text,i,1)
                           ELSE "".

       IF SUBSTRING(note_text,i,1) = CHR(10) OR
          SUBSTRING(note_text,i,1) = CHR(13) THEN
          do:
             ASSIGN
                lv-got-return = lv-got-return + 1
                j = i.
          END.
     END.
  end.
   
  IF v-printline > 48 THEN DO:                  
     PAGE.
     v-printline = 0.
     {po/po-protg2.i}
  END.


  v-sig-image = "signature\" + po-ord.buyer + ".jpg". /*RD1*/
  FILE-INFO:FILE-NAME = v-sig-image.
  IF SEARCH(FILE-INFO:FULL-PATHNAME) = ? THEN DO:
     v-sig-image = "signature\" + po-ord.buyer + ".bmp".     
     FILE-INFO:FILE-NAME = v-sig-image.
  END.
  v-sig-image = IF file-info:FULL-PATHNAME <> ? THEN FILE-INFO:FULL-PATHNAME + ">" ELSE "".

  FIND FIRST users WHERE users.USER_id = po-ord.buyer NO-LOCK NO-ERROR.
  IF NOT AVAIL users THEN
     FIND FIRST buyer WHERE buyer.company = po-ord.company
                        AND buyer.buyer = po-ord.buyer NO-LOCK NO-ERROR.
     ASSIGN
      v-username = IF AVAIL users THEN users.USER_name
               ELSE IF AVAIL buyer THEN buyer.buyer-n
               ELSE po-ord.buyer
      v-tot-sqft = 0
      v-bot-lab[1] = "Tax        :"
                   + STRING(po-ord.tax,"->>>,>>9.99").
 
IF v-inst[1] <> "" OR v-inst[2] <> "" OR v-inst[3] <> "" OR v-inst[4] <> "" THEN DO:
  IF v-printline >= 46 THEN DO:                  
     PAGE.
     v-printline = 0.
     {po/po-protg2.i}
  END. 
END.
   
      PUT "<R50><C1>" v-inst[1] 
          "<R51><C1>" v-inst[2]
          "<R52><C1>" v-inst[3]
          "<R53><C1>" v-inst[4]
          "<R56><C57><#8><FROM><R+6><C+22><RECT> " 
    "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "->>>>,>>9.99"
    "<=8><R+2> "  v-bot-lab[1] 
    "<=8><R+3> "  " "
    "<=8><R+4> Grand Total:" po-ord.t-cost FORM "->>>>>,>>9.99" 
    "<=8><R+5>         " v-curr-dscr FORM "X(15)".

ASSIGN v-curr-dscr = "" .

PUT "<FArial><R57><C1><P13><B>Signed: </B> <P9> " SKIP
    "" SKIP
    "" SKIP  .

v-signature = IF v-sig-image <> "" THEN
              "<C10><#2><R-4.5><C+40><Image#2=" + v-sig-image        
              ELSE "".

PUT {1} 
    v-signature FORM "x(100)"
    SKIP. 

PUT "<FArial><R60><C2><P10><U><B>Forward Invoices via email to: pdinvoices@protagon.com  </B></U> </P10> " SKIP(1)
    "<R61><C20><P9><B> PROTAGON DISPLAY INC.</B>" SKIP
    "<R62><C2><B> 719 Tapscott Road Toronto, Ontario Canada  MIX1A2 </B>" SKIP
    "<R63><C2><B> Tel: (416)293-9500   Fax: (416)293-9600 </B>" SKIP.
/*   Removed per task# 05131104     
PUT "<FArial> <R58><C53><P10><B> ONTARIO RETAIL SALES TAX </B>" skip 
    " <R59><C44><P8><B>The Contracts of insurance or benefits plan or the taxable goods and</B>" SKIP
    " <R60><C44><B>taxable services ordered on this purchase order are covered by a blanket </B>" SKIP
    " <R61><C44><B>purchase exemption certificate or Identity Card in the name of:</B>" SKIP
    " <R62><C44><B>Protagon Display Inc.719 Tapscott Road Toronto, Ontario Canada</B>" SKIP 
    " <R63><C44><B>RST Vendor Permit No. 3942-3603</B>" SKIP.  */

v-printline = v-printline + 6.

IF v-printline < 60 THEN PUT SKIP(80 - v-printline).

END. /* for each po-ord record */.
/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */
/*PROCEDURE calc-cost:

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
     
     IF AVAIL b-qty THEN
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

END PROCEDURE.*/


