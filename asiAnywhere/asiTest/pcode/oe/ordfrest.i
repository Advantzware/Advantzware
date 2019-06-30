
DEF INPUT PARAM ip-new-ord AS LOG NO-UNDO.

DEF VAR char-hdl AS cha NO-UNDO.
def var v-est-no like est.est-no no-undo.
def var v-est-type like est.est-type no-undo.
def var v-factor as dec no-undo.
def var v-run-list as char init
  "oe/calc-one.p,oe/calc-box.p,ce/tan/print4.p,ce/com/print4.p,cec/com/print4.p".
def var i as int no-undo.
def var j as int no-undo.
def var x as int no-undo.
def var v-blk-qty as int no-undo.
def var v-tax-rate as dec form ">,>>9.99<<<" no-undo.
def var v-frt-tax-rate like v-tax-rate no-undo.
def var v-quo-price like sys-ctrl.log-fld no-undo.
def var li-line-no as int no-undo.
def var choice as log no-undo.
def var hld-id as recid no-undo.
def var hld-stat like job.stat no-undo.
def var hld-nufile as log no-undo.
def var lv-pr-uom as cha no-undo.
def var lv-job-recid as recid no-undo.
DEF VAR ll-canceled AS LOG NO-UNDO.
DEF VAR v-run-schedule AS LOG NO-UNDO.
DEF VAR lv-cas-cnt LIKE eb.cas-cnt NO-UNDO.
DEF VAR ll-do-entry AS LOG NO-UNDO.
DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.
DEF VAR ll-new-fg AS LOG NO-UNDO.
DEF VAR v-com AS DEC NO-UNDO.
DEF VAR ld-marg% AS DEC NO-UNDO.

DEF BUFFER b-oe-ord FOR oe-ord.
DEF BUFFER b-oe-ordl FOR oe-ordl.

session:set-wait-state("general").

DISABLE TRIGGERS FOR LOAD OF oe-ordl.

find first sys-ctrl where sys-ctrl.company eq cocode
                     and sys-ctrl.name    eq "SCHEDULE" no-lock no-error.
/*v-run-schedule = if avail sys-ctrl AND sys-ctrl.log-fld THEN YES ELSE NO. 
         Task  09130412 */
v-run-schedule = IF sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO
                 ELSE IF sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES
                 ELSE NO.   /*sys-ctrl.log-fld.*/

FOR EACH w-ord:
  DELETE w-ord.
END.

FOR EACH tt-oe-ordl:
  DELETE tt-oe-ordl.
END.

assign
 v-qty-mod = ip-new-ord EQ ?
 v-est-no  = oe-ord.est-no.

run util/rjust.p (input-output v-est-no,8).
 
find first xest where xest.company eq cocode
                  and xest.est-no  eq v-est-no 
                NO-LOCK no-error.

v-est-type = xest.est-type - if xest.est-type gt 4 then 4 else 0.

if avail xest then do:
  IF ip-new-ord THEN DO:
    /** CHECK for INACTIVE CUSTOMERS **/
    for each eb where eb.company = cocode and
                      eb.est-no   eq xest.est-no
                  and eb.form-no ne 0
                  AND TRIM(eb.cust-no) NE ""
             no-lock break by eb.est-no by eb.cust-no:
      
      if first-of(eb.cust-no) then do:
        /** finding the customer is done this way because the index is not
        setup efficently to find the customer regardles of active stat **/
        FIND first cust {sys/ref/cust.w}
                        and cust.cust-no eq eb.cust-no
             use-index cust no-lock no-error.
        if not avail cust or cust.active eq "I" then do:
          message              "   Inactive Customer:" cust.name skip
             "   Orders May not Be Processed for Inactive Customers.  "
             view-as alert-box warning.
          assign v-inactive = yes.
          return.
        end. /* do */
      end. /* first-of(eb.cust-no) */

      IF v-est-fg1 = "HOLD" AND eb.stock-no = "" THEN DO:
/*        MESSAGE "Sorry, FG item does not exist. Order has not been approved."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-ord.est-no IN FRAME {&FRAME-NAME}.
        RETURN "NO FGITEM" .*/
      END.
    end. /* each eb */

    /* ======== get qty for order ===========*/
    lv-qty = 0.
    find first est-qty where est-qty.company = xest.company
                         and est-qty.est-no = xest.est-no
                      no-lock no-error.
    if avail est-qty and est-qty.qty[1] <> 0 and
       (est-qty.qty[2] <> 0 or est-qty.qty[3] <> 0 or est-qty.qty[4] <> 0) 
    then do:
      run oe/d-ordqty.w (recid(est-qty), output lv-qty).  
      /*if lv-qty = 0 then return "No Qty".*/
    end.
    else lv-qty = est-qty.qty[1].
  END.

  ELSE
  IF v-est-type EQ 3 OR v-est-type EQ 4 THEN DO:
    CREATE w-ord.
    w-ord-no = oe-ord.ord-no.

    FOR EACH oe-ordl
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ oe-ord.ord-no:
      
      IF oe-ordl.est-no EQ oe-ord.est-no THEN DO:
        CREATE tt-oe-ordl.
        BUFFER-COPY oe-ordl TO tt-oe-ordl
        ASSIGN tt-oe-ordl.row-id = ROWID(oe-ordl).

        ASSIGN
         oe-ordl.qty       = 0
         oe-ordl.t-weight  = 0
         oe-ordl.t-freight = 0
         oe-ordl.cost      = 0.
      END.

      ELSE
      IF oe-ordl.est-no NE "" THEN DELETE oe-ordl.
    END.
  END.

  session:set-wait-state('general').

  /* RECALC ESTIMATE */
  save_id = RECID(oe-ord).
  IF v-est-type EQ 3 OR v-est-type EQ 4 THEN  /* not done ??? */
    RUN VALUE(ENTRY(v-est-type + INT(xest.est-type EQ 8),v-run-list)).
  
  assign
   j         = 1
   v-job-no  = if oe-ord.job-no <> "" then oe-ord.job-no else string(oe-ord.ord-no)
   v-job-no2 = oe-ord.job-no2
   fil_id    = save_id.        /* reset fil_id, scrambled in calc...*/
 
  find first xeb
      where xeb.company = xest.company 
        and xeb.est-no eq xest.est-no
        and xeb.form-no  eq 0
      no-lock no-error.     
  for each eb
      where eb.company = xest.company 
        and eb.est-no  eq xest.est-no
        and eb.form-no  ne 0
        and eb.blank-no ne 0
        AND TRIM(eb.cust-no) NE ""
      no-lock,
      FIRST ef OF eb NO-LOCK,
      FIRST cust NO-LOCK
      {sys/ref/cust.w}
        AND cust.cust-no eq eb.cust-no
      USE-INDEX cust    
      break by eb.est-no by eb.cust-no by eb.form-no by eb.blank-no
      TRANSACTION:

    FIND xoe-ord WHERE RECID(xoe-ord) EQ fil_id.
      
    if first-of(eb.cust-no) AND ip-new-ord then do:
      if not first(eb.cust-no) then do:
         do x = 1 to v-exp-limit:
            find first oe-ctrl where oe-ctrl.company eq cocode exclusive-lock  no-wait no-error.
            if not avail oe-ctrl then do:
               if x eq 1 then
                    message " Searching for next available Order Number. ".
               readkey pause 1.
            end.         
            if not avail oe-ctrl then do:
               message " Unable to Obtain next available Order Number. " view-as alert-box error.
               return.
            end.
         end. /* 1 to v-exp-limit */        
         if avail oe-ctrl then  assign v-n-ord       = oe-ctrl.n-ord
                                       oe-ctrl.n-ord = oe-ctrl.n-ord + 1.
         else do:
               message "Company " cocode " has no Order-Control record." 
                       view-as alert-box error.
               return.        
         end.
         FIND CURRENT oe-ctrl NO-LOCK NO-ERROR.
         /*************** create ORDER HEADER ******************/
         {oe/oe-ord.a} 

         ASSIGN
          xoe-ord.est-no    = oe-ord.est-no
          xoe-ord.type      = oe-ord.type
          xoe-ord.ord-date  = TODAY
          xoe-ord.job-no    = v-job-no
          xoe-ord.job-no2   = v-job-no2
          v-estord-id[j]    = RECID(xoe-ord)
          j                 = j + 1
          li-line-no  = 0
          v-multord         = YES
          fil_id            = RECID(xoe-ord)
          xoe-ord.sold-no   = 1  /** DEFAULT to first SOLD to **/
          xoe-ord.sold-id   = eb.cust-no  /** DEFAULT to first SOLD to **/
          xoe-ord.sman[1]   = eb.sman
          xoe-ord.cust-no   = eb.cust-no
          xoe-ord.carrier   = eb.carrier
          xoe-ord.frt-pay   = eb.chg-method
          xoe-ord.s-comm[1] = eb.comm
          xoe-ord.s-pct[1]  = 100
          xoe-ord.est-type  = xest.est-type
          xoe-ord.due-code  = "ON"
          
          xoe-ord.cust-no   = cust.cust-no
          xoe-ord.cust-name = cust.name
          xoe-ord.addr[1]   = cust.addr[1]
          xoe-ord.addr[2]   = cust.addr[2]
          xoe-ord.city      = cust.city
          xoe-ord.state     = cust.state
          xoe-ord.zip       = cust.zip
          xoe-ord.contact   = cust.contact
          xoe-ord.last-date = xoe-ord.ord-date + cust.ship-days
          xoe-ord.due-date  = xoe-ord.last-date
          xoe-ord.terms     = cust.terms
          xoe-ord.over-pct  = cust.over-pct
          xoe-ord.under-pct = cust.under-pct
          xoe-ord.fob-code  = cust.fob-code
          xoe-ord.f-bill    = (oe-ord.frt-pay eq "B")
          xoe-ord.tax-gr    = cust.tax-gr
          v-custype         = cust.TYPE
          v-factor = if xest.est-type ge 1 and xest.est-type le 4 then ld-lastship-dec
                     else 1.

         IF xest.ord-no NE 0 THEN xoe-ord.pord-no = xest.ord-no.

         if lastship-cha eq "Fibre" then
           ASSIGN
            xoe-ord.last-date = xoe-ord.ord-date + (cust.ship-days * v-factor)
            xoe-ord.due-date  = xoe-ord.ord-date + (lastship-int * v-factor).
      end.  /* not first */

      xoe-ord.lead-days = xoe-ord.last-date - xoe-ord.last-date.

      find first shipto where shipto.company eq cocode
                          and shipto.cust-no eq cust.cust-no
                          no-lock no-error.
      if avail shipto then
         assign
         xoe-ord.ship-i[1] = shipto.notes[1]
         xoe-ord.ship-i[2] = shipto.notes[2]
         xoe-ord.ship-i[3] = shipto.notes[3]
         xoe-ord.ship-i[4] = shipto.notes[4].

      CREATE w-ord.
      w-ord-no = xoe-ord.ord-no.
    end. /* first-of(eb.cust-no) */

    /************* create LINE ITEMS ***************/
    find first oe-ordl where oe-ordl.company  eq cocode
          and oe-ordl.ord-no   eq xoe-ord.ord-no
          and oe-ordl.cust-no  eq xoe-ord.cust-no
          and (oe-ordl.part-no eq eb.part-no
           or  (oe-ordl.i-no   eq eb.stock-no and eb.stock-no ne ""))
          and (v-est-type eq 4 or
               can-find(first tt-oe-ordl where tt-oe-ordl.row-id eq rowid(oe-ordl)))
        no-error.
    if avail oe-ordl then oe-ordl.qty = oe-ordl.qty + eb.bl-qty.
    
    if not avail oe-ordl                                                    or
       can-find(first tt-oe-ordl where tt-oe-ordl.row-id eq rowid(oe-ordl)) then do:
      if not avail oe-ordl then do:
        li-line-no = 1.
        FOR EACH oe-ordl
            WHERE oe-ordl.company EQ xoe-ord.company
              AND oe-ordl.ord-no  EQ xoe-ord.ord-no
            BY oe-ordl.line DESC:
          li-line-no = oe-ordl.line + 1.
          LEAVE.
        END.
        create oe-ordl.
        assign
         oe-ordl.company    = cocode
         oe-ordl.ord-no     = xoe-ord.ord-no  /* input screen-value */
         oe-ordl.line       = li-line-no
         oe-ordl.po-no      = xoe-ord.po-no
         oe-ordl.job-no     = xoe-ord.job-no
         oe-ordl.job-no2    = xoe-ord.job-no2
         oe-ordl.req-code   = xoe-ord.due-code
         oe-ordl.prom-code  = xoe-ord.due-code
         oe-ordl.req-date   = xoe-ord.due-date
         oe-ordl.prom-date  = xoe-ord.due-date
         oe-ordl.i-no       = if v-est-type eq 2 and avail xeb then
                                xeb.stock-no else eb.stock-no
         oe-ordl.qty        = if v-est-type eq 3 or v-est-type eq 4 THEN eb.bl-qty ELSE lv-qty
         v-qty-mod          = YES
         oe-ordl.over-pct   = xoe-ord.over-pct
         oe-ordl.under-pct  = xoe-ord.under-pct
         .

        IF oe-ordl.i-no EQ "0" THEN oe-ordl.i-no = "".

        if xoe-ord.est-no ne "" then
          assign
           oe-ordl.est-no = xoe-ord.est-no
           oe-ordl.pr-uom = "M".

        do i = 1 to 3:
          assign oe-ordl.s-man[i] = xoe-ord.sman[i]
                 oe-ordl.s-pct[i] = xoe-ord.s-pct[i]
                 oe-ordl.s-comm[i] = xoe-ord.s-comm[i].
        end.

        IF v-foamdate-log                                         AND
           CAN-FIND(FIRST style WHERE style.company EQ eb.company
                                  AND style.style   EQ eb.style
                                  AND style.type    EQ "F")       THEN
          oe-ordl.req-date = xoe-ord.ord-date + v-foamdate-int.
      end.  /* not avail oe-ordl */

      ELSE
      IF NOT ll-new-fg               AND
         oe-ordl.i-no NE eb.stock-no THEN
        IF eb.stock-no EQ "" THEN
          MESSAGE "Do you wish to recreate the FG Item#?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-new-fg.
        ELSE
          MESSAGE "Do you wish to update the FG Item# from the estimate?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-new-fg.

      IF ll-new-fg THEN oe-ordl.i-no = eb.stock-no.

      if oe-ordl.i-no eq "" THEN DO:
        if v-est-fg THEN
          oe-ordl.i-no = if v-est-type eq 2 and avail xeb then
                           xeb.part-no else eb.part-no.   

        else
        if v-est-fg1 ne "Manual" then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.part-no eq (if v-est-type eq 2 and avail xeb then
                                         xeb.part-no else eb.part-no)
                and itemfg.cust-no eq eb.cust-no
              no-lock no-error.
          if avail itemfg then
            assign
             oe-ordl.i-no       = itemfg.i-no
             oe-ordl.part-dscr2 = itemfg.part-dscr2.
        end.

        IF v-est-fg1 EQ "Hughes" THEN
          RUN fg/hughesfg.p ((IF v-est-type EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                             OUTPUT oe-ordl.i-no).
        ELSE
        IF v-est-fg1 EQ "Fibre" THEN
          RUN fg/fibre-fg.p ((IF v-est-type EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),                              OUTPUT oe-ordl.i-no).
      END.

      ASSIGN
       oe-ordl.i-name     = if v-est-type eq 2 and avail xeb then
                              xeb.part-dscr1 else eb.part-dscr1
       oe-ordl.part-no    = if v-est-type eq 2 and avail xeb then
                              xeb.part-no else eb.part-no
       oe-ordl.part-dscr1 = if v-est-type eq 2 and avail xeb then
                                xeb.part-dscr2 else eb.part-dscr2
       oe-ordl.est-type   = eb.est-type
       oe-ordl.form-no    = eb.form-no
       oe-ordl.blank-no   = eb.blank-no
       oe-ordl.cust-no    = xoe-ord.cust-no
       oe-ordl.disc       = cust.disc
       oe-ordl.tax        = cust.sort EQ "Y" AND xoe-ord.tax-gr NE "".

      {custom/shptotax.i xoe-ord.cust-no xoe-ord.sold-id oe-ordl.tax}

      RUN est/getcscnt.p ((IF xest.est-type EQ 6 AND
                              AVAIL xeb          AND
                              xeb.cas-no NE ""   THEN ROWID(xeb) ELSE ROWID(eb)),
                         OUTPUT oe-ordl.cas-cnt,OUTPUT oe-ordl.cases-unit).
                   
      ASSIGN
       oe-ordl.cases      = TRUNC(oe-ordl.qty / oe-ordl.cas-cnt,0)
       oe-ordl.partial    = oe-ordl.qty MOD oe-ordl.cas-cnt.

      IF xest.est-type EQ 6 AND
         AVAIL xeb          AND
         xeb.pur-man        THEN
        ASSIGN
         /*oe-ordl.cases-unit = xeb.cas-pal task# 05010610*/
         oe-ordl.unit-count = xeb.tr-cnt.
      ELSE
        ASSIGN
         /*oe-ordl.cases-unit = eb.cas-pal   task# 05010610*/
         oe-ordl.unit-count = eb.tr-cnt.

      find first itemfg where itemfg.company eq cocode
                          and itemfg.i-no    eq oe-ordl.i-no
          no-lock no-error.  
      if avail itemfg then 
        assign
         oe-ordl.price      = itemfg.sell-price
         oe-ordl.pr-uom     = itemfg.sell-uom
         oe-ordl.part-dscr2 = itemfg.part-dscr2.

      oe-ordl.type-code = 
            STRING(AVAIL itemfg AND
                   CAN-FIND(FIRST b-oe-ordl
                            WHERE b-oe-ordl.company EQ itemfg.company
                              AND b-oe-ordl.i-no    EQ itemfg.i-no
                              AND b-oe-ordl.ord-no  LT oe-ordl.ord-no
                              AND ROWID(b-oe-ordl)  NE ROWID(oe-ordl)),"R/O").

      if v-est-type eq 3 or v-est-type eq 4 then do:
        find first blk where blk.id eq eb.part-no no-lock no-error.           
        if avail blk then do:
          if v-est-type eq 4 then do:
            v-blk-qty = 0.
            
            for each blk where blk.id eq eb.part-no no-lock,
                first xjob
                where xjob.form-no  eq blk.snum
                  and xjob.blank-no eq blk.bnum:
              
              assign
               oe-ordl.t-weight  = oe-ordl.t-weight + blk.fg-wt
               oe-ordl.t-freight = oe-ordl.t-freight +
                                     (blk.fg-wt$ * (blk.fg-wt / 100))
               v-blk-qty           = v-blk-qty + blk.qyld.
               
              if v-full-cost then
                oe-ordl.cost = oe-ordl.cost + blk.cost.
              
              else
                oe-ordl.cost = oe-ordl.cost + 
                                 ((xjob.lab + xjob.mat + xjob.voh + xjob.foh) *
                                  blk.qyld / 1000).
            end.
            
            oe-ordl.cost = oe-ordl.cost / (v-blk-qty / 1000).
          end.

          else do:
            assign
             oe-ordl.t-weight  = blk.fg-wt
             oe-ordl.t-freight = blk.fg-wt$ * (blk.fg-wt / 100).
             
            if v-full-cost then
              oe-ordl.cost = blk.cost -
                               (((blk.fg-wt / 100) * blk.fg-wt$)
                                  * (blk.qyld / xest.est-qty[1])).
                                   
            else do:
              find first xjob no-error.
              if avail xjob then
                oe-ordl.cost = xjob.lab + xjob.mat + xjob.voh + xjob.foh.
            end.  
          end.
          
          /*assign
           xoe-ord.t-weight = xoe-ord.t-weight  + oe-ordl.t-weight
           xoe-ord.t-freight = xoe-ord.t-freight + oe-ordl.t-freight.
          
          find first oe-ctrl where oe-ctrl.company = cocode no-wait no-error.
          if avail oe-ctrl then v-fr-tax = oe-ctrl.f-tax.
          run ar/cctaxrt.p (input cocode, xoe-ord.tax-gr,
                            output v-tax-rate, output v-frt-tax-rate).
          if v-fr-tax then
            cust.ord-bal = (cust.ord-bal + oe-ordl.t-freight +
                            round((oe-ordl.t-freight * v-frt-tax-rate) / 100,2)).
          else
            cust.ord-bal = cust.ord-bal + oe-ordl.t-freight.*/
        end.  /* avail blk */

        else do:
          message "NO BLANK AVAILABLE!!!" eb.part-no.
          hide message.
        end.
      end.  /* est-type = 3 or 4 */
    end.

    if avail xest and v-quo-price-log then do:
      run oe/getqpric.p (recid(xest), oe-ordl.part-no, "", oe-ordl.qty,
                              input-output oe-ordl.price,
                              input-output oe-ordl.pr-uom,
                              OUTPUT lv-q-no).
      IF lv-q-no NE 0 THEN DO:
        RUN oe/ordlq-no.p (ROWID(oe-ordl), lv-q-no).
        FIND CURRENT oe-ordl NO-ERROR.
      END.
    END.

    oe-ordl.t-price = oe-ordl.price * oe-ordl.qty /
                      (IF oe-ordl.pr-uom EQ "C" THEN 100  ELSE
                       IF oe-ordl.pr-uom EQ "M" THEN 1000 ELSE 1).

    {oe/defwhsed.i oe-ordl}

    FIND FIRST tt-oe-ordl WHERE tt-oe-ordl.row-id EQ ROWID(oe-ordl) NO-ERROR.
    IF AVAIL tt-oe-ordl THEN tt-oe-ordl.to-be-deleted = NO.
         
    if v-est-type eq 2 then leave. /** 2pc box & Set headers **/
  end. /* each eb */

  RELEASE cust.

  IF oeestcom-log = YES THEN        
  DO:
     RELEASE probe.

     FOR EACH probe WHERE
         probe.company = xest.company and
         probe.est-no = xest.est-no and
         probe.probe-date ne ? and
         probe.est-qty eq INT(oe-ordl.qty) AND
         probe.sell-price EQ DEC(oe-ordl.price)
         NO-LOCK
         BY probe.probe-date DESC
         BY probe.probe-time DESC:
         
         LEAVE.
     END.

     IF NOT AVAIL probe THEN
        FOR EACH probe WHERE
            probe.company = xest.company and
            probe.est-no = xest.est-no and
            probe.probe-date ne ? and
            probe.est-qty eq INT(oe-ordl.qty)
            NO-LOCK
            BY probe.probe-date DESC
            BY probe.probe-time DESC:
            
            LEAVE.
        END.

     IF AVAIL probe THEN
     DO:
        ld-marg% = (1 - (probe.full-cost / oe-ordl.price)) * 100.

        RUN est/getsmanmtrx.p (INPUT ROWID(xest),
                               INPUT "C",
                               INPUT-OUTPUT v-com,
                               INPUT-OUTPUT ld-marg%).
        ASSIGN
           oe-ordl.s-comm[1] = v-com
           xoe-ord.s-comm[1] = oe-ordl.s-comm[1].

        RELEASE probe.
     END.
  END.

  RUN release-shared-buffers.

  ll-do-entry = NO.
  IF v-est-type GE 3 AND v-est-type LE 4      AND
     v-oecomb-int EQ 1 AND NOT ll-from-tandem THEN
    MESSAGE "Do you need to update items on order?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-do-entry.

  do-all-order-lines:
  FOR EACH w-ord,
      FIRST xoe-ord
      WHERE xoe-ord.company EQ cocode
        AND xoe-ord.ord-no  EQ w-ord-no
      NO-LOCK
      BY w-ord-no.

    FOR EACH oe-ordl OF xoe-ord
        WHERE oe-ordl.est-no NE ""
          AND NOT CAN-FIND(FIRST tt-oe-ordl WHERE tt-oe-ordl.row-id EQ ROWID(oe-ordl))
        NO-LOCK:
      RUN oe/d-oeitem.w (RECID(oe-ordl),oe-ordl.ord-no,
                         "update-" +
                         STRING(INT(ll-from-tandem OR ll-do-entry) + 2,"9"),
                         OUTPUT ll-canceled).
      IF ll-canceled THEN LEAVE do-all-order-lines.
    END.

    DO TRANSACTION:
      FIND FIRST est
          WHERE est.company EQ cocode
            AND est.est-no  EQ v-est-no 
          EXCLUSIVE NO-ERROR.
      est.ord-no = xoe-ord.ord-no.
      IF est.ord-date LE xoe-ord.ord-date OR est.ord-date EQ ? THEN
        est.ord-date = xoe-ord.ord-date.
      IF est.ord-no LT xoe-ord.ord-no OR est.ord-no EQ 0 THEN
        est.ord-no = xoe-ord.ord-no.
      RELEASE est.
    
      FOR EACH tt-oe-ordl WHERE tt-oe-ordl.to-be-deleted,
          FIRST oe-ordl WHERE ROWID(oe-ordl) EQ tt-oe-ordl.row-id:
        IF AVAIL oe-ordl THEN DELETE oe-ordl.
      END.

      FOR EACH oe-ordl OF xoe-ord NO-LOCK
          WHERE oe-ordl.est-no NE "" 
            AND NOT oe-ordl.is-a-component, 
          EACH eb EXCLUSIVE
          WHERE eb.company EQ xoe-ord.company
            AND eb.est-no  EQ xoe-ord.est-no
            AND ((eb.cust-no EQ xoe-ord.cust-no AND
                  eb.part-no EQ oe-ordl.part-no) OR
                 eb.est-type EQ 2 OR
                 eb.est-type EQ 6)
          TRANSACTION:

        IF eb.form-no EQ 0 OR (eb.est-type NE 2 AND eb.est-type NE 6) THEN DO:
          ASSIGN
           eb.part-no    = oe-ordl.part-no
           eb.part-dscr1 = oe-ordl.i-name
           eb.part-dscr2 = oe-ordl.part-dscr1
           eb.stock-no   = oe-ordl.i-no.

          IF ll-from-tandem THEN
            ASSIGN
             eb.bl-qty  = oe-ordl.qty
             eb.yld-qty = oe-ordl.qty.
        END.

        eb.ord-no = oe-ordl.ord-no.

        RELEASE eb.
      END. /* each oe-ordl */

      IF ll-from-tandem THEN DO:
        FIND FIRST xest
            WHERE xest.company EQ cocode
              AND xest.est-no  EQ v-est-no 
            NO-LOCK NO-ERROR.
        IF AVAIL xest THEN RUN est/oeselest.p.
        RELEASE xest.
      END.
    END. /* trans */
  END.

  FOR EACH tt-oe-ordl BREAK BY tt-oe-ordl.ord-no:
    FIND FIRST oe-ordl WHERE ROWID(oe-ordl) EQ tt-oe-ordl.row-id NO-ERROR.
    v-qty-mod = NOT AVAIL oe-ordl OR tt-oe-ordl.qty NE oe-ordl.qty OR v-qty-mod.
      
    IF ll-canceled AND AVAIL oe-ordl THEN BUFFER-COPY tt-oe-ordl TO oe-ordl.

    IF LAST-OF(tt-oe-ordl.ord-no) THEN DO:
      FIND FIRST xoe-ord OF oe-ordl NO-LOCK NO-ERROR.
      IF AVAIL xoe-ord THEN RUN oe/calcordt.p (ROWID(xoe-ord)).
    END.
  END.

  IF ll-canceled AND ip-new-ord THEN DO:
    FOR EACH w-ord WHERE w-ord-no NE oe-ord.ord-no,
        FIRST xoe-ord
        WHERE xoe-ord.company EQ cocode
          AND xoe-ord.ord-no  EQ w-ord-no
        EXCLUSIVE
        TRANSACTION:
      FOR EACH oe-ordl
          WHERE oe-ordl.company EQ xoe-ord.company
            AND oe-ordl.ord-no  EQ xoe-ord.ord-no:
        DELETE oe-ordl.
      END.
      FOR EACH oe-ordm
          WHERE oe-ordm.company EQ xoe-ord.company
            AND oe-ordm.ord-no  EQ xoe-ord.ord-no:
        DELETE oe-ordm.
      END.
      DELETE xoe-ord.
    END.

    adm-new-record = YES.
    RUN dispatch ("cancel-record").
    ll-is-new-rec = NO.
    RETURN NO-APPLY.
  END.

  IF NOT ll-canceled THEN DO:
    FOR EACH w-ord,
        EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ w-ord-no
          AND oe-ordl.is-a-component EQ NO,
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ oe-ordl.company
          AND itemfg.i-no    EQ oe-ordl.i-no:

      IF NOT CAN-FIND(FIRST tt-oe-ordl
                      WHERE tt-oe-ordl.row-id EQ ROWID(oe-ordl)) THEN
        RUN fg/makenote.p (BUFFER oe-ordl,
                           BUFFER quoteqty,
                           BUFFER ar-invl,
                           NO,
                           itemfg.rec_key).

      RUN sys/inc/ordlcomp.p (ROWID(oe-ordl)).
    END.

    IF v-create-job AND oe-ord.job-no NE "" THEN DO:
      FIND FIRST job NO-LOCK
          WHERE job.company EQ oe-ord.company
            AND job.job-no  EQ oe-ord.job-no
            AND job.job-no2 EQ oe-ord.job-no2
          NO-ERROR.

      IF AVAIL job AND TRIM(job.est-no) NE TRIM(oe-ord.est-no) THEN
        IF CAN-FIND(FIRST job-hdr
                    WHERE job-hdr.company EQ job.company
                      AND job-hdr.job     EQ job.job
                      AND job-hdr.job-no  EQ job.job-no
                      AND job-hdr.job-no2 EQ job.job-no2
                      AND job-hdr.ord-no  NE oe-ord.ord-no) OR
           CAN-FIND(FIRST b-oe-ord
                    WHERE b-oe-ord.company EQ job.company
                      AND b-oe-ord.job-no  EQ job.job-no
                      AND b-oe-ord.job-no2 EQ job.job-no2
                      AND b-oe-ord.est-no  EQ job.est-no)   OR
           CAN-FIND(FIRST b-oe-ordl
                    WHERE b-oe-ordl.company EQ job.company
                      AND b-oe-ordl.job-no  EQ job.job-no
                      AND b-oe-ordl.job-no2 EQ job.job-no2
                      AND b-oe-ordl.est-no  EQ job.est-no)  THEN RELEASE job.
        ELSE
        DO TRANSACTION:
          FIND CURRENT job NO-ERROR.
          IF AVAIL job THEN DELETE job.
        END.

      IF NOT AVAIL job THEN DO:
        RUN create-job (OUTPUT lv-job-recid).
        FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
      END.                 

      IF AVAIL job AND INDEX("HWPRL",job.stat) NE 0 THEN DO:
        IF NOT v-qty-mod THEN RUN oe/job-qty.p (ROWID(oe-ord), OUTPUT v-qty-mod).

        IF job.stat EQ "P" OR v-qty-mod THEN DO:
          RUN jc/chkrebld.p (RECID(job), OUTPUT choice).     
          IF NOT choice THEN DO:
            ASSIGN hld-id     = fil_id
                   hld-nufile = nufile 
                   hld-stat   = job.stat
                   nufile     = YES.

            RUN jc/jc-calc.p(RECID(job)).
            ASSIGN fil_id   = hld-id
                   nufile   = hld-nufile.
           
            IF hld-stat NE "P" THEN DO TRANSACTION:
              FIND CURRENT job EXCLUSIVE.
              job.stat = hld-stat.
              FIND CURRENT job NO-LOCK.
            END.
          END.
        END.
      END.
            
      FOR EACH w-ord,
          EACH oe-ordl NO-LOCK
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ w-ord-no
            AND oe-ordl.is-a-component EQ NO

          BREAK BY oe-ordl.job-no
                BY oe-ordl.job-no2:

        IF LAST-OF(oe-ordl.job-no2) THEN DO:
          ASSIGN
           hld-id     = fil_id
           hld-nufile = nufile
           fil_id     = RECID(oe-ordl).
         
          /*IF oe-ordl.ord-no NE oe-ord.ord-no THEN*/ RUN po/do-po.p.
          /* check oe-ordl.due-date and calc promised date and job's start-date */
   
          IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date.

          STATUS DEFAULT "".
          ASSIGN
           fil_id = hld-id
           nufile = hld-nufile.
        END.
      END.
    END.  /* v-create-job */

    FOR EACH tt-oe-ordl
        WHERE tt-oe-ordl.to-be-deleted
          AND NOT CAN-FIND(FIRST oe-ordl WHERE ROWID(oe-ordl) EQ tt-oe-ordl.row-id):
      CREATE oe-ordl.
      BUFFER-COPY tt-oe-ordl TO oe-ordl.
    END.
  END.

  RUN release-shared-buffers.

  RUN dispatch ('open-query').
  RUN dispatch ('row-changed').
end. /* avail xest */
