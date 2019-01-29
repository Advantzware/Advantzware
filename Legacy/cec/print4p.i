  
  DEF VAR v-line LIKE probe.line no-undo.
  DEF VAR v-yld-qty AS DEC FORMAT ">>>>9.9<<<" NO-UNDO.
  DEF VAR v-hdr-depth AS CHAR FORMAT "x(5)" NO-UNDO.
  DEF VAR v-n-out AS INT NO-UNDO.
  DEF VAR v-n-up  AS INT NO-UNDO.
  DEF VAR ll-use-defaults AS LOG NO-UNDO.
  DEF BUFFER reftable-broker-pct FOR reftable.
  DEF BUFFER b-est-qty-2 FOR est-qty.
  DEF VAR v-count-2 AS INT NO-UNDO.
  DEF VAR v-update-qty-gsa AS LOG NO-UNDO.
  DEF VAR ld-gsa-brd AS DEC NO-UNDO.
  DEF VAR ld-gsa-mat AS DEC NO-UNDO.
  DEF VAR ld-gsa-lab AS DEC NO-UNDO.

  {cec/get-vend.i}  /* get vendor number */

  qtty = 0. 

  do transaction:
     {cec/msfcalc.i}
     {cec/rollfac.i}
  end.
  find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
  assign
     ctrl[1]  = ce-ctrl.whse-mrkup / 100
     ctrl[2]  = ce-ctrl.hand-pct / 100
     ctrl[3]  = ce-ctrl.rm-rate
     ctrl[4]  = ce-ctrl.spec-%[1]
     ctrl[5]  = int(ce-ctrl.comm-add)
     ctrl[6]  = int(ce-ctrl.shp-add)
     ctrl[7]  = int(ce-ctrl.sho-labor)
     ctrl[8]  = int(ce-ctrl.trunc-99)
     ctrl[11] = ce-ctrl.spec-%[2]
     ctrl[12] = ce-ctrl.spec-%[3]
     ctrl[13] = int(ce-ctrl.spec-add[1])
     ctrl[14] = int(ce-ctrl.spec-add[2])
     ctrl[15] = int(ce-ctrl.spec-add[3])
     ctrl[16] = int(ce-ctrl.spec-add[6])
     ctrl[17] = int(ce-ctrl.spec-add[7])
     ctrl[18] = int(ce-ctrl.spec-add[8])
     v-gsa    = index("SB",ce-ctrl.sell-by) eq 0.
     

     ctrl[19] = ce-ctrl.broker-pct.

  fg-rate-f = ce-ctrl.fg-rate-farm.
 
  rm-rate-f = ce-ctrl.rm-rate-farm.

  
  hand-pct-f = ce-ctrl.hand-pct-farm / 100.

  find first xop where xop.company = xest.company and
                       xop.est-no = xest.est-no and
                       xop.op-speed = 0 no-lock no-error.
  {sys/inc/setsh.i}

  ASSIGN
  save-qty = qty  /* need to check qty value not sure where value is assigned ???? */
  save-lock = xef.op-lock.

  do transaction:
    find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "CEDFAULT"
      no-lock no-error.
    if not avail sys-ctrl then do:
      create sys-ctrl.
      assign
         sys-ctrl.company = cocode
         sys-ctrl.name    = "CEDFAULT"
         sys-ctrl.log-fld = no
         sys-ctrl.descrip = "Ask CERUN & CEGSA log values on Whatif?  " +
                        "No uses saved est. values!".
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
    end.
    ll-use-defaults = sys-ctrl.log-fld.

    {est/recalc-mr.i xest}
    FIND CURRENT recalc-mr NO-LOCK.

    {sys/inc/cerun.i C}
    ASSIGN
     do-speed  = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.recalc
     do-mr     = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE (recalc-mr.val[1] EQ 1)
     v-board-cost-from-blank = recalc-mr.val[3] EQ 1
     vmclean   = sys-ctrl.char-fld NE ""
     vsuthrlnd = lookup(sys-ctrl.char-fld,"Suthrlnd,Clevelnd,Brick") ne 0.

    {sys/inc/cerun.i}

    ASSIGN
       v-bqty = sys-ctrl.int-fld
       v-module = module.
    
    if sys-ctrl.char-fld eq "Brick" then
       assign  v-module = v-module + " - ISO# CS-03-1-F"
               {sys/inc/ctrtext.i "v-module" 60}.
    {sys/inc/ctrtext.i "v-module" 60}.
    find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "CEGSA"
         no-lock no-error.
    if not avail sys-ctrl then do:
       create sys-ctrl.
       assign
          sys-ctrl.company = cocode
          sys-ctrl.name    = "CEGSA"
          sys-ctrl.descrip = "Default for GS&A override".
       MESSAGE sys-ctrl.descrip
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE sys-ctrl.log-fld.
    end.
    do-gsa = IF NOT v-gsa THEN do-gsa ELSE
             IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.override.

    find first sys-ctrl  where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "CESLIT"
         no-lock no-error.
    if not avail sys-ctrl then do:
       create sys-ctrl.
       assign
          sys-ctrl.company = cocode
          sys-ctrl.name    = "CESLIT"
          sys-ctrl.descrip = "Ask 'Drop Slitter...' question at OE cost calculation?"
          sys-ctrl.log-fld = no.
       MESSAGE sys-ctrl.descrip
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE sys-ctrl.log-fld.
    end.
    v-drop-rc = sys-ctrl.log-fld.
  end.  /* do transaction */

  if vprint then do:
     {cec/blkeqsht.i}

     j = 0.
     EMPTY TEMP-TABLE tt-bqty.
     FOR EACH est-qty NO-LOCK
         WHERE est-qty.company EQ xest.company
           AND est-qty.est-no  EQ xest.est-no:
       DO i = 1 TO 20:
         IF est-qty.qty[i] NE 0 THEN DO:

           j = j + 1.
           CREATE tt-bqty.
           ASSIGN
            tt-bqty = est-qty.qty[i]
            tt-brel = est-qty.qty[i + 20].
           IF v-bqty GT 0             AND
              (j EQ 1 OR v-bqty GT 1) AND
              j + 1 LE EXTENT(qtty)   THEN DO:
             CREATE tt-bqty.
             RUN cec/pr4-bqty.p (v-vend-no, est-qty.qty[i], OUTPUT tt-bqty).
             DO v-count-2 = 1 TO 20:
                IF est-qty.qty[v-count-2] EQ tt-bqty THEN
                DO:
                   tt-brel = est-qty.qty[v-count-2 + 20].
                   LEAVE.
                END.
             END.
           END.
         END.   
       END.  /* do i */ 
     END.
         
     FOR EACH tt-bqty WHERE tt-bqty GT 99999999:
       tt-bqty = 99999999.
     END.

     j = 0.
     FOR EACH tt-bqty BREAK BY tt-bqty:
       IF FIRST-OF(tt-bqty) THEN DO:
         j = j + 1.
         IF j LE EXTENT(qtty) THEN
           ASSIGN
            qtty[j] = tt-bqty
            rels[j] = tt-brel.
       END.
     END.

     {sys/inc/srtqty.i &sub=i &ext=EXTENT(qtty) &qty=qtty &fil=1 &rel=rels}
     FOR EACH tt-qtty:
       DELETE tt-qtty.
     END.
     CREATE tt-qtty.
     DO i = 1 TO EXTENT(qtty):
        assign tt-qtty.qtty[i] = qtty[i]
               tt-qtty.rel[i] = IF qtty[i] EQ 0 THEN 0
                                ELSE
                                IF rels[i] EQ 0 THEN 1
                                ELSE rels[i].
     end.
     ASSIGN
       v-do-all-forms-ink = NO.

     run est/getqty.w (input-output do-speed, input-output do-mr, input-output do-gsa, input-output v-drop-rc,
                       input-output v-match-up, INPUT-OUTPUT v-do-all-forms-ink, INPUT-OUTPUT v-board-cost-from-blank, input no, output lv-error). 
     if lv-error then return error.

     IF lv-override THEN DO:
         for each probe where probe.company = xest.company and
                           probe.est-no = xest.est-no:
            delete probe.                 
         end.
         RUN est\CostResetHeaders.p(ROWID(xest), ROWID(job)).
     END.
  
     DO i = 1 TO EXTENT(qtty):
        ASSIGN
           qtty[i] = tt-qtty.qtty[i]
           rels[i] = tt-qtty.rel[i].
     end.
     {sys/inc/srtqty.i &sub=i &ext=EXTENT(qtty) &qty=qtty &fil=2 &rel=rels}
     DO i = 1 TO EXTENT(qtty):
        if qtty[i] eq 0 then rels[i] = 0.
        else if rels[i] eq 0 then rels[i] = 1.
     end.

  end.   /* vprint */
  else do:
     assign qtty[1] = qty.

     IF v-shared-rel EQ 0 THEN /*no quote from o/e*/
        FOR EACH est-qty WHERE
            est-qty.company EQ xest.company AND
            est-qty.est-no  EQ xest.est-no
            NO-LOCK:

            DO i = 1 TO 20:
               IF est-qty.qty[i] EQ qty THEN DO:
                  rels[1] = est-qty.qty[i + 20].
                  LEAVE.
               END.
            END.
        END.
     ELSE
        rels[1] = v-shared-rel.

     IF rels[1] = 0 THEN
        rels[1] = 1.

     find first sys-ctrl  where sys-ctrl.company eq cocode
                            and sys-ctrl.name    eq "FGCOST"
                  no-lock no-error.
     if not avail sys-ctrl then do transaction:
         create sys-ctrl.
         assign  sys-ctrl.company = cocode
                 sys-ctrl.name    = "FGCOST"
                 sys-ctrl.log-fld = no
                 sys-ctrl.descrip = "Create FG Cost in Job File with only Board Cost?".
         MESSAGE sys-ctrl.descrip
             VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
             UPDATE sys-ctrl.log-fld.
     end.
     v-brd-only = sys-ctrl.log-fld.
  end.  /* else vprint  */

  DO TRANSACTION:
    {est/op-lock.i xest}

    FIND xef WHERE RECID(xef) EQ lv-ef-recid .
    FIND xef WHERE RECID(xef) EQ lv-ef-recid NO-LOCK.
    FIND est WHERE RECID(est) EQ RECID(xest).
    FIND CURRENT recalc-mr.
    ASSIGN
     est.recalc       = do-speed
     recalc-mr.val[1] = INT(do-mr)
     recalc-mr.val[3] = INT(v-board-cost-from-blank)
     est.override     = do-gsa
     op-lock.val[1]   = INT(est.recalc)
     op-lock.val[2]   = recalc-mr.val[1].
    FIND est WHERE RECID(est) EQ RECID(xest) NO-LOCK.
    FIND xest WHERE RECID(xest) EQ RECID(est) NO-LOCK.
    FIND CURRENT recalc-mr NO-LOCK.
    FIND CURRENT op-lock NO-LOCK.  
     
  END.

  session:set-wait-state("General").
  find first sman   where   sman.company = cocode and
                          sman.sman    = xeb.sman no-lock no-error.
  find first cust   where   cust.company = cocode and
                          cust.cust-no = xeb.cust-no no-lock no-error.
  find first shipto where shipto.company = cocode and
                          shipto.cust-no = xeb.cust-no and
                        shipto.ship-id = xeb.ship-id no-lock no-error.
  find first style  where  style.company = cocode and
                         style.style = xeb.style no-lock no-error.
  if cust.cust-no ne "Temp" then
     assign cust-ad[1] = cust.name cust-ad[2] = cust.addr[1] cust-ad[3] = cust.addr[2]
            cust-ad[4] = cust.city + ", " + cust.state + " " + cust.zip.
  else
     assign cust-ad[1] = xeb.ship-name
            cust-ad[2] = xeb.ship-addr[1]
            cust-ad[3] = xeb.ship-addr[2]
            cust-ad[4] = xeb.ship-city + ", " + xeb.ship-state + " " + xeb.ship-zip.
   
  if cust-ad[3] eq "" then  assign cust-ad[3] = cust-ad[4]
                                   cust-ad[4] = "".
  if cust-ad[2] eq "" then  assign cust-ad[2] = cust-ad[3]
                                   cust-ad[3] = cust-ad[4]
                                   cust-ad[4] = "".
  assign ship-ad[1] = shipto.ship-name
         ship-ad[2] = shipto.ship-addr[1]
         ship-ad[3] = shipto.ship-addr[2]
         ship-ad[4] = shipto.ship-city + ", " + shipto.ship-state + " " +
                      shipto.ship-zip.
 
  if ship-ad[3] eq "" then assign  ship-ad[3] = ship-ad[4]
                                   ship-ad[4] = "".
  if ship-ad[2] eq "" then assign  ship-ad[2] = ship-ad[3]
                                   ship-ad[3] = ship-ad[4]
                                   ship-ad[4] = "".
  assign dsc[1]     = xeb.part-dscr1 dsc[2] = xeb.part-dscr2
         sizcol[1]  =
         string(int(xeb.len - .499999) + ((xeb.len - int(xeb.len - .499999))
                                      / k_frac)) + "x" +
         string(int(xeb.wid - .499999) + ((xeb.wid - int(xeb.wid - .499999))
                                      / k_frac)) + "x" +
         string( int(xeb.dep - .499999) + ((xeb.dep - int(xeb.dep - .499999))
                                      / k_frac))
         sizcol[2]  = xeb.i-coldscr
         stypart[1] = style.dscr
         stypart[2] = xeb.part-no
         brd-l[1]   = xeb.t-len
         brd-l[2]   = xef.nsh-len
         brd-l[3]   = xef.gsh-len
         brd-w[1]   = xeb.t-wid
         brd-w[2]   = xef.nsh-wid
         brd-w[3]   = xef.gsh-wid
         brd-d[1]   = xeb.t-dep
         brd-d[2]   = xef.nsh-dep
         brd-d[3]   = xef.gsh-dep
         .

  if brd-l[3] = 0 and brd-w[3] = 0 then assign brd-l[3] = xef.lsh-len
                                             brd-w[3] = xef.lsh-wid.
  if xef.roll = true then assign brd-w[4] = xef.roll-wid
                               brd-l[4] = sh-len.
  else brd-w[4] = 0.

  assign brd-sq[1] = xeb.t-sqin
         brd-sq[2] = brd-l[2] * brd-w[2]
         brd-sq[3] = brd-l[3] * brd-w[3]
         brd-sq[4] = brd-l[4] * brd-w[4].
  if v-corr then  assign   brd-sf[1] = brd-sq[1] * .007
                           brd-sf[2] = brd-sq[2] * .007
                           brd-sf[3] = brd-sq[3] * .007.
  else assign  brd-sf[1] = brd-sq[1] / 144
               brd-sf[2] = brd-sq[2] / 144
               brd-sf[3] = brd-sq[3] / 144.

  do transaction:
     /* take out window if any */
     call_id = recid(xeb).
     find xeb where recid(xeb) = call_id no-error.
     xeb.t-win = 0.
     find xeb where recid(xeb) = call_id no-lock no-error.
  end.
  brd-wu[1] = xeb.t-sqin - xeb.t-win.
  find first item {sys/look/itemW.i} and
                      item.i-no = xef.board no-lock no-error.

  if v-corr then brd-wu[1] = brd-wu[1] * .007.
  else brd-wu[1] = brd-wu[1] / 144.

  assign brd-wu[1] = brd-wu[1] * if avail item then item.basis-w else 1
         brd-wu[2] = brd-sf[2] * if avail item then item.basis-w else 1
         brd-wu[3] = brd-sf[3] * if avail item then item.basis-w else 1
         day_str = string(today,"99/99/9999")
         tim_str = string(time,"hh:mm am") .

  form day_str
       v-module
       tim_str to 79  skip(1)
       with frame hdr page-top width 80 no-labels no-box stream-io.

  for each blk:
      delete blk.
  end.
  for each xjob:
      delete xjob.
  end.
  /******************** l  o  o  p  **************/
  loupe:
  do k = 1 to EXTENT(qtty) with color value("White/blue"):
    ASSIGN
     v-op-qty = 0
     op-tot   = 0
     dm-tot   = 0
     ctrl2    = 0.

    IF vprint THEN DO:
	{custom/statusMsg.i " 'Calculating... Est#  '  + xest.est-no  + ' Qty - ' + string(qtty[k]) "}
    END.

    FOR EACH w-form: 
      DELETE w-form.
    END.
    FOR EACH ink:
      DELETE ink.
    END.
    FOR EACH tt-prep-sep:
        DELETE tt-prep-sep.
    END.
    FOR EACH est-op
        WHERE est-op.company EQ xest.company 
          AND est-op.est-no  EQ xest.est-no 
          AND est-op.line    LT 500
        NO-LOCK
        BREAK BY est-op.qty:
    
      IF FIRST-OF(est-op.qty) THEN DO:
        IF FIRST(est-op.qty) OR
           CAN-FIND(FIRST est-qty
                    WHERE est-qty.company EQ est-op.company
                      AND est-qty.est-no  EQ est-op.est-no
                      AND est-qty.eqty    EQ est-op.qty)
        THEN v-op-qty = est-op.qty.
        IF est-op.qty GE qtty[k] THEN LEAVE.
      END.
    END.

    do transaction:
      for each est-op WHERE est-op.company = xest.company 
                        AND est-op.est-no eq xest.est-no
                        and est-op.line  gt 500:
        delete est-op.
      end.
      for each est-op WHERE est-op.company = xest.company 
                        AND est-op.est-no eq xest.est-no
                        and est-op.line  lt 500:
        create xop.
        buffer-copy est-op to xop
        assign
         xop.line = est-op.line + 500.
      end.
    end.

    qty = qtty[k] * IF xeb.quantityPerSet EQ 0 THEN 1 ELSE xeb.quantityPerSet.
    if qty = 0 then leave loupe.
    vmcl = k.
    iMasterQuantity = qtty[k].
    {est/probeset.i qtty[k] v-match-up}

    maxpage = k.
    run cec/prokalk.p . 

    ASSIGN
       k = maxpage /* k used in kmr-run.i */
       qty = qtty[k] * xeb.quantityPerSet.

    /*find first xop where xop.company = xest.company and xop.est-no = xest.est-no and xop.line >= 500 no-lock no-error.*/
    find first item {sys/look/itemW.i} and item.i-no = xef.board no-lock no-error.
    if available item then do:
       find first e-item of item no-lock no-error.

       assign     brd-sf[4] = /*if avail xop then */ brd-sf[3] * /*xop.num-sh*/ xef.gsh-qty   / 1000  /*else 0*/  /* tot msf */
                  brd-wu[4] = (brd-sf[4] * item.basis-w) / 2000. /* tons*/
    end.

    IF probe.LINE LT 100 THEN
       assign outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"99")
              outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"99")
              outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"99")
              outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"99").
    ELSE
       assign outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"999")
              outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"999")
              outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"999")
              outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"999").

    output to value(outfile1) .

    display day_str FORM "x(10)" v-module tim_str with frame hdr stream-io.

    display "Est#" TRIM(xest.est-no) FORMAT "x(8)"
          "SlsRep:" sman.sname when avail sman
          "UserID:" xest.updated-id
          "Prober:" probe.probe-user
          skip
          "Cust:" xeb.cust-no
                  cust-ad[1] FORMAT "x(29)" TO 44
          "Ship:" ship-ad[1] FORMAT "x(29)" TO 80 SKIP
      with no-labels no-box frame qwqw STREAM-IO.

    if cust-ad[2] ne "" or ship-ad[2] ne "" then
      put cust-ad[2] FORMAT "x(29)" TO 44
          ship-ad[2] FORMAT "x(29)" TO 80 SKIP.
    if cust-ad[3] ne "" OR ship-ad[3] ne "" then
      put cust-ad[3] FORMAT "x(29)" TO 44
          ship-ad[3] FORMAT "x(29)" TO 80 SKIP.
    if cust-ad[4] ne "" OR ship-ad[4] ne "" then
      put cust-ad[4] FORMAT "x(29)" TO 44
          ship-ad[4] FORMAT "x(29)" TO 80 SKIP.

    display /*skip(1)*/
            " --Qty---- --- Description ------ -- Size / Color ----- --- Style / Part No ---"
            qty / xeb.quantityPerSet format "->>>,>>9.9<<<"
            dsc[1] space(1) sizcol[1] space(2) stypart[1] skip
            space(11)
            dsc[2] space(1) sizcol[2] space(2) stypart[2] skip
            space(11) xeb.procat FORMAT "x(8)" space(37) "Last Ordered:" xest.ord-date
            skip(1)
            with no-box no-labels color value(col-norm) stream-io width 80 frame aa1 .

    ASSIGN
     v-yld-qty   = xeb.quantityPerSet
     v-hdr-depth = IF xeb.t-dep   EQ 0 AND
                      xef.nsh-dep EQ 0 AND
                      xef.gsh-dep EQ 0 THEN "" ELSE "Depth".

    RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).
    v-n-up = xeb.num-up.  
    display space(13)
            "   Width  Length  "
            v-hdr-depth
            "    #On Qty/Set      Sq.Feet     Wgt/Units"
            skip
            "  Blank Size:"
            brd-w[1]                            format ">>>9.99<<<"
            brd-l[1]                            format ">>>9.99<<<" 
            xeb.t-dep WHEN xeb.t-dep NE 0       format ">>>9.99<<<"
            xeb.num-up                          format ">>>,>>>" 
            v-yld-qty                           FORMAT ">>>>9.9<<<"
            brd-sf[1]                              
            "Sf/BL"
            brd-wu[1]
            space(0)
            "/MBL" skip

            " NetSht Size:"
            brd-w[2]                            format ">>>9.99<<<"
            brd-l[2]                            format ">>>9.99<<<"
            xef.nsh-dep WHEN xef.nsh-dep NE 0   format ">>>9.99<<<"
            v-n-up                              format ">>>,>>9"
            SPACE(9)
            brd-sf[2]
            "Sf/NS"
            brd-wu[2]
            space(0)
            "/MNS"
            skip

            " GrsSht Size:"
            brd-w[3]                            format ">>>9.99<<<"
            brd-l[3]                            format ">>>9.99<<<"
            xef.gsh-dep WHEN xef.gsh-dep NE 0   format ">>>9.99<<<"
            v-n-out                             format ">>>,>>9"
            SPACE(9)
            brd-sf[3]
            "Sf/GS"
            brd-wu[3]
            space(0)
            "/MGS" skip

        with stream-io no-box no-labels color value("blu/brown") width 80 frame aa2. 

    IF v-yld-qty LT 0 THEN DO WITH FRAME aa:
      ASSIGN
       v-yld-qty        = -1 / v-yld-qty
       v-yld-qty:FORMAT = ">>>>9.9<<<<".

      DISPLAY v-yld-qty.
    END.

    IF NOT vsuthrlnd THEN DO WITH FRAME aa2:
       ASSIGN
        brd-w[1]:FORMAT    = ">>>9.99"
        brd-l[1]:FORMAT    = ">>>9.99"
        xeb.t-dep:FORMAT   = ">>>9.99"
        brd-w[2]:FORMAT    = ">>>9.99"
        brd-l[2]:FORMAT    = ">>>9.99"
        xef.nsh-dep:FORMAT = ">>>9.99"
        brd-w[3]:FORMAT    = ">>>9.99"
        brd-l[3]:FORMAT    = ">>>9.99"
        xef.gsh-dep:FORMAT = ">>>9.99".

       display {sys/inc/k16v.i brd-w[1]} @ brd-w[1]
               {sys/inc/k16v.i brd-l[1]} @ brd-l[1]
               "" @ xeb.t-dep
               {sys/inc/k16v.i xeb.t-dep} WHEN xeb.t-dep NE 0 @ xeb.t-dep
               {sys/inc/k16v.i brd-w[2]} @ brd-w[2]
               {sys/inc/k16v.i brd-l[2]} @ brd-l[2]
               "" @ xef.nsh-dep
               {sys/inc/k16v.i xef.nsh-dep} WHEN xef.nsh-dep NE 0 @ xef.nsh-dep
               {sys/inc/k16v.i brd-w[3]} @ brd-w[3]
               {sys/inc/k16v.i brd-l[3]} @ brd-l[3]
               "" @ xef.gsh-dep
               {sys/inc/k16v.i xef.gsh-dep} WHEN xef.gsh-dep NE 0 @ xef.gsh-dep.
    END.

    if brd-w[4] ne 0 then
       display     "  Roll Size:" brd-w[4]                format ">>9.99<<" to 22
                with stream-io no-box no-labels color value(col-norm) width 80 frame aa3.

    if not vsuthrlnd THEN DO WITH FRAME aa3:
       brd-w[4]:FORMAT = ">>>9.99".

       if brd-w[4] ne 0 then
          display {sys/inc/k16v.i brd-w[4]} @ brd-w[4].
    END.
    display brd-sf[4] TO 52 "MSF"
            brd-wu[4] TO 70 "Tons"
        with frame aa3.

    assign  v-fac-hdr = "Mat$/M" + (if v-rollfac then "SF" else "")
            v-fac-hdr = fill(" ",8 - length(trim(v-fac-hdr))) + trim(v-fac-hdr).

    display skip(1) "Materials            Weight  Vendor          QTY/Unit  SU $"
             v-fac-hdr space(7) "TOTAL" skip
             with stream-io no-box no-labels color value("blu/brown") width 80 frame aa4.

    maxpage = k.
    /* board    */  run cec/pr4-brd.p (v-vend-no, OUTPUT v-vend-list).
    v-brd-cost = dm-tot[5].

    /* adders   */  run cec/pr4-add.p (v-vend-list).

    FIND CURRENT probe NO-ERROR.
    IF AVAIL probe THEN
      probe.boardCostTotal = probe.boardCostTotal + dm-tot[5].
    FIND CURRENT probe NO-LOCK NO-ERROR.

    /* i n k s  */  run cec/pr4-ink.p (v-vend-no).

    /* films    */  run cec/pr4-flm.p (v-vend-no).

    /* cs/tr/pal*/  run cec/pr4-cas.p (v-vend-no).

    /* special  */  run cec/pr4-spe.p (v-vend-no).

    do with frame ac5 no-labels no-box:
       display "TOTAL  DIRECT  MATERIALS "
              dm-tot[5] / (qty / 1000) / v-sqft-fac format ">>,>>9.99" to 68
              dm-tot[5] format ">>>>,>>9.99"                           to 80
              skip(1) with stream-io.
    end.

    run cec/pr4-prp.p.
    run cec/pr4-mis.p.
    k = maxpage.

    FIND FIRST xeb WHERE
         xeb.company EQ xest.company AND
         xeb.est-no EQ xest.est-no AND
         xeb.form-no NE 0
         NO-LOCK NO-ERROR.
    
    run cec/pr4-mch.p.

    if v-gsa then do:
       /* mat */
       DO i = 1 TO EXTENT(ce-ctrl.mat-pct):
          ctrl[9] = ce-ctrl.mat-pct[i] / 100.
          if ce-ctrl.mat-cost[i] > dm-tot[5] + tprep-mat 
          then leave.
       end.
       /* lab */
       DO i = 1 TO EXTENT(ce-ctrl.lab-pct):
          ctrl[10] = ce-ctrl.lab-pct[i] / 100.
          if ce-ctrl.lab-cost[i] > op-tot[5] + tprep-lab
          then leave.
       end.
    end.
    DO TRANSACTION:
      {est/calcpcts.i xest}
      IF v-gsa THEN
        ASSIGN
         calcpcts.val[1] = ctrl[9] * 100
         calcpcts.val[2] = v-brd-cost.
      FIND CURRENT calcpcts NO-LOCK NO-ERROR.
    END.

    assign  gsa-mat = ctrl[9]  * 100
            gsa-lab = ctrl[10] * 100
            gsa-com = ce-ctrl.comm-mrkup
            gsa-war = ctrl[1] * 100.

    FIND FIRST cust WHERE
         cust.company EQ xeb.company AND
         cust.cust-no EQ xeb.cust-no
         NO-LOCK NO-ERROR.

    IF AVAIL probe THEN
      gsa-fm = int(probe.gsa-fm).
    ELSE IF AVAIL cust AND cust.scomm NE 0 THEN
       gsa-fm = cust.scomm.
    ELSE
       gsa-fm = ctrl[19].

    output close.

    run cec/gsa.p (ROWID(probe), qtty[vmcl], rels[vmcl],
                   INPUT YES,
                   INPUT-OUTPUT v-update-qty-gsa, INPUT-OUTPUT ld-gsa-brd,
                   INPUT-OUTPUT ld-gsa-mat, INPUT-OUTPUT ld-gsa-lab).

   session:set-wait-state("general").
   assign
    ctrl[9]  = gsa-mat / 100
    ctrl[10] = gsa-lab / 100
    ctrl[1]  = gsa-war / 100
    ctrl[19] = gsa-fm / 100
    v-prep-mat = tprep-mat
    v-prep-lab = tprep-lab.
 
   run cec/pr4-tots.p.

   run cec/probemk.p (ROWID(probe)).

   find first blk where blk.id eq xeb.part-no no-error.
   find first xjob
        where xjob.i-no eq blk.id
          and xjob.qty  eq qtty[k]
        no-error.
   if not available xjob then do:
     create xjob.
     assign
      xjob.i-no     = blk.id
      xjob.qty      = qtty[k]
      xjob.cust-no  = xeb.cust-no
      xjob.form-no  = xeb.form-no
      xjob.blank-no = xeb.blank-no
      xjob.pct      = 1.00
      xjob.stock-no = xeb.stock-no.
   end.

   if v-brd-only then
     assign
      xjob.mat = v-brd-cost / (qtty[k] / 1000)
      xjob.lab = 0
      xjob.voh = 0
      xjob.foh = 0
      ord-cost = v-brd-cost.

   else
     assign
      xjob.mat = (dm-tot[5] + mis-tot[1] + v-prep-mat) / (qtty[k] / 1000)
      xjob.lab = (opsplit$[1] + mis-tot[3] + v-prep-lab) /
                 (qtty[k] / 1000)
      xjob.voh = opsplit$[2] / (qtty[k] / 1000)
      xjob.foh = opsplit$[3] / (qtty[k] / 1000).

    IF probe.LINE LT 100 THEN
       assign outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"99")
              outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"99")
              outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"99")
              outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"99")
              ls-outfile = tmp-dir + trim(est.est-no) + ".p" + string(probe.line,"99").
    ELSE
       assign outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"999")
              outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"999")
              outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"999")
              outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"999")
              ls-outfile = tmp-dir + trim(est.est-no) + ".p" + string(probe.line,"999").

    if search(outfile1) <> ? then do:       
       os-copy value(outfile3) value(ls-outfile).
       os-append value(outfile2) value(ls-outfile).
    end.
    else next.
                 
    if not vprint then DO TRANSACTION:

      IF probe.LINE LT 100 THEN
         os-delete value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"99")).
      ELSE
         os-delete value(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"999")).

      FIND CURRENT probe.
      DELETE probe.
    end.
  end.  /* do k=1toEXTENT(qtty)*/

  DO v = 1 TO EXTENT(qtty):
    v-qtty[v] = qtty[v].
  end.
  if vprint then run cec/pr4-mcl.p.

  DO TRANSACTION:
    FIND CURRENT op-lock NO-ERROR.
    IF AVAIL op-lock THEN DELETE op-lock.
  END.

  release xef.
  release xeb.
  session:set-wait-state("").

IF vprint THEN DO:
{custom/statusMsg.i " 'Calculating Complete....  '  "}
END.