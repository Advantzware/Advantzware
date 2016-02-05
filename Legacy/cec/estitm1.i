/* cec/estitm1.i size limit */
DEF VAR ld-markup AS DEC NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.


if not ll-is-copy-record and not ll-copied-from-eb THEN DO:
  IF (ll-new-record or is-first-record)              then do:
     if eb.stock-no = "" then do:
        find first ce-ctrl where ce-ctrl.company = gcompany and
                                 ce-ctrl.loc = gloc
                                 no-lock no-error.
        ASSIGN
        eb.cas-no = ce-ctrl.def-case
        eb.tr-no = ce-ctrl.def-pal.      
     end.
     find FIRST cust where cust.company = gcompany and
                     cust.cust-no = eb.cust-no
                     no-lock no-error.
     ASSIGN
     eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no
     eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no.      
     /* get default values from rm table */
     find item where item.company = eb.company and
                     item.i-no = eb.cas-no
              no-lock no-error.
     if avail item then assign eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)         
                              .
     find FIRST item where item.company = eb.company and
                     item.i-no = eb.tr-no
              no-lock no-error.
     if avail item then assign eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)
                               .
     find FIRST style where style.company = est.company and
                      style.style = eb.style:screen-value in browse {&browse-name}
                      no-lock no-error.
     if avail style then do:
        assign eb.adhesive = style.material[7]
               eb.gluelap = style.dim-gl
               eb.fpanel = style.dim-pan5
               eb.lock = style.dim-fit
               eb.tuck = style.dim-tk.

        FIND FIRST ITEM WHERE ITEM.company = eb.company 
                          AND ITEM.i-no = eb.adhesive NO-LOCK NO-ERROR.
        IF AVAIL ITEM AND index("G,S,T",ITEM.mat-type) > 0 AND ITEM.i-no <> "No Joint"
        THEN eb.lin-in = eb.dep.
     end.  /* avail style */ 
     run calc-pass.
     run calc-blank-size.

     /*if eb.gluelap <> 0 then eb.lin-in = eb.dep.    old  new logic in style block */
     if not avail cust then find cust where cust.company = eb.company and
                                 cust.cust-no = eb.cust-no
                                 no-lock no-error.

     RELEASE bf-eb.
     IF est.est-type EQ 6 THEN
     FIND FIRST bf-eb
         WHERE bf-eb.company EQ eb.company
           AND bf-eb.est-no  EQ eb.est-no
           AND bf-eb.form-no EQ 0
           AND bf-eb.procat  NE ""
         NO-LOCK NO-ERROR.

     RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
     if v-shiptorep-log then	/* task 05301401 */
     RUN sys/inc/getsmncm-2.p (eb.cust-no, INPUT-OUTPUT eb.sman,
                             (IF AVAIL bf-eb THEN bf-eb.procat ELSE eb.procat),
                             ld-markup,
                             OUTPUT eb.comm,eb.ship-id).
     else
         RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman,
                             (IF AVAIL bf-eb THEN bf-eb.procat ELSE eb.procat),
                             ld-markup,
                             OUTPUT eb.comm).
     
        FIND FIRST item NO-LOCK
            WHERE item.company EQ gcompany
              AND item.i-no    EQ ef.board
            NO-ERROR.
        if avail item then do:
           assign ef.i-code = item.i-code
                  ef.flute = item.flute
                  ef.test = item.reg-no
                  ef.weight = item.basis-w.
           RUN sys/ref/uom-rm.p (item.mat-type, output uom-list).
           IF uom-list NE "" THEN ef.cost-uom = ENTRY(1,uom-list).
           if item.i-code = "R" then assign ef.lsh-len = item.s-len
                                            ef.lsh-wid = item.s-wid
                                            ef.gsh-wid = item.s-wid.
           if item.r-wid <> 0 then assign ef.roll = true
                                          ef.roll-wid = item.r-wid
                                          ef.lsh-wid = item.r-wid
                                          ef.gsh-wid = item.r-wid.  
           FIND FIRST e-item OF item NO-LOCK NO-ERROR.
           IF AVAIL e-item THEN ef.cost-uom = e-item.std-uom.                                        
        end.          
     /*end.  /* avail reftable */*/

     if style.material[4] ne "" then do:  /* adder*/ 
               find first item  where item.company eq cocode
                     and item.i-no    eq style.material[4]
                   no-lock no-error.
               if avail item then
                  do i = 1 to 6:  
                     if ef.adder[i] = "" then do:
                         assign ef.adder[i]     = item.i-no
                                ef.adder[i + 6] = item.est-dscr
                                ef.weight       = ef.weight + item.basis-w.
                         leave.       
                     end.           
                  end.
            end.
      if style.material[5] ne "" then do:  /* leaf label */
               find first item  where item.company eq cocode
                                  and item.i-no    eq style.material[5]
                   no-lock no-error.
               if avail item then /*leaf-block:
               for each ef where ef.company eq xest.company and
                                 ef.est-no = xest.est-no,
                   first eb of ef no-lock:  */
                  do i = 1 to 2:
                     if ef.leaf[i] = "" then do:
                        assign ef.leaf-snum[i] = ef.form-no
                               ef.leaf-bnum[i] = 1
                               ef.leaf[i]      = item.i-no
                               ef.leaf-dscr[i] = item.est-dscr
                               ef.leaf-l[i] = eb.t-len
                               ef.leaf-w[i] = eb.t-wid.
                         leave.
                     end.   
                  end.
            /*   end. */
     end.

     IF ll-add-set-part EQ NO AND ll-add-set-part-2 EQ NO THEN
        ef.xgrain = "N".
     ELSE
        ef.xgrain = v-assem-grain.

     find xest where recid(xest) = recid(est).
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).
     if lv-foam then assign xeb.t-wid = xeb.wid
                         xeb.t-len = xeb.len
                         xeb.t-dep = xeb.dep
                         xef.cost-uom = "BF".
     run create-inst.
     run create-prep.

     def var lv-cas-pal as dec no-undo.
     def var lv-tr-cnt as int no-undo.
     def var lv-numstack as int no-undo.
     def var lv-stackcode as cha no-undo.
     def var lv-error as log no-undo.
     run cec/kpallet.p (recid(xeb), output lv-cas-pal, output lv-tr-cnt,
                        output lv-numstack, output lv-stackcode, output lv-error).

     if lv-error then do:
        message "An error occured while attempting to calculate the number of pallets. "
                skip
                "Please review any previous error messages for more information." 
                 view-as alert-box error.
     end.

     ELSE DO:
       lv-layers = lv-cas-pal / lv-numstack.
       {sys/inc/roundup.i lv-layers}

       ASSIGN
        eb.cas-pal    = lv-cas-pal
        eb.tr-cnt     = lv-tr-cnt
        eb.tr-cas     = lv-layers
        eb.stacks     = lv-numstack
        eb.stack-code = lv-stackcode.
     END.

     RUN calc-layout (YES).
  END.  /* not new or first */

  ELSE
  IF lv-hld-wid   NE eb.wid   OR
     lv-hld-len   NE eb.len   OR
     lv-hld-dep   NE eb.dep   OR
     lv-hld-style NE eb.style THEN DO:

    RUN calc-blank-size.
    RUN calc-layout (NO).
  END.
END.
