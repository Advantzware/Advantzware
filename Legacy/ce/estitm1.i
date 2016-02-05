
  if not ll-is-copy-record and not ll-copied-from-eb THEN DO:
    IF (ll-new-record or is-first-record) then do:
      if eb.stock-no = "" then do:
        find first ce-ctrl where ce-ctrl.company = gcompany and
                                 ce-ctrl.loc = gloc
                                 no-lock no-error.
        eb.cas-no = ce-ctrl.def-case.
        eb.tr-no = ce-ctrl.def-pal.      
      end.
      find cust where cust.company = gcompany and
                      cust.cust-no = eb.cust-no
                      no-lock no-error.
      eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no.
      eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no.      
      /* get default values from rm table */
      find item where item.company = eb.company and
                      item.i-no = eb.cas-no
               no-lock no-error.
      if avail item then assign /*eb.cas-cost:Screen-value = */
                               eb.cas-cnt = (item.box-case)
                               eb.cas-len = (item.case-l)
                               eb.cas-wid = (item.case-w)
                               eb.cas-dep = (item.case-d)
                               eb.cas-pal = (item.case-pall)
                               eb.cas-wt = (item.avg-w)         
                               .
      find item where item.company = eb.company and
                      item.i-no = eb.tr-no
               no-lock no-error.
      if avail item then assign /*eb.cas-cost:Screen-value = */
                               eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)
                               .
      ASSIGN
       eb.tr-cnt = eb.cas-cnt * eb.cas-pal
       eb.tr-cas = 1.

      find style where style.company = est.company and
                       style.style = eb.style:screen-value in browse {&browse-name}
                       no-lock no-error.
      if avail style then do:
        assign eb.adhesive = style.material[7]
               eb.gluelap = style.dim-gl
               eb.k-len = style.dim-dkl
               eb.k-wid = style.dim-dkw
               eb.fpanel = style.dim-pan5
               eb.lock = style.dim-fit
               eb.tuck = style.dim-tk.                 
               .
        FIND FIRST ITEM WHERE ITEM.company = eb.company 
                          AND ITEM.i-no = eb.adhesive NO-LOCK NO-ERROR.
        IF AVAIL ITEM AND index("G,S,T",ITEM.mat-type) > 0 AND ITEM.i-no <> "No Joint"
        THEN eb.lin-in = eb.dep.
      end.  /* avail style */ 

      run calc-pass.
      run calc-blank-size.

      /*if eb.gluelap <> 0 then eb.lin-in = eb.dep.    old logic, new in style block*/

      if not avail cust then find cust where cust.company = eb.company and
                                  cust.cust-no = eb.cust-no
                                  no-lock no-error.

      RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).

      if v-shiptorep-log then  /* task 05301401 */
      RUN sys/inc/getsmncm-2.p (eb.cust-no, INPUT-OUTPUT eb.sman, eb.procat, ld-markup,
                              OUTPUT eb.comm,eb.ship-id).
	else
	  RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman, eb.procat, ld-markup,
                              OUTPUT eb.comm).
		
      /*find first reftable where reftable.reftable = "STYFLU"
                           and reftable.company = eb.style
                           and (reftable.loc = eb.flute or lv-foam)
                           and reftable.code = "BOARD"
                        no-lock no-error.
     if avail reftable and reftable.dscr <> "" then do:
        find first item where item.company = gcompany and
                              item.i-no = reftable.dscr
                              no-lock no-error.*/
        FIND FIRST item NO-LOCK
            WHERE item.company EQ gcompany
              AND item.i-no    EQ ef.board
            NO-ERROR.
        if avail item then do:
           assign /*ef.board = item.i-no */
                  ef.i-code = item.i-code
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
                               ef.leaf-w[i] = eb.t-wid
                               .
                         leave.
                     end.   
                  end.
            /*   end. */
      end.

      RUN calc-layout (YES).
    END. /* not new or first */

    ELSE
    IF lv-hld-wid   NE eb.wid   OR
       lv-hld-len   NE eb.len   OR
       lv-hld-dep   NE eb.dep   OR
       lv-hld-style NE eb.style THEN DO:
      RUN calc-blank-size.
      RUN calc-layout (NO).
    END.
  END.
