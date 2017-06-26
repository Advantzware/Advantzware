
  IF NOT ll-is-copy-record AND NOT ll-copied-from-eb THEN DO:
    IF (ll-new-record OR is-first-record) THEN DO:
      IF eb.stock-no = "" THEN DO:
        FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany AND
                                 ce-ctrl.loc = gloc
                                 NO-LOCK NO-ERROR.
        eb.cas-no = ce-ctrl.def-case.
        eb.tr-no = ce-ctrl.def-pal.      
      END.
      FIND cust WHERE cust.company = gcompany AND
                      cust.cust-no = eb.cust-no
                      NO-LOCK NO-ERROR.
      RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
      IF cPackCodeOverride GT "" THEN 
          eb.cas-no = cPackCodeOverride.
      eb.tr-no = IF AVAIL shipto AND shipto.pallet <> "" THEN shipto.pallet ELSE IF AVAIL cust AND cust.pallet <> "" THEN cust.pallet ELSE eb.tr-no.      
      /* get default values from rm table */
      FIND item WHERE item.company = eb.company AND
                      item.i-no = eb.cas-no
               NO-LOCK NO-ERROR.
      IF AVAIL item THEN ASSIGN /*eb.cas-cost:Screen-value = */
                               eb.cas-cnt = (item.box-case)
                               eb.cas-len = (item.case-l)
                               eb.cas-wid = (item.case-w)
                               eb.cas-dep = (item.case-d)
                               eb.cas-pal = (item.case-pall)
                               eb.cas-wt = (item.avg-w)         
                               .
      FIND item WHERE item.company = eb.company AND
                      item.i-no = eb.tr-no
               NO-LOCK NO-ERROR.
      IF AVAIL item THEN ASSIGN /*eb.cas-cost:Screen-value = */
                               eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)
                               .
      ASSIGN
       eb.tr-cnt = eb.cas-cnt * eb.cas-pal
       eb.tr-cas = 1.

      FIND style WHERE style.company = est.company AND
                       style.style = eb.style:screen-value IN BROWSE {&browse-name}
                       NO-LOCK NO-ERROR.
      IF AVAIL style THEN DO:
        ASSIGN eb.adhesive = style.material[7]
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
      END.  /* avail style */ 

      RUN calc-pass.
      RUN calc-blank-size.

      /*if eb.gluelap <> 0 then eb.lin-in = eb.dep.    old logic, new in style block*/

      IF NOT AVAIL cust THEN FIND cust WHERE cust.company = eb.company AND
                                  cust.cust-no = eb.cust-no
                                  NO-LOCK NO-ERROR.

      RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).

      IF v-shiptorep-log THEN  /* task 05301401 */
      RUN sys/inc/getsmncm-2.p (eb.cust-no, INPUT-OUTPUT eb.sman, eb.procat, ld-markup,
                              OUTPUT eb.comm,eb.ship-id).
	ELSE
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
        IF AVAIL item THEN DO:
           ASSIGN /*ef.board = item.i-no */
                  ef.i-code = item.i-code
                  ef.flute = item.flute
                  ef.test = item.reg-no
                  ef.weight = item.basis-w.
           RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT uom-list).
           IF uom-list NE "" THEN ef.cost-uom = ENTRY(1,uom-list).
           IF item.i-code = "R" THEN ASSIGN ef.lsh-len = item.s-len
                                            ef.lsh-wid = item.s-wid
                                            ef.gsh-wid = item.s-wid.
           IF item.r-wid <> 0 THEN ASSIGN ef.roll = TRUE
                                          ef.roll-wid = item.r-wid
                                          ef.lsh-wid = item.r-wid
                                          ef.gsh-wid = item.r-wid.   
           FIND FIRST e-item OF item NO-LOCK NO-ERROR.
           IF AVAIL e-item THEN ef.cost-uom = e-item.std-uom.
        END.          
     /*end.  /* avail reftable */*/

      IF style.material[5] NE "" THEN DO:  /* leaf label */
               FIND FIRST item  WHERE item.company EQ cocode
                                  AND item.i-no    EQ style.material[5]
                   NO-LOCK NO-ERROR.
               IF AVAIL item THEN /*leaf-block:
               for each ef where ef.company eq xest.company and
                                 ef.est-no = xest.est-no,
                   first eb of ef no-lock:  */
                  DO i = 1 TO 2:
                     IF ef.leaf[i] = "" THEN DO:
                        ASSIGN ef.leaf-snum[i] = ef.form-no
                               ef.leaf-bnum[i] = 1
                               ef.leaf[i]      = item.i-no
                               ef.leaf-dscr[i] = item.est-dscr
                               ef.leaf-l[i] = eb.t-len
                               ef.leaf-w[i] = eb.t-wid
                               .
                         LEAVE.
                     END.   
                  END.
            /*   end. */
      END.

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
