/* cec/estitm1.i size limit */
DEF VAR ld-markup AS DEC NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.


IF NOT ll-is-copy-record AND NOT ll-copied-from-eb THEN DO:
  IF (ll-new-record OR is-first-record)              THEN DO:
     IF eb.stock-no = "" THEN DO:
        FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany AND
                                 ce-ctrl.loc = gloc
                                 NO-LOCK NO-ERROR.
        ASSIGN
        eb.cas-no = ce-ctrl.def-case
        eb.tr-no = ce-ctrl.def-pal.      
     END.
     FIND FIRST cust WHERE cust.company = gcompany AND
                     cust.cust-no = eb.cust-no
                     NO-LOCK NO-ERROR.   
     RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
     IF cPackCodeOverride GT "" THEN 
         eb.cas-no = cPackCodeOverride.
     ASSIGN     
     eb.tr-no = IF AVAIL shipto AND shipto.pallet <> "" THEN shipto.pallet ELSE IF AVAIL cust AND cust.pallet <> "" THEN cust.pallet ELSE eb.tr-no.      

     /* get default values from rm table */
     FIND item WHERE item.company = eb.company AND
                     item.i-no = eb.cas-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)         
                              .
     FIND FIRST item WHERE item.company = eb.company AND
                     item.i-no = eb.tr-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)
                               .
     FIND FIRST style WHERE style.company = est.company AND
                      style.style = eb.style:screen-value IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.
     IF AVAIL style THEN DO:
        ASSIGN eb.adhesive = style.material[7]
               eb.gluelap = style.dim-gl
               eb.fpanel = style.dim-pan5
               eb.lock = style.dim-fit
               eb.tuck = style.dim-tk.

        FIND FIRST ITEM WHERE ITEM.company = eb.company 
                          AND ITEM.i-no = eb.adhesive NO-LOCK NO-ERROR.
        IF AVAIL ITEM AND index("G,S,T",ITEM.mat-type) > 0 AND ITEM.i-no <> "No Joint"
        THEN eb.lin-in = eb.dep.
     END.  /* avail style */ 
     RUN calc-pass.
     RUN calc-blank-size.

     /*if eb.gluelap <> 0 then eb.lin-in = eb.dep.    old  new logic in style block */
     IF NOT AVAIL cust THEN FIND cust WHERE cust.company = eb.company AND
                                 cust.cust-no = eb.cust-no
                                 NO-LOCK NO-ERROR.

     RELEASE bf-eb.
     IF est.est-type EQ 6 THEN
     FIND FIRST bf-eb
         WHERE bf-eb.company EQ eb.company
           AND bf-eb.est-no  EQ eb.est-no
           AND bf-eb.form-no EQ 0
           AND bf-eb.procat  NE ""
         NO-LOCK NO-ERROR.

     RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
     IF v-shiptorep-log THEN	/* task 05301401 */
     RUN sys/inc/getsmncm-2.p (eb.cust-no, INPUT-OUTPUT eb.sman,
                             (IF AVAIL bf-eb THEN bf-eb.procat ELSE eb.procat),
                             ld-markup,
                             OUTPUT eb.comm,eb.ship-id).
     ELSE
         RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman,
                             (IF AVAIL bf-eb THEN bf-eb.procat ELSE eb.procat),
                             ld-markup,
                             OUTPUT eb.comm).
     
        FIND FIRST item NO-LOCK
            WHERE item.company EQ gcompany
              AND item.i-no    EQ ef.board
            NO-ERROR.
        IF AVAIL item THEN DO:
           ASSIGN ef.i-code = item.i-code
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

     IF style.material[4] NE "" THEN DO:  /* adder*/ 
               FIND FIRST item  WHERE item.company EQ cocode
                     AND item.i-no    EQ style.material[4]
                   NO-LOCK NO-ERROR.
               IF AVAIL item THEN
                  DO i = 1 TO 6:  
                     IF ef.adder[i] = "" THEN DO:
                         ASSIGN ef.adder[i]     = item.i-no
                                ef.adder[i + 6] = item.est-dscr
                                ef.weight       = ef.weight + item.basis-w.
                         LEAVE.       
                     END.           
                  END.
            END.
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
                               ef.leaf-w[i] = eb.t-wid.
                         LEAVE.
                     END.   
                  END.
            /*   end. */
     END.

     IF ll-add-set-part EQ NO AND ll-add-set-part-2 EQ NO THEN
        ef.xgrain = "N".
     ELSE
        ef.xgrain = v-assem-grain.

     FIND xest WHERE RECID(xest) = recid(est).
     FIND xef WHERE RECID(xef) = recid(ef).
     FIND xeb WHERE RECID(xeb) = recid(eb).
     IF lv-foam THEN ASSIGN xeb.t-wid = xeb.wid
                         xeb.t-len = xeb.len
                         xeb.t-dep = xeb.dep
                         xef.cost-uom = "BF".
     RUN create-inst.
     RUN create-prep.

     DEF VAR lv-cas-pal AS DEC NO-UNDO.
     DEF VAR lv-tr-cnt AS INT NO-UNDO.
     DEF VAR lv-numstack AS INT NO-UNDO.
     DEF VAR lv-stackcode AS cha NO-UNDO.
     DEF VAR lv-error AS LOG NO-UNDO.
     RUN cec/kpallet.p (RECID(xeb), OUTPUT lv-cas-pal, OUTPUT lv-tr-cnt,
                        OUTPUT lv-numstack, OUTPUT lv-stackcode, OUTPUT lv-error).

     IF lv-error THEN DO:
        MESSAGE "An error occured while attempting to calculate the number of pallets. "
                SKIP
                "Please review any previous error messages for more information." 
                 VIEW-AS ALERT-BOX ERROR.
     END.

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
