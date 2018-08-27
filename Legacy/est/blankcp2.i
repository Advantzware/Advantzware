
DEF BUFFER xest-flm    FOR est-flm.
DEF BUFFER xref-tab    FOR reftable.
DEF BUFFER xef-nsh     FOR ef-nsh.
DEF BUFFER bf-notes    FOR notes.
DEF BUFFER blankcp2-eb FOR eb.
DEF BUFFER xest-prep FOR est-prep.
DEF BUFFER bf-est-op FOR est-op.

DEF VAR j AS INT NO-UNDO.
DEF VAR ll-copied-from-eb AS LOG INIT NO NO-UNDO.
DEF VAR ll-new-adder AS LOG NO-UNDO.
DEF VAR v-next AS INT NO-UNDO.

IF lv-copied NE ? THEN DO:
  FIND CURRENT eb EXCLUSIVE-LOCK NO-ERROR.
  FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR.

  FIND itemfg WHERE ROWID(itemfg) EQ lv-copied NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN
    ASSIGN
     eb.part-dscr2 = itemfg.part-dscr1
     eb.plate-no   = itemfg.plate-no
     eb.die-no     = itemfg.die-no
     eb.cad-no     = itemfg.cad-no
     eb.spc-no     = itemfg.spc-no
     eb.upc-no     = itemfg.upc-no.

  ELSE
  FIND bf-eb WHERE ROWID(bf-eb) EQ lv-copied NO-LOCK NO-ERROR.
  IF AVAIL bf-eb THEN DO:
    FIND FIRST bf-est
        WHERE bf-est.company EQ bf-eb.company
          AND bf-est.est-no  EQ bf-eb.est-no
        NO-LOCK NO-ERROR.

    BUFFER-COPY bf-eb EXCEPT est-no e-num eqty est-type form-no blank-no cust-no ship-id rec_key ord-no TO eb.

    ll-copied-from-eb = YES.

    if not avail b-ef then
    for each est-flm
        where est-flm.company eq bf-eb.company
          and est-flm.est-no  eq bf-eb.est-no
          and est-flm.snum    eq bf-eb.form-no
          and est-flm.bnum    eq bf-eb.blank-no
        no-lock:

      find last xest-flm
          where xest-flm.company eq est.company
            and xest-flm.est-no  eq est.est-no
          use-index est-qty no-lock no-error.
      j = if avail xest-flm then xest-flm.line else 0.

      create xest-flm.    
      buffer-copy est-flm except rec_key to xest-flm
      assign
       xest-flm.e-num  = eb.e-num
       xest-flm.est-no = eb.est-no
       xest-flm.line   = j + 1
       xest-flm.snum   = eb.form-no
       xest-flm.bnum   = eb.blank-no.
    end.

    release bf-ef.

    IF NOT AVAIL b-ef THEN      /* if not running est/d-selest.w */
    FIND FIRST bf-ef
        WHERE bf-ef.company EQ bf-eb.company
          AND bf-ef.est-no  EQ bf-eb.est-no
          AND bf-ef.form-no EQ bf-eb.form-no
        NO-LOCK NO-ERROR.

    IF AVAIL bf-ef                                        AND
       NOT CAN-FIND(FIRST est-flm
                    WHERE est-flm.company EQ eb.company
                      AND est-flm.est-no  EQ eb.est-no
                      AND est-flm.snum    EQ eb.form-no
                      AND est-flm.bnum    EQ eb.blank-no) THEN
    do i = 1 to 4:
      if bf-ef.leaf[i] ne ""                                               and
         (bf-ef.leaf-bnum[i] eq bf-eb.blank-no or bf-ef.leaf-bnum[i] eq 0) then
      do j = 1 to 4:
        if ef.leaf[j] eq "" then do:
          assign
           ef.leaf[j]      = bf-ef.leaf[i]
           ef.leaf-dscr[j] = bf-ef.leaf-dscr[i]
           ef.leaf-cost[j] = bf-ef.leaf-cost[i]
           ef.leaf-snum[j] = ef.form-no
           ef.leaf-bnum[j] = if bf-ef.leaf-bnum[i] eq 0 then 0 else bf-eb.blank-no
           ef.leaf-w[j]    = bf-ef.leaf-w[i]
           ef.leaf-l[j]    = bf-ef.leaf-l[i]
           ef.leaf-o1[j]   = bf-ef.leaf-o1[i]
           ef.leaf-o2[j]   = bf-ef.leaf-o2[i]
           ef.leaf-o3[j]   = bf-ef.leaf-o3[i]
           ef.leaf-o4[j]   = bf-ef.leaf-o4[i].
         
          leave.
        end.  
      end.
    end.
  
    if avail bf-ef then
    do i = 1 to 5:
      if bf-ef.mis-cost[i] ne ""                                         and
         (bf-ef.mis-bnum[i] eq bf-eb.blank-no or bf-ef.mis-bnum[i] eq 0) then
      do j = 1 to 5:
        if ef.mis-cost[j] eq "" then do:
          assign
           ef.mis-cost[j]  = bf-ef.mis-cost[i]
           ef.mis-snum[j]  = ef.form-no
           ef.mis-bnum[j]  = if bf-ef.mis-bnum[i] eq 0 then 0 else bf-eb.blank-no
           ef.mis-matf[j]  = bf-ef.mis-matf[i]
           ef.mis-labf[j]  = bf-ef.mis-labf[i]
           ef.mis-matm[j]  = bf-ef.mis-matm[i]
           ef.mis-labm[j]  = bf-ef.mis-labm[i]
           ef.mis-simon[j] = bf-ef.mis-simon[i]
           ef.mis-mkup[j]  = bf-ef.mis-mkup[i].
           
          for each reftable
              where reftable.reftable           eq "EST-MISC"
                and reftable.company            eq bf-ef.company
                and reftable.loc                eq bf-ef.loc
                and reftable.code               eq trim(bf-ef.est-no) +
                                                   string(bf-ef.form-no,"/99")
                and substr(reftable.code2,8,2)  eq string(i,"99")
                and (substr(reftable.code2,1,7) eq "MAT-QTY" or
                     substr(reftable.code2,1,7) eq "MAT-CST" or
                     substr(reftable.code2,1,7) eq "LAB-QTY" or
                     substr(reftable.code2,1,7) eq "LAB-CST")
              no-lock:
            
            create xref-tab.
            buffer-copy reftable except rec_key to xref-tab
            assign
             xref-tab.company = ef.company
             xref-tab.loc     = ef.loc
             xref-tab.code    = trim(ef.est-no) + string(ef.form-no,"/99")
             xref-tab.code2   = substr(reftable.code2,1,7) + string(j,"99").
          end.
                 
          leave.
        end.
      end.
    end.
    
    if avail bf-ef then
    do i = 1 to 8:
      if bf-ef.spec-no[i] ne "" then 
      do j = 1 to 8:
        if ef.spec-no[j] eq "" then do:
          assign
           ef.spec-no[j]   = bf-ef.spec-no[i]
           ef.spec-dscr[j] = bf-ef.spec-dscr[i]
           ef.spec-uom[j]  = bf-ef.spec-uom[i]
           ef.spec-qty[j]  = bf-ef.spec-qty[i].
         
          leave.
        end.
      end.
    end.
    
    IF AVAIL bf-ef THEN
    DO i = 1 TO EXTENT(bf-ef.adder) / 2:
      IF bf-ef.adder[i] NE "" THEN DO:
        ll-new-adder = YES.

        DO j = 1 TO EXTENT(ef.adder) / 2:
          IF ef.adder[j] EQ bf-ef.adder[i] THEN DO:
            ll-new-adder = NO.
            LEAVE.
          END.
        END.

        IF ll-new-adder THEN
        DO j = 1 TO EXTENT(ef.adder) / 2:
          IF ef.adder[j] EQ "" THEN DO:
            ASSIGN
             ef.adder[j]     = bf-ef.adder[i]
             ef.adder[j + 6] = bf-ef.adder[i + 6].
         
            LEAVE.
          END.
        END.
      END.
    END.

    IF NOT CAN-FIND(FIRST ef-nsh
                    WHERE ef-nsh.company EQ eb.company
                      AND ef-nsh.est-no  EQ eb.est-no
                      AND ef-nsh.form-no EQ eb.form-no) THEN
    FOR EACH ef-nsh
        WHERE ef-nsh.company EQ bf-eb.company
          AND ef-nsh.est-no  EQ bf-eb.est-no
          AND ef-nsh.form-no EQ bf-eb.form-no
        NO-LOCK:
      CREATE xef-nsh.
      BUFFER-COPY ef-nsh EXCEPT rec_key TO xef-nsh
      ASSIGN
       xef-nsh.company = eb.company
       xef-nsh.est-no  = eb.est-no
       xef-nsh.eqty    = eb.eqty
       xef-nsh.form-no = eb.form-no.
    END.

    FOR EACH est-prep WHERE
        est-prep.company EQ bf-eb.company AND
        est-prep.est-no EQ bf-eb.est-no AND
        est-prep.s-num EQ bf-eb.form-no
        NO-LOCK:

        IF est-prep.b-num EQ bf-eb.blank-no OR
           est-prep.b-num EQ 0 THEN
           DO:
              FOR EACH xest-prep WHERE
                  xest-prep.company EQ est.company AND
                  xest-prep.est-no  EQ est.est-no
                  USE-INDEX est-qty NO-LOCK
                  BY xest-prep.line DESC:
                  LEAVE.
              END.

              v-next = (IF AVAIL xest-prep THEN xest-prep.line ELSE 0) + 1.

              CREATE xest-prep.
              BUFFER-COPY est-prep EXCEPT b-num LINE e-num company est-no eqty s-num rec_key TO
                          xest-prep
                  ASSIGN
                     xest-prep.company = eb.company
                     xest-prep.est-no  = eb.est-no
                     xest-prep.eqty    = eb.eqty
                     xest-prep.s-num = eb.form-no
                     xest-prep.b-num = IF est-prep.b-num EQ 0 THEN 0
                                       ELSE eb.blank-no
                     xest-prep.e-num = est.e-num
                     xest-prep.LINE  = v-next.

              IF xest-prep.mat-type = "P" AND est.ord-no <> 0 AND xest-prep.simon EQ "S" AND
                 xest-prep.amtz    EQ 100 THEN 
              DO:
                 FIND FIRST oe-ordm WHERE
                      oe-ordm.company = est.company AND
                      oe-ordm.ord-no = est.ord-no AND
                      oe-ordm.charge = xest-prep.CODE
                       NO-ERROR.
                
                 IF AVAIL oe-ordm AND 
                     (oe-ordm.miscType <> 1 OR
                      oe-ordm.estPrepEqty <> xest-prep.eqty OR
                      oe-ordm.estPrepLine <> xest-prep.line)      
                     
                  THEN DO:
                
                    ASSIGN 
                         oe-ordm.miscType = 1
                         oe-ordm.estPrepEqty   = xest-prep.eqty
                         oe-ordm.estPrepLine   = xest-prep.line
                         oe-ordm.est-no  = xest-prep.est-no.    
                     RELEASE oe-ordm.
                  END.
              END.

              RELEASE xest-prep.
           END.
    END.

    IF program-name(3) MATCHES "*oe/v-ord*" THEN
    FOR EACH est-op WHERE
        est-op.company EQ bf-eb.company AND
        est-op.est-no EQ bf-eb.est-no AND
        est-op.s-num EQ bf-eb.form-no AND
        est-op.line LT 500
        NO-LOCK:
 
        IF est-op.b-num EQ bf-eb.blank-no THEN
           DO:
              v-next = 1 .
              FOR EACH bf-est-op NO-LOCK
                  WHERE bf-est-op.company EQ est.company
                  AND bf-est-op.est-no  EQ est.est-no
                  AND bf-est-op.line    LT 500
                  BY bf-est-op.line DESCENDING:
                  v-next = bf-est-op.line + 1.
                  LEAVE.
              END.
              
              CREATE bf-est-op.
              BUFFER-COPY est-op EXCEPT s-num LINE company est-no qty rec_key TO
                          bf-est-op
                  ASSIGN
                     bf-est-op.company = eb.company
                     bf-est-op.est-no  = eb.est-no
                     bf-est-op.qty    = eb.eqty
                     bf-est-op.s-num = eb.form-no
                     bf-est-op.LINE  = v-next.
              
              RELEASE bf-est-op.
           END.
    END.

    DO lj = 1 TO 2:
      FOR EACH reftable
          WHERE reftable.reftable EQ "ce/v-est3.w Unit#" + TRIM(STRING(lj - 1,">"))
            AND reftable.company  EQ bf-eb.company
            AND reftable.loc      EQ bf-eb.est-no
            AND reftable.code     EQ STRING(bf-eb.form-no,"9999999999")
            AND reftable.code2    EQ STRING(bf-eb.blank-no,"9999999999")
          NO-LOCK:

        FIND FIRST b-ref
            WHERE b-ref.reftable EQ reftable.reftable
              AND b-ref.company  EQ eb.company
              AND b-ref.loc      EQ eb.est-no
              AND b-ref.code     EQ STRING(eb.form-no,"9999999999")
              AND b-ref.code2    EQ STRING(eb.blank-no,"9999999999")
            NO-ERROR.
        IF NOT AVAIL b-ref THEN DO:
          CREATE b-ref.
          ASSIGN
           b-ref.reftable = reftable.reftable
           b-ref.company  = eb.company
           b-ref.loc      = eb.est-no
           b-ref.code     = STRING(eb.form-no,"9999999999")
           b-ref.code2    = STRING(eb.blank-no,"9999999999").
        END.

        DO li = 1 TO 12:
          b-ref.val[li] = reftable.val[li].
        END.

        b-ref.dscr = reftable.dscr.

        LEAVE.
      END.
    END.

    IF AVAIL bf-est                                     AND
       AVAIL bf-ef                                      AND
       (cecopy-cha EQ "Both"                         OR
        (cecopy-cha EQ "Fold" AND est.est-type LE 4) OR
        (cecopy-cha EQ "Corr" AND est.est-type GE 5))   THEN
    FOR EACH bf-notes
        WHERE bf-notes.rec_key        EQ bf-est.rec_key
          AND (bf-notes.note_form_no  EQ bf-ef.form-no OR
               (bf-notes.note_form_no EQ 0 AND
                ROWID(est)            NE ROWID(bf-est)))
          AND bf-notes.note_type      NE "S"
        NO-LOCK:

      FIND FIRST notes
          WHERE notes.rec_key      EQ est.rec_key
            AND notes.note_form_no EQ (IF bf-notes.note_form_no EQ 0 THEN 0
                                       ELSE ef.form-no)
            AND notes.note_type    EQ bf-notes.note_type
            AND notes.note_code    EQ bf-notes.note_code
          NO-ERROR.

      IF NOT AVAIL notes THEN DO:
        CREATE notes.
        BUFFER-COPY bf-notes EXCEPT rec_key note_form_no note_date
                                    note_time user_id viewed
                 TO notes
        ASSIGN
         notes.rec_key      = est.rec_key
         notes.note_date    = TODAY
         notes.note_time    = TIME
         notes.user_id      = USERID("nosweat")
         notes.note_form_no = ef.form-no.
      END.

      ELSE
      IF INDEX(notes.note_text,TRIM(bf-notes.note_text)) EQ 0 THEN
        notes.note_text = (IF TRIM(notes.note_text) EQ "" THEN ""
                           ELSE (TRIM(notes.note_text) + CHR(10))) +
                          TRIM(bf-notes.note_text).
    END.    

    IF AVAIL bf-ef AND ll-form THEN
      ASSIGN
       ef.m-code   = bf-ef.m-code
       ef.m-dscr   = bf-ef.m-dscr
       ef.lsh-wid  = bf-ef.lsh-wid
       ef.lsh-len  = bf-ef.lsh-len
       ef.xgrain   = bf-ef.xgrain
       ef.i-code   = bf-ef.i-code
       ef.flute    = bf-ef.flute
       ef.test     = bf-ef.test
       ef.cost-uom = bf-ef.cost-uom
       ef.cost-msh = bf-ef.cost-msh
       ef.weight   = bf-ef.weight
       ef.fr-uom   = bf-ef.fr-uom
       ef.fr-msh   = bf-ef.fr-msh
       ef.nc       = bf-ef.nc
       ef.gsh-wid  = bf-ef.gsh-wid
       ef.gsh-len  = bf-ef.gsh-len
       ef.gsh-dep  = bf-ef.gsh-dep
       ef.nsh-wid  = bf-ef.nsh-wid
       ef.nsh-len  = bf-ef.nsh-len
       ef.nsh-dep  = bf-ef.nsh-dep
       ef.n-out    = bf-ef.n-out
       ef.n-out-l  = bf-ef.n-out-l
       ef.n-out-d  = bf-ef.n-out-d
       ef.n-cuts   = bf-ef.n-cuts
       ef.trim-w   = bf-ef.trim-w
       ef.trim-l   = bf-ef.trim-l
       ef.trim-d   = bf-ef.trim-d
       ef.die-in   = bf-ef.die-in
       eb.num-len  = bf-eb.num-len
       eb.num-wid  = bf-eb.num-wid
       eb.num-dep  = bf-eb.num-dep
       eb.num-up   = bf-eb.num-up.
  END.

  FIND CURRENT eb NO-LOCK NO-ERROR.
  FIND CURRENT ef NO-LOCK NO-ERROR.

  lv-copied = ?.
END.

ll-form = NO.

