/* -------------------------------------------------- fg/updfgcs1.p 02/99 JLF */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
DEF INPUT PARAM ip-recalc AS LOG NO-UNDO.

{sys/inc/VAR.i NEW SHARED}

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-fg-rcpth FOR fg-rcpth.
DEF BUFFER bf-fg-rcpth FOR fg-rcpth.
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.
DEF BUFFER b-fg-bin   FOR fg-bin.
DEFINE VARIABLE v-qty             AS INTEGER.
DEFINE VARIABLE v-binqty          AS INTEGER.
DEFINE VARIABLE v-date            AS DATE              INIT 01/01/0001.
DEFINE VARIABLE v-uom             LIKE fg-rcpth.pur-uom.
DEFINE VARIABLE v-cost-ea         AS DECIMAL.
DEFINE VARIABLE v-cost            AS DECIMAL           EXTENT 8.
DEFINE VARIABLE v-r-no            LIKE fg-rcpth.r-no.
DEFINE VARIABLE v-rec             AS LOGICAL           INIT NO.
DEFINE VARIABLE ll-all-empty-bins AS LOGICAL           NO-UNDO.
DEFINE VARIABLE v-last-fix        AS DECIMAL           NO-UNDO.
DEFINE VARIABLE v-last-lab        AS DECIMAL           NO-UNDO.
DEFINE VARIABLE v-last-mat        AS DECIMAL           NO-UNDO.
DEFINE VARIABLE v-last-var        AS DECIMAL           NO-UNDO.
DEFINE VARIABLE lvdTransDate      AS DATE              NO-UNDO.
DEFINE VARIABLE v-recalc-if1      AS LOGICAL           NO-UNDO.
DEFINE VARIABLE llFound           AS log.
DEFINE VARIABLE ldSaveCost        LIKE fg-rdtlh.cost EXTENT 6 NO-UNDO.
DEFINE VARIABLE cPgmStack         AS CHARACTER   NO-UNDO.
{fg/fullset.i NEW}
FUNCTION pgmStack RETURNS CHAR
  ( /* parameter-definitions */ )  FORWARD.

cPgmStack = pgmStack().
v-recalc-if1 = INDEX(cPgmStack,"fg/d-recost.w") > 0
OR INDEX(cPgmStack,"fg/fgpstall.w") > 0.

DISABLE TRIGGERS FOR LOAD OF itemfg.
    
    FIND itemfg WHERE RECID(itemfg) EQ ip-recid NO-LOCK.
    
    cocode = itemfg.company.
    
    FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ itemfg.company NO-LOCK NO-ERROR.
    
    IF itemfg.isaset AND itemfg.alloc THEN RUN fg/fullset.p (ROWID(itemfg)).
    
    FIND FIRST tt-fg-set NO-ERROR.
    
    IF AVAIL tt-fg-set AND tt-fg-set.part-no NE itemfg.i-no THEN DO:
      RELEASE tt-fg-set.
      
      FOR EACH tt-fg-set,
        
        FIRST b-itemfg
        WHERE b-itemfg.company EQ cocode
        AND b-itemfg.i-no    EQ tt-fg-set.part-no
        NO-LOCK:
        
        IF b-itemfg.prod-uom EQ "EA" THEN
        v-cost-ea = b-itemfg.avg-cost.
        ELSE
        RUN sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
        b-itemfg.avg-cost, OUTPUT v-cost-ea).
        
        v-cost[1] = v-cost[1] + (v-cost-ea * tt-fg-set.part-qty-dec).
        
        IF b-itemfg.prod-uom EQ "EA" THEN
        v-cost-ea = b-itemfg.last-cost.
        ELSE
        RUN sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
        b-itemfg.last-cost, OUTPUT v-cost-ea).
        
        v-cost[2] = v-cost[2] + (v-cost-ea * tt-fg-set.part-qty-dec).
        
        IF b-itemfg.prod-uom EQ "EA" THEN
        v-cost-ea = b-itemfg.std-mat-cost.
        ELSE
        RUN sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
        b-itemfg.std-mat-cost, OUTPUT v-cost-ea).
        
        v-cost[3] = v-cost[3] + (v-cost-ea * tt-fg-set.part-qty-dec).
        
        IF b-itemfg.prod-uom EQ "EA" THEN
        v-cost-ea = b-itemfg.std-lab-cost.
        ELSE
        RUN sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
        b-itemfg.std-lab-cost, OUTPUT v-cost-ea).
        
        v-cost[4] = v-cost[4] + (v-cost-ea * tt-fg-set.part-qty-dec).
        
        IF b-itemfg.prod-uom EQ "EA" THEN
        v-cost-ea = b-itemfg.std-var-cost.
        ELSE
        RUN sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
        b-itemfg.std-var-cost, OUTPUT v-cost-ea).
        
        v-cost[5] = v-cost[5] + (v-cost-ea * tt-fg-set.part-qty-dec).
        
        IF b-itemfg.prod-uom EQ "EA" THEN
        v-cost-ea = b-itemfg.std-fix-cost.
        ELSE
        RUN sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
        b-itemfg.std-fix-cost, OUTPUT v-cost-ea).
        
        v-cost[6] = v-cost[6] + (v-cost-ea * tt-fg-set.part-qty-dec).
      END.
      
      v-qty = 1.
    END.
    
    ELSE DO:
      IF ip-recalc THEN
      FOR EACH fg-bin
        WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no:
        
        IF fg-bin.job-no NE "" THEN
        RUN oe/fgbincst.p (ROWID(fg-bin)).
        ELSE DO:
          ASSIGN
          fg-bin.std-tot-cost = 0
          fg-bin.std-lab-cost = 0
          fg-bin.std-mat-cost = 0
          fg-bin.std-var-cost = 0
          fg-bin.std-fix-cost = 0.
          
          RUN fg/upfgbinc.p (ROWID(fg-bin)).
        END.
      END.
      
      ll-all-empty-bins = NOT CAN-FIND(FIRST fg-bin
      WHERE fg-bin.company EQ itemfg.company
      AND fg-bin.i-no    EQ itemfg.i-no
      AND fg-bin.qty     NE 0).
      
      FOR EACH fg-bin
        WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        BREAK BY fg-bin.i-no:
        
        IF fg-bin.pur-uom EQ "" THEN fg-bin.pur-uom = itemfg.prod-uom.
        
        IF fg-bin.pur-uom EQ "EA" THEN
        v-cost-ea = fg-bin.std-tot-cost.
        ELSE
        RUN sys/ref/convcuom.p(fg-bin.pur-uom, "EA", 0, 0, 0, 0,
        fg-bin.std-tot-cost, OUTPUT v-cost-ea).
        
        v-binqty = fg-bin.qty * (IF fg-bin.qty LT 0 THEN -1 ELSE 1).
        
        IF ll-all-empty-bins THEN v-binqty = 1.
        
        ASSIGN
        v-cost[1] = v-cost[1] + (v-binqty * v-cost-ea)
        v-qty     = v-qty + v-binqty.
        
        RELEASE fg-rcpth.
        RELEASE fg-rdtlh.
        /* btr 01252011 moved commented out code to bottom of program -
        Place here
        */
        
        IF v-recalc-if1 THEN
        DO:
          if llFound then do:
              /* Assign based on save values since code below */
              /* calculates per item for some reason          */
              
              assign 
                v-cost[1] = ldSaveCost[1] 
                v-cost[2] = ldSaveCost[2] 
                v-cost[3] = ldSaveCost[3] 
                v-cost[4] = ldSaveCost[4] 
                v-cost[5] = ldSaveCost[5] 
                v-cost[6] = ldSaveCost[6].
          end.
          else do:
              RELEASE b-fg-rcpth.
              RELEASE b-fg-rdtlh.

              /* This speeds up the for each below */
              lvdTransDate = ?.
              FIND LAST b-fg-rcpth
                WHERE b-fg-rcpth.company   EQ itemfg.company
                   AND b-fg-rcpth.i-no      EQ itemfg.i-no
                   AND b-fg-rcpth.rita-code EQ "R"
                USE-INDEX tran NO-LOCK NO-ERROR.
              IF AVAIL b-fg-rcpth THEN
                lvdTransDate = b-fg-rcpth.trans-date.

              FOR EACH b-fg-rcpth
                WHERE b-fg-rcpth.company      EQ itemfg.company
                    AND b-fg-rcpth.i-no       EQ itemfg.i-no
                    AND b-fg-rcpth.rita-code  EQ "R"
                    AND b-fg-rcpth.trans-date EQ lvdTransDate
                USE-INDEX tran NO-LOCK,
                
                FIRST b-fg-rdtlh
                WHERE b-fg-rdtlh.r-no    EQ b-fg-rcpth.r-no 
                    AND b-fg-rdtlh.rita-code EQ b-fg-rcpth.rita-code
                    AND b-fg-rdtlh.qty     GT 0
                NO-LOCK
                
                BY b-fg-rcpth.trans-date desc
                BY b-fg-rdtlh.trans-time DESC
                BY b-fg-rcpth.r-no       desc
                BY RECID(b-fg-rdtlh)     desc:
                                        
                
                llFound = yes.
                FIND FIRST bf-fg-rcpth
                    WHERE bf-fg-rcpth.company   EQ itemfg.company
                        AND bf-fg-rcpth.i-no      EQ itemfg.i-no
                        AND bf-fg-rcpth.rita-code EQ "R"
                        AND bf-fg-rcpth.r-no      NE b-fg-rcpth.r-no
                    USE-INDEX i-no NO-LOCK NO-ERROR.
                /* If more than one receipt, can use the "speed up" logic, */
                /* have to go through each record individually */
                
                IF AVAIL bf-fg-rcpth THEN
                    llFound = NO.
                v-cost[2] = b-fg-rdtlh.cost.
                FIND b-fg-bin WHERE b-fg-bin.company = b-fg-rdtlh.company
                    AND b-fg-bin.i-no    = b-fg-rdtlh.i-no
                    AND b-fg-bin.loc     = b-fg-rdtlh.loc
                    AND b-fg-bin.loc-bin = b-fg-rdtlh.loc-bin
                    AND b-fg-bin.tag     = b-fg-rdtlh.tag
                NO-LOCK NO-ERROR.
                
                IF AVAIL b-fg-bin AND  fg-ctrl.inv-meth EQ "L" THEN DO:
                  ASSIGN
                  v-last-fix        = b-fg-bin.std-fix-cost
                  v-last-lab        = b-fg-bin.std-lab-cost
                  v-last-mat        = b-fg-bin.std-mat-cost
                  v-last-var        = b-fg-bin.std-var-cost.
                  
                  IF itemfg.prod-uom EQ "EA" THEN
                  v-cost-ea = v-last-mat.
                  ELSE
                  RUN sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                  v-last-mat, OUTPUT v-cost-ea).
                  
                  v-cost[3] = (v-cost-ea * 1).
                  
                  IF itemfg.prod-uom EQ "EA" THEN
                  v-cost-ea = v-last-lab.
                  ELSE
                  RUN sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                  v-last-lab, OUTPUT v-cost-ea).
                  
                  v-cost[4] =  (v-cost-ea * 1).
                  
                  IF itemfg.prod-uom EQ "EA" THEN
                  v-cost-ea = v-last-var.
                  ELSE
                  RUN sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                  v-last-var, OUTPUT v-cost-ea).
                  
                  v-cost[5] = (v-cost-ea * 1).
                  
                  IF itemfg.prod-uom EQ "EA" THEN
                  v-cost-ea = v-last-fix.
                  ELSE
                  RUN sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                  v-last-fix, OUTPUT v-cost-ea).
                  
                  v-cost[6] = (v-cost-ea * 1).
                END.
                
                assign 
                  ldSaveCost[1] = v-cost[1]
                  ldSaveCost[2] = v-cost[2]
                  ldSaveCost[3] = v-cost[3]
                  ldSaveCost[4] = v-cost[4]
                  ldSaveCost[5] = v-cost[5]
                  ldSaveCost[6] = v-cost[6].
                LEAVE.
              END. /* each history */
          end. /* else do */         
        END.
        
        IF fg-ctrl.inv-meth EQ "A" THEN DO:
          
          IF fg-bin.pur-uom EQ "EA" THEN
          v-cost-ea = fg-bin.std-mat-cost.
          ELSE
          ASSIGN v-cost-ea =  fg-bin.std-mat-cost / 1000.
          
          v-cost[3] = v-cost[3] + (v-binqty * v-cost-ea).
          
          IF fg-bin.pur-uom EQ "EA" THEN
          v-cost-ea = fg-bin.std-lab-cost.
          ELSE
          ASSIGN v-cost-ea = fg-bin.std-lab-cost / 1000.
          
          v-cost[4] = v-cost[4] + (v-binqty * v-cost-ea).
          
          IF fg-bin.pur-uom EQ "EA" THEN
          v-cost-ea = fg-bin.std-var-cost.
          ELSE
          ASSIGN v-cost-ea = fg-bin.std-var-cost / 1000.
          
          v-cost[5] = v-cost[5] + (v-binqty * v-cost-ea).
          
          IF fg-bin.pur-uom EQ "EA" THEN
          v-cost-ea = fg-bin.std-fix-cost.
          ELSE
          ASSIGN v-cost-ea = fg-bin.std-fix-cost / 1000.
          
          v-cost[6] = v-cost[6] + (v-binqty * v-cost-ea).
        END.
        ELSE IF fg-ctrl.inv-meth = "L" THEN DO:
          IF v-qty NE 0 OR ll-all-empty-bins THEN
          v-cost[1] = v-cost[1] / v-qty.
          v-qty = 1.
          
        END.
      END.
    END.
    
    REPEAT:
      
      FIND CURRENT itemfg EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      
      IF AVAIL itemfg THEN
      DO:
        IF v-qty NE 0 OR ll-all-empty-bins THEN DO:
          IF v-qty EQ 0 THEN
              RUN cost-when-zero-qty.
          ELSE DO:
            
            IF v-qty EQ 0 THEN
                RUN cost-when-zero-qty.
            
            /* for "L", this is handled above */
            IF  fg-ctrl.inv-meth NE "L" THEN
            v-cost[1] = v-cost[1] / v-qty.

            IF fg-ctrl.inv-meth EQ "A" THEN
            ASSIGN
            v-cost[3] = v-cost[3] / v-qty
            v-cost[4] = v-cost[4] / v-qty
            v-cost[5] = v-cost[5] / v-qty
            v-cost[6] = v-cost[6] / v-qty.
            
            IF v-cost[1] <> 0 AND v-cost[1] <> ? AND fg-ctrl.inv-meth EQ "A" THEN DO:
                
              IF itemfg.prod-uom EQ "EA" THEN
              ASSIGN itemfg.avg-cost = v-cost[1].
              ELSE
              ASSIGN itemfg.avg-cost = v-cost[1] * 1000.
            END.
            
            /*
            if itemfg.prod-uom eq "EA" then
            itemfg.last-cost = v-cost[2].
            else
            run sys/ref/convcuom.p("EA", itemfg.prod-uom, 0, 0, 0, 0,
            v-cost[2], output itemfg.last-cost).
            */
            
            IF v-recalc-if1 THEN
            itemfg.last-cost = v-cost[2].
         
            IF NOT ((v-qty eq 0 AND v-cost[3] EQ 0) OR v-cost[3] EQ ?) THEN DO:
              IF itemfg.prod-uom EQ "EA" THEN
              ASSIGN itemfg.std-mat-cost = v-cost[3].
              ELSE
              ASSIGN  itemfg.std-mat-cost = v-cost[3] * 1000.
            END.
            
            IF NOT ((v-qty eq 0 AND v-cost[4] EQ 0) OR v-cost[4] EQ ?) THEN DO:
              IF itemfg.prod-uom EQ "EA" THEN
              itemfg.std-lab-cost = v-cost[4].
              ELSE
              ASSIGN  itemfg.std-lab-cost = v-cost[4] * 1000.
            END.
            
            IF NOT ((v-qty eq 0 AND v-cost[5] EQ 0) OR v-cost[5] EQ ?) THEN DO:
              IF itemfg.prod-uom EQ "EA" THEN
              ASSIGN itemfg.std-var-cost = v-cost[5].
              ELSE
              ASSIGN itemfg.std-var-cost = v-cost[5] * 1000.
            END.
            
            IF NOT ((v-qty eq 0 AND v-cost[6] EQ 0) OR v-cost[6] EQ ?) THEN DO:
              IF itemfg.prod-uom EQ "EA" THEN
              ASSIGN itemfg.std-fix-cost = v-cost[6].
              ELSE
              ASSIGN  itemfg.std-fix-cost = v-cost[6] * 1000.
            END.
            
            ASSIGN
            itemfg.std-tot-cost   = itemfg.std-mat-cost + itemfg.std-lab-cost +
            itemfg.std-var-cost + itemfg.std-fix-cost
            itemfg.total-std-cost = itemfg.std-tot-cost.
            
          END.
          
        END. /* if v-qty ne 0 or ll-all-empty-bins */
        
        
        IF itemfg.avg-cost       EQ ? THEN itemfg.avg-cost       = 0.
        IF itemfg.last-cost      EQ ? THEN itemfg.last-cost      = 0.
        IF itemfg.std-mat-cost   EQ ? THEN itemfg.std-mat-cost   = 0.
        IF itemfg.std-lab-cost   EQ ? THEN itemfg.std-lab-cost   = 0.
        IF itemfg.std-var-cost   EQ ? THEN itemfg.std-var-cost   = 0.
        IF itemfg.std-fix-cost   EQ ? THEN itemfg.std-fix-cost   = 0.
        IF itemfg.std-tot-cost   EQ ? THEN itemfg.std-tot-cost   = 0.
        IF itemfg.total-std-cost EQ ? THEN itemfg.total-std-cost = 0.
        
        FIND CURRENT itemfg NO-LOCK NO-ERROR.
        LEAVE.
      END. /*if avail itemfg*/
    END. /*end repeat*/
    
    
    /* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
    
    
    /*
    IF TRIM(fg-bin.tag) EQ "" THEN
    for each fg-rcpth
    where fg-rcpth.company   eq fg-bin.company
    and fg-rcpth.i-no      eq fg-bin.i-no
    and fg-rcpth.job-no    eq fg-bin.job-no
    and fg-rcpth.job-no2   eq fg-bin.job-no2
    and fg-rcpth.rita-code eq "R"
    use-index i-no no-lock,
    
    first fg-rdtlh
    where fg-rdtlh.r-no    eq fg-rcpth.r-no
    and fg-rdtlh.loc     eq fg-bin.loc
    and fg-rdtlh.loc-bin eq fg-bin.loc-bin
    and fg-rdtlh.tag     eq fg-bin.tag
    AND fg-rdtlh.cust-no EQ fg-bin.cust-no
    and fg-rdtlh.qty     gt 0
    use-index rm-rdtl no-lock
    
    by fg-rcpth.trans-date desc
    by fg-rcpth.r-no       desc
    by recid(fg-rdtlh)     desc:
    leave.
    end.
    
    ELSE
    for each fg-rdtlh
    where fg-rdtlh.company   eq fg-bin.company
    and fg-rdtlh.loc       eq fg-bin.loc
    and fg-rdtlh.tag       eq fg-bin.tag
    and fg-rdtlh.loc-bin   eq fg-bin.loc-bin
    and fg-rdtlh.rita-code eq "R"
    AND fg-rdtlh.cust-no   EQ fg-bin.cust-no
    and fg-rdtlh.qty       gt 0
    use-index tag no-lock,
    
    first fg-rcpth
    where fg-rcpth.r-no      eq fg-rdtlh.r-no
    and fg-rcpth.i-no      eq fg-bin.i-no
    and fg-rcpth.job-no    eq fg-bin.job-no
    and fg-rcpth.job-no2   eq fg-bin.job-no2
    use-index r-no no-lock
    
    by fg-rcpth.trans-date desc
    by fg-rcpth.r-no       desc
    by recid(fg-rdtlh)     desc:
    leave.
    end.
    
    if (avail fg-rdtlh                            and
    ((fg-rcpth.trans-date  gt v-date    or
    (fg-rcpth.trans-date eq v-date and
    fg-rcpth.r-no       gt v-r-no))     or
    v-date                eq 01/01/0001   or
    v-date                eq ?))                 or
    (not v-rec                                 and
    not avail fg-rdtlh                        and
    fg-bin.aging-date      gt v-date)             or
    first(fg-bin.i-no)                             then do:
    
    if avail fg-rdtlh then do:
    assign
    v-rec     = yes
    v-r-no    = fg-rcpth.r-no
    v-date    = fg-rcpth.trans-date
    v-uom     = "M"
    v-cost[2] = fg-rdtlh.cost.
    
    FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ fg-rcpth.r-no USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    
    IF AVAIL fg-rctd THEN
    ASSIGN
    v-uom     = "EA"
    v-cost[2] = fg-rctd.ext-cost / fg-rctd.t-qty.
    END.
    
    else
    assign
    v-r-no    = 0
    v-date    = fg-bin.aging-date
    v-uom     = fg-bin.pur-uom
    v-cost[2] = fg-bin.std-tot-cost.
    
    if v-uom ne "EA" then
    run sys/ref/convcuom.p(v-uom, "EA", 0, 0, 0, 0,
    v-cost[2], output v-cost[2]).
    
    if fg-ctrl.inv-meth eq "L" then
    assign
    v-cost[3] = fg-bin.std-mat-cost / fg-bin.std-tot-cost * v-cost[2]
    v-cost[4] = fg-bin.std-lab-cost / fg-bin.std-tot-cost * v-cost[2]
    v-cost[5] = fg-bin.std-var-cost / fg-bin.std-tot-cost * v-cost[2]
    v-cost[6] = fg-bin.std-fix-cost / fg-bin.std-tot-cost * v-cost[2].
    end.
    */
    
PROCEDURE cost-when-zero-qty:
DEF VAR lv-uom AS CHAR NO-UNDO.

    FOR EACH b-fg-rcpth
      WHERE b-fg-rcpth.company   EQ itemfg.company
      AND b-fg-rcpth.i-no      EQ itemfg.i-no
      AND b-fg-rcpth.rita-code EQ "R"
      USE-INDEX i-no NO-LOCK,

      FIRST b-fg-rdtlh
      WHERE b-fg-rdtlh.r-no    EQ b-fg-rcpth.r-no
      AND b-fg-rdtlh.rita-code EQ b-fg-rcpth.rita-code
      AND b-fg-rdtlh.qty     GT 0
      NO-LOCK

      BY b-fg-rcpth.trans-date desc
      BY b-fg-rdtlh.trans-time DESC
      BY b-fg-rcpth.r-no       desc
      BY RECID(b-fg-rdtlh)     desc:

      FIND FIRST fg-rctd WHERE fg-rctd.r-no = fg-rdtlh.r-no NO-LOCK NO-ERROR.

      lv-uom = "EA".
      IF AVAIL fg-rctd THEN
          lv-uom = fg-rctd.cost-uom.
      
      IF lv-uom NE "M" THEN
        ASSIGN itemfg.avg-cost = b-fg-rdtlh.cost.
      ELSE
        ASSIGN itemfg.avg-cost = b-fg-rdtlh.cost * 1000.

      ASSIGN itemfg.std-mat-cost   = itemfg.avg-cost
             itemfg.total-std-cost = itemfg.avg-cost
             itemfg.last-cost      = itemfg.avg-cost.

      /* If found the fg-rctd, assume we have the correct uom,
         otherwise keep looking */
      IF itemfg.avg-cost NE 0 AND AVAIL(fg-rctd) THEN
        LEAVE.
    END.

END PROCEDURE.

FUNCTION pgmStack RETURNS CHAR
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cStack AS CHAR        NO-UNDO.
    cStack = PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "").
  RETURN cStack.   /* Function return value. */

END FUNCTION.
