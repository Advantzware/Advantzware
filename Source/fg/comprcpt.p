
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

{fg/fullset.i NEW}

DEF BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b-itemfg  FOR itemfg.

DEF VAR li AS INT NO-UNDO.
DEF VAR li-factor AS INT NO-UNDO.

DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lFGSetAssembly AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cFGSetAssembly AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lGetBin AS LOGICAL     NO-UNDO.


DO TRANSACTION:
  {sys/inc/autopost.i}
  {sys/inc/fgsetrec.i}
  {sys/inc/tspostfg.i}
  {sys/inc/fgrecpt.i}
END.
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "FGSetAssembly",
                       INPUT "L",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFGSetAssembly,
                       OUTPUT lFound).
IF lFound THEN
    lFGSetAssembly = cFGSetAssembly EQ "YES".
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "FGSetAssembly",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFGSetAssembly,
                       OUTPUT lFound).

FIND fg-rctd WHERE ROWID(fg-rctd) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL fg-rctd THEN
FIND FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ fg-rctd.i-no
    NO-LOCK NO-ERROR.

IF AVAIL itemfg                     AND
   itemfg.isaset                    AND
   NOT tspostfg-log                 AND
   itemfg.alloc NE NO               AND
   (itemfg.alloc EQ ?          OR
    fgrecpt-char NE "AUTOPOST" OR
    TRIM(fg-rctd.job-no) EQ "")     THEN DO:

   RUN fg/fullset.p (ROWID(itemfg)).
   
   FOR EACH tt-fg-set,
  
       FIRST b-itemfg
       WHERE b-itemfg.company EQ cocode
         AND b-itemfg.i-no    EQ tt-fg-set.part-no
         AND ROWID(b-itemfg)  NE ROWID(itemfg)
         AND NOT CAN-FIND(FIRST fg-rcpts
                          WHERE fg-rcpts.company EQ cocode
                            AND fg-rcpts.i-no    EQ b-itemfg.i-no
                            AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999"))
       NO-LOCK:
  

       IF fg-rctd.job-no NE "" THEN
       DO:
          FIND FIRST job WHERE
               job.company EQ cocode AND
               job.job-no EQ fg-rctd.job-no AND
               job.job-no2 EQ fg-rctd.job-no2
               NO-LOCK NO-ERROR.

          IF AVAIL job THEN
          DO:
             FIND FIRST eb WHERE
                  eb.company  EQ cocode AND
                  eb.est-no   EQ job.est-no AND
                  eb.stock-no EQ b-itemfg.i-no
                  NO-LOCK NO-ERROR.
             IF AVAIL eb THEN
             DO:
                /* Task 11111303, these records needed to create the     */
                /* negative for components and can be delete after       */
                /* so taking out the next                                */
                /* IF eb.pur-man EQ YES THEN NEXT. */
             END.
             ELSE
                /* IF b-itemfg.pur-man EQ YES THEN NEXT. */
                 .
          END.
       END.
       ELSE
           .
       /* IF b-itemfg.pur-man EQ YES THEN NEXT. */
       
       li = 0.
       FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
       IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT li THEN li = b-fg-rctd.r-no.
       
       FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
       IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
       
       CREATE fg-rcpts.
       ASSIGN
        fg-rcpts.r-no       = li + 1
        fg-rcpts.company    = cocode
        fg-rcpts.i-no       = b-itemfg.i-no
        fg-rcpts.i-name     = b-itemfg.i-name
        fg-rcpts.trans-date = fg-rctd.rct-date
        fg-rcpts.linker     = "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999").
       
       RELEASE fg-rcpts.
       
       CREATE b-fg-rctd.
       BUFFER-COPY fg-rctd EXCEPT rec_key TO b-fg-rctd
       ASSIGN
        b-fg-rctd.r-no       = li + 1
        b-fg-rctd.i-no       = b-itemfg.i-no
        b-fg-rctd.i-name     = b-itemfg.i-name
        b-fg-rctd.qty-case   = fg-rctd.qty-case * tt-fg-set.part-qty-dec
        b-fg-rctd.partial    = fg-rctd.partial  * tt-fg-set.part-qty-dec
        b-fg-rctd.t-qty      = fg-rctd.t-qty    * tt-fg-set.part-qty-dec
        b-fg-rctd.std-cost   = 0
        b-fg-rctd.SetHeaderRno = fg-rctd.r-no
        .
        
       
       IF b-fg-rctd.job-no NE "" THEN DO:
         FIND FIRST job
             WHERE job.company EQ cocode
               AND job.job-no  EQ b-fg-rctd.job-no
               AND job.job-no2 EQ b-fg-rctd.job-no2
             NO-LOCK NO-ERROR.
         IF AVAIL job THEN
         FIND FIRST reftable
             WHERE reftable.reftable EQ "jc/jc-calc.p"
               AND reftable.company  EQ job.company
               AND reftable.loc      EQ ""
               AND reftable.code     EQ STRING(job.job,"999999999")
               AND reftable.code2    EQ b-fg-rctd.i-no
             NO-LOCK NO-ERROR.
       
         IF AVAIL reftable THEN b-fg-rctd.std-cost = reftable.val[5].
       END.
       
       b-fg-rctd.ext-cost = b-fg-rctd.std-cost * b-fg-rctd.t-qty / 1000.
       /* Per Task # 11111303, tag should not be assigned to component */
       /* records regardless of fgsetrec                               */
       b-fg-rctd.tag      = "". 
       /*##BL - IF FGSETAssembly is on, use the char value as the bin*/
       /*##BL - If that bin doesn't exist, continue to find bin as usual*/
       lGetBin = YES.
     
       IF lFGSetAssembly THEN DO:
           b-fg-rctd.loc-bin = cFGSetAssembly.
           FIND FIRST fg-bin
           WHERE fg-bin.company EQ cocode
             AND fg-bin.i-no    EQ b-fg-rctd.i-no
             AND fg-bin.loc     EQ b-fg-rctd.loc
             AND fg-bin.loc-bin EQ b-fg-rctd.loc-bin
             AND fg-bin.tag     EQ b-fg-rctd.tag
             AND fg-bin.job-no  EQ b-fg-rctd.job-no
             AND fg-bin.job-no2 EQ b-fg-rctd.job-no2
           NO-ERROR.

           /* Try again without tag */
           IF NOT AVAIL fg-bin THEN
             FIND FIRST fg-bin
               WHERE fg-bin.company EQ cocode
                 AND fg-bin.i-no    EQ b-fg-rctd.i-no
                 AND fg-bin.loc     EQ b-fg-rctd.loc
                 AND fg-bin.loc-bin EQ b-fg-rctd.loc-bin
                 AND fg-bin.job-no  EQ b-fg-rctd.job-no
                 AND fg-bin.job-no2 EQ b-fg-rctd.job-no2
               NO-ERROR.

           /* Try again without job also */
           IF NOT AVAIL fg-bin THEN
             FIND FIRST fg-bin
               WHERE fg-bin.company EQ cocode
                 AND fg-bin.i-no    EQ b-fg-rctd.i-no
                 AND fg-bin.loc     EQ b-fg-rctd.loc
                 AND fg-bin.loc-bin EQ b-fg-rctd.loc-bin
               NO-ERROR.
            
           IF AVAIL fg-bin THEN
               lGetBin = NO.
           
       END.
     
       IF fgsetrec EQ "Item" AND lGetBin THEN DO:
          ASSIGN
           b-fg-rctd.loc      = ""
           b-fg-rctd.loc-bin  = "".
       
             
          IF autopost EQ "ShipTo" THEN DO: /*get customer file from estimate blank file*/
             FIND FIRST cust
                 WHERE cust.company EQ cocode
                   AND cust.cust-no EQ b-itemfg.cust-no
                 NO-LOCK NO-ERROR.
             IF AVAIL cust THEN DO: 
               FIND FIRST shipto
                   WHERE shipto.company EQ cocode
                     AND shipto.cust-no EQ cust.cust-no
                   NO-LOCK NO-ERROR.
               IF AVAIL shipto THEN DO:
                 FIND FIRST fg-bin
                     WHERE fg-bin.company EQ cocode
                       AND fg-bin.loc     EQ shipto.loc
                       AND fg-bin.loc-bin EQ shipto.loc-bin
                       AND fg-bin.i-no    EQ ""
                     NO-LOCK NO-ERROR.
                 IF AVAIL fg-bin THEN 
                   ASSIGN
                    b-fg-rctd.loc     = shipto.loc
                    b-fg-rctd.loc-bin = shipto.loc-bin.
               END.
             END. /*if avail cust*/
                                
             IF b-fg-rctd.loc EQ "" AND b-fg-rctd.loc-bin EQ "" THEN DO:
               FIND FIRST fg-bin
                   WHERE fg-bin.company EQ cocode
                     AND fg-bin.loc     EQ b-itemfg.def-loc
                     AND fg-bin.loc-bin EQ b-itemfg.def-loc-bin
                     AND fg-bin.i-no    EQ ""
                   NO-LOCK NO-ERROR.
               IF AVAIL fg-bin THEN 
                 ASSIGN
                  b-fg-rctd.loc     = b-itemfg.def-loc
                  b-fg-rctd.loc-bin = b-itemfg.def-loc-bin.
             END.
          END. /*if system default is shipto*/
              
          ELSE DO:
             FIND FIRST fg-bin
                 WHERE fg-bin.company EQ cocode
                   AND fg-bin.loc     EQ b-itemfg.def-loc
                   AND fg-bin.loc-bin EQ b-itemfg.def-loc-bin
                   AND fg-bin.i-no    EQ ""
                 NO-LOCK NO-ERROR.
             IF AVAIL fg-bin THEN 
               ASSIGN
                b-fg-rctd.loc     = b-itemfg.def-loc
                b-fg-rctd.loc-bin = b-itemfg.def-loc-bin.
          END. /*else FGFILE*/
         
          /*if bin and warehouse are blank, goto cust "X" shipto file*/
          IF b-fg-rctd.loc EQ "" AND b-fg-rctd.loc-bin EQ "" THEN DO:
             FIND FIRST cust
                 WHERE cust.company EQ cocode
                   AND cust.active  EQ "X"
                 NO-LOCK NO-ERROR.
                                     
             IF AVAIL cust THEN DO:
                FIND FIRST shipto
                    WHERE shipto.company EQ cocode
                      AND shipto.cust-no EQ cust.cust-no  
                    NO-LOCK NO-ERROR.
                IF AVAIL shipto THEN DO:
                   FIND FIRST fg-bin
                       WHERE fg-bin.company EQ cocode
                         AND fg-bin.loc     EQ shipto.loc
                         AND fg-bin.loc-bin EQ shipto.loc-bin
                         AND fg-bin.i-no    EQ ""
                       NO-LOCK NO-ERROR.
                   IF AVAIL fg-bin THEN
                     ASSIGN
                      b-fg-rctd.loc     = shipto.loc
                      b-fg-rctd.loc-bin = shipto.loc-bin.
                END.                                  
             END.
          END.
       END.
       
       FIND FIRST fg-bin
           WHERE fg-bin.company EQ cocode
             AND fg-bin.i-no    EQ b-fg-rctd.i-no
             AND fg-bin.loc     EQ b-fg-rctd.loc
             AND fg-bin.loc-bin EQ b-fg-rctd.loc-bin
             AND fg-bin.tag     EQ b-fg-rctd.tag
             AND fg-bin.job-no  EQ b-fg-rctd.job-no
             AND fg-bin.job-no2 EQ b-fg-rctd.job-no2
           NO-ERROR.
           
       ASSIGN
        b-fg-rctd.qty-case = IF AVAIL fg-bin AND fg-bin.case-count NE 0 THEN
                               fg-bin.case-count
                             ELSE
                             IF b-itemfg.case-count NE 0 THEN
                               b-itemfg.case-count
                             ELSE
                               b-fg-rctd.qty-case
        li-factor           = IF b-fg-rctd.t-qty GE 0 THEN 1 ELSE -1
        
        b-fg-rctd.t-qty    = b-fg-rctd.t-qty *
                             li-factor
        b-fg-rctd.cases    = TRUNC(b-fg-rctd.t-qty / b-fg-rctd.qty-case,0) *
                             li-factor
        b-fg-rctd.partial  = b-fg-rctd.t-qty MODULO b-fg-rctd.qty-case *
                             li-factor
        b-fg-rctd.t-qty    = b-fg-rctd.t-qty *
                             li-factor.
       
       IF AVAIL fg-bin THEN DO:
          IF fg-bin.cases-unit EQ 0 OR fg-bin.cases-unit EQ ? THEN
            fg-bin.cases-unit = 1.
       
          IF fg-bin.units-pallet EQ 0 OR fg-bin.units-pallet EQ ? THEN
            fg-bin.units-pallet = 1.
       
          fg-bin.unit-count = fg-rctd.qty-case * fg-bin.cases-unit.
       END.
       
       RELEASE b-fg-rctd.
   END.
END.

IF AVAIL fg-rctd THEN RUN fg/comprct1.p (ROWID(fg-rctd)).

