/* Program for oe/ordfrest.i                                                */
/* Allows creating an order from an estimate without the dependence on an   */
/* include file so avoids the transaction warnings in the calling proc      */
DEF INPUT        PARAM ip-callproc AS HANDLE NO-UNDO.
DEF INPUT        PARAM ip-ord-rec AS ROWID NO-UNDO.
DEF INPUT-OUTPUT PARAM ip-new-ord AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAM v-qty-mod AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAM v-inactive AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAM v-est-fg1 AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER lv-qty AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-d-rel AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-quo-price-log AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER save_id AS RECID NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-job-no AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-job-no2 AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER io_fil_id AS RECID NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-exp-limit AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-n-ord AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER nufile AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-estord-id AS RECID EXTENT 10 NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-multord AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-margin AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-custype LIKE cust.TYPE NO-UNDO.
DEF INPUT-OUTPUT PARAMETER ld-lastship-dec AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAMETER lastship-cha AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER lastship-int AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-foamdate-log AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-foamdate-int AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-est-fg AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-full-cost AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER oeestcom-log AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-oecomb-int AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER ll-from-tandem AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER adm-new-record AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER ll-is-new-rec AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAMETER v-create-job AS LOG NO-UNDO.

DEF OUTPUT PARAM op-apply-entry-to-est AS LOG NO-UNDO.


DEF NEW SHARED VAR fil_id as recid no-undo.

/* Not used in calling procedure so not shared */
{oe/tt-item-qty-price.i}

FIND oe-ord WHERE ROWID(oe-ord) EQ ip-ord-rec NO-LOCK NO-ERROR.
IF NOT AVAIL oe-ord THEN
    RETURN.


DEF BUFFER xoe-ord FOR oe-ord. /* Record is found based on fil_id */
DEF NEW SHARED BUFFER xeb FOR eb .        /* First record found */
DEF NEW SHARED BUFFER xest FOR est.       /* Record found from parameter */
DEF NEW SHARED BUFFER xef FOR ef.
DEF BUFFER oe-ordl-whs-item FOR reftable .
DEF BUFFER oe-ord-whs-order FOR reftable .
DEF VAR cReturnVal AS CHAR NO-UNDO.


{custom/globdefs.i}
{ce/print4.i "shared"}
{ce/print42.i "shared"}
{sys/inc/var.i "shared" }

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF SHARED TEMP-TABLE w-ord NO-UNDO FIELD w-ord-no LIKE oe-ord.ord-no.
DEF SHARED TEMP-TABLE tt-oe-ordl NO-UNDO LIKE oe-ordl
    FIELD to-be-deleted AS LOG INIT YES
    FIELD row-id AS ROWID
    INDEX row-id row-id. 

DEF NEW SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.


/* begin defined at top of ordfrest.i */
DEFINE VARIABLE char-hdl   AS cha          NO-UNDO.
DEFINE VARIABLE v-est-no   LIKE est.est-no   NO-UNDO.
DEFINE VARIABLE v-est-type LIKE est.est-type NO-UNDO.
DEFINE VARIABLE v-factor   AS DECIMAL      NO-UNDO.
DEFINE VARIABLE v-run-list AS CHARACTER    INIT
  "oe/calc-one.p,oe/calc-box.p,ce/tan/print4.p,ce/com/print4.p,cec/com/print4.p".
/* DEFINE VARIABLE i                  AS INTEGER          NO-UNDO. */
/* DEFINE VARIABLE j                  AS INTEGER          NO-UNDO. */
/* DEFINE VARIABLE X                  AS INTEGER          NO-UNDO. */
DEFINE VARIABLE v-blk-qty          AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-blk-qty-2        AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-tax-rate         AS DECIMAL          FORM ">,>>9.99<<<" NO-UNDO.
DEFINE VARIABLE v-frt-tax-rate     LIKE v-tax-rate       NO-UNDO.
DEFINE VARIABLE v-quo-price        LIKE sys-ctrl.log-fld NO-UNDO.
DEFINE VARIABLE li-line-no         AS INTEGER          NO-UNDO.
DEFINE VARIABLE choice             AS LOGICAL          NO-UNDO.
DEFINE VARIABLE hld-id             AS RECID            NO-UNDO.
DEFINE VARIABLE hld-stat           LIKE job.stat         NO-UNDO.
DEFINE VARIABLE hld-nufile         AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lv-pr-uom          AS cha              NO-UNDO.
DEFINE VARIABLE lv-job-recid       AS RECID            NO-UNDO.
DEFINE VARIABLE ll-canceled        AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-run-schedule     AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lv-cas-cnt         LIKE eb.cas-cnt       NO-UNDO.
DEFINE VARIABLE ll-do-entry        AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lv-q-no            LIKE quotehd.q-no     NO-UNDO.
DEFINE VARIABLE ll-new-fg          AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-com              AS DECIMAL          NO-UNDO.
DEFINE VARIABLE ld-marg%           AS DECIMAL          NO-UNDO.
DEFINE VARIABLE lv-sell-by-ce-ctrl AS CHARACTER        NO-UNDO.
DEFINE VARIABLE lv-sell-by         AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-pct-2            LIKE eb.comm          NO-UNDO.
DEFINE VARIABLE v-comm-2           AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-tot-comm-2       AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-basis            AS CHARACTER        NO-UNDO.
DEFINE VARIABLE ll-use-margin      AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-sell-price       AS DECIMAL          DECIMALS 2 NO-UNDO.
DEFINE VARIABLE v-probe-comm       AS DECIMAL          DECIMALS 5 NO-UNDO.
DEFINE VARIABLE v-mp               AS DECIMAL          DECIMALS 5 NO-UNDO.
DEFINE VARIABLE v-qty-2            AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-qty-3            AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-board-cst        AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-d-ordqty-price   LIKE quoteqty.price   INIT -1 NO-UNDO.
DEFINE VARIABLE v-d-ordqty-uom     AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-chose-quote      AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-d-ordqty-error   AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-cost-2           LIKE oe-ordl.cost.
DEFINE VARIABLE blk-fact           LIKE blk.fact         NO-UNDO.
DEFINE VARIABLE v-rowid-list       AS CHARACTER        NO-UNDO.
DEFINE VARIABLE lv-disc            LIKE cust.disc        NO-UNDO.

DEF BUFFER b-oe-ord FOR oe-ord.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-ef2 FOR ef.
DEF BUFFER b-eb2 FOR eb.
DEF BUFFER bf-blk FOR blk.

DISABLE TRIGGERS FOR LOAD OF oe-ordl.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                     AND sys-ctrl.NAME    EQ "SCHEDULE" NO-LOCK NO-ERROR.
/*v-run-schedule = if avail sys-ctrl AND sys-ctrl.log-fld THEN YES ELSE NO. 
         Task  09130412 */
v-run-schedule = IF sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO
                 ELSE IF sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES
                 ELSE NO.   /*sys-ctrl.log-fld.*/
DO TRANSACTION:
  {sys/inc/cerun.i C}
  {sys/inc/cecomm.i}

END.

DEF BUFFER b-blk FOR blk.

RUN ord-from-est (ip-new-ord).


cReturnVal = RETURN-VALUE.
IF cReturnVal NE "" THEN
    RETURN cReturnVal.

FORM
    oe-ord.est-no
    WITH FRAME xyz.

 HIDE FRAME xyz.

PROCEDURE ord-from-est:
    &scoped-define FRAME-NAME xyz
    
   { oe/ordfrest.i SUPRESS}  

    io_fil_id = fil_id.
END PROCEDURE.



PROCEDURE release-shared-buffers:
/* required for oe-ordfrest.i */
END PROCEDURE.

PROCEDURE dispatch:
/* required for oe-ordfrest.i */
DEF INPUT PARAMETER c-null AS CHAR NO-UNDO.
END PROCEDURE.

PROCEDURE recalc-itemfg-loc:
      DEF INPUT PARAMETER ipr-oe-ord AS ROWID NO-UNDO.
      DEF BUFFER b-oe-ordl FOR oe-ordl.
      DEF BUFFER bf-oe-ord FOR oe-ord.
      DEF BUFFER xoe-ord FOR oe-ord.

      FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ipr-oe-ord NO-LOCK.

      /*task 12271002 sometimes set header calcqono.p does not get called*/ 
      FOR EACH b-oe-ordl WHERE
          b-oe-ordl.company EQ bf-oe-ord.company AND
          b-oe-ordl.ord-no EQ bf-oe-ord.ord-no
          NO-LOCK,
          FIRST itemfg WHERE
                itemfg.company EQ bf-oe-ord.company AND
                itemfg.i-no EQ b-oe-ordl.i-no AND
                itemfg.isaset EQ YES:
    
          RUN fg/calcqono.p (ROWID(itemfg), OUTPUT itemfg.q-ono).
          FOR FIRST xoe-ord
              WHERE xoe-ord.company EQ cocode
                AND xoe-ord.ord-no  EQ b-oe-ordl.ord-no
              NO-LOCK
              :
    
            IF AVAIL(itemfg) THEN DO:          
                RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT xoe-ord.loc).
                FIND FIRST itemfg-loc 
                    WHERE itemfg-loc.company EQ itemfg.company
                      AND itemfg-loc.i-no    EQ itemfg.i-no
                      AND itemfg-loc.loc     EQ xoe-ord.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                RUN fg/calcqool.p (ROWID(itemfg), xoe-ord.loc, OUTPUT itemfg-loc.q-ono).
            END. /* if avail itemfg */
          END. /* for first xoe-ord */
      END. /* each b-oe-ordl */
END PROCEDURE.

PROCEDURE new-order:


    /** CHECK for INACTIVE CUSTOMERS **/
    FOR EACH eb WHERE eb.company = cocode AND
                      eb.est-no   EQ xest.est-no
                  AND eb.form-no NE 0
                  AND TRIM(eb.cust-no) NE ""
             NO-LOCK BREAK BY eb.est-no BY eb.cust-no:
      

      IF FIRST-OF(eb.cust-no) THEN DO:
        /** finding the customer is done this way because the index is not
        setup efficently to find the customer regardles of active stat **/
        FIND FIRST cust {sys/ref/custW.i}
                        AND cust.cust-no EQ eb.cust-no
             USE-INDEX cust NO-LOCK NO-ERROR.
        IF NOT avail cust OR cust.active EQ "I" THEN DO:
          IF NOT "{1}" EQ "SUPRESS" THEN
          MESSAGE              "   Inactive Customer:" cust.NAME SKIP
             "   Orders May not Be Processed for Inactive Customers.  "
             VIEW-AS ALERT-BOX WARNING.
          ASSIGN v-inactive = YES.
          RETURN "Inactive Cust".
        END. /* do */
      END. /* first-of(eb.cust-no) */


      IF v-est-fg1 = "HOLD" AND eb.stock-no = "" THEN DO:
        IF NOT "{1}" EQ "SUPRESS" THEN DO:
            MESSAGE "Sorry, FG item does not exist. Order has not been approved."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ord.est-no IN FRAME {&FRAME-NAME}.
        END.
        RETURN "NO FGITEM" .
      END. /* if order on hold */

    END. /* each eb */
    
    ASSIGN
       i = 0
       v-d-ordqty-price = -1
       v-d-ordqty-uom = "-"
       v-chose-quote = NO.

    EMPTY TEMP-TABLE tt-item-qty-price.



    FOR EACH quoteitm NO-LOCK WHERE quoteitm.company EQ xest.company AND
          quoteitm.est-no EQ xest.est-no,
          EACH quoteqty OF quoteitm NO-LOCK BREAK BY quoteqty.LINE BY quoteqty.quote-date DESC 
              BY quoteqty.qty :

        IF FIRST-OF(quoteqty.LINE) THEN
           i = 0.

        i = i + 1.
        IF i > 1 THEN LEAVE.


        ASSIGN
           lv-qty = quoteqty.qty
           v-d-rel = quoteqty.rels.


    END. /* each quoteitm */

    
    
    
    IF v-quo-price-log AND i > 1 THEN DO:
       FIND FIRST est-qty WHERE est-qty.company = xest.company
                         AND est-qty.est-no = xest.est-no
                      NO-LOCK NO-ERROR.

       RUN oe/d-ordqty.w (RECID(est-qty), OUTPUT lv-qty, OUTPUT v-d-ordqty-price, OUTPUT v-d-ordqty-uom, OUTPUT v-d-rel, OUTPUT v-d-ordqty-error,
                          OUTPUT TABLE tt-item-qty-price).
       
       IF v-d-ordqty-error = NO THEN
          v-chose-quote = YES.
    END. /* quo-price-log and i > 1 */

END PROCEDURE.

PROCEDURE not-new-ord:


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
          END. /* if oe-ordl.est-no eq oe-ord.est-no */
    
          ELSE
          IF oe-ordl.est-no NE "" THEN DELETE oe-ordl.
    
    
        END. /* each oe-ordl */
    END. /* not ip-new-ord and type is 3 or 4 */

END PROCEDURE.

PROCEDURE create-new-order.
    /* dependencies  ...
      oe-ord
      xoe-ord
      v-job-no
      v-job-no2
      j
      v-multord
      eb buffer
      xest buffer
      
    */
          /*************** create ORDER HEADER ******************/
          {oe/oe-ord.a} 
          v-n-ord = xoe-ord.ord-no.
          ASSIGN
           xoe-ord.est-no    = oe-ord.est-no
           xoe-ord.TYPE      = oe-ord.TYPE
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
           v-margin          = 0
           xoe-ord.s-pct[1]  = 100
           xoe-ord.est-type  = xest.est-type
           xoe-ord.due-code  = "ON"
           
           xoe-ord.cust-no   = cust.cust-no
           xoe-ord.cust-name = cust.NAME
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
           xoe-ord.f-bill    = (oe-ord.frt-pay EQ "B")
           xoe-ord.tax-gr    = cust.tax-gr
           v-custype         = cust.TYPE
           v-factor = IF xest.est-type GE 1 AND xest.est-type LE 4 THEN ld-lastship-dec
                      ELSE 1.
      
          IF xest.ord-no NE 0 THEN
             ASSIGN
                xoe-ord.pord-no = xest.ord-no
                xoe-ord.po-no2  = STRING(xest.ord-no).
      
          IF lastship-cha EQ "Fibre" THEN
            ASSIGN
             xoe-ord.last-date = xoe-ord.ord-date + (cust.ship-days * v-factor)
             xoe-ord.due-date  = xoe-ord.ord-date + (lastship-int * v-factor).
END PROCEDURE.



PROCEDURE create-order-lines.
  /* dependencies ...
    cocode
    xoe-ord buffer
    eb buffer
    v-est-type
    tt-item-qty-price temp-table
    li-line-no (local)
    v-d-rel
    ll-new-fg
    v-probe-comm
    v-mp
    lv-q-no
    .
    .
    .
  */
    /************* create LINE ITEMS ***************/   
    FIND FIRST oe-ordl WHERE oe-ordl.company  EQ cocode
          AND oe-ordl.ord-no   EQ xoe-ord.ord-no
          AND oe-ordl.cust-no  EQ xoe-ord.cust-no
          AND (oe-ordl.part-no EQ eb.part-no
           OR  (oe-ordl.i-no   EQ eb.stock-no AND eb.stock-no NE ""))
          AND (v-est-type EQ 4 OR
               CAN-FIND(FIRST tt-oe-ordl WHERE tt-oe-ordl.row-id EQ ROWID(oe-ordl)))
        NO-ERROR.

    IF avail oe-ordl AND /*quote combines quantities so get qty from quote only for
                           same fg on different forms*/
       NOT CAN-FIND(FIRST tt-item-qty-price WHERE
           tt-item-qty-price.tt-selected = YES AND
           (tt-item-qty-price.part-no EQ oe-ordl.part-no OR
           (tt-item-qty-price.part-no EQ oe-ordl.i-no AND oe-ordl.i-no NE ""))) THEN
           oe-ordl.qty = oe-ordl.qty + eb.bl-qty.
    
    IF NOT avail oe-ordl                                                    OR
       CAN-FIND(FIRST tt-oe-ordl WHERE tt-oe-ordl.row-id EQ ROWID(oe-ordl)) THEN DO:
       IF NOT avail oe-ordl THEN DO:
          li-line-no = 1.
          FOR EACH oe-ordl FIELDS(LINE)
              WHERE oe-ordl.company EQ xoe-ord.company
                AND oe-ordl.ord-no  EQ xoe-ord.ord-no
              BY oe-ordl.LINE DESC:
            li-line-no = oe-ordl.LINE + 1.
            LEAVE.
          END.
          
          CREATE oe-ordl.
          ASSIGN
           oe-ordl.company    = cocode
           oe-ordl.ord-no     = xoe-ord.ord-no  /* input screen-value */
           oe-ordl.LINE       = li-line-no
           oe-ordl.po-no      = xoe-ord.po-no
           oe-ordl.job-no     = xoe-ord.job-no
           oe-ordl.job-no2    = xoe-ord.job-no2
           oe-ordl.req-code   = xoe-ord.due-code
           oe-ordl.prom-code  = xoe-ord.due-code
           oe-ordl.req-date   = xoe-ord.due-date
           oe-ordl.prom-date  = xoe-ord.due-date
           oe-ordl.i-no       = IF v-est-type EQ 2 AND avail xeb THEN
                                  xeb.stock-no ELSE eb.stock-no
           oe-ordl.qty        = IF v-est-type EQ 3 OR v-est-type EQ 4 THEN eb.bl-qty ELSE lv-qty
           oe-ordl.rel        = IF v-d-rel EQ 0 THEN 1 ELSE v-d-rel
           v-qty-mod          = YES
           oe-ordl.over-pct   = xoe-ord.over-pct
           oe-ordl.under-pct  = xoe-ord.under-pct.
      
          IF oe-ordl.i-no EQ "0" THEN oe-ordl.i-no = "".
         
          IF xoe-ord.est-no NE "" THEN
            ASSIGN
             oe-ordl.est-no = xoe-ord.est-no
             oe-ordl.pr-uom = "M".
          
          DO i = 1 TO 3:
            ASSIGN oe-ordl.s-man[i] = xoe-ord.sman[i]
                   oe-ordl.s-pct[i] = xoe-ord.s-pct[i]
                   oe-ordl.s-comm[i] = xoe-ord.s-comm[i].
          END.
         
          oe-ordl.q-qty = xoe-ord.t-fuel.
         
          IF v-foamdate-log                                         AND
             CAN-FIND(FIRST style WHERE style.company EQ eb.company
                                    AND style.style   EQ eb.style
                                    AND style.TYPE    EQ "F")       THEN
            oe-ordl.req-date = xoe-ord.ord-date + v-foamdate-int.
       END.  /* not avail oe-ordl */

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

       IF oe-ordl.i-no EQ "" THEN DO:
          IF v-est-fg THEN
              oe-ordl.i-no = IF v-est-type EQ 2 AND avail xeb THEN
               xeb.part-no ELSE eb.part-no.   
         
          ELSE
          IF v-est-fg1 NE "Manual" THEN DO:
             FIND FIRST itemfg
                 WHERE itemfg.company EQ cocode
                   AND itemfg.part-no EQ (IF v-est-type EQ 2 AND avail xeb THEN
                                            xeb.part-no ELSE eb.part-no)
                   AND itemfg.cust-no EQ eb.cust-no
                 NO-LOCK NO-ERROR.
             IF avail itemfg THEN
              ASSIGN
                oe-ordl.i-no       = itemfg.i-no
                oe-ordl.part-dscr2 = itemfg.part-dscr2
                oe-ordl.part-dscr3 = itemfg.part-dscr3 .  /* task 08041404 */
             END.

          IF v-est-fg1 EQ "Hughes" THEN
             RUN fg/hughesfg.p ((IF v-est-type EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                 OUTPUT oe-ordl.i-no).
          ELSE
          IF v-est-fg1 EQ "Fibre" THEN 
              RUN fg/fibre-fg.p ((IF v-est-type EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                  OUTPUT oe-ordl.i-no).
          ELSE IF can-do("Manual,None,Hold",v-est-fg1)  THEN.
          ELSE do:
              
              RUN fg/autofg.p ((IF v-est-type EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                  v-est-fg1, 
                                  (IF v-est-type EQ 2 AND AVAIL xeb THEN xeb.procat ELSE eb.procat),
                                  IF xest.est-type LE 4 THEN "F" ELSE "C",
                                  (IF v-est-type EQ 2 AND AVAIL xeb THEN xeb.cust-no ELSE eb.cust-no),
                                  OUTPUT oe-ordl.i-no).
          END.
       END. /* if i-no eq "" */
       
       /*BV - override customer discount if FG item is exempt from discount*/
       FIND FIRST itemfg 
           WHERE itemfg.company EQ cocode
             AND itemfg.i-no EQ oe-ordl.i-no
           NO-LOCK NO-ERROR.
       IF AVAIL itemfg AND itemfg.exempt-disc THEN 
           lv-disc = 0.
       ELSE 
           lv-disc = cust.disc.
 
/*        FIND FIRST reftable WHERE reftable.reftable EQ "itemfg.exempt-disc" */
/*            AND reftable.company  EQ cocode                                 */
/*            AND reftable.loc      EQ ""                   ~                 */
/*            AND reftable.CODE     EQ oe-ordl.i-no                           */
/*            NO-LOCK NO-ERROR.                                               */
/*         IF AVAIL reftable THEN DO:                                         */
/*             IF reftable.val[1] = 1 THEN                                    */
/*                 lv-disc = 0.                                               */
/*             ELSE                                                           */
/*                 lv-disc = cust.disc.                                       */
/*         END.                                                               */

       ASSIGN
        oe-ordl.i-name     = IF v-est-type EQ 2 AND avail xeb THEN
                               xeb.part-dscr1 ELSE eb.part-dscr1
        oe-ordl.part-no    = IF v-est-type EQ 2 AND avail xeb THEN
                               xeb.part-no ELSE eb.part-no
        oe-ordl.part-dscr1 = IF v-est-type EQ 2 AND avail xeb THEN
                                xeb.part-dscr2 ELSE eb.part-dscr2
        oe-ordl.est-type   = eb.est-type
        oe-ordl.form-no    = eb.form-no
        oe-ordl.blank-no   = eb.blank-no
        oe-ordl.cust-no    = xoe-ord.cust-no
        oe-ordl.disc       = lv-disc
        oe-ordl.tax        = cust.SORT EQ "Y" AND xoe-ord.tax-gr NE "".
      
       FIND FIRST tt-item-qty-price WHERE
            tt-item-qty-price.tt-selected = YES AND
            (tt-item-qty-price.part-no EQ oe-ordl.part-no OR
            (tt-item-qty-price.part-no EQ oe-ordl.i-no AND oe-ordl.i-no NE ""))
            NO-ERROR.
      
       IF AVAIL tt-item-qty-price THEN
          ASSIGN
             oe-ordl.qty = tt-item-qty-price.qty
             oe-ordl.rel = tt-item-qty-price.rels.
      
       IF v-d-ordqty-uom NE "-" THEN
       DO:
          IF AVAIL tt-item-qty-price THEN
             oe-ordl.pr-uom = tt-item-qty-price.uom.
          ELSE
             oe-ordl.pr-uom = v-d-ordqty-uom.
       END.
      
       IF v-d-ordqty-price NE -1 THEN
       DO:
          IF AVAIL tt-item-qty-price THEN
             oe-ordl.price = tt-item-qty-price.price.
          ELSE
             oe-ordl.price = v-d-ordqty-price.
       END.
      
       {custom/shptotax.i xoe-ord.cust-no xoe-ord.sold-id oe-ordl.tax}
      
       RUN est/getcscnt.p ((IF xest.est-type EQ 6 AND
                               AVAIL xeb          AND
                               xeb.cas-no NE ""   THEN ROWID(xeb) ELSE ROWID(eb)),
                          OUTPUT oe-ordl.cas-cnt,OUTPUT oe-ordl.cases-unit).
                    
       ASSIGN
        oe-ordl.cases      = TRUNC(oe-ordl.qty / oe-ordl.cas-cnt,0)
        /*oe-ordl.partial    = oe-ordl.qty MOD oe-ordl.cas-cnt*/ .
      
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
      
       FIND FIRST itemfg WHERE itemfg.company EQ cocode
                           AND itemfg.i-no    EQ oe-ordl.i-no
           NO-LOCK NO-ERROR.  
       IF avail itemfg THEN
       DO:
           ASSIGN
               oe-ordl.part-dscr2 = itemfg.part-dscr2
               oe-ordl.part-dscr3 = itemfg.part-dscr3 .   /* task 08041404 */
      
         IF v-d-ordqty-price EQ -1 THEN
            ASSIGN
               oe-ordl.price      = itemfg.sell-price
               oe-ordl.pr-uom     = itemfg.sell-uom.
       END. /* if avail itemfg */
      
       oe-ordl.type-code = 
             STRING(AVAIL itemfg AND
                    CAN-FIND(FIRST b-oe-ordl
                             WHERE b-oe-ordl.company EQ itemfg.company
                               AND b-oe-ordl.i-no    EQ itemfg.i-no
                               AND b-oe-ordl.ord-no  LT oe-ordl.ord-no
                               AND ROWID(b-oe-ordl)  NE ROWID(oe-ordl)),"R/O").
      
       IF v-est-type EQ 3 OR v-est-type EQ 4 THEN DO:
         FIND FIRST blk WHERE blk.id EQ eb.part-no NO-LOCK NO-ERROR.           
         IF avail blk THEN DO:
           IF v-est-type EQ 4 THEN DO:
              ASSIGN
                 v-blk-qty = 0
                 v-tot-comm-2 = 0
                 v-blk-qty-2 = 0.
             
             FOR EACH blk WHERE blk.id EQ eb.part-no NO-LOCK,
                 FIRST xjob
                 WHERE xjob.form-no  EQ blk.snum
                   AND xjob.blank-no EQ blk.bnum:
               
               IF v-full-cost AND eb.est-type EQ 8 THEN
               DO:
                  IF cerunc EQ "Fibre" THEN
                     RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).
                 
                  ASSIGN
                     lv-sell-by = ce-ctrl.sell-by
                     lv-sell-by-ce-ctrl = ce-ctrl.sell-by
                     v-pct-2 = ce-ctrl.prof-mrkup
                     v-probe-comm = eb.comm.
                 
                  RUN custom/combasis.p (cocode,
                                         eb.sman,
                                         cust.TYPE,
                                         eb.procat,
                                         0,
                                         cust.cust-no,
                                         OUTPUT v-basis).
                 
                  IF cust.markup NE 0 THEN
                     v-pct-2 = cust.markup.
                 
                  IF NOT cecomm-log THEN v-probe-comm = 0.
                 
                  IF ll-use-margin THEN                 /* Get Margin% */
                     RUN est/getsmanmtrx.p (ROWID(xest), "M",
                                            INPUT-OUTPUT v-probe-comm,
                                            INPUT-OUTPUT v-mp).
                 
                   ASSIGN
                        v-board-cst = 0
                        t-blkqty = 0.
                 
                     FOR EACH b-eb2 FIELDS(form-no yrprice yld-qty bl-qty) NO-LOCK WHERE
                         b-eb2.company EQ xest.company AND
                         b-eb2.est-no  EQ xest.est-no AND
                         b-eb2.form-no EQ eb.form-no:
                         /* set total # of blanks on all forms */
                 
                         t-blkqty[b-eb2.form-no] = t-blkqty[b-eb2.form-no] +
                                                   IF b-eb2.yrprice THEN b-eb2.yld-qty ELSE b-eb2.bl-qty.
                     END. /* each b-eb2 */
                 
                     FOR EACH b-blk WHERE b-blk.id EQ xeb.part-no,
                         FIRST b-ef2 NO-LOCK
                         WHERE b-ef2.company EQ xest.company
                           AND b-ef2.est-no  EQ xest.est-no
                           AND b-ef2.form-no EQ b-blk.snum,
                         EACH brd WHERE brd.form-no EQ b-ef2.form-no:
                    
                         v-board-cst = v-board-cst + (brd.cost-m * b-blk.pct * (t-blkqty[b-ef2.form-no] / 1000)).
                     END. /* each b-blk */
                    
                     v-board-cst = v-board-cst / (v-qty-2 / 1000).
                  /*Why is Price being recalculated based on target markup?*/   
                  RUN custom/markup.p (ROWID(xeb),
                                       v-board-cst,
                                       v-board-cst,
                                       v-board-cst,
                                       0,
                                       INPUT-OUTPUT lv-sell-by,
                                       INPUT-OUTPUT v-pct-2).
                 
                  IF ll-use-margin THEN
                     v-pct-2 = v-mp.
                 
                  v-qty-2 = IF eb.yrprice THEN blk.qyld ELSE blk.qreq.
                 
/*                  IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN DO:                                    */
/*                     ASSIGN                                                                                      */
/*                        v-board-cst = 0                                                                          */
/*                        t-blkqty = 0.                                                                            */
/*                                                                                                                 */
/*                     FOR EACH b-eb2 FIELDS(form-no yrprice yld-qty bl-qty) NO-LOCK WHERE                         */
/*                         b-eb2.company EQ xest.company AND                                                       */
/*                         b-eb2.est-no  EQ xest.est-no AND                                                        */
/*                         b-eb2.form-no EQ eb.form-no:                                                            */
/*                         /* set total # of blanks on all forms */                                                */
/*                                                                                                                 */
/*                         t-blkqty[b-eb2.form-no] = t-blkqty[b-eb2.form-no] +                                     */
/*                                                   IF b-eb2.yrprice THEN b-eb2.yld-qty ELSE b-eb2.bl-qty.        */
/*                     END. /* each b-eb2 */                                                                       */
/*                                                                                                                 */
/*                     FOR EACH b-blk WHERE b-blk.id EQ xeb.part-no,                                               */
/*                         FIRST b-ef2 NO-LOCK                                                                     */
/*                         WHERE b-ef2.company EQ xest.company                                                     */
/*                           AND b-ef2.est-no  EQ xest.est-no                                                      */
/*                           AND b-ef2.form-no EQ b-blk.snum,                                                      */
/*                         EACH brd WHERE brd.form-no EQ b-ef2.form-no:                                            */
/*                                                                                                                 */
/*                         v-board-cst = v-board-cst + (brd.cost-m * b-blk.pct * (t-blkqty[b-ef2.form-no] / 1000)).*/
/*                     END. /* each b-blk */                                                                       */
/*                                                                                                                 */
/*                     v-board-cst = v-board-cst / (v-qty-2 / 1000).                                               */
/*                  END. /* sell-by-c-ctrl ne 'b' and sell-by eq 'b' */                                            */

                  RUN custom/sellpric.p (lv-sell-by-ce-ctrl,
                                         lv-sell-by,
                                         v-basis,
                                         (IF lv-sell-by-ce-ctrl NE "B" AND
                                             lv-sell-by EQ "B" THEN v-board-cst
                                          ELSE blk.fact / (v-qty-2 / 1000)),
                                         (IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0
                                          ELSE (blk.cost / (v-qty-2 / 1000)) - (blk.fact / (v-qty-2 / 1000))),
                                         (IF ll-use-margin OR
                                             (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN 0
                                          ELSE v-comm-2),
                                         v-pct-2,
                                         OUTPUT v-sell-price,
                                         OUTPUT v-comm-2).
                  IF ll-use-margin OR
                     (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN
                     v-comm-2 = v-sell-price * v-probe-comm / 100.


               END. /*v-full-cost AND eb.est-type EQ 8*/
               ELSE
                   v-comm-2 = 0.
      
               ASSIGN
                v-tot-comm-2 = v-tot-comm-2 + v-comm-2
                oe-ordl.t-weight  = oe-ordl.t-weight + blk.fg-wt
                oe-ordl.t-freight = oe-ordl.t-freight +
                                      (blk.fg-wt$ * (blk.fg-wt / 100))
                v-blk-qty           = v-blk-qty + blk.qyld.
      
               IF eb.est-type EQ 4 OR eb.est-type EQ 8 THEN
                  v-blk-qty-2 = v-blk-qty-2 + (IF eb.yrprice THEN blk.qyld ELSE blk.qreq).
      
               IF v-full-cost THEN
                 oe-ordl.cost = oe-ordl.cost + blk.cost.
               ELSE
               DO:
                 IF eb.est-type EQ 4 THEN
                 DO:
                    ASSIGN
                       blk-fact = 0
                       v-qty-3 = IF eb.yrprice THEN blk.qyld ELSE blk.qreq.
      
                    FOR EACH bf-blk:
                        blk-fact = blk-fact + bf-blk.fact.
                    END. /* each bf-blk */
      
                    v-cost-2 = blk.fact * (fac-tot / blk-fact).
                 END. /* type eq 4 */
                 ELSE
                    oe-ordl.cost = oe-ordl.cost +
                                  ((xjob.lab + xjob.mat + xjob.voh + xjob.foh) *
                                    v-qty-3 / 1000).
               END. /* not full cost */
             END. /* each blk */
      
             IF eb.est-type EQ 4 AND v-full-cost EQ NO THEN
                oe-ordl.cost = oe-ordl.cost + (v-cost-2 / (v-blk-qty-2 / 1000)).
             ELSE
             IF eb.est-type EQ 8 THEN
                oe-ordl.cost = (oe-ordl.cost / (v-blk-qty-2 / 1000)) + v-tot-comm-2.
             ELSE
                oe-ordl.cost = (oe-ordl.cost / (v-blk-qty / 1000)) + v-tot-comm-2.
      
           END. /* est-type = 4 */
      
           ELSE DO:
             ASSIGN
              oe-ordl.t-weight  = blk.fg-wt
              oe-ordl.t-freight = blk.fg-wt$ * (blk.fg-wt / 100).
              
             IF v-full-cost THEN
               oe-ordl.cost = blk.cost -
                                (((blk.fg-wt / 100) * blk.fg-wt$)
                                   * (blk.qyld / xest.est-qty[1])).
                                    
             ELSE DO:
               FIND FIRST xjob NO-ERROR.
               IF avail xjob THEN
                 oe-ordl.cost = xjob.lab + xjob.mat + xjob.voh + xjob.foh.
             END.  /* ... else do */
           END. /* ... else ... est-type ne 4 */
         END.   /* avail blk */
      
         ELSE DO:
           MESSAGE "NO BLANK AVAILABLE!!!" eb.part-no.
           HIDE MESSAGE.
         END.
       END.  /* est-type = 3 or 4 */
    END. /* not avail oe-ordl */

    IF avail xest AND v-quo-price-log AND NOT v-chose-quote THEN DO:

       RUN oe/getqpric.p (RECID(xest), oe-ordl.part-no, IF v-est-type EQ 2 THEN eb.part-no ELSE "",
                          INPUT-OUTPUT oe-ordl.price,
                          INPUT-OUTPUT oe-ordl.pr-uom,
                          OUTPUT lv-q-no,
                          INPUT-OUTPUT oe-ordl.qty).

       IF lv-q-no NE 0 THEN DO:
        FIND CURRENT oe-ordl.
        ASSIGN oe-ordl.q-no = lv-q-no.
       END. /* lv-q-no ne 0 */
    END. /* avail xest and quo price log ... */

    oe-ordl.t-price = oe-ordl.price * oe-ordl.qty /
                      (IF oe-ordl.pr-uom EQ "C" THEN 100  ELSE
                       IF oe-ordl.pr-uom EQ "M" THEN 1000 ELSE 
                       IF oe-ordl.pr-uom = "L" THEN oe-ordl.qty ELSE 1).

    {oe/defwhsed.i oe-ordl}

    FIND FIRST tt-oe-ordl WHERE tt-oe-ordl.row-id EQ ROWID(oe-ordl) NO-ERROR.
    IF AVAIL tt-oe-ordl THEN tt-oe-ordl.to-be-deleted = NO.
         
    IF v-est-type EQ 2 THEN LEAVE. /** 2pc box & Set headers **/

    IF oeestcom-log = YES THEN        
    DO:
       RELEASE probe.

       FIND FIRST sman WHERE
            sman.company EQ eb.company AND
            sman.sman EQ eb.sman
            NO-LOCK NO-ERROR.

       IF AVAIL sman AND sman.commbasis EQ "M" THEN
       DO:
          DEF VAR v-price-per-1000 AS DEC NO-UNDO.
          DEF VAR v-tmp-price AS DEC NO-UNDO.

          IF oe-ordl.pr-uom NE "M" THEN
          DO:
             FIND FIRST itemfg WHERE
                  itemfg.company EQ cocode AND
                  itemfg.i-no EQ oe-ordl.i-no
                  NO-LOCK NO-ERROR.

             ASSIGN
              v-tmp-price = IF oe-ordl.pr-uom BEGINS "L" AND
                               oe-ordl.pr-uom NE "LB" THEN
                               IF oe-ordl.qty LT 0 THEN -1
                               ELSE 1
                            ELSE
                            IF oe-ordl.pr-uom EQ "CS" THEN
                               oe-ordl.qty / (IF oe-ordl.cas-cnt NE 0 THEN oe-ordl.cas-cnt ELSE
                                  IF avail itemfg AND itemfg.case-count NE 0 THEN
                                           itemfg.case-count ELSE 1)
                             ELSE
                             IF oe-ordl.pr-uom EQ "C" THEN
                                oe-ordl.qty / 100
                             ELSE
                                oe-ordl.qty
                  
              v-tmp-price = v-tmp-price * oe-ordl.price
              v-price-per-1000 = v-tmp-price / ( oe-ordl.qty / 1000).
          END. /* pr-uom ne "M" */
          ELSE
             v-price-per-1000 = oe-ordl.price.

          IF NOT(xest.est-type EQ 4 OR xest.est-type EQ 8) THEN
          DO:
             FOR EACH probe WHERE
                 probe.company = xest.company AND
                 probe.est-no = xest.est-no AND
                 probe.probe-date NE ? AND
                 probe.est-qty EQ INT(oe-ordl.qty)
                 NO-LOCK
                 BY probe.probe-date DESC
                 BY probe.probe-time DESC:
                 
                 IF probe.sell-price EQ v-price-per-1000 OR
                    ROUND(probe.sell-price,2) EQ v-price-per-1000 THEN

                 LEAVE.
             END. /* each probe */
            
             IF NOT AVAIL probe THEN DO:
                 FOR EACH probe WHERE
                    probe.company = xest.company AND
                    probe.est-no = xest.est-no AND
                    probe.probe-date NE ? AND
                    probe.est-qty EQ INT(oe-ordl.qty)
                    NO-LOCK
                    BY probe.probe-date DESC
                    BY probe.probe-time DESC:
                    
                    LEAVE.
                END. /* EACH probe */
             END. /* not avail probe */
          END. /* est type 4 or 8 */
          ELSE
          DO:
             FOR EACH probe WHERE
                 probe.company = xest.company AND
                 probe.est-no = xest.est-no AND
                 probe.probe-date NE ?
                 NO-LOCK
                 BY probe.probe-date DESC
                 BY probe.probe-time DESC:
                 
                 IF probe.sell-price EQ v-price-per-1000 OR
                    ROUND(probe.sell-price,2) EQ v-price-per-1000 THEN

                 LEAVE.
             END.
            
             IF NOT AVAIL probe THEN
                FOR EACH probe WHERE
                    probe.company = xest.company AND
                    probe.est-no = xest.est-no AND
                    probe.probe-date NE ?
                    NO-LOCK
                    BY probe.probe-date DESC
                    BY probe.probe-time DESC:
                    
                    LEAVE.
                END.
          END. /* else est-type ne 4 or 8 */

          IF AVAIL probe THEN
          DO:
             ASSIGN
                oe-ordl.s-comm[1] = probe.comm
                xoe-ord.s-comm[1] = oe-ordl.s-comm[1]
                oe-ordl.q-qty     = probe.market-price
                v-margin          = probe.market-price.
         
             RELEASE probe.
          END. /* avail probe */
       END. /* avail sman and commbasis = "M" */
    END. /* oeestlog ne yes */ /* not avail oe-ordl */

END PROCEDURE.

PROCEDURE update-order-lines.
    /* dependencies ...
      v-est-type
      ll-from-tandem
      cocode
      w-ord-no
      tt-oe-ordl temp-table
      ll-do-entry (sets this)
      tt-item-qty-price temp-table
      v-est-no
      xoe-ord buffer
    */
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
                           INPUT TABLE tt-item-qty-price,
                           OUTPUT v-rowid-list, OUTPUT ll-canceled).
        IF ll-canceled THEN LEAVE do-all-order-lines.
      END. /* each oe-ordl */

      /* Start Transaction */
      DO TRANSACTION :
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
        END. /* each tt-oe-ordl */

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
           /* TRANSACTION */:

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
          END. /* form-no eq 0 or ... */

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
        END. /* if from tandem */
      END. /* end transaction */
    END. /* each w-ord */

    FOR EACH tt-oe-ordl BREAK BY tt-oe-ordl.ord-no:
      FIND FIRST oe-ordl WHERE ROWID(oe-ordl) EQ tt-oe-ordl.row-id NO-ERROR.
      v-qty-mod = NOT AVAIL oe-ordl OR tt-oe-ordl.qty NE oe-ordl.qty OR v-qty-mod.

      IF ll-canceled AND AVAIL oe-ordl THEN BUFFER-COPY tt-oe-ordl TO oe-ordl.

      IF LAST-OF(tt-oe-ordl.ord-no) THEN DO:
        FIND FIRST xoe-ord OF oe-ordl NO-LOCK NO-ERROR.
        IF AVAIL xoe-ord THEN RUN oe/calcordt.p (ROWID(xoe-ord)).
      END.
    END.

END PROCEDURE.



PROCEDURE was-canceled.
  /* dependencies ... 
    cocode
    xoe-ord buffer (deletes)
    w-ord temp-table    
  */
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
      END. /* each w-ord */
      /* End Transaction */

      adm-new-record = YES.
      RUN dispatch ("cancel-record").
      ll-is-new-rec = NO.
      IF NOT "{1}" EQ "SUPRESS" THEN
        RETURN NO-APPLY.
      ELSE
        RETURN "CANCEL".

END PROCEDURE. /* was canceled */

PROCEDURE was-not-canceled:

/* dependencies...
  temp-table w-ord
  cocode
  w-ord-no
  oe-ordl buffer
  tt-oe-ordl temp-table
  v-create-job
  oe-ord buffer
  v-qty-mod (logical)
  hld-id
  fil_id
  hld-nufile
  nufile
*/

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
    END. /* each w-ord */
    
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
        END. /* do transaction */

      IF NOT AVAIL job THEN DO:
          
        RUN create-job IN ip-callproc (OUTPUT lv-job-recid).
        FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
      END. /* if not avail job */
      
      IF AVAIL job AND INDEX("HWPRL",job.stat) NE 0 THEN DO:
        IF NOT v-qty-mod THEN RUN oe/job-qty.p (ROWID(oe-ord), OUTPUT v-qty-mod).
        
        IF job.stat EQ "P" OR v-qty-mod THEN DO:
          RUN jc/chkrebld.p (RECID(job), OUTPUT choice).     
          IF NOT choice THEN DO:
            ASSIGN hld-id     = fil_id
                   hld-nufile = nufile 
                   hld-stat   = job.stat
                   nufile     = YES.
            
            RUN jc/jc-calc.p(RECID(job), NO).
            ASSIGN fil_id   = hld-id
                   nufile   = hld-nufile.
            /* Start Transaction */
            IF hld-stat NE "P" THEN DO TRANSACTION:
              FIND CURRENT job EXCLUSIVE.
              job.stat = hld-stat.
              FIND CURRENT job NO-LOCK.
            END. /* hld-stat ne "P", end transaction  */
          END. /* if not choice */
        END.  /* job-stat = "P" or ... */
      END. /* avail job and ... */

     
            
      FOR EACH w-ord,
          EACH oe-ordl NO-LOCK
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ w-ord-no
            AND oe-ordl.is-a-component EQ NO

          BREAK BY oe-ordl.job-no
                BY oe-ordl.job-no2:

        /* IF LAST-OF(oe-ordl.job-no2) THEN */ DO:

          ASSIGN
           hld-id     = fil_id
           hld-nufile = nufile
           fil_id     = RECID(oe-ordl).
         
          /*IF oe-ordl.ord-no NE oe-ord.ord-no THEN*/ 
          /* Only prompt for rm items on first one */
          RUN po/doPo.p (FIRST-OF(oe-ordl.job-no2)).

          /* check oe-ordl.due-date and calc promised date and job's start-date */
          IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date IN ip-callproc.

          STATUS DEFAULT "".
          ASSIGN
           fil_id = hld-id
           nufile = hld-nufile.
        END. /* last of ordl.job-no2 */
      END. /* each w-ord */

    END.  /* if v-create-job then do */

    

    FOR EACH tt-oe-ordl
        WHERE tt-oe-ordl.to-be-deleted
          AND NOT CAN-FIND(FIRST oe-ordl WHERE ROWID(oe-ordl) EQ tt-oe-ordl.row-id):
      CREATE oe-ordl.
      BUFFER-COPY tt-oe-ordl TO oe-ordl.
    END. /* each tt-oe-ordl */

END PROCEDURE. /* was-not-canceled */

PROCEDURE recalc-estimate:
/* dependencies ...
  v-run-list
  v-est-type
  save_id
  fil_id
  v-job-no
  v-job-no2
  j
  xest buffer
  xeb buffer
  fi_id
  cocode
  xoe-ord buffer (finds the record)
  w-ord temp-table (creates a record)
  
*/
    /* RECALC ESTIMATE */
    save_id = RECID(oe-ord).

    IF v-est-type EQ 3 OR v-est-type EQ 4 THEN  /* not done ??? */
      RUN VALUE(ENTRY(v-est-type + INT(xest.est-type EQ 8),v-run-list)).

    ASSIGN
     j         = 1
     v-job-no  = IF oe-ord.job-no <> "" THEN oe-ord.job-no ELSE STRING(oe-ord.ord-no)
     v-job-no2 = oe-ord.job-no2
     fil_id    = save_id.        /* reset fil_id, scrambled in calc...*/

    FIND FIRST xeb
        WHERE xeb.company = xest.company 
          AND xeb.est-no EQ xest.est-no
          AND xeb.form-no  EQ 0
        NO-LOCK NO-ERROR.
    /* Start Transaction */
    FOR EACH eb
        WHERE eb.company = xest.company 
          AND eb.est-no  EQ xest.est-no
          AND eb.form-no  NE 0
          AND eb.blank-no NE 0
          AND TRIM(eb.cust-no) NE ""
        NO-LOCK,
        FIRST ef OF eb NO-LOCK,
        FIRST cust NO-LOCK
        {sys/ref/custW.i}
          AND cust.cust-no EQ eb.cust-no
        USE-INDEX cust    
        BREAK BY eb.est-no BY eb.cust-no BY eb.form-no BY eb.blank-no
        TRANSACTION:

      FIND xoe-ord WHERE RECID(xoe-ord) EQ fil_id.

      IF FIRST-OF(eb.cust-no) AND ip-new-ord THEN DO:


         IF NOT FIRST(eb.cust-no) THEN DO:
            RUN create-new-order.

         END.  /* not first */

         xoe-ord.lead-days = xoe-ord.last-date - xoe-ord.last-date.

         FIND FIRST shipto WHERE shipto.company EQ cocode
                             AND shipto.cust-no EQ cust.cust-no
                             NO-LOCK NO-ERROR.
         IF avail shipto THEN
            ASSIGN
            xoe-ord.ship-i[1] = shipto.notes[1]
            xoe-ord.ship-i[2] = shipto.notes[2]
            xoe-ord.ship-i[3] = shipto.notes[3]
            xoe-ord.ship-i[4] = shipto.notes[4].

         CREATE w-ord.
         w-ord-no = xoe-ord.ord-no.
      END. /* first-of(eb.cust-no) */

      RELEASE oe-ordl.
      RUN create-order-lines.  
      if v-est-type eq 2 then leave. /** 2pc box & Set headers **/
    END.
END PROCEDURE. /* recalc estimate */
