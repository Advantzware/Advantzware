


/*------------------------------------------------------------------------
    File         : UpdEBLook.p
    Purpose     :  estimate lookup

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUpdateEstItem NO-UNDO
        FIELD price      AS DECIMAL
        FIELD pr-uom     AS CHARACTER
        FIELD part-dscr2 AS CHARACTER
        FIELD vdscr      AS CHARACTER
        FIELD tax        AS CHARACTER
        FIELD est-no     AS CHARACTER
        FIELD i-no       AS CHARACTER
        FIELD part-no    AS CHARACTER
        FIELD i-name     AS CHARACTER
        FIELD part-dscr1 AS CHARACTER
        FIELD qty        AS DECIMAL
        FIELD cas-cnt    AS INT
        FIELD cases-unit AS INT
        FIELD s-pct      AS DECIMAL
        FIELD s-man      AS CHAR
        FIELD s-name     AS CHAR
        FIELD req-date   AS DATE
        FIELD prom-date  AS DATE
        FIELD job-no2    AS INT
        FIELD job-no     AS CHARACTER
        FIELD s-comm     AS DECIMAL
        FIELD type-code  AS CHAR
        FIELD vtype      AS CHAR
        FIELD t-price    AS DECIMAL
        FIELD disc       AS DECIMAL
        FIELD company    AS CHAR
        FIELD ord-no     AS INT
        FIELD cust-no    AS CHAR
        FIELD po-no      AS CHAR
        FIELD req-code   AS CHAR 
        FIELD prom-code  AS CHAR
        FIELD over-pct   AS DECIMAL
        FIELD under-pct  AS DECIMAL
        .
DEFINE DATASET dsUpdateItemEst FOR ttUpdateEstItem.

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrder      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmEst      AS CHARACTER  NO-UNDO.
IF prmUser = ?  THEN ASSIGN prmUser = "".
IF prmOrder = ? THEN ASSIGN prmOrder = 0.
IF prmEst = ?   THEN ASSIGN prmEst = "".

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUpdateItemEst.

DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
  
def {1} {2} NEW SHARED var cocode     as   char  format "x(3)"  no-undo.
def {1} {2} NEW SHARED VAR locode     as   char  format "x(5)"  no-undo.
def var v-tmp-price as dec format ">,>>>,>>9.9999" no-undo.
  def var lv-t-price as dec no-undo.
def var ls-stock as cha no-undo.  /* for eb.stock-no */
DEF VAR lv-new-tandem AS ROWID NO-UNDO.
DEF VAR ll-is-tandem AS LOG NO-UNDO.
DEF BUFFER xest FOR est.
DEF VAR v-quo-price-log AS LOG.
DEF VAR ll-got-qtprice AS LOG. 
DEF VAR v-est-fg AS LOG.
DEF VAR v-est-fg1 AS CHAR.
def var lv-qty LIKE oe-ordl.qty no-undo.
def var lv-price as dec no-undo.
def var i as int no-undo.
def var lv-pr-uom as cha no-undo.
DEF VAR lv-i-no AS CHAR NO-UNDO.
DEF VAR v-job-no LIKE oe-ordl.job-no NO-UNDO.
DEF VAR v-job-no2 LIKE oe-ordl.job-no2 NO-UNDO.
def var li-cnt as int no-undo.
DEF VAR ll-tax AS LOG NO-UNDO.
DEF VAR ll-do-job AS LOG NO-UNDO.
def var li-cases as int no-undo.
DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.
DEF VAR oeestcom-log AS LOG.
DEFINE VARIABLE v-disp-prod-cat AS CHARACTER  NO-UNDO.
DEF VAR ld-marg% AS DEC NO-UNDO.
DEF VAR v-com AS DEC NO-UNDO.
DEF BUFFER b-eb FOR eb.
DEF BUFFER b-oe-ordl FOR oe-ordl.
MESSAGE "prmOrder"  prmOrder prmEst vEst.
 
FIND first oe-ord  WHERE oe-ord.ord-no = prmOrder  no-lock no-error.
MESSAGE "utest"  oe-ord.ord-no oe-ord.cust-no .
FOR EACH  eb where eb.est-no = vEst no-lock :

IF eb.stock-no NE "" THEN
FOR EACH itemfg
    where itemfg.company eq eb.company
    and itemfg.i-no    eq eb.stock-no
    no-lock :

CREATE ttUpdateEstItem.    
assign
        ttUpdateEstItem.price      = itemfg.sell-price
        ttUpdateEstItem.pr-uom     = itemfg.sell-uom
        ttUpdateEstItem.part-dscr2 = itemfg.part-dscr2.
    
        FIND FIRST cust WHERE 
            /*{sys/ref/cust.w}AND*/ 
            cust.cust-no EQ oe-ord.cust-no AND cust.company = oe-ord.company
            USE-INDEX cust
            NO-LOCK NO-ERROR.
        ASSIGN
        ttUpdateEstItem.disc      = cust.disc .
        ll-tax = AVAIL cust AND cust.sort EQ "Y" AND oe-ord.tax-gr NE "" AND itemfg.taxable.
        IF NOT ll-tax THEN DO:
            {custom/shptotax.i oe-ord.cust-no oe-ord.sold-id ll-tax}
        END.   /*IF NOT ll-tax THEN DO:**/
            IF ll-tax = TRUE THEN ASSIGN ttUpdateEstItem.tax = "Yes" .
END.    /*if avail itemfg THEN DO:*/

assign
    ttUpdateEstItem.company   = prmComp
    ttUpdateEstItem.ord-no    = oe-ord.ord-no
    ttUpdateEstItem.type-code = oe-ord.type
    ttUpdateEstItem.cust-no   = oe-ord.cust-no
    ttUpdateEstItem.po-no     = oe-ord.po-no
    ttUpdateEstItem.req-code  = oe-ord.due-code
    ttUpdateEstItem.req-date  = oe-ord.due-date
    ttUpdateEstItem.prom-code = oe-ord.due-code
    ttUpdateEstItem.prom-date = oe-ord.due-date
    
    ttUpdateEstItem.over-pct  = oe-ord.over-pct   
    ttUpdateEstItem.under-pct = oe-ord.under-pct
    .

FIND est WHERE est.company = eb.company
    AND est.est-no = vEst NO-LOCK NO-ERROR.

RUN default-type (BUFFER itemfg).
lv-qty = IF est.est-type EQ 3 OR
    est.est-type EQ 4 OR
    est.est-type EQ 8 THEN eb.bl-qty ELSE eb.eqty.
    FIND FIRST est-qty NO-LOCK
        WHERE est-qty.company EQ prmComp
        AND est-qty.est-no  EQ vEst
        NO-ERROR.
    IF AVAIL est-qty AND est-qty.qty[1] NE 0                               AND
        (est-qty.qty[2] NE 0 OR est-qty.qty[3] NE 0 OR est-qty.qty[4] NE 0) THEN
       /*RUN oe/d-ordqty.w (RECID(est-qty), OUTPUT lv-qty).*/
        
  /*find est-qty where recid(est-qty) = recid(est) no-lock.**/
  
  do i = 1 to 30:
    if est-qty.qty[i] <> 0 then 
    do:
       
       assign lv-qty = est-qty.qty[i].
              .
    end. 
  end.
  IF est.est-type EQ 2 OR est.est-type EQ 6 THEN DO:
        ll-do-job = CAN-FIND(FIRST b-eb WHERE b-eb.company EQ eb.company
                             AND b-eb.est-no  EQ eb.est-no
                             AND b-eb.pur-man EQ NO
                             AND b-eb.form-no NE 0).
        FIND FIRST b-eb
            WHERE b-eb.company EQ est.company
            AND b-eb.est-no  EQ est.est-no
            AND b-eb.form-no NE 0
            NO-LOCK NO-ERROR.
     END.  /*IF est.est-type EQ 2 */
     ELSE DO:
         FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) NO-LOCK NO-ERROR.
         ll-do-job = NOT b-eb.pur-man.
     END.   /*ELSE DO:*/

/*     IF NOT ll-do-job THEN ll-do-job = job#-int EQ 0.*/

     RUN est/getcscnt.p ((IF eb.est-type EQ 6 AND
                             eb.cas-no NE ""  THEN ROWID(eb) ELSE ROWID(b-eb)),
                         OUTPUT li-cnt,OUTPUT li-cases).

     ASSIGN
      ls-stock = eb.stock-no
      ttUpdateEstItem.est-no     = eb.est-no
      ttUpdateEstItem.i-no      = eb.stock-no
      ttUpdateEstItem.part-no    = eb.part-no
      ttUpdateEstItem.i-name    = eb.part-dscr1 
      ttUpdateEstItem.part-dscr1 = eb.part-dscr2
      ttUpdateEstItem.qty        = lv-qty
      ttUpdateEstItem.cas-cnt    = li-cnt
      ttUpdateEstItem.cases-unit = li-cases
      ttUpdateEstItem.s-pct     = 100
      ttUpdateEstItem.type-code = "0".
      
      
     
         IF ttUpdateEstItem.pr-uom EQ "" THEN
             ttUpdateEstItem.pr-uom = "M".
         IF INT(ttUpdateEstItem.qty) GT 0 THEN
             IF INT(ttUpdateEstItem.qty) LT INT(ttUpdateEstItem.cas-cnt) THEN
                 ttUpdateEstItem.cas-cnt = int(ttUpdateEstItem.qty).
             ELSE
                 IF INT(ttUpdateEstItem.cas-cnt) EQ 0 AND ttUpdateEstItem.i-no NE "0" THEN
                     ttUpdateEstItem.cas-cnt = 1.
                 
                 IF INT(ttUpdateEstItem.cases-unit) EQ 0 THEN
                     ttUpdateEstItem.cases-unit = 1.
                 IF AVAIL b-eb THEN DO:
                          ttUpdateEstItem.s-man  = b-eb.sman.
                          FIND FIRST sman WHERE sman.sman = b-eb.sman NO-LOCK NO-ERROR.
                                           ttUpdateEstItem.s-name = sman.sname.
                 IF   CAN-FIND(FIRST style WHERE style.company EQ b-eb.company
                                  AND style.style   EQ b-eb.style
                                  AND style.type    EQ "F") THEN DO:
                
                 IF DATE(ttUpdateEstItem.req-date)  GT
                     DATE(ttUpdateEstItem.prom-date) THEN
                     ttUpdateEstItem.prom-date = ttUpdateEstItem.req-date.
                 END.
                 END. /*avail b-eb*/
                 IF ttUpdateEstItem.i-no EQ "" THEN DO:
                     IF v-est-fg THEN lv-i-no = eb.part-no.
                     ELSE
                         IF v-est-fg1 NE "Manual" THEN DO:
                             FIND FIRST itemfg
                                 WHERE itemfg.company EQ eb.company
                                 AND itemfg.part-no EQ eb.part-no
                                 AND itemfg.cust-no eq eb.cust-no
                                 NO-LOCK no-error.
                             IF AVAIL itemfg THEN
                                 ASSIGN
                                 lv-i-no                         = itemfg.i-no
                                 ttUpdateEstItem.part-dscr2 = itemfg.part-dscr2.
                         END.  /*IF v-est-fg1 NE "Manual" THEN DO:*/
                 
                     IF v-est-fg1 EQ "Hughes" THEN RUN fg/hughesfg.p (ROWID(eb), OUTPUT lv-i-no).
                     ELSE
                         IF v-est-fg1 EQ "Fibre"  THEN RUN fg/fibre-fg.p (ROWID(eb), OUTPUT lv-i-no).
                         
                         IF lv-i-no NE "" THEN ttUpdateEstItem.i-no = lv-i-no.
                END. /* IF ttUpdateEstItem.i-no EQ "" THEN DO:*/
                
                IF  ll-do-job THEN DO:   /*lv-new-tandem EQ ? AND*/
                    v-disp-prod-cat = eb.procat .
                    v-job-no = fill(" ",6 - length(trim(string(prmOrder)))) + string(prmOrder).
                    RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-disp-prod-cat).
                    IF v-job-no NE "" THEN DO:
                        ASSIGN
                            ttUpdateEstItem.job-no  = v-job-no
                            ttUpdateEstItem.job-no2 = v-job-no2.
                        
                    END.  /*IF v-job-no NE "" THEN DO:*/
                    for each job-hdr  where job-hdr.company eq cocode
                        and job-hdr.job-no  eq oe-ord.job-no
                        use-index job-no no-lock
                        by job-hdr.job-no desc by job-hdr.job-no2 desc:
                        ASSIGN
                        
                        ttUpdateEstItem.job-no2 = (if avail job-hdr then job-hdr.job-no2 + 1 else 0).
                         
                    end.   /*for each job-hdr  where job-hdr.company eq cocode*/
                END. /* lv-new-tandem EQ ? AND ll-do-job */
                find xest where xest.company = eb.company and
                    xest.est-no = eb.est-no no-lock no-error.
                
                assign 
                    lv-price  = ttUpdateEstItem.price
                    lv-pr-uom = ttUpdateEstItem.pr-uom.
                
                if avail xest and v-quo-price-log AND NOT ll-got-qtprice then do:
                    ll-got-qtprice = yes.
                    run oe/getqpric.p (recid(xest), ttUpdateEstItem.part-no,
                                       ttUpdateEstItem.i-no,
                                       int(ttUpdateEstItem.qty),
                                       input-output lv-price,
                                       input-output lv-pr-uom,
                                       OUTPUT lv-q-no).
                END.  /* if avail xest and do:*/
                assign
                    ttUpdateEstItem.price  = lv-price
                    ttUpdateEstItem.pr-uom = lv-pr-uom.
                
              /* {oe/ordltot.i oe-ordl qty oe-ordl}
                oe-ordl.rec_key = est.rec_key.      */
RUN ordltot.




                
                IF oeestcom-log = YES AND AVAIL b-eb THEN
                    DO:
                    FOR EACH probe WHERE
                        probe.company = b-eb.company and
                        probe.est-no = b-eb.est-no and
                        probe.probe-date ne ? and
                        probe.est-qty eq INT(ttUpdateEstItem.qty) AND
                        probe.sell-price EQ DEC(ttUpdateEstItem.price)
                        NO-LOCK
                        BY probe.probe-date DESC
                        BY probe.probe-time DESC:
                    
                    END.  /*    FOR EACH probe */
                    IF NOT AVAIL probe THEN
                        FOR EACH probe WHERE
                        probe.company = b-eb.company and
                        probe.est-no = b-eb.est-no and
                        probe.probe-date ne ? and
                        probe.est-qty eq INT(ttUpdateEstItem.qty)
                        NO-LOCK
                        BY probe.probe-date DESC
                        BY probe.probe-time DESC:
                        
                        IF AVAIL probe THEN
                            DO:
                            ld-marg% = probe.market-price.
                        END.  /*IF AVAIL probe THEN*/
                        END.
                END.  /*IF oeestcom-log = YES AND AVAIL b-eb THEN*/
                RUN est/getsmanmtrx.p (INPUT ROWID(est),
                               INPUT "C",
                               INPUT-OUTPUT v-com,
                               INPUT-OUTPUT ld-marg%).

        ttUpdateEstItem.s-comm = v-com.
               
     END.
 
/*******************************************************************/
  PROCEDURE default-type :
         DEF PARAM BUFFER io-itemfg FOR itemfg.
             DEF BUFFER def-oe-ordl FOR oe-ordl.
             
    IF ttUpdateEstItem.type-code NE "T"  THEN DO:
      ttUpdateEstItem.type-code = "O".

      /*IF AVAIL io-itemfg THEN DO:
        IF TRIM(itemfg.type-code) NE ""  AND
           itemfg.type-code       NE "T" THEN
          ttUpdateEstItem.type-code = io-itemfg.type-code.

        IF ttUpdateEstItem.type-code EQ "O"                 AND
           CAN-FIND(FIRST def-oe-ordl
                    WHERE def-oe-ordl.company EQ io-itemfg.company
                      /*AND def-oe-ordl.i-no    EQ io-itemfg.i-no*/
                      AND def-oe-ordl.ord-no  LT oe-ordl.ord-no
                      AND ROWID(def-oe-ordl)  NE ROWID(oe-ordl)) THEN
          ttUpdateEstItem.type-code = "R".
      END.*/
    END.

   /* RUN new-type.*/
  
END PROCEDURE.


/*******************************************************/
/* -------------------------------------------------- oe/ordltot.i 6/93 rd  */
/* o/e module - Calculate order line ext. price                              */
/* -------------------------------------------------------------------------- */
PROCEDURE ordltot:
FIND FIRST ttUpdateEstItem NO-LOCK NO-ERROR.
MESSAGE "jyoti" ttUpdateEstItem.i-no  ttUpdateEstItem.pr-uom   ttUpdateEstItem.cas-cnt ttUpdateEstItem.qty.
  find first itemfg   WHERE itemfg.company = prmComp and
       itemfg.i-no eq ttUpdateEstItem.i-no
      no-lock no-error.
      
  assign
   v-tmp-price = if ttUpdateEstItem.pr-uom begins "L" AND ttUpdateEstItem.pr-uom NE "LB" then
                   if ttUpdateEstItem.qty lt 0 then -1 else 1
                 else
                 if ttUpdateEstItem.pr-uom eq "CS" then
                   ttUpdateEstItem.qty / (if ttUpdateEstItem.cas-cnt ne 0 then ttUpdateEstItem.cas-cnt else
                                    if avail itemfg and itemfg.case-count ne 0
                                                   then itemfg.case-count else
                                                        1)
                 else
                 if ttUpdateEstItem.pr-uom eq "C" then
                    ttUpdateEstItem.qty / 100
                 else
                 if  ttUpdateEstItem.pr-uom eq "M" then
                    ttUpdateEstItem.qty / 1000
                 else
                    ttUpdateEstItem.qty.
                            
    lv-t-price = v-tmp-price * ttUpdateEstItem.price.
    /*{1}.t-price:screen-value = string(round(lv-t-price - (lv-t-price * INPUT {1}.disc / 100),2)).*/
    ttUpdateEstItem.t-price = (lv-t-price - ROUND(lv-t-price * ttUpdateEstItem.disc / 100,2)).
          

        /*STRING(
          IF v-print-fmt EQ "Dayton" THEN 
        ELSE
            ROUND(lv-t-price * (1 - (ttUpdateEstItem.disc / 100)),2)).*/

/* end ---------------------------------- copr. 1992  advanced software, inc. */
  END PROCEDURE.
