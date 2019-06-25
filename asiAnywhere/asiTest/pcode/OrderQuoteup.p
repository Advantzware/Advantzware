

/*------------------------------------------------------------------------
    File         : OrderQuoteup.p
    Purpose     :  quote lookup

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrderQuoteup NO-UNDO
    
    FIELD Estimate AS CHARACTER
    FIELD Customer AS CHARACTER    
    FIELD shipname   AS CHARACTER
    FIELD dscr     AS CHARACTER
    FIELD partno    AS CHARACTER
    FIELD Rfqno     AS INTEGER
    FIELD Quoteno   AS INTEGER
    FIELD soldid   AS CHARACTER
    FIELD sman     AS CHARACTER
    FIELD sdscr     AS CHARACTER
    FIELD Carrier  AS CHARACTER 
    FIELD freight  AS CHARACTER
    FIELD Spct     AS DECIMAL
    FIELD DueCode  AS CHARACTER
    FIELD JobNo    AS CHARACTER
    FIELD JobNo2   AS INTEGER
    FIELD CName    AS CHARACTER
    FIELD CAddr    AS CHARACTER
    FIELD CAddr2   AS CHARACTER
    FIELD CCity    AS CHARACTER
    FIELD CState   AS CHARACTER
    FIELD CZip     AS CHARACTER
    FIELD Contact  AS CHARACTER
    FIELD LstDate  AS DATE
    FIELD ShipDay  AS INT
    FIELD DueDate  AS DATE
    FIELD Terms    AS CHARACTER
    FIELD Tdscr    AS CHARACTER
    FIELD OverPct  AS DECIMAL
    FIELD UnderPct AS DECIMAL
    FIELD FobCode  AS CHARACTER
    FIELD TaxGr    AS CHARACTER
    FIELD CType    AS CHARACTER
    FIELD ItemDesc    AS CHARACTER
    FIELD CustPart    AS CHARACTER
    FIELD soldname    AS CHARACTER
    FIELD soldadd1    AS CHARACTER
    FIELD soldadd2    AS CHARACTER
    FIELD soldcity    AS CHARACTER
    FIELD soldstat    AS CHARACTER
    FIELD soldzip     AS CHARACTER
    FIELD Comm        AS DECIMAL
    FIELD salesman    AS CHARACTER
    FIELD Order       AS INTEGER
    FIELD prevord     AS INTEGER
    FIELD rfq         AS INTEGER
    FIELD xyz         AS INTEGER
    FIELD esttype     AS INTEGER
      .
DEFINE DATASET dsOrderQuoteup FOR ttOrderQuoteup.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote      AS INT  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderQuoteup.

DEFINE VAR vEst AS CHAR NO-UNDO.
def var v-est-no like est.est-no no-undo.
def var v-est-type like est.est-type no-undo.
def var v-factor as dec no-undo.
def var v-run-list as char init
                "oe/calc-one.p,oe/calc-box.p,ce/tan/print4.p,ce/com/print4.p".
def var i as int no-undo.
def var j as int no-undo.
def var x as int no-undo.
def var nufile as log no-undo.
def var v-blk-qty as int no-undo.
def var v-tax-rate as dec form "->>>.99" no-undo.
def var v-frt-tax-rate as dec form "->>>,99" no-undo.
def var v-quo-price like sys-ctrl.log-fld no-undo.
def var li-line-no as int no-undo.
def var choice as log no-undo.
def var hld-id as recid no-undo.
def var hld-stat like job.stat no-undo.
def var hld-nufile as log no-undo.
def var oeestcom-log as log no-undo.
def var lv-pr-uom as cha no-undo.
DEF VAR lv-date AS DATE NO-UNDO.
DEF VAR ll-do-job AS LOG NO-UNDO.
DEFINE VARIABLE v-prod-cat AS CHARACTER  NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR lv-qty AS INT NO-UNDO.
DEF VAR ld-marg% AS DEC NO-UNDO.
DEF VAR v-com AS DEC NO-UNDO.
DEFINE VAR Ordernum   AS INTEGER.
def var v-job-no like oe-ord.job-no no-undo.
def var v-job-no2 like oe-ord.job-no2 no-undo.
def {1} {2} NEW SHARED var cocode     as   char  format "x(3)"  no-undo.
def {1} {2} NEW SHARED VAR locode     as   char  format "x(5)"  no-undo.
DEF VAR vQuote AS INT NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.


IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmComp        = ? THEN ASSIGN prmComp        = "".
IF prmQuote       = ? THEN ASSIGN prmQuote       = 0.


FIND FIRST usercomp WHERE
 usercomp.user_id = prmUser AND
 usercomp.loc = '' AND
 usercomp.company_default = YES
 NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
    cocode = prmComp .
{sys/inc/lastship.i}

 
 DEFINE VAR custcount AS CHAR NO-UNDO.

FOR EACH usercust WHERE
    usercust.user_id =  prmUser AND
    usercust.company = prmComp
     NO-LOCK:
    ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END. /* end of usercust*/

IF prmAction = "OrderQuote"  THEN DO:
       
        FOR EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK BY quotehd.q-no DESC: 
           IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
          
          FOR  EACH quoteitm WHERE quoteitm.q-no =  quotehd.q-no  NO-LOCK: 
                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no AND eb.company = prmComp and eb.form-no ne 0  NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttOrderQuoteup.
            assign                                     
                ttOrderQuoteup.Estimate   = quotehd.est-no
                ttOrderQuoteup.Customer   = quotehd.cust-no 
                ttOrderQuoteup.shipname   = eb.ship-name
                ttOrderQuoteup.dscr       = quoteitm.part-dscr1
                ttOrderQuoteup.partno     = quoteitm.part-no 
                ttOrderQuoteup.Rfqno      = int(quotehd.rfq)
                ttOrderQuoteup.Quoteno    = quotehd.q-no 
                  . 
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = quotehd.est-no.
                   run util/rjust.p (input-output v-est-no,8).
                   vEst = v-est-no.
                   ASSIGN
                    v-est-type = eb.est-type - IF eb.est-type GT 4 THEN 4 ELSE 0
                        ll-do-job  = CAN-FIND(FIRST eb WHERE eb.company EQ prmComp
                                              AND eb.est-no  EQ v-est-no
                                              AND eb.pur-man EQ NO
                                              AND eb.form-no NE 0).
                    j = 1.
                    FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = eb.cust-no NO-LOCK NO-ERROR.
                    IF AVAIL cust THEN
                        v-prod-cat = eb.procat.
                    IF ll-do-job THEN DO:
                        v-job-no = string(Ordernum) .
                        RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-prod-cat).
                        IF v-job-no EQ "" THEN
                            v-job-no = fill(" ",6 - length(trim(vEst))) + trim(vEst).
                    END.
                    ELSE
                        ASSIGN
                            v-job-no  = ""
                            v-job-no2 = 0.
                        assign 
                            ttOrderQuoteup.prevord      = oe-ord.ord-no
                            ttOrderQuoteup.Order      = Ordernum
                            ttOrderQuoteup.Estimate   = eb.est-no
                            ttOrderQuoteup.soldid     = eb.cust-no 
                            ttOrderQuoteup.sman       = eb.sman 
                            ttOrderQuoteup.Customer   = eb.cust-no
                            ttOrderQuoteup.Carrier    = eb.carrier 
                            ttOrderQuoteup.freight    = eb.chg-method
                            ttOrderQuoteup.Spct       = 100          
                            ttOrderQuoteup.DueCode    = "ON"          
                            ttOrderQuoteup.JobNo      = v-job-no         
                            ttOrderQuoteup.JobNo2     = v-job-no2         
                            ttOrderQuoteup.CName      = cust.NAME          
                            ttOrderQuoteup.CAddr      = cust.addr[1]          
                            ttOrderQuoteup.CAddr2     = cust.addr[2]            
                            ttOrderQuoteup.CCity      = cust.city          
                            ttOrderQuoteup.CState     = cust.state         
                            ttOrderQuoteup.CZip       = cust.zip           
                            ttOrderQuoteup.Contact    = cust.contact          
                            ttOrderQuoteup.LstDate    = (today + cust.ship-days)          
                            ttOrderQuoteup.DueDate    = (today + cust.ship-days)       
                            ttOrderQuoteup.Terms      = cust.terms                        
                            ttOrderQuoteup.OverPct    = cust.over-pct           
                            ttOrderQuoteup.UnderPct   = cust.under-pct            
                            ttOrderQuoteup.FobCode    = cust.fob-code                     
                            ttOrderQuoteup.TaxGr      = cust.tax-gr                          
                            ttOrderQuoteup.CType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN  
                            
                            ttOrderQuoteup.soldname = soldto.sold-name 
                            ttOrderQuoteup.soldadd1 = soldto.sold-addr[1] 
                            ttOrderQuoteup.soldadd2 = soldto.sold-addr[2]
                            ttOrderQuoteup.soldcity = soldto.sold-city
                            ttOrderQuoteup.soldstat = soldto.sold-state
                            ttOrderQuoteup.soldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttOrderQuoteup.salesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttOrderQuoteup.ItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttOrderQuoteup.Comm = eb.comm.
                        ELSE
                            DO:
                            lv-qty = 0.
                            find first est-qty where
                                est-qty.company = prmComp AND
                                est-qty.est-no = eb.est-no
                                no-lock no-error.
                            if avail est-qty THEN
                                lv-qty = est-qty.qty[1].
                            v-qty = if NOT(v-est-type eq 3 or v-est-type eq 4) THEN
                                lv-qty
                                ELSE
                                    eb.bl-qty.
                       FOR EACH probe WHERE
                           probe.company = eb.company and
                           probe.est-no = eb.est-no and
                           probe.probe-date ne ? and
                           probe.est-qty eq v-qty
                           NO-LOCK
                           BY probe.probe-date DESC
                           BY probe.probe-time DESC:
                           
                           LEAVE.
                       END.
                       IF AVAIL probe THEN
                           DO:
                           ld-marg% = probe.market-price.
                           
                           RUN est/getsmanmtrx.p (INPUT ROWID(eb),
                                                  INPUT "C",
                                                  INPUT-OUTPUT v-com,
                                                  INPUT-OUTPUT ld-marg%).
                           ttOrderQuoteup.Comm = v-com.
                       
                       END.
                       ELSE
                           ttOrderQuoteup.Comm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttOrderQuoteup.TDscr  = terms.dscr.
                       else ttOrderQuoteup.TDscr   = "".
                            if cust.active eq "X" then ttOrderQuoteup.CTyp = "T".
                            if ttOrderQuoteup.Carrier eq "" then ttOrderQuoteup.Carrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN 
                               ttOrderQuoteup.esttype = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttOrderQuoteup.LstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttOrderQuoteup.DueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           

               END.
            END.  /*do transaction*/
     END. /* if avail quotehd*/
    
END.  /* end of seach */
