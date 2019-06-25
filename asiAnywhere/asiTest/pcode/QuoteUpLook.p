


/*------------------------------------------------------------------------
    File         : QuoteUpLook.p
    Purpose     :  quote lookup

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttQuoteUpLook NO-UNDO
    
    FIELD vEstimate AS CHARACTER
    FIELD vCustomer AS CHARACTER    
    FIELD vshipname   AS CHARACTER
    FIELD vdscr     AS CHARACTER
    FIELD vpartno    AS CHARACTER
    FIELD vRfqno     AS INTEGER
    FIELD vQuoteno   AS INTEGER
    FIELD vsoldid   AS CHARACTER
    FIELD vsman     AS CHARACTER
    FIELD vsdscr     AS CHARACTER
    FIELD vCarrier  AS CHARACTER 
    FIELD vfreight  AS CHARACTER
    FIELD vSpct     AS DECIMAL
    FIELD vDueCode  AS CHARACTER
    FIELD vJobNo    AS CHARACTER
    FIELD vJobNo2   AS INTEGER
    FIELD vCName    AS CHARACTER
    FIELD vCAddr    AS CHARACTER
    FIELD vCAddr2   AS CHARACTER
    FIELD vCCity    AS CHARACTER
    FIELD vCState   AS CHARACTER
    FIELD vCZip     AS CHARACTER
    FIELD vContact  AS CHARACTER
    FIELD vLstDate  AS DATE
    FIELD vShipDay  AS INT
    FIELD vDueDate  AS DATE
    FIELD vTerms    AS CHARACTER
    FIELD vTdscr    AS CHARACTER
    FIELD vOverPct  AS DECIMAL
    FIELD vUnderPct AS DECIMAL
    FIELD vFobCode  AS CHARACTER
    FIELD vTaxGr    AS CHARACTER
    FIELD vCType    AS CHARACTER
    FIELD vItemDesc    AS CHARACTER
    FIELD vCustPart    AS CHARACTER
    FIELD vsoldname    AS CHARACTER
    FIELD vsoldadd1    AS CHARACTER
    FIELD vsoldadd2    AS CHARACTER
    FIELD vsoldcity    AS CHARACTER
    FIELD vsoldstat    AS CHARACTER
    FIELD vsoldzip     AS CHARACTER
    FIELD vComm        AS DECIMAL
    FIELD vsalesman    AS CHARACTER
    FIELD vOrder       AS INTEGER
    FIELD vprevord     AS INTEGER
    FIELD vrfq         AS INTEGER
    FIELD vEstType     AS INTEGER 
    FIELD vFgNo     AS CHARACTER 
    FIELD vxyz         AS INTEGER
      .
DEFINE DATASET dsQuoteUpLookup FOR ttQuoteUpLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHAR NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuoteUpLookup.

DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
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
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmCust        = ? THEN ASSIGN prmCust        = "".


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
    usercust.company = prmComp AND (usercust.cust-no = prmCust OR prmCust = "")
     NO-LOCK:
   
     ASSIGN 
     custcount = custcount + "," + usercust.cust-no .
    
END. /* end of usercust*/


IF prmAction <> "search"  THEN DO:
        v-count = 0.
        MAIN-LOOP:
        FOR EACH quotehd WHERE   quotehd.company = prmComp NO-LOCK BY quotehd.q-no DESC: 
           IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
          
          FOR  EACH quoteitm WHERE quoteitm.q-no =  quotehd.q-no  NO-LOCK: 
                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no AND eb.company = prmComp and eb.form-no ne 0 NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quotehd.est-no
                ttQuoteUpLook.vCustomer   = quotehd.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quotehd.rfq)
                ttQuoteUpLook.vQuoteno    = quotehd.q-no 
                ttQuoteUpLook.vFgNo       = quoteitm.i-no
                  . 

            IF  ttQuoteUpLook.vFgNo = "" THEN ASSIGN ttQuoteUpLook.vFgNo       = eb.stock-no.
              
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
                            ttQuoteUpLook.vprevord    = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN
                                     ASSIGN
                                        ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           v-count = v-count + 1.
                                           IF v-count = 50 THEN LEAVE MAIN-LOOP.
                                         

               END.
            END.  /*do transaction*/
     END. /* if avail quotehd*/
    
END.  /* end of seach */

IF prmAction = "search"  THEN DO:
    if prmField = "Quote"  then do:
        
        FOR EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.q-no = INT(prmText) NO-LOCK BY quotehd.q-no DESC: 
           IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
          
          FOR  EACH quoteitm WHERE quoteitm.q-no =  quotehd.q-no  NO-LOCK: 
                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no AND eb.company = prmComp and eb.form-no ne 0  NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quotehd.est-no
                ttQuoteUpLook.vCustomer   = quotehd.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quotehd.rfq)
                ttQuoteUpLook.vQuoteno    = quotehd.q-no 
                ttQuoteUpLook.vFgNo       = quoteitm.i-no
                  . 
            
            IF  ttQuoteUpLook.vFgNo = "" THEN ASSIGN ttQuoteUpLook.vFgNo       = eb.stock-no.


              
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN  ASSIGN
                                                ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                               END.
                     END.  /*do transaction*/
           END. /* if avail quotehd*/
 end.  /* if prmField = quote */



if prmField = "Cust"  then do:
    
        if prmCondition = "EQUAL" then do:
             v-count = 0.
        MAIN-LOOP:
        FOR EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.cust-no = prmText NO-LOCK BY quotehd.q-no DESC: 
           IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
          
          FOR  EACH quoteitm WHERE quoteitm.q-no =  quotehd.q-no  NO-LOCK: 
                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no /*AND eb.company = prmComp*/ NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quotehd.est-no
                ttQuoteUpLook.vCustomer   = quotehd.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quotehd.rfq)
                ttQuoteUpLook.vQuoteno    = quotehd.q-no 
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           v-count = v-count + 1.
                                           IF v-count = 50 THEN LEAVE MAIN-LOOP.

                                           END.
                                 END.  /*do transaction*/
                  END. /* if avail quotehd*/
   END. /*if prmCondition = EQUAL */


        IF prmCondition = "BEGIN" then do:
            v-count = 0.
        MAIN-LOOP:
        FOR EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.cust-no BEGINS prmText  NO-LOCK BY quotehd.q-no DESC: 
           IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
          
          FOR  EACH quoteitm WHERE quoteitm.q-no =  quotehd.q-no  NO-LOCK: 
                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no /*AND eb.company = prmComp*/ NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quotehd.est-no
                ttQuoteUpLook.vCustomer   = quotehd.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quotehd.rfq)
                ttQuoteUpLook.vQuoteno    = quotehd.q-no 
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           v-count = v-count + 1.
                                           IF v-count = 50 THEN LEAVE MAIN-LOOP.

                                      END.
                          END.  /*do transaction*/
              END. /* if avail quotehd*/
       /*end.  /*FOR EACH item wher*/*/
    end.    /*if prmCondition = BEGIN*/    
end.  /* if prmField = cust  */



if prmField = "CustPart"  then do:
    
        if prmCondition = "EQUAL" then do:
             v-count = 0.
        MAIN-LOOP:
       

        FOR EACH quoteitm WHERE quoteitm.company =  prmComp AND quoteitm.part-no =  prmText  NO-LOCK,
            FIRST quotehd WHERE quotehd.q-no = quoteitm.q-no AND quotehd.company = prmComp :  

              IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no AND eb.company = prmComp NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quoteitm.est-no
                ttQuoteUpLook.vCustomer   = quoteitm.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quoteitm.rfq)
                ttQuoteUpLook.vQuoteno    = quoteitm.q-no              
                ttQuoteUpLook.vFgNo       = quoteitm.i-no
                  . 
            
                IF  ttQuoteUpLook.vFgNo = "" THEN ASSIGN ttQuoteUpLook.vFgNo       = eb.stock-no.
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = quoteitm.est-no.
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           v-count = v-count + 1.
                                           IF v-count = 50 THEN LEAVE MAIN-LOOP.

                                           END.
                                 END.  /*do transaction*/    
   END. /*if prmCondition = EQUAL */


        IF prmCondition = "BEGIN" then do:
            v-count = 0.
        MAIN-LOOP:

        FOR EACH quoteitm WHERE quoteitm.company =  prmComp AND quoteitm.part-no BEGINS  prmText  NO-LOCK,
            FIRST quotehd WHERE quotehd.q-no = quoteitm.q-no AND quotehd.company = prmComp :  

              IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
                  
           FIND FIRST eb WHERE eb.est-no = quoteitm.est-no AND eb.company = prmComp NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quoteitm.est-no
                ttQuoteUpLook.vCustomer   = quoteitm.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quoteitm.rfq)
                ttQuoteUpLook.vQuoteno    = quoteitm.q-no        
                ttQuoteUpLook.vFgNo       = quoteitm.i-no
                  . 
            
            IF  ttQuoteUpLook.vFgNo = "" THEN ASSIGN ttQuoteUpLook.vFgNo       = eb.stock-no.
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = quoteitm.est-no.
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           v-count = v-count + 1.
                                           IF v-count = 50 THEN LEAVE MAIN-LOOP.

                                      END.
                         /* END.  /*do transaction*/*/
       end.  /*FOR EACH item wher*/
    end.    /*if prmCondition = BEGIN*/    
end.  /* if prmField = CustPart  */




/*
if prmField = "Fgnum"  then do:
    
        if prmCondition = "EQUAL" then do:
             v-count = 0.
        MAIN-LOOP:
       

       /* FOR EACH quoteitm WHERE quoteitm.company =  prmComp AND quoteitm.i-no =  prmText  NO-LOCK,*/
        FOR EACH quoteitm WHERE quoteitm.company =  prmComp NO-LOCK,
            FIRST quotehd WHERE quotehd.q-no = quoteitm.q-no AND quotehd.company = prmComp :

           /* IF NOT AVAIL quoteitm.i-no THEN
                FIND FIRST eb WHERE eb.company =  prmComp AND quoteitm.stock-no =  prmText 
                */

              IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no AND eb.company = prmComp AND eb.stock-no = prmText NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quoteitm.est-no
                ttQuoteUpLook.vCustomer   = quoteitm.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quoteitm.rfq)
                ttQuoteUpLook.vQuoteno    = quoteitm.q-no              
                ttQuoteUpLook.vFgNo       = quoteitm.i-no
                  . 
            
                IF  ttQuoteUpLook.vFgNo = "" THEN ASSIGN ttQuoteUpLook.vFgNo       = eb.stock-no.
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = quoteitm.est-no.
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           v-count = v-count + 1.
                                           IF v-count = 50 THEN LEAVE MAIN-LOOP.

                                           END.
                                 END.  /*do transaction*/    
   END. /*if prmCondition = EQUAL */


        IF prmCondition = "BEGIN" then do:
            v-count = 0.
        MAIN-LOOP:

        /*FOR EACH quoteitm WHERE quoteitm.company =  prmComp AND quoteitm.i-no BEGINS  prmText  NO-LOCK,*/
        FOR EACH quoteitm WHERE quoteitm.company =  prmComp  NO-LOCK,
            FIRST quotehd WHERE quotehd.q-no = quoteitm.q-no AND quotehd.company = prmComp :     

            IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
                  
           FIND FIRST eb WHERE eb.est-no = quoteitm.est-no AND eb.company = prmComp AND eb.stock-no BEGINS  prmText  NO-LOCK NO-ERROR.            

           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quoteitm.est-no
                ttQuoteUpLook.vCustomer   = quoteitm.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quoteitm.rfq)
                ttQuoteUpLook.vQuoteno    = quoteitm.q-no        
                ttQuoteUpLook.vFgNo       = quoteitm.i-no
                  . 
            
            IF  ttQuoteUpLook.vFgNo = "" THEN ASSIGN ttQuoteUpLook.vFgNo       = eb.stock-no.
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = quoteitm.est-no.
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           v-count = v-count + 1.
                                           IF v-count = 50 THEN LEAVE MAIN-LOOP.

                                      END.
                         /* END.  /*do transaction*/*/
       end.  /*FOR EACH item wher*/
    end.    /*if prmCondition = BEGIN*/    
end.  /* if prmField = Fgnum  */

*/




if prmField = "Fgnum"  then do:
    
        if prmCondition = "EQUAL" then do:
             v-count = 0.
          
        MAIN-LOOP:
       

        FOR EACH quoteitm WHERE quoteitm.company =  prmComp AND quoteitm.i-no = prmText  NO-LOCK,
            FIRST quotehd WHERE quotehd.q-no = quoteitm.q-no AND quotehd.company = prmComp NO-LOCK :                                         

              IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.        
                                          

           FIND FIRST eb WHERE eb.est-no = quotehd.est-no AND eb.company = prmComp NO-LOCK NO-ERROR.           
           IF NOT AVAIL eb THEN NEXT.

           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quoteitm.est-no
                ttQuoteUpLook.vCustomer   = quoteitm.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quoteitm.rfq)
                ttQuoteUpLook.vQuoteno    = quoteitm.q-no              
                ttQuoteUpLook.vFgNo       = quoteitm.i-no
                  . 
            
                IF  ttQuoteUpLook.vFgNo = "" THEN ASSIGN ttQuoteUpLook.vFgNo       = eb.stock-no.
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = quoteitm.est-no.
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           v-count = v-count + 1.
                                           IF v-count = 50 THEN LEAVE MAIN-LOOP.

                                           END.
                                 END.  /*do transaction*/    
   END. /*if prmCondition = EQUAL */


        IF prmCondition = "BEGIN" then do:
            v-count = 0.
        MAIN-LOOP:
        
        FOR EACH quoteitm WHERE quoteitm.company =  prmComp AND quoteitm.i-no BEGINS prmText  NO-LOCK,
            FIRST quotehd WHERE quotehd.q-no = quoteitm.q-no AND quotehd.company = prmComp NO-LOCK :                                         

              IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.                                                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no AND eb.company = prmComp NO-LOCK NO-ERROR.           

           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quoteitm.est-no
                ttQuoteUpLook.vCustomer   = quoteitm.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quoteitm.rfq)
                ttQuoteUpLook.vQuoteno    = quoteitm.q-no        
                ttQuoteUpLook.vFgNo       = quoteitm.i-no
                  . 
            
            IF  ttQuoteUpLook.vFgNo = "" THEN ASSIGN ttQuoteUpLook.vFgNo       = eb.stock-no.
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = quoteitm.est-no.
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           v-count = v-count + 1.
                                           IF v-count = 50 THEN LEAVE MAIN-LOOP.

                                      END.
                         /* END.  /*do transaction*/*/
       end.  /*FOR EACH item wher*/
    end.    /*if prmCondition = BEGIN*/    
end.  /* if prmField = Fgnum  */









if prmField = "Est"  then do:
        if prmCondition = "EQUAL" then do:
         FOR EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) NO-LOCK BY quotehd.q-no DESC: 
           IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
          
          FOR  EACH quoteitm WHERE quoteitm.q-no =  quotehd.q-no  NO-LOCK: 
                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no AND eb.company = prmComp NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quotehd.est-no
                ttQuoteUpLook.vCustomer   = quotehd.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quotehd.rfq)
                ttQuoteUpLook.vQuoteno    = quotehd.q-no 
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           
                                     END.
                           END.  /*do transaction*/
                  END. /* if avail quotehd*/
  END. /*if prmCondition = EQUAL */


        IF prmCondition = "BEGIN" then do:
           
        FOR EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.est-no BEGINS FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) NO-LOCK BY quotehd.q-no DESC: 
           IF LOOKUP(quotehd.cust-no, custcount) = 0  THEN NEXT.
          
          FOR  EACH quoteitm WHERE quoteitm.q-no =  quotehd.q-no  NO-LOCK: 
                  
           FIND FIRST eb WHERE eb.est-no = quotehd.est-no /*AND eb.company = prmComp*/ NO-LOCK NO-ERROR. 
           IF NOT AVAIL eb THEN NEXT.
           create ttQuoteUpLook.
            assign                                     
                ttQuoteUpLook.vEstimate   = quotehd.est-no
                ttQuoteUpLook.vCustomer   = quotehd.cust-no 
                ttQuoteUpLook.vshipname   = eb.ship-name
                ttQuoteUpLook.vdscr       = quoteitm.part-dscr1
                ttQuoteUpLook.vpartno     = quoteitm.part-no 
                ttQuoteUpLook.vRfqno      = int(quotehd.rfq)
                ttQuoteUpLook.vQuoteno    = quotehd.q-no 
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
                            ttQuoteUpLook.vprevord      = oe-ord.ord-no
                            ttQuoteUpLook.vOrder      = Ordernum
                            ttQuoteUpLook.vEstimate   = eb.est-no
                            ttQuoteUpLook.vsoldid     = eb.cust-no 
                            ttQuoteUpLook.vsman       = eb.sman 
                            /*ttQuoteUpLook.vCustomer   = eb.cust-no*/
                            ttQuoteUpLook.vCarrier    = eb.carrier 
                            ttQuoteUpLook.vfreight    = eb.chg-method
                            ttQuoteUpLook.vSpct       = 100          
                            ttQuoteUpLook.vDueCode    = "ON"          
                            ttQuoteUpLook.vJobNo      = v-job-no         
                            ttQuoteUpLook.vJobNo2     = v-job-no2         
                            ttQuoteUpLook.vCName      = cust.NAME          
                            ttQuoteUpLook.vCAddr      = cust.addr[1]          
                            ttQuoteUpLook.vCAddr2     = cust.addr[2]            
                            ttQuoteUpLook.vCCity      = cust.city          
                            ttQuoteUpLook.vCState     = cust.state         
                            ttQuoteUpLook.vCZip       = cust.zip           
                            ttQuoteUpLook.vContact    = cust.contact          
                            ttQuoteUpLook.vLstDate    = (today + cust.ship-days)          
                            ttQuoteUpLook.vDueDate    = (today + cust.ship-days)       
                            ttQuoteUpLook.vTerms      = cust.terms                        
                            ttQuoteUpLook.vOverPct    = cust.over-pct           
                            ttQuoteUpLook.vUnderPct   = cust.under-pct            
                            ttQuoteUpLook.vFobCode    = cust.fob-code                     
                            ttQuoteUpLook.vTaxGr      = cust.tax-gr                          
                            ttQuoteUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttQuoteUpLook.vsoldname = soldto.sold-name 
                            ttQuoteUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttQuoteUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttQuoteUpLook.vsoldcity = soldto.sold-city
                            ttQuoteUpLook.vsoldstat = soldto.sold-state
                            ttQuoteUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttQuoteUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttQuoteUpLook.vComm = eb.comm.
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
                           ttQuoteUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttQuoteUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttQuoteUpLook.vTDscr  = terms.dscr.
                       else ttQuoteUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttQuoteUpLook.vCTyp = "T".
                            if ttQuoteUpLook.vCarrier eq "" then ttQuoteUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttQuoteUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttQuoteUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttQuoteUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.
                                           
                                      END.
                            END.  /*do transaction*/
                  END. /* if avail quotehd*/
            end.    /*if prmCondition = BEGIN*/    
     end.  /* if end of field  */
END. /* end of search */
