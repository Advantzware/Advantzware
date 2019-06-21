


/*------------------------------------------------------------------------
    File         : EstimateUpLook.p
    Purpose     :  estimate lookup

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttEstimateUpLook NO-UNDO
    
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
    FIELD vStyle     AS CHARACTER
    FIELD vLength     AS DECIMAL
    FIELD vWidth     AS DECIMAL
    FIELD vDepth     AS DECIMAL
    FIELD vQty     AS DECIMAL    
    FIELD vmkl         AS INTEGER
      .
DEFINE DATASET dsEstimateUpLookup FOR ttEstimateUpLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHAR NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEstimateUpLookup.

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
           FOR EACH eb WHERE eb.company = prmComp NO-LOCK BY eb.est-no DESC:       

           IF LOOKUP(eb.cust-no, custcount) = 0  THEN NEXT.

           create ttEstimateUpLook.
            assign                                                  
                ttEstimateUpLook.vEstimate   = eb.est-no
                ttEstimateUpLook.vCustomer   = eb.cust-no 
                ttEstimateUpLook.vshipname   = eb.ship-name
                ttEstimateUpLook.vdscr       = eb.part-dscr1
                ttEstimateUpLook.vpartno     = eb.part-no              
                ttEstimateUpLook.vFgNo       = eb.stock-no

                ttEstimateUpLook.vStyle      = eb.style
                ttEstimateUpLook.vLength     = eb.len
                ttEstimateUpLook.vWidth      = eb.wid
                ttEstimateUpLook.vDepth      = eb.dep
                ttEstimateUpLook.vQty        = eb.eqty
                . 

           /* IF  ttEstimateUpLook.vFgNo = "" THEN ASSIGN ttEstimateUpLook.vFgNo       = eb.stock-no.*/
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = eb.est-no.
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
                            ttEstimateUpLook.vprevord    = oe-ord.ord-no
                            ttEstimateUpLook.vOrder      = Ordernum
                            ttEstimateUpLook.vEstimate   = eb.est-no
                            ttEstimateUpLook.vsoldid     = eb.cust-no 
                            ttEstimateUpLook.vsman       = eb.sman 
                            /*ttEstimateUpLook.vCustomer   = eb.cust-no*/
                            ttEstimateUpLook.vCarrier    = eb.carrier 
                            ttEstimateUpLook.vfreight    = eb.chg-method
                            ttEstimateUpLook.vSpct       = 100          
                            ttEstimateUpLook.vDueCode    = "ON"          
                            ttEstimateUpLook.vJobNo      = v-job-no         
                            ttEstimateUpLook.vJobNo2     = v-job-no2         
                            ttEstimateUpLook.vCName      = cust.NAME          
                            ttEstimateUpLook.vCAddr      = cust.addr[1]          
                            ttEstimateUpLook.vCAddr2     = cust.addr[2]            
                            ttEstimateUpLook.vCCity      = cust.city          
                            ttEstimateUpLook.vCState     = cust.state         
                            ttEstimateUpLook.vCZip       = cust.zip           
                            ttEstimateUpLook.vContact    = cust.contact          
                            ttEstimateUpLook.vLstDate    = (today + cust.ship-days)          
                            ttEstimateUpLook.vDueDate    = (today + cust.ship-days)       
                            ttEstimateUpLook.vTerms      = cust.terms                        
                            ttEstimateUpLook.vOverPct    = cust.over-pct           
                            ttEstimateUpLook.vUnderPct   = cust.under-pct            
                            ttEstimateUpLook.vFobCode    = cust.fob-code                     
                            ttEstimateUpLook.vTaxGr      = cust.tax-gr                          
                            ttEstimateUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttEstimateUpLook.vsoldname = soldto.sold-name 
                            ttEstimateUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttEstimateUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttEstimateUpLook.vsoldcity = soldto.sold-city
                            ttEstimateUpLook.vsoldstat = soldto.sold-state
                            ttEstimateUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN
                                     ASSIGN
                                        ttEstimateUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttEstimateUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttEstimateUpLook.vComm = eb.comm.
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
                           ttEstimateUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttEstimateUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttEstimateUpLook.vTDscr  = terms.dscr.
                       else ttEstimateUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttEstimateUpLook.vCTyp = "T".
                            if ttEstimateUpLook.vCarrier eq "" then ttEstimateUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = eb.est-no  no-lock no-error.
                            ASSIGN
                               ttEstimateUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttEstimateUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttEstimateUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.      
                            
                            FIND FIRST quotehd WHERE quotehd.est-no = eb.est-no AND quotehd.company = prmComp NO-LOCK NO-ERROR.                            
                            FIND FIRST quoteitm WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
                            IF AVAIL quotehd AND AVAIL quoteitm THEN DO:             
                                ttEstimateUpLook.vQuoteno  = quoteitm.q-no .
                            END.                            
               END.
            END.   /*do transaction*/  
               
    
END.  /* end of seach */

IF prmAction = "search"  THEN DO:
    if prmField = "Estimate"  then do:

        if prmCondition = "EQUAL" then do:      
                               
           FOR EACH eb WHERE eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) AND eb.company = prmComp NO-LOCK BY eb.est-no DESC :
           IF LOOKUP(eb.cust-no, custcount) = 0  THEN NEXT.
           create ttEstimateUpLook.
            assign                                     
                ttEstimateUpLook.vEstimate   = eb.est-no
                ttEstimateUpLook.vCustomer   = eb.cust-no 
                ttEstimateUpLook.vshipname   = eb.ship-name
                ttEstimateUpLook.vdscr       = eb.part-dscr1
                ttEstimateUpLook.vpartno     = eb.part-no                
                ttEstimateUpLook.vFgNo       = eb.stock-no

                ttEstimateUpLook.vStyle      = eb.style
                ttEstimateUpLook.vLength     = eb.len
                ttEstimateUpLook.vWidth      = eb.wid
                ttEstimateUpLook.vDepth      = eb.dep
                ttEstimateUpLook.vQty        = eb.eqty             
                . 
                        


              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = eb.est-no.
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
                            ttEstimateUpLook.vprevord      = oe-ord.ord-no
                            ttEstimateUpLook.vOrder      = Ordernum
                            ttEstimateUpLook.vEstimate   = eb.est-no
                            ttEstimateUpLook.vsoldid     = eb.cust-no 
                            ttEstimateUpLook.vsman       = eb.sman 
                            /*ttEstimateUpLook.vCustomer   = eb.cust-no*/
                            ttEstimateUpLook.vCarrier    = eb.carrier 
                            ttEstimateUpLook.vfreight    = eb.chg-method
                            ttEstimateUpLook.vSpct       = 100          
                            ttEstimateUpLook.vDueCode    = "ON"          
                            ttEstimateUpLook.vJobNo      = v-job-no         
                            ttEstimateUpLook.vJobNo2     = v-job-no2         
                            ttEstimateUpLook.vCName      = cust.NAME          
                            ttEstimateUpLook.vCAddr      = cust.addr[1]          
                            ttEstimateUpLook.vCAddr2     = cust.addr[2]            
                            ttEstimateUpLook.vCCity      = cust.city          
                            ttEstimateUpLook.vCState     = cust.state         
                            ttEstimateUpLook.vCZip       = cust.zip           
                            ttEstimateUpLook.vContact    = cust.contact          
                            ttEstimateUpLook.vLstDate    = (today + cust.ship-days)          
                            ttEstimateUpLook.vDueDate    = (today + cust.ship-days)       
                            ttEstimateUpLook.vTerms      = cust.terms                        
                            ttEstimateUpLook.vOverPct    = cust.over-pct           
                            ttEstimateUpLook.vUnderPct   = cust.under-pct            
                            ttEstimateUpLook.vFobCode    = cust.fob-code                     
                            ttEstimateUpLook.vTaxGr      = cust.tax-gr                          
                            ttEstimateUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttEstimateUpLook.vsoldname = soldto.sold-name 
                            ttEstimateUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttEstimateUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttEstimateUpLook.vsoldcity = soldto.sold-city
                            ttEstimateUpLook.vsoldstat = soldto.sold-state
                            ttEstimateUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN  ASSIGN
                                                ttEstimateUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttEstimateUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttEstimateUpLook.vComm = eb.comm.
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
                           ttEstimateUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttEstimateUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttEstimateUpLook.vTDscr  = terms.dscr.
                       else ttEstimateUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttEstimateUpLook.vCTyp = "T".
                            if ttEstimateUpLook.vCarrier eq "" then ttEstimateUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no) no-lock no-error.
                            ASSIGN
                               ttEstimateUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttEstimateUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttEstimateUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.

                                          FIND FIRST quotehd WHERE quotehd.est-no = eb.est-no AND quotehd.company = prmComp NO-LOCK NO-ERROR.                            
                                          FIND FIRST quoteitm WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
                                          IF AVAIL quotehd AND AVAIL quoteitm THEN DO:             
                                            ttEstimateUpLook.vQuoteno  = quoteitm.q-no .
                                          END.

                               END.
                     END.  /*do transaction*/
             END. 

          IF prmCondition = "BEGIN" then DO:
             FOR EACH eb WHERE eb.est-no BEGINS FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) AND eb.company = prmComp NO-LOCK BY eb.est-no DESC :
           IF LOOKUP(eb.cust-no, custcount) = 0  THEN NEXT.
           create ttEstimateUpLook.
            assign                                     
                ttEstimateUpLook.vEstimate   = eb.est-no
                ttEstimateUpLook.vCustomer   = eb.cust-no 
                ttEstimateUpLook.vshipname   = eb.ship-name
                ttEstimateUpLook.vdscr       = eb.part-dscr1
                ttEstimateUpLook.vpartno     = eb.part-no                
                ttEstimateUpLook.vFgNo       = eb.stock-no

                ttEstimateUpLook.vStyle      = eb.style
                ttEstimateUpLook.vLength     = eb.len
                ttEstimateUpLook.vWidth      = eb.wid
                ttEstimateUpLook.vDepth      = eb.dep
                ttEstimateUpLook.vQty        = eb.eqty
                . 
                        
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = eb.est-no.
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
                            ttEstimateUpLook.vprevord      = oe-ord.ord-no
                            ttEstimateUpLook.vOrder      = Ordernum
                            ttEstimateUpLook.vEstimate   = eb.est-no
                            ttEstimateUpLook.vsoldid     = eb.cust-no 
                            ttEstimateUpLook.vsman       = eb.sman 
                            /*ttEstimateUpLook.vCustomer   = eb.cust-no*/
                            ttEstimateUpLook.vCarrier    = eb.carrier 
                            ttEstimateUpLook.vfreight    = eb.chg-method
                            ttEstimateUpLook.vSpct       = 100          
                            ttEstimateUpLook.vDueCode    = "ON"          
                            ttEstimateUpLook.vJobNo      = v-job-no         
                            ttEstimateUpLook.vJobNo2     = v-job-no2         
                            ttEstimateUpLook.vCName      = cust.NAME          
                            ttEstimateUpLook.vCAddr      = cust.addr[1]          
                            ttEstimateUpLook.vCAddr2     = cust.addr[2]            
                            ttEstimateUpLook.vCCity      = cust.city          
                            ttEstimateUpLook.vCState     = cust.state         
                            ttEstimateUpLook.vCZip       = cust.zip           
                            ttEstimateUpLook.vContact    = cust.contact          
                            ttEstimateUpLook.vLstDate    = (today + cust.ship-days)          
                            ttEstimateUpLook.vDueDate    = (today + cust.ship-days)       
                            ttEstimateUpLook.vTerms      = cust.terms                        
                            ttEstimateUpLook.vOverPct    = cust.over-pct           
                            ttEstimateUpLook.vUnderPct   = cust.under-pct            
                            ttEstimateUpLook.vFobCode    = cust.fob-code                     
                            ttEstimateUpLook.vTaxGr      = cust.tax-gr                          
                            ttEstimateUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttEstimateUpLook.vsoldname = soldto.sold-name 
                            ttEstimateUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttEstimateUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttEstimateUpLook.vsoldcity = soldto.sold-city
                            ttEstimateUpLook.vsoldstat = soldto.sold-state
                            ttEstimateUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN  ASSIGN
                                                ttEstimateUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttEstimateUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttEstimateUpLook.vComm = eb.comm.
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
                           ttEstimateUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttEstimateUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttEstimateUpLook.vTDscr  = terms.dscr.
                       else ttEstimateUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttEstimateUpLook.vCTyp = "T".
                            if ttEstimateUpLook.vCarrier eq "" then ttEstimateUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no =  FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no) no-lock no-error.
                            ASSIGN
                               ttEstimateUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttEstimateUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttEstimateUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.

                                          FIND FIRST quotehd WHERE quotehd.est-no = eb.est-no AND quotehd.company = prmComp NO-LOCK NO-ERROR.                            
                                          FIND FIRST quoteitm WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
                                          IF AVAIL quotehd AND AVAIL quoteitm THEN DO:             
                                            ttEstimateUpLook.vQuoteno  = quoteitm.q-no .
                                          END.
                               END.
                     END.  /*do transaction*/
             END.
 end.  /* if prmField = Estimate */


if prmField = "Customer"  then do:

        if prmCondition = "EQUAL" then do:            
                               
           FOR EACH eb WHERE eb.cust-no = prmText AND eb.company = prmComp  NO-LOCK :
           IF LOOKUP(eb.cust-no, custcount) = 0  THEN NEXT.
           create ttEstimateUpLook.
            assign                                     
                ttEstimateUpLook.vEstimate   = eb.est-no
                ttEstimateUpLook.vCustomer   = eb.cust-no 
                ttEstimateUpLook.vshipname   = eb.ship-name
                ttEstimateUpLook.vdscr       = eb.part-dscr1
                ttEstimateUpLook.vpartno     = eb.part-no                
                ttEstimateUpLook.vFgNo       = eb.stock-no

                ttEstimateUpLook.vStyle      = eb.style
                ttEstimateUpLook.vLength     = eb.len
                ttEstimateUpLook.vWidth      = eb.wid
                ttEstimateUpLook.vDepth      = eb.dep
                ttEstimateUpLook.vQty        = eb.eqty
                . 
                        


              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = eb.est-no.
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
                            ttEstimateUpLook.vprevord      = oe-ord.ord-no
                            ttEstimateUpLook.vOrder      = Ordernum
                            ttEstimateUpLook.vEstimate   = eb.est-no
                            ttEstimateUpLook.vsoldid     = eb.cust-no 
                            ttEstimateUpLook.vsman       = eb.sman 
                            /*ttEstimateUpLook.vCustomer   = eb.cust-no*/
                            ttEstimateUpLook.vCarrier    = eb.carrier 
                            ttEstimateUpLook.vfreight    = eb.chg-method
                            ttEstimateUpLook.vSpct       = 100          
                            ttEstimateUpLook.vDueCode    = "ON"          
                            ttEstimateUpLook.vJobNo      = v-job-no         
                            ttEstimateUpLook.vJobNo2     = v-job-no2         
                            ttEstimateUpLook.vCName      = cust.NAME          
                            ttEstimateUpLook.vCAddr      = cust.addr[1]          
                            ttEstimateUpLook.vCAddr2     = cust.addr[2]            
                            ttEstimateUpLook.vCCity      = cust.city          
                            ttEstimateUpLook.vCState     = cust.state         
                            ttEstimateUpLook.vCZip       = cust.zip           
                            ttEstimateUpLook.vContact    = cust.contact          
                            ttEstimateUpLook.vLstDate    = (today + cust.ship-days)          
                            ttEstimateUpLook.vDueDate    = (today + cust.ship-days)       
                            ttEstimateUpLook.vTerms      = cust.terms                        
                            ttEstimateUpLook.vOverPct    = cust.over-pct           
                            ttEstimateUpLook.vUnderPct   = cust.under-pct            
                            ttEstimateUpLook.vFobCode    = cust.fob-code                     
                            ttEstimateUpLook.vTaxGr      = cust.tax-gr                          
                            ttEstimateUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttEstimateUpLook.vsoldname = soldto.sold-name 
                            ttEstimateUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttEstimateUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttEstimateUpLook.vsoldcity = soldto.sold-city
                            ttEstimateUpLook.vsoldstat = soldto.sold-state
                            ttEstimateUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN  ASSIGN
                                                ttEstimateUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttEstimateUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttEstimateUpLook.vComm = eb.comm.
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
                           ttEstimateUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttEstimateUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttEstimateUpLook.vTDscr  = terms.dscr.
                       else ttEstimateUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttEstimateUpLook.vCTyp = "T".
                            if ttEstimateUpLook.vCarrier eq "" then ttEstimateUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no)  no-lock no-error.
                            ASSIGN
                               ttEstimateUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttEstimateUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttEstimateUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.

                                          FIND FIRST quotehd WHERE quotehd.est-no = eb.est-no AND quotehd.company = prmComp NO-LOCK NO-ERROR.                            
                                          FIND FIRST quoteitm WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
                                          IF AVAIL quotehd AND AVAIL quoteitm THEN DO:             
                                            ttEstimateUpLook.vQuoteno  = quoteitm.q-no .
                                          END.
                               END.
                     END.  /*do transaction*/
             END. 

             IF prmCondition = "BEGIN" then DO:
                FOR EACH eb WHERE eb.cust-no BEGINS prmText AND eb.company = prmComp NO-LOCK :
           IF LOOKUP(eb.cust-no, custcount) = 0  THEN NEXT.
           create ttEstimateUpLook.
            assign                                     
                ttEstimateUpLook.vEstimate   = eb.est-no
                ttEstimateUpLook.vCustomer   = eb.cust-no 
                ttEstimateUpLook.vshipname   = eb.ship-name
                ttEstimateUpLook.vdscr       = eb.part-dscr1
                ttEstimateUpLook.vpartno     = eb.part-no                
                ttEstimateUpLook.vFgNo       = eb.stock-no

                ttEstimateUpLook.vStyle      = eb.style
                ttEstimateUpLook.vLength     = eb.len
                ttEstimateUpLook.vWidth      = eb.wid
                ttEstimateUpLook.vDepth      = eb.dep
                ttEstimateUpLook.vQty        = eb.eqty
                . 
                        
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = eb.est-no.
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
                            ttEstimateUpLook.vprevord      = oe-ord.ord-no
                            ttEstimateUpLook.vOrder      = Ordernum
                            ttEstimateUpLook.vEstimate   = eb.est-no
                            ttEstimateUpLook.vsoldid     = eb.cust-no 
                            ttEstimateUpLook.vsman       = eb.sman 
                            /*ttEstimateUpLook.vCustomer   = eb.cust-no*/
                            ttEstimateUpLook.vCarrier    = eb.carrier 
                            ttEstimateUpLook.vfreight    = eb.chg-method
                            ttEstimateUpLook.vSpct       = 100          
                            ttEstimateUpLook.vDueCode    = "ON"          
                            ttEstimateUpLook.vJobNo      = v-job-no         
                            ttEstimateUpLook.vJobNo2     = v-job-no2         
                            ttEstimateUpLook.vCName      = cust.NAME          
                            ttEstimateUpLook.vCAddr      = cust.addr[1]          
                            ttEstimateUpLook.vCAddr2     = cust.addr[2]            
                            ttEstimateUpLook.vCCity      = cust.city          
                            ttEstimateUpLook.vCState     = cust.state         
                            ttEstimateUpLook.vCZip       = cust.zip           
                            ttEstimateUpLook.vContact    = cust.contact          
                            ttEstimateUpLook.vLstDate    = (today + cust.ship-days)          
                            ttEstimateUpLook.vDueDate    = (today + cust.ship-days)       
                            ttEstimateUpLook.vTerms      = cust.terms                        
                            ttEstimateUpLook.vOverPct    = cust.over-pct           
                            ttEstimateUpLook.vUnderPct   = cust.under-pct            
                            ttEstimateUpLook.vFobCode    = cust.fob-code                     
                            ttEstimateUpLook.vTaxGr      = cust.tax-gr                          
                            ttEstimateUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttEstimateUpLook.vsoldname = soldto.sold-name 
                            ttEstimateUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttEstimateUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttEstimateUpLook.vsoldcity = soldto.sold-city
                            ttEstimateUpLook.vsoldstat = soldto.sold-state
                            ttEstimateUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN  ASSIGN
                                                ttEstimateUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttEstimateUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttEstimateUpLook.vComm = eb.comm.
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
                           ttEstimateUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttEstimateUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttEstimateUpLook.vTDscr  = terms.dscr.
                       else ttEstimateUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttEstimateUpLook.vCTyp = "T".
                            if ttEstimateUpLook.vCarrier eq "" then ttEstimateUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no =  FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no)  no-lock no-error.
                            ASSIGN
                               ttEstimateUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttEstimateUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttEstimateUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.

                                          FIND FIRST quotehd WHERE quotehd.est-no = eb.est-no AND quotehd.company = prmComp NO-LOCK NO-ERROR.                            
                                          FIND FIRST quoteitm WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
                                          IF AVAIL quotehd AND AVAIL quoteitm THEN DO:             
                                            ttEstimateUpLook.vQuoteno  = quoteitm.q-no .
                                          END.
                               END.
                     END.  /*do transaction*/
             END.
 end.  /* if prmField = Customer */



if prmField = "CustomerName"  then do:

        if prmCondition = "EQUAL" then do:            
                               
           FOR EACH eb WHERE eb.company = prmComp  NO-LOCK :
           IF LOOKUP(eb.cust-no, custcount) = 0  THEN NEXT.

           FIND FIRST cust WHERE cust.company = prmComp AND eb.cust-no = cust.cust-no AND cust.NAME = prmText NO-LOCK NO-ERROR.
           IF NOT AVAIL cust THEN NEXT. 

           create ttEstimateUpLook.
            assign                                     
                ttEstimateUpLook.vEstimate   = eb.est-no
                ttEstimateUpLook.vCustomer   = eb.cust-no 
                ttEstimateUpLook.vshipname   = eb.ship-name
                ttEstimateUpLook.vdscr       = eb.part-dscr1
                ttEstimateUpLook.vpartno     = eb.part-no                
                ttEstimateUpLook.vFgNo       = eb.stock-no

                ttEstimateUpLook.vStyle      = eb.style
                ttEstimateUpLook.vLength     = eb.len
                ttEstimateUpLook.vWidth      = eb.wid
                ttEstimateUpLook.vDepth      = eb.dep
                ttEstimateUpLook.vQty        = eb.eqty
                . 
                        


              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = eb.est-no.
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
                            ttEstimateUpLook.vprevord      = oe-ord.ord-no
                            ttEstimateUpLook.vOrder      = Ordernum
                            ttEstimateUpLook.vEstimate   = eb.est-no
                            ttEstimateUpLook.vsoldid     = eb.cust-no 
                            ttEstimateUpLook.vsman       = eb.sman 
                            /*ttEstimateUpLook.vCustomer   = eb.cust-no*/
                            ttEstimateUpLook.vCarrier    = eb.carrier 
                            ttEstimateUpLook.vfreight    = eb.chg-method
                            ttEstimateUpLook.vSpct       = 100          
                            ttEstimateUpLook.vDueCode    = "ON"          
                            ttEstimateUpLook.vJobNo      = v-job-no         
                            ttEstimateUpLook.vJobNo2     = v-job-no2         
                            ttEstimateUpLook.vCName      = cust.NAME          
                            ttEstimateUpLook.vCAddr      = cust.addr[1]          
                            ttEstimateUpLook.vCAddr2     = cust.addr[2]            
                            ttEstimateUpLook.vCCity      = cust.city          
                            ttEstimateUpLook.vCState     = cust.state         
                            ttEstimateUpLook.vCZip       = cust.zip           
                            ttEstimateUpLook.vContact    = cust.contact          
                            ttEstimateUpLook.vLstDate    = (today + cust.ship-days)          
                            ttEstimateUpLook.vDueDate    = (today + cust.ship-days)       
                            ttEstimateUpLook.vTerms      = cust.terms                        
                            ttEstimateUpLook.vOverPct    = cust.over-pct           
                            ttEstimateUpLook.vUnderPct   = cust.under-pct            
                            ttEstimateUpLook.vFobCode    = cust.fob-code                     
                            ttEstimateUpLook.vTaxGr      = cust.tax-gr                          
                            ttEstimateUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttEstimateUpLook.vsoldname = soldto.sold-name 
                            ttEstimateUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttEstimateUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttEstimateUpLook.vsoldcity = soldto.sold-city
                            ttEstimateUpLook.vsoldstat = soldto.sold-state
                            ttEstimateUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN  ASSIGN
                                                ttEstimateUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttEstimateUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttEstimateUpLook.vComm = eb.comm.
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
                           ttEstimateUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttEstimateUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttEstimateUpLook.vTDscr  = terms.dscr.
                       else ttEstimateUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttEstimateUpLook.vCTyp = "T".
                            if ttEstimateUpLook.vCarrier eq "" then ttEstimateUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no = FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no) no-lock no-error.
                            ASSIGN
                               ttEstimateUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttEstimateUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttEstimateUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.

                                          FIND FIRST quotehd WHERE quotehd.est-no = eb.est-no AND quotehd.company = prmComp NO-LOCK NO-ERROR.                            
                                          FIND FIRST quoteitm WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
                                          IF AVAIL quotehd AND AVAIL quoteitm THEN DO:             
                                            ttEstimateUpLook.vQuoteno  = quoteitm.q-no .
                                          END.
                               END.
                     END.  /*do transaction*/
             END. 

             IF prmCondition = "BEGIN" then DO:
                FOR EACH eb WHERE eb.company = prmComp NO-LOCK :
           IF LOOKUP(eb.cust-no, custcount) = 0  THEN NEXT.
           
           FIND FIRST cust WHERE cust.company = prmComp AND eb.cust-no = cust.cust-no AND cust.NAME BEGINS prmText NO-LOCK NO-ERROR.
           IF NOT AVAIL cust THEN NEXT. 

           create ttEstimateUpLook.
            assign                                     
                ttEstimateUpLook.vEstimate   = eb.est-no
                ttEstimateUpLook.vCustomer   = eb.cust-no 
                ttEstimateUpLook.vshipname   = eb.ship-name
                ttEstimateUpLook.vdscr       = eb.part-dscr1
                ttEstimateUpLook.vpartno     = eb.part-no                
                ttEstimateUpLook.vFgNo       = eb.stock-no

                ttEstimateUpLook.vStyle      = eb.style
                ttEstimateUpLook.vLength     = eb.len
                ttEstimateUpLook.vWidth      = eb.wid
                ttEstimateUpLook.vDepth      = eb.dep
                ttEstimateUpLook.vQty        = eb.eqty
                . 
                        
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = eb.est-no.
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
                            ttEstimateUpLook.vprevord      = oe-ord.ord-no
                            ttEstimateUpLook.vOrder      = Ordernum
                            ttEstimateUpLook.vEstimate   = eb.est-no
                            ttEstimateUpLook.vsoldid     = eb.cust-no 
                            ttEstimateUpLook.vsman       = eb.sman 
                            /*ttEstimateUpLook.vCustomer   = eb.cust-no*/
                            ttEstimateUpLook.vCarrier    = eb.carrier 
                            ttEstimateUpLook.vfreight    = eb.chg-method
                            ttEstimateUpLook.vSpct       = 100          
                            ttEstimateUpLook.vDueCode    = "ON"          
                            ttEstimateUpLook.vJobNo      = v-job-no         
                            ttEstimateUpLook.vJobNo2     = v-job-no2         
                            ttEstimateUpLook.vCName      = cust.NAME          
                            ttEstimateUpLook.vCAddr      = cust.addr[1]          
                            ttEstimateUpLook.vCAddr2     = cust.addr[2]            
                            ttEstimateUpLook.vCCity      = cust.city          
                            ttEstimateUpLook.vCState     = cust.state         
                            ttEstimateUpLook.vCZip       = cust.zip           
                            ttEstimateUpLook.vContact    = cust.contact          
                            ttEstimateUpLook.vLstDate    = (today + cust.ship-days)          
                            ttEstimateUpLook.vDueDate    = (today + cust.ship-days)       
                            ttEstimateUpLook.vTerms      = cust.terms                        
                            ttEstimateUpLook.vOverPct    = cust.over-pct           
                            ttEstimateUpLook.vUnderPct   = cust.under-pct            
                            ttEstimateUpLook.vFobCode    = cust.fob-code                     
                            ttEstimateUpLook.vTaxGr      = cust.tax-gr                          
                            ttEstimateUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttEstimateUpLook.vsoldname = soldto.sold-name 
                            ttEstimateUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttEstimateUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttEstimateUpLook.vsoldcity = soldto.sold-city
                            ttEstimateUpLook.vsoldstat = soldto.sold-state
                            ttEstimateUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN  ASSIGN
                                                ttEstimateUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttEstimateUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttEstimateUpLook.vComm = eb.comm.
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
                           ttEstimateUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttEstimateUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttEstimateUpLook.vTDscr  = terms.dscr.
                       else ttEstimateUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttEstimateUpLook.vCTyp = "T".
                            if ttEstimateUpLook.vCarrier eq "" then ttEstimateUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no =  FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no)  no-lock no-error.
                            ASSIGN
                               ttEstimateUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttEstimateUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttEstimateUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.

                                          FIND FIRST quotehd WHERE quotehd.est-no = eb.est-no AND quotehd.company = prmComp NO-LOCK NO-ERROR.                            
                                          FIND FIRST quoteitm WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
                                          IF AVAIL quotehd AND AVAIL quoteitm THEN DO:             
                                            ttEstimateUpLook.vQuoteno  = quoteitm.q-no .
                                          END.
                               END.
                     END.  /*do transaction*/
             END.
 end.  /* if prmField = CustomerName*/




if prmField = "CustomerPart"  then do:

        if prmCondition = "EQUAL" then do:            
                               
           FOR EACH eb WHERE eb.part-no = prmText AND eb.company = prmComp  NO-LOCK :
           IF LOOKUP(eb.cust-no, custcount) = 0  THEN NEXT.
           create ttEstimateUpLook.
            assign                                     
                ttEstimateUpLook.vEstimate   = eb.est-no
                ttEstimateUpLook.vCustomer   = eb.cust-no 
                ttEstimateUpLook.vshipname   = eb.ship-name
                ttEstimateUpLook.vdscr       = eb.part-dscr1
                ttEstimateUpLook.vpartno     = eb.part-no                
                ttEstimateUpLook.vFgNo       = eb.stock-no

                ttEstimateUpLook.vStyle      = eb.style
                ttEstimateUpLook.vLength     = eb.len
                ttEstimateUpLook.vWidth      = eb.wid
                ttEstimateUpLook.vDepth      = eb.dep
                ttEstimateUpLook.vQty        = eb.eqty
                . 
                        


              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = eb.est-no.
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
                            ttEstimateUpLook.vprevord      = oe-ord.ord-no
                            ttEstimateUpLook.vOrder      = Ordernum
                            ttEstimateUpLook.vEstimate   = eb.est-no
                            ttEstimateUpLook.vsoldid     = eb.cust-no 
                            ttEstimateUpLook.vsman       = eb.sman 
                            /*ttEstimateUpLook.vCustomer   = eb.cust-no*/
                            ttEstimateUpLook.vCarrier    = eb.carrier 
                            ttEstimateUpLook.vfreight    = eb.chg-method
                            ttEstimateUpLook.vSpct       = 100          
                            ttEstimateUpLook.vDueCode    = "ON"          
                            ttEstimateUpLook.vJobNo      = v-job-no         
                            ttEstimateUpLook.vJobNo2     = v-job-no2         
                            ttEstimateUpLook.vCName      = cust.NAME          
                            ttEstimateUpLook.vCAddr      = cust.addr[1]          
                            ttEstimateUpLook.vCAddr2     = cust.addr[2]            
                            ttEstimateUpLook.vCCity      = cust.city          
                            ttEstimateUpLook.vCState     = cust.state         
                            ttEstimateUpLook.vCZip       = cust.zip           
                            ttEstimateUpLook.vContact    = cust.contact          
                            ttEstimateUpLook.vLstDate    = (today + cust.ship-days)          
                            ttEstimateUpLook.vDueDate    = (today + cust.ship-days)       
                            ttEstimateUpLook.vTerms      = cust.terms                        
                            ttEstimateUpLook.vOverPct    = cust.over-pct           
                            ttEstimateUpLook.vUnderPct   = cust.under-pct            
                            ttEstimateUpLook.vFobCode    = cust.fob-code                     
                            ttEstimateUpLook.vTaxGr      = cust.tax-gr                          
                            ttEstimateUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttEstimateUpLook.vsoldname = soldto.sold-name 
                            ttEstimateUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttEstimateUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttEstimateUpLook.vsoldcity = soldto.sold-city
                            ttEstimateUpLook.vsoldstat = soldto.sold-state
                            ttEstimateUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN  ASSIGN
                                                ttEstimateUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttEstimateUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttEstimateUpLook.vComm = eb.comm.
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
                           ttEstimateUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttEstimateUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttEstimateUpLook.vTDscr  = terms.dscr.
                       else ttEstimateUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttEstimateUpLook.vCTyp = "T".
                            if ttEstimateUpLook.vCarrier eq "" then ttEstimateUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no =  FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no)  no-lock no-error.
                            ASSIGN
                               ttEstimateUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttEstimateUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttEstimateUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.

                                          FIND FIRST quotehd WHERE quotehd.est-no = eb.est-no AND quotehd.company = prmComp NO-LOCK NO-ERROR.                            
                                          FIND FIRST quoteitm WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
                                          IF AVAIL quotehd AND AVAIL quoteitm THEN DO:             
                                            ttEstimateUpLook.vQuoteno  = quoteitm.q-no .
                                          END.
                               END.
                     END.  /*do transaction*/
             END. 

             IF prmCondition = "BEGIN" then DO:
                FOR EACH eb WHERE eb.part-no BEGINS prmText AND eb.company = prmComp NO-LOCK :
           IF LOOKUP(eb.cust-no, custcount) = 0  THEN NEXT.
           create ttEstimateUpLook.
            assign                                     
                ttEstimateUpLook.vEstimate   = eb.est-no
                ttEstimateUpLook.vCustomer   = eb.cust-no 
                ttEstimateUpLook.vshipname   = eb.ship-name
                ttEstimateUpLook.vdscr       = eb.part-dscr1
                ttEstimateUpLook.vpartno     = eb.part-no                
                ttEstimateUpLook.vFgNo       = eb.stock-no

                ttEstimateUpLook.vStyle      = eb.style
                ttEstimateUpLook.vLength     = eb.len
                ttEstimateUpLook.vWidth      = eb.wid
                ttEstimateUpLook.vDepth      = eb.dep
                ttEstimateUpLook.vQty        = eb.eqty
                . 
                        
              
           FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
               ASSIGN    Ordernum = oe-ord.ord-no + 1.
               END.  /*IF AVAIL oe-ord THEN DO:*/
               ELSE DO:
                   ASSIGN  Ordernum = 1.
               END.
               
               DO TRANSACTION:
                   v-est-no = eb.est-no.
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
                            ttEstimateUpLook.vprevord      = oe-ord.ord-no
                            ttEstimateUpLook.vOrder      = Ordernum
                            ttEstimateUpLook.vEstimate   = eb.est-no
                            ttEstimateUpLook.vsoldid     = eb.cust-no 
                            ttEstimateUpLook.vsman       = eb.sman 
                            /*ttEstimateUpLook.vCustomer   = eb.cust-no*/
                            ttEstimateUpLook.vCarrier    = eb.carrier 
                            ttEstimateUpLook.vfreight    = eb.chg-method
                            ttEstimateUpLook.vSpct       = 100          
                            ttEstimateUpLook.vDueCode    = "ON"          
                            ttEstimateUpLook.vJobNo      = v-job-no         
                            ttEstimateUpLook.vJobNo2     = v-job-no2         
                            ttEstimateUpLook.vCName      = cust.NAME          
                            ttEstimateUpLook.vCAddr      = cust.addr[1]          
                            ttEstimateUpLook.vCAddr2     = cust.addr[2]            
                            ttEstimateUpLook.vCCity      = cust.city          
                            ttEstimateUpLook.vCState     = cust.state         
                            ttEstimateUpLook.vCZip       = cust.zip           
                            ttEstimateUpLook.vContact    = cust.contact          
                            ttEstimateUpLook.vLstDate    = (today + cust.ship-days)          
                            ttEstimateUpLook.vDueDate    = (today + cust.ship-days)       
                            ttEstimateUpLook.vTerms      = cust.terms                        
                            ttEstimateUpLook.vOverPct    = cust.over-pct           
                            ttEstimateUpLook.vUnderPct   = cust.under-pct            
                            ttEstimateUpLook.vFobCode    = cust.fob-code                     
                            ttEstimateUpLook.vTaxGr      = cust.tax-gr                          
                            ttEstimateUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttEstimateUpLook.vsoldname = soldto.sold-name 
                            ttEstimateUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttEstimateUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttEstimateUpLook.vsoldcity = soldto.sold-city
                            ttEstimateUpLook.vsoldstat = soldto.sold-state
                            ttEstimateUpLook.vsoldzip  = soldto.sold-zip .
                        

                       FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = eb.sman  NO-LOCK NO-ERROR.
                       IF AVAIL sman THEN  ASSIGN
                                                ttEstimateUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttEstimateUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttEstimateUpLook.vComm = eb.comm.
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
                           ttEstimateUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttEstimateUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttEstimateUpLook.vTDscr  = terms.dscr.
                       else ttEstimateUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttEstimateUpLook.vCTyp = "T".
                            if ttEstimateUpLook.vCarrier eq "" then ttEstimateUpLook.vCarrier = cust.carrier.
                             
                            find first est WHERE  est.company = prmComp AND 
                                 est.est-no =  FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no) no-lock no-error.
                            ASSIGN
                               ttEstimateUpLook.vEstType  = est.est-type .
                            v-factor = if est.est-type ge 1 and est.est-type le 4 then lastship-dec
                                else 1.
                                     
                                     if lastship-cha eq "Fibre" THEN DO:
                                         
                                         ASSIGN
                                         ttEstimateUpLook.vLstDate    = (TODAY + (cust.ship-days * v-factor))          
                                         ttEstimateUpLook.vDueDate    = (TODAY + (lastship-int * v-factor)) .
                                          END.

                                          FIND FIRST quotehd WHERE quotehd.est-no = eb.est-no AND quotehd.company = prmComp NO-LOCK NO-ERROR.                            
                                          FIND FIRST quoteitm WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
                                          IF AVAIL quotehd AND AVAIL quoteitm THEN DO:             
                                            ttEstimateUpLook.vQuoteno  = quoteitm.q-no .
                                          END.
                               END.
                     END.  /*do transaction*/
             END.
 end.  /* if prmField = CustomerPart*/

END. /* end of search */
