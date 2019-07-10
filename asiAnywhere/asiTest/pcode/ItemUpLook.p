

/*------------------------------------------------------------------------
    File         : ItemUpLook.p
    Purpose     :  estimate lookup

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemUpLook NO-UNDO
    
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
    FIELD vsoldname  AS CHARACTER
    FIELD vsoldadd1    AS CHARACTER
    FIELD vsoldadd2    AS CHARACTER
    FIELD vsoldcity    AS CHARACTER
    FIELD vsoldstat    AS CHARACTER
    FIELD vsoldzip     AS CHARACTER
    FIELD vComm     AS DECIMAL
    FIELD vsalesman     AS CHARACTER
    FIELD vOrder    AS INTEGER
    FIELD vprevord AS INTEGER
    FIELD vrfq AS INTEGER
      .
DEFINE DATASET dsItemUpLookup FOR ttItemUpLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemUpLookup.

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
DEF VAR vEstim AS CHARACTER NO-UNDO.

IF prmAction      = ? THEN ASSIGN prmAction      = "".

FIND FIRST usercomp WHERE
 usercomp.user_id = prmUser AND
 usercomp.loc = '' AND
 usercomp.company_default = YES
 NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
FOR EACH usercust WHERE
    usercust.user_id =  prmUser AND
    usercust.company = prmComp
     NO-LOCK:

    ASSIGN vEstim = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText).
    FOR EACH eb WHERE  (eb.est-no = vEstim OR vEstim = "" OR vEstim = ? ) AND eb.cust-no = usercust.cust-no  no-lock:
        FIND FIRST rfqitem WHERE rfqitem.est-no =  eb.est-no    NO-LOCK NO-ERROR. 
        IF AVAIL rfqitem THEN DO:
          FOR EACH rfqitem WHERE rfqitem.est-no =  eb.est-no   NO-LOCK: 
            create ttItemUpLook.
            assign                                     
                ttItemUpLook.vEstimate   = eb.est-no
                ttItemUpLook.vCustomer   = eb.cust-no 
                ttItemUpLook.vshipname   = eb.ship-name
                ttItemUpLook.vdscr       = eb.part-dscr1
                ttItemUpLook.vpartno     = eb.part-no 
                ttItemUpLook.vRfqno      = rfqitem.rfq-no .
              ASSIGN 
                 ttItemUpLook.vrfq        = rfqitem.rfq-no  .

            FIND FIRST  quotehd WHERE quotehd.est-no = rfqitem.est-no   NO-LOCK NO-ERROR.
            IF AVAIL quotehd THEN
                ASSIGN
                ttItemUpLook.vQuoteno    = quotehd.q-no   .
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
                            ttItemUpLook.vprevord      = oe-ord.ord-no
                            ttItemUpLook.vOrder      = Ordernum
                            ttItemUpLook.vEstimate   = eb.est-no
                            ttItemUpLook.vsoldid     = eb.cust-no 
                            ttItemUpLook.vsman       = eb.sman 
                            ttItemUpLook.vCustomer   = eb.cust-no
                            ttItemUpLook.vCarrier    = eb.carrier 
                            ttItemUpLook.vfreight    = eb.chg-method
                            ttItemUpLook.vSpct       = 100          
                            ttItemUpLook.vDueCode    = "ON"          
                            ttItemUpLook.vJobNo      = v-job-no         
                            ttItemUpLook.vJobNo2     = v-job-no2         
                            ttItemUpLook.vCName      = cust.NAME          
                            ttItemUpLook.vCAddr      = cust.addr[1]          
                            ttItemUpLook.vCAddr2     = cust.addr[2]            
                            ttItemUpLook.vCCity      = cust.city          
                            ttItemUpLook.vCState     = cust.state         
                            ttItemUpLook.vCZip       = cust.zip           
                            ttItemUpLook.vContact    = cust.contact          
                            ttItemUpLook.vLstDate    = (today + cust.ship-days)          
                            ttItemUpLook.vDueDate    = (today + cust.ship-days)        
                            ttItemUpLook.vTerms      = cust.terms                        
                            ttItemUpLook.vOverPct    = cust.over-pct           
                            ttItemUpLook.vUnderPct   = cust.under-pct            
                            ttItemUpLook.vFobCode    = cust.fob-code                     
                            ttItemUpLook.vTaxGr      = cust.tax-gr                          
                            ttItemUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttItemUpLook.vsoldname = soldto.sold-name 
                            ttItemUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttItemUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttItemUpLook.vsoldcity = soldto.sold-city
                            ttItemUpLook.vsoldstat = soldto.sold-state
                            ttItemUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttItemUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttItemUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttItemUpLook.vComm = eb.comm.
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
                           ttItemUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttItemUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttItemUpLook.vTDscr  = terms.dscr.
                       else ttItemUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttItemUpLook.vCTyp = "T".
                            if ttItemUpLook.vCarrier eq "" then ttItemUpLook.vCarrier = cust.carrier.
                            
            END.  /*do transaction*/
       END.  /*for each rfqitem*/ 
        END. /* if avail rfqitem*/

        IF NOT AVAIL rfqitem  THEN DO:
              CREATE ttItemUpLook.
                assign  
                    ttItemUpLook.vEstimate   = eb.est-no
                    ttItemUpLook.vCustomer   = eb.cust-no 
                    ttItemUpLook.vshipname   = eb.ship-name
                    ttItemUpLook.vdscr       = eb.part-dscr1
                    ttItemUpLook.vpartno     = eb.part-no .

               
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
                            ttItemUpLook.vprevord      = oe-ord.ord-no
                            ttItemUpLook.vOrder      = Ordernum
                            ttItemUpLook.vEstimate   = eb.est-no
                            ttItemUpLook.vsoldid     = eb.cust-no 
                            ttItemUpLook.vsman       = eb.sman 
                            ttItemUpLook.vCustomer   = eb.cust-no
                            ttItemUpLook.vCarrier    = eb.carrier 
                            ttItemUpLook.vfreight    = eb.chg-method
                            ttItemUpLook.vSpct       = 100          
                            ttItemUpLook.vDueCode    = "ON"          
                            ttItemUpLook.vJobNo      = v-job-no         
                            ttItemUpLook.vJobNo2     = v-job-no2         
                            ttItemUpLook.vCName      = cust.NAME          
                            ttItemUpLook.vCAddr      = cust.addr[1]          
                            ttItemUpLook.vCAddr2     = cust.addr[2]            
                            ttItemUpLook.vCCity      = cust.city          
                            ttItemUpLook.vCState     = cust.state         
                            ttItemUpLook.vCZip       = cust.zip           
                            ttItemUpLook.vContact    = cust.contact          
                            ttItemUpLook.vLstDate    = (today + cust.ship-days)          
                            ttItemUpLook.vDueDate    = (today + cust.ship-days)        
                            ttItemUpLook.vTerms      = cust.terms                        
                            ttItemUpLook.vOverPct    = cust.over-pct           
                            ttItemUpLook.vUnderPct   = cust.under-pct            
                            ttItemUpLook.vFobCode    = cust.fob-code                     
                            ttItemUpLook.vTaxGr      = cust.tax-gr                          
                            ttItemUpLook.vCType      = cust.TYPE.


                        FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN     
                            ttItemUpLook.vsoldname = soldto.sold-name 
                            ttItemUpLook.vsoldadd1 = soldto.sold-addr[1] 
                            ttItemUpLook.vsoldadd2 = soldto.sold-addr[2]
                            ttItemUpLook.vsoldcity = soldto.sold-city
                            ttItemUpLook.vsoldstat = soldto.sold-state
                            ttItemUpLook.vsoldzip  = soldto.sold-zip .

                       FIND FIRST sman WHERE sman.sman = eb.sman  NO-LOCK NO-ERROR.
                        ASSIGN
                            ttItemUpLook.vsalesman = sman.sname.  

                        FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no AND itemfg.company = prmComp NO-LOCK NO-ERROR.
                        ASSIGN
                            ttItemUpLook.vItemDesc   = itemfg.i-dscr .
                        IF oeestcom-log = NO THEN
                            ttItemUpLook.vComm = eb.comm.
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
                           ttItemUpLook.vComm = v-com.
                       
                       END.
                       ELSE
                           ttItemUpLook.vComm = eb.comm.
                       END.
                       find first terms where terms.company eq prmComp
                           and terms.t-code  eq cust.terms
                           no-lock no-error.
                       if avail terms then  ttItemUpLook.vTDscr  = terms.dscr.
                       else ttItemUpLook.vTDscr   = "".
                            if cust.active eq "X" then ttItemUpLook.vCTyp = "T".
                            if ttItemUpLook.vCarrier eq "" then ttItemUpLook.vCarrier = cust.carrier.
                            
            END.  /*do transaction*/

        END.  /* not avail rfqitem */
     END. /*for each eb**/

END. /*for each usercust*/




