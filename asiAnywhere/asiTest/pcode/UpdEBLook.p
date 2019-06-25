
/*------------------------------------------------------------------------
    File         : UpdEBLook.p
    Purpose     :  estimate lookup

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUpdEBLook NO-UNDO 

        FIELD vEstimate AS CHARACTER
        FIELD vsoldid   AS CHARACTER
        FIELD vsman     AS CHARACTER
        FIELD vsdscr     AS CHARACTER
        FIELD vCustomer AS CHARACTER
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
                                           
    
DEFINE DATASET dsUpdEBLook FOR ttUpdEBLook.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEst      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUpdEBLook.

DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEF BUFFER xest FOR est.
    DEF BUFFER xeb FOR est.

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
FIND LAST oe-ord WHERE oe-ord.company = prmComp  NO-LOCK NO-ERROR.
    IF AVAIL oe-ord THEN DO:
        ASSIGN    Ordernum = oe-ord.ord-no + 1.
    END.  /*IF AVAIL bf-ord THEN DO:*/
    ELSE DO:
        ASSIGN  Ordernum = 1.
    END.

    
DO TRANSACTION:
v-est-no = vEst.
run util/rjust.p (input-output v-est-no,8).
vEst = v-est-no.
 
find first xest
    where xest.company eq prmComp
      and xest.est-no  eq vEst 
    no-lock no-error.
 
ASSIGN
 v-est-type = xest.est-type - IF xest.est-type GT 4 THEN 4 ELSE 0
 ll-do-job  = /*job#-int EQ 0 OR*/
              CAN-FIND(FIRST eb WHERE eb.company EQ xest.company
                                  AND eb.est-no  EQ xest.est-no
                                  AND eb.pur-man EQ NO
                                  AND eb.form-no NE 0).

if avail xest then do:
  assign
   j         = 1.

   
  FOR EACH   rfqitem WHERE rfqitem.est-no = xest.est-no NO-LOCK:
  for each eb where eb.company = xest.company 
                and eb.est-no  eq xest.est-no
                and eb.form-no  ne 0
                and eb.blank-no ne 0
                AND TRIM(eb.cust-no) NE ""
      no-lock,
      FIRST cust NO-LOCK WHERE 
      /*{sys/ref/cust.w}*/
         cust.cust-no eq eb.cust-no
      USE-INDEX cust
      break by eb.est-no by eb.cust-no by eb.form-no by eb.blank-no:
      

    if first-of(eb.cust-no) then do:
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

    
      create ttUpdEBLook.
            assign 
                 ttUpdEBLook.vprevord      = oe-ord.ord-no
                ttUpdEBLook.vOrder      = Ordernum
                ttUpdEBLook.vEstimate   = eb.est-no
                ttUpdEBLook.vsoldid     = eb.cust-no 
                ttUpdEBLook.vsman       = eb.sman
                ttUpdEBLook.vCustomer   = eb.cust-no
                ttUpdEBLook.vCarrier    = eb.carrier 
                ttUpdEBLook.vfreight    = eb.chg-method
               
                ttUpdEBLook.vSpct       = 100          
                ttUpdEBLook.vDueCode    = "ON"          
                ttUpdEBLook.vJobNo      = v-job-no         
                ttUpdEBLook.vJobNo2     = v-job-no2         
                ttUpdEBLook.vCName      = cust.NAME          
                ttUpdEBLook.vCAddr      = cust.addr[1]          
                ttUpdEBLook.vCAddr2     = cust.addr[2]            
                ttUpdEBLook.vCCity      = cust.city          
                ttUpdEBLook.vCState     = cust.state         
                ttUpdEBLook.vCZip       = cust.zip           
                ttUpdEBLook.vContact    = cust.contact          
                ttUpdEBLook.vLstDate    = (today + cust.ship-days)          
                ttUpdEBLook.vDueDate    = (today + cust.ship-days)        
                ttUpdEBLook.vTerms      = cust.terms                        
                                     
                ttUpdEBLook.vOverPct    = cust.over-pct           
                ttUpdEBLook.vUnderPct   = cust.under-pct            
                ttUpdEBLook.vFobCode    = cust.fob-code                     
                ttUpdEBLook.vTaxGr      = cust.tax-gr                          
                ttUpdEBLook.vCType      = cust.TYPE
                ttUpdEBLook.vrfq        = rfqitem.rfq-no                         .
MESSAGE "rfq"  rfqitem.rfq-no.
             FIND FIRST soldto WHERE soldto.cust-no = eb.cust-no AND soldto.company = prmComp NO-LOCK NO-ERROR.
             ASSIGN     
              ttUpdEBLook.vsoldname = soldto.sold-name 
              ttUpdEBLook.vsoldadd1 = soldto.sold-addr[1] 
              ttUpdEBLook.vsoldadd2 = soldto.sold-addr[2]
              ttUpdEBLook.vsoldcity = soldto.sold-city
              ttUpdEBLook.vsoldstat = soldto.sold-state
              ttUpdEBLook.vsoldzip  = soldto.sold-zip .
             FIND FIRST sman WHERE sman.sman = eb.sman  NO-LOCK NO-ERROR.
             ASSIGN
                 ttUpdEbLook.vsalesman = sman.sname.
             FIND FIRST itemfg WHERE itemfg.cust-no = eb.cust-no  NO-LOCK NO-ERROR.
             ASSIGN
                  ttUpdEBLook.vItemDesc   = itemfg.i-dscr .
             

MESSAGE "ttUpdEBLook.vprevord" ttUpdEBLook.vprevord.

      IF oeestcom-log = NO THEN
         ttUpdEBLook.vComm = eb.comm.
      ELSE
      DO:
         lv-qty = 0.
         find first est-qty where
              est-qty.company = xest.company AND
              est-qty.est-no = xest.est-no
              no-lock no-error.

         /*best guess before actually picking qty
           order header will be updated later with current commission*/
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
MESSAGE "testest" prmEst.
         IF AVAIL probe THEN
         DO:
            ld-marg% = probe.market-price.

            RUN est/getsmanmtrx.p (INPUT ROWID(xest),
                                   INPUT "C",
                                   INPUT-OUTPUT v-com,
                                   INPUT-OUTPUT ld-marg%).
            
            ttUpdEBLook.vComm = v-com.
         END.
         ELSE
            ttUpdEBLook.vComm = eb.comm.
      END.
      find first terms where terms.company eq prmComp
                        and terms.t-code  eq cust.terms
               no-lock no-error.
      if avail terms then  ttUpdEBLook.vTDscr  = terms.dscr.
      else ttUpdEBLook.vTDscr   = "".

      if cust.active eq "X" then ttUpdEBLook.vCTyp = "T".
      if ttUpdEBLook.vCarrier eq "" then ttUpdEBLook.vCarrier = cust.carrier.
    end. /* first-of(eb.cust-no) */
    end. /* each eb */
  END.  /*for each rfqitem*/
end. /* avail xest */
END. /* transaction */
            

                       /*************************************************************************/
