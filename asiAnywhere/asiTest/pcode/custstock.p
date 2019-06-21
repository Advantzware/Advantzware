/*------------------------------------------------------------------------
    File        : CustStockLook.p
    Purpose     :
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustStockLook NO-UNDO 
    FIELD vActive        AS CHARACTER
    FIELD vTerms         AS CHARACTER
    FIELD vLoc           AS CHARACTER
    FIELD vCarrier       AS CHAR
    FIELD vDelzone      AS CHAR
    FIELD vUnderpct      AS DECIMAL
    FIELD vOverpct       AS DECIMAL
    FIELD  vMarkup       AS DECIMAL
    FIELD  vShipdays     AS INT
    FIELD  vPallet       AS CHAR
    FIELD  vCasebundle   AS CHAR
    FIELD  vIntfield     AS INT
    FIELD  vInvmeth      LIKE cust.inv-meth
    FIELD  vFrtpay       AS CHAR
    FIELD  vFobcode      AS CHAR
    FIELD  vTaxgr        AS CHAR
    FIELD  vTaxid         AS CHAR
    FIELD  vCurrcode     AS CHAR
    FIELD  vOrdlim       AS DECIMAL
    FIELD  vCrlim        AS  DECIMAL
                    
    FIELD  vType         AS CHAR
    FIELD  vSman         AS CHAR
    FIELD  vdescsman     AS CHAR
    FIELD  vdesctype     AS CHAR
    FIELD  vdescterms    AS CHAR
    FIELD vdescloc       AS CHAR
    FIELD vdescarrier    AS CHAR
    FIELD vdesterr       AS CHAR
    FIELD vdeszone       AS CHAR
    FIELD vtexcode       AS CHAR
    FIELD vterr          AS CHAR

    .
                                           
    
DEFINE DATASET dsCustStockLook FOR ttCustStockLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustStockLook.
       

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmComp     = ? THEN ASSIGN prmComp      = "".
IF prmUser     = ? THEN  ASSIGN prmUser     = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction =  "CustStock" then do:
    FIND FIRST cust WHERE cust.company = prmComp AND cust.ACTIVE = "X" NO-LOCK NO-ERROR.
    create ttCustStockLook.
    ASSIGN
        ttCustStockLook.vActive           = "A" 
        ttCustStockLook.vTerms            = cust.terms 
        ttCustStockLook.vLoc              = cust.loc 
        ttCustStockLook.vCarrier          = cust.carrier 
        ttCustStockLook.vDelzone         = cust.del-zone
        ttCustStockLook.vUnderpct        = cust.under-pct 
        ttCustStockLook.vOverpct         = cust.over-pct
        ttCustStockLook.vMarkup           = cust.markup 
        ttCustStockLook.vShipdays        = cust.ship-days
        ttCustStockLook.vPallet           = cust.pallet
        ttCustStockLook.vCasebundle      = cust.case-bundle
        ttCustStockLook.vIntfield        = cust.int-field[1]
        ttCustStockLook.vInvmeth         = cust.inv-meth 
        ttCustStockLook.vFrtpay          = cust.frt-pay
        ttCustStockLook.vFobcode         = cust.fob-code
        ttCustStockLook.vTaxgr           = cust.tax-gr  
        ttCustStockLook.vTaxid           = cust.tax-id
        ttCustStockLook.vCurrcode        = cust.curr-code 
        ttCustStockLook.vOrdlim          = cust.ord-lim
        ttCustStockLook.vCrlim           = cust.cr-lim

        ttCustStockLook.vType          = cust.type
        ttCustStockLook.vSman          = cust.sman 
        ttCustStockLook.vterr          = cust.terr 
         .

        FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = cust.sman NO-LOCK NO-ERROR.
     IF  AVAILABLE sman  THEN DO:
         ASSIGN
             ttCustStockLook.vdescsman = sman.sname.
     END.

     FIND FIRST custype WHERE custype.custype = cust.TYPE NO-LOCK NO-ERROR.
     IF AVAILABLE custype THEN  DO:
         ASSIGN
             ttCustStockLook.vdesctype = custype.dscr.
     END.

     FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
     IF  AVAILABLE terms  THEN DO:
         ASSIGN
             ttCustStockLook.vdescterms = terms.dscr.
     END.

     FIND FIRST loc  WHERE loc.loc = cust.loc NO-LOCK NO-ERROR.
      IF  AVAILABLE loc  THEN DO:
        ASSIGN
            ttCustStockLook.vdescloc = loc.dscr.
     END.


     FIND FIRST carrier  WHERE carrier.carrier = cust.carrier NO-LOCK NO-ERROR.
      IF  AVAILABLE carrier  THEN DO:
       ASSIGN
           ttCustStockLook.vdescarrier = carrier.dscr.
     END.

     FIND FIRST terr WHERE terr.terr = cust.terr  NO-LOCK NO-ERROR.
      IF  AVAILABLE terr  THEN DO:
      ASSIGN
          ttCustStockLook.vdesterr = terr.dscr.
          
     END.

      FIND FIRST carr-mtx WHERE carr-mtx.del-zone = cust.del-zone   NO-LOCK NO-ERROR.
      IF  AVAILABLE carr-mtx  THEN DO:
      ASSIGN
          ttCustStockLook.vdeszone = carr-mtx.del-dscr.
     END.

     

    FIND FIRST stax WHERE stax.tax-group = cust.tax-gr NO-LOCK NO-ERROR.
   IF   AVAILABLE stax  THEN DO:
       ASSIGN
         ttCustStockLook.vtexcode = stax.tax-dscr[1].
   END.
     
       
 END.
