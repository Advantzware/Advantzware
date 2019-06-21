

                                 
/*------------------------------------------------------------------------
    File        : AddOrderValidate.p
    Purpose     : Validation for add order

    Syntax      :

    Description : Validation for add order

    Author(s)   : Sewa
    Created     : April 7, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{OrderEstUpdate.i}

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmExt         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum    AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmUserid      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmStat        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSold        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmOrdate      AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldName    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDueCode     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDueDate     AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmCustAddr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldAddr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmLastDate    AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmcustAddr2   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldAddr2   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmProdDate    AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmCity        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmState       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmZip         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldCity    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldState   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldZip     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmPonum       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmContact     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOverpct     AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmUnderpct    AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmTerms       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmTermdscr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmProd        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmTaxgr       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFreight     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCarrier     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFob         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSname       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman2       AS CHAR NO-UNDO.                   
DEFINE INPUT PARAMETER prmSname2      AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmSman3       AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmSname3      AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmCtype       AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmcExp        AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER prmCnum        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmCauth       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCustName    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmType        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmLine        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmWhis        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER VRowid         AS RECID       NO-UNDO.
DEFINE INPUT PARAMETER prmJob         AS CHARACTER.
DEFINE INPUT PARAMETER prmJob2        AS INTEGER.
DEFINE INPUT PARAMETER prmEst         AS CHARACTER.
DEFINE INPUT PARAMETER prmSales1      AS DECIMAL.
DEFINE INPUT PARAMETER prmSales2      AS DECIMAL.
DEFINE INPUT PARAMETER prmSales3      AS DECIMAL.
DEFINE INPUT PARAMETER prmComm1       AS DECIMAL.
DEFINE INPUT PARAMETER prmComm2       AS DECIMAL.
DEFINE INPUT PARAMETER prmComm3       AS DECIMAL.
DEFINE INPUT PARAMETER prmQuote       AS INTEGER NO-UNDO. 
                                    
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser        = ?  THEN ASSIGN    prmUser = "".
IF prmExt         = ?  THEN ASSIGN    prmExt = "".
IF prmOrderNum    = ?  THEN ASSIGN    prmOrderNum = 0.
IF prmAction      = ?  THEN ASSIGN    prmAction      = "".
IF prmAction      = "" THEN ASSIGN    prmAction      = "Select".
IF prmContact     = ?  THEN ASSIGN    prmContact     = "".      
IF prmCustomer    = ?  THEN ASSIGN    prmCustomer    = "".
IF prmUserid      = ?  THEN ASSIGN    prmUserid      = "".  
IF prmStat        = ?  THEN ASSIGN    prmStat        = "".  
IF prmSold        = ?  THEN ASSIGN    prmSold        = "".  
IF prmSoldName    = ?  THEN ASSIGN    prmSoldName    = "".  
IF prmDueCode     = ?  THEN ASSIGN    prmDueCode     = "".             
IF prmCustAddr    = ?  THEN ASSIGN    prmCustAddr    = "".     
IF prmSoldAddr    = ?  THEN ASSIGN    prmSoldAddr    = "". 
IF prmcustAddr2   = ?  THEN ASSIGN    prmcustAddr2   = "".              
IF prmSoldAddr2   = ?  THEN ASSIGN    prmSoldAddr2   = "".     
IF prmCity        = ?  THEN ASSIGN    prmCity        = "". 
IF prmState       = ?  THEN ASSIGN    prmState       = "". 
IF prmZip         = ?  THEN ASSIGN    prmZip         = "".    
IF prmSoldCity    = ?  THEN ASSIGN    prmSoldCity    = "".  
IF prmSoldState   = ?  THEN ASSIGN    prmSoldState   = "".  
IF prmSoldZip     = ?  THEN ASSIGN    prmSoldZip     = "".  
IF prmPonum       = ?  THEN ASSIGN    prmPonum       = "".  
IF prmOverpct     = ?  THEN ASSIGN    prmOverpct     = 0.  
IF prmUnderpct    = ?  THEN ASSIGN    prmUnderpct    = 0.  
IF prmTerms       = ?  THEN ASSIGN    prmTerms       = "".  
IF prmTermdscr    = ?  THEN ASSIGN    prmTermdscr    = "".       
IF prmProd        = ?  THEN ASSIGN    prmProd        = 0.  
IF prmTaxgr       = ?  THEN ASSIGN    prmTaxgr       = "".  
IF prmFreight     = ?  THEN ASSIGN    prmFreight     = "".  
IF prmCarrier     = ?  THEN ASSIGN    prmCarrier     = "".  
IF prmFob         = ?  THEN ASSIGN    prmFob         = "".  
IF prmSman        = ?  THEN ASSIGN    prmSman        = "".  
IF prmSname       = ?  THEN ASSIGN    prmSname       = "".  
IF prmSman2       = ?  THEN ASSIGN    prmSman2       = "".  
IF prmSname2      = ?  THEN ASSIGN    prmSname2      = "".  
IF prmSman3       = ?  THEN ASSIGN    prmSman3       = "".  
IF prmSname3      = ?  THEN ASSIGN    prmSname3      = "".  
IF prmCtype       = ?  THEN ASSIGN    prmCtype       = "".
IF prmCnum        = ?  THEN ASSIGN    prmCnum        = "". 
IF prmCauth       = ?  THEN ASSIGN    prmCauth       = "". 
IF prmCustName    = ?  THEN ASSIGN    prmCustName    = "". 
IF prmType        = ?  THEN ASSIGN    prmType        = "". 
IF prmLine        = ?  THEN ASSIGN    prmLine        = 0. 
IF prmEst         = ? THEN ASSIGN     prmEst         =  "".
IF prmJob         = ? THEN ASSIGN     prmJob         = "".
IF prmJob2        = ? THEN ASSIGN     prmJob2        = 0.
IF prmQuote       = ? THEN ASSIGN     prmQuote       = 0.
                                                 
IF prmAction = ""  THEN ASSIGN prmAction = "Select".

DEF VAR prmLoc AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.

{sys/inc/var.i "new shared" }
    {oe/oe-sysct1.i NEW}

   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
g_company = prmComp
g_loc     = "Main"
 cocode = g_company
 locode = g_loc.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

DEF VAR vEstim AS CHARACTER NO-UNDO.
DEFINE VARIABLE Custnum AS CHAR NO-UNDO.
DEFINE VAR vest AS CHAR.
DEF VAR ll-valid-po-no AS LOG INITIAL NO NO-UNDO.
def var oeprompt like sys-ctrl.log-fld no-undo.


IF prmAction = "Validate" THEN DO:
    
    /*FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
IF AVAILABLE users THEN DO:
    IF users.internal-user = NO THEN DO:
        FOR EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK BY quotehd.q-no DESC: 
        FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no
             AND eb.form-no NE 0  NO-LOCK NO-ERROR.
        IF eb.stock-no = "" THEN DO:
            cError = "Sorry Order cannot be Added as Quote does not have an FG Item Code".
            RETURN.
        END.
        END.
    END.
    END. /*IF users.internal-user = NO*/*/

    FIND FIRST cust WHERE cust.company EQ cocode
                                 AND cust.cust-no EQ prmCustomer NO-LOCK NO-ERROR.
    IF NOT AVAIL cust THEN DO:
        ASSIGN 
               cError ="Invalid Customer. Try Help..".
           RETURN.
    END.

     find first soldto where soldto.company = g_company and
                            soldto.cust-no = prmCustomer
                        and trim(soldto.sold-id) = trim(prmSold)
                        no-lock no-error.
   IF NOT AVAIL soldto THEN DO:
        ASSIGN 
               cError ="Invalid Sold To. Try Help..".
           RETURN.
    END.


    IF date(prmDueDate) < DATE(prmOrdate) THEN DO:
       ASSIGN 
               cError ="Due date can not be less than order date".
           RETURN.
            END.

            IF prmCtype <> "" THEN do:
                  IF date(prmcExp) <> DATE("01/13/001") THEN DO:
                      IF date(prmcExp) > DATE("12/31/2099") THEN DO:
                          ASSIGN 
                              cError ="Expire Date Between 01/01/2009 & 12/31/2099".
                          RETURN.
                      END.
                 IF date(prmcExp) < DATE("01/01/2009") THEN DO:
                     ASSIGN 
                         cError ="Expire Date Between 01/01/2009 & 12/31/2099".
                     RETURN.
                     END.
               END.
            END.


   IF prmEst <> "" THEN DO:
         ASSIGN vEstim = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst).
    FIND FIRST eb WHERE  eb.est-no = vEstim  AND eb.company = prmComp AND eb.cust-no = prmCustomer  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE eb THEN DO:
         ASSIGN cError  = "Invalid Estimate Try Help....".
         RETURN.
     END. /*IF NOT AVAILABLE eb THEN DO:*/
     END. 
IF prmEst <> "" THEN DO:
   RUN valid-est-no.
END.

     RUN valid-po-no.

     
    FIND FIRST carrier WHERE carrier.carrier = prmCarrier NO-LOCK NO-ERROR.
     IF NOT AVAILABLE carrier THEN DO:
         ASSIGN cError  = "Invalid carrier".
         RETURN.
     END. /*IF NOT AVAILABLE carrier THEN DO:*/

    FIND first terms where terms.t-code = prmTerms  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE terms  then do:
        ASSIGN cError  = "Invalid Terms Code. Try help. ".
        RETURN.
    END.  /*FIND first terms*/

    IF prmSman <> "" THEN DO:
        FIND sman WHERE sman.sman = prmSman AND sman.company = prmComp NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sman THEN DO:
            ASSIGN cError = "Invalid Salesman".
            RETURN.
        END.   /* IF NOT AVAILABLE sman THEN DO:*/
    END.  /*IF prmSman <> "" THEN DO:*/

    IF prmSman2 <> "" THEN DO:
        FIND sman WHERE sman.sman = prmSman2 AND sman.company = prmComp  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sman THEN DO:
            ASSIGN cError = "Invalid Salesman".
            RETURN.
        END.   /**IF NOT AVAILABLE sman THEN DO:*/
    END.    /**IF prmSman2 <> "" THEN DO:*/
    IF prmSman3 <> "" THEN DO:
        FIND sman WHERE sman.sman = prmSman3  AND sman.company = prmComp NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sman THEN DO:                                                    
            ASSIGN cError = "Invalid Salesman".
            RETURN .
        END.  /*IF NOT AVAILABLE sman THEN DO: */
    END.  /*IF prmSman3 <> "" THEN DO:*/

 IF prmTaxgr <> "" THEN DO: 
    FIND first stax where stax.tax-group = prmTaxgr  NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE stax  then do:
        ASSIGN cError  = "Invalid Tax Code. Try help. ".
        RETURN.
    end.
END.
 IF prmEst NE "" AND
     prmJob NE "" AND
     CAN-FIND(FIRST job-hdr
              WHERE job-hdr.company EQ prmComp
              AND job-hdr.job-no  EQ prmJob
              AND job-hdr.job-no2 EQ INT(prmJob2)) THEN DO:
     cError = "Sorry, job# already exists...".
      RETURN .
      END.  /*IF prmEst NE "" AND*/
     
END.

/******************************update***********************/


IF prmAction = "Update" THEN DO:

     
         IF date(prmDueDate) < DATE(prmOrdate) THEN DO:
             ASSIGN 
                 cError ="Due date can not be less than order date".
             RETURN.
         END.
        
       IF prmCtype <> "" THEN do:
         IF date(prmcExp) <> DATE("01/13/001") THEN DO:
             IF date(prmcExp) > DATE("12/31/2099") THEN DO:
                 ASSIGN 
                     cError ="Expire Date Between 01/01/2009 & 12/31/2099".
                 RETURN.
             END.
        IF date(prmcExp) < DATE("01/01/2009") THEN DO:
            ASSIGN 
                cError ="Expire Date Between 01/01/2009 & 12/31/2099".
            RETURN.
            END.
         END.
      END.

    RUN valid-est-no.
     RUN valid-po-no.
    /*FIND FIRST carrier WHERE carrier.carrier = prmCarrier NO-LOCK NO-ERROR.
     IF NOT AVAILABLE carrier THEN DO:
         ASSIGN cError  = "Invalid carrier".
         RETURN.
     END. /*IF NOT AVAILABLE carrier THEN DO:*/

    FIND first terms where terms.t-code = prmTerms  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE terms  then do:
        ASSIGN cError  = "Invalid Terms Code. Try help. ".
        RETURN.
    END.  /*FIND first terms*/*/

    IF prmSman <> "" THEN DO:
        FIND sman WHERE sman.sman = prmSman AND sman.company = prmComp NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sman THEN DO:
            ASSIGN cError = "Invalid Salesman".
            RETURN.
        END.   /* IF NOT AVAILABLE sman THEN DO:*/
    END.  /*IF prmSman <> "" THEN DO:*/

    IF prmSman2 <> "" THEN DO:
        FIND sman WHERE sman.sman = prmSman2 AND sman.company = prmComp  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sman THEN DO:
            ASSIGN cError = "Invalid Salesman".
            RETURN.
        END.   /**IF NOT AVAILABLE sman THEN DO:*/
    END.    /**IF prmSman2 <> "" THEN DO:*/
    IF prmSman3 <> "" THEN DO:
        FIND sman WHERE sman.sman = prmSman3  AND sman.company = prmComp NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sman THEN DO:                                                    
            ASSIGN cError = "Invalid Salesman".
            RETURN .
        END.  /*IF NOT AVAILABLE sman THEN DO: */
    END.  /*IF prmSman3 <> "" THEN DO:*/

IF prmTaxgr <> "" THEN DO: 
    FIND first stax where stax.tax-group = prmTaxgr  NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE stax  then do:
        ASSIGN cError  = "Invalid Tax Code. Try help. ".
        RETURN.
    end.
END.


 /* IF LOOKUP(prmType,lv-type-codes) LE 0 OR
       (prmType EQ "T" AND
        NOT CAN-FIND(FIRST cust WHERE cust.company EQ prmComp
                                  AND cust.cust-no EQ prmCustomer
                                  AND cust.active  EQ "X")) THEN DO:
      ASSIGN cError =  "Invalid Type, try help...".
      RETURN .
    END.*/

    IF prmEst NE "" AND
       prmJob NE "" AND
       CAN-FIND(FIRST job-hdr
                WHERE job-hdr.company EQ prmComp
                  AND job-hdr.job-no  EQ prmJob
                  AND job-hdr.job-no2 EQ INT(prmJob2)) THEN DO:
      cError = "Sorry, job# already exists...".
      RETURN .
    END.  /*IF prmEst NE "" AND*/
END.   /*IF prmAction = "UPdate" THEN DO:*/                      

/*************************************/



PROCEDURE valid-est-no :
    Custnum = "0" .
    DEFINE VAR vEst AS CHAR.
    ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) .
    
IF prmEst <> "" THEN DO:
      FIND FIRST est
          WHERE est.company = prmComp  AND
             est.est-no  = vEst   NO-LOCK NO-ERROR.
        IF NOT AVAIL est THEN DO:
        ASSIGN cError = "Invalid Estimate#, try help...".
        RETURN .
      END.
    
    IF v-quo-price-log AND v-quo-price-dec EQ 1 THEN DO:
        FOR EACH quotehd
            WHERE quotehd.company eq prmComp
              AND quotehd.loc     eq locode
              AND quotehd.est-no  eq vEst
             NO-LOCK,
             EACH quoteitm OF quotehd NO-LOCK,
             EACH quoteqty OF quoteitm NO-LOCK:
          LEAVE.
        END.
              
        IF NOT AVAIL quoteqty THEN DO:
          cError = "No quotes exists for this estimate...".
          RETURN .
        END.
      END.
END.
END PROCEDURE.

PROCEDURE valid-po-no :

  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER cust-po-mand FOR reftable.
  FIND FIRST cust NO-LOCK
        WHERE cust.company EQ prmComp
          AND cust.cust-no EQ prmCustomer
          AND CAN-FIND(FIRST cust-po-mand
                       WHERE cust-po-mand.reftable EQ "cust.po-mand"
                         AND cust-po-mand.company  EQ cust.company
                         AND cust-po-mand.loc      EQ ""
                         AND cust-po-mand.code     EQ cust.cust-no
                         AND cust-po-mand.val[1]   EQ 1)
        NO-ERROR.
    
    IF AVAIL cust AND TRIM(prmPonum) EQ "" THEN DO:
      ASSIGN cError = "PO# is mandatory for this Customer...".
      RETURN .
    END.

    IF NOT ll-valid-po-no AND oeprompt AND prmPonum NE "" THEN
    FIND FIRST b-oe-ordl
        WHERE b-oe-ordl.company EQ oe-ord.company
          AND b-oe-ordl.po-no   EQ prmPonum
          AND b-oe-ordl.cust-no EQ prmCustomer
          AND b-oe-ordl.ord-no  NE prmOrderNum
        NO-LOCK NO-ERROR.

    IF AVAIL b-oe-ordl THEN DO:
      ASSIGN cError = "Customer PO already exists for Order/Item - " .
          /*+ 
              TRIM(STRING(b-oe-ordl.ord-no,">>>>>>>>")) + "/" +
              TRIM(b-oe-ordl.i-no) " ." SKIP
              "Do you want to continue?".*/
     
        RETURN .
      END.
      ELSE ll-valid-po-no = YES.
  
END PROCEDURE.
