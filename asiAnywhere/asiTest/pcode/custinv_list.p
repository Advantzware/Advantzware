
/*------------------------------------------------------------------------
    File        : custinv_list.p
    Purpose     : Customer
    Main File   : ar\v-arinv.w
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCustArinvoiceslist NO-UNDO
    FIELD invno       AS INT     
    FIELD custno      AS CHAR         
    FIELD custname    AS CHAR     
    FIELD invdate     AS CHAR  
    FIELD gross       AS DECIMAL        
    FIELD paid        AS DECIMAL        
    FIELD baldue      AS DECIMAL  
    FIELD taxamt      AS DECIMAL
    FIELD due         AS DECIMAL
    FIELD shipid      AS CHAR
    FIELD shipname    AS CHAR
    FIELD pono        AS CHAR
    FIELD duedate     AS CHAR
    FIELD taxcode     AS CHAR
    FIELD terms       AS CHAR
    FIELD termsdesc   AS CHAR
    FIELD discount    AS DECIMAL
    FIELD disctaken   AS DECIMAL
    FIELD discdays    AS INT
    FIELD carrier     AS CHAR
    FIELD freight     AS DECIMAL
    FIELD currcode    AS CHAR
    FIELD exrate      AS DECIMAL
    FIELD reckey      AS CHAR

    FIELD bmmm AS CHAR 
    .

DEFINE DATASET dsCustArinvoiceslist FOR ttCustArinvoiceslist.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInv      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmOldInv   AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmPosted   AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER prmCustname    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInvdate     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmGross       AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmPaid        AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmBaldue      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmTaxamt      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmDue         AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmShipid      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShipname    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPono        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDuedate     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTaxcode     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTerms       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTermsdesc   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDiscount    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmDisctaken   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmDiscdays    AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmCarrier     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFreight     AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmCurrcode    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmExrate      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey      AS CHAR NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustArinvoiceslist .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttCustArinvoiceslist:
        DELETE ttCustArinvoiceslist .
    END.

IF prmAction       = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp         = ?  THEN ASSIGN prmComp       = "".
IF prmUser         = ?  THEN ASSIGN prmUser       = "".
IF prmCust         = ?  THEN ASSIGN prmCust     = "".
IF prmInv          = ?  THEN ASSIGN prmInv      = 0.
IF prmCustname        = ?  THEN ASSIGN prmCustname   = "".
IF prmInvdate         = ?  THEN ASSIGN prmInvdate    = "". 
IF prmGross           = ?  THEN ASSIGN prmGross      = 0.
IF prmPaid            = ?  THEN ASSIGN prmPaid       = 0.
IF prmBaldue          = ?  THEN ASSIGN prmBaldue     = 0.
IF prmTaxamt          = ?  THEN ASSIGN prmTaxamt     = 0.
IF prmDue             = ?  THEN ASSIGN prmDue        = 0.
IF prmShipid          = ?  THEN ASSIGN prmShipid     = "".
IF prmShipname        = ?  THEN ASSIGN prmShipname   = "".
IF prmPono            = ?  THEN ASSIGN prmPono       = "".
IF prmDuedate         = ?  THEN ASSIGN prmDuedate    = "".
IF prmTaxcode         = ?  THEN ASSIGN prmTaxcode    = "".
IF prmTerms           = ?  THEN ASSIGN prmTerms      = "".
IF prmTermsdesc       = ?  THEN ASSIGN prmTermsdesc  = "".
IF prmDiscount        = ?  THEN ASSIGN prmDiscount   = 0.
IF prmDisctaken       = ?  THEN ASSIGN prmDisctaken  = 0.
IF prmDiscdays        = ?  THEN ASSIGN prmDiscdays   = 0.
IF prmCarrier         = ?  THEN ASSIGN prmCarrier    = "".
IF prmFreight         = ?  THEN ASSIGN prmFreight    = 0.
IF prmCurrcode        = ?  THEN ASSIGN prmCurrcode   = "".
IF prmExrate          = ?  THEN ASSIGN prmExrate     = 0.


DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
  DEF BUFFER bARInvl FOR ar-invl. 
   DEF VAR X AS INT NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.

  
         DEF BUFFER b-ar-inv FOR ar-inv.
         DEF BUFFER b-ar-invl FOR ar-invl.
         DEF BUFFER bf-inv FOR ar-inv.
         DEF VAR lv-msg AS CHAR NO-UNDO.
         DEFINE VAR custcount AS CHAR NO-UNDO.
     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp  .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

IF prmAction = "Select" THEN DO:
          FOR EACH ar-inv WHERE  ar-inv.company = cocode AND ar-inv.posted =  LOGICAL(prmPosted) AND LOOKUP(ar-inv.cust-no,custcount) <> 0   NO-LOCK :
             CREATE ttCustArinvoiceslist.
             ASSIGN 
                 ttCustArinvoiceslist.invno       = ar-inv.inv-no 
                 ttCustArinvoiceslist.custno      = ar-inv.cust-no
                 ttCustArinvoiceslist.custname    = ar-inv.cust-name
                 ttCustArinvoiceslist.invdate     = string(ar-inv.inv-date)
                 ttCustArinvoiceslist.gross       = ar-inv.gross
                 ttCustArinvoiceslist.paid        = ar-inv.paid 
                 ttCustArinvoiceslist.baldue      = ar-inv.due 
                 ttCustArinvoiceslist.reckey      = ar-inv.rec_key 
                .
            
      END. /*FOR EACH buff-cust  */
      
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "Search" THEN DO:
     FOR EACH ar-inv WHERE  ar-inv.company = cocode AND (ar-inv.inv-no = prmInv OR prmInv = 0) 
               AND (ar-inv.cust-no = prmCust OR prmCust = "") AND ar-inv.posted = LOGICAL(prmPosted)  AND LOOKUP(ar-inv.cust-no,custcount) <> 0    NO-LOCK :
        CREATE ttCustArinvoiceslist.
           ASSIGN 
                 ttCustArinvoiceslist.invno       = ar-inv.inv-no 
                 ttCustArinvoiceslist.custno      = ar-inv.cust-no
                 ttCustArinvoiceslist.custname    = ar-inv.cust-name
                 ttCustArinvoiceslist.invdate     = string(ar-inv.inv-date)
                 ttCustArinvoiceslist.gross       = ar-inv.gross
                 ttCustArinvoiceslist.paid        = ar-inv.paid 
                 ttCustArinvoiceslist.baldue      = ar-inv.due 
                 ttCustArinvoiceslist.reckey      = ar-inv.rec_key    .
            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/

/********************************Add **********************************/

IF prmAction = "CancelDelete" THEN DO:
    FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ar-inv THEN
        DELETE ar-inv .

END.

IF prmAction = "Addno" THEN DO:

    ASSIGN
    x = 0
    y = 0.

    FIND LAST bf-inv USE-INDEX x-no NO-LOCK NO-ERROR.
    X = IF AVAIL bf-inv THEN bf-inv.x-no + 1 ELSE 1.
    FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
    Y = IF AVAIL ar-ctrl THEN ar-ctrl.last-inv + 1 ELSE 1.

    DO WHILE TRUE:
        FIND FIRST b-ar-inv
            WHERE b-ar-inv.company EQ g_company
            AND b-ar-inv.inv-no  EQ y
            NO-LOCK NO-ERROR.
        FIND FIRST inv-head
            WHERE inv-head.company EQ g_company
            AND inv-head.inv-no  EQ y
            NO-LOCK NO-ERROR.
        IF NOT AVAIL b-ar-inv AND NOT AVAIL inv-head THEN LEAVE.
         y = y + 1.
    END.
 CREATE ar-inv .
    assign
        ar-inv.company  = g_company
        ar-inv.inv-date = today
        ar-inv.x-no     = x 
        ar-inv.inv-no   = y 
        .
    CREATE ttCustArinvoiceslist .
        ASSIGN
            ttCustArinvoiceslist.invno    = ar-inv.inv-no 
            ttCustArinvoiceslist.reckey   = ar-inv.rec_key .
END.

IF prmAction = "ValidateAdd" THEN DO:

     prmCust = CAPS(prmCust).

    IF NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ g_company  AND LOOKUP(prmCust,custcount) <> 0 
                      AND cust.cust-no EQ prmCust
                      AND lookup(cust.active,"A,E") > 0 )        
    THEN DO:
      cError = "Invalid Customer, try help..." .
      RETURN .
    END.

      IF prmTaxcode <> "" then do:
       find first stax where stax.company = g_company and
                          stax.tax-group = prmTaxcode
                          no-lock no-error.
       if not avail stax then do:
          cError = "Invalid Tax Group. Try help. " .
          return .
       end.
    end.   

     FIND FIRST shipto WHERE shipto.company = cocode 
                       AND shipto.cust-no = prmCust
                       AND shipto.ship-id = prmShipid NO-LOCK NO-ERROR.
     IF NOT AVAIL shipto THEN DO:
        cError = "Invalid Ship To. Try Help." .
       RETURN .
     END.
     
     FIND FIRST terms WHERE terms.t-code = prmTerms NO-LOCK NO-ERROR.
     IF NOT AVAIL terms THEN DO:
        cError = "Invalid Terms. Try Help. " .
        RETURN .
     END.
     IF DATE(prmInvdate) > TODAY THEN DO:
        cError = "Invoice Date is Past Today, Continue?" .
        RETURN .
     END.
    

     IF DATE(prmDuedate) LT DATE(prmInvdate) THEN DO:
      cError = TRIM(prmDuedate) + " Due Date may not be before Invoice Date " + TRIM(prmInvdate) + "..." .
      RETURN  .
    END.

    /*FIND FIRST currency WHERE currency.company = cocode 
                          AND currency.c-code = prmCurrcode NO-LOCK NO-ERROR.
    IF AVAIL currency AND prmExrate = 0
         THEN ar-inv.ex-rate:SCREEN-VALUE = string(currency.ex-rate).
     ELSE IF NOT AVAIL currency AND ar-inv.curr-code[1]:SCREEN-VALUE <> ""
     THEN DO:
       MESSAGE "Invalid Currency Code. Try Help. " VIEW-AS ALERT-BOX.
       APPLY "entry" TO ar-inv.curr-code[1].
       RETURN NO-APPLY.
     END.*/

            IF INT(prmInv) LE 0 THEN
                 lv-msg = "may not be zero".

             ELSE
                 IF CAN-FIND(FIRST b-ar-inv
                             WHERE b-ar-inv.company EQ cocode
                             AND b-ar-inv.inv-no  EQ INT(prmInv)
                             AND b-ar-inv.rec_key  NE prmReckey ) THEN
                     lv-msg = "already exists".

                 IF lv-msg EQ "" AND 
                     CAN-FIND(FIRST b-ar-invl
                              WHERE b-ar-invl.company EQ cocode
                              AND b-ar-invl.inv-no  EQ INT(prmInv))
                     THEN  lv-msg = "already exists".

                 IF lv-msg NE "" THEN DO:
                     cError =  STRING(prmInv) +  " " + TRIM(lv-msg) + " ..." .
                     RETURN .
                 END.
       

END.

/********************************************************************/

IF prmAction = "Add" THEN DO:
     FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN
        /*ar-inv.inv-no      =  prmInv*/
        ar-inv.cust-no     =  prmCust
        ar-inv.cust-name   =  prmCustname      
        ar-inv.inv-date    =  DATE(prmInvdate)
        ar-inv.gross       =  prmGross      
        ar-inv.paid        =  prmPaid     
        ar-inv.due         =  prmBaldue     
                                   
        ar-inv.tax-amt     =  prmTaxamt       
        ar-inv.due         =  prmDue      
        ar-inv.ship-id     =  prmShipid      
        ar-inv.po-no       =  prmPono           
        ar-inv.due-date    =  DATE(prmDuedate)
        ar-inv.tax-code    =  prmTaxcode      
        ar-inv.terms       =  prmTerms
        ar-inv.disc-%      =  prmDiscount      
        ar-inv.disc-taken  =  prmDisctaken      
        ar-inv.disc-days   =  prmDiscdays     
        ar-inv.carrier     =  prmCarrier      
        ar-inv.freight     =  prmFreight      
        ar-inv.curr-code[1] = prmCurrcode      
        ar-inv.ex-rate     =  prmExrate  .

    loop:
    REPEAT:
       FIND FIRST ar-ctrl WHERE
            ar-ctrl.company EQ g_company
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

       IF AVAILABLE ar-ctrl THEN
       DO:
           ar-ctrl.last-inv = ar-ctrl.last-inv + 1.
           RELEASE ar-ctrl.
           LEAVE loop.
       END.
    END.

    FIND FIRST ar-invl EXCLUSIVE-LOCK
             WHERE ar-invl.x-no EQ ar-inv.x-no NO-ERROR.
         IF AVAIL ar-invl THEN DO:
             ASSIGN ar-invl.inv-no = ar-inv.inv-no.         
         END.
         RELEASE ar-invl.

         /*{ar/ar-invk.i ar-inv} */

        FOR EACH bARInvl WHERE bARInvl.company EQ ar-inv.company
            AND bARInvl.inv-no EQ ar-inv.inv-no:
            IF bARInvl.po-no NE ar-inv.po-no THEN
                bARInvl.po-no = ar-inv.po-no.
        END. /* each barinvl */
    ASSIGN prmAction = "View" 
           prmInv    = ar-inv.inv-no  .
END.


/**************Update *************************************************/

IF prmAction = "Update" THEN DO:
  FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
    IF ar-inv.posted THEN DO:
     cError =  "Invoice already posted. No Editing allowed!" .
     RETURN .
      END.

    IF prmTaxcode <> "" then do:
       find first stax where stax.company = g_company and
                          stax.tax-group = prmTaxcode
                          no-lock no-error.
       if not avail stax then do:
          cError = "Invalid Tax Group. Try help. " .
          return .
       end.
    end.   

     FIND FIRST shipto WHERE shipto.company = cocode 
                       AND shipto.cust-no = prmCust
                       AND shipto.ship-id = prmShipid NO-LOCK NO-ERROR.
     IF NOT AVAIL shipto THEN DO:
        cError = "Invalid Ship To. Try Help." .
       RETURN .
     END.
     
     FIND FIRST terms WHERE terms.t-code = prmTerms NO-LOCK NO-ERROR.
     IF NOT AVAIL terms THEN DO:
        cError = "Invalid Terms. Try Help. " .
        RETURN .
     END.
     IF DATE(prmInvdate) > TODAY THEN DO:
        cError = "Invoice Date is Past Today, Continue?" .
        RETURN .
     END.
    

     IF DATE(prmDuedate) LT DATE(prmInvdate) THEN DO:
      cError = TRIM(prmDuedate) + " Due Date may not be before Invoice Date " + TRIM(prmInvdate) + "..." .
      RETURN  .
    END.

    /*FIND FIRST currency WHERE currency.company = cocode 
                          AND currency.c-code = prmCurrcode NO-LOCK NO-ERROR.
    IF AVAIL currency AND prmExrate = 0
         THEN ar-inv.ex-rate:SCREEN-VALUE = string(currency.ex-rate).
     ELSE IF NOT AVAIL currency AND ar-inv.curr-code[1]:SCREEN-VALUE <> ""
     THEN DO:
       MESSAGE "Invalid Currency Code. Try Help. " VIEW-AS ALERT-BOX.
       APPLY "entry" TO ar-inv.curr-code[1].
       RETURN NO-APPLY.
     END.*/

     IF (prmOldInv) NE prmInv THEN DO:
       
         IF INT(prmInv) LE 0 THEN
                 lv-msg = "may not be zero".

             ELSE
                 IF CAN-FIND(FIRST b-ar-inv
                             WHERE b-ar-inv.company EQ cocode
                             AND b-ar-inv.inv-no  EQ INT(prmInv)
                             AND ROWID(b-ar-inv)  NE ROWID(ar-inv)) THEN
                     lv-msg = "already exists".

                 IF lv-msg EQ "" AND 
                     CAN-FIND(FIRST b-ar-invl
                              WHERE b-ar-invl.company EQ cocode
                              AND b-ar-invl.inv-no  EQ INT(prmInv))
                     THEN  lv-msg = "already exists".

                 IF lv-msg NE "" THEN DO:
                     cError =  STRING(prmInv) +  " " + TRIM(lv-msg) + " ..." .
                     RETURN .
                 END.
        END.
     

END.

IF prmAction = "Update" THEN DO:
     FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            ar-inv.inv-no      =  prmInv
            ar-inv.cust-name   =  prmCustname      
            ar-inv.inv-date    =  DATE(prmInvdate)
            /*ar-inv.gross       =  prmGross      
            ar-inv.paid        =  prmPaid     
            ar-inv.due         =  prmBaldue
            ar-inv.tax-amt     =  prmTaxamt       
            ar-inv.due         =  prmDue */     
            ar-inv.ship-id     =  prmShipid      
            ar-inv.po-no       =  prmPono           
            ar-inv.due-date    =  DATE(prmDuedate)
            ar-inv.tax-code    =  prmTaxcode      
            ar-inv.terms       =  prmTerms
            ar-inv.disc-%      =  prmDiscount      
            /*ar-inv.disc-taken  =  prmDisctaken      */
            ar-inv.disc-days   =  prmDiscdays         
            ar-inv.carrier     =  prmCarrier      
            ar-inv.freight     =  prmFreight      
            ar-inv.curr-code[1] = prmCurrcode      
              .

         FIND FIRST ar-invl EXCLUSIVE-LOCK
             WHERE ar-invl.x-no EQ ar-inv.x-no NO-ERROR.
         IF AVAIL ar-invl THEN DO:
             ASSIGN ar-invl.inv-no = ar-inv.inv-no.         
         END.
         RELEASE ar-invl.

         {ar/ar-invk.i ar-inv} 

        FOR EACH bARInvl WHERE bARInvl.company EQ ar-inv.company
            AND bARInvl.inv-no EQ ar-inv.inv-no:
            IF bARInvl.po-no NE ar-inv.po-no THEN
                bARInvl.po-no = ar-inv.po-no.
        END. /* each barinvl */
ASSIGN prmAction = "View" .

END.  

/*********************************delete ******************************/

IF prmAction = "ValidateDelete"  THEN DO:
   FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF ar-inv.posted THEN DO:
     cError =  "Invoice already posted. No Deletion allowed!"  .
     RETURN .
    END.

END.

IF prmAction = "DataDelete"  THEN DO:

    FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
   
    FOR EACH ar-invl WHERE ar-invl.company = ar-inv.company AND ar-invl.inv-no = ar-inv.inv-no EXCLUSIVE-LOCK:
        DELETE ar-invl .
    END.
    IF AVAIL ar-inv THEN DELETE ar-inv .

    FIND LAST ar-inv WHERE ar-inv.company = prmComp AND LOOKUP(ar-inv.cust-no,custcount) <> 0  NO-LOCK NO-ERROR.
    IF AVAIL ar-inv THEN
        ASSIGN
        prmInv = ar-inv.inv-no
        prmReckey = ar-inv.rec_key 
        prmAction = "View" .

END.

/*******************************View************************************/


IF prmAction = "View" THEN DO:
     FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.rec_key = prmReckey   NO-LOCK NO-ERROR.
        CREATE ttCustArinvoiceslist.
           ASSIGN 
                 ttCustArinvoiceslist.invno       = ar-inv.inv-no 
                 ttCustArinvoiceslist.custno      = ar-inv.cust-no
                 ttCustArinvoiceslist.custname    = ar-inv.cust-name
                 ttCustArinvoiceslist.invdate     = string(ar-inv.inv-date)
                 ttCustArinvoiceslist.gross       = ar-inv.gross
                 ttCustArinvoiceslist.paid        = ar-inv.paid 
                 ttCustArinvoiceslist.baldue      = ar-inv.due 
               
                 ttCustArinvoiceslist.taxamt      = ar-inv.tax-amt 
                 ttCustArinvoiceslist.due         = ar-inv.due
                 ttCustArinvoiceslist.shipid      = ar-inv.ship-id
                 ttCustArinvoiceslist.pono        = ar-inv.po-no
                 ttCustArinvoiceslist.duedate     = STRING(ar-inv.due-date )
                 ttCustArinvoiceslist.taxcode     = ar-inv.tax-code
                 ttCustArinvoiceslist.terms       = ar-inv.terms 
                 /*ttCustArinvoiceslist.termsdesc   = ar-inv.cust-no*/
                 ttCustArinvoiceslist.discount    = ar-inv.disc-%
                 ttCustArinvoiceslist.disctaken   = ar-inv.disc-taken
                 ttCustArinvoiceslist.discdays    = ar-inv.disc-days
                 ttCustArinvoiceslist.carrier     = ar-inv.carrier 
                 ttCustArinvoiceslist.freight     = ar-inv.freight
                 ttCustArinvoiceslist.currcode    = ar-inv.curr-code[1]
                 ttCustArinvoiceslist.exrate      = ar-inv.ex-rate
                 ttCustArinvoiceslist.reckey      = ar-inv.rec_key
                
                 .

            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ ar-inv.company
                AND shipto.cust-no EQ ar-inv.cust-no
                AND shipto.ship-id EQ ar-inv.ship-id  NO-ERROR.

            IF AVAIL shipto THEN 
                ASSIGN  ttCustArinvoiceslist.shipname  = shipto.ship-name.

            FIND FIRST currency NO-LOCK
                WHERE currency.company EQ ar-inv.company 
                AND currency.c-code  EQ ar-inv.curr-code[1] NO-ERROR.
            IF AVAIL currency THEN
                ASSIGN
                  ttCustArinvoiceslist.exrate = currency.ex-rate.
              FIND FIRST terms WHERE terms.t-code = ar-inv.terms NO-LOCK NO-ERROR.
                IF AVAIL terms THEN
                     ASSIGN ttCustArinvoiceslist.termsdesc = terms.dscr .    

END. /*IF prmAction = "Select" THEN DO:*/

/************************Cutomer Info************************************/

IF prmAction = "CustInfo"  THEN DO:

     FIND FIRST cust
        WHERE cust.company EQ g_company
          AND cust.cust-no EQ prmCust 
        NO-LOCK NO-ERROR.    

     IF AVAIL cust THEN DO:
         CREATE ttCustArinvoiceslist .
         ASSIGN
             ttCustArinvoiceslist.custname = cust.NAME
             ttCustArinvoiceslist.terms  = cust.terms
             ttCustArinvoiceslist.carrier = cust.carrier
             ttCustArinvoiceslist.taxcode = cust.tax-gr
             ttCustArinvoiceslist.currcode = cust.curr-code.
         IF cust.curr-code = "" THEN DO:
             FIND company WHERE company.company = g_company NO-LOCK NO-ERROR.
             IF AVAIL company THEN ttCustArinvoiceslist.currcode = company.curr-code.
         END.

         FIND FIRST shipto WHERE shipto.company = g_company 
             AND shipto.cust-no = cust.cust-no
             NO-LOCK NO-ERROR.
         IF AVAIL shipto THEN
              ASSIGN 
                ttCustArinvoiceslist.shipid = shipto.ship-id
                ttCustArinvoiceslist.shipname = shipto.ship-name.
         FIND currency WHERE currency.company = g_company
             AND currency.c-code = cust.curr-code NO-LOCK NO-ERROR.
         IF AVAIL currency THEN 
             ASSIGN 
               ttCustArinvoiceslist.exrate = currency.ex-rate.

         FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
         IF AVAIL terms THEN 
             ASSIGN 
                ttCustArinvoiceslist.termsdesc = terms.dscr
                ttCustArinvoiceslist.duedate = STRING((TODAY) + terms.net-days)
                 ttCustArinvoiceslist.discount = terms.disc-rate
                  ttCustArinvoiceslist.discdays = terms.disc-days  .
  
  END.


END.

/*****************************procedure**********************************/



