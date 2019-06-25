
/*------------------------------------------------------------------------
    File        : vendinv_list.p
    Purpose     : Vendor
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVendorinvoiceslist NO-UNDO
    FIELD invno       AS CHAR     
    FIELD vendno      AS CHAR         
    FIELD vendname    AS CHAR     
    FIELD invdate     AS CHAR 
    FIELD paid        AS DECIMAL        
    FIELD baldue      AS DECIMAL  
    FIELD taxamt      AS DECIMAL
    FIELD due         AS DECIMAL
    FIELD duedate     AS CHAR
    FIELD taxcode     AS CHAR
    FIELD stats       AS CHAR
    FIELD discount    AS DECIMAL
    FIELD discdays    AS INT
    FIELD freight     AS DECIMAL
    FIELD USR         AS CHAR
    FIELD net         AS DECIMAL
    FIELD mnulchec    AS CHAR
    FIELD overwrtx    AS CHAR
    FIELD currcode    AS CHAR
    FIELD exrate      AS DEC
    FIELD reckey      AS CHAR

    FIELD extra       AS CHAR 
    .

DEFINE DATASET dsVendorinvoiceslist FOR ttVendorinvoiceslist.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvend     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInv      AS CHAR  NO-UNDO.

DEFINE INPUT PARAMETER prmPosted   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmunPosted   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmvendname    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInvdate     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmnet         AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmPaid        AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmBaldue      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmTaxamt      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmDue         AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmDuedate     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTaxcode     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDiscount    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmDiscdays    AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmCurrcode    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmExrate      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmMnlchac     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmTxovrwrt    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmFreight     AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey      AS CHAR NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendorinvoiceslist .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttVendorinvoiceslist:
        DELETE ttVendorinvoiceslist .
    END.

IF prmAction         = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmvend           = ?  THEN ASSIGN prmvend       = "".
IF prmInv            = ?  THEN ASSIGN prmInv        = "".
IF prmPosted         = ?  THEN ASSIGN prmPosted     = "". 
IF prmunPosted       = ?  THEN ASSIGN prmunPosted   = "". 
IF prmvendname       = ?  THEN ASSIGN prmvendname   = "".
IF prmInvdate        = ?  THEN ASSIGN prmInvdate    = "".
IF prmnet            = ?  THEN ASSIGN prmnet        = 0.
IF prmPaid           = ?  THEN ASSIGN prmPaid       = 0.
IF prmBaldue         = ?  THEN ASSIGN prmBaldue     = 0.
IF prmTaxamt         = ?  THEN ASSIGN prmTaxamt     = 0.
IF prmDue            = ?  THEN ASSIGN prmDue        = 0.
IF prmDuedate        = ?  THEN ASSIGN prmDuedate    = "".
IF prmTaxcode        = ?  THEN ASSIGN prmTaxcode    = "".
IF prmDiscount       = ?  THEN ASSIGN prmDiscount   = 0.
IF prmDiscdays       = ?  THEN ASSIGN prmDiscdays   = 0.
IF prmCurrcode       = ?  THEN ASSIGN prmCurrcode   = "".
IF prmExrate         = ?  THEN ASSIGN prmExrate     = 0.
IF prmMnlchac        = ?  THEN ASSIGN prmMnlchac    = "".
IF prmTxovrwrt       = ?  THEN ASSIGN prmTxovrwrt   = "".



DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
  DEF BUFFER bARInvl FOR ap-invl. 
   DEF VAR X AS INT NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.

  
         DEF BUFFER b-ap-inv FOR ap-inv.
         DEF BUFFER b-ap-invl FOR ap-invl.
         DEF BUFFER bf-inv FOR ap-inv.
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
    g_company = prmComp
    g_user    = prmUser  .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .

  {sys/inc/apsecure.i}


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.



IF prmAction = "Search" THEN DO:
    
     FOR EACH ap-inv                                      
        WHERE  ap-inv.recur   EQ NO                        
          AND ap-inv.vend-no BEGINS prmvend               
          AND ap-inv.inv-no  BEGINS string(prmInv)
          AND (ap-inv.inv-date GE date(prmInvdate) OR prmInvdate EQ "")
          AND ((ap-inv.posted EQ YES AND prmPosted = "True") OR    
               (ap-inv.posted EQ NO  AND prmunPosted = "True"))    NO-LOCK :

        CREATE ttVendorinvoiceslist.
           ASSIGN 
                 ttVendorinvoiceslist.invno       = (ap-inv.inv-no )
                 ttVendorinvoiceslist.vendno      = ap-inv.vend-no
                 ttVendorinvoiceslist.invdate     = string(ap-inv.inv-date)
                 ttVendorinvoiceslist.duedate     = string(ap-inv.due-date)
                 ttVendorinvoiceslist.net         = ap-inv.net
                 ttVendorinvoiceslist.paid        = ap-inv.paid 
                 ttVendorinvoiceslist.baldue      = ap-inv.due 
                 ttVendorinvoiceslist.reckey      = ap-inv.rec_key    .
            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "ValidateAdd" THEN DO:

   IF prmExrate = 1 THEN do:

    IF prmInv EQ "" THEN DO:
    cError = "Invoice# must be entered...".
    RETURN.
    END.

    FIND FIRST bf-inv
          WHERE bf-inv.company EQ cocode
            AND bf-inv.vend-no EQ prmvend
            AND bf-inv.inv-no  EQ STRING(prmInv)
          NO-LOCK NO-ERROR.
    IF AVAIL bf-inv THEN DO:
      cError = "Vendor#/Invoice# already entered, re-enter Vendor#?  (NO to re-enter Invoice#)".
      RETURN.
    END.

    IF DATE(prmInvdate) > TODAY THEN DO:
        cError = "Invoice Date is Past Today, Continue?" .
        RETURN .
     END.
    

     IF DATE(prmDuedate) LT DATE(prmInvdate) THEN DO:
      cError = TRIM(prmDuedate) + " Due Date may not be before Invoice Date " + TRIM(prmInvdate) + "..." .
      RETURN  .
    END.  

   END.  /*prmexrate */

    FIND FIRST vend
        WHERE vend.company EQ cocode
        AND vend.vend-no EQ prmvend
        NO-LOCK NO-ERROR.
    IF NOT AVAIL vend                                                      OR
        (vend.active NE "A" AND
         (ap-inv.vend-no NE prmvend)) THEN DO:
        IF AVAIL vend THEN
            cError = TRIM(ap-inv.vend-no) + " not active, try help...".
        ELSE 
            cError = "Invalid " + TRIM(ap-inv.vend-no) + ", try help...".
            RETURN.
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

 END.

/********************************************************************/

IF prmAction = "Add" THEN DO:
     /*FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.*/

    FIND FIRST company WHERE company.company = g_company NO-LOCK NO-ERROR.
  FIND FIRST currency WHERE currency.company = g_company AND
                          currency.c-code = company.curr-code
                          NO-LOCK NO-ERROR.
    CREATE ap-inv .

    ASSIGN
        ap-inv.company      = cocode
        ap-inv.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
        ap-inv.ex-rate      = IF AVAIL currency THEN currency.ex-rate ELSE 0 .

       IF prmExrate = 1 THEN
           ap-inv.recur        = NO.
       ELSE
           ap-inv.recur        = YES.

    ASSIGN
        ap-inv.inv-no      = IF ap-inv.recur THEN STRING(ap-inv.i-no,"9999999999") ELSE  string(prmInv)
        ap-inv.vend-no     =  prmvend
        ap-inv.inv-date    =  DATE(prmInvdate)
        ap-inv.net         =  prmnet      
        ap-inv.paid        =  prmPaid     
        ap-inv.tax-amt     =  prmTaxamt       
        ap-inv.due         =  prmDue
        ap-inv.due-date    =  DATE(prmDuedate)
        ap-inv.tax-gr      =  prmTaxcode      
        
        ap-inv.disc-%      =  prmDiscount  
        ap-inv.disc-days   =  prmDiscdays  
        ap-inv.freight     =  prmFreight
        ap-inv.receiver-no =  prmMnlchac  
        ap-inv.freq        = prmCurrcode 
          .
    IF ap-inv.recur THEN
        ASSIGN ap-inv.inv-no = STRING(ap-inv.i-no,"9999999999") .
    ELSE
        ap-inv.inv-no = string(prmInv) .

    ASSIGN
                ap-inv.user-id  = prmUser .
    FIND FIRST vend
        WHERE vend.company EQ g_company
          AND vend.vend-no EQ ap-inv.vend-no
        NO-LOCK NO-ERROR.
    IF AVAIL vend THEN ap-inv.terms = vend.terms.

    IF(prmTxovrwrt  = "True ") THEN
            RUN update-header (ROWID(ap-inv), YES).
        ELSE
            RUN update-header (ROWID(ap-inv), NO).
    
    ASSIGN prmAction = "View" 
           prmReckey = ap-inv.rec_key.

END.


/**************Update *************************************************/

IF prmAction = "Update" THEN DO:
  FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF ap-inv.posted THEN DO:
        cError =  "Invoice already posted. No Editing allowed!" .
        RETURN .
    END.
    
    IF apsecure-log AND ap-inv.user-id NE prmUser THEN do:
        ASSIGN cError = "This invoice may only be updated by UserID: " + TRIM(ap-inv.user-id) + "..." . 
        RETURN.
    END.

    FIND FIRST vend
        WHERE vend.company EQ cocode
        AND vend.vend-no EQ prmvend
        NO-LOCK NO-ERROR.
    IF NOT AVAIL vend                                                      OR
        (vend.active NE "A" AND
         (ap-inv.vend-no NE prmvend)) THEN DO:
        IF AVAIL vend THEN
            cError = TRIM(ap-inv.vend-no) + " not active, try help...".
        ELSE 
            cError = "Invalid " + TRIM(ap-inv.vend-no) + ", try help...".
            RETURN.
    END.

  IF prmExrate = 1 THEN do:
    
    IF prmInv EQ "" THEN DO:
      cError = "Invoice# must be entered...".
      RETURN.
    END.

    FIND FIRST bf-inv
          WHERE bf-inv.company EQ cocode
            AND bf-inv.vend-no EQ prmvend
            AND bf-inv.inv-no  EQ STRING(prmInv)
            AND ROWID(bf-inv)  NE ROWID(ap-inv)
          NO-LOCK NO-ERROR.
    IF AVAIL bf-inv THEN DO:
      cError = "Vendor#/Invoice# already entered, re-enter Vendor#?  (NO to re-enter Invoice#)".
      RETURN.
    END.
   
     IF DATE(prmInvdate) > TODAY THEN DO:
        cError = "Invoice Date is Past Today, Continue?" .
        RETURN .
     END.
    

     IF DATE(prmDuedate) LT DATE(prmInvdate) THEN DO:
      cError = TRIM(prmDuedate) + " Due Date may not be before Invoice Date " + TRIM(prmInvdate) + "..." .
      RETURN  .
    END.    

  END. /* exrate */

   IF prmTaxcode <> "" then do:
       find first stax where stax.company = g_company and
                          stax.tax-group = prmTaxcode
                          no-lock no-error.
       if not avail stax then do:
          cError = "Invalid Tax Group. Try help. " .
          return .
       end.
    end.   
     

END.

IF prmAction = "Update" THEN DO:
     FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            ap-inv.inv-no      =  string(prmInv)
            ap-inv.vend-no     =  prmvend   
            ap-inv.inv-date    =  DATE(prmInvdate)
            ap-inv.tax-amt     =  prmTaxamt  
            ap-inv.due-date    =  DATE(prmDuedate)
            ap-inv.tax-gr      =  prmTaxcode      
            ap-inv.disc-%      =  prmDiscount      
            ap-inv.disc-days   =  prmDiscdays         
            ap-inv.receiver-no =  prmMnlchac  
            ap-inv.freq        = prmCurrcode  .
            ASSIGN
                ap-inv.user-id  = prmUser .
            

        FIND FIRST vend
        WHERE vend.company EQ g_company
          AND vend.vend-no EQ ap-inv.vend-no
        NO-LOCK NO-ERROR.
        IF AVAIL vend THEN ap-inv.terms = vend.terms.

        IF(prmTxovrwrt  = "True ") THEN
            RUN update-header (ROWID(ap-inv), YES).
        ELSE
            RUN update-header (ROWID(ap-inv), NO).
        
        ASSIGN prmAction = "View" .

END.  

/*********************************delete ******************************/

IF prmAction = "ValidateDelete"  THEN DO:
   FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF ap-inv.posted THEN DO:
     cError =  "Invoice already posted. No Deletion allowed!"  .
     RETURN .
    END.
 IF apsecure-log AND ap-inv.user-id NE prmUser THEN DO:
  cError = "This invoice may only be deleted by UserID: " + TRIM(ap-inv.user-id) + "..." .
  RETURN .
END.

END.

IF prmAction = "DataDelete"  THEN DO:

    FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
   
    IF AVAIL ap-inv THEN DELETE ap-inv .
    IF prmExrate = 1 THEN
        FIND LAST ap-inv WHERE ap-inv.company = prmComp  and ap-inv.posted = NO AND ap-inv.recur = NO NO-LOCK NO-ERROR.
    ELSE
        FIND LAST ap-inv WHERE ap-inv.company = prmComp and ap-inv.recur = yes NO-LOCK NO-ERROR.

    IF AVAIL ap-inv THEN
        ASSIGN
        prmInv = (ap-inv.inv-no)
        prmReckey = ap-inv.rec_key 
        prmAction = "View" .

END.


IF prmAction = "HoldRel" THEN DO:
    
    FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL ap-inv THEN DO:
         ASSIGN
             ap-inv.stat = if ap-inv.stat eq "H" then "R" else "H".    
         END.

         FIND CURRENT ap-inv NO-LOCK NO-ERROR.

      ASSIGN
          prmAction = "View" .

END.


/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
     FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey   NO-LOCK NO-ERROR.
        CREATE ttVendorinvoiceslist.
           ASSIGN 
                 ttVendorinvoiceslist.invno       = ap-inv.inv-no 
                 ttVendorinvoiceslist.vendno      = ap-inv.vend-no
                 /*ttVendorinvoiceslist.vendname    = ap-inv.cust-name*/
                 ttVendorinvoiceslist.invdate     = string(ap-inv.inv-date)
                 ttVendorinvoiceslist.net         = ap-inv.net
                 ttVendorinvoiceslist.paid        = ap-inv.paid 
                 ttVendorinvoiceslist.taxamt      = ap-inv.tax-amt 
                 ttVendorinvoiceslist.due         = ap-inv.due
                 ttVendorinvoiceslist.duedate     = STRING(ap-inv.due-date )
                 ttVendorinvoiceslist.taxcode     = ap-inv.tax-gr
                 ttVendorinvoiceslist.discount    = ap-inv.disc-%
                 ttVendorinvoiceslist.discdays    = ap-inv.disc-days
                 ttVendorinvoiceslist.freight     = ap-inv.freight
                 ttVendorinvoiceslist.stats       = ap-inv.stat
                 ttVendorinvoiceslist.mnulchec    = ap-inv.receiver-no
                 ttVendorinvoiceslist.usr         = ap-inv.USER-ID
                 ttVendorinvoiceslist.reckey      = ap-inv.rec_key  
                 ttVendorinvoiceslist.extra       = ap-inv.freq .

            ASSIGN ttVendorinvoiceslist.currcode = ap-inv.curr-code[1]
                   ttVendorinvoiceslist.exrate = ap-inv.ex-rate.
 

            FIND FIRST vend
                WHERE vend.company EQ g_company
                AND vend.vend-no EQ ap-inv.vend-no
                NO-LOCK NO-ERROR.
            
            IF AVAIL vend THEN ttVendorinvoiceslist.vendname = vend.name.


            
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "AddNewInv" THEN DO:

    FIND LAST bf-inv
      WHERE bf-inv.company EQ g_company
        AND bf-inv.vend-no NE ""
      USE-INDEX i-no NO-LOCK NO-ERROR.

     CREATE ttVendorinvoiceslist.

        IF AVAIL bf-inv THEN ttVendorinvoiceslist.vendno = bf-inv.vend-no.
   
           ASSIGN 
                  ttVendorinvoiceslist.invno       = ""
                  ttVendorinvoiceslist.invdate     = string(TODAY) .
           

            FIND FIRST vend
                WHERE vend.company EQ g_company
                AND vend.vend-no EQ ttVendorinvoiceslist.vendno
                NO-LOCK NO-ERROR.
                 
         IF AVAIL vend THEN DO:
             FIND FIRST terms WHERE terms.t-code EQ vend.terms NO-LOCK NO-ERROR.
             
             ASSIGN
                 ttVendorinvoiceslist.vendname   = vend.name
                 ttVendorinvoiceslist.discount   = (vend.disc-%)
                 ttVendorinvoiceslist.discdays   = (vend.disc-days)
                 ttVendorinvoiceslist.taxcode    = vend.tax-gr.

             IF AVAIL terms THEN
                 ASSIGN
                 ttVendorinvoiceslist.discount  = (terms.disc-rate)
                 ttVendorinvoiceslist.discdays  = (terms.disc-days).
             IF AVAIL terms THEN
                 ttVendorinvoiceslist.duedate  = IF AVAIL terms THEN STRING(terms.net-day + TODAY)
                             ELSE string(TODAY) .
                             
    END.
     
    
END. /* new inv data*/




/*****************************procedure**********************************/



PROCEDURE update-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-overwrite-tax AS LOG NO-UNDO.

  DEF VAR ld AS DEC NO-UNDO.
  DEF VAR v-tax-rate AS DEC NO-UNDO.
  DEF VAR v-frt-tax-rate AS DEC NO-UNDO.

  DEF BUFFER b-ap-inv  FOR ap-inv.
  DEF BUFFER b-ap-invl FOR ap-invl.


  FIND b-ap-inv WHERE ROWID(b-ap-inv) EQ ip-rowid EXCLUSIVE NO-ERROR.

  IF AVAIL b-ap-inv THEN DO:

    IF NOT ip-overwrite-tax THEN
       b-ap-inv.tax-amt = 0.

    ASSIGN
     b-ap-inv.net     = 0
     b-ap-inv.freight = 0.

    IF b-ap-inv.tax-gr NE "" THEN
      RUN ar/cctaxrt.p (b-ap-inv.company, b-ap-inv.tax-gr,
                        OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

    FOR EACH b-ap-invl WHERE b-ap-invl.i-no EQ b-ap-inv.i-no NO-LOCK:
      b-ap-inv.net = b-ap-inv.net + b-ap-invl.amt.

      IF b-ap-invl.tax AND NOT ip-overwrite-tax THEN
        b-ap-inv.tax-amt = b-ap-inv.tax-amt +
                           ROUND((b-ap-invl.amt * v-tax-rate / 100),2).

      IF b-ap-invl.po-no NE 0 THEN DO:
        FIND FIRST po-ordl
            WHERE po-ordl.company EQ cocode
              AND po-ordl.po-no   EQ (IF b-ap-invl.po-no EQ 0 THEN b-ap-inv.po-no
                                                              ELSE b-ap-invl.po-no)
              AND po-ordl.line    EQ (b-ap-invl.line + (b-ap-invl.po-no * 1000 * -1)) 
            USE-INDEX po-no NO-ERROR.

        IF AVAIL po-ordl THEN DO:
          RUN po/getfrtcs.p (ROWID(po-ordl), b-ap-invl.qty, OUTPUT ld).
          b-ap-inv.freight = b-ap-inv.freight + ld.
        END.
      END.
    END.

    ASSIGN
     b-ap-inv.tax-amt = b-ap-inv.tax-amt +
                        ROUND((b-ap-inv.freight * v-frt-tax-rate / 100),2)
     b-ap-inv.net     = b-ap-inv.net + b-ap-inv.tax-amt
     b-ap-inv.due     = b-ap-inv.net - b-ap-inv.disc-taken -
                        b-ap-inv.paid + b-ap-inv.freight.
  END.

  FIND CURRENT b-ap-inv NO-LOCK.

END PROCEDURE.

/*-----------------------------AP Invoice Inquiry(VQ4)-----------------------------*/

IF prmAction = "InqSearch" THEN DO:
    
     FOR EACH ap-inv                                      
        WHERE ap-inv.company EQ cocode 
         AND ap-inv.posted EQ YES                      
          AND ap-inv.vend-no BEGINS prmvend               
          AND ap-inv.inv-no  BEGINS string(prmInv)
          AND (ap-inv.inv-date GE date(prmInvdate) OR prmInvdate EQ "") NO-LOCK,
         EACH ap-invl WHERE ap-invl.i-no eq ap-inv.i-no NO-LOCK BY ap-inv.inv-no DESC:

        CREATE ttVendorinvoiceslist.
           ASSIGN 
                 ttVendorinvoiceslist.invno       = (ap-inv.inv-no )
                 ttVendorinvoiceslist.vendno      = ap-inv.vend-no
                 ttVendorinvoiceslist.invdate     = string(ap-inv.inv-date)
                 ttVendorinvoiceslist.duedate     = string(ap-inv.due-date)
                 ttVendorinvoiceslist.net         = ap-inv.net
                 ttVendorinvoiceslist.discount    = ap-inv.disc-taken
                 ttVendorinvoiceslist.paid        = ap-inv.paid 
                 ttVendorinvoiceslist.baldue      = ap-inv.due 
                 ttVendorinvoiceslist.reckey      = ap-inv.rec_key    .
            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/
