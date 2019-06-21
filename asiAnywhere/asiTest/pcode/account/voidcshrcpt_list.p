
/*------------------------------------------------------------------------
    File        : voidcshrcpt_list.p
    Purpose     : Account Receivable
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVoidCashReceiptList NO-UNDO
    FIELD chkno       AS INT
    FIELD custno      AS CHAR         
    FIELD custname    AS CHAR     
    FIELD custdt      AS CHAR 
    FIELD bnk         AS CHAR  
    FIELD bnknam      AS CHAR
    FIELD voided      AS CHAR
    FIELD amt         AS DEC
    FIELD ext         AS CHAR
    FIELD reckey      AS CHAR
    .

DEFINE DATASET dsVoidCashReceiptList FOR ttVoidCashReceiptList.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmchkno       AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmcustno      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmcustname    AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmcustdt      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmbnk         AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmbnknam      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmvoided      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmamt         AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmOut         AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER prmreckey      AS CHAR     NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVoidCashReceiptList .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttVoidCashReceiptList:
        DELETE ttVoidCashReceiptList .
    END.

IF prmAction        = ?  THEN ASSIGN prmAction    = "Select".
IF prmComp          = ?  THEN ASSIGN prmComp      = "".
IF prmUser          = ?  THEN ASSIGN prmUser      = "".
IF prmchkno         = ?  THEN ASSIGN prmchkno     = 0.
IF prmcustno        = ?  THEN ASSIGN prmcustno    = "".
IF prmcustname      = ?  THEN ASSIGN prmcustname  = "". 
IF prmcustdt        = ?  THEN ASSIGN prmcustdt    = "". 
IF prmbnk           = ?  THEN ASSIGN prmbnk       = "".
IF prmbnknam        = ?  THEN ASSIGN prmbnknam    = "".
IF prmvoided        = ?  THEN ASSIGN prmvoided    = "".
IF prmamt           = ?  THEN ASSIGN prmamt       = 0.
IF prmOut           = ?  THEN ASSIGN prmOut       = "".




DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
  DEF BUFFER bARInvl FOR ap-invl. 
   DEF VAR X AS INT NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.
DEF BUFFER bf-cashl FOR ar-cashl.
  
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

DEF VAR v-c-no AS INT NO-UNDO.


IF prmAction = "getdata" THEN DO:
    
     FIND FIRST ar-cash WHERE ar-cash.company = prmComp
         AND ar-cash.check-no = prmchkno
         AND ar-cash.cust-no = prmcustno
         AND ar-cash.posted EQ YES EXCLUSIVE-LOCK NO-ERROR.
    
     IF AVAIL ar-cash THEN do:
        ASSIGN prmAction = "View".
    END.
    ELSE do: 
        ASSIGN 
            cError = "Invalid Check".
             RETURN.
    END.
            
    
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "Save" THEN DO:
    
    FIND FIRST ar-cash WHERE ar-cash.company = cocode 
        AND ar-cash.check-no = prmchkno
         AND ar-cash.cust-no = prmcustno
         AND ar-cash.posted EQ YES EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL ar-cash THEN DO:
       
         IF prmvoided EQ "Yes" THEN DO:
            FOR EACH ar-cashl WHERE ar-cashl.company = ar-cash.company
                                AND ar-cashl.c-no = ar-cash.c-no:
               IF ar-cashl.inv-no = 0 THEN DO:
                  cError = "Must be applied before voiding receipt. " .
               END.
            END.
         END.
          
         ar-cash.cleared = (IF prmvoided = "Yes" THEN ? ELSE NO). 
         RELEASE ar-cash.
      END.
      ELSE do:
          ASSIGN 
              cError = "Check not available for updating." .
          RETURN.
      END.

END.


       
/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
    v-c-no = 0.

     FOR EACH ar-cash WHERE ar-cash.company = prmComp
         AND ar-cash.check-no = prmchkno
         AND ar-cash.cust-no = prmcustno
         AND ar-cash.posted EQ YES NO-LOCK:
        

        CREATE ttVoidCashReceiptList.
           ASSIGN 
                 ttVoidCashReceiptList.chkno          =  ar-cash.check-no 
                 ttVoidCashReceiptList.custno         =  ar-cash.cust-no
                 ttVoidCashReceiptList.custdt         =  STRING(ar-cash.check-date) 
                 ttVoidCashReceiptList.bnk            =  ar-cash.bank-code
                 ttVoidCashReceiptList.amt            = ar-cash.check-amt
                 ttVoidCashReceiptList.reckey         = ar-cash.rec_key .

           
               ttVoidCashReceiptList.voided = IF ar-cash.cleared = ? THEN "Yes" ELSE "No".

           
           IF AVAIL ar-cash THEN DO:
               FIND FIRST cust WHERE  cust.company = ar-cash.company
                   AND cust.cust-no = ar-cash.cust-no NO-LOCK NO-ERROR.
               IF AVAIL cust THEN 
                   ASSIGN 
                   ttVoidCashReceiptList.custname = cust.NAME .
           

           FIND FIRST bank WHERE
             bank.company   EQ cocode AND
             bank.bank-code EQ ar-cash.bank-code
             NO-LOCK NO-ERROR.

           IF AVAIL bank THEN 
               ttVoidCashReceiptList.bnknam = bank.bank-name.

           ASSIGN v-c-no = ar-cash.c-no.

           END.
           
     END.
END. /*IF prmAction = "Select" THEN DO:*/
