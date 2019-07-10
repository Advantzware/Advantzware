
/*------------------------------------------------------------------------
    File        : listdisbr.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttDisbursements NO-UNDO
    FIELD vend        AS CHAR
    FIELD vname       AS CHAR
    FIELD checkno     AS INT
    FIELD checkdate   AS CHAR
    FIELD payee       AS CHAR
    FIELD checkamt    AS DEC
    FIELD bnkcod      AS CHAR
    FIELD bnkname     AS CHAR
    FIELD currcod     AS CHAR
    FIELD exrat       AS DEC

    FIELD reckey      AS CHAR
    FIELD extra       AS CHAR 
    .

DEFINE DATASET dsDisbursements FOR ttDisbursements.
    

DEFINE INPUT PARAMETER prmAction    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmvend      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmvname     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmcheckno   AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmcheckdate AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmpayee     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmcheckamt  AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmbnkcod    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmbnkname   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmcurrcod   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmexrat     AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey    AS CHAR NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDisbursements .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.
DEF BUFFER b-ap-dis FOR ap-dis.
DEF BUFFER bf-dis FOR ap-dis.     

IF prmAction         = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmvend           = ?  THEN ASSIGN prmvend       = "".
IF prmvname          = ?  THEN ASSIGN prmvname      = "".
IF prmcheckno        = ?  THEN ASSIGN prmcheckno    = 0. 
IF prmcheckdate      = ?  THEN ASSIGN prmcheckdate  = "". 
IF prmpayee          = ?  THEN ASSIGN prmpayee      = "".
IF prmcheckamt       = ?  THEN ASSIGN prmcheckamt   = 0.
IF prmbnkcod         = ?  THEN ASSIGN prmbnkcod     = "".
IF prmbnkname        = ?  THEN ASSIGN prmbnkname    = "". 
IF prmcurrcod        = ?  THEN ASSIGN prmcurrcod    = "". 
IF prmexrat          = ?  THEN ASSIGN prmexrat      = 0.
IF prmReckey         = ?  THEN ASSIGN prmReckey     = "".




DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
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
    
     FOR EACH ap-dis WHERE ap-dis.company = prmComp
         AND ap-dis.posted = no 
         AND (ap-dis.check-no = prmcheckno OR prmcheckno = 0)
         AND (ap-dis.vend-no BEGINS prmvend OR prmvend = "") NO-LOCK:
         

        CREATE ttDisbursements.
           ASSIGN 
                 ttDisbursements.vend          = ap-dis.vend-no
                 ttDisbursements.bnkcod        = ap-dis.bank-code
                 ttDisbursements.checkno       = ap-dis.check-no
                 ttDisbursements.checkdate     = string(ap-dis.check-date)
                 ttDisbursements.payee         = ap-dis.payee
                 ttDisbursements.checkamt      = ap-dis.check-amt
                 ttDisbursements.reckey        = ap-dis.rec_key    .
            
    END. /*FOR EACH ap-dis  */
END. /*IF prmAction = "Search" THEN DO:*/


IF prmAction = "ValidateAdd" THEN DO:

    FIND FIRST vend WHERE vend.company = cocode
                        AND vend.vend-no = prmvend
                        NO-LOCK NO-ERROR.
  IF NOT AVAIL vend THEN DO:
     ASSIGN cError = "Vendor does not exist. Try Help.." . 
     RETURN.
  END.

  FIND FIRST bank WHERE bank.company = cocode AND                         
                         bank.bank-code = prmbnkcod
                         NO-LOCK NO-ERROR.
    IF NOT AVAIL bank THEN DO:
         cError = "Invalid bankor. Try Help.".
         RETURN.
    END.

    IF CAN-FIND(FIRST b-ap-dis
                WHERE b-ap-dis.company   EQ ap-dis.company
                  AND b-ap-dis.bank-code EQ prmbnkcod
                  AND b-ap-dis.check-no  EQ INT(prmcheckno)
                  AND ROWID(b-ap-dis)    NE ROWID(ap-dis))               OR
       CAN-FIND(FIRST ap-pay
                WHERE ap-pay.company   EQ ap-dis.company
                  AND ap-pay.bank-code EQ prmbnkcod
                  AND ap-pay.check-no  EQ INT(prmcheckno)) OR
       INT(prmcheckno) EQ 0                                THEN DO:

        IF prmcheckno = 0 THEN do: 
            ASSIGN cError = "Check# may not be zero".
            RETURN.
        END.
        ELSE do: 
            ASSIGN cError = "Check# already exists, please re-enter".
            RETURN.
        END.
    END.

   
 END.

/********************************************************************/

IF prmAction = "Add" THEN DO:
    
   FIND FIRST ap-dis WHERE ap-dis.company = g_company
          AND ap-dis.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
    
    ASSIGN
       ap-dis.vend-no              =  prmvend               
       ap-dis.bank-code            =  prmbnkcod
       ap-dis.check-no             =  prmcheckno         
       ap-dis.check-date           =  DATE(prmcheckdate)
       ap-dis.payee                =  prmpayee  
       ap-dis.check-amt            =  prmcheckamt  
       ap-dis.curr-code[1]         =  prmcurrcod
       ap-dis.ex-rate              =  prmexrat .       


   ASSIGN prmAction = "View"  .
           

END.

IF prmAction = "AddNewRecord" THEN DO:

   DEF VAR X AS INT NO-UNDO.

  
  FOR EACH bf-dis NO-LOCK BY d-no DESCENDING:
      X = bf-dis.d-no.
      LEAVE.
  END.
  
 find first ap-ctrl where ap-ctrl.company = g_company no-lock no-error.
 find first bank where bank.company = g_company and
                      bank.actnum  = ap-ctrl.cash-act no-error.
 FIND FIRST company WHERE company.company = g_company NO-LOCK NO-ERROR.
 FIND FIRST currency WHERE currency.company = g_company AND
                           currency.c-code = company.curr-code
                           NO-LOCK NO-ERROR.
      CREATE ap-dis.
 assign ap-dis.company = g_company
        ap-dis.check-no = IF AVAIL bank THEN bank.last-chk + 1 ELSE 0
        ap-dis.d-no = x + 1
        ap-dis.bank-code = IF AVAIL bank THEN bank.bank-code ELSE ""
        ap-dis.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
        ap-dis.ex-rate = IF AVAIL currency THEN currency.ex-rate ELSE 0
        ap-dis.check-date = TODAY
        ap-dis.posted = NO .

 IF AVAIL bank THEN bank.last-chk = ap-dis.check-no.

   
   ASSIGN prmAction = "View" 
           prmReckey = ap-dis.rec_key.
    

END. /* add new record */  


/**************Update *************************************************/

IF prmAction = "Update" THEN DO:
  
    FIND FIRST vend WHERE vend.company = cocode
        AND vend.vend-no = prmvend
        NO-LOCK NO-ERROR.

    IF NOT AVAIL vend THEN DO:
        ASSIGN cError = "Vendor does not exist. Try Help.." . 
        RETURN.
    END.
    
    FIND FIRST bank WHERE bank.company = cocode AND                         
        bank.bank-code = prmbnkcod
        NO-LOCK NO-ERROR.
    IF NOT AVAIL bank THEN DO:
        cError = "Invalid bankor. Try Help.".
        RETURN.
    END.

    IF CAN-FIND(FIRST b-ap-dis
                WHERE b-ap-dis.company   EQ ap-dis.company
                  AND b-ap-dis.bank-code EQ prmbnkcod
                  AND b-ap-dis.check-no  EQ INT(prmcheckno)
                  AND ROWID(b-ap-dis)    NE ROWID(ap-dis))               OR
       CAN-FIND(FIRST ap-pay
                WHERE ap-pay.company   EQ ap-dis.company
                  AND ap-pay.bank-code EQ prmbnkcod
                  AND ap-pay.check-no  EQ INT(prmcheckno)) OR
       INT(prmcheckno) EQ 0                                THEN DO:

       IF prmcheckno = 0 THEN do: 
            ASSIGN cError = "Check# may not be zero".
            RETURN.
        END.
        ELSE do: 
            ASSIGN cError = "Check# already exists, please re-enter".
            RETURN.
        END.
      
      RETURN.
    END.
  

END.

IF prmAction = "Update" THEN DO:
      FIND FIRST ap-dis WHERE ap-dis.company = g_company
          AND ap-dis.rec_key = prmReckey
          AND ap-dis.posted = NO EXCLUSIVE-LOCK NO-ERROR. 

        ASSIGN    
            ap-dis.vend-no              =  prmvend 
            ap-dis.bank-code            =  prmbnkcod
            ap-dis.check-no             =  prmcheckno         
            ap-dis.check-date           =  DATE(prmcheckdate)
            ap-dis.payee                =  prmpayee  
            ap-dis.check-amt            =  prmcheckamt  
            ap-dis.curr-code[1]         =  prmcurrcod
            ap-dis.ex-rate              =  prmexrat . 

          
        
    
       ASSIGN prmAction = "View" .

END.  

/*********************************delete ******************************/


IF prmAction = "DataDelete"  THEN DO:

   FIND FIRST ap-dis WHERE ap-dis.company = g_company
          AND ap-dis.rec_key = prmReckey
          AND ap-dis.posted = NO EXCLUSIVE-LOCK NO-ERROR. 
         
   
    IF AVAIL ap-dis THEN DELETE ap-dis .

 
  FOR EACH ap-dis WHERE ap-dis.company = prmComp
         AND ap-dis.posted = no NO-LOCK:
          ASSIGN
        prmReckey = ap-dis.rec_key  .
          LEAVE.
  END.

    prmAction = "View" .

END.


/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
      FOR EACH ap-dis WHERE ap-dis.company = g_company
          AND ap-dis.rec_key = prmReckey
          AND ap-dis.posted = NO NO-LOCK:
          
     
        CREATE ttDisbursements.

           ASSIGN 
                 ttDisbursements.vend          = ap-dis.vend-no
                 ttDisbursements.checkno       = ap-dis.check-no
                 ttDisbursements.checkdate     = STRING(ap-dis.check-date)
                 ttDisbursements.payee         = ap-dis.payee
                 ttDisbursements.checkamt      = ap-dis.check-amt
                 ttDisbursements.bnkcod        = ap-dis.bank-code
                 
                 ttDisbursements.currcod       = ap-dis.curr-code[1]
                 ttDisbursements.exrat         = ap-dis.ex-rate 
                 ttDisbursements.reckey        = ap-dis.rec_key    .

        
        FIND FIRST vend WHERE vend.company = g_company
               AND vend.vend-no = ap-dis.vend-no EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL vend  THEN
            ASSIGN
            ttDisbursements.vname         = vend.NAME .

        FIND FIRST bank WHERE bank.company = g_company
               AND bank.bank-code = ap-dis.bank-code EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL bank THEN DO:
               ASSIGN 
                   ttDisbursements.bnkname       = bank.bank-name.
           END. 

      END.
END. /*IF prmAction = "Select" THEN DO:*/




/*****************************procedure**********************************/

