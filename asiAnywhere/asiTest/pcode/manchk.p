
/*------------------------------------------------------------------------
    File        : manchk.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttBManChkList NO-UNDO
    FIELD vendno      AS CHAR
    FIELD bankcode    AS CHAR
    FIELD checkno     AS INT
    FIELD checkdate   AS CHAR
    FIELD checkamt    AS DEC
    FIELD vendname    AS CHAR
    FIELD bankname    AS CHAR
    FIELD reckey      AS CHAR
    FIELD manchk      AS CHAR 
   .

DEFINE DATASET dsBManChkList FOR ttBManChkList.
    

DEFINE INPUT PARAMETER prmAction     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvendno     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmbankcode   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcheckno    AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmcheckdate  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmcheckamt   AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmreckey     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmmanchk     AS CHAR NO-UNDO.
  
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBManChkList .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction         = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmvendno         = ?  THEN ASSIGN prmvendno     = "".
IF prmbankcode       = ?  THEN ASSIGN prmbankcode   = "".
IF prmcheckno        = ?  THEN ASSIGN prmcheckno    = 0. 
IF prmcheckdate      = ?  THEN ASSIGN prmcheckdate  = "". 
IF prmcheckamt       = ?  THEN ASSIGN prmcheckamt   = 0.
IF prmreckey         = ?  THEN ASSIGN prmreckey     = "".
IF prmmanchk         = ?  THEN ASSIGN prmmanchk     = "".



DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF BUFFER bf-chk FOR ap-chk.
DEF BUFFER bf-sel FOR ap-sel.
DEF VAR lv-bank-acct LIKE bank.actnum NO-UNDO.
DEF VAR X AS INT NO-UNDO.
DEF VAR lv-old-check-no   LIKE ap-sel.check-no   NO-UNDO.
DEF VAR lv-old-check-date LIKE ap-sel.check-date NO-UNDO.
DEF VAR lv-old-bank-code LIKE ap-sel.bank-code NO-UNDO.


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

 
IF prmAction = "Search" THEN DO:
    FOR EACH ap-chk WHERE ap-chk.company = g_company and 
        ap-chk.man-check and
        (ap-chk.vend-no BEGINS prmvendno OR prmvendno = "") AND
        (ap-chk.bank-code BEGINS prmbankcode OR prmbankcode = "") AND
        (ap-chk.check-no EQ prmcheckno OR prmcheckno = 0)  NO-LOCK: 
        FIND FIRST ap-sel WHERE ap-sel.company = ap-chk.company 
        AND ap-sel.check-no = ap-chk.check-no  NO-LOCK NO-ERROR.

        CREATE ttBManChkList.
           ASSIGN 
                 ttBManChkList.vendno        = ap-chk.vend-no
                 ttBManChkList.bankcode      = ap-chk.bank-code
                 ttBManChkList.checkno       = ap-chk.check-no
                 ttBManChkList.checkdate     = string(ap-chk.check-date)
                 ttBManChkList.checkamt      = ap-chk.check-amt
                 ttBManChkList.reckey        = ap-chk.rec_key    .
            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "AddNewRecord" THEN DO:

    FOR EACH bf-chk NO-LOCK BY bf-chk.c-no DESCENDING:
      X = bf-chk.c-no.
      LEAVE.
  END.

  /* Dispatch standard ADM method.                             */
  CREATE ap-chk .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST ap-ctrl WHERE ap-ctrl.company = g_company NO-LOCK NO-ERROR.
  FIND FIRST bank WHERE bank.company = g_company
                    AND bank.actnum = ap-ctrl.cash-act NO-LOCK NO-ERROR.

  ASSIGN ap-chk.bank-code = IF AVAIL bank THEN bank.bank-code ELSE ""
         ap-chk.check-no = IF AVAIL bank THEN bank.last-chk + 1 ELSE 0 
         ap-chk.man-check = YES   
         ap-chk.check-date = TODAY
         ap-chk.c-no = X + 1
         ap-chk.company = g_company         
         .
   
   ASSIGN prmAction = "View" 
           prmReckey = ap-chk.rec_key.
    

END. /* add new record */  


/**************Update *************************************************/
IF prmAction = "ValidateAdd" THEN DO:
  
    FIND FIRST vend WHERE vend.company = cocode
                        AND vend.vend-no = prmvendno
                        NO-LOCK NO-ERROR.
  IF NOT AVAIL vend THEN DO:
     ASSIGN cError = "Vendor does not exist. Try Help.. " . 
  END.
  FIND FIRST bank WHERE bank.company = g_company
                       AND bank.bank-code = prmbankcode NO-LOCK NO-ERROR.
  IF AVAIL bank THEN ASSIGN
      lv-bank-acct = bank.actnum.
  IF NOT AVAIL bank THEN DO:
     ASSIGN cError = "Bank Code does not exist. Try Help.. " . 
  END.

  IF prmcheckno <> 0 THEN DO:
        FIND FIRST ap-pay WHERE ap-pay.company = g_company
                        AND ap-pay.check-no = prmcheckno
                        AND (ap-pay.check-act = lv-bank-acct OR 
                             ap-pay.bank-code = prmbankcode)
                        AND ap-pay.posted 
                        NO-LOCK NO-ERROR.
        IF AVAIL ap-pay THEN DO:
           cError = "Check Number " + string(prmcheckno) + " has already been posted." + "Please enter different check number." .
           RETURN .
        END.
     END.

END. /*validate add*/  

IF prmAction = "Update" THEN DO:
  
    FIND FIRST vend WHERE vend.company = cocode
                        AND vend.vend-no = prmvendno
                        NO-LOCK NO-ERROR.
  IF NOT AVAIL vend THEN DO:
     ASSIGN cError = "Vendor does not exist. Try Help.. " . 
  END.
  FIND FIRST bank WHERE bank.company = g_company
                       AND bank.bank-code = prmbankcode NO-LOCK NO-ERROR.
  IF AVAIL bank THEN ASSIGN
      lv-bank-acct = bank.actnum.
  IF NOT AVAIL bank THEN DO:
     ASSIGN cError = "Bank Code does not exist. Try Help.. " . 
  END.

  IF prmcheckno <> 0 THEN DO:
        FIND FIRST ap-pay WHERE ap-pay.company = g_company
                        AND ap-pay.check-no = prmcheckno
                        AND (ap-pay.check-act = lv-bank-acct OR 
                             ap-pay.bank-code = prmbankcode)
                        AND ap-pay.posted 
                        NO-LOCK NO-ERROR.
        IF AVAIL ap-pay THEN DO:
           cError = "Check Number " + string(prmcheckno) + " has already been posted." + "Please enter different check number." .
           RETURN .
        END.
     END.

END.

IF prmAction = "Update" THEN DO:
     

    FIND FIRST ap-chk WHERE  ap-chk.company = g_company and 
       ap-chk.man-check AND ap-chk.rec_key = prmreckey EXCLUSIVE-LOCK NO-ERROR. 
        ASSIGN
            lv-old-check-no   = ap-chk.check-no
            lv-old-check-date = ap-chk.check-date
            lv-old-bank-code  = ap-chk.bank-code .
        
      ASSIGN    
            ap-chk.check-date   =  DATE(prmcheckdate)    
            ap-chk.vend-no      = prmvendno      
            ap-chk.bank-code    = prmbankcode      
            ap-chk.check-no     = prmcheckno      
            ap-chk.check-amt    = prmcheckamt .

        IF lv-old-check-no   NE ap-chk.check-no   OR
            lv-old-check-date NE ap-chk.check-date OR
            lv-old-bank-code NE ap-chk.bank-code  THEN
            FOR EACH ap-sel
            WHERE ap-sel.company   EQ ap-chk.company
            AND ap-sel.vend-no   EQ ap-chk.vend-no
            AND ap-sel.man-check EQ ap-chk.man-check
            AND ap-sel.bank-code EQ lv-old-bank-code
            AND ap-sel.check-no  EQ lv-old-check-no:
            ASSIGN
                ap-sel.check-no = ap-chk.check-no
                ap-sel.pre-date = ap-chk.check-date
                ap-sel.bank-code = ap-chk.bank-code .
            END.

           FIND FIRST bank WHERE bank.company = g_company
               AND bank.bank-code = ap-chk.bank-code EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL bank THEN DO:
               IF ap-chk.check-no > bank.last-chk THEN bank.last-chk = ap-chk.check-no.
               ap-chk.check-act = bank.actnum.
           END.
    
       ASSIGN prmAction = "View" .

END.  

/*********************************delete ******************************/


IF prmAction = "DataDelete"  THEN DO:

 FIND FIRST ap-chk WHERE  ap-chk.company = g_company and 
       ap-chk.man-check AND ap-chk.rec_key = prmreckey EXCLUSIVE-LOCK NO-ERROR. 

    IF AVAIL ap-chk THEN DELETE ap-chk .

 
  FOR EACH ap-chk WHERE ap-chk.company = g_company
          AND ap-chk.man-check NO-LOCK, 
          FIRST ap-sel WHERE ap-sel.company = ap-chk.company 
         AND ap-sel.check-no = ap-chk.check-no  NO-LOCK:
          ASSIGN
             prmReckey = ap-chk.rec_key  .
             LEAVE.
  END.

    prmAction = "View" .

END.


/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
   FOR EACH ap-chk WHERE  ap-chk.company = g_company and 
       ap-chk.man-check AND ap-chk.rec_key = prmreckey NO-LOCK:
       FIND FIRST ap-sel WHERE ap-sel.company = ap-chk.company 
       AND ap-sel.check-no = ap-chk.check-no  NO-LOCK NO-ERROR.
        CREATE ttBManChkList.

           ASSIGN 
                 ttBManChkList.vendno        = ap-chk.vend-no
                 ttBManChkList.bankcode      = ap-chk.bank-code
                 ttBManChkList.checkno       = ap-chk.check-no
                 ttBManChkList.checkdate     = string(ap-chk.check-date)
                 ttBManChkList.checkamt      = ap-chk.check-amt
                 ttBManChkList.reckey        = ap-chk.rec_key    .
           
           FIND FIRST vend WHERE vend.company = g_company
               AND vend.vend-no = ap-chk.vend-no NO-LOCK NO-ERROR.
           FIND FIRST bank WHERE bank.company = g_company
               AND bank.bank-code = ap-chk.bank-code NO-LOCK NO-ERROR.
           ASSIGN
               ttBManChkList.vendname = IF AVAIL vend THEN vend.NAME ELSE ""
               ttBManChkList.bankname = IF AVAIL bank THEN bank.bank-NAME ELSE "".

          
      END.
END. /*IF prmAction = "Select" THEN DO:*/




/*****************************procedure**********************************/

