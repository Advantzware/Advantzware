
/*------------------------------------------------------------------------
    File        : mansel.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttManSalApChkItem NO-UNDO
    FIELD invno       AS CHAR
    FIELD duedate     AS CHAR
    FIELD amtdue      AS DEC
    FIELD amtpaid     AS DEC
    FIELD amtdisc     AS DEC
    FIELD reckey      AS CHAR
    FIELD selno        AS CHAR 
     
   .
  
DEFINE DATASET dsManSalApChkItem FOR ttManSalApChkItem.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prminvno      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmduedate    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmamtdue     AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmamtpaid    AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmamtdisc    AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmreckey     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmchkno      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmapsal     AS CHAR   NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsManSalApChkItem .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction          = ?  THEN ASSIGN prmAction      = "Select".
IF prmComp            = ?  THEN ASSIGN prmComp        = "".
IF prmUser            = ?  THEN ASSIGN prmUser        = "".
IF prminvno           = ?  THEN ASSIGN prminvno       = "".
IF prmduedate         = ?  THEN ASSIGN prmduedate    = "".
IF prmamtdue          = ?  THEN ASSIGN prmamtdue     = 0. 
IF prmamtpaid         = ?  THEN ASSIGN prmamtpaid    = 0. 
IF prmamtdisc         = ?  THEN ASSIGN prmamtdisc    = 0.
IF prmreckey          = ?  THEN ASSIGN prmreckey     = "".
IF prmchkno           = ?  THEN ASSIGN prmchkno      = "".
IF prmapsal           = ?  THEN ASSIGN prmapsal      = "".




DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.

DEF VAR X AS INT NO-UNDO.        
     
 IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  g_company = prmComp
           cocode  = prmComp .



IF prmAction = "GridSelect" THEN DO:

  FIND FIRST ap-chk WHERE ap-chk.rec_key = prmchkno AND 
      ap-chk.company = cocode NO-LOCK NO-ERROR.
    
  FOR EACH ap-sel WHERE ap-sel.company = ap-chk.company 
        AND ap-sel.vend-no = ap-chk.vend-no 
        AND ap-sel.man-check = ap-chk.man-check 
        AND ap-sel.bank-code = ap-chk.bank-code 
        AND ap-sel.check-no = ap-chk.check-no NO-LOCK :
    
        CREATE ttManSalApChkItem.
        ASSIGN
            ttManSalApChkItem.invno   = string(ap-sel.inv-no)
            ttManSalApChkItem.duedate = string(ap-sel.due-date)
            ttManSalApChkItem.amtdue  = ap-sel.inv-bal 
            ttManSalApChkItem.amtpaid = ap-sel.amt-paid
            ttManSalApChkItem.amtdisc = ap-sel.disc-amt
            ttManSalApChkItem.reckey  = ap-sel.rec_key 
            .

       END.
     
END. /*IF prmAction = "Select" THEN DO:*/



 IF prmAction = "UpdateValidate" THEN DO:
     FIND FIRST ap-chk WHERE ap-chk.rec_key = prmchkno AND 
      ap-chk.company = cocode NO-LOCK NO-ERROR.

     FIND FIRST ap-sel WHERE ap-sel.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR. 

    IF prminvno <> "" THEN DO:
     FIND FIRST ap-inv WHERE ap-inv.company = g_company
                        AND ap-inv.vend-no = ap-chk.vend-no
                        AND ap-inv.inv-no = prminvno
                        AND ap-inv.posted = YES
                        AND ap-inv.due <> 0 NO-LOCK NO-ERROR.
     IF NOT AVAIL ap-inv THEN DO:
        cError = "Invalid AP Invoice. Try Help. " .
        RETURN .
     END.
    END.


 END. /* validate update */

 IF prmAction = "Update" THEN DO:
     
     FIND FIRST ap-chk WHERE ap-chk.rec_key = prmchkno AND 
      ap-chk.company = cocode NO-LOCK NO-ERROR.

     FIND FIRST ap-sel WHERE ap-sel.rec_key = prmReckey
         AND ap-sel.company = cocode EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL ap-sel THEN
     ASSIGN
            ap-sel.inv-no           = prminvno    
            ap-sel.due-date         = date(prmduedate)
            ap-sel.inv-bal          = prmamtdue   
            ap-sel.amt-paid         = prmamtpaid  
            ap-sel.disc-amt         = prmamtdisc   .
            
             RUN update-header.

            ASSIGN
                prmAction = "View" .


 END.

 IF prmAction = "AddnewRecValidate" THEN DO:
      FIND FIRST ap-chk WHERE ap-chk.rec_key = prmchkno AND 
      ap-chk.company = cocode NO-LOCK NO-ERROR.

     FIND FIRST ap-sel WHERE ap-sel.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR. 

    IF prminvno <> "" THEN DO:
     FIND FIRST ap-inv WHERE ap-inv.company = g_company
                        AND ap-inv.vend-no = ap-chk.vend-no
                        AND ap-inv.inv-no = prminvno
                        AND ap-inv.posted = YES
                        AND ap-inv.due <> 0 NO-LOCK NO-ERROR.
     IF NOT AVAIL ap-inv THEN DO:
        cError = "Invalid AP Invoice. Try Help. " .
        RETURN .
     END.
    END.

 END. /* validate Add*/

 IF prmAction = "Add" THEN DO:

      FIND FIRST ap-chk WHERE ap-chk.rec_key = prmchkno AND 
      ap-chk.company = cocode NO-LOCK NO-ERROR.

      CREATE ap-sel .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST bank WHERE bank.company = g_company
                    AND bank.bank-code = ap-chk.bank-code NO-LOCK NO-ERROR.
  ASSIGN ap-sel.company = g_company
         ap-sel.vend-no = ap-chk.vend-no
         ap-sel.check-no = ap-chk.check-no
         ap-sel.bank-code = ap-chk.bank-code
         ap-sel.man-check = YES
         ap-sel.pre-date = ap-chk.check-date
         ap-sel.actnum = IF AVAIL bank THEN bank.actnum ELSE "NO Account"
         . 
        ASSIGN
            ap-sel.inv-no           = prminvno    
            ap-sel.due-date         = date(prmduedate)
            ap-sel.inv-bal          = prmamtdue   
            ap-sel.amt-paid         = prmamtpaid  
            ap-sel.disc-amt         = prmamtdisc   
            .
         RUN update-header.

         ASSIGN
                prmReckey = ap-sel.rec_key
                prmAction = "View" .

 END. /*addd*/ 


 /*IF prmAction = "Delete" THEN DO:

    FIND FIRST ap-chk WHERE ap-chk.rec_key = prmchkno AND 
      ap-chk.company = cocode NO-LOCK NO-ERROR.

     IF ap-pay.posted THEN do:
     cError = "This Cash Receipt has been posted. No deletion allowed!" .
     RETURN.
  END.
       
 END. /*validate delete */ */


 IF prmAction = "Delete" THEN DO:
     FIND FIRST ap-chk WHERE ap-chk.rec_key = prmchkno AND 
      ap-chk.company = cocode NO-LOCK NO-ERROR.

     FIND FIRST ap-sel WHERE ap-sel.rec_key = prmReckey
         AND ap-sel.company = cocode EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL ap-sel THEN
         DELETE ap-sel.

      RUN update-header.

    FIND LAST ap-sel WHERE ap-sel.company = ap-chk.company 
        AND ap-sel.vend-no = ap-chk.vend-no 
        AND ap-sel.man-check = ap-chk.man-check 
        AND ap-sel.bank-code = ap-chk.bank-code 
        AND ap-sel.check-no = ap-chk.check-no NO-LOCK NO-ERROR.
            
   IF AVAIL ap-sel THEN
         ASSIGN
         prmAction = "View"
         prmReckey = ap-sel.rec_key.

 END. /*delete */

  
 IF prmAction = "View" THEN DO:
     
     FIND FIRST ap-sel WHERE ap-sel.rec_key = prmReckey
         AND ap-sel.company = cocode NO-LOCK NO-ERROR.
     IF AVAIL ap-sel  THEN DO:
      CREATE ttManSalApChkItem.
        ASSIGN
            ttManSalApChkItem.invno   = string(ap-sel.inv-no)
            ttManSalApChkItem.duedate = string(ap-sel.due-date)
            ttManSalApChkItem.amtdue  = ap-sel.inv-bal 
            ttManSalApChkItem.amtpaid = ap-sel.amt-paid
            ttManSalApChkItem.amtdisc = ap-sel.disc-amt
            ttManSalApChkItem.reckey  = ap-sel.rec_key 
             . 
     END.
 END.



 PROCEDURE update-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR ld-tmp-amt AS DEC NO-UNDO.

   DEF BUFFER bf-sel FOR ap-sel.


   ld-tmp-amt = 0.
   FOR EACH bf-sel WHERE bf-sel.company = ap-chk.company
                    AND bf-sel.vend-no = ap-chk.vend-no
                    AND bf-sel.man-check = ap-chk.man-check
                    AND bf-sel.bank-code = ap-chk.bank-code
                    AND bf-sel.check-no = ap-chk.check-no NO-LOCK:
         ld-tmp-amt = ld-tmp-amt + bf-sel.amt-paid.
   END.
   
   FIND CURRENT ap-chk EXCLUSIVE-LOCK.
   ap-chk.check-amt = ld-tmp-amt.
   FIND CURRENT ap-chk NO-LOCK.

END PROCEDURE.
