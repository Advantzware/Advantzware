

/*------------------------------------------------------------------------
    File        : voidapchk.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVoidApChecks NO-UNDO
        FIELD vend            AS CHAR
        FIELD chkno           AS INT  
        FIELD bnkcod          AS CHAR   
        FIELD chkdate         AS CHAR 
        FIELD chkamt          AS DEC 
        FIELD manchk          AS CHAR  
        FIELD vendname        AS CHAR
        FIELD bnkname         AS CHAR 
        FIELD voided          AS CHAR 
        FIELD extra           AS CHAR
        FIELD vRecKey         AS CHAR 
        .


DEFINE DATASET dsVoidApChecks FOR ttVoidApChecks .

DEFINE INPUT PARAMETER prmUser          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmvend          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmchkno         AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmbnkcod        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmchkdate       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmchkamt        AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmmanchk        AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmvendname      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmbnkname       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmvoided        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey        AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError          AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVoidApChecks.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
/*DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.*/
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF VAR v-auto-add-tag AS LOG NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR v-next-tag AS cha NO-UNDO.
DEF VAR vuser AS CHAR NO-UNDO.
{sys/inc/var.i "new shared"}


IF prmUser          = ? THEN ASSIGN prmUser        = "".
IF prmAction        = ? THEN ASSIGN prmAction      = "Select".
IF prmvend          = ? THEN ASSIGN prmvend        = "".
IF prmchkno         = ? THEN ASSIGN prmchkno       = 0.  
IF prmbnkcod        = ? THEN ASSIGN prmbnkcod      = "".
IF prmchkdate       = ? THEN ASSIGN prmchkdate     = "".
IF prmchkamt        = ? THEN ASSIGN prmchkamt      = 0.
IF prmmanchk        = ? THEN ASSIGN prmmanchk      = "".
IF prmvendname      = ? THEN ASSIGN prmvendname    = "".
IF prmbnkname       = ? THEN ASSIGN prmbnkname     = "".
IF prmvoided        = ? THEN ASSIGN prmvoided      = "".
IF prmRecKey        = ? THEN ASSIGN prmRecKey      = "".
IF cError           = ? THEN ASSIGN cError         = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp
    g_company = prmComp
    vuser     = prmUser  .

   FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

FUNCTION display-voided RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 IF AVAIL ap-pay AND ap-pay.cleared = ? THEN RETURN TRUE.
 ELSE RETURN FALSE.   /* Function return value. */

END FUNCTION.



 IF prmAction = "GridSelect" THEN DO:

     FOR EACH ap-pay WHERE ap-pay.company = g_company 
          AND ap-pay.reconciled = no 
          AND ap-pay.check-amt <> 0 
          AND ap-pay.check-no < 90000000 NO-LOCK:

         create ttVoidApChecks.
           assign
               ttVoidApChecks.vend          = ap-pay.vend-no
               ttVoidApChecks.chkno         = ap-pay.check-no
               ttVoidApChecks.bnkcod        = ap-pay.bank-code
               ttVoidApChecks.chkdate       = string(ap-pay.check-date)
               ttVoidApChecks.chkamt        = ap-pay.check-amt
               ttVoidApChecks.manchk        = string(ap-pay.man-check)
               ttVoidApChecks.voided        = string(display-voided())
               ttVoidApChecks.vRecKey       = ap-pay.rec_key .



     END.
 
     
 END.  /* end of grid select */


 IF prmAction = "GridSearch" THEN DO:

      FOR EACH ap-pay WHERE ap-pay.company = g_company 
          AND ap-pay.reconciled = no 
          AND ap-pay.check-amt <> 0 
          AND ap-pay.check-no < 90000000
          AND (ap-pay.vend-no BEGINS prmvend OR prmvend = "")
          AND (ap-pay.check-no EQ prmchkno OR prmchkno = 0)
          AND (ap-pay.bank-code BEGINS prmbnkcod OR prmbnkcod = "") NO-LOCK:

        create ttVoidApChecks.
          assign
               ttVoidApChecks.vend          = ap-pay.vend-no
               ttVoidApChecks.chkno         = ap-pay.check-no
               ttVoidApChecks.bnkcod        = ap-pay.bank-code
               ttVoidApChecks.chkdate       = string(ap-pay.check-date)
               ttVoidApChecks.chkamt        = ap-pay.check-amt
               ttVoidApChecks.manchk        = string(ap-pay.man-check)
               ttVoidApChecks.voided        = string(display-voided())  
               ttVoidApChecks.vRecKey       = ap-pay.rec_key .


      END.


END.  /* end of grid select */


IF prmAction = "Update" THEN DO: 
   
   FIND FIRST ap-pay WHERE ap-pay.company = g_company 
          AND ap-pay.rec_key  = prmRecKey 
          AND ap-pay.reconciled = no 
          AND ap-pay.check-amt <> 0 
          AND ap-pay.check-no < 90000000 EXCLUSIVE-LOCK NO-ERROR.      
       
   IF AVAIL ap-pay THEN
       ASSIGN
       ap-pay.vend-no       =  prmvend            
       ap-pay.check-no      =  prmchkno        
       ap-pay.bank-code     =  prmbnkcod       
       ap-pay.check-date    =  date(prmchkdate)  
       ap-pay.check-amt     =  prmchkamt       
       ap-pay.man-check     =  IF prmmanchk = "Yes" THEN YES ELSE NO
       ap-pay.cleared       =  IF prmvoided = "Yes" THEN ? ELSE NO  .



  ASSIGN
          prmAction = "Select" . 
            
END.  /*** update*/  




 IF prmAction = "Select" THEN DO:
  
     FOR EACH ap-pay WHERE ap-pay.company = g_company 
             AND ap-pay.reconciled = no 
             AND ap-pay.rec_key  = prmRecKey 
             AND ap-pay.check-amt <> 0 
             AND ap-pay.check-no < 90000000 NO-LOCK:

           create ttVoidApChecks.
             assign
                  ttVoidApChecks.vend          = ap-pay.vend-no
                  ttVoidApChecks.chkno         = ap-pay.check-no
                  ttVoidApChecks.bnkcod        = ap-pay.bank-code
                  ttVoidApChecks.chkdate       = string(ap-pay.check-date)
                  ttVoidApChecks.chkamt        = ap-pay.check-amt
                  ttVoidApChecks.vRecKey       = ap-pay.rec_key .

             
             ASSIGN
                 ttVoidApChecks.vendname = ""
                 ttVoidApChecks.bnkname = "".

             ttVoidApChecks.voided = IF ap-pay.cleared = ? THEN "YES" ELSE "NO".
             
             RELEASE vend.
             RELEASE ap-dis.

             ttVoidApChecks.vendname = "".

             IF ap-pay.vend-no NE "" THEN
                 FIND FIRST vend
                 WHERE vend.company EQ ap-pay.company
                 AND vend.vend-no EQ ap-pay.vend-no
                 NO-LOCK NO-ERROR.
             IF AVAIL vend THEN ttVoidApChecks.vendname = vend.name.
             
             IF ap-pay.d-no NE 0 THEN
                 FIND FIRST ap-dis WHERE ap-dis.d-no EQ ap-pay.d-no NO-LOCK NO-ERROR.
             IF AVAIL ap-dis THEN
                 ASSIGN
                 ttVoidApChecks.vendname       = ap-dis.payee
                 ttVoidApChecks.vendname       = "Payee".
             
             FIND FIRST bank
                 WHERE bank.company   EQ ap-pay.company
                 AND bank.bank-code EQ ap-pay.bank-code
                 NO-LOCK NO-ERROR.
             IF AVAIL bank THEN ttVoidApChecks.bnkname = bank.bank-name.
     END.

   
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/

