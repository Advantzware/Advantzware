
/*------------------------------------------------------------------------
    File        : currency.p
    Purpose     : Customer

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttcurrencylistval NO-UNDO
    FIELD   c-code          AS CHAR
    FIELD   c-desc          AS CHAR
    FIELD   country         AS CHAR 
    FIELD   c-num           AS CHAR
    FIELD   minor-unit      AS CHAR 
    FIELD   is-base         AS CHAR
    FIELD   ex-rate         AS CHAR
    FIELD   ar-ast-acct     AS CHAR
    FIELD   reckey          AS CHAR .

     
DEFINE DATASET dscurrencylistval FOR ttcurrencylistval.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCcode    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCdesc    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCountry  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCnum     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prMinorunit AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmIsbase   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmExrate   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmArastacct AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmReckey    AS CHAR NO-UNDO. 



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dscurrencylistval.
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.

     FOR EACH ttcurrencylistval:
        DELETE ttcurrencylistval .
    END.

     IF prmAction      = ?  THEN ASSIGN prmAction        = "Search".
     IF prmComp        = ?  THEN ASSIGN prmComp          = "".
     IF prmUser        = ?  THEN ASSIGN prmUser          = "".
     IF prmCcode       = ?  THEN ASSIGN prmCcode         = "".
     IF prmCdesc       = ?  THEN ASSIGN prmCdesc         = "0".
     IF prmCountry     = ?  THEN ASSIGN prmCountry       = "0".
     IF prmCnum        = ?  THEN ASSIGN prmCnum          = "".
     IF prMinorunit    = ?  THEN ASSIGN prMinorunit      = "".
     IF prmIsbase      = ?  THEN ASSIGN prmIsbase        = "".
     IF prmExrate      = ?  THEN ASSIGN prmExrate        = "".
     IF prmArastacct   = ?  THEN ASSIGN prmArastacct     = "".
     IF prmReckey     = ?  THEN ASSIGN prmReckey       = "".




     
{sys/inc/var.i NEW SHARED}
    DEF BUFFER bf-currency FOR currency.


     
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
   
     ASSIGN
         cocode = prmComp .

       

     IF prmAction = "Search" THEN DO:
         FOR EACH currency WHERE  currency.company = prmComp NO-LOCK :
             CREATE ttcurrencylistval.
             ASSIGN 
                 ttcurrencylistval.c-code        = currency.c-code 
                 ttcurrencylistval.c-desc        = currency.c-desc
                 ttcurrencylistval.country       = currency.country
                 ttcurrencylistval.c-num         = string(currency.c-num)
                 ttcurrencylistval.minor-unit    = currency.minor-unit
                 ttcurrencylistval.is-base       = string(currency.is-base)
                 ttcurrencylistval.ex-rate       = string(currency.ex-rate)
                 ttcurrencylistval.ar-ast-acct   = currency.ar-ast-acct
                 ttcurrencylistval.reckey        = currency.rec_key .

      END. /*FOR EACH buff-cust  */
 END. /*IF prmAction = "Search" THEN DO:*/

 IF prmAction = "AddNewRec" THEN DO:

     FIND FIRST currency WHERE currency.company = cocode AND
                                currency.c-code = prmCcode NO-LOCK NO-ERROR.
     IF AVAIL currency THEN DO:
         cError = "Currency already exists with Company " + cocode + " Code " + prmCcode .
         RETURN.
     END.
 END. /* end of  validate*/

 IF prmAction = "AddNewRec" THEN DO:
     
     CREATE currency .
     ASSIGN 
         currency.company       = cocode
         currency.c-code        = prmCcode    
         currency.c-desc        = prmCdesc    
         currency.country       = prmCountry  
         currency.c-num         = int(prmCnum)
         currency.minor-unit    = prMinorunit 
         currency.ex-rate       = decimal(prmExrate)
         currency.ar-ast-acct   = prmArastacct
         currency.is-base       = IF prmIsbase = "True" THEN TRUE ELSE FALSE
         .

    ASSIGN
        prmAction = "View"
        prmReckey = currency.rec_key .


 END.  /* add new rec*/

 IF prmAction = "UpdateRec" THEN DO:
    FIND FIRST currency WHERE  currency.company = prmComp 
        AND currency.rec_key = prmReckey NO-LOCK NO-ERROR .

   /*  FIND FIRST bf-currency WHERE bf-currency.company = cocode AND
                                bf-currency.c-code = prmCcode AND
                                ROWID(bf-currency) NE ROWID(currency)  NO-LOCK NO-ERROR.
     IF AVAIL bf-currency THEN DO:
         cError = "Currency already exists with Company " + cocode + " Code " + prmCcode .
         RETURN.
     END.*/

 END. /* end of validate update */

 IF prmAction = "Update"  THEN DO:
    MESSAGE "test 123213 " .
      FIND FIRST currency WHERE  currency.company = prmComp 
        AND currency.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR .

      MESSAGE "test 22222 " .
             CREATE ttcurrencylistval.
             MESSAGE "test 3333 " .
             IF AVAIL currency THEN
                 ASSIGN 
                 currency.c-desc        = prmCdesc    
                 currency.country       = prmCountry  
                 currency.c-num         = int(prmCnum)
                 currency.minor-unit    = prMinorunit 
                 currency.ex-rate       = decimal(prmExrate)
                 currency.ar-ast-acct   = prmArastacct
                 currency.is-base       = IF prmIsbase = "True" THEN TRUE ELSE FALSE
                     .
                   
   ASSIGN
       prmAction = "View".

   

 END. /* end of update */


 IF prmAction = "DeleteRec" THEN DO:

    FIND FIRST currency WHERE  currency.company = prmComp 
        AND currency.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR .

   IF AVAIL currency THEN
         DELETE currency .

     FIND LAST currency WHERE currency.company = prmComp  NO-LOCK NO-ERROR.
     IF AVAIL currency THEN
     ASSIGN 
         prmAction = "View"
         prmReckey =  currency.rec_key .

 END.




IF prmAction = "View" THEN DO:
   
    FIND FIRST currency WHERE  currency.company = prmComp 
        AND currency.rec_key = prmReckey NO-LOCK NO-ERROR .
             CREATE ttcurrencylistval.
             IF AVAIL currency THEN
                 ASSIGN 
                 ttcurrencylistval.c-code        = currency.c-code 
                 ttcurrencylistval.c-desc        = currency.c-desc
                 ttcurrencylistval.country       = currency.country
                 ttcurrencylistval.c-num         = string(currency.c-num)
                 ttcurrencylistval.minor-unit    = currency.minor-unit
                 ttcurrencylistval.is-base       = string(currency.is-base)
                 ttcurrencylistval.ex-rate       = string(currency.ex-rate)
                 ttcurrencylistval.ar-ast-acct   = currency.ar-ast-acct
                 ttcurrencylistval.reckey        = currency.rec_key     .
          
    
 END. /*IF prmAction = "View" THEN DO:*/




