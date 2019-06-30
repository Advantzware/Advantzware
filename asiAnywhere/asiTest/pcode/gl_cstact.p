

/*------------------------------------------------------------------------
    File        : gl_cstact.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttGLDistributedAccount NO-UNDO
    FIELD act            AS CHAR
    FIELD actdscr        AS CHAR
    FIELD cst_act        AS CHAR
    FIELD cst_dscr       AS CHAR
    FIELD c_rate         AS DEC
    FIELD actype         AS CHAR
    FIELD cst_ext        AS CHAR
    FIELD RecKey         AS CHAR
    
        .

DEFINE DATASET dsGLDistributedAccount FOR ttGLDistributedAccount .

DEFINE INPUT PARAMETER prmUser         AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmAction       AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmact          AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmactdscr      AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmcst_act      AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmcst_dscr     AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmc_rate      AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmactype       AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey       AS CHAR         NO-UNDO.

DEF OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGLDistributedAccount.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.

DEF BUFFER b-acctcost FOR acctcost.
DEF BUFFER b-account FOR account.

DEF VAR lv-tot-rate AS DEC NO-UNDO.  /* total distribute rate */
DEF VAR lv-actdscr LIKE account.dscr NO-UNDO.


IF prmUser         = ? THEN ASSIGN prmUser      = "".
IF prmAction       = ? THEN ASSIGN prmAction    = "Select".
IF prmact          = ? THEN ASSIGN prmact       = "".
IF prmactdscr      = ? THEN ASSIGN prmactdscr   = "".
IF prmc_rate       = ? THEN ASSIGN prmc_rate   = 0.
IF prmactype       = ? THEN ASSIGN prmactype    = "".
IF prmRecKey       = ? THEN ASSIGN prmRecKey    = "".




DEF NEW SHARED VAR v-basis-w AS DEC NO-UNDO. 
DEF NEW SHARED VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF NEW SHARED VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
DEF NEW SHARED VAR v-dep LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-tot-msf AS DEC NO-UNDO.
def NEW shared var factor# as decimal no-undo.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp .


FUNCTION get-act-dscr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL acctcost AND acctcost.costacct <> "" THEN
     FIND FIRST b-account WHERE b-account.company = acctcost.company
                          AND b-account.actnum = acctcost.costacct NO-LOCK NO-ERROR.
  ELSE
  FIND FIRST b-account WHERE b-account.company = account.company
                          AND b-account.actnum = prmcst_act
                          NO-LOCK NO-ERROR.

  IF AVAILABLE b-account THEN RETURN b-account.dscr.
  ELSE RETURN "".   /* Function return value. */
   


END FUNCTION.

IF prmAction = "Search" THEN DO:

    FIND FIRST account WHERE account.company = prmComp                 
         AND account.rec_key = prmRecKey NO-LOCK NO-ERROR.  
    
     FOR EACH acctcost WHERE acctcost.company = account.company         
         AND acctcost.actnum = account.actnum NO-LOCK:                  
            create ttGLDistributedAccount.                             
             assign                                                     
                 ttGLDistributedAccount.cst_act     = acctcost.costacct 
                 ttGLDistributedAccount.cst_dscr    = get-act-dscr()    
                 ttGLDistributedAccount.c_rate      = acctcost.c-rate   
                 ttGLDistributedAccount.actype      = acctcost.type 
                 ttGLDistributedAccount.reckey      = STRING(ROWID(acctcost)) 
                  .  
     END.
                                                                        
 END. /* IF prmAction = "View" THEN DO:*/



 IF prmAction = "ValidateAdd" THEN DO:

  
    IF prmcst_act EQ account.actnum OR
       NOT CAN-FIND(FIRST b-account
                    WHERE b-account.company EQ acctcost.company
                      AND b-account.actnum  EQ prmcst_act)
    THEN DO:
      cError = " is not valid, please try help..." .
      RETURN.
    END.


    lv-tot-rate = 100.

  FOR EACH b-acctcost NO-LOCK
      WHERE b-acctcost.company EQ account.company
      AND b-acctcost.actnum  EQ account.actnum:

      IF ROWID(b-acctcost) NE ROWID(acctcost) THEN
          lv-tot-rate = lv-tot-rate - b-acctcost.c-rate.
      ELSE
          lv-tot-rate = lv-tot-rate - 
              DEC(prmc_rate).
  END.

  IF lv-tot-rate NE 100 AND lv-tot-rate NE 0 THEN DO:
      cError = "Distribution Total must be 100..." .
  END.

 END. /*IF prmAction = "ValidateAdd" THEN DO:*/

 IF prmAction = "Add" THEN DO:

     FIND FIRST account WHERE account.company = prmComp                 
         AND account.actnum = prmact NO-LOCK NO-ERROR.  

   lv-tot-rate = 100.

  IF AVAIL account THEN 
  FOR EACH b-acctcost NO-LOCK
      WHERE b-acctcost.company EQ account.company
        AND b-acctcost.actnum  EQ account.actnum:
    lv-tot-rate = lv-tot-rate - b-acctcost.c-rate.
  END.

  ASSIGN
   acctcost.company = account.company
   acctcost.actnum  = account.actnum
   acctcost.c-rate  = lv-tot-rate
   acctcost.type    = "A".


     ASSIGN 
         acctcost.costacct = prmcst_act
         acctcost.c-rate = prmc_rate .

     ASSIGN prmAction = "View" .
                                 
 END. /*IF prmAction = "Add" THEN DO:*/


 IF prmAction = "ValidUpdate" THEN DO:

  
    IF prmcst_act EQ account.actnum OR
       NOT CAN-FIND(FIRST b-account
                    WHERE b-account.company EQ acctcost.company
                      AND b-account.actnum  EQ prmcst_act)
    THEN DO:
      cError = " is not valid, please try help..." .
      RETURN.
    END.


    lv-tot-rate = 100.

  FOR EACH b-acctcost NO-LOCK
      WHERE b-acctcost.company EQ account.company
      AND b-acctcost.actnum  EQ account.actnum:

      IF ROWID(b-acctcost) NE ROWID(acctcost) THEN
          lv-tot-rate = lv-tot-rate - b-acctcost.c-rate.
      ELSE
          lv-tot-rate = lv-tot-rate - DEC(prmc_rate).
  END.

  IF lv-tot-rate NE 100 AND lv-tot-rate NE 0 THEN DO:
      cError = "Distribution Total must be 100..." .
  END.

 END. /*IF prmAction = "ValidUpdate" THEN DO:*/



 IF prmAction = "Update" THEN DO:
     FIND FIRST account WHERE account.company = prmComp                 
         AND account.actnum = prmact NO-LOCK NO-ERROR. 

     FIND FIRST acctcost WHERE acctcost.company = account.company         
         AND acctcost.actnum = account.actnum AND rowid(acctcost) EQ TO-ROWID(prmReckey)
         EXCLUSIVE-LOCK NO-ERROR. 

     ASSIGN 
         acctcost.costacct = prmcst_act
         acctcost.c-rate = prmc_rate .


     ASSIGN prmAction = "View" .


 END. /* IF prmAction = "Update" THEN DO:*/

 IF prmAction = "Delete" THEN DO:
     FIND FIRST account WHERE account.company = prmComp                 
         AND account.actnum = prmact NO-LOCK NO-ERROR.                  
    

     FIND FIRST acctcost WHERE acctcost.company = account.company         
         AND acctcost.actnum = account.actnum AND rowid(acctcost) EQ TO-ROWID(prmReckey)
        EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL acctcost THEN DELETE acctcost.

     FIND LAST acctcost WHERE acctcost.company = account.company         
         AND acctcost.actnum = account.actnum NO-LOCK NO-ERROR.

     IF AVAIL account THEN
         ASSIGN
         prmact = acctcost.actnum
         prmAction = "view" .

 END. /* IF prmAction = "Delete" THEN DO:*/

 IF prmAction = "View" THEN DO:
    
      FIND FIRST acctcost WHERE acctcost.company = prmComp         
         AND acctcost.actnum = prmact AND rowid(acctcost) EQ TO-ROWID(prmReckey) NO-LOCK NO-ERROR.                  
     
             create ttGLDistributedAccount.                             
             IF AVAIL acctcost THEN
             assign                                                     
                 ttGLDistributedAccount.cst_act     = acctcost.costacct 
                 ttGLDistributedAccount.cst_dscr    = get-act-dscr()    
                 ttGLDistributedAccount.c_rate      = acctcost.c-rate   
                 ttGLDistributedAccount.actype      = acctcost.type  
                 ttGLDistributedAccount.reckey      = STRING(ROWID(acctcost)) .
                  .  
                    
                                                                        
 END. /* IF prmAction = "View" THEN DO:*/

 IF prmAction = "actdetail" THEN DO:

     FIND FIRST account WHERE account.company = prmComp                 
         AND account.actnum = prmact NO-LOCK NO-ERROR.                 
                      
         
             create ttGLDistributedAccount.                             
             assign                                                     
                 ttGLDistributedAccount.act       = account.actnum  
                 ttGLDistributedAccount.actdscr   = account.dscr    .  
     
                                                                        
 END. /* IF prmAction = "View" THEN DO:*/
