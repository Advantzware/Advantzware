
/*------------------------------------------------------------------------
    File        : gljrn_view.p
    Purpose     : General Ledger

     Main File   : 
    Syntax      :

    Description : Return a Dataset of all Invpice

    Author(s)   : 
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttGeneralLedgerView NO-UNDO
    FIELD line_no     AS INT
    FIELD actnum      AS CHAR         
    FIELD actdscr     AS CHAR     
    FIELD dscr        AS CHAR  
    FIELD amt         AS DECIMAL  
    FIELD xy          AS CHAR
    FIELD reckey      AS CHARACTER .

DEFINE DATASET dsGeneralLedgerView FOR ttGeneralLedgerView.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmline_no      AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmactnum       AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmactdscr      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmdscr         AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmamt          AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmjrl_no       AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHARACTER  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGeneralLedgerView .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     FOR EACH ttGeneralLedgerView:
        DELETE ttGeneralLedgerView .
    END.

IF prmAction    = ?  THEN ASSIGN prmAction      = "Select".
IF prmComp      = ?  THEN ASSIGN prmComp        = "".
IF prmUser      = ?  THEN ASSIGN prmUser        = "".
IF prmline_no   = ?  THEN ASSIGN prmline_no     = 0.
IF prmactnum    = ?  THEN ASSIGN prmactnum      = "".
IF prmactdscr   = ?  THEN ASSIGN prmactdscr     = "".
IF prmdscr      = ?  THEN ASSIGN prmdscr        = "". 
IF prmamt       = ?  THEN ASSIGN prmamt         = 0.
IF prmjrl_no    = ?  THEN ASSIGN prmjrl_no      = 0.
IF prmReckey    = ?  THEN ASSIGN prmReckey      = "".



DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF VAR v-po-qty as log initial true no-undo.
def var factor# as decimal no-undo.
DEF VAR lv-uom-list AS cha INIT "EA,MSF,M" NO-UNDO.
{oe/oe-sysct1.i NEW}
DEF VAR ll-inquiry AS LOG NO-UNDO.
DEF VAR v-actdscr LIKE account.dscr NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
 

{sys/inc/VAR.i "new shared"}  


 
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

DEF BUFFER bf-jrnl FOR gl-jrnl.
DEF VAR lv-account-recid AS RECID NO-UNDO.
DEF VAR v-debit AS DEC NO-UNDO.
DEF VAR v-credit AS DEC NO-UNDO.
DEF VAR lv-acct-dscr AS cha FORM "x(30)" LABEL "Account Name " NO-UNDO.

FUNCTION display-account RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 
    RELEASE account.
    IF AVAIL gl-jrnl THEN
    FIND FIRST account
        WHERE account.company EQ g_company
          AND account.actnum  EQ gl-jrnl.actnum
        NO-LOCK NO-ERROR.         
    RETURN IF AVAIL account THEN account.dscr ELSE "". 
  

END FUNCTION.






IF prmAction = "SelectGrid" THEN DO:
    
    FIND FIRST gl-jrn WHERE  gl-jrn.rec_key = prmReckey  NO-LOCK NO-ERROR.
         FOR EACH gl-jrnl OF gl-jrn NO-LOCK :
             CREATE ttGeneralLedgerView.
             ASSIGN 
                 ttGeneralLedgerView.line_no    = gl-jrnl.line 
                 ttGeneralLedgerView.actnum     = gl-jrnl.actnum 
                 ttGeneralLedgerView.actdscr    = display-account() 
                 ttGeneralLedgerView.dscr       = gl-jrnl.dscr   
                 ttGeneralLedgerView.amt        = gl-jrnl.tr-amt 
                 ttGeneralLedgerView.reckey     = gl-jrnl.rec_key .
             
            
      END. /*FOR EACH buff-cust  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/********************************Add **********************************/


IF prmAction = "ValidateAdd" THEN DO:

    FIND FIRST account
        WHERE account.company EQ g_company
          AND account.type    NE "T"
          AND account.actnum  EQ prmactnum
        NO-LOCK NO-ERROR.

    IF AVAIL account THEN lv-account-recid = RECID(account).

    ELSE DO:
      cError = "Account Number is invalid, try help..." .
      RETURN.
    END.
END.  /* end of validate add*/

/***********************check add user******************************************/


IF prmAction = "AddnewRec" THEN DO:
    
 DEF VAR v-dscr AS cha NO-UNDO.
 FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmactdscr NO-LOCK NO-ERROR.
    
  /* Code placed here will execute PRIOR to standard behavior. */  
  for each bf-jrnl WHERE bf-jrnl.j-no = gl-jrn.j-no NO-LOCK by line descending:
      assign
        x      = bf-jrnl.line
        v-dscr = bf-jrnl.dscr.
      leave.
  end.

   CREATE gl-jrnl .
  ASSIGN gl-jrnl.j-no = gl-jrn.j-no
         gl-jrnl.line = x + 1
         gl-jrnl.dscr = v-dscr.

  ASSIGN
      prmAction = "View"
      prmline_no   = gl-jrnl.line
      prmReckey = gl-jrnl.rec_key   .


END.  /* end of create ap-invl*/

IF prmAction = "Add" THEN DO:
  
     FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmactdscr NO-LOCK NO-ERROR.
     FIND FIRST gl-jrnl WHERE   gl-jrnl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

     
    ASSIGN
        gl-jrnl.line      =  prmline_no 
        gl-jrnl.actnum    =  prmactnum 
        gl-jrnl.dscr      =  prmdscr     
        gl-jrnl.tr-amt    =  prmamt .

    RUN redisplay-header.

      ASSIGN
      prmAction = "View"
       .
 

END.


/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:
    FIND FIRST account
        WHERE account.company EQ g_company
          AND account.type    NE "T"
          AND account.actnum  EQ prmactnum
        NO-LOCK NO-ERROR.

    IF AVAIL account THEN lv-account-recid = RECID(account).

    ELSE DO:
      cError = "Account Number is invalid, try help..." .
      RETURN.
    END.
     
END.


IF prmAction = "Update" THEN DO:
     
     FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmactdscr  NO-LOCK NO-ERROR.
     FIND FIRST gl-jrnl WHERE  gl-jrnl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN
        gl-jrnl.line      =  prmline_no 
        gl-jrnl.actnum    =  prmactnum
        gl-jrnl.dscr      =  prmdscr     
        gl-jrnl.tr-amt    =  prmamt .

    
 
      RUN redisplay-header.
        

         ASSIGN
             prmAction = "View"
             prmline_no   =  gl-jrnl.LINE  .
        

END.  

/*********************************delete ******************************/



IF prmAction = "DataDelete"  THEN DO:

    FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmactdscr  NO-LOCK NO-ERROR.
    FIND FIRST gl-jrnl WHERE gl-jrnl.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL gl-jrnl THEN
        DELETE gl-jrnl .

    FIND LAST gl-jrnl OF gl-jrn NO-LOCK NO-ERROR.
    IF AVAIL gl-jrnl THEN
        ASSIGN
        prmReckey = gl-jrnl.rec_key
        prmAction = "View" .

    RUN redisplay-header.

END.  

/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
         FOR EACH gl-jrnl WHERE  gl-jrnl.rec_key = prmReckey NO-LOCK :
             CREATE ttGeneralLedgerView.
             ASSIGN 
                 ttGeneralLedgerView.line_no    = gl-jrnl.line 
                 ttGeneralLedgerView.actnum     = gl-jrnl.actnum 
                 ttGeneralLedgerView.actdscr    = display-account()
                 ttGeneralLedgerView.dscr       = gl-jrnl.dscr   
                 ttGeneralLedgerView.amt        = gl-jrnl.tr-amt  
                 ttGeneralLedgerView.reckey     = gl-jrnl.rec_key 
                 
                .
            
      END. /*FOR EACH gl-jrnl  */
      
END. /*IF prmAction = "View" THEN DO:*/

PROCEDURE redisplay-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-gl-jrnl FOR gl-jrnl.
 
    FIND CURRENT gl-jrn.

    ASSIGN
     gl-jrn.tdeb  = 0
     gl-jrn.tcred = 0.

    FOR EACH b-gl-jrnl OF gl-jrn NO-LOCK:
      IF b-gl-jrnl.tr-amt GT 0 THEN
        gl-jrn.tdeb  = gl-jrn.tdeb  + b-gl-jrnl.tr-amt.
      ELSE
        gl-jrn.tcred = gl-jrn.tcred + b-gl-jrnl.tr-amt.
    END.

    gl-jrn.tr-amt = gl-jrn.tdeb + gl-jrn.tcred.

    FIND CURRENT gl-jrn NO-LOCK.   

END PROCEDURE.
