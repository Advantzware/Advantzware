
/*------------------------------------------------------------------------
    File        : glctrl_view.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttGLCtrlView NO-UNDO
    FIELD jrnl       AS INT
    FIELD trns       AS INT
    FIELD crtyr      AS CHAR
    FIELD crtdscr    AS CHAR
    FIELD profit     AS CHAR
    FIELD prodscr    AS CHAR
    FIELD reckey     AS CHAR
    FIELD ctrlview   AS CHAR 
    .

DEFINE DATASET dsGLCtrlView FOR ttGLCtrlView.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmjrnl     AS INTEGER    NO-UNDO.
DEFINE INPUT PARAMETER prmtrns     AS INTEGER    NO-UNDO.
DEFINE INPUT PARAMETER prmcrtyr    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcrtdscr  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmprofit   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmprodscr  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER ip_field    AS CHARACTER  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGLCtrlView .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction         = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmjrnl           = ?  THEN ASSIGN prmjrnl       = 0.
IF prmtrns           = ?  THEN ASSIGN prmtrns       = 0.
IF prmcrtyr          = ?  THEN ASSIGN prmcrtyr      = "".
IF prmcrtdscr        = ?  THEN ASSIGN prmcrtdscr    = "".
IF prmprofit         = ?  THEN ASSIGN prmprofit     = "".
IF prmprodscr        = ?  THEN ASSIGN prmprodscr    = "".
                                                  


DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.

         
     
 IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  cocode  = prmComp .


   IF NOT CAN-FIND(FIRST gl-ctrl WHERE gl-ctrl.company EQ prmComp) THEN DO:
    CREATE gl-ctrl.
    gl-ctrl.company = prmComp.
  END.
  FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ prmComp NO-LOCK NO-ERROR.


   IF prmAction = "Update" THEN DO:

       
       DEF VAR lv-acct LIKE account.actnum NO-UNDO.
       DEF VAR lv-type AS CHAR NO-UNDO.
       DEF VAR lv-types AS CHAR INIT "ACELRT" NO-UNDO.
       DEF VAR lv-type-dscr AS CHAR INIT
           "Asset,Capital,Expense,Liability,Revenue,Total" NO-UNDO.

      
           
           FIND FIRST account
               WHERE account.company EQ cocode
               AND account.actnum  EQ prmcrtyr
               AND account.TYPE    EQ "C"
               NO-LOCK NO-ERROR.
           IF AVAIL account THEN do: 
               ASSIGN prmcrtdscr = account.dscr.
           END.
           ELSE do:
               cError = "Invalid Current Year Earnings, try help..." .
           RETURN.
           END.

           FIND FIRST account
               WHERE account.company EQ cocode
               AND account.actnum  EQ prmprofit
               AND account.TYPE    EQ "E"
               NO-LOCK NO-ERROR.
           IF AVAIL account THEN do:
               ASSIGN prmprodscr = account.dscr.
           END.
           ELSE do:
               cError = "Invalid Profit Contra, try help..." .
           RETURN.
           END.


   END. /* validate update */
 

 IF prmAction = "Update" THEN DO:
     FIND FIRST gl-ctrl WHERE gl-ctrl.company = prmComp
          EXCLUSIVE-LOCK NO-ERROR.


    IF AVAIL gl-ctrl THEN
        ASSIGN
            gl-ctrl.journal      = prmjrnl   
            gl-ctrl.trnum        = prmtrns   
            gl-ctrl.ret          = prmcrtyr  
            gl-ctrl.ret-dscr     = prmcrtdscr
            gl-ctrl.contra       = prmprofit 
            gl-ctrl.con-dscr     = prmprodscr  .

        ASSIGN
            prmAction = "View" . 
 END.
 

 IF prmAction = "View" THEN DO:
     FIND FIRST gl-ctrl WHERE gl-ctrl.company = prmComp
          NO-LOCK NO-ERROR.

     CREATE ttGLCtrlView.
        ASSIGN
             ttGLCtrlView.jrnl    = gl-ctrl.journal 
             ttGLCtrlView.trns    = gl-ctrl.trnum   
             ttGLCtrlView.crtyr   = gl-ctrl.ret     
             ttGLCtrlView.crtdscr = gl-ctrl.ret-dscr
             ttGLCtrlView.profit  = gl-ctrl.contra  
             ttGLCtrlView.prodscr = gl-ctrl.con-dscr  .


 END.


