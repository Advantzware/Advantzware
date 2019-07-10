
/*------------------------------------------------------------------------
    File        : viewdisbr.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttViewDisbursements NO-UNDO
    FIELD vline       AS INT
    FIELD dscr        AS CHAR
    FIELD actnum      AS CHAR
    FIELD qty         AS DEC
    FIELD untprice    AS DEC
    FIELD amt         AS DEC
    FIELD reckey      AS CHAR .
  
DEFINE DATASET dsViewDisbursements FOR ttViewDisbursements.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvline       AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmdscr        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmactnum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmqty         AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmuntprice    AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmamt         AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmreckey      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmchkno       AS CHAR NO-UNDO.
                       

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewDisbursements .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction           = ?  THEN ASSIGN prmAction      = "Select".
IF prmComp             = ?  THEN ASSIGN prmComp        = "".
IF prmUser             = ?  THEN ASSIGN prmUser        = "".
IF prmvline            = ?  THEN ASSIGN prmvline       = 0.
IF prmdscr             = ?  THEN ASSIGN prmdscr        = "".
IF prmactnum           = ?  THEN ASSIGN prmactnum      = "". 
IF prmqty              = ?  THEN ASSIGN prmqty         = 0. 
IF prmuntprice         = ?  THEN ASSIGN prmuntprice    = 0.
IF prmamt              = ?  THEN ASSIGN prmamt         = 0.
IF prmreckey           = ?  THEN ASSIGN prmreckey      = "".
IF prmchkno            = ?  THEN ASSIGN prmchkno       = "".




DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
DEF BUFFER bf-disl FOR ap-disl.
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

  FIND FIRST ap-dis WHERE ap-dis.rec_key = prmchkno AND 
      ap-dis.company = cocode NO-LOCK NO-ERROR.
    
  FOR EACH ap-disl OF ap-dis NO-LOCK :
    
        CREATE ttViewDisbursements.
        ASSIGN
            ttViewDisbursements.vline    = ap-disl.line
            ttViewDisbursements.dscr     = ap-disl.dscr
            ttViewDisbursements.actnum   = ap-disl.actnum
            ttViewDisbursements.qty      = ap-disl.qty
            ttViewDisbursements.untprice = ap-disl.unit-pr
            ttViewDisbursements.amt      = ap-disl.amt
            ttViewDisbursements.reckey   = ap-disl.rec_key .
           
       END.
     
END. /*IF prmAction = "Select" THEN DO:*/



 IF prmAction = "UpdateValidate" THEN DO:
     FIND FIRST ap-dis WHERE ap-dis.rec_key = prmchkno AND 
      ap-dis.company = cocode NO-LOCK NO-ERROR.
    
  FIND FIRST ap-disl OF ap-dis EXCLUSIVE-LOCK NO-ERROR.

    IF prmactnum <> "" THEN DO:
     FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum = prmactnum
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          cError = "Invalid Account Number." .
          RETURN.
       END.   
    END.


 END. /* validate update */

 IF prmAction = "Update" THEN DO:
     
     FIND FIRST ap-dis WHERE ap-dis.rec_key = prmchkno AND 
      ap-dis.company = cocode NO-LOCK NO-ERROR.
    
   FIND FIRST ap-disl WHERE ap-disl.company = cocode
        AND ap-disl.rec_key = prmreckey EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL ap-disl THEN
     ASSIGN
            ap-disl.line          = prmvline   
            ap-disl.dscr          = prmdscr    
            ap-disl.actnum        = prmactnum  
            ap-disl.qty           = prmqty     
            ap-disl.unit-pr       = prmuntprice
            ap-disl.amt           = prmamt  . 
               

            ASSIGN
                prmAction = "View" .


 END.

 IF prmAction = "AddnewRecValidate" THEN DO:
     FIND FIRST ap-dis WHERE ap-dis.rec_key = prmchkno AND 
     ap-dis.company = cocode NO-LOCK NO-ERROR.

 FIND FIRST ap-disl OF ap-dis EXCLUSIVE-LOCK NO-ERROR.

   IF prmactnum <> "" THEN DO:
    FIND FIRST account WHERE account.company = g_company AND
                               account.TYPE <> "T" AND
                               account.actnum = prmactnum
                               NO-LOCK NO-ERROR.
      IF NOT AVAIL account THEN DO:
         cError = "Invalid Account Number." .
         RETURN.
      END.   
   END.

 END. /* validate Add*/

 IF prmAction = "AddnewRec" THEN DO:

     FIND FIRST ap-dis WHERE ap-dis.rec_key = prmchkno AND 
       ap-dis.company = cocode NO-LOCK NO-ERROR. 

      FOR EACH bf-disl NO-LOCK WHERE bf-disl.d-no = ap-dis.d-no BY bf-disl.LINE DESCENDING:
          X = bf-disl.LINE.
          LEAVE.
      END.
     
      CREATE ap-disl .
      
      ASSIGN ap-disl.d-no = ap-dis.d-no
          ap-disl.company = g_company
          ap-disl.check-no = ap-dis.check-no
          ap-disl.LINE = X + 1.
      
      FIND FIRST vend WHERE vend.company = g_company
          AND vend.vend-no = ap-dis.vend
          NO-LOCK NO-ERROR.
      ASSIGN ap-disl.actnum = IF AVAIL vend THEN vend.actnum ELSE "".
                  find first account where account.company = g_company and
                      account.actnum = if   avail(vend) then vend.actnum
                          else ap-disl.actnum
                              no-lock no-error.
                               
               if not available account then
                   find first account where account.company = g_company and
                   account.actnum = ap-disl.actnum
                   no-lock no-error.
               if not avail(account) THEN do :
                   find first ap-ctrl WHERE ap-ctrl.company = g_company no-lock no-error.
                   if AVAIL ap-ctrl THEN 
                       find first account WHERE account.company = g_company and
                       account.actnum  = ap-ctrl.purchases
                       no-lock no-error.
                   if avail account THEN assign ap-disl.actnum = account.actnum.
                   end.
        

         ASSIGN
                prmReckey = ap-disl.rec_key
                prmAction = "View" 
                prmvline = ap-disl.line.

 END. /*addd*/ 

 IF prmAction = "Add" THEN DO:

     FIND FIRST ap-dis WHERE ap-dis.rec_key = prmchkno AND 
     ap-dis.company = cocode NO-LOCK NO-ERROR.

       FIND FIRST ap-disl WHERE ap-disl.company = cocode
        AND ap-disl.rec_key = prmreckey EXCLUSIVE-LOCK NO-ERROR.
 
       IF AVAIL ap-disl THEN
       ASSIGN
            ap-disl.line          = prmvline   
            ap-disl.dscr          = prmdscr    
            ap-disl.actnum        = prmactnum  
            ap-disl.qty           = prmqty     
            ap-disl.unit-pr       = prmuntprice
            ap-disl.amt           = prmamt  .   

         ASSIGN 
                prmAction = "View" .


 END.


 IF prmAction = "Deletevalidate" THEN DO:

   FIND FIRST ap-dis WHERE ap-dis.rec_key = prmchkno AND 
     ap-dis.company = cocode NO-LOCK NO-ERROR.


     IF ap-dis.posted THEN do:
      cError = "This Cash Disbursement has been posted. No deletion allowed!".
      RETURN.
     END.
       
 END. /*validate delete */ 


 IF prmAction = "Delete" THEN DO:
     
     FIND FIRST ap-dis WHERE ap-dis.rec_key = prmchkno AND 
     ap-dis.company = cocode NO-LOCK NO-ERROR.

      FIND FIRST ap-disl WHERE ap-disl.company = cocode
        AND ap-disl.rec_key = prmreckey EXCLUSIVE-LOCK NO-ERROR.

     IF ap-dis.posted THEN do:
      cError = "This Cash Disbursement has been posted. No deletion allowed!".
      RETURN.
     END.
     ELSE do:
         IF AVAIL ap-disl THEN
             DELETE ap-disl.
     END.

      

    FIND LAST ap-disl WHERE ap-disl.d-no EQ ap-dis.d-no NO-LOCK NO-ERROR.
            
   IF AVAIL ap-disl THEN
         ASSIGN
         prmAction = "View"
         prmReckey = ap-disl.rec_key.

 END. /*delete */

  
 IF prmAction = "View" THEN DO:
    
  FIND FIRST ap-dis WHERE ap-dis.rec_key = prmchkno AND 
     ap-dis.company = cocode NO-LOCK NO-ERROR.

    FIND FIRST ap-disl WHERE ap-disl.company = cocode
        AND ap-disl.rec_key = prmreckey NO-LOCK NO-ERROR.
     
    IF AVAIL ap-disl  THEN DO:
     
        CREATE ttViewDisbursements.
        ASSIGN
            ttViewDisbursements.vline    = ap-disl.line
            ttViewDisbursements.dscr     = ap-disl.dscr
            ttViewDisbursements.actnum   = ap-disl.actnum
            ttViewDisbursements.qty      = ap-disl.qty
            ttViewDisbursements.untprice = ap-disl.unit-pr
            ttViewDisbursements.amt      = ap-disl.amt
            ttViewDisbursements.reckey   = ap-disl.rec_key .
     END.
     
 END.

