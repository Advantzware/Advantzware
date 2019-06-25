
/*------------------------------------------------------------------------
    File        : dcmemo_list.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttDbCrMemoList NO-UNDO
    FIELD vend        AS CHAR
    FIELD vname       AS CHAR
    FIELD checkno     AS INT
    FIELD checkdate   AS CHAR
    FIELD amt         AS DEC
    FIELD checkamt    AS DEC

    FIELD reckey      AS CHAR
    FIELD extra       AS CHAR 
    .

DEFINE DATASET dsDbCrMemoList FOR ttDbCrMemoList.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvend      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvname     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcheckno   AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmcheckdate   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmamt          AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmcheckamt     AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey      AS CHAR NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDbCrMemoList .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     

IF prmAction         = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmvend           = ?  THEN ASSIGN prmvend       = "".
IF prmvname          = ?  THEN ASSIGN prmvname      = "".
IF prmcheckno        = ?  THEN ASSIGN prmcheckno    = 0. 
IF prmcheckdate      = ?  THEN ASSIGN prmcheckdate  = "". 
IF prmamt            = ?  THEN ASSIGN prmamt        = 0.
IF prmcheckamt       = ?  THEN ASSIGN prmcheckamt   = 0.
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

DEF BUFFER xap-pay FOR ap-pay.
DEF BUFFER bf-payl FOR ap-payl.

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

FUNCTION display-amount RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
  DEF VAR ld-amt AS DEC NO-UNDO.

  ld-amt = 0.
  FOR EACH ap-payl OF ap-pay NO-LOCK:
      ld-amt =  ld-amt - ap-payl.amt-paid + ap-payl.amt-disc.
  END.
  RETURN ld-amt.
END FUNCTION.



IF prmAction = "Search" THEN DO:
    
     FOR EACH ap-pay WHERE ap-pay.company = g_company 
         AND ap-pay.memo = TRUE 
         AND ap-pay.posted = FALSE
         AND (ap-pay.check-no = prmcheckno OR prmcheckno = 0)
         AND (ap-pay.vend-no BEGINS prmvend OR prmvend = "") NO-LOCK,
         EACH vend OF ap-pay NO-LOCK:

        CREATE ttDbCrMemoList.
           ASSIGN 
                 ttDbCrMemoList.vend          = ap-pay.vend-no
                 ttDbCrMemoList.vname         = vend.name
                 ttDbCrMemoList.checkno       = ap-pay.check-no
                 ttDbCrMemoList.checkdate     = string(ap-pay.check-date)
                 ttDbCrMemoList.amt           = display-amount()
                 ttDbCrMemoList.checkamt      = ap-pay.check-amt
                 ttDbCrMemoList.reckey        = ap-pay.rec_key    .
            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "ValidateAdd" THEN DO:

    FIND FIRST vend WHERE vend.company = cocode
                        AND vend.vend-no = prmvend
                        NO-LOCK NO-ERROR.
  IF NOT AVAIL vend THEN DO:
     ASSIGN cError = "Vendor does not exist. Try Help.." . 
     RETURN.
  END.

   
 END.

/********************************************************************/

IF prmAction = "Add" THEN DO:
    
   FIND FIRST ap-pay WHERE ap-pay.company = g_company
          AND ap-pay.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
    
    ASSIGN
        ap-pay.vend-no      =  prmvend               
        ap-pay.check-date   =  DATE(prmcheckdate)
        ap-pay.check-amt    =  prmcheckamt         
          . 

   ASSIGN prmAction = "View"  .
           

END.

IF prmAction = "AddNewRecord" THEN DO:

    DEF VAR xchk LIKE ap-pay.check-no NO-UNDO.
    DEF VAR X AS INT NO-UNDO.

  
  for each xap-pay use-index c-no no-lock 
           where xap-pay.memo by xap-pay.check-no descending:
    xchk = xap-pay.check-no.
    leave.
  end.
  x = 0.
  for each xap-pay use-index c-no no-lock by c-no descending:
    x = xap-pay.c-no.
    leave.
  END.

   CREATE ap-pay .

  ap-pay.check-no = xchk + 1.
   if ap-pay.check-no < 90000001 then ap-pay.check-no = 90000001.

   assign ap-pay.memo       = yes
          ap-pay.c-no       = x + 1
          ap-pay.company    = cocode
          ap-pay.check-date = today
          ap-pay.posted = FALSE 

      .

   
   ASSIGN prmAction = "Viewitem" 
           prmReckey = ap-pay.rec_key.
    

END. /* add new record */  


/**************Update *************************************************/

IF prmAction = "Update" THEN DO:
  
    FIND FIRST vend WHERE vend.company = cocode
                        AND vend.vend-no = prmvend
                        NO-LOCK NO-ERROR.
  IF NOT AVAIL vend THEN DO:
     ASSIGN cError = "Vendor does not exist. Try Help.. " . 
     
  END.

END.

IF prmAction = "Update" THEN DO:
      FIND FIRST ap-pay WHERE ap-pay.company = g_company
          AND ap-pay.rec_key = prmReckey
          AND ap-pay.memo = TRUE 
          AND ap-pay.posted = FALSE EXCLUSIVE-LOCK NO-ERROR. 

        ASSIGN    
            ap-pay.check-date   =  DATE(prmcheckdate)        .
        
    
       ASSIGN prmAction = "View" .

END.  

/*********************************delete ******************************/


IF prmAction = "DataDelete"  THEN DO:

   FIND FIRST ap-pay WHERE ap-pay.company = g_company
          AND ap-pay.rec_key = prmReckey
          AND ap-pay.memo = TRUE 
          AND ap-pay.posted = FALSE EXCLUSIVE-LOCK NO-ERROR. 
         
   
    IF AVAIL ap-pay THEN DELETE ap-pay .

 
  FOR EACH ap-pay WHERE ap-pay.company = g_company
          AND ap-pay.memo = TRUE 
          AND ap-pay.posted = FALSE NO-LOCK, 
          EACH vend OF ap-pay NO-LOCK:
          ASSIGN
        prmReckey = ap-pay.rec_key  .
          LEAVE.
  END.

    prmAction = "View" .

END.


/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
      FOR EACH ap-pay WHERE ap-pay.company = g_company
          AND ap-pay.rec_key = prmReckey
          AND ap-pay.memo = TRUE 
          AND ap-pay.posted = FALSE NO-LOCK, 
          EACH vend OF ap-pay NO-LOCK:
     
        CREATE ttDbCrMemoList.

           ASSIGN 
                 ttDbCrMemoList.vend          = ap-pay.vend-no
                 ttDbCrMemoList.vname         = vend.name
                 ttDbCrMemoList.checkno       = ap-pay.check-no
                 ttDbCrMemoList.checkdate     = string(ap-pay.check-date)
                 ttDbCrMemoList.amt           = display-amount()
                 ttDbCrMemoList.checkamt      = ap-pay.check-amt
                 ttDbCrMemoList.reckey        = ap-pay.rec_key    .

      END.
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "Viewitem" THEN DO:
    
     FIND FIRST ap-pay WHERE ap-pay.company = g_company
          AND ap-pay.rec_key = prmReckey
          NO-LOCK NO-ERROR.
         CREATE ttDbCrMemoList.

           ASSIGN 
                 ttDbCrMemoList.vend          = ap-pay.vend-no
                 ttDbCrMemoList.checkno       = ap-pay.check-no
                 ttDbCrMemoList.checkdate     = string(ap-pay.check-date)
                 ttDbCrMemoList.checkamt      = ap-pay.check-amt
                 ttDbCrMemoList.reckey        = ap-pay.rec_key    .

 END. /*IF prmAction = "Select" THEN DO:*/



/*****************************Add/Edit DB/CR Memo(VQ3)**********************************/

 IF prmAction = "MemoSearch" THEN DO:
    
     FOR EACH ap-pay WHERE ap-pay.company EQ cocode 
         AND ap-pay.posted EQ YES 
         AND ASI.ap-pay.memo EQ YES 
         AND (ap-pay.check-no = prmcheckno OR prmcheckno = 0)
         AND (ap-pay.vend-no BEGINS prmvend OR prmvend = "") NO-LOCK,
         EACH vend OF ap-pay NO-LOCK BY ap-pay.check-no DESC:
    
        CREATE ttDbCrMemoList.
           ASSIGN 
                 ttDbCrMemoList.vend          = ap-pay.vend-no
                 ttDbCrMemoList.vname         = vend.name
                 ttDbCrMemoList.checkno       = ap-pay.check-no
                 ttDbCrMemoList.checkdate     = string(ap-pay.check-date)
                 ttDbCrMemoList.amt           = display-amount()
                 ttDbCrMemoList.checkamt      = ap-pay.check-amt
                 ttDbCrMemoList.reckey        = ap-pay.rec_key    .
            
    END. /*FOR EACH ap-pay  */
END. /*IF prmAction = "MemoSearch" THEN DO:*/

IF prmAction = "MemoView" THEN DO:
    
      FOR EACH ap-pay WHERE ap-pay.company = g_company
          AND ap-pay.rec_key = prmReckey
          AND ap-pay.memo = YES 
          AND ap-pay.posted = YES NO-LOCK, 
          EACH vend OF ap-pay NO-LOCK:
     
        CREATE ttDbCrMemoList.

           ASSIGN 
                 ttDbCrMemoList.vend          = ap-pay.vend-no
                 ttDbCrMemoList.vname         = vend.name
                 ttDbCrMemoList.checkno       = ap-pay.check-no
                 ttDbCrMemoList.checkdate     = string(ap-pay.check-date)
                 ttDbCrMemoList.amt           = display-amount()
                 ttDbCrMemoList.checkamt      = ap-pay.check-amt
                 ttDbCrMemoList.reckey        = ap-pay.rec_key    .

      END.
END. /*IF prmAction = "Select" THEN DO:*/
