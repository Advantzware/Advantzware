

/*------------------------------------------------------------------------
    File        : OrderMail.p
    Purpose     : OrderItem

    Syntax      :

    Description : Mail Sender of Order entry

    Author(s)   : 
    Created     : feb 25 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmComp     as Character no-undo.
DEFINE INPUT PARAMETER prmItem     AS CHAR NO-UNDO.

DEFINE OUTPUT PARAMETER MailTo   AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER MailFrom AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER Subject  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER Body     AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER vInternalUser AS CHARACTER  NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmAction   = ? THEN ASSIGN prmAction     = "".

DEFINE VAR MailBody AS cha NO-UNDO.
DEF VAR ls-to-list AS cha NO-UNDO.
DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

 FIND FIRST users WHERE  users.USER_id = prmUser  NO-LOCK NO-ERROR.
        IF AVAIL users THEN DO:
            ASSIGN 
                vInternalUser = STRING(users.internal-user)
                .
        END.


/* ********************  Preprocessor Definitions  ******************** */
        
IF prmAction = "MailOrder" THEN do: 
IF vInternalUser = "no"  THEN do:

 ASSIGN 
    v-prgmname = "R-OrdOpn.".

FIND FIRST oe-ord WHERE oe-ord.company = prmComp AND oe-ord.ord-no = int(prmOrderNum) NO-LOCK NO-ERROR.
 FIND FIRST cust WHERE cust.company =  prmComp AND cust.cust-no = oe-ord.cust-no   NO-LOCK NO-ERROR .
    ASSIGN 
        Subject   = "Thanks for Your Order, We appreciate Your Business."
        MailFrom  = "joe@advantzware.com" .
       
    ASSIGN
        MailBody = MailBody + 'Order:  ' + STRING(oe-ord.ord-no) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '   Customer Po#:  ' + oe-ord.po-no .
      

    FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
        oe-ordl.ord-no = int(prmOrderNum) NO-LOCK :

        ASSIGN 
            MailBody = MailBody + '<br>' + 'Item:    ' + oe-ordl.i-no + '&nbsp;&nbsp;&nbsp;&nbsp;' + '  Quantity:    ' + STRING(oe-ordl.qty) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '  Price:     ' +  STRING(oe-ordl.price) .
     END.

      {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}
        
          IF ls-to-list NE '' THEN DO:
            ASSIGN MailTo =  ls-to-list.    
        END.

       
        FIND FIRST users WHERE  users.USER_id = prmUser  NO-LOCK NO-ERROR.
        IF AVAIL users THEN DO:
            ASSIGN 
                vInternalUser = STRING(users.internal-user)
                .
        END.

        IF MailTo = "" THEN ASSIGN  MailTo = MailTo + users.image_filename.
        IF MailTo <>  users.image_filename AND MailTo <> "" THEN
                ASSIGN 
                    MailTo = MailTo + "," +  users.image_filename.
        ASSIGN
                Body = MailBody .

END.  /* end of not internal*/

IF vInternalUser = "yes"  THEN DO:

 ASSIGN 
    v-prgmname = "EordEnt.".

FIND FIRST oe-ord WHERE oe-ord.company = prmComp AND oe-ord.ord-no = int(prmOrderNum) NO-LOCK NO-ERROR.
 FIND FIRST cust WHERE cust.company =  prmComp AND cust.cust-no = oe-ord.cust-no   NO-LOCK NO-ERROR .
    ASSIGN 
        Subject   = "New Order Raised ." .
        
       
    ASSIGN
        MailBody = MailBody + '<b>Customer Code:</b>  ' + STRING(oe-ord.cust-no) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '<b>Customer Name:</b>   ' + STRING(oe-ord.cust-name) + '&nbsp;&nbsp;&nbsp;&nbsp;' +  '<b>Order Number:</b>  ' + STRING(oe-ord.ord-no) + '<br>'  .
      

    FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
        oe-ordl.ord-no = int(prmOrderNum) NO-LOCK :

        ASSIGN 
            MailBody = MailBody + '<br>' + '  Line:  ' + STRING(oe-ordl.LINE) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '  FG Item:  ' + STRING(oe-ordl.i-no) + '&nbsp;&nbsp;&nbsp;&nbsp;'  + '  Customer Part#:  ' + STRING(oe-ordl.part-no) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '  Quantity:    ' + STRING(oe-ordl.qty) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '  Price:     ' +  STRING(oe-ordl.price) + '&nbsp;&nbsp;&nbsp;&nbsp;' + 'UOM:    ' + oe-ordl.pr-uom + '&nbsp;&nbsp;&nbsp;&nbsp;' + 'Po#:    ' + oe-ordl.po-no + '&nbsp;&nbsp;&nbsp;&nbsp;' + 'Due Date:    ' + STRING(oe-ordl.req-date)  .
     END.
      
     ASSIGN
                Body = MailBody .

     DEFINE VAR ecodelogic AS CHAR NO-UNDO.
     
     FOR EACH empalert WHERE empalert.table_rec_key = cust.rec_key NO-LOCK:
       
         FIND FIRST users WHERE users.user_id  = empalert.USER-ID NO-LOCK NO-ERROR .
       
         FIND FIRST emailcod WHERE emailcod.emailcod = v-prgmname NO-LOCK NO-ERROR.
       
            FIND FIRST reftable NO-LOCK WHERE reftable.rec_key = STRING (RECID (empalert))
                         AND reftable.CODE   = emailcod.emailcod  NO-ERROR.
            IF AVAIL reftable THEN
                ASSIGN  ecodelogic = "Yes" .
                ELSE ASSIGN ecodelogic = "No" .
        
            IF ecodelogic = "Yes" AND  users.image_filename <> "" THEN DO:
                MailTo = MailTo + users.image_filename + ","  .
            END.
            
           
     END.  /* end of empalert*/ 

    FIND FIRST cust WHERE cust.cust-no = "ALL" NO-LOCK NO-ERROR.
    IF AVAIL cust THEN DO:
        FOR EACH empalert WHERE empalert.table_rec_key = cust.rec_key NO-LOCK:
       
         FIND FIRST users WHERE users.user_id  = empalert.USER-ID NO-LOCK NO-ERROR .
       
         FIND FIRST emailcod WHERE emailcod.emailcod = v-prgmname NO-LOCK NO-ERROR.
       
            FIND FIRST reftable NO-LOCK WHERE reftable.rec_key = STRING (RECID (empalert))
                         AND reftable.CODE   = emailcod.emailcod  NO-ERROR.
            IF AVAIL reftable THEN
                ASSIGN  ecodelogic = "Yes" .
                ELSE ASSIGN ecodelogic = "No" .
        
            IF ecodelogic = "Yes" AND  users.image_filename <> "" THEN DO:
                MailTo = MailTo + users.image_filename + ","  .
            END.
            
     END.  /* end of empalert*/ 

    END.  /* end of cust all*/


END.  /* end of internal */ 
       

    END.   /*prmAction mail*/
   
 




