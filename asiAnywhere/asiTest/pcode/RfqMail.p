
/*------------------------------------------------------------------------
    File        : RfqMail.p
    Purpose     : Create Rfq Mail

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : feb 25 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRfqNo    AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     as Character no-undo.

DEFINE OUTPUT PARAMETER MailTo   AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER MailFrom AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER Subject  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER Body     AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER vInternalUser AS CHARACTER  NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmRfqNo    = ? THEN ASSIGN prmRfqNo = 0.
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
    vInternalUser = STRING(users.internal-user) .
END.



/* ********************  Preprocessor Definitions  ******************** */
        
IF prmAction = "MailRfq" THEN do: 

    IF vInternalUser = "No"  THEN do:
        ASSIGN 
            v-prgmname = "ERFQ.".

        FIND FIRST rfq WHERE rfq.company = prmComp AND rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.

        FIND FIRST cust WHERE cust.company =  prmComp AND cust.cust-no = rfq.cust-no   NO-LOCK NO-ERROR .
            ASSIGN 
                Subject   = "Thanks for Your Rfq, We appreciate Your Business."
                MailFrom  = "joe@advantzware.com" .              

        ASSIGN
            MailBody = MailBody + '<b>Customer Code:</b>  ' + STRING(rfq.cust-no) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '<b>Customer Name:</b>   ' + STRING(cust.name) + '&nbsp;&nbsp;&nbsp;&nbsp;' +  '<b>Request for Quote Number:</b>  ' + STRING(rfq.rfq-no) + '&nbsp;&nbsp;&nbsp;&nbsp;' +  '<b>Due Date:</b>  ' + STRING(rfq.due-date) + '<br>'  .
      
        DEFINE VAR rfqline AS INT INITIAL 1 NO-UNDO.
        FOR EACH rfqitem where rfqitem.company EQ prmComp AND rfqitem.rfq-no = prmRfqNo NO-LOCK :
            ASSIGN                               
                MailBody = MailBody + '<br>' + '  Line:  ' + STRING(rfqline) + '&nbsp;&nbsp;&nbsp;&nbsp;'  + '  Customer Part#:    ' + STRING(rfqitem.part-no) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '  Rfq Quantity:     ' +  STRING(rfqitem.qty[1]) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '  Estimate Number:     ' +  STRING(rfqitem.est-no) .

            rfqline = rfqline + 1.
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
      
        IF MailTo <>   users.image_filename AND MailTo <> "" THEN
            ASSIGN 
                MailTo = MailTo + ","  + users.image_filename.
        IF MailTo <>   users.image_filename AND MailTo = "" THEN
            ASSIGN 
                MailTo = MailTo  + users.image_filename.

        ASSIGN
                Body = MailBody .

        
    END.  /* end of not internal*/

    IF vInternalUser = "yes"  THEN DO:
        ASSIGN 
            v-prgmname = "ERFQ.".        

        FIND FIRST rfq WHERE rfq.company = prmComp AND rfq.rfq-no =  prmRfqNo NO-LOCK NO-ERROR.
        FIND FIRST cust WHERE cust.company =  prmComp AND cust.cust-no = rfq.cust-no  NO-LOCK NO-ERROR .
        ASSIGN 
            Subject   = "Thanks for Your Rfq, We appreciate Your Business." .
              
        ASSIGN
            MailBody = MailBody + '<b>Customer Code:</b>  ' + STRING(rfq.cust-no) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '<b>Customer Name:</b>   ' + STRING(cust.name) + '&nbsp;&nbsp;&nbsp;&nbsp;' +  '<b>Request for Quote Number:</b>  ' + STRING(rfq.rfq-no) + '&nbsp;&nbsp;&nbsp;&nbsp;' +  '<b>Due Date:</b>  ' + STRING(rfq.due-date) + '<br>'  .


        DEFINE VAR linenum AS INT INITIAL 1 NO-UNDO.
        FOR EACH rfqitem where rfqitem.company EQ prmComp AND rfqitem.rfq-no = prmRfqNo NO-LOCK :
            ASSIGN 
                MailBody = MailBody + '<br>' + '  Line:  ' + STRING(linenum) + '&nbsp;&nbsp;&nbsp;&nbsp;'  + '  Customer Part#:    ' + STRING(rfqitem.part-no) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '  Rfq Quantity:     ' +  STRING(rfqitem.qty[1]) + '&nbsp;&nbsp;&nbsp;&nbsp;' + '  Estimate Number:     ' +  STRING(rfqitem.est-no) .

            linenum = linenum + 1.
        END.
      
        ASSIGN
            Body = MailBody .

        DEFINE VAR ecodelogic AS CHAR NO-UNDO.
     
        FOR EACH empalert WHERE empalert.table_rec_key = cust.rec_key NO-LOCK:      
            FIND FIRST users WHERE users.user_id  = empalert.USER-ID NO-LOCK NO-ERROR .      
            FIND FIRST emailcod WHERE emailcod.emailcod = v-prgmname NO-LOCK NO-ERROR.
       
            FIND FIRST reftable NO-LOCK WHERE reftable.rec_key = STRING (RECID (empalert)) AND reftable.CODE   = emailcod.emailcod  NO-ERROR.
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
       
                FIND FIRST reftable NO-LOCK WHERE reftable.rec_key = STRING (RECID (empalert)) AND reftable.CODE   = emailcod.emailcod  NO-ERROR.
                IF AVAIL reftable THEN
                    ASSIGN  ecodelogic = "Yes" .
                ELSE ASSIGN ecodelogic = "No" .
        
                IF ecodelogic = "Yes" AND  users.image_filename <> "" THEN DO:
                MailTo = MailTo + users.image_filename + ","  .                
            END.
            
        END.  /* IF AVAIL cust THEN DO:*/ 
    END.  /* end of cust all*/


END.  /* end of internal */ 
       
END.   /*prmAction mail*/
   
 




