

/*------------------------------------------------------------------------
    File        : CustNotes.p
    Purpose     : Download Notes for customer's contacts 

    Author(s)   : Kuldeep
    Created     : Mar 27 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttNotes NO-UNDO
    FIELD cust_no           LIKE  cust.cust-no
    FIELD contact           LIKE  cust.contact
    FIELD note_date         LIKE  notes.note_date     
    FIELD note_time         LIKE notes.note_time      
    FIELD viewed            LIKE notes.viewed    
    FIELD note_title        LIKE notes.note_title
    FIELD user_id           LIKE notes.user_id
    FIELD note_text         LIKE notes.note_text
    FIELD rec_key           LIKE notes.rec_key
    .
    DEFINE DATASET dsNotes FOR ttNotes.

    DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmComp    AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmCust    AS CHARACTER  NO-UNDO.
    
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsNotes.

    IF prmAction       = ?  THEN ASSIGN prmAction     = "Select".
    IF prmComp        = ?   THEN ASSIGN prmComp      = "".
    IF prmUser        = ?   THEN ASSIGN prmUser      = "".
    IF prmCust        = ?   THEN ASSIGN prmCust      = "".
    
    IF prmComp EQ "" THEN DO:
         FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.
         prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
   END.
   IF prmAction = "Select" THEN DO:
     FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no AND cust.company = prmComp NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
         FOR EACH phone WHERE phone.table_rec_key = cust.rec_key NO-LOCK:
             FOR EACH notes WHERE notes.rec_key = phone.rec_key NO-LOCK: 
                 CREATE ttNotes.
                 ASSIGN 
                     ttNotes.cust_no         = cust.cust-no
                     ttNotes.contact         = cust.contact
                     ttNotes.note_date       = notes.note_date       
                     ttNotes.note_time       = notes.note_time       
                     ttNotes.viewed          = notes.viewed       
                     ttNotes.note_title      = notes.note_title
                     ttNotes.user_id         = notes.user_id         
                     ttNotes.note_text       = notes.note_text           
                     ttNotes.rec_key         = notes.rec_key .
              END. /*FOR EACH Notes*/
           END. /* FOR EACH phone*/
       END. /* FOR EACH cust*/
     END. /* FOR EACH usercust*/
    END.
      

    IF prmAction = "Search" THEN DO:
         FOR EACH usercust WHERE usercust.user_id = prmUser 
           AND usercust.company EQ prmComp AND (usercust.cust-no = prmCust OR usercust.cust-no BEGINS prmCust) NO-LOCK:
           FOR EACH cust WHERE cust.cust-no = usercust.cust-no AND cust.company = prmComp NO-LOCK :
               FOR EACH phone WHERE phone.table_rec_key = cust.rec_key NO-LOCK:
                 FOR EACH notes WHERE notes.rec_key = phone.rec_key NO-LOCK: 
                     CREATE ttNotes.
                     ASSIGN 
                         ttNotes.cust_no         = cust.cust-no
                         ttNotes.contact         = cust.contact
                         ttNotes.note_date       = notes.note_date       
                         ttNotes.note_time       = notes.note_time       
                         ttNotes.viewed          = notes.viewed       
                         ttNotes.note_title      = notes.note_title
                         ttNotes.user_id         = notes.user_id         
                         ttNotes.note_text       = notes.note_text           
                         ttNotes.rec_key         = notes.rec_key .
                  END. /*FOR EACH Notes*/
               END. /* FOR EACH phone*/
           END. /* FOR EACH cust*/
         END.
        END.



