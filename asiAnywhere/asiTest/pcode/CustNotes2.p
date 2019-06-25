

/*------------------------------------------------------------------------
    File        : CustNotes2.p
    Purpose     : Download Notes for ship to's contacts 

    Author(s)   : Kuldeep
    Created     : Mar 27 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttNotes2 NO-UNDO
    FIELD note_date         LIKE  notes.note_date     
    FIELD note_time         LIKE notes.note_time      
    FIELD viewed            LIKE notes.viewed    
    FIELD note_title        LIKE notes.note_title
    FIELD user_id           LIKE notes.user_id
    FIELD note_text         LIKE notes.note_text
    FIELD rec_key           LIKE notes.rec_key
    .
    DEFINE DATASET dsNotes2 FOR ttNotes2.

    DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmComp    AS CHARACTER  NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsNotes2.

    IF prmAction       = ?  THEN ASSIGN prmAction     = "Select".
    IF prmComp        = ?   THEN ASSIGN prmComp      = "".
    IF prmUser        = ?   THEN ASSIGN prmUser      = "".

    IF prmComp EQ "" THEN DO:
         FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.
         prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
   END.
   IF prmAction = "Select" THEN DO:
     FOR EACH cust WHERE cust.company = prmComp NO-LOCK:
         FOR EACH shipto WHERE shipto.cust-no = cust.cust-no NO-LOCK:
             FOR EACH phone WHERE phone.table_rec_key = shipto.rec_key NO-LOCK:
                 FOR EACH notes WHERE notes.rec_key = phone.rec_key NO-LOCK: 
                 CREATE ttNotes2.
                 ASSIGN 
                          
                     ttNotes2.note_date       = notes.note_date       
                     ttNotes2.note_time       = notes.note_time       
                     ttNotes2.viewed          = notes.viewed       
                     ttNotes2.note_title      = notes.note_title
                     ttNotes2.user_id         = notes.user_id         
                     ttNotes2.note_text       = notes.note_text           
                     ttNotes2.rec_key         = notes.rec_key .
                 END. /*FOR EACH Notes*/
               END. /* FOR EACH phone*/
           END. /* FOR EACH shipto*/
       END.  /*FOR EACH cust*/
    END.
      




