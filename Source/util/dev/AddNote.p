
/*------------------------------------------------------------------------
    File        : AddNote.p
    Purpose     : 

    Syntax      :

    Description : Adds a note given rec_key and parameters and returns the rowid

    Author(s)   : BV
    Created     : Sun Feb 25 16:38:07 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER.
    DEFINE INPUT PARAMETER ipcText AS CHARACTER.
    DEFINE INPUT PARAMETER ipcTitle AS CHARACTER.
    DEFINE INPUT PARAMETER ipcCode AS CHARACTER.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER.
    DEFINE INPUT PARAMETER ipcGroup AS CHARACTER. 
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL.
    DEFINE OUTPUT PARAMETER opriNotes AS ROWID. 


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    DEFINE BUFFER bf-notes FOR notes.
    
    IF ipcText NE "" THEN 
    DO:
        FIND FIRST bf-notes EXCLUSIVE-LOCK 
            WHERE bf-notes.rec_key EQ ipcRecKey 
            AND bf-notes.note_title EQ ipcTitle 
            AND bf-notes.note_code EQ ipcCode 
            AND bf-notes.note_type EQ ipcType 
            NO-ERROR.
        IF NOT AVAILABLE bf-notes THEN DO: 
            CREATE bf-notes.
            ASSIGN
                bf-notes.rec_key    = ipcRecKey
                bf-notes.note_text  = ipcText
                bf-notes.note_title = ipcTitle
                bf-notes.note_code  = ipcCode
                bf-notes.note_type  = ipcType
                oplCreated          = YES
                .
        END.
        ASSIGN 
            bf-notes.note_group = ipcGroup
            bf-notes.note_text  = ipcText
            bf-notes.note_date  = TODAY
            bf-notes.note_time  = TIME            
            bf-notes.user_id    = USERID("NOSWEAT")            
            .
        opriNotes = ROWID(bf-notes).                    
    END.      
    
