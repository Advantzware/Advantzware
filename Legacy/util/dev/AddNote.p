
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
    DEFINE OUTPUT PARAMETER opriNotes AS ROWID. 


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

    IF ipcText NE "" THEN 
    DO:
        CREATE notes.
        ASSIGN
            notes.rec_key    = ipcRecKey
            notes.note_date  = TODAY
            notes.note_time  = TIME
            notes.note_text  = ipcText
            notes.note_title = ipcTitle
            notes.note_code  = ipcCode
            notes.user_id    = USERID("NOSWEAT")
            notes.note_type  = ipcType
            .
        opriNotes = ROWID(notes).                    
    END.      
    