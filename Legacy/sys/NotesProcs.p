
/*------------------------------------------------------------------------
    File        : NotesProcs.p
    Purpose     : Mulltiple Procs and Functions for processing Notes

    Syntax      :

    Description : Persistent Procedure file for all Notes processing and text manipulation

    Author(s)   : BV
    Created     : Sun Sep 09 20:35:41 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttLine
    FIELD cTextLine   AS CHARACTER 
    FIELD iLengthLine AS INTEGER.
    
DEFINE TEMP-TABLE ttWord
    FIELD riLineParent AS ROWID 
    FIELD cTextWord    AS CHARACTER 
    FIELD iLengthWord  AS INTEGER.
     
DEFINE VARIABLE gTypeShipNote AS CHARACTER NO-UNDO INIT "ES".
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fIsLineEnd RETURNS LOGICAL PRIVATE
    (ipcChar AS CHARACTER) FORWARD.

FUNCTION fIsWordEnd RETURNS LOGICAL PRIVATE
    (ipcChar AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE ConvertToArray:
    /*------------------------------------------------------------------------------
     Purpose: Converts a given blob of text into a 100 extent array based on 
                the character count per line.
     Notes:  Option to break line by a space
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTextToConvert AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiMaxCharCount AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcParsedText AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE OUTPUT PARAMETER opiFilledArraySize AS INTEGER NO-UNDO.

    DEFINE VARIABLE iLineTempCount AS INTEGER NO-UNDO.

    /*Parse the text into lines and words*/
    RUN pParseText(ipcTextToConvert).

    FOR EACH ttLine:
        /*Assign the line into each array element*/
        opiFilledArraySize = opiFilledArraySize + 1.
        opcParsedText[opiFilledArraySize] = ttLine.cTextLine.
        IF ttLine.iLengthLine GT ipiMaxCharCount THEN 
        DO: /*Too large to fit into one array element*/
            ASSIGN 
                iLineTempCount                    = 0
                opcParsedText[opiFilledArraySize] = "".
            FOR EACH ttWord WHERE ttWord.riLineParent EQ ROWID(ttLine):  /*Process each word for a given line*/
                IF ttWord.iLengthWord GE ipiMaxCharCount THEN   /*If the word is too large for the line itself*/
                    ASSIGN 
                        opcParsedText[opiFilledArraySize] = ttWord.cTextWord
                        opiFilledArraySize                = opiFilledArraySize + 1.
                ELSE 
                DO:  /*Break up the line*/
                    iLineTempCount = iLineTempCount + ttWord.iLengthWord.
                    IF iLineTempCount GT ipiMaxCharCount THEN 
                    DO:
                        ASSIGN 
                            iLineTempCount     = ttWord.iLengthWord
                            opiFilledArraySize = opiFilledArraySize + 1
                            .
                    END.
                    opcParsedText[opiFilledArraySize] = opcParsedText[opiFilledArraySize] + ttWord.cTextWord. 
                END. /*Break up the line*/       
            END. /*Each ttWord*/           
        END.  /*Line Break up to fit under ipcCharacter Count*/
    END.  /*Each ttLine*/

END PROCEDURE.

PROCEDURE CopyNoteOfType:
/*------------------------------------------------------------------------------
 Purpose: Propagates a single note from one rec_key to another
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

DEFINE VARIABLE cNoteText AS CHARACTER NO-UNDO.

RUN GetNoteOfType (ipcRecKeyFrom, ipcType, OUTPUT cNoteText).
IF cNoteText NE "" THEN 
    RUN UpdateNoteOfType (ipcRecKeyTo, ipcType, cNoteText).

END PROCEDURE.

PROCEDURE CopyNotes:
    /*------------------------------------------------------------------------------
     Purpose: Copies all notes from one source record (rec_key) to another 
     Notes:  Filter the scope of notes by type or code (commas separated or left blank for all)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTypes AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCodes AS CHARACTER NO-UNDO.

    DEFINE BUFFER b-notes FOR notes.

    FOR EACH notes NO-LOCK 
        WHERE notes.rec_key EQ ipcRecKeyFrom
        AND (ipcTypes EQ "" OR LOOKUP(notes.note_type,ipcTypes) GT 0)
        AND (ipcCodes EQ "" OR LOOKUP(notes.note_code,ipcCodes) GT 0)
        :
        CREATE b-notes.
        BUFFER-COPY notes EXCEPT rec_key TO b-notes.
        ASSIGN 
            b-notes.rec_key = ipcRecKeyTo.
    END.
END PROCEDURE.

PROCEDURE CopyShipNote:
/*------------------------------------------------------------------------------
 Purpose: Wrapper of CopyNoteOfType for Ship Note Type
 Notes:
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
 DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.
 
 RUN CopyNoteOfType (ipcRecKeyFrom, ipcRecKeyTo, gTypeShipNote).

END PROCEDURE.

PROCEDURE GetNoteOfType:
/*------------------------------------------------------------------------------
 Purpose: Given a rec_key and type, will return the notes content of first note
 of given type
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcObjectRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcFullText AS CHARACTER NO-UNDO.

FIND FIRST notes NO-LOCK 
    WHERE notes.rec_key EQ ipcObjectRecKey
        AND notes.note_type EQ ipcType
    NO-ERROR.
IF AVAILABLE notes THEN 
    opcFullText = notes.note_text.
    
END PROCEDURE.

PROCEDURE GetNotesArrayForObject:
/*------------------------------------------------------------------------------
 Purpose: Given a rec_key for an object, a set of type and codes, return an array of a max char length 
 Notes:  Types and Codes should be comma separated or left blank for all
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcObjectRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTypes AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCodes AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiMaxCharCount AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iplIncludeTitles AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcParsedText AS CHARACTER NO-UNDO EXTENT 100.
DEFINE OUTPUT PARAMETER opiArraySize AS INTEGER NO-UNDO.

DEFINE VARIABLE cFullText AS CHARACTER NO-UNDO.

 FOR EACH notes NO-LOCK 
        WHERE notes.rec_key EQ ipcObjectRecKey
        AND (ipcTypes EQ "" OR LOOKUP(notes.note_type,ipcTypes) GT 0)
        AND (ipcCodes EQ "" OR LOOKUP(notes.note_code,ipcCodes) GT 0)
        :
    IF iplIncludeTitles THEN 
        cFullText = cFullText + TRIM(CAPS(notes.note_title)) + CHR(13).
    cFullText = cFullText + TRIM(notes.note_text) + CHR(13).  
END.

IF cFullText NE "" THEN DO:
    cFullText = TRIM(cFullText,CHR(13)). 
    RUN ConvertToArray(cFullText, ipiMaxCharCount, OUTPUT opcParsedText, OUTPUT opiArraySize).
END. 
END PROCEDURE.

PROCEDURE pParseText PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Parses text into Line temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTextToParse AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iCharCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cChar       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iWordCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLineCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotalChars AS INTEGER   NO-UNDO.

    iTotalChars = LENGTH(ipcTextToParse).
    IF iTotalChars GT 0 THEN 
    DO:
        CREATE ttLine.
        CREATE ttWord.
        ASSIGN 
            ttWord.riLineParent = ROWID(ttLine).
        DO iCharCount = 1 TO iTotalChars:
            ASSIGN 
                cChar            = SUBSTR(ipcTextToParse, iCharCount, 1)
                ttWord.cTextWord = ttWord.cTextWord + cChar
                ttLine.cTextLine = ttLine.cTextLine + cChar
                iWordCount       = iWordCount + 1
                iLineCount       = iLineCount + 1.
            IF fIsWordEnd(cChar) AND NOT iCharCount EQ iTotalChars THEN 
            DO:
                IF fIsLineEnd(cChar) THEN 
                DO:
                    ASSIGN 
                        ttLine.iLengthLine = iLineCount
                        iLineCount         = 0.
                    CREATE ttLine.
                END.
                ASSIGN 
                    ttWord.iLengthWord = iWordCount
                    iWordCount         = 0.
                CREATE ttWord.
                ASSIGN 
                    ttWord.riLineParent = ROWID(ttLine).
            
            END.    
            IF iCharCount EQ iTotalChars THEN 
            DO:
                ASSIGN 
                    ttLine.iLengthLine = iLineCount
                    ttWord.iLengthWord = iWordCount.
            END.
        END.
    END.


END PROCEDURE.

PROCEDURE UpdateNoteOfType:
/*------------------------------------------------------------------------------
 Purpose: Given a rec_key and type, this proce will update the first note of that type
    or add it if it doesn't exist.
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcObjectRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcNoteType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcNoteText AS CHARACTER NO-UNDO.

FIND FIRST notes EXCLUSIVE-LOCK 
    WHERE notes.rec_key EQ ipcObjectRecKey
        AND notes.note_type EQ ipcNoteType
    NO-ERROR.
IF NOT AVAILABLE notes THEN DO:
    CREATE notes.
    ASSIGN 
        notes.rec_key = ipcObjectRecKey
        notes.note_type = ipcNoteType
        .
END. /*not avail notes*/

notes.note_text = ipcNoteText.

END PROCEDURE.

PROCEDURE UpdateShipNote:
/*------------------------------------------------------------------------------
 Purpose: Wrapper for UpdateNoteofType specifically for Ship Notes
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcObjectRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcNoteText AS CHARACTER NO-UNDO.

RUN UpdateNoteOfType (ipcObjectRecKey, gTypeShipNote, ipcNoteText).

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fIsLineEnd RETURNS LOGICAL PRIVATE
    ( ipcChar AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Determines if passed in character is a line end
     Notes: Carriage return.
    ------------------------------------------------------------------------------*/	
    RETURN ipcChar EQ CHR(10) OR ipcChar EQ CHR(13).  		

END FUNCTION.

FUNCTION fIsWordEnd RETURNS LOGICAL PRIVATE
    (ipcChar AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Determines if passed in character is a word end.
     Notes:
    ------------------------------------------------------------------------------*/	
    RETURN INDEX(" ,;-:&)", ipcChar) GT 0 OR fIsLineEnd(ipcChar).
	
END FUNCTION.

