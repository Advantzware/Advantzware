
/*------------------------------------------------------------------------
    File        : NotesProcs.i
    Purpose     : Add to definition for ability to return temp-tables from NotesProcs.

    Syntax      :

    Description : Temp-table definitions for NotesProcs.

    Author(s)   : BV
    Created     : Thu Aug 01 15:23:21 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttNotesFormatted
    FIELD rec_key AS CHARACTER
    FIELD noteTitle AS CHARACTER 
    FIELD noteType AS CHARACTER 
    FIELD noteCode AS CHARACTER 
    FIELD noteTextArray AS CHARACTER EXTENT 100
    FIELD noteTextArraySize AS INTEGER
    FIELD createdByUserID AS CHARACTER 
    FIELD createdDateTime AS DATETIME
    FIELD updatedByUserID AS CHARACTER 
    FIELD updatedDateTime AS DATETIME.
     
