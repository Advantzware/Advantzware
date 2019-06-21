/*------------------------------------------------------------------------
    File        : AdderLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all AdderLook

    Author(s)   : 
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTopListNotes NO-UNDO 
    FIELD vNoteDate     AS DATE FORMAT "99/99/9999":U
    FIELD vNoteTime     AS CHAR FORMAT "X(11)":U
    FIELD vNoteTitle    AS CHAR FORMAT "X(50)":U
    FIELD vUserId       AS CHAR FORMAT "X(12)":U
    FIELD vNoteCode     AS CHAR FORMAT "X(2)":U
    FIELD vNoteFrmNo    AS INT FORMAT ">9":U
    FIELD vNoteType     AS CHAR FORMAT "X":U .

DEFINE DATASET dsTopListNotes FOR ttTopListNotes.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmHeader    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDate      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmDept      AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopListNotes.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.


    IF prmAction = ?  THEN ASSIGN prmAction = "".
    IF prmUser   = ?  THEN ASSIGN prmUser   = "".
    IF prmRecKey = ?  THEN ASSIGN prmRecKey = "".
    IF prmHeader = ?  THEN ASSIGN prmHeader = "".
    IF prmDept   = ?  THEN ASSIGN prmDept   = "".
    IF prmDate   = ?  THEN ASSIGN prmDate   = "".

DEF VAR prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/*************************Select****************************************/
 IF prmAction = "Select" THEN DO:
     FOR EACH notes WHERE notes.rec_key = prmRecKey
        AND notes.note_type <> "S" NO-LOCK:
         CREATE ttTopListNotes.
    ASSIGN
        ttTopListNotes.vNoteDate = notes.note_date
        ttTopListNotes.vNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopListNotes.vNoteTitle = notes.note_title
        ttTopListNotes.vUserId    = notes.user_id
        ttTopListNotes.vNoteCode  = notes.note_code
        ttTopListNotes.vNoteFrmNo = notes.note_form_no
        ttTopListNotes.vNoteType  = notes.note_type
                 .
     END.
 END.

/*************************End Select***********************************/

 /*************************Search**************************************/

    IF prmAction = "Search" THEN DO:
        FOR EACH notes WHERE notes.rec_key = prmRecKey AND (notes.note_date = date(prmDate) OR prmDate = "" )
            AND (notes.note_code = prmDept OR prmDept = "")
        AND notes.note_type <> "S" NO-LOCK:
         CREATE ttTopListNotes.
    ASSIGN
        ttTopListNotes.vNoteDate = notes.note_date
        ttTopListNotes.vNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopListNotes.vNoteTitle = notes.note_title
        ttTopListNotes.vUserId    = notes.user_id
        ttTopListNotes.vNoteCode  = notes.note_code
        ttTopListNotes.vNoteFrmNo = notes.note_form_no
        ttTopListNotes.vNoteType  = notes.note_type
                 .
     END.
    END.
    
 /*************************End Search**********************************/
