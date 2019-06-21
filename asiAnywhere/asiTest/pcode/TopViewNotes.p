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
DEFINE TEMP-TABLE ttTopViewNotes NO-UNDO 
    FIELD vNoteDate     AS DATE FORMAT "99/99/9999":U
    FIELD vNoteTime     AS CHAR FORMAT "X(11)":U
    FIELD vNoteTitle    AS CHAR FORMAT "X(50)":U
    FIELD vNoteText     AS CHAR 
    FIELD vUserId       AS CHAR FORMAT "X(12)":U
    FIELD vDeptCode     AS CHAR FORMAT "X(2)":U
    FIELD vDeptName     AS CHAR 
    FIELD vNoteFrmNo    AS INT FORMAT ">9":U
    FIELD vNoteViewed     AS CHAR .

DEFINE DATASET dsTopViewNotes FOR ttTopViewNotes.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmHeader    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmNoteDate    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER prmNoteTime    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUserId      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmViewed      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDeptCode    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDeptName    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmForm        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmNoteTitle    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmNewNoteTitle    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmNoteText     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate     AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopViewNotes.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.


    IF prmAction = ?  THEN ASSIGN prmAction = "".
    IF prmUser   = ?  THEN ASSIGN prmUser   = "".
    IF prmRecKey = ?  THEN ASSIGN prmRecKey = "".
    IF prmHeader = ?  THEN ASSIGN prmHeader = "".
    IF prmNoteTime = ?  THEN ASSIGN prmNoteTime = "".
    IF prmUserId   = ? THEN ASSIGN prmUserId = "".
    IF prmViewed   = ? THEN ASSIGN prmViewed = "".
    IF prmDeptCode   = ? THEN ASSIGN prmDeptCode = "".
    IF prmDeptName   = ? THEN ASSIGN prmDeptName = "".
    IF prmForm   = ? THEN ASSIGN prmForm = 0.
    IF prmNoteTitle = ?  THEN ASSIGN prmNoteTitle = "".
    IF prmNoteText   = ? THEN ASSIGN prmNoteText = "".
    IF prmEstimate  = ?  THEN ASSIGN  prmEstimate = "".
    IF prmNewNoteTitle = ? THEN ASSIGN prmNewNoteTitle = "".

DEF VAR prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/*********************************Add*********************************/
    IF prmAction = "Add" THEN DO:
         
        IF prmDeptCode NE '' THEN DO:
            /*dept-dscr:SCREEN-VALUE = ''.*/
            FIND FIRST dept NO-LOCK WHERE dept.code EQ prmDeptCode NO-ERROR.
            IF NOT AVAIL dept THEN DO:
              ASSIGN  cError =  "Invalid dept code, try help..." .
                    RETURN .
            END.
          /*dept-dscr:SCREEN-VALUE = dept.dscr.*/
          /*IF notes.note_code:SCREEN-VALUE NE saveNoteCode THEN
              notes.note_title:SCREEN-VALUE = dept.dscr.*/
          END.
        IF prmForm <> 0 THEN DO:
                 FIND FIRST ef WHERE ef.company EQ prmComp
                     AND ef.est-no  EQ prmEstimate
                     AND ef.form-no EQ INT(prmForm) NO-LOCK NO-ERROR.
                 IF NOT AVAIL ef THEN DO:
                     ASSIGN cError = "Invalid Form" .
                     RETURN.
                 END.
         END.


        CREATE notes.
        ASSIGN
            notes.note_date = TODAY
            notes.note_time = int(prmNoteTime)
            notes.user_id   = prmUser
            notes.viewed   = IF prmViewed = "Yes" THEN TRUE ELSE FALSE
            notes.note_code = prmDeptCode
            notes.note_title = prmNewNoteTitle
            notes.note_text  = prmNoteText
            notes.note_form_no = prmForm 
            notes.rec_key      =  prmRecKey 
            notes.note_type = "D"
                .

ASSIGN 
        prmRecKey = notes.rec_key
        prmNoteDate = notes.note_date    
        prmNoteTime = STRING(notes.note_time)
        prmNoteTitle = notes.note_title
        prmAction = "Select"
        .

    END.

/*********************************End Add*********************************/

/*********************************Update*********************************/
    IF prmAction = "Update" THEN DO:

        IF prmDeptCode NE '' THEN DO:
            /*dept-dscr:SCREEN-VALUE = ''.*/
            FIND FIRST dept NO-LOCK WHERE dept.code EQ prmDeptCode NO-ERROR.
            IF NOT AVAIL dept THEN DO:
              ASSIGN  cError =  "Invalid dept code, try help..." .
                    RETURN .
            END.
          /*dept-dscr:SCREEN-VALUE = dept.dscr.*/
          /*IF notes.note_code:SCREEN-VALUE NE saveNoteCode THEN
              notes.note_title:SCREEN-VALUE = dept.dscr.*/
          END.

          IF prmForm <> 0 THEN DO:
                 FIND FIRST ef WHERE ef.company EQ prmComp
                     AND ef.est-no  EQ prmEstimate
                     AND ef.form-no EQ INT(prmForm) NO-LOCK NO-ERROR.
                 IF NOT AVAIL ef THEN DO:
                     ASSIGN cError = "Invalid Form" .
                     RETURN.
                 END.
         END.

        FIND FIRST notes WHERE notes.rec_key = prmRecKey
         AND notes.note_date = prmNoteDate
         AND notes.note_time = int(prmNoteTime)
         AND notes.note_title = prmNoteTitle EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            notes.note_code = prmDeptCode
            notes.note_title = prmNewNoteTitle
            notes.note_text  = prmNoteText
            notes.note_form_no = prmForm .

        ASSIGN
             prmNoteTitle = prmNewNoteTitle
             prmAction = "Select" .
      END.

/*********************************End Update*********************************/

/*********************************Delete*************************************/
    IF prmAction = "Delete" THEN DO:

        FIND FIRST notes WHERE notes.rec_key = prmRecKey
         AND notes.note_date = prmNoteDate
         AND notes.note_time = int(prmNoteTime)
         AND notes.note_title = prmNoteTitle EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL notes THEN DELETE notes.
FIND LAST notes WHERE notes.rec_key = prmRecKey
         NO-LOCK NO-ERROR.
IF AVAIL notes THEN 
        ASSIGN
            prmRecKey = notes.rec_key
            prmNoteDate = notes.note_date
            prmNoteTime = string(notes.note_time)
            prmNoteTitle = notes.note_title
            prmAction = "Select" .
    END.

/********************************End Delete*********************************/

/*********************************Select*********************************/
 IF prmAction = "Select" THEN DO:
     
     FOR EACH notes WHERE notes.rec_key = prmRecKey
         AND notes.note_date = prmNoteDate
         AND notes.note_time = int(prmNoteTime)
         AND notes.note_title = prmNoteTitle
        /* AND notes.note_type <> "S"*/ NO-LOCK:
         CREATE ttTopViewNotes.
    ASSIGN
        ttTopViewNotes.vNoteDate = notes.note_date
        ttTopViewNotes.vNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopViewNotes.vNoteTitle = notes.note_title
        ttTopViewNotes.vNoteText  = notes.note_text
        ttTopViewNotes.vUserId    = notes.user_id
        ttTopViewNotes.vDeptCode  = notes.note_code
        ttTopViewNotes.vNoteFrmNo = notes.note_form_no
        ttTopViewNotes.vNoteViewed  = IF notes.viewed = TRUE THEN "Yes" ELSE "No"
                 .
    find first dept where dept.code eq notes.note_code no-lock no-error.
    ASSIGN ttTopViewNotes.vDeptName = dept.dscr . /*THEN  dept.dscr else "".*/
     END.
     FOR EACH ttTopViewNotes WHERE ttTopViewNotes.vDeptName = ? NO-LOCK:
         ASSIGN ttTopViewNotes.vDeptName = "" .
     END.
 END.
/*********************************End Select*********************************/
