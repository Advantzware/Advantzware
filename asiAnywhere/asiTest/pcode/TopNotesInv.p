/*------------------------------------------------------------------------
    File        : TopNotesInv.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all AdderLook

    Author(s)   : 
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTopViewNoteinvoice NO-UNDO 
    FIELD vNoteDate     AS DATE FORMAT "99/99/9999":U
    FIELD vNoteTime     AS CHAR FORMAT "X(11)":U
    FIELD vNoteTitle    AS CHAR FORMAT "X(50)":U
    FIELD vNoteText     AS CHAR 
    FIELD vUserId       AS CHAR FORMAT "X(12)":U
    FIELD vDeptCode     AS CHAR FORMAT "X(2)":U
    FIELD vDeptName     AS CHAR 
    FIELD vNoteFrmNo    AS INT FORMAT ">9":U
    FIELD vNoteViewed     AS CHAR 
    FIELD rec_key       AS CHAR
    FIELD notetype      AS CHAR 
    FIELD notegroup     AS CHAR      .

DEFINE DATASET dsTopViewNoteinvoice FOR ttTopViewNoteinvoice.

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
DEFINE INPUT PARAMETER prmType     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmGroup     AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopViewNoteinvoice.
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
    IF prmGroup        = ? THEN ASSIGN prmGroup = "".
    IF prmType         = ? THEN ASSIGN prmType = "C".


DEF VAR prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "GridSelect" THEN DO:
  
     FOR EACH notes WHERE notes.rec_key = prmRecKey
        AND notes.note_type <> "o" 
        /*AND (notes.note_date = prmNoteDate OR prmNoteDate = ?) */
        AND (notes.note_group    = prmGroup  OR prmGroup = "") 
        AND (notes.note_code = prmDeptCode OR prmDeptCode = "") NO-LOCK:
         
         CREATE ttTopViewNoteinvoice.
    ASSIGN
        ttTopViewNoteinvoice.vNoteDate = notes.note_date
        ttTopViewNoteinvoice.vNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopViewNoteinvoice.vNoteTitle = notes.note_title
        ttTopViewNoteinvoice.vNoteText  = notes.note_text
        ttTopViewNoteinvoice.vUserId    = notes.user_id
        ttTopViewNoteinvoice.vDeptCode  = notes.note_code
        ttTopViewNoteinvoice.vNoteFrmNo = notes.note_form_no
        ttTopViewNoteinvoice.rec_key     = string(rowid(notes))
        ttTopViewNoteinvoice.notegroup     = notes.note_group
        ttTopViewNoteinvoice.vNoteViewed  = IF notes.viewed = TRUE THEN "Yes" ELSE "No"
                 .
    find first dept where dept.code eq notes.note_code no-lock no-error.
    IF AVAIL dept THEN
    ASSIGN ttTopViewNoteinvoice.vDeptName = dept.dscr . /*THEN  dept.dscr else "".*/
     END.
     FOR EACH ttTopViewNoteinvoice WHERE ttTopViewNoteinvoice.vDeptName = ? NO-LOCK:
         ASSIGN ttTopViewNoteinvoice.vDeptName = "" .
     END.
 END.

/*********************************Add*********************************/
    IF prmAction = "Add" THEN DO:
         
        IF prmDeptCode NE '' THEN DO:
            
            FIND FIRST dept NO-LOCK WHERE dept.code EQ prmDeptCode NO-ERROR.
            IF NOT AVAIL dept THEN DO:
              ASSIGN  cError =  "Invalid dept code, try help..." .
                    RETURN .
            END.
          
          END.
          IF prmGroup <> "" THEN DO:
             FIND first usergrps where usergrps.usergrps = prmGroup NO-LOCK NO-ERROR.
             IF NOT AVAIL usergrps THEN do:
             cError = "Invalid Group. Try Help."  .
             return .
             END.
           END.


        CREATE notes.
        ASSIGN
            notes.note_date     = TODAY
            notes.note_time     = int(prmNoteTime)
            notes.user_id       = prmUser
            notes.viewed        = IF prmViewed = "Yes" THEN TRUE ELSE FALSE
            notes.note_code     = prmDeptCode
            notes.note_title    = prmNoteTitle
            notes.note_text     = prmNoteText
            notes.note_form_no  = prmForm 
            notes.rec_key       =  prmHeader 
            notes.note_group    = prmGroup 
            notes.note_type     = prmType
                .

ASSIGN 
        prmRecKey = string(rowid(notes))
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
          
          END.

          IF prmGroup <> "" THEN DO:
             FIND first usergrps where usergrps.usergrps = prmGroup NO-LOCK NO-ERROR.
             IF NOT AVAIL usergrps THEN do:
             cError = "Invalid Group. Try Help."  .
             return .
             END.
         END.

        FIND FIRST notes WHERE notes.rec_key = prmHeader
         AND rowid(notes) = TO-ROWID(prmRecKey)
          AND notes.note_type <> "o"   EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            notes.note_code = prmDeptCode
            notes.note_title = prmNewNoteTitle
            notes.note_text  = prmNoteText
            notes.note_group = prmGroup 
            notes.note_type  = prmType.

        ASSIGN
           prmAction = "Select" .
      END.

/*********************************End Update*********************************/

/*********************************Delete*************************************/
    IF prmAction = "Delete" THEN DO:

        FIND FIRST notes WHERE notes.rec_key = prmHeader
         AND rowid(notes) = TO-ROWID(prmRecKey)
          AND notes.note_type <> "o"   EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL notes THEN DELETE notes.

FIND LAST notes WHERE notes.rec_key = prmHeader
         NO-LOCK NO-ERROR.
IF AVAIL notes THEN 
        ASSIGN
            prmRecKey = STRING(ROWID(notes))
            prmNoteDate = notes.note_date
            prmNoteTime = string(notes.note_time)
            prmNoteTitle = notes.note_title
            prmAction = "Select" .
    END.

/********************************End Delete*********************************/

/*********************************Select*********************************/
 IF prmAction = "Select" THEN DO:
 
     FOR EACH notes WHERE notes.rec_key = prmHeader
         AND rowid(notes) = TO-ROWID(prmRecKey)
          AND notes.note_type <> "o"   NO-LOCK:
        
         CREATE ttTopViewNoteinvoice.
    ASSIGN
        ttTopViewNoteinvoice.vNoteDate = notes.note_date
        ttTopViewNoteinvoice.vNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopViewNoteinvoice.vNoteTitle = notes.note_title
        ttTopViewNoteinvoice.vNoteText  = notes.note_text
        ttTopViewNoteinvoice.vUserId    = notes.user_id
        ttTopViewNoteinvoice.vDeptCode  = notes.note_code
        ttTopViewNoteinvoice.vNoteFrmNo = notes.note_form_no  
        ttTopViewNoteinvoice.notetype     = notes.note_type
         ttTopViewNoteinvoice.notegroup     = notes.note_group
        ttTopViewNoteinvoice.rec_key     = string(rowid(notes))
        ttTopViewNoteinvoice.vNoteViewed  = IF notes.viewed = TRUE THEN "Yes" ELSE "No"
                 .
    find first dept where dept.code eq notes.note_code no-lock no-error.

    IF AVAIL dept THEN
    ASSIGN ttTopViewNoteinvoice.vDeptName = dept.dscr . /*THEN  dept.dscr else "".*/
     END.

     FOR EACH ttTopViewNoteinvoice WHERE ttTopViewNoteinvoice.vDeptName = ? NO-LOCK:
         ASSIGN ttTopViewNoteinvoice.vDeptName = "" .
     END.
 END.
/*********************************End Select*********************************/
