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
DEFINE TEMP-TABLE ttTopSpecNote NO-UNDO 
    FIELD vNoteDate     AS DATE FORMAT "99/99/9999":U
    FIELD vNoteTime     AS CHAR FORMAT "X(11)":U
    FIELD vNoteTitle    AS CHAR FORMAT "X(50)":U
    FIELD vNoteText     AS CHAR 
    FIELD vUserId       AS CHAR FORMAT "X(12)":U
    FIELD vDeptCode     AS CHAR FORMAT "X(2)":U
    FIELD vDeptName     AS CHAR 
    FIELD vNoteFrmNo    AS INT FORMAT ">9":U
    FIELD vNoteViewed     AS CHAR 
    FIELD vNoteType        AS CHAR .

DEFINE DATASET dsTopListNotes FOR ttTopSpecNote.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmHeader    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmNoteDate    AS CHAR  NO-UNDO.
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

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopListNotes.
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
    IF prmNewNoteTitle = ? THEN ASSIGN prmNewNoteTitle = "" .
    IF prmNoteDate     = ? THEN ASSIGN prmNoteDate = "" .


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
        AND notes.note_type = "S" NO-LOCK:
         CREATE ttTopSpecNote.
    ASSIGN
        ttTopSpecNote.vNoteDate = notes.note_date
        ttTopSpecNote.vNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopSpecNote.vNoteTitle = notes.note_title
        ttTopSpecNote.vUserId    = notes.user_id
        ttTopSpecNote.vDeptCode  = notes.note_code
        ttTopSpecNote.vNoteFrmNo = notes.note_form_no
        ttTopSpecNote.vNoteType  = notes.note_type
                 .
     END.
 END.

/*************************End Select***********************************/

 /*************************Search**************************************/

    IF prmAction = "Search" THEN DO:
        MESSAGE "search" prmDeptCode prmNoteDate.
        FOR EACH notes WHERE notes.rec_key = prmRecKey AND (notes.note_date = date(prmNoteDate) OR prmNoteDate = "" )
            AND (notes.note_code BEGINS prmDeptCode OR prmDeptCode = "")
        AND notes.note_type = "S" NO-LOCK:
         CREATE ttTopSpecNote.
    ASSIGN
        ttTopSpecNote.vNoteDate = notes.note_date
        ttTopSpecNote.vNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopSpecNote.vNoteTitle = notes.note_title
        ttTopSpecNote.vUserId    = notes.user_id
        ttTopSpecNote.vDeptCode  = notes.note_code
        ttTopSpecNote.vNoteFrmNo = notes.note_form_no
        ttTopSpecNote.vNoteType  = notes.note_type
                 .
     END.
    END.
    
 /*************************End Search**********************************/

/*********************************Add*********************************/
    IF prmAction = "Add" THEN DO:
         
       FIND FIRST item-spec
                    WHERE item-spec.company EQ prmComp
                      AND item-spec.i-no    EQ ""
                      AND item-spec.code    EQ prmDeptCode NO-LOCK NO-ERROR.
            IF NOT AVAIL item-spec THEN DO:
                ASSIGN
                    cError = "Invalid RM/FG Specfication, try help...".
                     RETURN.
            END.
          
MESSAGE "Add" prmNoteTime prmUser prmDeptCode prmNoteTitle prmNoteText prmForm prmRecKey .
        CREATE notes.
        ASSIGN
            notes.note_date = TODAY
            notes.note_time = int(prmNoteTime)
            notes.user_id   = prmUser
            notes.viewed   = IF prmViewed = "Yes" THEN TRUE ELSE FALSE
            notes.note_code = prmDeptCode
            notes.note_title = prmNoteTitle
            notes.note_text  = prmNoteText
            notes.note_form_no = prmForm 
            notes.rec_key      =  prmRecKey 
            notes.note_type = "S"
                .

ASSIGN 
        prmRecKey = notes.rec_key
        prmNoteDate = string(notes.note_date)    
        prmNoteTime = STRING(notes.note_time)
        prmNoteTitle = notes.note_title
        prmAction = "View"
        .

    END.

/*********************************End Add*********************************/

/*********************************Update*********************************/
    IF prmAction = "Update" THEN DO:

       FIND FIRST item-spec
                    WHERE item-spec.company EQ prmComp
                      AND item-spec.i-no    EQ ""
                      AND item-spec.code    EQ prmDeptCode NO-LOCK NO-ERROR.
            IF NOT AVAIL item-spec THEN DO:
                ASSIGN
                    cError = "Invalid RM/FG Specfication, try help...".
                     RETURN.
            END.
                     
       
          /*dept-dscr:SCREEN-VALUE = dept.dscr.*/
          /*IF notes.note_code:SCREEN-VALUE NE saveNoteCode THEN
              notes.note_title:SCREEN-VALUE = dept.dscr.*/
       
            
MESSAGE "val" prmNoteTitle  prmNewNoteTitle .
        FIND FIRST notes WHERE notes.rec_key = prmRecKey
         AND notes.note_date = DATE(prmNoteDate)
         AND notes.note_time = int(prmNoteTime)
         AND notes.note_title = prmNoteTitle EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            notes.note_code = prmDeptCode
            notes.note_title = prmNewNoteTitle
            notes.note_text  = prmNoteText
            notes.note_form_no = prmForm .

        ASSIGN
             prmNoteTitle = prmNewNoteTitle
             prmAction = "View" .
      END.

/*********************************End Update*********************************/

/*********************************Delete*************************************/
    IF prmAction = "Delete" THEN DO:

        FIND FIRST notes WHERE notes.rec_key = prmRecKey
         AND notes.note_date = date(prmNoteDate)
         AND notes.note_time = int(prmNoteTime)
         AND notes.note_title = prmNoteTitle EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL notes THEN DELETE notes.
FIND LAST notes WHERE notes.rec_key = prmRecKey
         NO-LOCK NO-ERROR.
IF AVAIL notes THEN 
        ASSIGN
            prmRecKey = notes.rec_key
            prmNoteDate = STRING(notes.note_date)
            prmNoteTime = string(notes.note_time)
            prmNoteTitle = notes.note_title
            prmAction = "View" .
    END.

/********************************End Delete*********************************/

/*********************************Select*********************************/
 IF prmAction = "View" THEN DO:
     
     FOR EACH notes WHERE notes.rec_key = prmRecKey
         AND notes.note_date = DATE(prmNoteDate)
         AND notes.note_time = int(prmNoteTime)
         AND notes.note_title = prmNoteTitle 
        /* AND notes.note_type = "S"*/ NO-LOCK:
         CREATE ttTopSpecNote.
    ASSIGN
        ttTopSpecNote.vNoteDate = notes.note_date
        ttTopSpecNote.vNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopSpecNote.vNoteTitle = notes.note_title
        ttTopSpecNote.vNoteText  = notes.note_text
        ttTopSpecNote.vUserId    = notes.user_id
        ttTopSpecNote.vDeptCode  = notes.note_code
        ttTopSpecNote.vNoteFrmNo = notes.note_form_no
        ttTopSpecNote.vNoteViewed  = IF notes.viewed = TRUE THEN "Yes" ELSE "No"
                 .
       MESSAGE "testpo"  notes.note_code .
           FIND FIRST item-spec NO-LOCK
               WHERE item-spec.company EQ prmComp
               AND item-spec.i-no    EQ ""
               AND item-spec.code    EQ notes.note_code  NO-ERROR.
           IF AVAIL item-spec THEN DO:
               ASSIGN
                  ttTopSpecNote.vDeptName = item-spec.notes[1]  . 

         END.
      FOR EACH ttTopSpecNote WHERE ttTopSpecNote.vDeptName = ? NO-LOCK:
         ASSIGN ttTopSpecNote.vDeptName = "" .
     END.
     END.
 END.
/*********************************End Select*********************************/

