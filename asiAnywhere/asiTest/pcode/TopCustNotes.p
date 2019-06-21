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
DEFINE TEMP-TABLE ttTopCustNotes NO-UNDO 
    FIELD vCustDate        AS DATE FORMAT "99/99/9999":U
    FIELD vCustNoteTime    AS CHAR FORMAT "X(11)":U
    FIELD vCustNoteTitle   AS CHAR FORMAT "X(50)":U
    FIELD vCustCode        AS CHAR FORMAT "X(2)":U
    
    FIELD vNoteGroup       AS CHAR 
    FIELD vNoteViewed      AS CHAR 
    FIELD vNoteType        AS CHAR 
    FIELD vNoteText        AS CHAR 
    FIELD vUserId          AS CHAR FORMAT "X(12)":U  .

DEFINE DATASET dsTopCustNote FOR ttTopCustNotes.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmHeader    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmNoteDate    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmNoteTime    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUserId      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmViewed      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmGroup    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmNoteTitle    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmNewNoteTitle    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmNoteText     AS CHAR NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopCustNote.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.


    IF prmAction = ?  THEN ASSIGN prmAction = "".
    IF prmUser   = ?  THEN ASSIGN prmUser   = "".
    IF prmRecKey = ?  THEN ASSIGN prmRecKey = "".
    IF prmHeader = ?  THEN ASSIGN prmHeader = "".
    IF prmNoteTime = ?  THEN ASSIGN prmNoteTime = "".
    IF prmUserId   = ? THEN ASSIGN prmUserId = "".
    IF prmViewed   = ? THEN ASSIGN prmViewed = "".
    IF prmCode   = ? THEN ASSIGN prmCode = "".
    IF prmGroup   = ? THEN ASSIGN prmGroup = "".
    IF prmNoteTitle = ?  THEN ASSIGN prmNoteTitle = "".
    IF prmNoteText   = ? THEN ASSIGN prmNoteText = "".
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
     FOR EACH notes WHERE notes.rec_key = prmRecKey NO-LOCK:
         CREATE ttTopCustNotes.
    ASSIGN
        ttTopCustNotes.vCustDate = notes.note_date
        ttTopCustNotes.vCustNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopCustNotes.vCustNoteTitle = notes.note_title
        ttTopCustNotes.vUserId    = notes.user_id
        ttTopCustNotes.vCustCode  = notes.note_code
        ttTopCustNotes.vNoteGroup = notes.note_group
        ttTopCustNotes.vNoteType  = notes.note_type
                 .
     END.
 END.

/*************************End Select***********************************/

 /*************************Search**************************************/

    IF prmAction = "Search" THEN DO:
      
        FOR EACH notes WHERE notes.rec_key = prmRecKey AND (notes.note_date = date(prmNoteDate) OR prmNoteDate = "" )
            AND (notes.note_code BEGINS prmCode OR prmCode = "") AND (notes.note_group BEGINS prmGroup OR prmGroup = "")  NO-LOCK:
         CREATE ttTopCustNotes.
    ASSIGN
         ttTopCustNotes.vCustDate = notes.note_date
        ttTopCustNotes.vCustNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopCustNotes.vCustNoteTitle = notes.note_title
        ttTopCustNotes.vUserId    = notes.user_id
        ttTopCustNotes.vCustCode  = notes.note_code
        ttTopCustNotes.vNoteGroup = notes.note_group
        ttTopCustNotes.vNoteType  = notes.note_type
                 .
     END.
    END.
    
 /*************************End Search**********************************/

/*********************************Add*********************************/
    IF prmAction = "Add" THEN DO:
        IF prmCode <> "" THEN do:
            FIND FIRST dept where dept.code = prmCode  NO-LOCK NO-ERROR.
            IF NOT AVAIL dept THEN DO:
                ASSIGN
                    cError = "Invalid Deptartment Code. Try Help." .
                RETURN.
                END.
            END.

        if  prmGroup <> "" THEN do:
            FIND first usergrps where usergrps.usergrps = prmGroup  NO-LOCK NO-ERROR.
            IF NOT AVAIL usergrps THEN do:
                ASSIGN
                    cError =  "Invalid Group. Try Help." .
                return .
                end.
         END.

          
MESSAGE "Add" prmNoteTime prmUser prmCode prmNoteTitle prmNewNoteTitle prmNoteText  prmRecKey .
        CREATE notes.
        ASSIGN
            notes.note_date = TODAY
            notes.note_time = int(prmNoteTime)
            notes.user_id   = prmUser
             notes.note_source = "CUST"
            notes.viewed   = IF prmViewed = "Yes" THEN TRUE ELSE FALSE
            notes.note_code = prmCode
            notes.note_title = prmNoteTitle
            notes.note_text  = prmNoteText
            notes.rec_key      =  prmRecKey
           
            notes.note_group  = prmGroup 
            notes.note_type = prmHeader 
           
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
        MESSAGE "val" prmNoteTitle  prmNewNoteTitle prmCode prmGroup  .
        IF prmCode <> "" THEN do:
            FIND FIRST dept where dept.code = prmCode  NO-LOCK NO-ERROR.
            IF NOT AVAIL dept THEN DO:
                ASSIGN
                    cError = "Invalid Deptartment Code. Try Help." .
                RETURN.
             END.
         END.
        if  prmGroup <> "" THEN do:
            FIND first usergrps where usergrps.usergrps = prmGroup  NO-LOCK NO-ERROR.
            IF NOT AVAIL usergrps THEN do:
                ASSIGN
                    cError =  "Invalid Group. Try Help." .
                return .
                end.
         END.
        
            

        FIND FIRST notes WHERE notes.rec_key = prmRecKey
         AND notes.note_date = DATE(prmNoteDate)
         AND notes.note_time = int(prmNoteTime)
         AND notes.note_title = prmNoteTitle EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            notes.note_code = prmCode
            notes.note_title = prmNewNoteTitle
            notes.note_text  = prmNoteText
            notes.note_group  = prmGroup 
            notes.note_type = prmHeader  .

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

 MESSAGE "view" prmRecKey prmNoteDate prmNoteTime prmNoteTitle.
/*********************************Select*********************************/
 IF prmAction = "View" THEN DO:
     MESSAGE "test" prmRecKey prmNoteDate prmNoteTime prmNoteTitle .
     FOR EACH notes WHERE notes.rec_key = prmRecKey
         AND notes.note_date = DATE(prmNoteDate)
         AND notes.note_time = int(prmNoteTime)
         AND notes.note_title = prmNoteTitle NO-LOCK:
         CREATE ttTopCustNotes.
    ASSIGN
        ttTopCustNotes.vCustDate = notes.note_date
        ttTopCustNotes.vCustNoteTime = STRING(notes.note_time,'HH:MM:SS am')
        ttTopCustNotes.vCustNoteTitle = notes.note_title
        ttTopCustNotes.vNoteText  = notes.note_text
        ttTopCustNotes.vUserId    = notes.user_id
        ttTopCustNotes.vCustCode  = notes.note_code
        ttTopCustNotes.vNoteGroup = notes.note_group
        ttTopCustNotes.vNoteViewed  = IF notes.viewed = TRUE THEN "Yes" ELSE "No"
        ttTopCustNotes.vNoteType = notes.note_type
                 .
     
     END.
 END.
/*********************************End Select*********************************/

