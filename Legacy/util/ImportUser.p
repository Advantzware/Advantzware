/*------------------------------------------------------------------------
    File        : ImportUser.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for User	
    Author(s)   : Sewa
    Created     : 09-06-2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportUsers
    FIELD Company            AS CHARACTER 
    FIELD Location           AS CHARACTER 
    FIELD cUserID            AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "User Id" HELP "Required - Character - Size:8"
    FIELD cUserName          AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "User Name" HELP "Optional - Character - Size:30"
    FIELD cPassword          AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "User Password" HELP "Required - Character - Size:50"
    FIELD cAlias             AS CHARACTER FORMAT "X(40)" COLUMN-LABEL "Login Alias" HELP "Optional - Character - Size:40"
    FIELD cEmail             AS CHARACTER FORMAT "X(40)" COLUMN-LABEL "Email" HELP "Optional - Character - Size:40" 
    FIELD cPhoneCountry      AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Phone Country" HELP "Optional - Character - Size:8"
    FIELD cPhoneArea         AS CHARACTER FORMAT "xxx" COLUMN-LABEL "Phone Area" HELP "Optional - Character - Size:3"
    FIELD cPhone             AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Phone #" HELP "Optional - Character - Size:7"
    FIELD cFaxCountry        AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Fax Country" HELP "Optional - Character - Size:8"
    FIELD cFaxArea           AS CHARACTER FORMAT "xxx" COLUMN-LABEL "Fax Area" HELP "Optional - Character - Size:3"
    FIELD cFax               AS CHARACTER FORMAT "xxx-xxxx" COLUMN-LABEL "Fax #" HELP "Optional - Character - Size:7"
    FIELD iSecurityLevel     AS INTEGER   FORMAT ">999" COLUMN-LABEL "Security Level" HELP "Optional - Integer - Size:4" 
    FIELD cUserType          AS CHARACTER FORMAT "X(20)" COLUMN-LABEL "User Type" HELP "Optional - Character - Size:20"
    FIELD cImageViewer       AS CHARACTER FORMAT "X(80)" COLUMN-LABEL "Image Viewer" HELP "Optional - Character - Size:80"
    FIELD cReportPath        AS CHARACTER FORMAT "X(100)" COLUMN-LABEL "Report Path" HELP "Optional - Character - Size:80"
    FIELD cDocumentPath      AS CHARACTER FORMAT "X(100)" COLUMN-LABEL "Document Path" HELP "Optional - Character - Size:80"
    FIELD cIsDeveloper       AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Developer" HELP "Optional - Yes or N0"
    FIELD cTrackUsage        AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Track Usage" HELP "Optional - Yes or N0"
    FIELD cUserDefinedColors AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Use Defined Colors" HELP "Optional - Yes or N0"
    FIELD cUserDefinedFonts  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Use Defined Fonts" HELP "Optional - Yes or N0"
    FIELD cModes             AS CHARACTER FORMAT "X(120)" COLUMN-LABEL "Modes" HELP "Optional - PipeSeparated - Character - Size:120"
    FIELD cEnvironments      AS CHARACTER FORMAT "X(120)" COLUMN-LABEL "Environments" HELP "Optional - PipeSeparated - Character - Size:120"
    FIELD cDatabases         AS CHARACTER FORMAT "X(120)" COLUMN-LABEL "Databases" HELP "Optional - PipeSeparated - Character - Size:120"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportUsers"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportUsers FOR ttImportUsers.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportUsers FOR ttImportUsers.
    DEFINE VARIABLE iInd AS INTEGER NO-UNDO.
    DEFINE VARIABLE cUserType AS CHARACTER NO-UNDO.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportUsers.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportUsers.cUserID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: User Id".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportUsers.cPassword EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: User Password".
    END.
    
    
    IF oplValid THEN 
    DO:
        FIND FIRST users NO-LOCK
            WHERE users.user_id EQ ipbf-ttImportUsers.cUserID
            AND users.userAlias EQ ipbf-ttImportUsers.cAlias
            NO-ERROR .
        IF AVAILABLE users THEN 
        DO:
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate Exists:  Will be skipped"
                    .
            ELSE
                ASSIGN 
                    opcNote = "Update record - All fields to be overwritten"
                    .        
        END.
        ELSE 
            ASSIGN 
                opcNote = "Add record"
                .
    END.
    
    DO iInd = 1 TO NUM-ENTRIES(ipbf-ttImportUsers.cUserType, "|"):
       cUserType = ENTRY(iInd, ipbf-ttImportUsers.cUserType).
       IF LOOKUP(cUserType, "Full User,Production Floor,Administrator,Portal User") EQ 0 THEN 
         ASSIGN
           oplValid = NO
           opcNote = "Invalid User Type".
    END.
    
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportUsers FOR ttImportUsers.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-usercomp FOR usercomp.
    DEFINE VARIABLE cDefaultComp AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDefaultLoc  AS CHARACTER NO-UNDO.
    
    FIND FIRST users EXCLUSIVE-LOCK
        WHERE users.user_id EQ ipbf-ttImportUsers.cUserId
        NO-ERROR.  
    IF NOT AVAILABLE users THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE users.
        ASSIGN 
            users.user_id = ipbf-ttImportUsers.cUserId
            .
    END.
    
    
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportUsers.cUserName, iplIgnoreBlanks, INPUT-OUTPUT users.user_name).
    RUN pAssignValueC (ipbf-ttImportUsers.cAlias, iplIgnoreBlanks, INPUT-OUTPUT  users.userAlias).
    RUN pAssignValueC (ipbf-ttImportUsers.cEmail, iplIgnoreBlanks, INPUT-OUTPUT users.image_filename).
    RUN pAssignValueC (ipbf-ttImportUsers.cPhoneCountry, iplIgnoreBlanks, INPUT-OUTPUT users.phone-cnty).
    RUN pAssignValueC ((ipbf-ttImportUsers.cPhoneArea + ipbf-ttImportUsers.cPhone) , iplIgnoreBlanks, INPUT-OUTPUT users.phone).
    RUN pAssignValueC (ipbf-ttImportUsers.cFaxCountry, iplIgnoreBlanks, INPUT-OUTPUT users.fax-cnty).
    RUN pAssignValueC ((ipbf-ttImportUsers.cFaxArea + ipbf-ttImportUsers.cFax) , iplIgnoreBlanks, INPUT-OUTPUT users.fax).
    RUN pAssignValueI (ipbf-ttImportUsers.iSecurityLevel, YES, INPUT-OUTPUT users.securityLevel).
    IF ipbf-ttImportUsers.cUserType EQ "" THEN
        ipbf-ttImportUsers.cUserType = "Full User".
    RUN pAssignValueC (ipbf-ttImportUsers.cUserType, iplIgnoreBlanks, INPUT-OUTPUT users.userType).
    RUN pAssignValueC (ipbf-ttImportUsers.cImageViewer, iplIgnoreBlanks, INPUT-OUTPUT users.user_program[1]).
    RUN pAssignValueC (ipbf-ttImportUsers.cReportPath, iplIgnoreBlanks, INPUT-OUTPUT  users.user_program[2]).
    RUN pAssignValueC (ipbf-ttImportUsers.cDocumentPath, iplIgnoreBlanks, INPUT-OUTPUT   users.user_program[3]).
    RUN pAssignValueC (ipbf-ttImportUsers.cIsDeveloper, YES, INPUT-OUTPUT users.developer).
    RUN pAssignValueC (ipbf-ttImportUsers.cTrackUsage, YES, INPUT-OUTPUT users.track_usage).
    RUN pAssignValueC (ipbf-ttImportUsers.cUserDefinedColors, YES, INPUT-OUTPUT users.use_colors).
    RUN pAssignValueC (ipbf-ttImportUsers.cUserDefinedFonts, YES, INPUT-OUTPUT users.use_fonts).
    RUN pAssignValueC (ipbf-ttImportUsers.cModes, iplIgnoreBlanks, INPUT-OUTPUT   users.modeList).
    RUN pAssignValueC (ipbf-ttImportUsers.cEnvironments, iplIgnoreBlanks, INPUT-OUTPUT users.envList).
    RUN pAssignValueC (ipbf-ttImportUsers.cDatabases, iplIgnoreBlanks, INPUT-OUTPUT users.dbList).

    FIND FIRST _user EXCLUSIVE WHERE
        _user._userid = users.user_id
        NO-ERROR.
    IF NOT AVAILABLE _user THEN 
    DO:
        CREATE _user.
        ASSIGN
            _user._userid    = users.user_id
            _user._password  = ENCODE(ipbf-ttImportUsers.cPassword)
            _user._user-name = users.user_name.
    END.
    ELSE 
    DO:
        _user._password = ENCODE(ipbf-ttImportUsers.cPassword).
    END.
    
    /* Add default usercomp records for new user */
    FIND FIRST bf-usercomp NO-LOCK
        WHERE bf-usercomp.USER_id = "ASI"
        AND bf-usercomp.company_default NO-ERROR.
    
    ASSIGN 
        cDefaultComp = IF AVAILABLE bf-usercomp THEN bf-usercomp.company ELSE "001".
     
    FIND FIRST usercomp NO-LOCK
        WHERE  usercomp.USER_id = users.user_id
        AND usercomp.company = cDefaultComp
        AND usercomp.loc = "" NO-ERROR.
    IF NOT AVAILABLE usercomp THEN 
    DO:
        CREATE usercomp.
        ASSIGN 
            usercomp.user_id         = users.user_id
            usercomp.company         = IF AVAILABLE bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc             = ""
            usercomp.company_default = YES.
    END.
    
    FIND FIRST bf-usercomp NO-LOCK
        WHERE bf-usercomp.user_id = "ASI"
        AND bf-usercomp.loc_default NO-ERROR.

    ASSIGN 
        cDefaultLoc = IF AVAILABLE bf-usercomp THEN bf-usercomp.loc ELSE "MAIN".
    
    FIND FIRST usercomp NO-LOCK
        WHERE  usercomp.user_id = users.user_id
        AND usercomp.company = cDefaultComp 
        AND usercomp.loc = cDefaultLoc NO-ERROR.
    IF NOT AVAILABLE usercomp THEN 
    DO:
        CREATE usercomp.
        ASSIGN 
            usercomp.user_id     = users.user_id
            usercomp.company     = IF AVAILABLE bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc         = IF AVAILABLE bf-usercomp THEN bf-usercomp.loc ELSE "MAIN"
            usercomp.loc_DEFAULT = YES.
    END.
    
    FIND FIRST usr WHERE usr.uid EQ users.user_id NO-ERROR.
    IF NOT AVAILABLE usr THEN 
    DO:
        CREATE usr.
        ASSIGN
            usr.uid      = users.user_id
            usr.usr-lang = "English"
            usr.last-chg = TODAY.
    END.
    ELSE 
    DO:
        IF usr.usr-lang = "EN" THEN ASSIGN
                usr.usr-lang = "English".
        usr.last-chg = today.
    END.
    
END PROCEDURE.
