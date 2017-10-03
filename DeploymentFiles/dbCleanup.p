&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : dbCleanup.p
    Purpose     : allows removal of notes duplicated when combining nosweat
                  and asinos databases; generates report of *possible* (but
                  not identical) notes. corrects preConversion data issues.
    Syntax      : RUN dbCleanup.p
    Author(s)   : MYT
    Created     : 09/26/17
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DISABLE TRIGGERS FOR LOAD OF notes.
DISABLE TRIGGERS FOR LOAD OF parmfile.
DISABLE TRIGGERS FOR LOAD OF userlog.
DISABLE TRIGGERS FOR LOAD OF mfdata.
DISABLE TRIGGERS FOR LOAD OF usercontrol.

DEF BUFFER bnotes FOR notes.
DEF STREAM outStream.

DEF VAR delCtr AS INT NO-UNDO.
DEF VAR dupCtr AS INT NO-UNDO.
DEF VAR cTemp AS CHAR NO-UNDO.
DEF VAR timestring AS CHAR NO-UNDO.
DEF VAR iUserCount AS INT NO-UNDO.
DEF VAR iExtra AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* Remove parmfile database connections */
FOR EACH parmfile:
    DELETE parmfile.
END.

/* Remove bad UDF entries */
FOR EACH mfdata:
    IF INDEX(mfdata.mfgroup_data,"General Class") <> 0 THEN
        DELETE mfdata.
END.

/* Remove userlogs for proper user control counting */
FOR EACH userlog:
    DELETE userlog.
END.

/* Create/update usercontrol record */
FIND FIRST usercontrol EXCLUSIVE NO-ERROR.
IF NOT AVAIL usercontrol THEN 
    CREATE usercontrol.
IF usercontrol.numLicensedUsers = 0 
OR usercontrol.numLicensedUsers = ? 
OR usercontrol.rec_key = ? THEN DO:
    UPDATE 
        iUserCount LABEL "Licensed Users"
        WITH FRAME a VIEW-AS DIALOG-BOX.
    ASSIGN
        timeString = STRING(time,"HH:MM:SS")
        timeString = REPLACE(timeString,":","") + "00"
        iExtra = MAXIMUM(1,INTEGER(iUserCount / 10))
        usercontrol.numLicensedUsers = iUserCount
        usercontrol.maxAllowedUsers = iUserCount + iExtra
        usercontrol.maxSessionsPerUser = 2
        usercontrol.numUsersOverLimit = iExtra
        usercontrol.rec_key = STRING(year(today),"9999") +
                             STRING(month(today),"99") +
                             STRING(day(today),"99") +
                             timeString.
    FIND FIRST rec_key WHERE
        rec_key.table_name = "usercontrol" AND
        rec_key.rec_key = usercontrol.rec_key
        NO-LOCK NO-ERROR.
    IF NOT AVAIL rec_key THEN DO:
        CREATE rec_key.
        ASSIGN
            rec_key.table_name = "usercontrol"
            rec_key.rec_key = usercontrol.rec_key.
    END.
    
END.

/* Remove duplicate notes */
ASSIGN
    delCtr = 0
    dupCtr = 0.
    
FOR EACH notes:
    /* Remove exact duplicates */
    FIND bnotes WHERE
        ROWID(bnotes) <> ROWID(notes) AND
        bnotes.note_code = notes.note_code AND
        bnotes.note_date = notes.note_date AND
        bnotes.note_form_no = notes.note_form_no AND
        bnotes.note_group = notes.note_group AND
        bnotes.note_text = notes.note_text AND
        bnotes.note_time = notes.note_time AND
        bnotes.note_title = notes.note_title AND
        bnotes.note_type = notes.note_type AND
        bnotes.rec_key = notes.rec_key AND
        bnotes.user_id = notes.user_id
        EXCLUSIVE NO-ERROR.
    IF AVAIL bnotes THEN DO:
        ASSIGN 
            delCtr = delCtr + 1.
        DELETE bnotes.
    END.
    
    /* Add additional field data - from WK populateNotesFields */
    IF notes.createDate = ? THEN ASSIGN 
        notes.updateDate   = notes.note_date  
        notes.updateTime   = notes.note_time 
        notes.updateUser   = notes.user_id
        notes.createDate   = notes.note_date  
        notes.createTime   = notes.note_time 
        notes.createUser   = notes.user_id         
        .
    
    /* Write possible duplicates to file */
    FIND bnotes WHERE
        ROWID(bnotes) <> ROWID(notes) AND
        bnotes.note_code = notes.note_code AND
        bnotes.note_date = notes.note_date AND
        bnotes.note_form_no = notes.note_form_no AND
        bnotes.note_group = notes.note_group AND
        bnotes.note_time = notes.note_time AND
        bnotes.note_title = notes.note_title AND
        bnotes.note_type = notes.note_type AND
        bnotes.rec_key = notes.rec_key AND
        bnotes.user_id = notes.user_id
        EXCLUSIVE NO-ERROR.
    IF AVAIL bnotes THEN DO:
        ASSIGN 
            dupCtr = dupCtr + 1.
        IF dupCtr = 1 THEN DO:
            ASSIGN
                FILE-INFO:FILE-NAME = "c:\tmp\."
                cTemp = IF FILE-INFO:FULL-PATHNAME <> ? THEN "c:\tmp" ELSE "".
            IF cTemp = "" THEN DO:
                ASSIGN
                    FILE-INFO:FILE-NAME = "c:\temp\."
                    cTemp = IF FILE-INFO:FULL-PATHNAME <> ? THEN "c:\temp" ELSE "".
            END.
            IF cTemp = "" THEN ASSIGN
                cTemp = OS-GETENV("TEMP").
            OUTPUT STREAM outstream TO VALUE(cTemp + "\dupNotes.txt").
            PUT STREAM outstream UNFORMATTED
                "List of possible duplicate notes detected on " + STRING(TODAY,"99/99/99") + CHR(10) + 
                "-----------------------------------------------------------------------------------------" + CHR(10) + 
                "Note Code Group     Type      Form      Date      Time      UserID       Text Begins     " + CHR(10) +
                "-----------------------------------------------------------------------------------------".
        END.
        PUT STREAM outstream   
            bnotes.note_code                    AT 1
            bnotes.note_group                   AT 11
            bnotes.note_type                    AT 21
            STRING(bnotes.note_form_no)         AT 31
            STRING(bnotes.note_date,"99/99/99") AT 41
            STRING(bnotes.note_time,"HH:MM:SS") AT 51
            bnotes.USER_id                      AT 61
            bnotes.note_text FORMAT "x(30)"     AT 74.
    END.
END.
IF dupCtr > 0 
OR delCtr > 0 THEN DO:
    OUTPUT STREAM outstream CLOSE.
    MESSAGE
        "Removed " + STRING(delCtr) + " duplicate records." SKIP
        IF dupCtr > 0 THEN "List of " + STRING(dupCtr) + " possible duplicates saved in file " + cTemp + "\dupNotes.txt" ELSE ""
        VIEW-AS ALERT-BOX INFO.
END.
ELSE MESSAGE
    "Could not find any duplicate note records."
    VIEW-AS ALERT-BOX INFO.
/* End dupe note processing */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


