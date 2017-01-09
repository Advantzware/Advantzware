/*********************************************************************
* Copyright (C) 2007,2011 by Progress Software Corporation. All rights    *
* reserved.  Prior versions of this work may contain portions        *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/

/* _dmpdata.p */ /**** Data Dictionary dump contents module ****/ 

/*
input:
  user_env[1] = comma-sep list of filenames to dump
                or it may be in user_longchar, if list was too big.
  user_env[2] = directory (if >1 file in user_env[1]) or filename to dump into
  user_env[3] = "MAP <name>" or "NO-MAP" OR ""
  user_env[4] = comma separated list of table numbers for table that we should not
                disable trigger for when the dump is done. All other tables will
                have triggers disabled by default.
                We may still get called with the old method if called from an
                old dumpo_d.p. For the old method:
                comma separated list of "y" (yes) or "n" (no) which
                      corresponds to file list in user_env[1], indicating for each,
                             whether triggers should be disabled when the dump is done.
  user_env[5] = "<internal defaults apply>" or "<target-code-page>"
  user_env[6] = "no-alert-boxes" or something else

History:
    Mario B     99/06/15    support for -numsep -numdec parameters
    hutegger    95/01/24    single-files in multiple schemas
    hutegger    94/02/22    added code-page-stuff
    D. McMann   02/03/03    Added lob directory
    K. McIntosh Apr 25, 2005  Added Auditing Support.
    K. McIntosh Jul 11, 2005  Changed logic to rule out just the 
                              tables that begin with "_aud" 20050711-026. 
    K. McIntosh Oct 19, 2005  Insert a slash between the directory and filename
                              just in case there isn't one already 20050928-004.
    fernando    Nov 03, 2005  Added code to audit dump operation                          
    fernando    Mar 14, 2006  Handle case with too many tables selected - bug 20050930-006. 
    fernando    Jun 19, 2007  Support for large files
    fernando    Dec 12, 2007  Improved how we use user_env[4].
    rkamboj     11/11/2011 Fixed issue of dump data for Lob field. bug OE00214956.
*/
/*h-*/

using Progress.Lang.*.

/* ensure that errors from directory functions are thrown   
   this also allows us to use errorHandler 
   this does not affect _runload.i whihc does not throw, bit handles all error   
*/ 
routine-level on error undo, throw.

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE NEW SHARED STREAM   dump.
DEFINE NEW SHARED VARIABLE recs AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
 
DEFINE VARIABLE cerr        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cntr        AS INTEGER   NO-UNDO.
DEFINE VARIABLE fil         AS CHARACTER NO-UNDO.
DEFINE VARIABLE loop        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE addfilename AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mdy         AS CHARACTER NO-UNDO.
DEFINE VARIABLE msg         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stamp       AS CHARACTER NO-UNDO.
DEFINE VARIABLE tableexpression       AS CHARACTER NO-UNDO.
define variable exceptfields as character no-undo.
DEFINE VARIABLE yy          AS INTEGER   NO-UNDO.
DEFINE VARIABLE stopped     AS LOGICAL   NO-UNDO init true.
DEFINE VARIABLE CodeOut     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lobdir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTemp       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSlash      AS CHARACTER   NO-UNDO.
 
DEFINE VARIABLE phdbname    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE numCount  AS INTEGER      NO-UNDO.
DEFINE VARIABLE ix        AS INTEGER      NO-UNDO.
DEFINE VARIABLE ilast     AS INTEGER      NO-UNDO.
DEFINE VARIABLE has_lchar AS LOGICAL      NO-UNDO.
DEFINE VARIABLE has_aud   AS LOGICAL      NO-UNDO.
DEFINE VARIABLE isCpUndefined AS LOGICAL  NO-UNDO.
DEFINE VARIABLE cRecords  AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dis_trig     AS CHARACTER NO-UNDO.
DEFINE VARIABLE old_dis_trig AS LOGICAL   NO-UNDO.
define variable isSuperTenant as logical no-undo. 
define variable cGroupName     as char     no-undo.
define variable lSkipCodePageValidation  as logical no-undo.
define variable xUseDefault    as character no-undo init "<default>":U.
DEFINE VARIABLE cMsg          AS CHARACTER NO-UNDO.
define variable lImmediatedisp as logical no-undo.
define variable xDumpTerminatedMsg as character no-undo init "Dump terminated.":U.

FORM
  DICTDB._File._File-name FORMAT "x(32)":U LABEL "Table":U  
  SPACE(0) fil            FORMAT "x(32)":U LABEL "Dump File":U SPACE(0)
  msg                     FORMAT "x(13)":U  LABEL "Records":U 
  HEADER 
    " Dumping Data.   Press ":U + 
    KBLABEL("STOP":U) + " to terminate the dump process.":U format "x(66)":U SKIP(1)
  WITH FRAME dumpdata NO-ATTR-SPACE USE-TEXT SCROLLABLE
  SCREEN-LINES - 8 DOWN ROW 2 CENTERED &IF "{&WINDOW-SYSTEM}" <> "TTY"
  &THEN THREE-D &ENDIF.


/*--------------------------- FUNCTIONS  --------------------------*/
function validDirectory returns logical ( cValue as char):
  
    IF cValue <> "" THEN 
    DO:
        /* not a propath search - use logic blank is default 
          - the directory is currently used as-is in output to and os-create
          
           */ 
        if not (cValue begins "/":U or cvalue begins "~\":U or index(cValue,":":U) <> 0) then
            cValue = "./":U + cValue.  
        ASSIGN FILE-INFORMATION:FILE-NAME = cValue. 
        return SUBSTRING(FILE-INFORMATION:FILE-TYPE,1,1) = "D":U.
    END.
        
    return true.
 
end function. /* validDirectory */

function createDirectoryIf returns logical ( cdirname as char):
    define variable iStat as integer no-undo.
    if not validdirectory(cdirname) then
    do:
        OS-CREATE-DIR VALUE(cdirname). 
        iStat = OS-ERROR. 
        if iStat <> 0 then
            undo, throw new AppError(xDumpTerminatedMsg + " Cannot create directory ":U + cdirname + ". System error:":U + string(iStat),?).
    end.
    return true.
end function. /* createDirectoryIf */


IF SESSION:CPINTERNAL EQ "undefined":U THEN
    isCpUndefined = YES.

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
  cSlash = "/".
&ELSE 
  cSlash = "~\":U.
&ENDIF
 
IF NOT isCpUndefined THEN 
DO:

  IF user_longchar <> "":U AND user_longchar <> ? THEN
     ASSIGN has_lchar = TRUE.

/* The only Audit table that can be dumped through this program is the
   _audit-event table, so we remove it from the templist and check for 
   instances of the _aud string in the table list. */

/* let's use a longchar in case the string is too big, and because
   the code below can be generic 
*/

   IF NOT has_lchar THEN
   do:
      user_longchar = user_env[1].
      has_lchar = true.
   end.   
END.

IF (isCpUndefined AND user_env[1] NE "":U AND user_env[1] NE ?)
   OR ((NOT isCpUndefined) AND (user_longchar NE "" AND user_longchar NE ?)) THEN 
DO:

   ASSIGN ix = (IF isCpUndefined 
                THEN INDEX(user_env[1],",_aud":U)
                ELSE INDEX(user_longchar,",_aud":U)).

   IF user_env[9] = "e":U THEN DO:

       ASSIGN iLast = (IF isCpUndefined 
                       THEN INDEX(user_env[1],"_aud-event":U)
                       ELSE INDEX(user_longchar,"_aud-event":U)).

       /* check if there is another aud table other than _aud-event */
        IF iLast NE 0 AND ix NE 0 AND ix = (iLast - 1) THEN
           ix = (IF isCpUndefined 
                 THEN INDEX(user_env[1],",_aud":U,ix + 1) 
                 ELSE INDEX(user_longchar,",_aud":U,ix + 1)).
   END.
   
   IF ix NE 0 THEN DO:
      IF NOT isCpUndefined AND NOT user_longchar BEGINS "_aud":U THEN
         has_aud = TRUE.

      IF isCpUndefined AND NOT user_env[1] BEGINS "_aud":U THEN
         has_aud = TRUE.
   END.
END.

/* free longchar if we don't need it */
IF NOT isCpUndefined AND NOT has_lchar THEN
   user_longchar = ?.

IF has_aud = TRUE THEN DO:
    cMsg =  "Dump Failed!":U + "~n":U +
        "You cannot dump Audit Policies or Data through this utility!":U . 
    
    if user_env[6] NE "dump-silent":U then
        MESSAGE cMsg
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     else 
          undo, throw new AppError(cMsg).
  RETURN.
END.

IF NOT CONNECTED(user_dbname) THEN 
DO:
   cMsg =  "Database ":U + user_dbname +
        "is not connected!":U . 
   if user_env[6] NE "dump-silent":U then
      MESSAGE cMsg
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   else 
      undo, throw new AppError(cMsg).     
    RETURN.
END.     

/* don't display frame */
if user_env[6] NE "dump-silent":U then 
do:
  PAUSE 0.
  lImmediatedisp = SESSION:IMMEDIATE-DISPLAY.
  SESSION:IMMEDIATE-DISPLAY = yes.
  VIEW FRAME dumpdata.
  run adecomm/_setcurs.p ("WAIT":U).
end.

/* check if we are supposed to skip the code page vaidation. 
   this is signaled by adding a skip in front of the actual code page.. 
   hacky, but what can you do with this code.   
   When silent we cannot have the message box appearing, so the code page check 
   throws an error instead of continuing. This setting allows us to bypass the 
   check. Typically set from UI after the error has occured first time . */  
if user_env[5] begins "skip-":U then
do:
    lSkipCodePageValidation = true.
    user_env[5] =  substr(user_env[5],6).
    if user_env[5] = '?':U then  user_env[5] = ?.
end.    

IF user_env[5] = " ":U OR user_env[5] = ?  THEN 
    assign user_env[5] = "<internal defaults apply>":U.

/* Used in numformat output */
CodeOut = IF user_env[5] = "<internal defaults apply>":U 
          THEN SESSION:CPINTERNAL
          ELSE user_env[5].

RUN "prodict/_dctyear.p":U (OUTPUT mdy,OUTPUT yy).

ASSIGN
  phDbName = PDBNAME("DICTDB":U) /* for logging audit event */
  cntr = 0
  addfilename = (IF has_lchar THEN INDEX(user_longchar,",":U) > 0 ELSE INDEX(user_env[1],",":U) > 0)
                or
                user_env[2] = "":U
                or
                user_env[2] = ".":U
                or
                user_env[2] matches "*/":U 
                or 
                user_env[2] matches "*~~~\":U. 
  
  loop = TRUE. /* use this to mark initial entry into loop */

PAUSE 5 BEFORE-HIDE.

IF has_lchar THEN
   numCount = NUM-ENTRIES(user_longchar).
ELSE
   numCount = NUM-ENTRIES(user_env[1]).

/* it user_env[5] has the old format, remember this */
/* this is just in case someone is using an old version of dump_d.p */
IF ENTRY(1, user_env[4]) = "y":U OR ENTRY(1, user_env[4]) = "n":U THEN
  ASSIGN old_dis_trig = YES.

if int(dbversion("dictdb":U)) > 10 then
do:
    isSuperTenant = can-find(first dictdb._tenant) and tenant-id("dictdb":U) < 0.
   
    /* if tenant was explicitly set by the tool then verify that it still matches
       the effective tenant also set there.
       The UI sets this and dump_d.p sets this when setEffectiveTenant is used 
       It is allowed to set-effective-tenant before running dump_d though (user_env[32]).
      */
    if isSuperTenant
    and user_env[32] <> "":U
    and user_env[32] <> get-effective-tenant-name("dictdb":U) then
    do:
        cMsg =  "Dump Aborted!":U + "~n":U +
        "The ABL get-effective-tenant-name returns":U  + quoter(get-effective-tenant-name("dictdb":U)) + 
        "while the effective tenant that was last selected in the Data Administration dump dialog or utility is":U + quoter(user_env[32]).
        if user_env[6] NE "dump-silent":U then   
            MESSAGE cMsg
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        else
            undo, throw new AppError(cMsg).
        RETURN.
    end.
    
end.

if addfilename then
do: 
   if not validDirectory(user_env[2]) then 
   do:
       cMsg = xDumpTerminatedMsg + " Directory ":U + user_env[2] + " does not exist.":U.
       if user_env[6] NE "dump-silent":U then   
           message cMsg
               view-as alert-box error buttons ok.
       else
           undo, throw new AppError(cMsg).
   end.
end.
 
DO ON STOP UNDO, LEAVE:
    /* if not specifically skippng this check and not the no-convert case, 
       check utf-8 case  */
    IF not lSkipCodePageValidation 
    and user_env[5] NE "<internal defaults apply>":U THEN 
    DO:
        FIND FIRST DICTDB._Db WHERE RECID(DICTDB._Db) = drec_db.
        IF DICTDB._Db._db-xl-name = "UTF-8":U 
        AND (SESSION:CHARSET NE "UTF-8":U OR TRIM(user_env[5]) NE "utf-8":U) THEN 
        DO:

            cMsg = "The database codepage is 'UTF-8' but -cpinternal and/or the codepage":U + "~n":U +
                   "for output is not, which can cause some data not to be dumped properly":U + "~n":U +
                   "if it cannot be represented in that codepage.":U + "~n":U +
                   "It's recommended that you dump the data from a session with the 'UTF-8'":U + "~n":U +
                   "codepage to avoid possible data corruption.":U.
            if user_env[6] = "dump-silent":U then 
            do:    
                undo, throw new AppError(cMsg).
            END.
            else if user_env[6] = "no-alert-boxes":U then 
            do:  /* output WITHOUT alert-box */
                MESSAGE cMsg.
            END.
            ELSE DO:
                MESSAGE cMsg
                    VIEW-AS ALERT-BOX WARNING.
            END.
        END.
    END.
  
    DO FOR DICTDB._File ix = 1 to numCount ON ERROR UNDO,NEXT:

        ASSIGN cTemp = IF has_lchar THEN ENTRY(ix,user_longchar) ELSE ENTRY(ix,user_env[1]).
    
        IF INTEGER(DBVERSION("DICTDB":U)) > 8 THEN 
            FIND DICTDB._File WHERE DICTDB._File._Db-recid = drec_db 
             AND DICTDB._File._File-name = cTemp 
             AND (DICTDB._File._Owner = "PUB":U OR DICTDB._File._Owner = "_FOREIGN":U).
        ELSE
            FIND DICTDB._File WHERE DICTDB._File._Db-recid = drec_db 
             AND DICTDB._File._File-name = cTemp.
            
        IF loop THEN .
        ELSE if user_env[6] NE "dump-silent":U  then 
        do:
            if FRAME-LINE(dumpdata) = FRAME-DOWN(dumpdata) THEN
                UP FRAME-LINE(dumpdata) - 1 WITH FRAME dumpdata.
            ELSE
                DOWN 1 WITH FRAME dumpdata.
        end.
    
        /* if super tenant or use default    */ 
        if user_env[32] > "":U
        and (issupertenant or user_env[33] = xUseDefault) 
        and int(dbversion("dictdb":U)) > 10 
        and dictdb._file._file-attributes[1] 
        and addfilename then
        do:  
            cgroupName = "":U.
            if user_env[37] > "" then
                run GetGroupName(user_env[32],dictdb._file._file-number,OUTPUT cgroupname).
            
            if cgroupName > "":U then
            do:
                /* use default then [37] is a subdir name to append to root */
                if user_env[33] = xUseDefault then
                do:
                    if index(user_env[2],"/":U) > 0 then
                        cSlash = "/":U. 
                    fil = (IF user_env[2] EQ "":U OR user_env[2] EQ ".":U 
                           THEN "":U
                           ELSE RIGHT-TRIM(user_env[2],cSlash) + cSlash) 
                        +  user_env[37]. 
                    /* create the groups directory if necessary */    
                    createDirectoryIf(fil). 
                    /* add group name */
                    fil = fil 
                        + cSlash
                        + cGroupName 
                        + cSlash.
                    /* create the group name directory if necessary */    
                    createDirectoryIf(fil). 
                end.
                else do:
                    if index(user_env[37],"/":U) > 0 then
                        cSlash = "/":U. 
                    /* add group name to groupdir */
                    fil = (IF user_env[37] EQ ".":U THEN "":U
                           ELSE RIGHT-TRIM(user_env[37],cSlash) + cSlash)
                        + cgroupName 
                        + cSlash. 
                end.
                /* deal with lob dir */
                if user_env[33] = xUseDefault then
                do:
                    if user_env[40] > "":U then
                    do:
                        lobdir = user_env[40].
                        /* create the group name lob directory if necessary */    
                        createDirectoryIf(fil + lobdir). 
                    end.
                    else 
                        lobdir = "":U.
                end.
                /** somewhat questionable -
                   use tenant lob dir to check 
                   currently no var for group lob dir 
                   unless usedefault */
                else if user_env[34] > "":U then 
                    lobdir = "lobs":U.
                else
                    lobdir = "":U.
            end. /* cgroupName */
            else do: /* no group = tenant dump */   
                /* if use default use dir named from tenant create if necessary */ 
                if user_env[33] = xUseDefault then
                do:
                    if index(user_env[2],"/":U) > 0 then
                        cSlash = "/":U. 
                    fil = (IF user_env[2] EQ "" OR user_env[2] EQ ".":U 
                           THEN "":U
                           ELSE RIGHT-TRIM(user_env[2],cSlash) + cSlash) 
                        +  user_env[32] + cslash. 
                    /* create tenant directory if necessary */
                    createDirectoryIf(fil). 
                end. 
                else do:   
                    if index(user_env[33],"/":U) > 0 then
                        cSlash = "/":U. 
                     
                    fil = (IF user_env[33] EQ "":U OR user_env[33] EQ ".":U THEN "":U
                           ELSE RIGHT-TRIM(user_env[33],cSlash) + cSlash).
                end.
                
                /* deal with lob dir */
                if user_env[33] = xUseDefault then
                do:
                    if user_env[40] > "":U then
                    do:
                        lobdir = user_env[40].
                        /* create tenant lob directory if necessary */
                        createDirectoryIf(fil + lobdir). 
                    end.
                    else 
                        lobdir = "":U.
                end.
                else if user_env[34] > "":U THEN
                    lobdir = user_env[34].
                ELSE
                    lobdir = "":U.
            end.
            if addfilename then
               fil = fil             
                   + ( IF DICTDB._File._Dump-name = ?
                       THEN DICTDB._File._File-name
                       ELSE DICTDB._File._Dump-name
                      ) + ".d":U.         
             if valid-object(dictMonitor) then
             do: 
                 if cGroupName > "" then         
                    dictMonitor:StartTable(dictdb._file._file-name,"Group":U,cGroupName,fil).          
                 else 
                    dictMonitor:StartTable(dictdb._file._file-name,"Tenant":U,user_env[32],fil).          
             end. 
        end.
        else do:
            if index(user_env[2],"/":U) > 0 then
                cSlash = "/":U. 
            /* don't add file yet as we need the path to 
               add lob directory */    
            if addfilename then
                fil = (IF user_env[2] EQ "" OR user_env[2] EQ ".":U
                       THEN "":U
                       ELSE RIGHT-TRIM(user_env[2],cSlash) + cSlash). 
            else
                fil = user_env[2].
            
            if user_env[33] = xUseDefault then
            do:
                if user_env[40] > "":U then
                do:
                    if not addfilename then
                    do:
                        /* this should not happen.. 
                        _usrdump passes root-only also for single table 
                        in this case, but it is conceivable that 
                        we could get here with some combination of dump_d params  */
                        undo, throw new apperror("Cannot dump lobs using default location if directory is specified with file-name":U).
                    end.    
                    lobdir = user_env[40].
                    /* create lob directory if necessary */
                    createDirectoryIf(fil + lobdir). 
                end.
                else 
                    lobdir = "":U.
            end.
            else if user_env[30] > "":U then 
                lobdir = user_env[30].
            else 
                lobdir = "":U.  
            
            IF addfilename then
                fil = fil 
                    + (IF DICTDB._File._Dump-name = ?
                       THEN DICTDB._File._File-name
                       ELSE DICTDB._File._Dump-name) + ".d":U.
        
            if valid-object(dictMonitor) then
                dictMonitor:StartTable(dictdb._file._file-name,"Shared":U,"":U,fil).          
      
        end.        
        assign
            loop = FALSE
            recs = 0.
   
        if user_env[6] NE "dump-silent":U then 
        do: 
            DISPLAY DICTDB._File._File-name fil "Dumping":U @ msg WITH FRAME dumpdata.
            COLOR DISPLAY MESSAGES DICTDB._File._File-name fil msg WITH FRAME dumpdata.
        end.
        ASSIGN
            stamp = STRING(YEAR( TODAY),"9999":U) + "/":U
                  + STRING(MONTH(TODAY),"99":U  ) + "/":U
                  + STRING(DAY(  TODAY),"99":U  ) + "-":U
                  + STRING(TIME,"HH:MM:SS":U)
            cerr  = TRUE
            exceptfields = "":U
            tableexpression = " NO-LOCK":U.
        
        IF DICTDB._File._Prime-Index <> ? AND user_dbtype = "PROGRESS":U THEN 
        DO:
            FIND DICTDB._Index WHERE RECID(DICTDB._Index) = DICTDB._File._Prime-Index.
            IF NOT DICTDB._Index._Active THEN 
            DO:
                FIND FIRST DICTDB._Index OF DICTDB._File WHERE DICTDB._Index._Active NO-ERROR.
                IF NOT AVAILABLE DICTDB._Index THEN 
                DO:
                    cMsg = "Cannot dump {&PRO_DISPLAY_NAME} data when all indexes for a table are inactive.":U.
                    if user_env[6] NE "dump-silent":U then 
                    do:
                        DISPLAY "Error!":U @ msg WITH FRAME dumpdata.
                        COLOR DISPLAY NORMAL DICTDB._File._File-name fil msg WITH FRAME dumpdata.
                        MESSAGE cMsg.
                    end.
                    else
                        undo, throw new AppError(cMsg).
                    NEXT.
                END.
                tableexpression = "USE-INDEX ":U + DICTDB._Index._Index-name + tableexpression.
            END.
        END.
        if dictdb._file._file-name = "_user":U then
        do:
            exceptfields = "EXCEPT _Tenantid":U.  
        end.    
        else if dictdb._file._file-name = "_sec-authentication-domain":U then
        do:
/*             tableexpression = "WHERE DICTDB2._sec-authentication-domain._Domain-category = 0 " + tableexpression.*/
               exceptfields = "EXCEPT _Domain-id":U.
        end.
/*        else if dictdb._file._file-name = "_sec-authentication-system" then                                                     */
/*        do:                                                                                                                     */
/*             tableexpression = "WHERE (DICTDB2._sec-authentication-system._domain-type begins '_') = false "  + tableexpression.*/
/*        end.                                                                                                                    */
               
        DO ON ERROR UNDO,LEAVE:      /* code-page-stuf <hutegger> 94/02 */
            IF user_env[3] = "":U OR user_env[3] = "NO-MAP":U THEN 
            DO:
                IF user_env[5] = "<internal defaults apply>":U THEN 
                DO:
                    IF lobdir <> "":U THEN
                        OUTPUT STREAM dump TO VALUE(fil) LOB-DIR VALUE(lobdir)
                            NO-ECHO NO-MAP NO-CONVERT.
                    ELSE
                        OUTPUT STREAM dump TO VALUE(fil) 
                            NO-ECHO NO-MAP NO-CONVERT.
                END.
                ELSE DO:
                    IF lobdir <> "" THEN
                        OUTPUT STREAM dump TO VALUE(fil) LOB-DIR VALUE(lobdir) NO-ECHO NO-MAP
                            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].
                    ELSE 
                        OUTPUT STREAM dump TO VALUE(fil) NO-ECHO NO-MAP
                            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].
                END.
            END.
            ELSE DO:
                IF user_env[5] = "<internal defaults apply>":U THEN 
                DO:
                    IF lobdir <> "":U THEN 
                        OUTPUT STREAM dump TO VALUE(fil) LOB-DIR VALUE(lobdir) NO-ECHO 
                            MAP VALUE(SUBSTRING(user_env[3],5,-1,"character":U))
                            NO-CONVERT.
                    ELSE
                        OUTPUT STREAM dump TO VALUE(fil) NO-ECHO 
                            MAP VALUE(SUBSTRING(user_env[3],5,-1,"character":U))
                            NO-CONVERT.
                END.
                ELSE DO:
                    IF lobdir <> "" THEN
                        OUTPUT STREAM dump TO VALUE(fil) LOB-DIR VALUE(lobdir) NO-ECHO 
                            MAP VALUE(SUBSTRING(user_env[3],5,-1,"character":U))
                            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].
                    ELSE 
                        OUTPUT STREAM dump TO VALUE(fil)  NO-ECHO 
                            MAP VALUE(SUBSTRING(user_env[3],5,-1,"character":U))
                            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].
                END.
            END.
            cerr = FALSE.
        END. 
        IF cerr THEN 
        DO:
            if user_env[6] NE "dump-silent":U then 
            do:
                DISPLAY "Error!":U @ msg WITH FRAME dumpdata.
                COLOR DISPLAY NORMAL DICTDB._File._File-name fil msg WITH FRAME dumpdata.
            end.
            else 
                undo, throw new AppError ("Error!":U).
            NEXT.
        END.
      
      
        /* Mike Fechner, Consultingwerk Ltd. 22.05.2014
           need to build directory name from file, when not lob-dir is set */
        DEFINE VARIABLE cLobDir AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.           
           
        IF lobdir > "":U THEN 
            ASSIGN cLobDir = lobdir . 
        ELSE DO:
            FILE-INFORMATION:FILE-NAME = fil .
            
            ASSIGN cFile = REPLACE (FILE-INFORMATION:FULL-PATHNAME, "~\":U, "/":U) . 
                   
            IF R-INDEX (cFile, "/":U) > 0 THEN 
                cLobDir = SUBSTRING (cFile, 1, R-INDEX (cFile, "/":U)) .
            ELSE 
                cLobDir = "./":U .                   
        END.
        
        CREATE ALIAS "DICTDB2":U FOR DATABASE VALUE(user_dbname).
    
        /* check if we can disable triggers. List now contains just the
          table numbers for tables that we should not disable triggers for.
        */
        ASSIGN dis_trig = ENTRY(1, user_env[4]).
        IF NOT old_dis_trig THEN 
        DO:
            IF dis_trig NE "":U AND dis_trig = STRING(_File._File-number) THEN
               ASSIGN dis_trig = "n":U.
            ELSE 
               ASSIGN dis_trig = "y":U.
        END.
        
        
        /* Mike Fechner, Consultingwerk Ltd. 22.05.2014
           Create a lob-free temp-table to workaround lack of dynamic EXPORT() method
           on a buffer handle */
        DEFINE VARIABLE hField           AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hBuffer          AS HANDLE    NO-UNDO.
        
        DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cLobs            AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFieldType       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cDefineTempTable AS CHARACTER NO-UNDO.
        
        ASSIGN cDefineTempTable = "DEFINE TEMP-TABLE ttDump NO-UNDO ~n":U 
               cLobs            = "":U.

        DO ON ERROR UNDO, THROW:
            CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE ("&1.&2":U, user_dbname, _File._File-name) . 
            
            DO i = 1 TO hBuffer:NUM-FIELDS:
            
/*                IF i MODULO 10 = 0 THEN                                                                      */
/*                    ASSIGN cDefineTempTable = cDefineTempTable + ".~nDEFINE TEMP-TABLE ttDump NO-UNDO ~n":U .*/
            
                ASSIGN hField     = hBuffer:BUFFER-FIELD (i) 
                       cFieldType = hField:DATA-TYPE.
            
                IF cFieldType = "BLOB":U or cFieldType = "CLOB":U THEN 
                    ASSIGN cLobs      = cLobs + hField:NAME + ",":U
                           cFieldType = "CHARACTER":U .
            
                ASSIGN cDefineTempTable = cDefineTempTable + SUBSTITUTE ("    FIELD &1 AS &2~n":U, hField:NAME, cFieldType) .            
            END.
            
            FINALLY:
                IF VALID-HANDLE (hBuffer) THEN 
                    DELETE OBJECT hBuffer .
            END FINALLY.
        END.
        
        ASSIGN cDefineTempTable = cDefineTempTable + ".":U 
               cLobs = TRIM (cLobs, ",":U).
        
        IF cLobs > "":U THEN DO:
        
            RUN "prodict/misc/_rundumpX.i":U (INPUT dis_trig, cLobs, cLobDir)
                        VALUE(_File._File-name) 
                        VALUE(tableexpression) 
                        VALUE(user_env[31])
                        VALUE(exceptfields)
                        VALUE(cDefineTempTable) 
                        VALUE (REPLACE (cLobs, ",":U, " ":U)).
                                           
        END . 
        ELSE DO:
        
            RUN "prodict/misc/_rundump.i":U (INPUT dis_trig)
                        VALUE(_File._File-name) 
                        VALUE(tableexpression) 
                        VALUE(user_env[31])
                        VALUE(exceptfields).            
        END. 
                    
        /* move on to the next one in the list */
        IF old_dis_trig OR (dis_trig = "n":U) THEN
           user_env[4] = SUBSTRING(user_env[4]
                                   ,LENGTH(ENTRY(1,user_env[4]),"character":U) + 2
                                   ,-1
                                   ,"character":U
                                   ).
        if valid-object(dictMonitor) then
            dictMonitor:EndTable(fil,int64(recs)).                
         
        /*------------------ Trailer-INFO ------------------*/
    
        /* if value is too large to fit in format, write it as it is */
        ASSIGN cRecords = STRING(recs,"9999999999999":U) NO-ERROR.
        IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
            ASSIGN cRecords = STRING(recs).
        
        {prodict/dump/dmptrail.i
            &entries      = "PUT STREAM dump UNFORMATTED
                          ""filename="":U   DICTDB._File._File-name SKIP
                          ""records="":U    cRecords SKIP
                          ""ldbname="":U    LDBNAME(user_dbname) SKIP
                          ""timestamp="":U  stamp SKIP
                          ""numformat="":U
                       STRING(ASC(
               SESSION:NUMERIC-SEPARATOR,CodeOut,SESSION:CPINTERNAL))     +
                       "","":U                                                      +                   STRING(ASC(
               SESSION:NUMERIC-DECIMAL-POINT,CodeOut,SESSION:CPINTERNAL))
                            SKIP
                            ""dateformat="":U mdy STRING(- yy) SKIP.
                        IF user_env[3] = ""NO-MAP"":U THEN
                          PUT STREAM dump UNFORMATTED ""map=NO-MAP"":U SKIP.
                        ELSE
                        IF user_env[3] <> """" THEN
                          PUT STREAM dump UNFORMATTED ""map=MAP:"":U 
                            SUBSTRING(user_env[3],4,-1,""character"":U) SKIP.
                        "  
            &seek-stream  = "dump"
            &stream       = "STREAM dump"
        }  /* adds trailer with code-page-entrie to end of file */
        
        /*------------------ Trailer-INFO ------------------*/
    
        OUTPUT STREAM dump CLOSE.
        
        if user_env[6] NE "dump-silent":U then 
        do:  
            COLOR DISPLAY NORMAL DICTDB._File._File-name fil msg WITH FRAME dumpdata.
            DISPLAY DICTDB._File._File-name fil WITH FRAME dumpdata.
            /* make sure value can fit in format */
            msg = STRING(recs,">>>>>>>>>>>>9":U) NO-ERROR.
            IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
                msg = "**********":U. /* too big to fit on the screen */
    
            DISPLAY msg WITH FRAME dumpdata.
        end.
        
        cntr = cntr + 1.
      
        /* audit dump of tables */
        AUDIT-CONTROL:LOG-AUDIT-EVENT(10213, 
                                      phDbName + ".":U +  DICTDB._File._File-name /* db-name.table-name */, 
                                      "":U /* detail */).
        /* this block has on error, undo next, so handle error so we can throw in case we are running silent */
        catch e as Progress.Lang.Error :
            run handleError(e).    
        end catch.      
    END. /* DO FOR DICTDB._File ix = 1 to numCount ON ERROR UNDO,NEXT:*/

    stopped = false.
END.  /* on stop */

if user_env[6] NE "dump-silent":U then 
do:
    DO WHILE FRAME-LINE(dumpdata) < FRAME-DOWN(dumpdata):
        DOWN 1 WITH FRAME dumpdata.
        CLEAR FRAME dumpdata NO-PAUSE.
    END.
end.

run adecomm/_setcurs.p ("":U).
 
if user_env[6] = "dump-silent":U then 
do:  /* output WITHOUT alert-box */
  IF stopped THEN
      undo, throw new AppError(xDumpTerminatedMsg).
end.      /* output WITHOUT alert-box */

else if user_env[6] = "no-alert-boxes":U then 
do:  /* output WITHOUT alert-box */

  IF stopped THEN
    MESSAGE xDumpTerminatedMsg.
  ELSE
    MESSAGE "Dump of database contents completed:":U 
                    cntr "table(s) dumped successfully.":U.
end.      /* output WITHOUT alert-box */
else do:  /* output WITH alert-box */

  IF stopped THEN
      MESSAGE xDumpTerminatedMsg
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  ELSE
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
      MESSAGE "Dump of database contents completed:" 
                    cntr "table(s) dumped successfully.".
      pause.
   &ELSE
      MESSAGE "Dump of database contents completed:":U SKIP
              cntr "table(s) dumped successfully.":U
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   &ENDIF
  
end.     /* output WITH alert-box */

RETURN.

catch e as Progress.Lang.Error :
    run handleError(e).    
end catch.

finally:
   IF NOT isCpUndefined THEN
       ASSIGN user_longchar = "":U.
   if user_env[6] = "dump-silent":U then 
   do:
      HIDE FRAME dumpdata NO-PAUSE.
      SESSION:IMMEDIATE-DISPLAY = if lImmediatedisp <> ? then lImmediatedisp else no.
   end.
end finally.

procedure handleError:
    define input parameter pError as Progress.Lang.Error no-undo.
    define variable i as integer no-undo.
    define variable cMsg as character no-undo.
    if pError:NumMessages = 0 and type-of(pError,AppError)then 
        cmsg = cast(perror,AppError):ReturnValue.
    else  
    do i = 1 to pError:NumMessages:
        cmsg = cmsg + pError:GetMessage(i) + "~n":U.
    end.  
    
    if user_env[6] NE "dump-silent":U then
        message cmsg
            view-as alert-box error.      
    else do:
        undo, throw new AppError(cmsg).
    end.
end.

procedure GetGroupName  :
    define input parameter pcTenant as character no-undo. 
    define input parameter piFileNumber as int no-undo. 
    define output parameter pcName as character no-undo. 
      
    find dictdb._tenant where dictdb._tenant._tenant-name = user_env[32] no-lock.   
             
    find dictdb._Partition-Set-Detail 
               where dictdb._Partition-Set-Detail._object-type = 1         
                 and dictdb._Partition-Set-Detail._object-number = piFileNumber        
                 and dictdb._Partition-Set-Detail._Tenantid = dictdb._tenant._tenantid 
                 no-lock no-error.
    if avail dictdb._Partition-Set-Detail then 
    do: 
        find dictdb._Partition-Set 
           where dictdb._Partition-Set._object-type = dictdb._Partition-Set-Detail._object-type        
             and dictdb._Partition-Set._object-number = dictdb._Partition-Set-Detail._object-number
             and dictdb._Partition-Set._PSetId = dictdb._Partition-Set-Detail._PSetId
             and dictdb._Partition-Set._PSet-Type = 1 no-lock.
          pcname = dictdb._Partition-Set._Pset-name.
    end.
end procedure.

 