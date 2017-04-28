/*********************************************************************
* Copyright (C) 2005-2009 by Progress Software Corporation. All rights *
* reserved.  Prior versions of this work may contain portions        *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/

/* _loddata.p */ /**** Data Dictionary load contents module ****/

/*
  user_env[1] = comma-delim file list
                or it may be in user_longchar, if list was too big.
  user_env[2] = load path (for >1 file) or load filename (for 1 file)
  user_env[3] = "MAP <name>" or "NO-MAP" OR ""
  user_env[4] = error percentage
  user_env[5] = comma separated list of table numbers of tables not to disable
                triggers for. We'll disable triggers for all other tables.
                May bet called by old version of load_d.p with old method:
                comma separated list of "y" (yes) or "n" (no) which
                corresponds to file list in user_env[1], indicating for each,
                whether triggers should be disabled when the dump is done.
  user_env[10] = codepage (set in _usrload.p)

history
    D. McMann   03/03/03    Added support for LOB Directory and NO-LOBS
    D. McMann   02/11/19    Turned off suppress-warinings so that if checkwidth is set to 1
                            the code can catch warnings.
    D. McMann   00/06/30    Added assignement of load_size for Oracle
    D. McMann   00/02/07    Support for oracle bulk insert
    Mario B     99/06/15    Support for -numsep -numdec parameters
    mcmann      98/07/13    Added check for DBVERSION and _Owner for V9 finds
    laurief     98/06/09    removed any remaining code that defaults to
                            message alert-boxes by using user_env[6] as a
                            flag for screen or no-screen display, which is
                            now a toggle box in the data load dialog
    hutegger    95/06/26    removed message alert-boxes and output error
                            to <file>.e instead; and "!ERROR!" displayed 
                            in line on screen
    hutegger    95/05/08    PDBNAME brings "<dbname>.db" instead of
                            "<dbname>". So we work around it.
    gfs         94/07/25    use new lodtrail.i
    gfs         94/07/21    expanded "load" message for Windows
    hutegger    94/06/27    uses codepage of trailer if user_env[10] = ""
    gfs         94/06/24    removed lodtrail.i support, codepage now set
                            in _usrload.p and is available via user_env[10]
                            (bug 94-04-28-032)
    hutegger    94/02/24    code-page support
                            new error-message, new input-statments
    kmcintos    Apr 25, 2005  Added Auditing Support
    kmcintos    May 10, 2005  Changed logic to rule out tables beginning 
                              with "_aud".  Bug # 20050510-001
    fernando    Nov 03, 2005  Added code to audit dump operation         
    fernando    Mar 14, 2006  Handle case with too many tables selected - bug 20050930-006.                         
    fernando    Sep 14, 2006  Log error messages when a stop condition was raised - 20060905-013
    fernando    Jun 20, 2007  Support for large files
    fernando    Dec 12, 2007  Improved use of user_env[5]
    fernando    Nov  4, 2008  Output number of records to .ds file
*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

/* can only fit 8 digits in the recs value in the form */
&SCOPED-DEFINE MAX_RECS_FORMAT      "ZZZZZZZ9"
&SCOPED-DEFINE MAX_RECS_FORMAT_SIZE 8

DEFINE NEW SHARED STREAM   loaderr.
DEFINE NEW SHARED VARIABLE errs   AS INTEGER  NO-UNDO.
DEFINE NEW SHARED VARIABLE recs   AS INT64.   /*UNDO*/
DEFINE NEW SHARED VARIABLE xpos   AS INTEGER  NO-UNDO.
DEFINE NEW SHARED VARIABLE ypos   AS INTEGER  NO-UNDO.

DEFINE VARIABLE c          AS CHARACTER           NO-UNDO.
DEFINE VARIABLE cerror     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE cerrors    AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE VARIABLE codepage   AS CHARACTER           NO-UNDO init "UNDEFINED".
DEFINE VARIABLE d-ldbname  AS CHARACTER           NO-UNDO.
DEFINE VARIABLE d-was      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE dsname     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE error%     AS INTEGER             NO-UNDO.
DEFINE VARIABLE fil-d      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE fil-e      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE i          AS INT64               NO-UNDO.
DEFINE VARIABLE infinity   AS LOGICAL             NO-UNDO.
DEFINE VARIABLE irecs      AS INT64               NO-UNDO.
DEFINE VARIABLE crecs      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE l          AS LOGICAL             NO-UNDO.
DEFINE VARIABLE load_size  AS INT64               NO-UNDO.
DEFINE VARIABLE lpath      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE lvar       AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE lvar#      AS INTEGER             NO-UNDO.
DEFINE VARIABLE maptype    AS CHARACTER           NO-UNDO.
DEFINE VARIABLE mdy        AS CHARACTER           NO-UNDO.
DEFINE VARIABLE msg        AS CHARACTER           NO-UNDO.
DEFINE VARIABLE numformat  AS CHARACTER           NO-UNDO.
DEFINE VARIABLE terrors    AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE VARIABLE use_ds     AS LOGICAL             NO-UNDO.
DEFINE VARIABLE yy         AS INTEGER             NO-UNDO.
DEFINE VARIABLE stopped    AS LOGICAL   INIT TRUE NO-UNDO.
DEFINE VARIABLE NumProcRun AS LOGICAL             NO-UNDO.
DEFINE VARIABLE lobdir     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE cTemp      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE phDbName   AS CHARACTER           NO-UNDO.
DEFINE VARIABLE newAppCtx  AS LOGICAL   INIT NO   NO-UNDO.
DEFINE VARIABLE dis_trig   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE old_dis_trig  AS LOGICAL  NO-UNDO.

DEFINE VARIABLE numCount  AS INTEGER      NO-UNDO.
DEFINE VARIABLE ix        AS INTEGER      NO-UNDO.
DEFINE VARIABLE ilast     AS INTEGER      NO-UNDO.
DEFINE VARIABLE has_lchar AS LOGICAL      NO-UNDO.
DEFINE VARIABLE has_aud   AS LOGICAL      NO-UNDO.
DEFINE VARIABLE isCpUndefined AS LOGICAL  NO-UNDO.

DEFINE STREAM dsfile.

DEFINE FRAME loaddata
  DICTDB._File._File-name  COLUMN-LABEL "Table" FORMAT "x(18)" AT 2
    ATTR-SPACE SPACE(0)
  fil-d  FORMAT "x(27)"    COLUMN-LABEL "Load file"
  msg    FORMAT "x(8)"     COLUMN-LABEL "Records!Loaded"
  errs   FORMAT "ZZZZZZZ9" COLUMN-LABEL "Total  !Errors " 
    ATTR-SPACE SPACE(0)
  crecs  FORMAT "X({&MAX_RECS_FORMAT_SIZE})" COLUMN-LABEL "Expected! Records" SPACE(1)
  HEADER
    " Loading Data.   Press " +
    KBLABEL("STOP") + " to terminate the load process." format "x(66)" SKIP(1)
  WITH
  NO-ATTR-SPACE
  SCREEN-LINES - 7 DOWN ROW 1 CENTERED USE-TEXT
  &IF "{&WINDOW-SYSTEM}" <> "TTY"
  &THEN VIEW-AS DIALOG-BOX THREE-D SCROLLABLE WIDTH 84 TITLE "Load Table Contents"
  &ENDIF.  /* Move this here to scope above the internal procedure */

IF SESSION:CPINTERNAL EQ "undefined":U THEN
    isCpUndefined = YES.

IF NOT isCpUndefined THEN DO:
    IF user_longchar <> "" AND user_longchar <> ? THEN
       ASSIGN has_lchar = TRUE.
    
    /* The only Audit table that can be loaded through this program is the
       _audit-event table, so we remove it from the templist and check for 
       instances of the _aud string in the table list. */
    
    IF NOT has_lchar THEN
       user_longchar = user_env[1].
END.

IF (isCpUndefined AND user_env[1] NE "" AND user_env[1] NE ?)
   OR (NOT isCpUndefined AND user_longchar NE "" AND user_longchar NE ?) THEN DO:

   ASSIGN ix = (IF isCpUndefined 
                THEN INDEX(user_env[1],",_aud")
                ELSE INDEX(user_longchar,",_aud")).

   IF user_env[9] = "e" THEN DO:

       ASSIGN iLast = (IF isCpUndefined 
                       THEN INDEX(user_env[1],"_aud-event")
                       ELSE INDEX(user_longchar,"_aud-event")).

       /* check if there is another aud table other than _aud-event */
        IF iLast NE 0 AND ix NE 0 AND ix = (iLast - 1) THEN
           ix = (IF isCpUndefined 
                 THEN INDEX(user_env[1],",_aud",ix + 1) 
                 ELSE INDEX(user_longchar,",_aud",ix + 1)).
   END.
   
   IF ix NE 0 THEN DO:
      IF NOT isCpUndefined AND NOT user_longchar BEGINS "_aud" THEN
         has_aud = TRUE.

      IF isCpUndefined AND NOT user_env[1] BEGINS "_aud" THEN
         has_aud = TRUE.
   END.
END.

/* free longchar if we don't need it */
IF NOT isCpUndefined AND NOT has_lchar THEN
   user_longchar = ?.

IF has_aud THEN DO:
   MESSAGE "Load Failed!" SKIP(1)
           "You cannot load Audit Policies or Data through this utility!"
       VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   RETURN.
END.


/* LANGUAGE DEPENDENCIES START */ /*---------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 14 NO-UNDO INITIAL [
  /* 1*/ "Loading",
  /* 2*/ "Could not find load file",
  /* 3*/ "for table",
  /* 4*/ "Load of database contents completed.",
  /* 5*/ "Errors/Warnings listed in .e files placed into same directory as .d files",
  /* 6*/ "ERROR! Trailer indicated",
  /* 7*/ "records, but",
  /* 8*/ "records were loaded.",
  /* 9*/ "ERROR! -d <mdy> or -yy <n> settings of dump were",
  /*10*/ "but current settings are",
  /*11*/ "May cause load errors.",
  /*12*/ "!ERROR!",
  /*13*/ "Take a look at .ds file placed into same directory as Your database.",
  /*14*/ "Expected # of records is unknown.Error % interpreted as absolute #."
].

DEFINE VARIABLE no-file AS CHARACTER FORMAT "x(32)".
DEFINE VARIABLE fil-out AS CHARACTER FORMAT "x(32)".

/*----- Output the error messages for invalid numeric formats -----*/
PROCEDURE NumFormatErr:

     /* Logical Variable NumProcRun is external in order to scope to .p  */
     DEF INPUT PARAM ErrType AS CHAR NO-UNDO.

     OUTPUT STREAM loaderr TO VALUE(fil-e) NO-ECHO APPEND.

     IF NOT NumProcRun THEN
     DO:
        NumProcRun = NOT NumProcRun.
        DISPLAY new_lang[12] @ msg WITH FRAME loaddata. /* !ERROR! */           
        PUT STREAM loaderr UNFORMATTED
            "The numeric formats of this {&PRO_DISPLAY_NAME}-session"   SKIP
            "and the .d-file don't match!"                   SKIP   
            "Please exit {&PRO_DISPLAY_NAME} and start a new session"   SKIP.
     END.

     /* What type of error do we want to output this time through? */
     /* One or both of the following could be wrong.               */
     CASE ErrType:
     WHEN "EparmErr" THEN
     DO:
        PUT STREAM loaderr UNFORMATTED
           (IF numformat = "." THEN "without " ELSE "with " ) + 
            "the -E startup parameter, or set the"                 SKIP   
            "-numsep parameter to " +
            (IF numformat = "." THEN "44 " ELSE "46 ") + "and -numdec to " +
            (IF numformat = "." THEN "46." ELSE "44.") SKIP.    
     END.
     WHEN "SepErr" THEN
     DO:
        PUT STREAM loaderr UNFORMATTED
           "with the -numsep startup parameter set to " 
           ASC(CHR(INT(ENTRY(1,numformat)),SESSION:CPINTERNAL,codepage)) SKIP.
     END.
     WHEN "DecErr" THEN
     DO:
        PUT STREAM loaderr UNFORMATTED
           "with the -numdec startup parameter set to " 
           ASC(CHR(INT(ENTRY(2,numformat)),SESSION:CPINTERNAL,codepage)) SKIP.
     END.
     WHEN "FmtErr" THEN
     DO:
        PUT STREAM loaderr UNFORMATTED
          "with the -numdec and -numsep parameters set to valid values."  SKIP
          "The current value of " + numformat + " is not valid."
          SKIP.
     END.
     WHEN "BlnkErr" THEN
     DO:
        PUT STREAM loaderr UNFORMATTED
          "with valid values in the ~"numformat=~" section of the .d trailer."
          SKIP.
     END.
     END CASE.
     
     OUTPUT STREAM loaderr CLOSE.  

END PROCEDURE.

FORM
  DICTDB._File._File-name FORMAT "x(12)" LABEL "Table"
  fil-d                   FORMAT "x(14)" LABEL "Input File"
  recs                    FORMAT "ZZZZZZZZZZZZZZZZZZ9" LABEL "# of Records Read"
  errs                                   LABEL "# of Errors"
  fil-e                   FORMAT "x(14)" LABEL "Error File"
  WITH FRAME dsfile DOWN NO-BOX NO-ATTR-SPACE USE-TEXT.


/* LANGUAGE DEPENDENCIES END */ /*-----------------------------------------*/
/* Add the suppress warnings assignment so that when a warning is issued the code can put out into
   the .e file.  Warnings are generally suppressed throughout the dictionary but in this case we
   need to trap */
SESSION:SUPPRESS-WARNINGS = NO.
PAUSE 0.
VIEW FRAME loaddata.
PAUSE 5 BEFORE-HIDE.

SESSION:IMMEDIATE-DISPLAY = yes.
run adecomm/_setcurs.p ("WAIT").

RUN "prodict/_dctyear.p" (OUTPUT mdy,OUTPUT yy).
{ prodict/dictgate.i &action=query &dbtype=user_dbtype &dbrec=? &output=c }


ASSIGN
  load_size = INTEGER(ENTRY(4,c))
  infinity  = TRUE /* use this to mark initial entry into loop */
  use_ds    = (IF has_lchar THEN (INDEX(user_longchar,",") > 0) ELSE (INDEX(user_env[1],",") > 0))
  lpath     = user_env[2] /* load path */
  error%    = INTEGER(user_env[4])
  dsname    = (IF user_dbtype = "PROGRESS"
                THEN PDBNAME(user_dbname)
                ELSE user_dbname
              )
  cerrors   = 0 - integer(NOT use_ds).

IF user_env[30] = "" OR user_env[30] = ? THEN
  ASSIGN lobdir = "".
ELSE
  ASSIGN lobdir = user_env[30].

IF index(dsname,".db") > 0
 THEN SUBSTRING(dsname,INDEX(dsname,".db"),3,"RAW") = "".

IF use_ds THEN OUTPUT STREAM dsfile TO VALUE(dsname + ".ds") NO-ECHO.

/* auditing - start a new application context so that one can report
   all the records that are loaded as a group, if one wasn't set by the
   caller.
*/
IF AUDIT-CONTROL:APPL-CONTEXT-ID = ? THEN DO:
   ASSIGN newAppCtx = YES.
   AUDIT-CONTROL:SET-APPL-CONTEXT("Data Administration", "Load Table Contents", "").
END.

ASSIGN phDbName = PDBNAME("DICTDB"). /* get the physical db name - for auditing */

IF has_lchar THEN
   numCount = NUM-ENTRIES(user_longchar).
ELSE
   numCount = NUM-ENTRIES(user_env[1]).

/* it user_env[5] has the old format, remember this */
/* this is just in case someone is using an old version of load_d.p */
IF ENTRY(1, user_env[5]) = "y" OR ENTRY(1, user_env[5]) = "n" THEN
   ASSIGN old_dis_trig = YES.

stoploop:
DO ON STOP UNDO, LEAVE:
  DO ix = 1 to numCount /*WHILE ENTRY(1,user_env[1]) <> ""*/ :

   ASSIGN cTemp = IF has_lchar THEN ENTRY(ix,user_longchar) ELSE ENTRY(ix,user_env[1]).

   IF INTEGER(DBVERSION("DICTDB")) > 8 THEN
      FIND DICTDB._File WHERE DICTDB._File._File-name = cTemp
                          AND DICTDB._File._Db-recid = drec_db
                          AND (DICTDB._File._Owner = "PUB" OR DICTDB._File._Owner = "_FOREIGN").
    ELSE    
      FIND DICTDB._File WHERE DICTDB._File._File-name = cTemp
                          AND DICTDB._File._Db-recid = drec_db.
        
    IF infinity THEN .
    ELSE do:
       &IF "{&WINDOW-SYSTEM}" begins "MS-WIN"
       &THEN
         /* Nothing to do */
         down 1 with Frame loaddata.
       &ELSE
          IF FRAME-LINE(loaddata) = FRAME-DOWN(loaddata) THEN
             UP FRAME-LINE(loaddata) - 1 WITH FRAME loaddata.
          ELSE DOWN 1 WITH FRAME loaddata.
       &ENDIF
    END.
 
    ASSIGN
      infinity = FALSE
      recs     = 0
      irecs    = ?
      crecs    = STRING(irecs,{&MAX_RECS_FORMAT})
      d-was    = ?
      maptype  = ""
      fil-d    = (IF NOT use_ds THEN lpath
                 ELSE lpath + (IF DICTDB._File._Dump-name = ?
                 OR SEARCH(lpath + DICTDB._File._Dump-name + ".d") = ?
                 THEN DICTDB._File._File-name ELSE DICTDB._File._Dump-name)
               + (IF DICTDB._File._File-name BEGINS "_aud" THEN ".ad" ELSE ".d")).
    IF SEARCH(fil-d) <> ? THEN fil-d = SEARCH(fil-d).
  
    fil-e = SUBSTRING(fil-d,1,LENGTH(fil-d,"character") - 1,"character") + "e".
  
    DISPLAY DICTDB._File._File-name fil-d new_lang[1] @ msg
      WITH FRAME loaddata. /* loading */
    COLOR DISPLAY MESSAGES DICTDB._File._File-name fil-d msg errs crecs
      WITH FRAME loaddata.
      
    IF SEARCH(fil-d) <> ? THEN DO:    
      {prodict/dump/lodtrail.i
        &file    = "fil-d"
        &entries = "IF lvar[i] BEGINS ""records=""
                      THEN irecs     = INT64(SUBSTRING(lvar[i],9,-1,""character"")).
                    IF lvar[i] BEGINS ""ldbname=""    
                      THEN d-ldbname = SUBSTRING(lvar[i],9,-1,""character"").
                    IF lvar[i] BEGINS ""dateformat=""
                      THEN d-was     = SUBSTRING(lvar[i],12,-1,""character"").
                    IF lvar[i] BEGINS ""numformat=""
                      THEN numformat = SUBSTRING(lvar[i],11,-1,""character"").
                    IF lvar[i] BEGINS ""map=""       
                      THEN maptype   = SUBSTRING(lvar[i],5,-1,""character"").
                   "
        }
     END.
     ELSE DO:
      ASSIGN fil-out = (IF NOT use_ds THEN lpath
                       ELSE lpath + (IF DICTDB._File._Dump-name = ?
                       OR SEARCH(lpath + DICTDB._File._Dump-name + ".d") = ?
                       THEN DICTDB._File._File-name 
                       ELSE DICTDB._File._Dump-name)).

      ASSIGN no-file = (IF DICTDB._File._Dump-name <> ? 
                        THEN DICTDB._File._Dump-name + ".d" ELSE fil-d).

      DISPLAY new_lang[12] @ msg WITH FRAME loaddata. /* !ERROR! */
      OUTPUT STREAM loaderr TO VALUE(fil-e) NO-ECHO APPEND. 
      PUT STREAM loaderr
        "The file " no-file "can not be found." SKIP.
      OUTPUT STREAM loaderr CLOSE.
      ASSIGN terrors = terrors + 1.
      next. /* skip that file */
      END.
         
    /* if we can't fit the value in the format, display asterisks */
    crecs = STRING(irecs,{&MAX_RECS_FORMAT}) NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
       crecs = "**********".

    DISPLAY crecs WITH FRAME loaddata.
   
    /* check if number of records is unknown or known */
    IF irecs = ? AND (error% > 0 AND error% < 100)
     THEN DO:
      DISPLAY new_lang[14] @ msg WITH FRAME loaddata. /* !Warning! */
      OUTPUT STREAM loaderr TO VALUE(fil-e) NO-ECHO APPEND. 
      PUT STREAM loaderr
              "Expected # of records is unknown."        SKIP
                    "Error percentage will be interpreted"     SKIP   
                    "as an absolute number of errors."         SKIP.
      OUTPUT STREAM loaderr CLOSE.
      ASSIGN terrors = terrors + 1.
      END.
           
    IF cerror = ? THEN
    DO:  /* conversion needed but NOT possible */

       /* screen display */
       IF user_env[6] = "f" THEN 
          OUTPUT STREAM loaderr TO fil-e NO-ECHO.
       DO:
        COLOR DISPLAY NORMAL DICTDB._File._File-name fil-d msg errs crecs
          WITH FRAME loaddata.
        DISPLAY DICTDB._File._File-name fil-d new_lang[12] @ msg crecs
          WITH FRAME loaddata. /* ERROR! */
       END.
       OUTPUT CLOSE.
       
       IF use_ds THEN DO:
          DISPLAY STREAM dsfile
            DICTDB._File._File-name fil-d recs errs 
            "Conversion-error" @ fil-e
            WITH FRAME dsfile.
          DOWN STREAM dsfile WITH FRAME dsfile.
       END.
       /* error-count */
       assign cerrors = cerrors + 1.
        /* skip this file */
       NEXT.

    END.     /* conversion needed but NOT possible */

    ELSE DO:  /* conversion not needed OR needed and possible */
      IF cerror = "no-convert" then do:
        IF maptype BEGINS "MAP:" THEN DO:
          IF lobdir = ""  THEN
            INPUT FROM VALUE(fil-d) NO-ECHO 
               MAP VALUE(SUBSTRING(maptype,5,-1,"character"))
               NO-CONVERT.
          ELSE
            INPUT FROM VALUE(fil-d) LOB-DIR VALUE(lobdir) NO-ECHO 
               MAP VALUE(SUBSTRING(maptype,5,-1,"character"))
               NO-CONVERT.
        END.
        ELSE DO: 
          IF lobdir = "" THEN
            INPUT FROM VALUE(fil-d) NO-ECHO NO-MAP NO-CONVERT.
          ELSE
            INPUT FROM VALUE(fil-d) LOB-DIR VALUE(lobdir)  
              NO-ECHO NO-MAP NO-CONVERT.
        END.
      end.
      else do:
        IF maptype BEGINS "MAP:" THEN DO:
          IF lobdir = "" THEN
            INPUT FROM VALUE(fil-d) NO-ECHO
               MAP VALUE(SUBSTRING(maptype,5,-1,"character"))
               CONVERT SOURCE codepage TARGET SESSION:CHARSET.
          ELSE
            INPUT FROM VALUE(fil-d) LOB-DIR VALUE(lobdir) NO-ECHO
               MAP VALUE(SUBSTRING(maptype,5,-1,"character"))
               CONVERT SOURCE codepage TARGET SESSION:CHARSET.
        END.
        ELSE DO: 
          IF lobdir = "" THEN
            INPUT FROM VALUE(fil-d) NO-ECHO 
              NO-MAP CONVERT SOURCE codepage TARGET SESSION:CHARSET.
          ELSE
            INPUT FROM VALUE(fil-d) LOB-DIR VALUE(lobdir) NO-ECHO 
              NO-MAP CONVERT SOURCE codepage TARGET SESSION:CHARSET.
        END.
      end.
    END.     /* conversion not needed OR needed and possible */

    /* check for nummeric format (-E set or not) and error out if wrong */
    IF numformat <> "" THEN
    DO:
       IF LENGTH(numformat) = 1 THEN
       DO:
          IF index(string(1.5,"9.9"),numformat) = 0 THEN
          DO:  /* sesion-format and .d-format don't match */
             RUN NumFormatErr IN THIS-PROCEDURE (INPUT "EparmErr").
             ASSIGN terrors = terrors + 1.
                    NumProcRun = NO.
             NEXT.   /* skip this file */
          END.    /* sesion-format and .d-format don't match */
       END. /* Above deals with the old format, below handles the new */ 
       ELSE
       DO:  /* Test for existence of 2 numeric values, or at least something *
             * translatable to numerics. */
             
          ASSIGN i = INTEGER(ENTRY(1,numformat)) + INTEGER(ENTRY(2,numformat))
             NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
          DO: 
             RUN NumFormatErr IN THIS-PROCEDURE (INPUT "FmtErr").
             ASSIGN terrors = terrors + 1.
             NumProcRun = NO.
             NEXT.
          END.
          IF INTEGER(ENTRY(1,numformat)) + INTEGER(ENTRY(2,numformat)) < 3 THEN
          DO:  /* 0 is not allowed, and the characters must be different. */
             RUN NumFormatErr IN THIS-PROCEDURE (INPUT "FmtErr").
             ASSIGN terrors = terrors + 1.
             NumProcRun = NO.
             NEXT.
          END.
          IF CHR(INT(ENTRY(1,numformat)),SESSION:CPINTERNAL,codepage) <>
          SESSION:NUMERIC-SEPARATOR THEN
          DO:
             RUN NumFormatErr IN THIS-PROCEDURE (INPUT "SepErr").
             ASSIGN terrors = terrors + 1.
          END.
          IF CHR(INT(ENTRY(2,numformat)),SESSION:CPINTERNAL,codepage) <>
          SESSION:NUMERIC-DECIMAL-POINT THEN
          DO:
             RUN NumFormatErr IN THIS-PROCEDURE (INPUT "DecErr").
             ASSIGN terrors = terrors + 1.
          END.
          IF NumProcRun THEN
          DO:
             NumProcRun = NO.
             NEXT.
          END.
       END.
    END.

    ASSIGN
      c    = DICTDB._File._File-name
      &IF "{&WINDOW-SYSTEM}" begins "MS-WIN"
      &THEN
          .
      &ELSE
        xpos = FRAME-COL(loaddata) + 49
        ypos = FRAME-ROW(loaddata) + FRAME-LINE(loaddata) + 5.
      &ENDIF
    CREATE ALIAS "DICTDB2" FOR DATABASE VALUE(user_dbname) NO-ERROR.

    OUTPUT STREAM loaderr TO VALUE(fil-e) NO-ECHO.

    IF SEEK(INPUT) = ? THEN DO:
      PUT STREAM loaderr UNFORMATTED 
        new_lang[2] ' "' fil-d '" ' new_lang[3] ' "' c '".'.
      errs = 1.
    END.
    ELSE DO:

      /* check if we can disable triggers. List now contains just the
        table numbers for tables that we should not disable triggers for.
      */
      ASSIGN dis_trig = ENTRY(1, user_env[5]).

      IF NOT old_dis_trig THEN DO:
         IF dis_trig NE "" AND dis_trig = STRING(_File._File-number) THEN
            dis_trig = "n".
         ELSE
            dis_trig = "y".
      END.

      IF caps(user_env[35]) = "ORA" THEN DO:
         IF irecs <> ? THEN
           ASSIGN load_size = irecs.  
         ELSE DO:
           MESSAGE "The .d file does not contain the number of records to be loaded" SKIP
                   "You must use the reqular Administration Load Table Contents Utility." SKIP(1)
               VIEW-AS ALERT-BOX ERROR.
           ASSIGN user_path = "".
           SESSION:SUPPRESS-WARNINGS = YES.
           RETURN.
         END.
         RUN "prodict/ora/_runload.i" (INPUT dis_trig)
                                    c 
                                    error%
                                    load_size 
                                    c 
                                    (IF irecs = ? THEN 100 else irecs).
      END.
      ELSE
        RUN "prodict/misc/_runload.i" (INPUT dis_trig)
                                    c 
                                    error%
                                    load_size 
                                    c 
                                    (IF irecs = ? THEN 100 else irecs)
                                     user_env[31].

      IF RETURN-VALUE = "stopped" THEN UNDO stoploop, LEAVE stoploop.

      /* move on to the next one in the list */
      IF old_dis_trig OR (dis_trig = "n") THEN
         user_env[5] = SUBSTRING(user_env[5]
                                 ,LENGTH(ENTRY(1,user_env[5]),"character") + 2
                                 ,-1
                                 ,"character"
                                 ).
  
      IF irecs = ? THEN _irecs: DO: /*----------- for reading damaged trailer */
        /* this code is used when a trailer might be present, but not in its
           proper relative offset from the start of the file.  this can happen
           if the file is edited or damaged, changing its length. */
        i = SEEK(INPUT).
        READKEY PAUSE 0. IF LASTKEY <> ASC("P") THEN LEAVE _irecs.
        READKEY PAUSE 0. IF LASTKEY <> ASC("S") THEN LEAVE _irecs.
        READKEY PAUSE 0. IF LASTKEY <> ASC("C") THEN LEAVE _irecs.
        SEEK INPUT TO i. /* just to get eol right */
        REPEAT WHILE irecs = ?:
          IMPORT c.
          IF c BEGINS "records=" THEN irecs = INT64(SUBSTRING(c,9,-1,"character")).
        END.
      END. /*---------------------------------- end of damaged trailer reader */
  
      INPUT CLOSE.
    END.
    IF irecs <> ? AND irecs <> recs THEN DO:
      ASSIGN errs = errs + 1.
      /* ERROR! Trailer indicated <n> records, but only <n> records loaded. */
      PUT STREAM loaderr UNFORMATTED 
        ">> " new_lang[6] " " irecs " " new_lang[7] " " recs " " 
              new_lang[8] SKIP.
    END.
    IF d-was <> ? AND d-was <> mdy + STRING(- yy) THEN DO:
      ASSIGN errs = errs + 1.
      /* ERROR! -d <mdy> or -yy <n> settings of dump were <mdy>-<nnnn> */
      /* but current settings are <mdy>-<nnnn>. */
      PUT STREAM loaderr UNFORMATTED
        ">> " new_lang[9] " " d-was SKIP
        "** " new_lang[10] " " mdy STRING(- yy) ".  " new_lang[11] SKIP.
    END.
    OUTPUT STREAM loaderr CLOSE.

    IF errs = 0 THEN OS-DELETE VALUE(fil-e).

    /* if value can't fit into format, display asterisks */
    crecs = STRING(irecs,{&MAX_RECS_FORMAT}) NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
        crecs = "**********".

    COLOR DISPLAY NORMAL DICTDB._File._File-name fil-d msg errs crecs
      WITH FRAME loaddata.

    DISPLAY
      DICTDB._File._File-name fil-d errs crecs WITH FRAME loaddata.

    /* if the value is too big to be displayed, display asterisks */
    ASSIGN msg = STRING(recs,"ZZZZZZZ9") NO-ERROR.
    IF ERROR-STATUS:ERROR OR ERROR-STATUS:NUM-MESSAGES > 0 THEN
       msg = "********".

    DISPLAY msg WITH FRAME loaddata NO-ERROR.

    IF use_ds THEN DO:
      DISPLAY STREAM dsfile
        DICTDB._File._File-name fil-d recs errs
        (IF errs = 0 THEN "-" ELSE fil-e) @ fil-e
        WITH FRAME dsfile.
      DOWN STREAM dsfile WITH FRAME dsfile.
    END.
    terrors = terrors + errs.
  
    IF recs > 0 THEN DO:
        /* audit the data loaded, even if an error happended, if at least one record was loaded */
        AUDIT-CONTROL:LOG-AUDIT-EVENT(10214, 
                                      phDbName + "." +  DICTDB._File._File-name /* db-name.table-name */, 
                                      "" /* detail */).
    END.

  END. /* for each _file */

  stopped = false. 
END. /* end stop */

/* 20060905-013
   If a stop condition was raised, log the error messages to the .e file 
*/
IF STOPPED THEN DO:
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
        DO i = 1 TO  ERROR-STATUS:NUM-MESSAGES:
            PUT STREAM loaderr UNFORMATTED
              ">> ERROR READING LINE #" recs + 1
              " " ERROR-STATUS:GET-MESSAGE(i) SKIP.
        END.
END.

IF use_ds THEN OUTPUT STREAM dsfile CLOSE.

&IF "{&WINDOW-SYSTEM}" begins "MS-WIN"
&THEN
   /* Nothing to Do */
&ELSE 
DO WHILE FRAME-LINE(loaddata) < FRAME-DOWN(loaddata):
  DOWN 1 WITH FRAME loaddata.
  CLEAR FRAME loaddata NO-PAUSE.
END.
&ENDIF
run adecomm/_setcurs.p ("").

/* for auditing - clear the application context, if we have set one */
IF newAppCtx THEN
   AUDIT-CONTROL:CLEAR-APPL-CONTEXT.

IF stopped THEN
   MESSAGE "Load terminated."
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
ELSE DO:
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    /* Don't use alert box in TTY because it obscures status report
       and you can't move it out of the away.
    */
    HIDE MESSAGE NO-PAUSE.
    IF      cerrors > 0 THEN MESSAGE new_lang[4] skip new_lang[13]. 
    ELSE IF terrors > 0 THEN MESSAGE new_lang[4] skip new_lang[5]. 
    ELSE                     MESSAGE new_lang[4]. 
    pause.
  &ELSE
    IF      cerrors > 0 THEN MESSAGE new_lang[4] skip new_lang[13] VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
    ELSE IF terrors > 0 THEN MESSAGE new_lang[4] skip new_lang[5]  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
    ELSE                     MESSAGE new_lang[4]                   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
  &ENDIF
END.
HIDE MESSAGE NO-PAUSE.
HIDE FRAME loaddata NO-PAUSE.
/* Make sure to turn on the suppress warnings again */
SESSION:SUPPRESS-WARNINGS = YES.
SESSION:IMMEDIATE-DISPLAY = no.
IF NOT isCpUndefined THEN
   ASSIGN user_longchar = "".
RETURN.
