/*********************************************************************
* Copyright (C) 2007 by Progress Software Corporation. All rights    *
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
*/
/*h-*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE NEW SHARED STREAM   dump.
DEFINE NEW SHARED VARIABLE recs AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE NEW SHARED VARIABLE xpos AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE ypos AS INTEGER NO-UNDO.

DEFINE VARIABLE cerr        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cntr        AS INTEGER   NO-UNDO.
DEFINE VARIABLE fil         AS CHARACTER NO-UNDO.
DEFINE VARIABLE loop        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lots        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mdy         AS CHARACTER NO-UNDO.
DEFINE VARIABLE msg         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stamp       AS CHARACTER NO-UNDO.
DEFINE VARIABLE useix       AS CHARACTER NO-UNDO.
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

FORM
  DICTDB._File._File-name FORMAT "x(32)" LABEL "Table"  
  SPACE(0) fil            FORMAT "x(32)" LABEL "Dump File" SPACE(0)
  msg                     FORMAT "x(13)"  LABEL "Records" 
  HEADER 
    " Dumping Data.   Press " + 
    KBLABEL("STOP") + " to terminate the dump process." format "x(66)" SKIP(1)
  WITH FRAME dumpdata NO-ATTR-SPACE USE-TEXT SCROLLABLE
  SCREEN-LINES - 8 DOWN ROW 2 CENTERED &IF "{&WINDOW-SYSTEM}" <> "TTY"
  &THEN THREE-D &ENDIF.

IF SESSION:CPINTERNAL EQ "undefined":U THEN
    isCpUndefined = YES.

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
  cSlash = "/".
&ELSE 
  cSlash = "~\".
&ENDIF
  
IF NOT isCpUndefined THEN DO:

  IF user_longchar <> "" AND user_longchar <> ? THEN
     ASSIGN has_lchar = TRUE.

/* The only Audit table that can be dumped through this program is the
   _audit-event table, so we remove it from the templist and check for 
   instances of the _aud string in the table list. */

/* let's use a longchar in case the string is too big, and because
   the code below can be generic 
*/

   IF NOT has_lchar THEN
      user_longchar = user_env[1].
END.

IF (isCpUndefined AND user_env[1] NE "" AND user_env[1] NE ?)
   OR ((NOT isCpUndefined) AND (user_longchar NE "" AND user_longchar NE ?)) THEN DO:

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

IF has_aud = TRUE THEN DO:
  MESSAGE "Dump Failed!" SKIP(1)
          "You cannot dump Audit Policies or Data through this utility!"
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

IF NOT CONNECTED(user_dbname) THEN DO:
    MESSAGE "Database" user_dbname "is not connected!"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

PAUSE 0.
SESSION:IMMEDIATE-DISPLAY = yes.
VIEW FRAME dumpdata.
run adecomm/_setcurs.p ("WAIT").

IF  user_env[5] = " "  
 OR user_env[5] = ?  THEN assign user_env[5] = "<internal defaults apply>".

IF user_env[30] = "" OR user_env[30]= ? THEN
    ASSIGN lobdir = "".
ELSE
    ASSIGN lobdir = user_env[30].

/* Used in numformat output */
CodeOut = IF user_env[5] = "<internal defaults apply>" THEN
             SESSION:CPINTERNAL
	  ELSE
	     user_env[5].

RUN "prodict/_dctyear.p" (OUTPUT mdy,OUTPUT yy).

ASSIGN
  phDbName = PDBNAME("DICTDB") /* for logging audit event */
  cntr = 0
  lots = (IF has_lchar THEN INDEX(user_longchar,",") > 0 ELSE INDEX(user_env[1],",") > 0)
  loop = TRUE. /* use this to mark initial entry into loop */

PAUSE 5 BEFORE-HIDE.

IF has_lchar THEN
   numCount = NUM-ENTRIES(user_longchar).
ELSE
   numCount = NUM-ENTRIES(user_env[1]).

/* it user_env[5] has the old format, remember this */
/* this is just in case someone is using an old version of dump_d.p */
IF ENTRY(1, user_env[4]) = "y" OR ENTRY(1, user_env[4]) = "n" THEN
  ASSIGN old_dis_trig = YES.

DO ON STOP UNDO, LEAVE:
  /* if not the no-convert case, check utf-8 case */
  IF  user_env[5] NE "<internal defaults apply>" THEN DO:
      
      FIND FIRST DICTDB._Db WHERE RECID(DICTDB._Db) = drec_db.
      IF DICTDB._Db._db-xl-name = "UTF-8" AND
         (SESSION:CHARSET NE "UTF-8" OR TRIM(user_env[5]) NE "utf-8") THEN DO:

          if user_env[6] = "no-alert-boxes"
          then do:  /* output WITHOUT alert-box */
              MESSAGE "The database codepage is 'UTF-8' but -cpinternal and/or the codepage" SKIP
                      "for output is not, which can cause some data not to be dumped properly" SKIP
                      "if it cannot be represented in that codepage." SKIP(1)
                      "It's recommended that you dump the data from a session with the 'UTF-8'" SKIP
                      "codepage to avoid possible data corruption.".
          END.
          ELSE DO:
              MESSAGE "The database codepage is 'UTF-8' but -cpinternal and/or the codepage" SKIP
                      "for output is not, which can cause some data not to be dumped properly" SKIP
                      "if it cannot be represented in that codepage." SKIP(1)
                      "It's recommended that you dump the data from a session with the 'UTF-8'" SKIP
                      "codepage to avoid possible data corruption."
                  VIEW-AS ALERT-BOX WARNING.
          END.
      END.
  END.

  DO FOR DICTDB._File ix = 1 to numCount ON ERROR UNDO,NEXT:

    ASSIGN cTemp = IF has_lchar THEN ENTRY(ix,user_longchar) ELSE ENTRY(ix,user_env[1]).

    IF INTEGER(DBVERSION("DICTDB")) > 8 THEN 
       FIND DICTDB._File WHERE DICTDB._File._Db-recid = drec_db AND 
                  DICTDB._File._File-name = cTemp AND
                 (DICTDB._File._Owner = "PUB" OR DICTDB._File._Owner = "_FOREIGN").
    ELSE
      FIND DICTDB._File WHERE DICTDB._File._Db-recid = drec_db AND 
                  DICTDB._File._File-name = cTemp.
                  
    IF loop THEN .
    ELSE IF FRAME-LINE(dumpdata) = FRAME-DOWN(dumpdata) THEN
      UP FRAME-LINE(dumpdata) - 1 WITH FRAME dumpdata.
    ELSE
      DOWN 1 WITH FRAME dumpdata.
  
    ASSIGN
      fil  = ( IF lots
                      /* Don't count on a slash being at the end of the 
                         directory.  Always trim it off and add one. */
                 THEN (IF user_env[2] EQ "" OR 
                          user_env[2] EQ "." THEN ""
                       ELSE RIGHT-TRIM(user_env[2],cSlash) + cSlash) +
                      ( IF DICTDB._File._Dump-name = ?
                          THEN DICTDB._File._File-name
                          ELSE DICTDB._File._Dump-name
                      ) + ".d"
                 ELSE user_env[2]
             )
      loop = FALSE
      recs = 0.
  
    DISPLAY DICTDB._File._File-name fil "Dumping" @ msg WITH FRAME dumpdata.
    COLOR DISPLAY MESSAGES DICTDB._File._File-name fil msg
      WITH FRAME dumpdata.

    ASSIGN
      xpos  = FRAME-COL(dumpdata) + 69
      ypos  = FRAME-ROW(dumpdata) + FRAME-LINE(dumpdata) + 5
      stamp = STRING(YEAR( TODAY),"9999") + "/"
            + STRING(MONTH(TODAY),"99"  ) + "/"
            + STRING(DAY(  TODAY),"99"  ) + "-"
            + STRING(TIME,"HH:MM:SS")
      cerr  = TRUE
      useix = " NO-LOCK".
  
    IF DICTDB._File._Prime-Index <> ? AND user_dbtype = "PROGRESS" THEN DO:
      FIND DICTDB._Index WHERE RECID(DICTDB._Index) = DICTDB._File._Prime-Index.
      IF NOT DICTDB._Index._Active THEN DO:
        FIND FIRST DICTDB._Index OF DICTDB._File WHERE DICTDB._Index._Active NO-ERROR.
        IF NOT AVAILABLE DICTDB._Index THEN DO:
          DISPLAY "Error!" @ msg WITH FRAME dumpdata.
          COLOR DISPLAY NORMAL DICTDB._File._File-name fil msg WITH FRAME dumpdata.
          MESSAGE
            "Cannot dump {&PRO_DISPLAY_NAME} data when all indexes for a table are inactive.".
          NEXT.
        END.
        useix = "USE-INDEX " + DICTDB._Index._Index-name + useix.
      END.
    END.
 
    DO ON ERROR UNDO,LEAVE:      /* code-page-stuf <hutegger> 94/02 */
      IF  user_env[3] = "" OR user_env[3] = "NO-MAP" THEN DO:
        IF  user_env[5] = "<internal defaults apply>" THEN DO:
          IF lobdir <> "" THEN
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
        IF  user_env[5] = "<internal defaults apply>" THEN DO:
          IF lobdir <> "" THEN 
            OUTPUT STREAM dump TO VALUE(fil) LOB-DIR VALUE(lobdir) NO-ECHO 
            MAP VALUE(SUBSTRING(user_env[3],5,-1,"character"))
            NO-CONVERT.
          ELSE
            OUTPUT STREAM dump TO VALUE(fil) NO-ECHO 
            MAP VALUE(SUBSTRING(user_env[3],5,-1,"character"))
            NO-CONVERT.
        END.
        ELSE DO:
          IF lobdir <> "" THEN
            OUTPUT STREAM dump TO VALUE(fil) LOB-DIR VALUE(lobdir) NO-ECHO 
            MAP VALUE(SUBSTRING(user_env[3],5,-1,"character"))
            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].
          ELSE 
            OUTPUT STREAM dump TO VALUE(fil)  NO-ECHO 
            MAP VALUE(SUBSTRING(user_env[3],5,-1,"character"))
            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].
        END.
      END.
      cerr = FALSE.
    END.
  
    IF cerr THEN DO:
      DISPLAY "Error!" @ msg WITH FRAME dumpdata.
      COLOR DISPLAY NORMAL DICTDB._File._File-name fil msg WITH FRAME dumpdata.
      NEXT.
    END.
  
    CREATE ALIAS "DICTDB2" FOR DATABASE VALUE(user_dbname).

    /* check if we can disable triggers. List now contains just the
      table numbers for tables that we should not disable triggers for.
    */
    ASSIGN dis_trig = ENTRY(1, user_env[4]).
    IF NOT old_dis_trig THEN DO:
        IF dis_trig NE "" AND dis_trig = STRING(_File._File-number) THEN
           ASSIGN dis_trig = "n".
        ELSE 
           ASSIGN dis_trig = "y".
    END.

    RUN "prodict/misc/_rundump.i" (INPUT dis_trig)
                VALUE(_File._File-name) VALUE(useix) VALUE(user_env[31]).
                                       
    /* move on to the next one in the list */
    IF old_dis_trig OR (dis_trig = "n") THEN
       user_env[4] = SUBSTRING(user_env[4]
                               ,LENGTH(ENTRY(1,user_env[4]),"character") + 2
                               ,-1
                               ,"character"
                               ).

/*------------------ Trailer-INFO ------------------*/

  /* if value is too large to fit in format, write it as it is */
  ASSIGN cRecords = STRING(recs,"9999999999999") NO-ERROR.
  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
     ASSIGN cRecords = STRING(recs).

  {prodict/dump/dmptrail.i
    &entries      = "PUT STREAM dump UNFORMATTED
                      ""filename=""   DICTDB._File._File-name SKIP
                      ""records=""    cRecords SKIP
                      ""ldbname=""    LDBNAME(user_dbname) SKIP
                      ""timestamp=""  stamp SKIP
                      ""numformat=""
                   STRING(ASC(
		   SESSION:NUMERIC-SEPARATOR,CodeOut,SESSION:CPINTERNAL))     +
                   "",""                                                      +                   STRING(ASC(
		   SESSION:NUMERIC-DECIMAL-POINT,CodeOut,SESSION:CPINTERNAL))
                        SKIP
                        ""dateformat="" mdy STRING(- yy) SKIP.
                    IF user_env[3] = ""NO-MAP"" THEN
                      PUT STREAM dump UNFORMATTED ""map=NO-MAP"" SKIP.
                    ELSE
                    IF user_env[3] <> """" THEN
                      PUT STREAM dump UNFORMATTED ""map=MAP:"" 
                        SUBSTRING(user_env[3],4,-1,""character"") SKIP.
                    "  
    &seek-stream  = "dump"
    &stream       = "STREAM dump"
    }  /* adds trailer with code-page-entrie to end of file */
    
/*------------------ Trailer-INFO ------------------*/

    OUTPUT STREAM dump CLOSE.
  
    COLOR DISPLAY NORMAL DICTDB._File._File-name fil msg WITH FRAME dumpdata.
    DISPLAY DICTDB._File._File-name fil WITH FRAME dumpdata.

    /* make sure value can fit in format */
    msg = STRING(recs,">>>>>>>>>>>>9") NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
        msg = "**********". /* too big to fit on the screen */

    DISPLAY msg WITH FRAME dumpdata.

    cntr = cntr + 1.
  
    /* audit dump of tables */
    AUDIT-CONTROL:LOG-AUDIT-EVENT(10213, 
                                  phDbName + "." +  DICTDB._File._File-name /* db-name.table-name */, 
                                  "" /* detail */).
  END. /* for each DICTDB._File */

  stopped = false.
END.  /* on stop */

DO WHILE FRAME-LINE(dumpdata) < FRAME-DOWN(dumpdata):
  DOWN 1 WITH FRAME dumpdata.
  CLEAR FRAME dumpdata NO-PAUSE.
END.
run adecomm/_setcurs.p ("").

if user_env[6] = "no-alert-boxes"
then do:  /* output WITHOUT alert-box */

  IF stopped THEN
    MESSAGE "Dump terminated.".
  ELSE
    MESSAGE "Dump of database contents completed:" 
                    cntr "table(s) dumped successfully.".
end.      /* output WITHOUT alert-box */

else do:  /* output WITH alert-box */

  IF stopped THEN
   MESSAGE "Dump terminated."
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  ELSE
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
      MESSAGE "Dump of database contents completed:" 
                    cntr "table(s) dumped successfully.".
      pause.
   &ELSE
      MESSAGE "Dump of database contents completed:" SKIP
              cntr "table(s) dumped successfully."
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   &ENDIF
  
end.     /* output WITH alert-box */

IF NOT isCpUndefined THEN
   ASSIGN user_longchar = "".

HIDE FRAME dumpdata NO-PAUSE.
SESSION:IMMEDIATE-DISPLAY = no.
RETURN.

