&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i new shared}

DEF VAR iCtr AS INT NO-UNDO.
DEF VAR cOutDir AS CHAR NO-UNDO.
DEF VAR lVerbose AS LOG NO-UNDO INITIAL FALSE.

ASSIGN
    cocode = gcompany
    locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiDate rsOpen fiStartJob fiStartJob2 ~
fiEndJob fiEndJob2 rsPurge tbVerbose btn-process btn-cancel eHelp 
&Scoped-Define DISPLAYED-OBJECTS fiText-2 fiDate rsOpen fiStartJob ~
fiStartJob2 fiEndJob fiEndJob2 rsPurge tbVerbose fiText-3 eHelp fiText1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Exit" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "Start Purge" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE eHelp AS CHARACTER INITIAL "This process will purge all jobs and related records created before the selected date." 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 99 BY 14.76 NO-UNDO.

DEFINE VARIABLE fiDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Job CLOSE Date is less than" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndJob AS CHARACTER FORMAT "X(6)":U 
     LABEL "TO" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndJob2 AS INTEGER FORMAT "99":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiStartJob AS CHARACTER FORMAT "X(6)":U 
     LABEL "(Optional) Job Range - FROM" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiStartJob2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiText-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Purge all jobs and related records where:" 
     VIEW-AS FILL-IN 
     SIZE 52 BY .95 NO-UNDO.

DEFINE VARIABLE fiText-3 AS CHARACTER FORMAT "X(256)":U INITIAL "(F1 to look up)" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE fiText1 AS CHARACTER FORMAT "X(256)":U INITIAL "Help/Notes on this function:" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE rsOpen AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Closed Jobs ONLY", "C",
"Closed and Open Jobs (not recommended)", "O"
     SIZE 82 BY .95 NO-UNDO.

DEFINE VARIABLE rsPurge AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "List and Purge", "P",
"Simulate Only", "L"
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE tbVerbose AS LOGICAL INITIAL no 
     LABEL "Write log entries for all child records deleted?" 
     VIEW-AS TOGGLE-BOX
     SIZE 58 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiText-2 AT ROW 1.48 COL 2 NO-LABEL NO-TAB-STOP 
     fiDate AT ROW 2.67 COL 39 COLON-ALIGNED
     rsOpen AT ROW 4.1 COL 6 NO-LABEL
     fiStartJob AT ROW 5.29 COL 39 COLON-ALIGNED
     fiStartJob2 AT ROW 5.29 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fiEndJob AT ROW 5.29 COL 62 COLON-ALIGNED
     fiEndJob2 AT ROW 5.29 COL 73.8 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     rsPurge AT ROW 6.71 COL 6 NO-LABEL
     tbVerbose AT ROW 7.91 COL 6
     fiText-3 AT ROW 5.29 COL 85 NO-LABEL NO-TAB-STOP 
     btn-process AT ROW 7.19 COL 75
     btn-cancel AT ROW 8.86 COL 75
     eHelp AT ROW 10.29 COL 1 NO-LABEL
     fiText1 AT ROW 9.33 COL 2 NO-LABEL NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104.2 BY 24.14
         FONT 5
         DEFAULT-BUTTON btn-cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge Jobs"
         HEIGHT             = 24.14
         WIDTH              = 104.2
         MAX-HEIGHT         = 24.14
         MAX-WIDTH          = 104.2
         VIRTUAL-HEIGHT     = 24.14
         VIRTUAL-WIDTH      = 104.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       eHelp:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiText-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiText-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiText-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiText-3:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiText1 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiText1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON HELP OF FRAME DEFAULT-FRAME /* job purge */
DO:
DEFINE VARIABLE lw-focus AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE ls-cur-val AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

   lw-focus = FOCUS.

   CASE lw-focus:NAME :

       WHEN "fiStartJob" THEN DO:             
           RUN system/openlookup.p (cocode, "job-no", 0, "", 0, OUTPUT cFieldsValue, OUTPUT cFoundValue, OUTPUT recFoundRecID).
           fiStartJob:SCREEN-VALUE = cFoundValue.            
           RETURN NO-APPLY.
       END.  /* cust-no*/  
       WHEN "fiStartJob2" THEN DO:
           ls-cur-val = fiStartJob:SCREEN-VALUE.
           RUN windows/l-jobno2.w (cocode,ls-cur-val,FOCUS:SCREEN-VALUE, OUTPUT char-val,OUTPUT recFoundRecID).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(2,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/  
       WHEN "fiEndJob" THEN DO:             
           RUN system/openlookup.p (cocode, "job-no", 0, "", 0, OUTPUT cFieldsValue, OUTPUT cFoundValue, OUTPUT recFoundRecID).
           fiEndJob:SCREEN-VALUE = cFoundValue.            
           RETURN NO-APPLY.
       END.  /* cust-no*/  
       WHEN "fiEndJob2" THEN DO:
           ls-cur-val = fiEndJob:SCREEN-VALUE.
           RUN windows/l-jobno2.w (cocode,ls-cur-val,FOCUS:SCREEN-VALUE, OUTPUT char-val,OUTPUT recFoundRecID).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(2,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/ 
       
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Jobs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Jobs */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME DEFAULT-FRAME /* Exit */
DO:
    APPLY 'close' TO THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME DEFAULT-FRAME /* Start Purge */
DO:
    DEF VAR lSuccess AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.

    /* Validate that start job is LE end job, if entered */
    IF fiStartJob:SCREEN-VALUE NE "" 
        AND fiEndJob:SCREEN-VALUE NE "" 
        AND fiStartJob:SCREEN-VALUE GT fiEndJob:SCREEN-VALUE THEN 
    DO:
        MESSAGE 
            "Start job is less than end job. Please correct."
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    
    IF integer(fiStartJob2:SCREEN-VALUE) GT  INTEGER(fiEndJob2:SCREEN-VALUE) THEN
    DO:
          MESSAGE 
            "Start job2 is less than end job2. Please correct."
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.

    MESSAGE 
        "Are you sure you want to " + TRIM(c-win:TITLE) +
        " within the selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process AS LOG.
    IF NOT v-process THEN RETURN NO-APPLY.
    
    RUN pSetup. /* Resets directory for run */
    
    DYNAMIC-FUNCTION ("fStartPurge", "job").
     
    /* ensure job-no values are formatted correctly */
    APPLY "LEAVE":U TO fiStartJob.
    APPLY "LEAVE":U TO fiEndJob.

    /* Optimize these to use the best index given entered values */
    IF rsOpen:SCREEN-VALUE EQ "C" THEN DO:  /* Closed jobs only, use close-date for index */
        FOR EACH job NO-LOCK WHERE 
            job.company EQ cocode AND 
            job.close-date LT DATE(fiDate:SCREEN-VALUE)
            USE-INDEX close-date:
                
            IF fiEndJob:SCREEN-VALUE NE "" THEN  DO: 
                IF (job.job-no LT fiStartJob:SCREEN-VALUE /* Job no outside of range specified */
                    OR job.job-no GT fiEndJob:SCREEN-VALUE
                    OR job.job-no2 LT INTEGER(fiStartJob2:SCREEN-VALUE) /* Job2 no outside of range specified */
                    OR job.job-no2 GT INTEGER(fiEndJob2:SCREEN-VALUE))    
                AND TRIM(job.job-no) NE "" THEN 
                    NEXT.
            END.
                
            STATUS DEFAULT "Purging job #" + job.job-no + "-" + STRING(job.job-no2,"99") + "...".
            
            IF rsPurge:SCREEN-VALUE EQ "P" THEN 
                RUN purge ("job", ROWID(job), lVerbose, OUTPUT lSuccess, OUTPUT cMessage).
            ELSE 
                RUN PrePurge ("job", ROWID(job), lVerbose, OUTPUT lSuccess, OUTPUT cMessage).   
        END.
    END.
    ELSE DO: /* Closed and open jobs, full table scan */
        FOR EACH job NO-LOCK WHERE 
            job.company EQ cocode AND 
            (job.create-date LT DATE(fiDate:SCREEN-VALUE)
            OR job.create-date EQ ?)
            :
            
            IF fiEndJob:SCREEN-VALUE NE "" THEN DO: 
                IF (job.job-no LT fiStartJob:SCREEN-VALUE /* Job no outside of range specified */
                    OR job.job-no GT fiEndJob:SCREEN-VALUE
                    OR job.job-no2 LT INTEGER(fiStartJob2:SCREEN-VALUE) /* Job2 no outside of range specified */
                    OR job.job-no2 GT INTEGER(fiEndJob2:SCREEN-VALUE)) 
                AND TRIM(job.job-no) NE "" THEN 
                    NEXT.
            END.
            
            STATUS DEFAULT "Purging job #" + job.job-no + "-" + STRING(job.job-no2,"99") + "...".
            
            IF rsPurge:SCREEN-VALUE EQ "P" THEN 
                RUN purge ("job", ROWID(job), lVerbose, OUTPUT lSuccess, OUTPUT cMessage).
            ELSE 
                RUN PrePurge ("job", ROWID(job), lVerbose, OUTPUT lSuccess, OUTPUT cMessage).
        END.         
    END.

    MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
    DYNAMIC-FUNCTION ("fEndPurge", "job").
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiStartJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStartJob C-Win
ON LEAVE OF fiStartJob IN FRAME DEFAULT-FRAME /* (Optional) Job Range - FROM */
OR LEAVE OF fiEndJob
DO:
    CASE SELF:NAME: 
        WHEN 'fiStartJob' THEN DO:
            IF SELF:SCREEN-VALUE NE ""
            AND fiEndJob:SCREEN-VALUE EQ "" THEN ASSIGN 
                fiEndJob:SCREEN-VALUE = SELF:SCREEN-VALUE .
            ASSIGN 
                SELF:SCREEN-VALUE = FILL(" ", 6 - INT(LENGTH(TRIM(SELF:SCREEN-VALUE)))) + TRIM(SELF:SCREEN-VALUE).
            IF SELF:SCREEN-VALUE NE "" 
            AND fiEndJob:SCREEN-VALUE NE "" 
            AND SELF:SCREEN-VALUE GT fiEndJob:SCREEN-VALUE THEN DO:
                MESSAGE 
                    "Start job is less than end job. Please correct."
                    VIEW-AS ALERT-BOX WARNING.
            END.  
        END.
        WHEN 'fiEndJob' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = FILL(" ", 6 - INT(LENGTH(TRIM(SELF:SCREEN-VALUE)))) + TRIM(SELF:SCREEN-VALUE).
            IF SELF:SCREEN-VALUE NE "" 
                AND fiStartJob:SCREEN-VALUE NE "" 
                AND SELF:SCREEN-VALUE LT fiStartJob:SCREEN-VALUE THEN 
            DO:
                MESSAGE 
                    "Start job is less than end job. Please correct."
                    VIEW-AS ALERT-BOX WARNING.
            END.  
        END.
    END CASE.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiStartJob2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStartJob2 C-Win
ON LEAVE OF fiStartJob2 IN FRAME DEFAULT-FRAME
OR LEAVE OF fiEndJob2
DO:
    CASE SELF:NAME: 
        WHEN 'fiStartJob2' THEN DO:            
            IF SELF:SCREEN-VALUE NE "" 
            AND integer(fiEndJob2:SCREEN-VALUE) NE 0  
            AND integer(SELF:SCREEN-VALUE) GT INTEGER(fiEndJob2:SCREEN-VALUE) THEN DO:
                MESSAGE 
                    "Start job2 is greater than end job2. Please correct."
                    VIEW-AS ALERT-BOX WARNING.
            END.  
        END.
        WHEN 'fiEndJob2' THEN DO:             
            IF SELF:SCREEN-VALUE NE "" 
                AND integer(fiStartJob2:SCREEN-VALUE) NE 0 
                AND INTEGER(SELF:SCREEN-VALUE) LT integer(fiStartJob2:SCREEN-VALUE) THEN 
            DO:
                MESSAGE 
                    "Start job2 is greater than end job2. Please correct."
                    VIEW-AS ALERT-BOX WARNING.
            END.  
        END.
    END CASE.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsOpen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsOpen C-Win
ON VALUE-CHANGED OF rsOpen IN FRAME DEFAULT-FRAME
DO:
    CASE SELF:SCREEN-VALUE:
        WHEN "C" THEN ASSIGN 
            fiDate:LABEL = "Job CLOSE date is less than".
        WHEN "O" THEN ASSIGN 
            fiDate:LABEL = "Job CREATE date is less than".
    END CASE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbVerbose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbVerbose C-Win
ON VALUE-CHANGED OF tbVerbose IN FRAME DEFAULT-FRAME /* Write log entries for all child records deleted? */
DO:
    ASSIGN 
        lVerbose = SELF:CHECKED.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

    /* check security */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
    RUN enable_UI.
    {methods/nowait.i}
    
    RUN pSetup.
    fiDate:SCREEN-VALUE = STRING(TODAY - 1095). /* Start at 3 years, user can change */    
    APPLY 'entry' TO fiDate.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiText-2 fiDate rsOpen fiStartJob fiStartJob2 fiEndJob fiEndJob2 
          rsPurge tbVerbose fiText-3 eHelp fiText1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiDate rsOpen fiStartJob fiStartJob2 fiEndJob fiEndJob2 rsPurge 
         tbVerbose btn-process btn-cancel eHelp 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetup C-Win 
PROCEDURE pSetup :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            cOutDir = DYNAMIC-FUNCTION ("fGetPurgeDir", "job") 
            eHelp:SCREEN-VALUE = "- This process will purge all jobs and related records based on the parameters you choose below. " + CHR(10) + CHR(10) +
                                 "- Selecting the 'Closed and Open' option will take longer 'per job' than the 'Closed' option, as these records are found differently." + CHR(10) + CHR(10) +
                                 "- It is not necessary to select a job number range, but if you do, this will be combined with the date option; choose both accordingly." + CHR(10) + 
                                 "(Using a wide job range can significantly slow the operation)" + CHR(10) + CHR(10) +
                                 "- Selecting the 'Simulate' option will NOT delete any records; it will provide a list of records which 'would be' deleted if the 'Purge' option were chosen. " +
                                 "List generation is approximately 5 times faster than the actual purge." + CHR(10) + CHR(10) +
                                 "- A folder containing the log of deletions, and files enabling you to recover from this " +
                                 "operation will be stored in: " + cOutDir + "\." + CHR(10) + CHR(10) + 
                                 "- This program cannot be interrupted once you choose the Start Purge button." + CHR(10).
        APPLY "entry" TO fiDate.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

