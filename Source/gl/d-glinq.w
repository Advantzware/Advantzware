&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opriRowid AS ROWID NO-UNDO.


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE i           AS INTEGER NO-UNDO.
DEFINE VARIABLE dtCheckDate AS DATE    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-30 bank_account dAmount dtDate Btn_save  ~
iPeriod Btn_Cancel iYear 
&Scoped-Define DISPLAYED-OBJECTS bank_account dAmount dtDate iPeriod iYear 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "Cancel" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_save  
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "&Save" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .
     
DEFINE VARIABLE bank_account AS CHARACTER FORMAT "X(18)" 
    LABEL "Account" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dAmount      AS DECIMAL   FORMAT "->>,>>>,>>9.99":U INITIAL 0 
    LABEL "Amount" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dtDate       AS DATE      FORMAT "99/99/9999":U 
    LABEL "Date" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iPeriod      AS INTEGER   FORMAT ">9":U INITIAL 0 
    LABEL "Period" 
    VIEW-AS FILL-IN 
    SIZE 5.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iYear        AS INTEGER   FORMAT ">>>9":U INITIAL 0 
    LABEL "Year" 
    VIEW-AS FILL-IN 
    SIZE 9.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-30
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 86.8 BY 6.71
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 19 BY 2.38
    BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    bank_account AT ROW 1.95 COL 23.8 COLON-ALIGNED
    dAmount AT ROW 3.52 COL 23.8 COLON-ALIGNED
    dtDate AT ROW 5 COL 23.8 COLON-ALIGNED
    iPeriod AT ROW 5 COL 55.4 COLON-ALIGNED WIDGET-ID 58
    iYear AT ROW 5 COL 72.2 COLON-ALIGNED WIDGET-ID 60
    Btn_save AT ROW 8.33 COL 69.4
    Btn_Cancel AT ROW 8.33 COL 78.2       
    RECT-30 AT ROW 1.19 COL 1.2 WIDGET-ID 56
    RECT-7 AT ROW 8.05 COL 68.8
    SPACE(0.79) SKIP(0.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "GL Update"
    CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* GL Update */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_save Dialog-Frame
ON CHOOSE OF Btn_save IN FRAME Dialog-Frame /* Save */
    DO:
        DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
    
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
    
        RUN pCheckTransDate(OUTPUT lReturnError) .
        IF lReturnError THEN RETURN NO-APPLY .
    
        RUN pCheckPeriod(OUTPUT lReturnError) .
        IF lReturnError THEN RETURN NO-APPLY .
    
        RUN pAssignValue.
    
        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME 

&Scoped-define SELF-NAME bank_account
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bank_account Dialog-Frame
ON HELP OF bank_account IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.


        RUN windows/l-acct.w (ipcCompany, "", {&self-name}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val NE "" AND ENTRY(1,char-val) NE {&self-name}:SCREEN-VALUE THEN 
        DO:
            bank_account:SCREEN-VALUE = ENTRY(1,char-val).     
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME          

&Scoped-define SELF-NAME dtDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dtDate Dialog-Frame
ON LEAVE OF dtDate IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
        RUN pCheckTransDate(OUTPUT lReturnError) .
        IF lReturnError THEN RETURN NO-APPLY .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME iPeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iPeriod Dialog-Frame
ON LEAVE OF iPeriod IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
        RUN pCheckPeriod(OUTPUT lReturnError) .
        IF lReturnError THEN RETURN NO-APPLY .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
   
    RUN pDisplayValue.
    RUN enable_UI.
 
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Hide all frames. */
    HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
    DISPLAY bank_account dAmount dtDate iPeriod iYear 
        WITH FRAME Dialog-Frame.
    ENABLE RECT-30 bank_account dAmount dtDate Btn_save iPeriod Btn_Cancel iYear 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayValue Dialog-Frame 
PROCEDURE pDisplayValue :
    /*------------------------------------------------------------------------------
                  Purpose:     
                  Parameters:  <none>
                  Notes:     
       ------------------------------------------------------------------------------*/

   
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST  glhist NO-LOCK
            WHERE ROWID(glhist) EQ ipriRowid NO-ERROR .
        IF AVAILABLE glhist THEN
        DO:
            ASSIGN 
                bank_account = glhist.actnum
                dAmount      = glhist.tr-amt
                dtDate       = glhist.tr-date
                iPeriod      = glhist.period
                iYear        = glhist.yr  .          
            dtCheckDate  = glhist.tr-date .
        END.
    
        IF NOT AVAILABLE glhist THEN 
        DO:
            FIND FIRST  gltrans NO-LOCK
                WHERE ROWID(gltrans) EQ ipriRowid NO-ERROR .
            IF AVAILABLE gltrans THEN
            DO:
                ASSIGN 
                    bank_account = gltrans.actnum
                    dAmount      = gltrans.tr-amt
                    dtDate       = gltrans.tr-date
                    iPeriod      = gltrans.period
                    iYear        = gltrans.yr  .
                dtCheckDate  = gltrans.tr-date .
           
            END.       
        END.
        APPLY "entry" TO bank_account .            
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssignValue Dialog-Frame 
PROCEDURE pAssignValue :
    /*------------------------------------------------------------------------------
                  Purpose:     
                  Parameters:  <none>
                  Notes:       
                ------------------------------------------------------------------------------*/

    DEFINE BUFFER bf-gltrans FOR gltrans.
    DEFINE BUFFER bf-glhist  FOR glhist.
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST  bf-glhist EXCLUSIVE-LOCK
            WHERE ROWID(bf-glhist) EQ ipriRowid NO-ERROR .
        IF AVAILABLE bf-glhist THEN
        DO:
            ASSIGN 
                bf-glhist.actnum    = bank_account
                bf-glhist.tr-amt    = dAmount
                bf-glhist.tr-date   = dtDate
                bf-glhist.period    = iPeriod
                bf-glhist.yr        = iYear
                bf-glhist.createdBy = USERID(LDBNAME(1)).          
            opriRowid = ROWID(bf-glhist).
        END.
    
        IF NOT AVAILABLE glhist THEN 
        DO:
            FIND FIRST  bf-gltrans EXCLUSIVE-LOCK
                WHERE ROWID(bf-gltrans) EQ ipriRowid NO-ERROR .
            IF AVAILABLE bf-gltrans THEN
            DO:
                ASSIGN 
                    bf-gltrans.actnum    = bank_account  
                    bf-gltrans.tr-amt    = dAmount       
                    bf-gltrans.tr-date   = dtDate        
                    bf-gltrans.period    = iPeriod       
                    bf-gltrans.yr        = iYear          
                    bf-gltrans.createdBy = USERID(LDBNAME(1)).
                opriRowid = ROWID(bf-gltrans).
           
            END.       
        END.
        RELEASE bf-glhist.
        RELEASE bf-gltrans.              
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckTransDate Dialog-Frame 
PROCEDURE pCheckTransDate :
    /*------------------------------------------------------------------------------
                  Purpose:     
                  Parameters:  <none>
                  Notes:       
                ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF MONTH(dtCheckDate) NE MONTH(DATE(dtDate:SCREEN-VALUE)) THEN
        DO:
            MESSAGE "Please enter date in transaction period .." 
                VIEW-AS ALERT-BOX INFORMATION .
            oplReturnError = TRUE .
        END.               
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckPeriod Dialog-Frame 
PROCEDURE pCheckPeriod :
    /*------------------------------------------------------------------------------
                  Purpose:     
                  Parameters:  <none>
                  Notes:       
                ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF INTEGER(iPeriod:SCREEN-VALUE) GT 12 OR integer(iPeriod:SCREEN-VALUE) LE 0 THEN
        DO:
            MESSAGE "Please enter a vaild period .." 
                VIEW-AS ALERT-BOX INFORMATION .
            oplReturnError = TRUE .
        END.               
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
