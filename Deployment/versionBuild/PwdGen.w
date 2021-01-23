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
DEF VAR cLetterList AS CHAR NO-UNDO INITIAL "A,B,C,D,E,F,G,H,J,K,L,M,N,P,R,S,T,U,V,W,X,Y,Z,a,b,c,d,e,f,g,h,i,k,m,n,o,p,r,s,t,u,v,w,x,y,z".
DEF VAR cOsUserName AS CHAR.
DEF VAR cAdminList AS CHAR NO-UNDO INITIAL "Administrator,DevAdmin,CustAdmin,mark.tyndall,terry.ellist,SysAdmin,tyndm".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiUserID Btn_OK Btn_Cancel rsType 
&Scoped-Define DISPLAYED-OBJECTS fiText2 fiUserID rsType fiLength tbNumbers ~
tbSymbols fiPassword 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-GO 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fiLength AS INTEGER FORMAT ">9":U INITIAL 10 
     LABEL "Length" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U INITIAL "XXXXXXXXXX" 
     LABEL "Your password is" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1.19
     FGCOLOR 12 FONT 37 NO-UNDO.

DEFINE VARIABLE fiText2 AS CHARACTER FORMAT "X(256)":U INITIAL "(it has been copied to your clipboard)" 
      VIEW-AS TEXT 
     SIZE 40 BY .62 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enter your Windows User ID" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE rsType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "ASI Standard", "Standard",
"New Random", "Random"
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE tbNumbers AS LOGICAL INITIAL yes 
     LABEL "Include Numbers" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tbSymbols AS LOGICAL INITIAL yes 
     LABEL "Include Symbols" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiText2 AT ROW 9.33 COL 32 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiUserID AT ROW 1.95 COL 32 COLON-ALIGNED
     Btn_OK AT ROW 1.71 COL 81
     Btn_Cancel AT ROW 2.95 COL 81
     rsType AT ROW 3.38 COL 34 NO-LABEL
     fiLength AT ROW 4.33 COL 40 COLON-ALIGNED
     tbNumbers AT ROW 5.52 COL 34
     tbSymbols AT ROW 6.48 COL 34
     fiPassword AT ROW 7.91 COL 32 COLON-ALIGNED NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 105.4 BY 9.76
         DEFAULT-BUTTON Btn_OK.


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
         TITLE              = "ASI Password Retrieval/Generator"
         HEIGHT             = 9.76
         WIDTH              = 105.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 132.6
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 132.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR FILL-IN fiLength IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPassword IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiPassword:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiText2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiText2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TOGGLE-BOX tbNumbers IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbSymbols IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ASI Password Retrieval/Generator */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ASI Password Retrieval/Generator */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY 'window-close' TO c-Win.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
    DEF VAR cWorking AS CHAR NO-UNDO.
    DEF VAR iPos AS INT NO-UNDO.
    DEF VAR lIsNumber AS LOG NO-UNDO.
    DEF VAR lIsSymbol AS LOG NO-UNDO.
    DEF VAR cPos AS CHAR NO-UNDO.
    
    ASSIGN 
        cWorking = ENCODE(fiUserID:SCREEN-VALUE)
        cWorking = ENCODE(cWorking).
        
    IF rsType:SCREEN-VALUE EQ "Random" THEN DO iPos = 1 TO LENGTH(cWorking):
        ASSIGN 
            SUBSTRING(cWorking,iPos,1) = ENTRY(RANDOM(1,NUM-ENTRIES(cLetterList)),cLetterList).
        ASSIGN 
            lIsNumber = IF (tbNumbers:CHECKED AND RANDOM(1,3) EQ 2) THEN TRUE ELSE FALSE 
            lIsSymbol = IF (tbSymbols:CHECKED AND RANDOM(1,5) EQ 3) THEN TRUE ELSE FALSE.
        IF lIsNumber THEN ASSIGN 
            SUBSTRING(cWorking,iPos,1) = STRING(RANDOM(2,9),"9").
        ELSE IF lIsSymbol THEN ASSIGN 
            SUBSTRING(cWorking,iPos,1) = ENTRY(RANDOM(1,7),"!,@,#,$,%,^,&").
        ASSIGN 
            cWorking = SUBSTRING(cWorking,1,integer(fiLength:SCREEN-VALUE)).
    END.
    ELSE DO:
        ASSIGN 
            cWorking = SUBSTRING(cWorking,1,10). 
        DO iPos = 1 TO LENGTH(cWorking):
            ASSIGN 
                cPos = SUBSTRING(cWorking,iPos,1).
            IF cPos = UPPER("I") THEN ASSIGN cPos = "3".
            ELSE IF cPos = UPPER("O") THEN ASSIGN cPos = "4".
            ELSE IF cPos = UPPER("Q") THEN ASSIGN cPos = "5".
            ELSE IF cPos = LOWER("j") THEN ASSIGN cPos = "$".
            ELSE IF cPos = LOWER("l") THEN ASSIGN cPos = "&".
            ELSE IF cPos = LOWER("q") THEN ASSIGN cPos = "^".
            ELSE IF cPos = "0" THEN ASSIGN cPos = "5".
            ELSE IF cPos = "1" THEN ASSIGN cPos = "6".
            ASSIGN 
                SUBSTRING(cWorking,iPos,1) = cPos.
        END.
    END.
        
    ASSIGN 
        fiPassword:SCREEN-VALUE = cWorking.
    CLIPBOARD:VALUE = cWorking.
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsType C-Win
ON VALUE-CHANGED OF rsType IN FRAME DEFAULT-FRAME
DO:
    IF SELF:SCREEN-VALUE EQ "Standard" THEN ASSIGN 
        fiLength:SENSITIVE = FALSE 
        tbNumbers:SENSITIVE = FALSE 
        tbSymbols:SENSITIVE = FALSE.
    ELSE ASSIGN  
        fiLength:SENSITIVE = TRUE 
        tbNumbers:SENSITIVE = TRUE 
        tbSymbols:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
  ASSIGN 
    cOsUserName = OS-GETENV("USERNAME")
    fiUserID:SENSITIVE = CAN-DO(cAdminList,cOsUserName)
    fiUserID:SCREEN-VALUE = cOsUserName.
  APPLY 'choose' TO btn_OK.  
  APPLY 'entry' TO fiUserID IN FRAME {&frame-name}.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
QUIT.

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
  DISPLAY fiText2 fiUserID rsType fiLength tbNumbers tbSymbols fiPassword 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiUserID Btn_OK Btn_Cancel rsType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

