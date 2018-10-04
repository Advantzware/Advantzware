&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-copied AS LOG NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-shipto FOR shipto.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust begin_ship end_cust end_ship ~
btn-process btn-cancel RECT-17 RECT-18 RECT-19 
&Scoped-Define DISPLAYED-OBJECTS begin_cust begin_cname begin_ship ~
begin_sname end_cust end_cname end_ship 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cname AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "x(8)" INITIAL "001" 
     LABEL "From Customer" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_ship AS CHARACTER FORMAT "x(8)" 
     LABEL "From Ship-To ID" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_sname AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE end_cname AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "x(8)" INITIAL "001" 
     LABEL "To Customer" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE end_ship AS CHARACTER FORMAT "x(8)" 
     LABEL "To Ship-To ID" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 10.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 4.52.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 3.14 COL 22 COLON-ALIGNED HELP
          "Enter Customer To Copy From"
     begin_cname AT ROW 3.14 COL 38 COLON-ALIGNED NO-LABEL
     begin_ship AT ROW 4.1 COL 22 COLON-ALIGNED HELP
          "Enter Ship-To to Be Copied From"
     begin_sname AT ROW 4.1 COL 38 COLON-ALIGNED NO-LABEL
     end_cust AT ROW 8.38 COL 22 COLON-ALIGNED HELP
          "Copy To Customer"
     end_cname AT ROW 8.38 COL 38 COLON-ALIGNED NO-LABEL
     end_ship AT ROW 9.33 COL 22 COLON-ALIGNED HELP
          "Enter Ship-To to Be Copied To"
     btn-process AT ROW 11.48 COL 26
     btn-cancel AT ROW 11.48 COL 57
     RECT-17 AT ROW 1 COL 1
     RECT-18 AT ROW 2.19 COL 2
     RECT-19 AT ROW 7.67 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .95 AT ROW 1.24 COL 4
          FONT 4
     "C O P Y  T O" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 7.19 COL 40
          BGCOLOR 8 FGCOLOR 9 
     "C O P Y  F R O M" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 1.95 COL 38
          BGCOLOR 8 FGCOLOR 9 
     "(Enter spaces to copy all)" VIEW-AS TEXT
          SIZE 31 BY 1 AT ROW 5.29 COL 26
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.8 BY 12.14
         FONT 6.


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
         TITLE              = "Copy Customer Ship-To"
         HEIGHT             = 12.24
         WIDTH              = 96.8
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 6
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN begin_cname IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_sname IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN end_cname IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Copy Customer Ship-To */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Copy Customer Ship-To */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* From Customer */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON VALUE-CHANGED OF begin_cust IN FRAME FRAME-A /* From Customer */
DO:
  RUN new-cust (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship C-Win
ON LEAVE OF begin_ship IN FRAME FRAME-A /* From Ship-To ID */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-begin_ship NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship C-Win
ON VALUE-CHANGED OF begin_ship IN FRAME FRAME-A /* From Ship-To ID */
DO:
  RUN new-begin_ship.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.


  RUN valid-cust (begin_cust:HANDLE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-begin_ship NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust (end_cust:HANDLE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-to_ship NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    IF begin_ship:SCREEN-VALUE EQ ""                           AND
       begin_cust:SCREEN-VALUE EQ end_cust:SCREEN-VALUE THEN DO:
      MESSAGE "From & To Companies may not be the same when copying all..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO end_cust.
      RETURN NO-APPLY.
    END.

    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) + " " +
          TRIM(STRING(begin_ship)) + " from Customer " + TRIM(begin_cust) + " to " +
          " Customer " + TRIM(end_cust) + "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE v-process.

  IF v-process THEN RUN run-process.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* To Customer */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON VALUE-CHANGED OF end_cust IN FRAME FRAME-A /* To Customer */
DO:
  RUN new-cust (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship C-Win
ON LEAVE OF end_ship IN FRAME FRAME-A /* To Ship-To ID */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-to_ship NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FIND shipto WHERE ROWID(shipto) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL shipto THEN DO:
    ASSIGN
     cocode     = shipto.company
     begin_cust = shipto.cust-no
     end_cust   = shipto.cust-no
     begin_ship = shipto.ship-id
     end_ship   = shipto.ship-id.

    RUN enable_UI.

    DO WITH FRAME {&FRAME-NAME}:
      RUN new-cust (begin_cust:HANDLE).
      RUN new-cust (end_cust:HANDLE).
    END.

    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.
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
  DISPLAY begin_cust begin_cname begin_ship begin_sname end_cust end_cname 
          end_ship 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust begin_ship end_cust end_ship btn-process btn-cancel RECT-17 
         RECT-18 RECT-19 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-begin_ship C-Win 
PROCEDURE new-begin_ship :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND b-shipto NO-LOCK
        WHERE b-shipto.company EQ cocode
          AND b-shipto.cust-no EQ begin_cust:SCREEN-VALUE
          AND b-shipto.ship-id EQ begin_ship:SCREEN-VALUE
        NO-ERROR.
    IF AVAIL b-shipto THEN DO:
      ASSIGN
       begin_sname:SCREEN-VALUE = b-shipto.ship-name
       end_ship:SCREEN-VALUE    = b-shipto.ship-id.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cust C-Win 
PROCEDURE new-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND cust NO-LOCK
        WHERE cust.company EQ cocode
          AND cust.cust-no BEGINS ip-focus:SCREEN-VALUE
        NO-ERROR.

    IF AVAIL cust THEN DO:
      ip-focus:SCREEN-VALUE = cust.cust-no.

      IF ip-focus:NAME EQ "begin_cust" THEN DO:
        begin_cname:SCREEN-VALUE = cust.name.
        RUN new-begin_ship.
      END.

      ELSE end_cname:SCREEN-VALUE = cust.name.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     copy order record
  Parameters:  <none>
  Notes:       copyOrder procedure is contained in oe/copyOrder.i include
------------------------------------------------------------------------------*/
  DEF VAR new-ship LIKE shipto.ship-id NO-UNDO.
  DEF VAR li-no LIKE shipto.ship-no NO-UNDO.

  DEF BUFFER b-reft FOR reftable.

  &SCOPED-DEFINE where-jded-id WHERE reftable.reftable EQ "JDEDWARDCUST#" ~
                                 AND reftable.company  EQ cocode          ~
                                 AND reftable.loc      EQ ""              ~
                                 AND reftable.code     EQ shipto.cust-no  ~
                                 AND reftable.code2    EQ shipto.ship-id

  &SCOPED-DEFINE where-mand-tax WHERE reftable.reftable EQ "shipto.mandatory-tax" ~
                                  AND reftable.company  EQ shipto.company         ~
                                  AND reftable.loc      EQ ""                     ~
                                  AND reftable.code     EQ shipto.cust-no         ~
                                  AND reftable.code2    EQ shipto.ship-id


  SESSION:SET-WAIT-STATE("general").

  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH shipto NO-LOCK
        WHERE shipto.company  EQ cocode
          AND shipto.cust-no  EQ begin_cust
          AND (shipto.ship-id EQ begin_ship OR begin_ship EQ ""):

      new-ship = IF begin_ship EQ "" THEN shipto.ship-id ELSE end_ship.

      IF NOT CAN-FIND(FIRST b-shipto
                      WHERE b-shipto.company EQ cocode
                        AND b-shipto.cust-no EQ end_cust
                        AND b-shipto.ship-id EQ new-ship) THEN DO:

        FIND LAST b-shipto NO-LOCK
            WHERE b-shipto.company eq cocode
              AND b-shipto.cust-no eq end_cust
            USE-INDEX ship-no NO-ERROR.

        li-no = (IF AVAIL b-shipto THEN b-shipto.ship-no ELSE 0) + 1.

        CREATE b-shipto.
        BUFFER-COPY shipto EXCEPT rec_key TO b-shipto
        ASSIGN
         b-shipto.ship-no = li-no
         b-shipto.cust-no = end_cust
         b-shipto.ship-id = new-ship.

        FOR EACH reftable NO-LOCK {&where-jded-id}:
          CREATE b-reft.
          BUFFER-COPY reftable EXCEPT rec_key TO b-reft
          ASSIGN
           b-reft.code  = end_cust
           b-reft.code2 = new-ship.
        END.

        FOR EACH reftable NO-LOCK {&where-mand-tax}:
          CREATE b-reft.
          BUFFER-COPY reftable EXCEPT rec_key TO b-reft
          ASSIGN
           b-reft.code  = end_cust
           b-reft.code2 = new-ship.
        END.
      END.
    END.
  END.

  op-copied = YES.

  SESSION:SET-WAIT-STATE("").

  MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-begin_ship C-Win 
PROCEDURE valid-begin_ship :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF v-msg EQ "" THEN
      IF begin_ship:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST b-shipto
                      WHERE b-shipto.company EQ cocode
                        AND b-shipto.cust-no EQ begin_cust:SCREEN-VALUE
                        AND b-shipto.ship-id EQ begin_ship:SCREEN-VALUE) THEN
        v-msg = TRIM(begin_ship:LABEL) + " doesn't exist, try help...".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_ship.
      RETURN ERROR.
    END.

    IF begin_ship:SCREEN-VALUE EQ "" THEN
      ASSIGN
       end_ship:SCREEN-VALUE = ""
       end_ship:SENSITIVE    = NO.

    ELSE DO:
      end_ship:SENSITIVE = YES.
      IF end_ship EQ "" THEN
        end_ship:SCREEN-VALUE = begin_ship:SCREEN-VALUE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust C-Win 
PROCEDURE valid-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.

  DEF VAR lv-msg AS CHAR INIT "" NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ cocode
                      AND cust.cust-no EQ ip-focus:SCREEN-VALUE) THEN
      lv-msg = "Invalid entry, try help".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-to_ship C-Win 
PROCEDURE valid-to_ship :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF v-msg EQ "" THEN
      IF end_ship:SCREEN-VALUE NE "" AND
         CAN-FIND(FIRST b-shipto
                  WHERE b-shipto.company EQ cocode
                    AND b-shipto.cust-no EQ end_cust:SCREEN-VALUE
                    AND b-shipto.ship-id EQ end_ship:SCREEN-VALUE) THEN
        v-msg = TRIM(end_ship:LABEL) + " already exists, please re-enter...".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO end_ship.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

