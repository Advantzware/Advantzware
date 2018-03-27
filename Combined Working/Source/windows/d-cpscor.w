&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
DEF INPUT PARAM ip-style LIKE style.style NO-UNDO.
DEF INPUT PARAM ip-flute LIKE flute.code NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_style end_style begin_flute end_flute ~
tb_reverse Btn_OK Btn_Cancel RECT-41 
&Scoped-Define DISPLAYED-OBJECTS begin_style end_style begin_flute ~
end_flute tb_reverse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_flute AS CHARACTER FORMAT "X(3)":U 
     LABEL "From Flute" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_style AS CHARACTER FORMAT "X(6)":U 
     LABEL "From Style" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_flute AS CHARACTER FORMAT "X(3)":U 
     LABEL "To Flute" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_style AS CHARACTER FORMAT "X(6)":U 
     LABEL "To Style" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 6.19.

DEFINE VARIABLE tb_reverse AS LOGICAL INITIAL no 
     LABEL "Reverse scores" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_style AT ROW 1.71 COL 17 COLON-ALIGNED
     end_style AT ROW 1.71 COL 49 COLON-ALIGNED
     begin_flute AT ROW 4.33 COL 17 COLON-ALIGNED
     end_flute AT ROW 4.33 COL 49 COLON-ALIGNED
     tb_reverse AT ROW 5.76 COL 29
     Btn_OK AT ROW 7.67 COL 17
     Btn_Cancel AT ROW 7.67 COL 44
     RECT-41 AT ROW 1 COL 1
     "(Change to blank spaces for all styles)" VIEW-AS TEXT
          SIZE 44 BY .62 AT ROW 2.91 COL 11
     SPACE(17.59) SKIP(5.89)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Copy Scores"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Copy Scores */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_flute Dialog-Frame
ON LEAVE OF begin_flute IN FRAME Dialog-Frame /* From Flute */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-flute (FOCUS).
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_style Dialog-Frame
ON LEAVE OF begin_style IN FRAME Dialog-Frame /* From Style */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-style (FOCUS).
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_style Dialog-Frame
ON VALUE-CHANGED OF begin_style IN FRAME Dialog-Frame /* From Style */
DO:
  IF TRIM({&self-name}:SCREEN-VALUE) EQ "" THEN
    ASSIGN
     end_style:SCREEN-VALUE = ""
     end_style:SENSITIVE    = NO.
  ELSE
    end_style:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-style (begin_style:HANDLE).
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-style (end_style:HANDLE).
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-flute (begin_flute:HANDLE).
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-flute (end_flute:HANDLE).
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    ASSIGN {&displayed-objects}.
  END.
   
  MESSAGE "Are you sure you want to " + TRIM(FRAME {&FRAME-NAME}:TITLE) +
          " for the selected parameters?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll.
  IF ll THEN RUN copy-scores.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_flute Dialog-Frame
ON LEAVE OF end_flute IN FRAME Dialog-Frame /* To Flute */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-flute (FOCUS).
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_style Dialog-Frame
ON LEAVE OF end_style IN FRAME Dialog-Frame /* To Style */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-style (FOCUS).
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  ASSIGN
   begin_style = ip-style
   end_style   = ip-style
   begin_flute = ip-flute.

  RUN enable_UI.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-scores Dialog-Frame 
PROCEDURE copy-scores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER fr-style FOR style.
DEF BUFFER to-style FOR style. 
DEF BUFFER fr-styfl FOR reftable.
DEF BUFFER to-styfl FOR reftable.
DEF BUFFER fr-stysc FOR reftable.
DEF BUFFER to-stysc FOR reftable.

DEF VAR li AS INT NO-UNDO.
DEF VAR li-score AS INT EXTENT 3 NO-UNDO.
  

SESSION:SET-WAIT-STATE("general").

FOR EACH fr-style NO-LOCK
    WHERE fr-style.company EQ cocode
      AND (fr-style.style  EQ begin_style OR begin_style EQ ""):

  DO li = 1 TO LENGTH(fr-style.formula[1]):
    IF SUBSTR(fr-style.formula[1],li,1) EQ "+" THEN
      li-score[1] = li-score[1] + 1.
  END.
  DO li = 1 TO LENGTH(fr-style.formula[2]):
    IF SUBSTR(fr-style.formula[2],li,1) EQ "+" THEN
      li-score[2] = li-score[2] + 1.
  END.

  FIND FIRST to-style NO-LOCK
      WHERE to-style.company EQ fr-style.company
        AND (to-style.style  EQ end_style OR
             (begin_style EQ "" AND end_style EQ "" AND
              to-style.style EQ fr-style.style))
      NO-ERROR.

  IF AVAIL to-style THEN DO:
    FOR EACH fr-styfl NO-LOCK
        WHERE fr-styfl.reftable EQ "STYFLU"
          AND fr-styfl.company  EQ fr-style.style
          AND fr-styfl.loc      GE begin_flute
          AND fr-styfl.loc      LE end_flute
          AND CAN-DO("1,2,3,4,5,6,7",fr-styfl.code)

        TRANSACTION:

      FIND FIRST to-styfl
          WHERE to-styfl.reftable EQ fr-styfl.reftable
            AND to-styfl.company  EQ to-style.style
            AND to-styfl.loc      EQ /*end_flute*/ fr-styfl.loc
            AND to-styfl.code     EQ fr-styfl.code
            AND to-styfl.code2    EQ fr-styfl.code2
          NO-ERROR.

      IF NOT AVAIL to-styfl THEN DO:
        CREATE to-styfl.
        ASSIGN
         to-styfl.reftable = fr-styfl.reftable
         to-styfl.company  = to-style.style
         to-styfl.loc      = /*end_flute*/ fr-styfl.loc
         to-styfl.code     = fr-styfl.code
         to-styfl.code2    = fr-styfl.code2.
      END.

      BUFFER-COPY fr-styfl EXCEPT reftable company loc code code2 rec_key TO
                  to-styfl.

      IF tb_reverse THEN DO:
        li-score[3] = IF fr-styfl.code GT "2" THEN li-score[2] ELSE li-score[1].

        DO li = li-score[3] TO 1 BY -1:
          to-styfl.val[li] = fr-styfl.val[li-score[3] - li + 1].
        END.
      END.
    END.

    FOR EACH fr-stysc NO-LOCK
        WHERE fr-stysc.reftable EQ "STYSCORE"
          AND fr-stysc.company  EQ fr-style.style
          AND fr-stysc.loc      GE begin_flute
          AND fr-stysc.loc      LE end_flute
          AND CAN-DO("1,2,3,4,5,6,7",fr-stysc.code)
        TRANSACTION:

      FIND FIRST to-stysc
          WHERE to-stysc.reftable EQ fr-stysc.reftable
            AND to-stysc.company  EQ to-style.style
            AND to-stysc.loc      EQ /*end_flute*/ fr-stysc.loc
            AND to-stysc.code     EQ fr-stysc.code
            AND to-stysc.code2    EQ fr-stysc.code2
          NO-ERROR.

      IF NOT AVAIL to-stysc THEN DO:
        CREATE to-stysc.
        ASSIGN
         to-stysc.reftable = fr-stysc.reftable
         to-stysc.company  = to-style.style
         to-stysc.loc      = /*end_flute*/ fr-stysc.loc
         to-stysc.code     = fr-stysc.code
         to-stysc.code2    = fr-stysc.code2.
      END.

      BUFFER-COPY fr-stysc EXCEPT reftable company loc code code2 rec_key TO
                  to-stysc.

      IF tb_reverse THEN DO:
        ASSIGN
         to-stysc.dscr = ""
         li-score[3] = IF fr-stysc.code GT "2" THEN li-score[2] ELSE li-score[1].

        DO li = li-score[3] TO 1 BY -1:
          to-stysc.dscr = to-stysc.dscr + SUBST(fr-stysc.dscr,li,1).
        END.
      END.
    END.
  END.
END.

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(FRAME {&FRAME-NAME}:TITLE) + " process completed..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-scores-old Dialog-Frame 
PROCEDURE copy-scores-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR i AS INT NO-UNDO.

/*dEF TEMP-TABLE tt-array FIELD tt-dec AS DEC FIELD tt-type AS CHAR.*/
  DEF BUFFER from-style FOR style.
  DEF BUFFER from-flute FOR flute.
  DEF BUFFER to-style FOR style.
  DEF BUFFER to-flute FOR flute.
  DEF BUFFER bf-reftable FOR reftable.
  DEF BUFFER to-reftable FOR reftable.
  DEF BUFFER to2-reftable FOR reftable.
  DEF VAR v-formula AS cha NO-UNDO.
  DEF VAR v-num-of-plus AS INT NO-UNDO.
  DEF VAR v-ref-dscr AS cha NO-UNDO.

  SESSION:SET-WAIT-STATE("general").
  FIND FIRST from-style WHERE from-style.company = g_company 
                          AND from-style.style = begin_style NO-LOCK NO-ERROR.
  FIND FIRST to-style WHERE to-style.company = g_company 
                        AND to-style.style = end_style NO-LOCK NO-ERROR.

  FOR EACH flute NO-LOCK WHERE flute.company EQ g_company:
    IF tb_reverse THEN DO:
       FIND first reftable where reftable.reftable = "STYFLU" 
                             and reftable.company = from-style.style
                             and reftable.loc = flute.code
                             and reftable.code = "1"  /* Joint tab */
                             no-lock no-error.
       IF AVAIL reftable THEN DO:
             FIND first to-reftable where to-reftable.reftable = "STYFLU" 
                             and to-reftable.company = to-style.style
                             and to-reftable.loc = flute.code
                             and to-reftable.code = "1"  
                             no-error
                             .
             IF NOT AVAIL to-reftable THEN do:
                CREATE to-reftable.
                ASSIGN to-reftable.company = to-style.style.
             END.
             BUFFER-COPY reftable EXCEPT reftable.company TO to-reftable.
       END.
       v-formula = from-style.formula[1].
       v-num-of-plus = 0.
       do i = 1 to length(v-formula):
          if substr(v-formula, i, 1) eq "+"  then v-num-of-plus = v-num-of-plus + 1.
       end.
       {est/copysco2.i 2} /* blank width score*/
       v-formula = from-style.formula[2].
       v-num-of-plus = 0.
       do i = 1 to length(v-formula):
          if substr(v-formula, i, 1) eq "+"  then v-num-of-plus = v-num-of-plus + 1.
       end.
       {est/copysco2.i 3} /* glue tab in*/
       {est/copysco2.i 4} /* glue tab out*/
       {est/copysco2.i 5} /* Stitch tab in*/
       {est/copysco2.i 6} /* Stitch tab Out*/
       {est/copysco2.i 7} /* Tape score total*/
    END.
    ELSE DO:

      FIND first reftable where reftable.reftable = "STYFLU" 
                         and reftable.company = from-style.style
                         and reftable.loc = flute.code
                         and reftable.code = "1"  /* Joint tab */
                         no-lock no-error.
      IF AVAIL reftable THEN DO:
         FIND first to-reftable where to-reftable.reftable = "STYFLU" 
                         and to-reftable.company = to-style.style
                         and to-reftable.loc = flute.code
                         and to-reftable.code = "1"  
                         no-error
                         .
         IF NOT AVAIL to-reftable THEN do:
            CREATE to-reftable.
            ASSIGN to-reftable.company = to-style.style.
         END.
         BUFFER-COPY reftable EXCEPT reftable.company TO to-reftable.
      END.

      {est/copyscor.i 2} /* blank width score*/
      {est/copyscor.i 3} /* glue tab in*/
      {est/copyscor.i 4} /* glue tab out*/
      {est/copyscor.i 5} /* Stitch tab in*/
      {est/copyscor.i 6} /* Stitch tab Out*/
      {est/copyscor.i 7} /* Tape score total*/
    END.
  END.
  SESSION:SET-WAIT-STATE("").
  MESSAGE "Process completed." VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY begin_style end_style begin_flute end_flute tb_reverse 
      WITH FRAME Dialog-Frame.
  ENABLE begin_style end_style begin_flute end_flute tb_reverse Btn_OK 
         Btn_Cancel RECT-41 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-flute Dialog-Frame 
PROCEDURE valid-flute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF lv-msg EQ ""                                               AND
       NOT CAN-FIND(FIRST flute
                    WHERE flute.company EQ cocode
                      AND flute.code    EQ ip-focus:SCREEN-VALUE) THEN
      lv-msg = "is invalid, try help...".

    IF lv-msg EQ ""                                                   AND
       ip-focus:NAME BEGINS "end_"                                    AND
       TRIM(begin_style:SCREEN-VALUE) EQ TRIM(end_style:SCREEN-VALUE) AND
       TRIM(ip-focus:SCREEN-VALUE) EQ TRIM(begin_flute:SCREEN-VALUE)  THEN
      lv-msg = "may not be the same as " + TRIM(begin_flute:NAME).

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " " + TRIM(lv-msg)
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style Dialog-Frame 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF ip-focus:SCREEN-VALUE NE ""                                AND
       NOT CAN-FIND(FIRST style
                    WHERE style.company EQ cocode
                      AND style.style   EQ ip-focus:SCREEN-VALUE)
    THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

