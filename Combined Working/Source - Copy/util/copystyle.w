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

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS from-company to-company from-style to-style ~
tb_flute Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS from-company to-company from-style ~
to-style tb_flute v-status 

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

DEFINE VARIABLE from-company AS CHARACTER FORMAT "X(3)":U 
     LABEL "From Company" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE from-style AS CHARACTER FORMAT "X(6)":U 
     LABEL "From Style" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE to-company AS CHARACTER FORMAT "X(3)":U 
     LABEL "To Company" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE to-style AS CHARACTER FORMAT "X(6)":U 
     LABEL "To Style" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1.1 NO-UNDO.

DEFINE VARIABLE tb_flute AS LOGICAL INITIAL no 
     LABEL "Copy Flute?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     from-company AT ROW 2.91 COL 22 COLON-ALIGNED
     to-company AT ROW 2.91 COL 53 COLON-ALIGNED
     from-style AT ROW 4.57 COL 22 COLON-ALIGNED
     to-style AT ROW 4.57 COL 53 COLON-ALIGNED
     tb_flute AT ROW 6.24 COL 24
     Btn_OK AT ROW 8.62 COL 21
     Btn_Cancel AT ROW 8.62 COL 55
     v-status AT ROW 10.76 COL 1 NO-LABEL
     SPACE(0.79) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 2
         TITLE "Copy Styles from company to company"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-status IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Copy Styles from company to company */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN {&DISPLAYED-OBJECTS}.
           
    MESSAGE "Are you sure you want to copy Styles?" VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN RUN copy-styles.

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
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-styles Dialog-Frame 
PROCEDURE copy-styles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER bf-style FOR style.
   DEF BUFFER bf-reftable FOR reftable.
   DEF BUFFER bf-flute FOR flute.
   DEF BUFFER bf-stack-flute FOR stack-flute.
   DEF BUFFER bf-routing-mtx FOR routing-mtx.
   DEF BUFFER bf-box-design-hdr FOR box-design-hdr.

   DEF VAR v-loc AS cha NO-UNDO.
   DEF VAR v-rec_key AS cha NO-UNDO.

   SESSION:SET-WAIT-STATE("general").

   FOR each style NO-LOCK WHERE style.company = from-company
                            AND style.style GE from-style
                            AND style.style LE to-style:
       FIND FIRST bf-style WHERE bf-style.company = to-company
                             AND bf-style.style = style.style NO-ERROR.
       v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Copying Style: " + style.style .
       
       IF NOT AVAIL bf-style THEN DO:
          CREATE bf-style.
          RUN util/reckey3.p ("Style",OUTPUT v-rec_key).
          BUFFER-COPY style EXCEPT company rec_key TO bf-style
             ASSIGN bf-style.company = to-company
                    bf-style.rec_key = v-rec_key.
       END.

       IF tb_flute THEN DO:
        
         FOR EACH flute NO-LOCK WHERE flute.company = from-company :
             IF NOT CAN-FIND(FIRST bf-flute WHERE bf-flute.company = to-company 
                                               AND bf-flute.CODE = flute.CODE)
             THEN DO:              
                  CREATE bf-flute.
                  RUN util/reckey3.p ("flute",OUTPUT v-rec_key).
                  BUFFER-COPY flute EXCEPT flute.company flute.rec_key TO bf-flute
                     ASSIGN bf-flute.company = to-company
                            bf-flute.rec_key = v-rec_key.
             END.
             FOR EACH stack-flute NO-LOCK WHERE stack-flute.company  = from-company
                                        /*AND ASI.stack-flute.loc  = gloc*/
                                           AND stack-flute.code = flute.CODE :
                 FIND FIRST usercomp WHERE usercomp.USER_id = USERID('nosweat')
                                       AND usercomp.company = to-company
                                       AND usercomp.loc_default NO-LOCK NO-ERROR.
                 IF NOT AVAIL usercomp THEN
                    FIND FIRST usercomp WHERE usercomp.company = to-company
                                          AND usercomp.loc_default NO-LOCK NO-ERROR.
                 IF NOT AVAIL usercomp THEN
                    FIND FIRST loc WHERE loc.company = to-company NO-LOCK NO-ERROR.
                                     
                 v-loc = IF AVAIL usercomp THEN usercomp.loc
                         ELSE IF AVAIL loc THEN loc.loc
                         ELSE "".

                 IF NOT CAN-FIND(FIRST bf-stack-flute WHERE bf-stack-flute.company = to-company
                                      AND bf-stack-flute.loc = v-loc AND bf-stack-flute.CODE = stack-flute.CODE)
                 THEN DO:
                      CREATE bf-stack-flute.
                      RUN util/reckey3.p ("stack-flute",OUTPUT v-rec_key).
                      BUFFER-COPY stack-flute EXCEPT stack-flute.company stack-flute.loc stack-flute.rec_key TO bf-stack-flute
                         ASSIGN bf-stack-flute.company = to-company
                                bf-stack-flute.loc = v-loc
                                bf-stack-flute.rec_key = v-rec_key.

                 END.
             END. /*stack-flute*/
         END. /*flute*/
       END. /* tb_flute*/

       FOR EACH routing-mtx OF style NO-LOCK:
           IF NOT CAN-FIND(FIRST bf-routing-mtx WHERE bf-routing-mtx.company = to-company
                                 AND bf-routing-mtx.style = style.style AND bf-routing-mtx.msf = routing-mtx.msf)
           THEN DO:
              CREATE bf-routing-mtx.
              RUN util/reckey3.p ("routing-mtx",OUTPUT v-rec_key).
              BUFFER-COPY routing-mtx EXCEPT company rec_key TO bf-routing-mtx
                 ASSIGN bf-routing-mtx.company = to-company
                        bf-routing-mtx.rec_key = v-rec_key.
           END.
       END.

       FOR EACH box-design-hdr NO-LOCK WHERE box-design-hdr.design-no = style.design-no
                                         and box-design-hdr.company = style.company :
           IF NOT CAN-FIND(FIRST bf-box-design-hdr WHERE bf-box-design-hdr.design-no = style.design-no
                                         and bf-box-design-hdr.company = to-company)
           THEN DO:
               CREATE bf-box-design-hdr.
               RUN util/reckey3.p ("box-design-hdr",OUTPUT v-rec_key).
               BUFFER-COPY box-design-hdr EXCEPT box-design-hdr.company box-design-hdr.rec_key TO bf-box-design-hdr
                  ASSIGN bf-box-design-hdr.company = to-company
                         bf-box-design-hdr.rec_key = v-rec_key.
           END.
       END.
    END.  /* each style*/
    
    MESSAGE "Style Copy Completed." VIEW-AS ALERT-BOX.
    APPLY "go" TO FRAME {&FRAME-NAME}.

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
  DISPLAY from-company to-company from-style to-style tb_flute v-status 
      WITH FRAME Dialog-Frame.
  ENABLE from-company to-company from-style to-style tb_flute Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

