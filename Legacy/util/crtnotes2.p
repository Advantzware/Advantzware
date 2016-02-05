&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

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

{custom/globdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-8 RECT-9 RECT-10 RECT-11 tg_notes ~
tg_title Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tg_notes v-f-cust v-t-cust tg_title ~
v-erel-fcust v-status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tg_notes v-f-cust v-t-cust 
&Scoped-define List-2 tg_title v-erel-fcust 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE v-erel-fcust AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Customer" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-f-cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer Range Begin" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1 NO-UNDO.

DEFINE VARIABLE v-t-cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "End" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 13.57.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92.6 BY 1.38.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 4.29.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 3.81.

DEFINE VARIABLE tg_notes AS LOGICAL INITIAL no 
     LABEL "Copy Department Notes From Customer to Estimates?" 
     VIEW-AS TOGGLE-BOX
     SIZE 66 BY 1.19
     FONT 6 NO-UNDO.

DEFINE VARIABLE tg_title AS LOGICAL INITIAL no 
     LABEL "Copy ERelease Note to All Customer?" 
     VIEW-AS TOGGLE-BOX
     SIZE 64 BY .81
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     tg_notes AT ROW 2.19 COL 16
     v-f-cust AT ROW 3.86 COL 36 COLON-ALIGNED
     v-t-cust AT ROW 3.86 COL 63 COLON-ALIGNED
     tg_title AT ROW 7.19 COL 16
     v-erel-fcust AT ROW 8.38 COL 39 COLON-ALIGNED
     Btn_OK AT ROW 12.43 COL 18
     Btn_Cancel AT ROW 12.43 COL 60
     v-status AT ROW 15.29 COL 2 NO-LABEL
     RECT-8 AT ROW 1.71 COL 2
     RECT-9 AT ROW 6.48 COL 2
     RECT-10 AT ROW 1 COL 1
     RECT-11 AT ROW 15.05 COL 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Copy Customer Notes"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
                                                                        */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tg_notes IN FRAME gDialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg_title IN FRAME gDialog
   2                                                                    */
/* SETTINGS FOR FILL-IN v-erel-fcust IN FRAME gDialog
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN v-f-cust IN FRAME gDialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN v-status IN FRAME gDialog
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN v-t-cust IN FRAME gDialog
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON HELP OF FRAME gDialog /* Copy Customer Notes */
DO:
   DEF VAR char-val AS cha NO-UNDO.

   IF lookup(FOCUS:NAME,"v-f-cust,v-t-cust,v-erel-fcust") > 0 THEN DO:
      RUN windows/l-cust.p (g_company,FOCUS:SCREEN-VALUE,OUTPUT char-val).
      IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Copy Customer Notes */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
   DEF VAR ll-ans AS LOG NO-UNDO.

   ASSIGN {&list-1} {&list-2}.
   v-status:SCREEN-VALUE = "".

   IF tg_notes AND tg_title THEN
      MESSAGE "Are you sure you want to copy Dept notes and ERelease Note?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
   ELSE IF tg_notes THEN
      MESSAGE "Are you sure you want to copy Dept notes?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
   IF tg_title THEN
      MESSAGE "Are you sure you want to copy ERelease Note?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
   IF ll-ans THEN DO:

      SESSION:SET-WAIT-STATE("general").

      IF tg_notes THEN RUN trans-notes.
      IF tg_title THEN RUN trans-phone.

      SESSION:SET-WAIT-STATE("").
      MESSAGE "Completed." VIEW-AS ALERT-BOX.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_notes gDialog
ON VALUE-CHANGED OF tg_notes IN FRAME gDialog /* Copy Department Notes From Customer to Estimates? */
DO:
   ASSIGN tg_notes.
   IF tg_notes THEN DO:
      ASSIGN v-f-cust:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             v-t-cust:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             v-t-cust:SCREEN-VALUE = "zzzzzzzz".
      APPLY "entry" TO v-f-cust.
   END.
   ELSE DO:
      ASSIGN v-f-cust:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             v-t-cust:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             v-f-cust:SCREEN-VALUE = ""
             v-t-cust:SCREEN-VALUE = "".
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_title
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_title gDialog
ON VALUE-CHANGED OF tg_title IN FRAME gDialog /* Copy ERelease Note to All Customer? */
DO:
  ASSIGN tg_title.
  IF tg_title THEN do:
     v-erel-fcust:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "entry" TO v-erel-fcust.
  END.
  ELSE DO:
     ASSIGN v-erel-fcust:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            v-erel-fcust:SCREEN-VALUE = "".
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY tg_notes v-f-cust v-t-cust tg_title v-erel-fcust v-status 
      WITH FRAME gDialog.
  ENABLE RECT-8 RECT-9 RECT-10 RECT-11 tg_notes tg_title Btn_OK Btn_Cancel 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trans-notes gDialog 
PROCEDURE trans-notes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF BUFFER bf-notes FOR notes.
 
 FOR EACH cust WHERE cust.company = g_company 
                 AND cust.cust-no >= v-f-cust 
                 AND cust.cust-no <= v-t-cust NO-LOCK,
     each eb WHERE eb.company = cust.company
                 AND eb.cust-no = cust.cust-no NO-LOCK,
     FIRST est WHERE est.company = cust.company
                 AND est.est-no = eb.est-no NO-LOCK:

         for each notes where notes.rec_key   eq cust.rec_key
                          and notes.note_type eq "D"
                          and notes.note_code ne "" no-lock:

             find first bf-notes where bf-notes.rec_key   eq est.rec_key
                                   and bf-notes.note_type eq notes.note_type
                                   and bf-notes.note_code eq notes.note_code 
                                   no-lock no-error.
             if not avail bf-notes then do:
                   create bf-notes.
                   buffer-copy notes except notes.note_form_no to bf-notes
                   ASSIGN bf-notes.rec_key   = est.rec_key
                          bf-notes.note_date = today
                          bf-notes.note_time = time.
                   /*DISP eb.est-no WITH DOWN.
                     DOWN.
                     PAUSE 0.                      */
                   v-status = " Updating  Notes  for " + cust.cust-no + ", " + bf-notes.note_code.
                   DISPLAY {&DISPLAYED-OBJECTS} WITH FRAME {&FRAME-NAME}.
             end.
         END.
 END.

 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trans-phone gDialog 
PROCEDURE trans-phone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-cust FOR cust.
  DEF BUFFER bf-phone FOR phone.
  DEF BUFFER bx-phone FOR phone.

  FIND FIRST bf-cust WHERE bf-cust.company = g_company
                       AND bf-cust.cust-no = v-erel-fcust NO-LOCK NO-ERROR.

  FOR EACH cust WHERE cust.company = g_company NO-LOCK:
      IF cust.cust-no <> v-erel-fcust THEN DO:
         DISPLAY {&DISPLAYED-OBJECTS} WITH FRAME {&FRAME-NAME}.
         FOR EACH phone WHERE phone.table_rec_key = bf-cust.rec_key 
                          AND phone.titlcode = "ERELEASE" NO-LOCK :
             FIND FIRST bf-phone WHERE bf-phone.TABLE_rec_key = cust.rec_key
                                   AND bf-phone.titlcode = phone.titlcode 
                                   AND bf-phone.attention = phone.attention NO-ERROR.
             IF NOT AVAIL bf-phone THEN DO:                
                FIND FIRST bx-phone WHERE bx-phone.TABLE_rec_key = cust.rec_key
                                      AND bx-phone.attention = phone.attention NO-ERROR.                
                CREATE bf-phone.
                BUFFER-COPY phone EXCEPT phone.rec_key phone.attention TO bf-phone.
                ASSIGN bf-phone.TABLE_rec_key = cust.rec_key
                       bf-phone.attention = IF AVAIL bx-phone THEN phone.titlcode + " " + phone.attention
                                            ELSE phone.attention.  
                v-status = " Updating  ERelease  for  " + cust.cust-no + ", " + bf-phone.attention .
                DISPLAY {&DISPLAYED-OBJECTS} WITH FRAME {&FRAME-NAME}.

             END.
         END.
         
      END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

