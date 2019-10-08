&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
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
{custom/globdefs.i}

DEF VAR correct-error AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES shift_break shifts
&Scoped-define FIRST-EXTERNAL-TABLE shift_break


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR shift_break, shifts.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS shift_break.charge_code ~
shift_break.description 
&Scoped-define ENABLED-TABLES shift_break
&Scoped-define FIRST-ENABLED-TABLE shift_break
&Scoped-Define ENABLED-OBJECTS RECT-3 
&Scoped-Define DISPLAYED-FIELDS shift_break.charge_code ~
shift_break.description 
&Scoped-define DISPLAYED-TABLES shift_break
&Scoped-define FIRST-DISPLAYED-TABLE shift_break
&Scoped-Define DISPLAYED-OBJECTS start_hour start_minute start_second ~
start_ampm end_hour end_minute end_second end_ampm 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define List-5 start_hour start_minute start_second start_ampm ~
end_hour end_minute end_second end_ampm 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE end_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "End Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_second AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "Start Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_second AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 5.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     shift_break.charge_code AT ROW 1.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     shift_break.description AT ROW 2.19 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     start_hour AT ROW 3.62 COL 19 COLON-ALIGNED HELP
          "Enter Starting Hour"
     start_minute AT ROW 3.62 COL 25 COLON-ALIGNED HELP
          "Enter Starting Minute"
     start_second AT ROW 3.62 COL 31 COLON-ALIGNED HELP
          "Enter Starting Second"
     start_ampm AT ROW 3.62 COL 35.6 COLON-ALIGNED NO-LABEL
     end_hour AT ROW 5.05 COL 19 COLON-ALIGNED HELP
          "Enter Ending Hour"
     end_minute AT ROW 5.05 COL 25 COLON-ALIGNED HELP
          "Enter Ending Minute"
     end_second AT ROW 5.05 COL 31 COLON-ALIGNED HELP
          "Enter Ending Second"
     end_ampm AT ROW 5.05 COL 35.6 COLON-ALIGNED NO-LABEL
     RECT-3 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: shift_break,shifts
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 9.67
         WIDTH              = 85.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX end_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN end_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN end_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN end_second IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR COMBO-BOX start_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN start_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN start_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN start_second IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME shift_break.charge_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shift_break.charge_code V-table-Win
ON HELP OF shift_break.charge_code IN FRAME F-Main /* Charge Code */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    RUN addon/touch/l-jobcod.w (trim(focus:screen-value),OUTPUT char-val).
    IF char-val <> "" THEN
        ASSIGN shift_break.charge_code:SCREEN-VALUE = ENTRY(1,char-val)
               shift_break.DESCRIPTION:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shift_break.charge_code V-table-Win
ON LEAVE OF shift_break.charge_code IN FRAME F-Main /* Charge Code */
DO:
   IF LASTKEY = -1 THEN return.

   RUN valid-chg-code.
   {&methods/lValidateError.i YES}
   IF RETURN-VALUE <> "" THEN DO:
      MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_hour V-table-Win
ON LEAVE OF end_hour IN FRAME F-Main /* End Time */
DO:
     IF LASTKEY = -1 THEN RETURN.
  RUN valid-hour(SELF:SCREEN-VALUE).
  {&methods/lValidateError.i YES}
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_minute V-table-Win
ON LEAVE OF end_minute IN FRAME F-Main
DO:
    IF LASTKEY = -1 THEN RETURN.
  RUN valid-min(SELF:SCREEN-VALUE).
  {&methods/lValidateError.i YES}
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_second
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_second V-table-Win
ON LEAVE OF end_second IN FRAME F-Main
DO:
  IF LASTKEY = -1 THEN RETURN.

  RUN valid-sec(SELF:SCREEN-VALUE).
  {&methods/lValidateError.i YES}
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_hour V-table-Win
ON LEAVE OF start_hour IN FRAME F-Main /* Start Time */
DO:
  IF LASTKEY = -1 THEN RETURN.
  RUN valid-hour(SELF:SCREEN-VALUE).
  {&methods/lValidateError.i YES}
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_minute V-table-Win
ON LEAVE OF start_minute IN FRAME F-Main
DO:
    IF LASTKEY = -1 THEN RETURN.

  RUN valid-min(SELF:SCREEN-VALUE).
  {&methods/lValidateError.i YES}
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_second
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_second V-table-Win
ON LEAVE OF start_second IN FRAME F-Main
DO:
  IF LASTKEY = -1 THEN RETURN.

  RUN valid-sec(SELF:SCREEN-VALUE).
  {&methods/lValidateError.i YES}
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "shift_break"}
  {src/adm/template/row-list.i "shifts"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "shift_break"}
  {src/adm/template/row-find.i "shifts"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*
  {custom/SET_time.i &ampm = 
  IF {&ampm} = 'PM' AND {&hour} NE '12' THEN
  {&hour} = STRING(INTEGER({&hour}) + 12,'99').
  IF {&ampm} = 'AM' AND {&hour} = '12' THEN
  {&hour} = '00'.
  {&field} = INTEGER({&hour}) * 3600 + INTEGER({&minute}) * 60.
  */

  IF START_ampm = "PM" AND START_hour <> "12" THEN
           START_hour = string(int(START_hour) + 12).
  IF START_ampm = "AM" AND START_hour = "12" THEN START_hour = "0".

  shift_break.start_time = INT(START_hour) * 3600 + INT(START_minute) * 60
                         + INT(START_second).

  IF end_ampm = "PM" AND end_hour <> "12" THEN
           end_hour = string(int(end_hour) + 12).
  IF end_ampm = "AM" AND end_hour = "12" THEN end_hour = "0".

  shift_break.end_time = INT(end_hour) * 3600 + INT(end_minute) * 60
                       + INT(end_second).

  RUN local-display-fields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN FRAME {&FRAME-NAME} {&list-5}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-brk FOR shift_break.
  DEF VAR lv-seq AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-seq = 0.
  FOR EACH bf-brk WHERE bf-brk.company = shifts.company
                    AND bf-brk.shift = shifts.shift BY bf-brk.seq DESC.
      lv-seq = bf-brk.seq.
      LEAVE.
  END.

  ASSIGN shift_break.company = shifts.company
         shift_break.shift = shifts.shift
         shift_break.seq = lv-seq + 1.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/askdel.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

   ASSIGN START_hour:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00"
       START_minute:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00"
       START_second:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00"
       START_ampm:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
       end_hour:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00"
       end_minute:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00"
       end_second:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00"
       end_ampm:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL shift_break THEN
     ASSIGN START_hour:SCREEN-VALUE IN FRAME {&FRAME-NAME}
               = SUBSTRING(STRING(shift_break.START_time,"hh:mm am"),1,2)
         START_minute:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(STRING(shift_break.START_time,"hh:mm am"),4,2)
         START_ampm:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(STRING(shift_break.START_time,"hh:mm am"),7,2)
         START_second:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(STRING(shift_break.START_time,"hh:mm:ss am"),7,2)
         end_hour:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(STRING(shift_break.end_time,"hh:mm am"),1,2)
         end_minute:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(STRING(shift_break.end_time,"hh:mm am"),4,2)
         end_second:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(STRING(shift_break.end_time,"hh:mm:ss am"),7,2)
         end_ampm:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(STRING(shift_break.end_time,"hh:mm am"),7,2).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN valid-hour(START_hour:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  {&methods/lValidateError.i YES}
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO START_hour.
     RETURN .
  END.
  RUN valid-min(START_minute:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO START_minute.
     RETURN .
  END.
  RUN valid-sec(START_second:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO START_second.
     RETURN .
  END.
  RUN valid-hour(end_hour:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO end_hour.
     RETURN .
  END.
  RUN valid-min(end_minute:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO end_minute.
     RETURN .
  END.
  RUN valid-sec(end_second:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO end_second.
     RETURN .
  END.

  RUN valid-chg-code.
  IF RETURN-VALUE <> "" THEN DO:
      MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
   DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "shift_break"}
  {src/adm/template/snd-list.i "shifts"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-chg-code V-table-Win 
PROCEDURE valid-chg-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
   IF NOT CAN-FIND(FIRST job-code 
             WHERE job-code.CODE = shift_break.charge_code:SCREEN-VALUE IN FRAME {&FRAME-NAME} )
   THEN RETURN "Invalie Charge Code!.".
   ELSE RETURN "".


  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-hour V-table-Win 
PROCEDURE valid-hour :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ip-hour AS CHAR.

  {methods/lValidateError.i YES}
 correct-error = INTEGER(ip-hour) LT 0 OR INTEGER(ip-hour) GT 12.
 IF correct-error THEN
     RETURN "Invalid Hour. Must be between 0 and 12... ".
 ELSE RETURN "". 

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-min V-table-Win 
PROCEDURE valid-min :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAM ip-min AS cha NO-UNDO.

  {methods/lValidateError.i YES}
 correct-error = INTEGER(ip-min) LT 0 OR INTEGER(ip-min) GT 59.
 IF correct-error THEN
     RETURN "Invalid Hour. Must be between 0 and 59... ".
 ELSE RETURN "".

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sec V-table-Win 
PROCEDURE valid-sec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-sec AS cha NO-UNDO.

  {methods/lValidateError.i YES}
   correct-error = INTEGER(ip-sec) LT 0 OR INTEGER(ip-sec) GT 59.
   IF correct-error THEN
      RETURN "Invalid Second. Must be between 0 and 59... ".
   ELSE RETURN "".
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

