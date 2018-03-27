&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEF INPUT PARAMETER ip-m-code AS cha NO-UNDO.
/*DEF INPUT PARAM ip-job LIKE job.job.
DEF INPUT PARAM ip-job-no LIKE job.job-no.
DEF INPUT PARAM ip-job-no2 LIKE job.job-no2.
DEF INPUT PARAM ip-frm LIKE job-mch.frm.
DEF INPUT PARAM ip-line LIKE job-mch.line.  */
DEF INPUT-OUTPUT PARAM iop-date-wkst AS DATE NO-UNDO.
DEF INPUT-OUTPUT PARAM iop-date-wkend AS DATE NO-UNDO.
DEF OUTPUT PARAM op-time-wkst AS INT NO-UNDO.
DEF OUTPUT PARAM op-time-wkend AS INT NO-UNDO.
DEF OUTPUT PARAM op-mcode AS cha NO-UNDO.
DEF OUTPUT PARAM op-due-sdate AS DATE NO-UNDO.
DEF OUTPUT PARAM op-due-edate AS DATE NO-UNDO.
DEF OUTPUT PARAM op-sch-date AS DATE NO-UNDO.

{custom/globdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-mach v-st-date v-end-date v-due-sdate ~
v-due-edate v-st-hr v-st-mn v-st-ampm v-end-hr v-end-mn v-end-ampm ~
v-sch-date Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS v-mach v-st-date v-end-date v-due-sdate ~
v-due-edate v-st-hr v-st-mn v-st-ampm v-end-hr v-end-mn v-end-ampm ~
v-sch-date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 v-mach v-st-date v-end-date v-due-sdate v-due-edate ~
v-st-hr v-st-mn v-st-ampm v-end-hr v-end-mn v-end-ampm v-sch-date 

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

DEFINE VARIABLE v-end-ampm AS CHARACTER FORMAT "X(256)":U INITIAL "AM" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "AM","PM" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE v-st-ampm AS CHARACTER FORMAT "X(256)":U INITIAL "AM" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "AM","PM" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE v-due-edate AS DATE FORMAT "99/99/9999":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-due-sdate AS DATE FORMAT "99/99/9999":U 
     LABEL "Due Date From" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-end-date AS DATE FORMAT "99/99/9999":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-end-hr AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "End" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-end-mn AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-mach AS CHARACTER FORMAT "X(256)":U 
     LABEL "Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE v-sch-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Schedule Start Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-st-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date From" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-st-hr AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Work Start Time" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-st-mn AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     v-mach AT ROW 1.71 COL 21 COLON-ALIGNED
     v-st-date AT ROW 3.14 COL 21 COLON-ALIGNED
     v-end-date AT ROW 3.14 COL 50 COLON-ALIGNED
     v-due-sdate AT ROW 4.57 COL 21 COLON-ALIGNED
     v-due-edate AT ROW 4.57 COL 50 COLON-ALIGNED
     v-st-hr AT ROW 6.71 COL 21 COLON-ALIGNED
     v-st-mn AT ROW 6.71 COL 28 COLON-ALIGNED
     v-st-ampm AT ROW 6.71 COL 33 COLON-ALIGNED NO-LABEL
     v-end-hr AT ROW 6.71 COL 54 COLON-ALIGNED
     v-end-mn AT ROW 6.71 COL 61 COLON-ALIGNED
     v-end-ampm AT ROW 6.71 COL 67 COLON-ALIGNED NO-LABEL
     v-sch-date AT ROW 8.62 COL 33 COLON-ALIGNED
     Btn_OK AT ROW 14.33 COL 14
     Btn_Cancel AT ROW 14.33 COL 52
     "This will sort the jobs by Job Start Date for the Date Range Selected." VIEW-AS TEXT
          SIZE 81 BY 2.14 AT ROW 10.29 COL 3
          BGCOLOR 14 FONT 6
     SPACE(0.59) SKIP(3.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Job Scheduling for Machine"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-due-edate IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN v-due-sdate IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR COMBO-BOX v-end-ampm IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN v-end-date IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN v-end-hr IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN v-end-mn IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN v-mach IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN v-sch-date IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR COMBO-BOX v-st-ampm IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN v-st-date IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN v-st-hr IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN v-st-mn IN FRAME D-Dialog
   4                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Job Scheduling for Machine */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
   ASSIGN op-time-wkst = 0
          op-time-wkend = 0
          op-mcode = ip-m-code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
    DEF BUFFER bx-mch FOR job-mch.

    ASSIGN v-st-hr v-st-mn v-st-ampm
           v-end-hr v-end-mn v-end-ampm
           v-st-date v-end-date v-due-sdate v-due-edate v-sch-date.
    

   /* validation */
    IF v-end-date < v-st-date THEN DO:
       MESSAGE "Invalid Start Date!" VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-end-date.
       RETURN NO-APPLY.
    END.
    IF v-due-edate < v-due-sdate THEN DO:
       MESSAGE "Invalid Due Date!" VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-due-edate.
       RETURN NO-APPLY.
    END.
    IF v-sch-date = ? THEN DO:
       MESSAGE "Invalid Schedule Date!" VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-sch-date.
       RETURN NO-APPLY.
    END.
    IF int(v-st-hr:SCREEN-VALUE) > 12 OR int(v-st-hr:SCREEN-VALUE) < 0 THEN DO:
       MESSAGE "Invalid Time Value. " VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-st-hr.
       RETURN NO-APPLY.
    END.
    IF int(v-st-mn:SCREEN-VALUE) > 59 OR int(v-st-mn:SCREEN-VALUE) < 0 THEN DO:
       MESSAGE "Invalid Minute Values. " VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-st-mn.
       RETURN NO-APPLY.
    END.
    IF int(v-end-hr:SCREEN-VALUE) > 12 OR int(v-end-hr:SCREEN-VALUE) < 0 THEN DO:
       MESSAGE "Invalid Time Value. " VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-end-hr.
       RETURN NO-APPLY.
    END.    
    IF int(v-end-mn:SCREEN-VALUE) > 59 OR int(v-end-mn:SCREEN-VALUE) < 0 THEN DO:
       MESSAGE "Invalid Minute Values. " VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-end-mn.
       RETURN NO-APPLY.
    END.

    ASSIGN {&list-4}.
    IF v-st-hr = v-end-hr AND v-st-mn = v-end-mn AND v-st-ampm = v-end-ampm THEN DO:
       MESSAGE "Work End Time can not be same as Start Time." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-end-ampm.
       RETURN NO-APPLY.
    END.

   IF v-st-ampm = "PM" AND v-st-hr <> 12 THEN v-st-hr = v-st-hr + 12.
   IF v-st-ampm = "AM" AND v-st-hr = 12 THEN v-st-hr = 0.
   op-time-wkst = v-st-hr * 3600 + v-st-mn * 60.

   IF v-end-ampm = "PM" AND v-end-hr <> 12 THEN v-end-hr = v-end-hr + 12.
   IF v-end-ampm = "AM" AND v-end-hr = 12 THEN v-end-hr = 0.
   op-time-wkend = v-end-hr * 3600 + v-end-mn * 60.

/*==============
   /* validate any prior machine start later than schedule start date*/
   FIND FIRST bx-mch WHERE bx-mch.company = g_company
                        AND bx-mch.job = ip-job
                        AND bx-mch.job-no = ip-job-no
                        AND bx-mch.job-no2 = ip-job-no2
                        AND bx-mch.frm = ip-frm
                        AND bx-mch.LINE < ip-line
                        AND bx-mch.m-code <> v-mach
                        AND (bx-mch.start-date > v-sch-date OR
                             (bx-mch.start-date = v-sch-date AND
                             bx-mch.end-time > op-time-wkst))
                   /*      AND bx-mch.blank-no >= bf-mch.blank-no
                     AND bx-mch.pass >= bf-mch.pass
                     AND NOT bf-mch.anchored
                     AND RECID(bx-mch) <> RECID(bf-mch)
                     BREAK BY bx-mch.frm BY bx-mch.blank-no 
                     by bx-mch.pass :*/
                     USE-INDEX line-idx NO-LOCK NO-ERROR.
   IF AVAIL bx-mch THEN DO:
      op-time-wkst = bx-mch.end-time. 
      op-time-wkend = 86400.
      MESSAGE "There is Prior Machine ending later date/time - " bx-mch.start-date " , "
              STRING(bx-mch.end-time,"hh:mm")  "." SKIP
             "Schedule Start Date should be later than " bx-mch.start-date VIEW-AS ALERT-BOX.

      v-sch-date = bx-mch.start-date.
      DISPLAY v-sch-date WITH FRAME {&FRAME-NAME}.
   END.

   

   ASSIGN v-st-hr = TRUNCATE(mach-calendar.START / 3600,0)        
          v-st-mn = (mach-calendar.START - (v-st-hr * 3600)) / 60
            .
   IF v-st-hr > 12 THEN ASSIGN v-st-ampm = "PM"
                                 v-st-hr = v-st-hr - 12.
   ELSE v-st-ampm = "AM".
   ASSIGN v-end-hr = TRUNCATE(mach-calendar.end-time / 3600,0)        
          v-end-mn = (mach-calendar.end-time - (v-end-hr * 3600) ) / 60
            .
     IF v-end-hr > 12 THEN ASSIGN v-end-ampm = "PM"
                                  v-end-hr = v-end-hr - 12.
     ELSE v-end-ampm = "AM".
=====================*/
   FIND FIRST mach-calendar WHERE mach-calendar.company = g_company
                        AND mach-calendar.m-code = v-mach  
                        AND mach-calendar.m-date = v-sch-date NO-ERROR.
   IF NOT AVAIL mach-calendar THEN CREATE mach-calendar.
   ASSIGN mach-calendar.company = g_company
          mach-calendar.m-code = v-mach
          mach-calendar.START-time = op-time-wkst
          mach-calendar.end-time = op-time-wkend
          mach-calendar.m-date = v-sch-date.

    ASSIGN op-mcode = v-mach
           iop-date-wkst = v-st-date
           iop-date-wkend = v-end-date
           op-due-sdate = v-due-sdate
           op-due-edate = v-due-edate
           op-sch-date = v-sch-date.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-due-sdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-due-sdate D-Dialog
ON LEAVE OF v-due-sdate IN FRAME D-Dialog /* Due Date From */
DO:
  v-sch-date:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-end-hr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-end-hr D-Dialog
ON LEAVE OF v-end-hr IN FRAME D-Dialog /* End */
DO:
    IF int(SELF:SCREEN-VALUE) > 12 OR int(SELF:SCREEN-VALUE) < 0  THEN DO:
       MESSAGE "Invalid Time Value. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-end-mn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-end-mn D-Dialog
ON LEAVE OF v-end-mn IN FRAME D-Dialog
DO:
    IF int(SELF:SCREEN-VALUE) > 59 OR int(SELF:SCREEN-VALUE) < 0
    THEN DO:
       MESSAGE "Invalid Minute Value. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-st-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-st-date D-Dialog
ON LEAVE OF v-st-date IN FRAME D-Dialog /* Start Date From */
DO:
    v-sch-date:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-st-hr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-st-hr D-Dialog
ON LEAVE OF v-st-hr IN FRAME D-Dialog /* Work Start Time */
DO:
    
    IF int(SELF:SCREEN-VALUE) > 12 OR int(SELF:SCREEN-VALUE) < 0  THEN DO:
       MESSAGE "Invalid Time Value. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-st-mn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-st-mn D-Dialog
ON LEAVE OF v-st-mn IN FRAME D-Dialog
DO:
    IF int(SELF:SCREEN-VALUE) > 59 OR int(SELF:SCREEN-VALUE) < 0 
    THEN DO:
       MESSAGE "Invalid Minute Values. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

ASSIGN v-mach = ip-m-code
       v-st-date = iop-date-wkst
       v-end-date = iop-date-wkend.
       
{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY v-mach v-st-date v-end-date v-due-sdate v-due-edate v-st-hr v-st-mn 
          v-st-ampm v-end-hr v-end-mn v-end-ampm v-sch-date 
      WITH FRAME D-Dialog.
  ENABLE v-mach v-st-date v-end-date v-due-sdate v-due-edate v-st-hr v-st-mn 
         v-st-ampm v-end-hr v-end-mn v-end-ampm v-sch-date Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND FIRST mach-calendar WHERE mach-calendar.company = g_company
                       AND mach-calendar.m-code = v-mach  NO-LOCK NO-ERROR.
  IF AVAIL mach-calendar THEN DO:
     ASSIGN v-st-hr = TRUNCATE(mach-calendar.START / 3600,0)        
            v-st-mn = (mach-calendar.START - (v-st-hr * 3600)) / 60
            .
     IF v-st-hr > 12 THEN ASSIGN v-st-ampm = "PM"
                                 v-st-hr = v-st-hr - 12.
     ELSE v-st-ampm = "AM".
     ASSIGN v-end-hr = TRUNCATE(mach-calendar.end-time / 3600,0)        
            v-end-mn = (mach-calendar.end-time - (v-end-hr * 3600) ) / 60
            .
     IF v-end-hr > 12 THEN ASSIGN v-end-ampm = "PM"
                                  v-end-hr = v-end-hr - 12.
     ELSE v-end-ampm = "AM".
  END.
  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

