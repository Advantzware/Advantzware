&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/emplog2.w

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

{custom/gcompany.i}
{custom/emprate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emplogin
&Scoped-define FIRST-EXTERNAL-TABLE emplogin


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emplogin.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emplogin.start_date emplogin.end_date ~
emplogin.shift 
&Scoped-define ENABLED-TABLES emplogin
&Scoped-define FIRST-ENABLED-TABLE emplogin
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS emplogin.machine emplogin.start_date ~
emplogin.end_date emplogin.total_time emplogin.shift 
&Scoped-define DISPLAYED-TABLES emplogin
&Scoped-define FIRST-DISPLAYED-TABLE emplogin
&Scoped-Define DISPLAYED-OBJECTS mach_m-dscr start_hour start_minute ~
start_ampm end_hour end_minute end_ampm F1 F-3 F-4 F-2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,TIME-FIELDS,F1 */
&Scoped-define ADM-CREATE-FIELDS emplogin.machine 
&Scoped-define DISPLAY-FIELD emplogin.machine 
&Scoped-define TIME-FIELDS start_hour start_minute start_ampm end_hour ~
end_minute end_ampm 
&Scoped-define F1 F1 F-3 F-4 F-2 

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

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-4 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE mach_m-dscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

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

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44.2 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     emplogin.machine AT ROW 1.24 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr AT ROW 2.43 COL 14 COLON-ALIGNED NO-LABEL
     emplogin.start_date AT ROW 3.62 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     start_hour AT ROW 4.81 COL 14 COLON-ALIGNED HELP
          "Enter Starting Hour"
     start_minute AT ROW 4.81 COL 20 COLON-ALIGNED HELP
          "Enter Starting Minute"
     start_ampm AT ROW 4.81 COL 25 COLON-ALIGNED NO-LABEL
     emplogin.end_date AT ROW 6 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     end_hour AT ROW 7.19 COL 14 COLON-ALIGNED HELP
          "Enter Ending Hour"
     end_minute AT ROW 7.19 COL 20 COLON-ALIGNED HELP
          "Enter Ending Minute"
     end_ampm AT ROW 7.19 COL 25 COLON-ALIGNED NO-LABEL
     emplogin.total_time AT ROW 8.38 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     emplogin.shift AT ROW 8.38 COL 36 COLON-ALIGNED FORMAT "XX"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 15 FONT 4
     F1 AT ROW 1.24 COL 28 NO-LABEL
     F-3 AT ROW 3.62 COL 30 NO-LABEL
     F-4 AT ROW 6 COL 30 NO-LABEL
     F-2 AT ROW 8.38 COL 42 NO-LABEL
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: emplogin
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
         HEIGHT             = 8.57
         WIDTH              = 44.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX end_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN end_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN end_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-4:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN emplogin.machine IN FRAME F-Main
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN mach_m-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN emplogin.shift IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR COMBO-BOX start_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN start_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN start_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN emplogin.total_time IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME emplogin.end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emplogin.end_date V-table-Win
ON HELP OF emplogin.end_date IN FRAME F-Main /* End Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_hour V-table-Win
ON LEAVE OF end_hour IN FRAME F-Main /* End Time */
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {methods/entryerr.i &error-message="Invalid Hour, range = 0 to 12"}
   {&methods/lValidateError.iNO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_minute V-table-Win
ON LEAVE OF end_minute IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emplogin.machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emplogin.machine V-table-Win
ON LEAVE OF emplogin.machine IN FRAME F-Main /* Machine */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  {methods/entryerr.i
      &can-find="mach WHERE mach.company = gcompany
                        AND mach.m-code = SELF:SCREEN-VALUE"
      &error-message="Invalid Machine Code"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emplogin.shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emplogin.shift V-table-Win
ON LEAVE OF emplogin.shift IN FRAME F-Main /* Shift */
DO:
  {&methods/lValidateError.i YES}
  {methods/entryerr.i
      &can-find="FIRST shifts WHERE shifts.company = gcompany
                                AND shifts.shift = SELF:SCREEN-VALUE"
      &error-message="Invalid Shift"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME emplogin.start_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emplogin.start_date V-table-Win
ON HELP OF emplogin.start_date IN FRAME F-Main /* Start Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_hour V-table-Win
ON LEAVE OF start_hour IN FRAME F-Main /* Start Time */
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 1 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {methods/entryerr.i &error-message="Invalid Hour, range = 1 to 12"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_minute V-table-Win
ON LEAVE OF start_minute IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  {custom/getcmpny.i}

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
  {src/adm/template/row-list.i "emplogin"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emplogin"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-mach-emp-trans V-table-Win 
PROCEDURE crt-mach-emp-trans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-machtran for machtran.

  /* need to create machtran & machemp */

  /*for each  w/h machine does not have end_date */
  find first machtran where machtran.company = emplogin.company
                      and machtran.machine = emplogin.machine
                    /*  and machtran.start_date = emplogin.start_date
                      and machtran.end_date = emplogin.end_date  */
                /*      and machtran.start_time >= emplogin.start_time
                      and machtran.end_time <= emplogin.end_time
                */      
                      no-lock. 
      find employee where employee.company = emplogin.company
                      and employee.employee = emplogin.employee
                      no-lock no-error.                

/*      CREATE bf-machtran.
      ASSIGN
        bf-machtran.company = machtran.company
        bf-machtran.machine = machtran.machine
        bf-machtran.job_number = machtran.job_number
        bf-machtran.job_sub = INTEGER(machtran.job_sub)
        bf-machtran.form_number = INTEGER(machtran.form_number)
        bf-machtran.blank_number = INTEGER(machtran.blank_number)
        bf-machtran.pass_sequence = INTEGER(machtran.pass_sequence)
        bf-machtran.start_date = machtran.start_date
        bf-machtran.start_time = machtran.start_time
        bf-machtran.jobseq = machtran.jobseq
        bf-machtran.charge_code = machtran.charge_code
        bf-machtran.shift = machtran.shift
        /*bf-machtran-rowid = ROWID(bf-machtran) */
        .      
  */    
     find first machemp where machemp.table_rec_key = machtran.rec_key and
                              machemp.employee = emplogin.employee and
                              machemp.start_date = /*machtran.start_date*/
                                                   emplogin.start_date and
                              machemp.end_date = emplogin.end_date                      
                              no-error.
     if not avail machemp then   CREATE machemp.
     else do:
          message "There is employee transaction for " emplogin.start_date 
                  ".  Are you sure you want to update information?"
                  view-as alert-box question button yes-no update ll-ans as log.
          if not ll-ans then return no-apply.                
     end.
     ASSIGN machemp.table_rec_key = machtran.rec_key
            machemp.employee = emplogin.employee
            machemp.start_date = emplogin.start_date /*machtran.start_date*/
            machemp.start_time = emplogin.start_time
            machemp.end_date = emplogin.end_date
            machemp.end_time = emplogin.end_time
            machemp.shift = emplogin.shift
            machemp.ratetype = 'Standard'
            machemp.rate_usage = employee.rate_usage.
          RUN Employee-Rate(emplogin.company,employee.employee,machemp.shift,
                            machtran.machine,machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).

      {custom/calctime.i &file="machemp"}

      RELEASE machemp.
/*  END. */


/*      {methods/run_link.i "CONTAINER" "Set_MachTran_Rowid" "(machtran-rowid)"}

     FIND FIRST machemp WHERE machemp.table_rec_key = bf-machtran.rec_key
                           AND machemp.employee = emplogin.employee
                           AND machemp.end_time = 0
                           AND machemp.total_time = 0 EXCLUSIVE-LOCK NO-ERROR.
     IF AVAILABLE machemp THEN
     DO:
        machemp.end_date = TODAY.
        RUN Get-Shift(company_code,machinecode,stoptime,"END",OUTPUT shiftvar).
        IF shiftvar = machemp.shift THEN
        machemp.end_time = stoptime. /* no shift change, close out current */
        ELSE
        DO: /* shift change, close out current */
          FIND employee WHERE employee.company = company_code
                          AND employee.employee = employee_code NO-LOCK NO-ERROR.
          RUN Shift-Data(company_code,machinecode,machemp.shift,
                         OUTPUT starttime,OUTPUT endtime).
          machemp.end_time = endtime.
          {custom/calctime.i &file="machemp"}
          RUN Missing-Shift(company_code,machinecode,machemp.shift,shiftvar,
                            OUTPUT missingshift).
          IF missingshift NE '' THEN /* skipped a shift */
          DO: /* create record for skipped shift */
            RUN Shift-Data(company_code,machinecode,missingshift,
                           OUTPUT starttime,OUTPUT endtime).
            CREATE machemp.
            ASSIGN
              machemp.table_rec_key = machtran.rec_key
              machemp.employee = employee_code
              machemp.start_date = TODAY
              machemp.start_time = starttime
              machemp.end_date = TODAY
              machemp.end_time = endtime
              machemp.shift = missingshift
              machemp.ratetype = 'Standard'
              machemp.rate_usage = employee.rate_usage.
            RUN Employee-Rate(company_code,employee_code,machemp.shift,machinecode,
                              machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
            {custom/calctime.i &file="machemp"}
          END.
          /* create record for current shift */
          RUN Shift-Data (company_code,machine_code,shiftvar,
                          OUTPUT starttime,OUTPUT endtime).
          CREATE machemp.
          ASSIGN
            machemp.table_rec_key = machtran.rec_key
            machemp.employee = employee_code
            machemp.start_date = TODAY
            machemp.start_time = starttime
            machemp.end_date = TODAY
            machemp.end_time = stoptime
            machemp.shift = shiftvar
            machemp.ratetype = 'Standard'
            machemp.rate_usage = employee.rate_usage.
          RUN Employee-Rate(company_code,employee_code,machemp.shift,machinecode,
                            machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
        END.
        {custom/calctime.i &file="machemp"}
    END. /* avail machemp */
  END. /* avail machtran */
  =======*/


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
  {methods/viewers/assign/emplogin.i}
  run crt-mach-emp-trans.  /* create all machine & employee transaction */
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
  DISABLE {&TIME-FIELDS} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/emplogin.i}

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
  {src/adm/template/snd-list.i "emplogin"}

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

