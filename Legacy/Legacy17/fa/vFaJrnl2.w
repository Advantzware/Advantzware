&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          ptdb1            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"fa/sdofajrnl.i"}.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
&Scoped-DEF FRAME-NAME F-Main
{pt/probase1.i}
{dev/method/mxpbrokr.i}

DEF VAR add-new AS LOG NO-UNDO.
/* CURRENT-WINDOW:TITLE = "mach-burden Rate Maintenance". */


{pt/setinit use-file 'sales-jrnl'}
DEF VAR do-copy AS LOG.
DEF SHARED VAR pgm       AS   CHAR FORMAT "x(13)".
DEF NEW SHARED BUFFER    wznumgen FOR  z_numgen.


FIND  login WHERE 
    terminal-no = terminalid AND
    user-id = ENTRY(1,USERID("ptdb":U),"@") AND 
     login.system = "fa":U NO-LOCK.
FIND  entity    OF  login NO-LOCK.
FIND fa-control     OF  entity NO-LOCK.
FIND gl-control     OF  entity NO-LOCK.
ASSIGN v-fa-entity  = fa-control.fa-entity.
FIND  z_user WHERE 
     z_user.user-id = ENTRY(1,USERID("ptdb":U),"@") NO-LOCK.

DEF VAR tcur-rate    LIKE exchange-d.conv-rate.
DEF BUFFER b-fa-jrnl FOR fa-jrnl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "fa/sdofajrnl.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Entry-no RowObject.Asset-code ~
RowObject.Trans-date RowObject.Prd RowObject.Yr RowObject.Debit-amt ~
RowObject.Credit-amt RowObject.Explanation RowObject.Entity-code ~
RowObject.Job-no 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS method-combo 
&Scoped-Define DISPLAYED-FIELDS RowObject.Entry-no RowObject.Line-no ~
RowObject.Asset-code RowObject.fa-entity RowObject.Trans-date ~
RowObject.Gl-code RowObject.rev RowObject.Prd RowObject.method RowObject.Yr ~
RowObject.Debit-amt RowObject.Credit-amt RowObject.Explanation ~
RowObject.Entity-code RowObject.Job-no RowObject.Currency-cod ~
RowObject.hm-debit-amt RowObject.hm-credit-amt 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS a-desc method-combo e-desc j-desc c-desc 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE method-combo AS CHARACTER FORMAT "X(256)":U INITIAL "Book" 
     LABEL "Method" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Book","Tax1","Tax2" 
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE a-desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE c-desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE e-desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE j-desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Entry-no AT ROW 1 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.57 BY 1
     RowObject.Line-no AT ROW 1.25 COL 89 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY 1
     RowObject.Asset-code AT ROW 2 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     a-desc AT ROW 2 COL 37 COLON-ALIGNED NO-LABEL
     RowObject.fa-entity AT ROW 2.33 COL 89 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     RowObject.Trans-date AT ROW 3 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY 1
     RowObject.Gl-code AT ROW 3.38 COL 89 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     method-combo AT ROW 4 COL 22 COLON-ALIGNED
     RowObject.rev AT ROW 4.42 COL 89 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     RowObject.Prd AT ROW 5.21 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.43 BY 1
     RowObject.method AT ROW 5.46 COL 89 COLON-ALIGNED
          LABEL "Method"
          VIEW-AS FILL-IN 
          SIZE 9.14 BY 1
     RowObject.Yr AT ROW 6.21 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY 1
     RowObject.Debit-amt AT ROW 7.21 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Credit-amt AT ROW 7.21 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Explanation AT ROW 8.21 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     RowObject.Entity-code AT ROW 9.21 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     e-desc AT ROW 9.21 COL 42 COLON-ALIGNED NO-LABEL
     RowObject.Job-no AT ROW 10.21 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     j-desc AT ROW 10.21 COL 37 COLON-ALIGNED NO-LABEL
     RowObject.Currency-cod AT ROW 11.21 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     c-desc AT ROW 11.21 COL 32 COLON-ALIGNED NO-LABEL
     RowObject.hm-debit-amt AT ROW 12.33 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.hm-credit-amt AT ROW 12.33 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "fa/sdofajrnl.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {fa/sdofajrnl.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 15.46
         WIDTH              = 109.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN a-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Currency-cod IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN e-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.fa-entity IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fa-entity:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Gl-code IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Gl-code:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.hm-credit-amt IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.hm-credit-amt:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.hm-debit-amt IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.hm-debit-amt:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN j-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Line-no IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Line-no:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.method IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.method:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.rev IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.rev:HIDDEN IN FRAME F-Main           = TRUE.

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

&Scoped-define SELF-NAME RowObject.Asset-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Asset-code vTableWin
ON ENTRY OF RowObject.Asset-code IN FRAME F-Main /* Asset Code */
DO:
    {pt/setinit br-file 'fa-mast'}. 
    {pt/setinit br-field 'asset-code'}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Asset-code vTableWin
ON LEAVE OF RowObject.Asset-code IN FRAME F-Main /* Asset Code */
DO:
    IF LASTKEY <> -1 AND LASTKEY <> 306 THEN DO:
        FIND fa-mast WHERE 
            fa-mast.fa-entity = v-fa-entity AND
            fa-mast.asset-code = rowobject.asset-code:{&SV} 
            NO-LOCK NO-ERROR.
        IF NOT AVAIL fa-mast THEN DO:
            MESSAGE "Asset Code does not exist"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO rowobject.asset-code.
            RETURN NO-APPLY.
        END.
        ASSIGN 
            a-desc:{&SV} = fa-mast.asset-desc
            rowobject.entity-code:{&SV} = fa-mast.entity-code.
        FIND entity WHERE
            entity.entity-code = rowobject.entity-code:{&SV} NO-LOCK NO-ERROR.
        IF AVAIL entity THEN
            ASSIGN e-desc:{&SV} = entity.NAME.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Credit-amt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Credit-amt vTableWin
ON LEAVE OF RowObject.Credit-amt IN FRAME F-Main /* Credit Amt */
DO:
    ASSIGN rowobject.hm-credit-amt:{&SV} = 
        STRING(DEC(rowobject.credit-amt:{&SV}) / fa-mast.exch-rate).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Debit-amt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Debit-amt vTableWin
ON LEAVE OF RowObject.Debit-amt IN FRAME F-Main /* Debit Amt */
DO:
    ASSIGN rowobject.hm-debit-amt:{&SV} = 
        STRING(DEC(rowobject.debit-amt:{&SV}) / fa-mast.exch-rate).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Entity-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Entity-code vTableWin
ON ENTRY OF RowObject.Entity-code IN FRAME F-Main /* Ent Code */
DO:
    {pt/setinit br-file 'entity'}. 
    {pt/setinit br-field 'entity-code'}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Entity-code vTableWin
ON LEAVE OF RowObject.Entity-code IN FRAME F-Main /* Ent Code */
DO:
    IF LASTKEY <> -1 AND LASTKEY <> 306 THEN DO:
        FIND entity WHERE
            entity.entity-code = rowobject.entity-code:{&SV} NO-LOCK NO-ERROR.
        IF NOT AVAIL entity THEN DO:
            MESSAGE "Entity not on file"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO rowobject.entity-code.
            RETURN NO-APPLY.
        END.
        ASSIGN e-desc:{&SV} = entity.NAME.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Entry-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Entry-no vTableWin
ON LEAVE OF RowObject.Entry-no IN FRAME F-Main /* Entry No */
DO:
    IF LASTKEY <> -1 THEN DO:
        IF INT(rowobject.entry-no:{&SV}) = 0 THEN DO:
            MESSAGE "Entry number may not be zero"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO rowobject.entry-no.
            RETURN NO-APPLY.
        END.
        FIND FIRST b-fa-jrnl WHERE
            b-fa-jrnl.fa-entity = v-fa-entity AND 
            b-fa-jrnl.entry-no = INT(rowobject.entry-no:{&SV}) NO-LOCK NO-ERROR.
        IF AVAIL b-fa-jrnl THEN DO:
            MESSAGE "Entry is already on file"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO rowobject.entry-no.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Job-no vTableWin
ON ENTRY OF RowObject.Job-no IN FRAME F-Main /* Job No */
DO:
    {pt/setinit br-file 'job'}. 
    {pt/setinit br-field 'job-no'}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Job-no vTableWin
ON LEAVE OF RowObject.Job-no IN FRAME F-Main /* Job No */
DO:
    IF LASTKEY <> -1 AND LASTKEY <> 306 AND rowobject.job-no:{&SV} <> "" THEN DO:
        FIND job WHERE
            job.job-no = rowobject.job-no:{&SV} NO-LOCK NO-ERROR.
        IF NOT AVAIL job THEN DO:
            MESSAGE "Job not found"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO rowobject.job-no.
            RETURN NO-APPLY.
        END.
        ASSIGN j-desc:{&SV} = job.DESCRIPTION.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Prd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Prd vTableWin
ON LEAVE OF RowObject.Prd IN FRAME F-Main /* Prd */
DO:
    IF INT(rowobject.prd:{&SV}) < 1 OR 
        INT(rowobject.prd:{&SV}) > fa-control.number-prd THEN DO:
        MESSAGE "ERROR: The period is not in the proper range"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO rowobject.prd.
        RETURN NO-APPLY.
    END.
                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Yr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Yr vTableWin
ON LEAVE OF RowObject.Yr IN FRAME F-Main /* Yr */
DO:
    IF INT(rowobject.yr:{&SV}) = 0 OR 
        (INT(rowobject.yr:{&SV}) <> fa-control.yr AND
                INT(rowobject.yr:{&SV}) <> fa-control.yr - 1) THEN DO:  
        MESSAGE "ERROR: The year must be equal to or 1 less than the F/A current year"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO rowobject.yr.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ENABLE
      rowobject.entry-no
      WITH FRAME {&FRAME-NAME}.

  IF fa-control.currency-cod <> gl-control.home-curr THEN
      ENABLE 
      rowobject.hm-debit-amt
      rowobject.hm-credit-amt
      WITH FRAME {&FRAME-NAME}.

  RUN SUPER.

  FIND LAST b-fa-jrnl NO-LOCK NO-ERROR.
  IF AVAIL b-fa-jrnl THEN 
      rowobject.entry-no:{&SV} = STRING(b-fa-jrnl.entry-no + 1).
  ELSE
      rowobject.entry-no:{&SV} = "1".
      FIND currency WHERE
      currency.currency-cod = fa-control.currency-cod NO-LOCK.

  ASSIGN 
      rowobject.fa-entity:{&SV}    = v-fa-entity
      rowobject.currency-cod:{&SV} = fa-control.currency-cod
      c-desc:{&SV}                 = currency.DESCRIPTION
      rowobject.trans-date:{&SV}   = STRING( login.current-date)
      rowobject.yr:{&SV}           = STRING(fa-control.yr)
      rowobject.prd:{&SV}          = STRING(fa-control.prd)
      method-combo:{&SV}           = "book":U
      add-new                      = YES
      a-desc:{&SV}                 = ""
      e-desc:{&SV}                 = ""
      j-desc:{&SV}                 = "".
 

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayRecord vTableWin 
PROCEDURE displayRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  FIND FIRST fa-jrnl WHERE
      fa-jrnl.fa-entity = rowobject.fa-entity:{&SV} AND 
      fa-jrnl.entry-no = INT(rowobject.entry-no:{&SV}) NO-LOCK NO-ERROR.
  ASSIGN method-combo:{&SV} = rowobject.method:{&SV}.

  FIND fa-mast WHERE 
      fa-mast.fa-entity = v-fa-entity AND
      fa-mast.asset-code = rowobject.asset-code:{&SV} 
      NO-LOCK NO-ERROR.
  IF AVAIL fa-mast THEN 
      ASSIGN a-desc:{&SV} = fa-mast.asset-desc.
  FIND entity WHERE
      entity.entity-code = rowobject.entity-code:{&SV} NO-LOCK NO-ERROR.
  IF AVAIL entity THEN 
      ASSIGN e-desc:{&SV} = entity.NAME.
  FIND currency WHERE
      currency.currency-cod = rowobject.currency-cod:{&SV} NO-LOCK NO-ERROR.
  FIND job WHERE
      job.job-no = rowobject.job-no:{&SV} NO-LOCK NO-ERROR.
  IF AVAIL job THEN
      ASSIGN j-desc:{&SV} = job.DESCRIPTION.
  IF AVAIL currency THEN
      ASSIGN c-desc:{&SV}= currency.DESCRIPTION.

  DISABLE
      rowobject.entry-no
      WITH FRAME {&FRAME-NAME}.
  add-new = NO.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  DISABLE
      rowobject.entry-no
      WITH FRAME {&FRAME-NAME}.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF DEC(rowobject.credit-amt:{&SV}) <> 0 AND
      DEC(rowobject.debit-amt:{&SV}) <> 0 THEN DO:
      RUN MessageOnly IN hMessage ("fa0111":U, "").
      APPLY 'entry' TO rowobject.credit-amt.
      RETURN NO-APPLY.
  END.
  
  FIND fa-mast WHERE 
      fa-mast.fa-entity = v-fa-entity AND
      fa-mast.asset-code = rowobject.asset-code:{&SV} 
      NO-LOCK NO-ERROR.
  IF NOT AVAIL fa-mast THEN DO:
      MESSAGE "Asset Code does not exist"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO rowobject.asset-code.
      RETURN NO-APPLY.
  END.
  FIND entity WHERE
      entity.entity-code = rowobject.entity-code:{&SV} NO-LOCK NO-ERROR.
  IF NOT AVAIL entity THEN DO:
      MESSAGE "Entity not on file"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO rowobject.entity-code.
      RETURN NO-APPLY.
  END.
  IF rowobject.job-no:{&SV} <> "" THEN DO:
      FIND job WHERE
          job.job-no = rowobject.job-no:{&SV} NO-LOCK NO-ERROR.
      IF NOT AVAIL job THEN DO:
          MESSAGE "Job not found"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO rowobject.job-no.
          RETURN NO-APPLY.
      END.
  END.
  ASSIGN rowobject.method:{&SV} = STRING(method-combo:{&SV}).

  RUN SUPER.

  DISABLE
      rowobject.entry-no
      WITH FRAME {&FRAME-NAME}.
  add-new = NO.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

