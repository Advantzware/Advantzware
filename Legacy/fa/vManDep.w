&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          ptdb1            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"fa/sdodeptab.i"}.


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


&Scoped-define FRAME-NAME F-Main
{pt/probase1.i}
{dev/method/mxpbrokr.i}
    DEF VAR dep-table-rowid AS ROWID.


        find  login where terminal-no eq terminalid
                     and user-id eq entry(1,userid("ptdb":U),"@") and system eq "fa" no-lock.
        find  entity of  login no-lock.
        find fa-control of  entity no-lock.
        v-fa-entity =  login.entity-code.
        logincontrol = fa-control.fa-entity.
         if not logged then do:
           display "You are logged off!  Cannot execute" functdesc
                   with {&UGUI-SCR} frame std_1 no-label.
           undo, leave.
        end.
        systemname = entity-name.


DEF VAR add-new AS LOG NO-UNDO.
/* CURRENT-WINDOW:TITLE = "dep-table Maintenance". */

{pt/setinit use-file 'dep-table'}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "fa/sdodeptab.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.method RowObject.Auto-sl ~
RowObject.Ratio RowObject.Conv RowObject.Manual RowObject.soyd ~
RowObject.Depr1 RowObject.Depr2 RowObject.Depr3 RowObject.Depr4 ~
RowObject.Depr5 RowObject.Depr6 RowObject.Depr7 RowObject.Depr8 ~
RowObject.Depr9 RowObject.Depr10 RowObject.Depr11 RowObject.Depr12 ~
RowObject.Depr13 RowObject.Depr14 RowObject.Depr15 RowObject.Depr16 ~
RowObject.Depr17 RowObject.Depr18 RowObject.Depr19 RowObject.Depr20 ~
RowObject.Depr21 RowObject.Depr22 RowObject.Depr23 RowObject.Depr24 ~
RowObject.Depr25 RowObject.Depr26 RowObject.Depr27 RowObject.Depr28 ~
RowObject.Depr29 RowObject.Depr30 RowObject.Depr31 RowObject.Depr32 ~
RowObject.Depr33 RowObject.Depr34 RowObject.Depr35 RowObject.Depr36 ~
RowObject.Depr37 RowObject.Depr38 RowObject.Depr39 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.method RowObject.Auto-sl ~
RowObject.Ratio RowObject.Conv RowObject.Manual RowObject.soyd ~
RowObject.Depr1 RowObject.Depr2 RowObject.Depr3 RowObject.Depr4 ~
RowObject.Depr5 RowObject.Depr6 RowObject.Depr7 RowObject.Depr8 ~
RowObject.Depr9 RowObject.Depr10 RowObject.Depr11 RowObject.Depr12 ~
RowObject.Depr13 RowObject.Depr14 RowObject.Depr15 RowObject.Depr16 ~
RowObject.Depr17 RowObject.Depr18 RowObject.Depr19 RowObject.Depr20 ~
RowObject.Depr21 RowObject.Depr22 RowObject.Depr23 RowObject.Depr24 ~
RowObject.Depr25 RowObject.Depr26 RowObject.Depr27 RowObject.Depr28 ~
RowObject.Depr29 RowObject.Depr30 RowObject.Depr31 RowObject.Depr32 ~
RowObject.Depr33 RowObject.Depr34 RowObject.Depr35 RowObject.Depr36 ~
RowObject.Depr37 RowObject.Depr38 RowObject.Depr39 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.method AT ROW 1 COL 19 COLON-ALIGNED
          LABEL "Depr. Method"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY 1
     RowObject.Auto-sl AT ROW 2.25 COL 19 COLON-ALIGNED
          LABEL "Str.Line/Declining"
          VIEW-AS FILL-IN 
          SIZE 14.57 BY 1
     RowObject.Ratio AT ROW 3.5 COL 19 COLON-ALIGNED
          LABEL "Depr. Multiplier"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY 1
     RowObject.Conv AT ROW 4.75 COL 19 COLON-ALIGNED
          LABEL "Conversion Method"
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     RowObject.Manual AT ROW 6 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     RowObject.soyd AT ROW 7.25 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     RowObject.Depr1 AT ROW 2.25 COL 38 COLON-ALIGNED
          LABEL "Period 1"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr2 AT ROW 3.5 COL 38 COLON-ALIGNED
          LABEL "2"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr3 AT ROW 4.75 COL 38 COLON-ALIGNED
          LABEL "3"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr4 AT ROW 6 COL 38 COLON-ALIGNED
          LABEL "4"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr5 AT ROW 7.25 COL 38 COLON-ALIGNED
          LABEL "5"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr6 AT ROW 8.5 COL 38 COLON-ALIGNED
          LABEL "6"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr7 AT ROW 9.75 COL 38 COLON-ALIGNED
          LABEL "7"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr8 AT ROW 11 COL 38 COLON-ALIGNED
          LABEL "8"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr9 AT ROW 12.25 COL 38 COLON-ALIGNED
          LABEL "9"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr10 AT ROW 13.5 COL 38 COLON-ALIGNED
          LABEL "10"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr11 AT ROW 14.75 COL 38 COLON-ALIGNED
          LABEL "11"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr12 AT ROW 16 COL 38 COLON-ALIGNED
          LABEL "12"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr13 AT ROW 17.25 COL 38 COLON-ALIGNED
          LABEL "13"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr14 AT ROW 2.25 COL 63 COLON-ALIGNED
          LABEL "14"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr15 AT ROW 3.5 COL 63 COLON-ALIGNED
          LABEL "15"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr16 AT ROW 4.75 COL 63 COLON-ALIGNED
          LABEL "16"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr17 AT ROW 6 COL 63 COLON-ALIGNED
          LABEL "17"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr18 AT ROW 7.25 COL 63 COLON-ALIGNED
          LABEL "18"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr19 AT ROW 8.5 COL 63 COLON-ALIGNED
          LABEL "19"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 111.72 BY 17.63
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Depr20 AT ROW 9.75 COL 63 COLON-ALIGNED
          LABEL "20"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr21 AT ROW 11 COL 63 COLON-ALIGNED
          LABEL "21"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr22 AT ROW 12.25 COL 63 COLON-ALIGNED
          LABEL "22"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr23 AT ROW 13.5 COL 63 COLON-ALIGNED
          LABEL "23"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr24 AT ROW 14.75 COL 63 COLON-ALIGNED
          LABEL "24"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr25 AT ROW 16 COL 63 COLON-ALIGNED
          LABEL "25"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr26 AT ROW 17.25 COL 63 COLON-ALIGNED
          LABEL "26"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr27 AT ROW 2.25 COL 88 COLON-ALIGNED
          LABEL "27"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr28 AT ROW 3.5 COL 88 COLON-ALIGNED
          LABEL "28"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr29 AT ROW 4.75 COL 88 COLON-ALIGNED
          LABEL "29"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr30 AT ROW 6 COL 88 COLON-ALIGNED
          LABEL "30"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr31 AT ROW 7.25 COL 88 COLON-ALIGNED
          LABEL "31"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr32 AT ROW 8.5 COL 88 COLON-ALIGNED
          LABEL "32"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr33 AT ROW 9.75 COL 88 COLON-ALIGNED
          LABEL "33"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr34 AT ROW 11 COL 88 COLON-ALIGNED
          LABEL "34"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr35 AT ROW 12.25 COL 88 COLON-ALIGNED
          LABEL "35"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr36 AT ROW 13.5 COL 88 COLON-ALIGNED
          LABEL "36"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr37 AT ROW 14.75 COL 88 COLON-ALIGNED
          LABEL "37"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr38 AT ROW 16 COL 88 COLON-ALIGNED
          LABEL "38"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     RowObject.Depr39 AT ROW 17.25 COL 88 COLON-ALIGNED
          LABEL "39"
          VIEW-AS FILL-IN 
          SIZE 19.86 BY 1
     "Manual Depreciation" VIEW-AS TEXT
          SIZE 20 BY .67 AT ROW 1.25 COL 65
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 111.72 BY 17.63
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "fa/sdodeptab.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {fa/sdodeptab.i}
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
         HEIGHT             = 17.63
         WIDTH              = 111.72.
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
   NOT-VISIBLE L-To-R,COLUMNS                                           */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Auto-sl IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Conv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr10 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr11 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr12 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr13 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr14 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr15 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr16 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr17 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr18 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr19 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr20 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr21 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr22 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr23 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr24 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr25 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr26 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr27 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr28 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr29 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr3 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr30 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr31 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr32 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr33 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr34 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr35 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr36 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr37 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr38 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr39 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr4 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr5 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr6 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr7 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr8 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Depr9 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.method IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Ratio IN FRAME F-Main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME RowObject.Manual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Manual vTableWin
ON LEAVE OF RowObject.Manual IN FRAME F-Main /* Manual */
DO:
    IF string(rowobject.manual:{&SV}) = "NO" THEN
        DISABLE 
        rowobject.depr1 
        rowobject.depr2
        rowobject.depr3
        rowobject.depr4
        rowobject.depr5
        rowobject.depr6
        rowobject.depr7
        rowobject.depr8
        rowobject.depr9
        rowobject.depr10
        rowobject.depr11
        rowobject.depr12
        rowobject.depr13
        rowobject.depr14
        rowobject.depr15
        rowobject.depr16
        rowobject.depr17
        rowobject.depr18
        rowobject.depr19
        rowobject.depr20
        rowobject.depr21
        rowobject.depr22
        rowobject.depr23
        rowobject.depr24
        rowobject.depr25
        rowobject.depr26
        rowobject.depr27
        rowobject.depr28
        rowobject.depr29
        rowobject.depr30
        rowobject.depr31
        rowobject.depr32
        rowobject.depr33
        rowobject.depr34
        rowobject.depr35
        rowobject.depr36
        rowobject.depr37
        rowobject.depr38
        rowobject.depr39
        rowobject.depr40
        WITH FRAME {&FRAME-NAME}.
    ELSE
        ENABLE 
        rowobject.depr1 
        rowobject.depr2
        rowobject.depr3
        rowobject.depr4
        rowobject.depr5
        rowobject.depr6
        rowobject.depr7
        rowobject.depr8
        rowobject.depr9
        rowobject.depr10
        rowobject.depr11
        rowobject.depr12
        rowobject.depr13
        rowobject.depr14
        rowobject.depr15
        rowobject.depr16
        rowobject.depr17
        rowobject.depr18
        rowobject.depr19
        rowobject.depr20
        rowobject.depr21
        rowobject.depr22
        rowobject.depr23
        rowobject.depr24
        rowobject.depr25
        rowobject.depr26
        rowobject.depr27
        rowobject.depr28
        rowobject.depr29
        rowobject.depr30
        rowobject.depr31
        rowobject.depr32
        rowobject.depr33
        rowobject.depr34
        rowobject.depr35
        rowobject.depr36
        rowobject.depr37
        rowobject.depr38
        rowobject.depr39
        rowobject.depr40
            WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.method
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.method vTableWin
ON LEAVE OF RowObject.method IN FRAME F-Main /* Depr. Method */
DO:
    IF LASTKEY <> -1 AND rowobject.method:{&SV} = "" THEN DO:
        MESSAGE "Method may not be blank"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' to rowobject.method.
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
  ENABLE rowobject.method WITH FRAME {&FRAME-NAME}.


  DISABLE 
        rowobject.depr1 
        rowobject.depr2
        rowobject.depr3
        rowobject.depr4
        rowobject.depr5
        rowobject.depr6
        rowobject.depr7
        rowobject.depr8
        rowobject.depr9
        rowobject.depr10
        rowobject.depr11
        rowobject.depr12
        rowobject.depr13
        rowobject.depr14
        rowobject.depr15
        rowobject.depr16
        rowobject.depr17
        rowobject.depr18
        rowobject.depr19
        rowobject.depr20
        rowobject.depr21
        rowobject.depr22
        rowobject.depr23
        rowobject.depr24
        rowobject.depr25
        rowobject.depr26
        rowobject.depr27
        rowobject.depr28
        rowobject.depr29
        rowobject.depr30
        rowobject.depr31
        rowobject.depr32
        rowobject.depr33
        rowobject.depr34
        rowobject.depr35
        rowobject.depr36
        rowobject.depr37
        rowobject.depr38
        rowobject.depr39
        rowobject.depr40
        WITH FRAME {&FRAME-NAME}.
   

  RUN SUPER.
  add-new = YES.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  add-new = NO.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord vTableWin 
PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ENABLE rowobject.method WITH FRAME {&FRAME-NAME}.
  DISABLE 
        rowobject.depr1 
        rowobject.depr2
        rowobject.depr3
        rowobject.depr4
        rowobject.depr5
        rowobject.depr6
        rowobject.depr7
        rowobject.depr8
        rowobject.depr9
        rowobject.depr10
        rowobject.depr11
        rowobject.depr12
        rowobject.depr13
        rowobject.depr14
        rowobject.depr15
        rowobject.depr16
        rowobject.depr17
        rowobject.depr18
        rowobject.depr19
        rowobject.depr20
        rowobject.depr21
        rowobject.depr22
        rowobject.depr23
        rowobject.depr24
        rowobject.depr25
        rowobject.depr26
        rowobject.depr27
        rowobject.depr28
        rowobject.depr29
        rowobject.depr30
        rowobject.depr31
        rowobject.depr32
        rowobject.depr33
        rowobject.depr34
        rowobject.depr35
        rowobject.depr36
        rowobject.depr37
        rowobject.depr38
        rowobject.depr39
        rowobject.depr40
        WITH FRAME {&FRAME-NAME}.

  RUN SUPER.
  add-new = YES.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord vTableWin 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  do-delete = no.
  message "Are you sure you want to delete this record?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
      update do-delete.
  if not do-delete then  return no-apply .
  deleteok = yes.


  FIND dep-table WHERE
    dep-table.method = rowobject.method:{&SV} NO-LOCK NO-ERROR.

  ASSIGN dep-table-rowid = rowid(dep-table).

 
        /* TEST IF RECORD IS REFERENCED ELSEWERE */

  run value("pt/hypdel.p":U) (input "dep-table":U,
                              input rowid(dep-table),
                              input no,
                              input-output deleteok,
                              input yes).

  if deleteok then do:
      run messageOnly IN hMessage ("fa0026":U,  string(dep-table.method) +  "~001":U ).
            /* Delete UDF value */
      run value("pt/deluval.p":U) ("dep-table":U).
      RUN SUPER.
  END.
  view frame {&frame-name}.

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

DEF VAR cmethod LIKE dep-table.method.
DEF BUFFER z_numgen FOR  z_numgen.

    /* Code placed here will execute PRIOR to standard behavior. */
    IF add-new THEN do:
        ENABLE rowobject.method WITH FRAME {&FRAME-NAME}.
    END.
    ELSE DO:
        DISABLE rowobject.method WITH FRAME {&FRAME-NAME}.
    END.

    RUN SUPER.
    ASSIGN cmethod = rowobject.method:{&SV}.

    {pt/setinit method cmethod}.

    ASSIGN   wkeylist = 
         rowobject.method:{&SV} + "," 
        wfilename = "dep-table":U.

    FIND dep-table WHERE
        dep-table.method = rowobject.method:{&SV} NO-LOCK NO-ERROR.


DO:
    IF string(rowobject.manual:{&SV}) = "NO" THEN
        DISABLE 
        rowobject.depr1 
        rowobject.depr2
        rowobject.depr3
        rowobject.depr4
        rowobject.depr5
        rowobject.depr6
        rowobject.depr7
        rowobject.depr8
        rowobject.depr9
        rowobject.depr10
        rowobject.depr11
        rowobject.depr12
        rowobject.depr13
        rowobject.depr14
        rowobject.depr15
        rowobject.depr16
        rowobject.depr17
        rowobject.depr18
        rowobject.depr19
        rowobject.depr20
        rowobject.depr21
        rowobject.depr22
        rowobject.depr23
        rowobject.depr24
        rowobject.depr25
        rowobject.depr26
        rowobject.depr27
        rowobject.depr28
        rowobject.depr29
        rowobject.depr30
        rowobject.depr31
        rowobject.depr32
        rowobject.depr33
        rowobject.depr34
        rowobject.depr35
        rowobject.depr36
        rowobject.depr37
        rowobject.depr38
        rowobject.depr39
        rowobject.depr40
        WITH FRAME {&FRAME-NAME}.
    ELSE
        ENABLE 
        rowobject.depr1 
        rowobject.depr2
        rowobject.depr3
        rowobject.depr4
        rowobject.depr5
        rowobject.depr6
        rowobject.depr7
        rowobject.depr8
        rowobject.depr9
        rowobject.depr10
        rowobject.depr11
        rowobject.depr12
        rowobject.depr13
        rowobject.depr14
        rowobject.depr15
        rowobject.depr16
        rowobject.depr17
        rowobject.depr18
        rowobject.depr19
        rowobject.depr20
        rowobject.depr21
        rowobject.depr22
        rowobject.depr23
        rowobject.depr24
        rowobject.depr25
        rowobject.depr26
        rowobject.depr27
        rowobject.depr28
        rowobject.depr29
        rowobject.depr30
        rowobject.depr31
        rowobject.depr32
        rowobject.depr33
        rowobject.depr34
        rowobject.depr35
        rowobject.depr36
        rowobject.depr37
        rowobject.depr38
        rowobject.depr39
        rowobject.depr40
            WITH FRAME {&FRAME-NAME}.
END.

    ASSIGN dep-table-rowid = rowid(dep-table).



    do for z_numgen transaction:
        {fa/depedt3}.   /* GET NEXT SEQUENCE FOR AUDIT */
    END.

  /* Code placed here will execute PRIOR to standard behavior. */

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
  /*
  ASSIGN wmemo-function = IF PROGRAM-NAME(2) BEGINS "pt/ddialog.w" 
      THEN "Inquiry" ELSE "Maintenance":U.
*/


  RUN SUPER.
  DISABLE rowobject.method WITH FRAME {&FRAME-NAME}.
  DISABLE 
        rowobject.depr1 
        rowobject.depr2
        rowobject.depr3
        rowobject.depr4
        rowobject.depr5
        rowobject.depr6
        rowobject.depr7
        rowobject.depr8
        rowobject.depr9
        rowobject.depr10
        rowobject.depr11
        rowobject.depr12
        rowobject.depr13
        rowobject.depr14
        rowobject.depr15
        rowobject.depr16
        rowobject.depr17
        rowobject.depr18
        rowobject.depr19
        rowobject.depr20
        rowobject.depr21
        rowobject.depr22
        rowobject.depr23
        rowobject.depr24
        rowobject.depr25
        rowobject.depr26
        rowobject.depr27
        rowobject.depr28
        rowobject.depr29
        rowobject.depr30
        rowobject.depr31
        rowobject.depr32
        rowobject.depr33
        rowobject.depr34
        rowobject.depr35
        rowobject.depr36
        rowobject.depr37
        rowobject.depr38
        rowobject.depr39
        rowobject.depr40
        WITH FRAME {&FRAME-NAME}.

  

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord vTableWin 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OUTPUT TO PRINTER.
    VIEW FRAME {&FRAME-NAME}.

    OUTPUT CLOSE.
    message "The help window has now been listed to the system printer".
    hide message.
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

  DEF BUFFER z_audit FOR  z_audit.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF add-new AND rowobject.method:{&SV} = "" THEN DO:
      MESSAGE "Method may not be blank"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' to rowobject.method.
      RETURN NO-APPLY.
  END.

  FIND dep-table WHERE
       dep-table.method = rowobject.method:{&SV} NO-LOCK NO-ERROR.

  
  IF add-new THEN DO: 
      ENABLE rowobject.method WITH FRAME {&FRAME-NAME}.
  END.
  ELSE do:
      DISABLE rowobject.method WITH FRAME {&FRAME-NAME}.
  END.

  if NOT add-new  AND AVAIL dep-table then do:
      if wdo-audit then do for z_audit TRANSACTION:
          {fa/depedt4}.
      END.

      waction = "CHANGE:NEW":U.
      waction1 = "Modifying an existing Depreciation Table record".
   END.     /* OF NOT NEW RECORD */
   else waction1 =  "Creating new Depreciation Table record.".
   RUN SUPER.

   FIND dep-table WHERE
        dep-table.method = rowobject.method:{&SV} NO-LOCK NO-ERROR.

    /* Update UDF value */
      if search("udf/dep-table.p":U)<>? or search("udf/dep-table.r":U)<>?
         then run value("udf/dep-table.p":U) ("update":U).
      if wdo-audit then do for  z_audit TRANSACTION:
          {fa/depedt4 "+ 0.5"}.   /* CREATE AUDIT "NEW" */
          
      end. /* OF AUDIT CREATION */
    add-new = NO.
  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

