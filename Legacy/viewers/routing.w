&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/routing.w

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
{custom/gcompany.i}
{custom/gloc.i}

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
&Scoped-define EXTERNAL-TABLES routing
&Scoped-define FIRST-EXTERNAL-TABLE routing


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR routing.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS routing.dscr routing.m-code[1] ~
routing.m-code[2] routing.m-code[3] routing.m-code[4] routing.m-code[5] ~
routing.m-code[6] routing.m-code[7] routing.m-code[8] routing.m-code[9] ~
routing.m-code[10] 
&Scoped-define ENABLED-TABLES routing
&Scoped-define FIRST-ENABLED-TABLE routing
&Scoped-Define ENABLED-OBJECTS RECT-5 RECT-1 RECT-6 
&Scoped-Define DISPLAYED-FIELDS routing.r-code routing.dscr ~
routing.m-code[1] routing.m-code[2] routing.m-code[3] routing.m-code[4] ~
routing.m-code[5] routing.m-code[6] routing.m-code[7] routing.m-code[8] ~
routing.m-code[9] routing.m-code[10] 
&Scoped-define DISPLAYED-TABLES routing
&Scoped-define FIRST-DISPLAYED-TABLE routing
&Scoped-Define DISPLAYED-OBJECTS mach_m-dscr mach_m-dscr2 mach_m-dscr3 ~
mach_m-dscr4 mach_m-dscr5 mach_m-dscr6 mach_m-dscr7 mach_m-dscr8 ~
mach_m-dscr9 mach_m-dscr10 F1 F-2 F-3 F-4 F-5 F-6 F-7 F-8 F-9 F-10 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS routing.r-code 
&Scoped-define DISPLAY-FIELD routing.m-code[1] routing.m-code[2] ~
routing.m-code[3] routing.m-code[4] routing.m-code[5] routing.m-code[6] ~
routing.m-code[7] routing.m-code[8] routing.m-code[9] routing.m-code[10] 
&Scoped-define F1 F1 F-2 F-3 F-4 F-5 F-6 F-7 F-8 F-9 F-10 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company|y|y|ASI.routing.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company",
     Keys-Supplied = "company"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE F-10 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

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

DEFINE VARIABLE F-5 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-6 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-7 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-8 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-9 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE mach_m-dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mach_m-dscr10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mach_m-dscr2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mach_m-dscr3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mach_m-dscr4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mach_m-dscr5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mach_m-dscr6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mach_m-dscr7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mach_m-dscr8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mach_m-dscr9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 16.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 2.38.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 13.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     routing.r-code AT ROW 1.48 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
     routing.dscr AT ROW 2.43 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     routing.m-code[1] AT ROW 4.81 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr AT ROW 4.81 COL 29 COLON-ALIGNED NO-LABEL
     routing.m-code[2] AT ROW 6 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr2 AT ROW 6 COL 29 COLON-ALIGNED NO-LABEL
     routing.m-code[3] AT ROW 7.19 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr3 AT ROW 7.19 COL 29 COLON-ALIGNED NO-LABEL
     routing.m-code[4] AT ROW 8.38 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr4 AT ROW 8.38 COL 29 COLON-ALIGNED NO-LABEL
     routing.m-code[5] AT ROW 9.57 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr5 AT ROW 9.57 COL 29 COLON-ALIGNED NO-LABEL
     routing.m-code[6] AT ROW 10.76 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr6 AT ROW 10.76 COL 29 COLON-ALIGNED NO-LABEL
     routing.m-code[7] AT ROW 11.95 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr7 AT ROW 11.95 COL 29 COLON-ALIGNED NO-LABEL
     routing.m-code[8] AT ROW 13.14 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr8 AT ROW 13.14 COL 29 COLON-ALIGNED NO-LABEL
     routing.m-code[9] AT ROW 14.33 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr9 AT ROW 14.33 COL 29 COLON-ALIGNED NO-LABEL
     routing.m-code[10] AT ROW 15.52 COL 7 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr10 AT ROW 15.52 COL 29 COLON-ALIGNED NO-LABEL
     F1 AT ROW 4.81 COL 21 NO-LABEL
     F-2 AT ROW 6 COL 21 NO-LABEL
     F-3 AT ROW 7.19 COL 21 NO-LABEL
     F-4 AT ROW 8.38 COL 21 NO-LABEL
     F-5 AT ROW 9.57 COL 21 NO-LABEL
     F-6 AT ROW 10.76 COL 21 NO-LABEL
     F-7 AT ROW 11.95 COL 21 NO-LABEL
     F-8 AT ROW 13.14 COL 21 NO-LABEL
     F-9 AT ROW 14.33 COL 21 NO-LABEL
     F-10 AT ROW 15.52 COL 21 NO-LABEL
     "Machine Code" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 4.1 COL 5
     "Description" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 4.1 COL 40
     RECT-5 AT ROW 1.24 COL 3
     RECT-1 AT ROW 1 COL 1
     RECT-6 AT ROW 3.86 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.routing
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
         HEIGHT             = 16.19
         WIDTH              = 84.8.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F-10 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-10:HIDDEN IN FRAME F-Main           = TRUE.

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

/* SETTINGS FOR FILL-IN F-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-5:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-6 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-6:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-7 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-7:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-8 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-8:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-9 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-9:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN routing.m-code[10] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN routing.m-code[1] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN routing.m-code[2] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN routing.m-code[3] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN routing.m-code[4] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN routing.m-code[5] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN routing.m-code[6] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN routing.m-code[7] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN routing.m-code[8] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN routing.m-code[9] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN mach_m-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach_m-dscr10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach_m-dscr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach_m-dscr3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach_m-dscr4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach_m-dscr5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach_m-dscr6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach_m-dscr7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach_m-dscr8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach_m-dscr9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN routing.r-code IN FRAME F-Main
   NO-ENABLE 1                                                          */
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   def var char-val as cha no-undo.   
   def var ls-cur-val as cha no-undo.

    case focus:name :
         when "m-code" then do:
             run windows/l-mach.w (routing.company, gloc, focus:screen-value, output char-val).
             if char-val <> "" then do:
                case focus:index :
                     when 1 then assign routing.m-code[1]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr:screen-value = entry(2,char-val).
                     when 2 then assign routing.m-code[2]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr2:screen-value = entry(2,char-val).                                        
                     when 3 then assign routing.m-code[3]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr3:screen-value = entry(2,char-val).
                     when 4 then assign routing.m-code[4]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr4:screen-value = entry(2,char-val).
                     when 5 then assign routing.m-code[5]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr5:screen-value = entry(2,char-val).
                     when 6 then assign routing.m-code[6]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr6:screen-value = entry(2,char-val).
                     when 7 then assign routing.m-code[7]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr7:screen-value = entry(2,char-val).
                     when 8 then assign routing.m-code[8]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr8:screen-value = entry(2,char-val).
                     when 9 then assign routing.m-code[9]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr9:screen-value = entry(2,char-val).
                     when 10 then assign routing.m-code[10]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        mach_m-dscr10:screen-value = entry(2,char-val).

                end case . /* focus index */ 
             end.  
         end .  /* m-code */
    end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[10] V-table-Win
ON LEAVE OF routing.m-code[10] IN FRAME F-Main /* Machine Code[10] */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[1] V-table-Win
ON LEAVE OF routing.m-code[1] IN FRAME F-Main /* Machine Code[1] */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}

  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = self:screen-value) and lastkey <> -1 
     and self:screen-value <> ""         
  then do:
     message "Invalid Machine Code. Try Help!" view-as alert-box error.
     return no-apply.         
  end.            
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[2] V-table-Win
ON LEAVE OF routing.m-code[2] IN FRAME F-Main /* Machine Code[2] */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = self:screen-value) and lastkey <> -1 
  and self:screen-value <> ""            
  then do:
     message "Invalid Machine Code. Try Help!" view-as alert-box error.
     return no-apply.         
  end.            
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[3] V-table-Win
ON LEAVE OF routing.m-code[3] IN FRAME F-Main /* Machine Code[3] */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = self:screen-value) and lastkey <> -1 
  and self:screen-value <> ""
  then do:
     message "Invalid Machine Code. Try Help!" view-as alert-box error.
     return no-apply.         
  end.            
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[4] V-table-Win
ON LEAVE OF routing.m-code[4] IN FRAME F-Main /* Machine Code[4] */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = self:screen-value) and lastkey <> -1
  and self:screen-value <> ""
  then do:
     message "Invalid Machine Code. Try Help!" view-as alert-box error.
     return no-apply.         
  end.            
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[5] V-table-Win
ON LEAVE OF routing.m-code[5] IN FRAME F-Main /* Machine Code[5] */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = self:screen-value) and lastkey <> -1
  and self:screen-value <> ""
  then do:
     message "Invalid Machine Code. Try Help!" view-as alert-box error.
     return no-apply.         
  end.            
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[6] V-table-Win
ON LEAVE OF routing.m-code[6] IN FRAME F-Main /* Machine Code[6] */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = self:screen-value) and lastkey <> -1 
              and self:screen-value <> ""
  then do:
     message "Invalid Machine Code. Try Help!" view-as alert-box error.
     return no-apply.         
  end.            
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[7] V-table-Win
ON LEAVE OF routing.m-code[7] IN FRAME F-Main /* Machine Code[7] */
DO:
   {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = self:screen-value) and lastkey <> -1 
  and self:screen-value <> ""
  then do:
     message "Invalid Machine Code. Try Help!" view-as alert-box error.
     return no-apply.         
  end.            
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[8] V-table-Win
ON LEAVE OF routing.m-code[8] IN FRAME F-Main /* Machine Code[8] */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = self:screen-value) and lastkey <> -1 
  and self:screen-value <> ""
  then do:
     message "Invalid Machine Code. Try Help!" view-as alert-box error.
     return no-apply.         
  end.            
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing.m-code[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing.m-code[9] V-table-Win
ON LEAVE OF routing.m-code[9] IN FRAME F-Main /* Machine Code[9] */
DO:
   {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = self:screen-value) and lastkey <> -1 
  and self:screen-value <> ""
  then do:
     message "Invalid Machine Code. Try Help!" view-as alert-box error.
     return no-apply.         
  end.            
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
session:data-entry-return = yes.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'company':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = routing
           &WHERE = "WHERE routing.company eq key-value"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "routing"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "routing"}

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

  assign routing.company = gcompany
         routing.loc = gloc.

  if adm-adding-record then do:  /* for add not copy */
     assign mach_m-dscr = "" 
            mach_m-dscr2 = ""
            mach_m-dscr3 = ""
            mach_m-dscr4 = ""
             mach_m-dscr5 = ""
             mach_m-dscr6 = ""
             mach_m-dscr7 = ""
             mach_m-dscr8 = ""
             mach_m-dscr9 = ""
             mach_m-dscr10 = ""
             .
     display mach_m-dscr mach_m-dscr2 mach_m-dscr3 mach_m-dscr4
             mach_m-dscr5 mach_m-dscr6 mach_m-dscr7 mach_m-dscr8 mach_m-dscr9
             mach_m-dscr10
             with frame {&frame-name}.
  end.                  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var i as int no-undo.
  {&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
do with frame {&frame-name}:
  run validate-machine (input routing.m-code[1]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[1] .
     return no-apply. 
  end.
  run validate-machine (input routing.m-code[2]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[2] .
     return no-apply.
  end.
  run validate-machine (input routing.m-code[3]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[3] .
     return no-apply.
  end.
  run validate-machine (input routing.m-code[4]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[4] .
     return no-apply.
  end.
  run validate-machine (input routing.m-code[5]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[5] .
     return no-apply.
  end.
  run validate-machine (input routing.m-code[6]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[6] .
     return no-apply.
  end.
  run validate-machine (input routing.m-code[7]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[7] .
     return no-apply.
  end.
  run validate-machine (input routing.m-code[8]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[8] .
     return no-apply.
  end.
  run validate-machine (input routing.m-code[9]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[9] .
     return no-apply.
  end.
  run validate-machine (input routing.m-code[10]:screen-value) no-error.
  if error-status:error then do:
     message "Invalid Machine Code. Try Help!"  view-as alert-box error.
     apply "entry" to routing.m-code[10] .
     return no-apply.
  end.
end.
{&methods/lValidateError.i NO}
/* ======== not working input ================
  do i = 1 to 10 with frame {&frame-name}:  
     if input routing.m-code[i] <> "" then do:        
        run validate-machine (input routing.m-code[i]) no-error.      
        if error-status:error then do:
           message "Invalid Machine Code. Try Help!" 
                   view-as alert-box error.
           case i :
              when 1 then apply "entry" to routing.m-code[1] .
              when 2 then apply "entry" to routing.m-code[2] .
              when 3 then apply "entry" to routing.m-code[3] .
              when 4 then apply "entry" to routing.m-code[4] .
              when 5 then apply "entry" to routing.m-code[5] .
              when 6 then apply "entry" to routing.m-code[6] .
              when 7 then apply "entry" to routing.m-code[7] .
              when 8 then apply "entry" to routing.m-code[8] .
              when 9 then apply "entry" to routing.m-code[9] .
              when 10 then apply "entry" to routing.m-code[10] .
           end case.
           return no-apply.         
        end.            
     end.   
  end.
 ============================*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "routing" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "routing"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-machine V-table-Win 
PROCEDURE validate-machine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-mach like mach.m-code no-undo.

  {methods/lValidateError.i YES}
  if not can-find(mach where mach.company = gcompany
              and mach.loc = gloc
              and mach.m-code = ip-mach) and ip-mach <> ""
  then do:
     return error.
  end.            
  {methods/lValidateError.i NO}
  return.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

