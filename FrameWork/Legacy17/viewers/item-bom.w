&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers\item-bom.w

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

def var v-bom-item like item-bom.i-no extent 10 no-undo.
def var v-bom-dscr like item.i-name extent 10 no-undo.
def var v-shrink like item.shrink extent 10 no-undo.
def var lv-bom-count as int no-undo.
def buffer b-item for item.
def var lv-first as log init YES NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS item.i-no 
&Scoped-define ENABLED-TABLES item
&Scoped-define FIRST-ENABLED-TABLE item
&Scoped-Define ENABLED-OBJECTS RECT-22 
&Scoped-Define DISPLAYED-FIELDS item.i-no 
&Scoped-define DISPLAYED-TABLES item
&Scoped-define FIRST-DISPLAYED-TABLE item
&Scoped-Define DISPLAYED-OBJECTS v-bom-1 v-bom-dscr-1 v-shrink-1 v-bom-2 ~
v-bom-dscr-2 v-shrink-2 v-bom-3 v-bom-dscr-3 v-shrink-3 v-bom-4 ~
v-bom-dscr-4 v-shrink-4 v-bom-5 v-bom-dscr-5 v-shrink-5 v-bom-6 ~
v-bom-dscr-6 v-shrink-6 v-bom-7 v-bom-dscr-7 v-shrink-7 v-bom-8 ~
v-bom-dscr-8 v-shrink-8 v-lam-code v-bom-dscr-9 v-adh-code v-bom-dscr-10 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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
DEFINE VARIABLE v-adh-code AS CHARACTER FORMAT "x(10)":U 
     LABEL "Adhesive" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-1 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 1" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-2 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 2" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-3 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 3" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-4 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 4" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-5 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 5" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-6 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 6" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-7 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 7" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-8 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 8" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-1 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-lam-code AS CHARACTER FORMAT "x(10)":U 
     LABEL "Laminate" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-shrink-1 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-shrink-2 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-shrink-3 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-shrink-4 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-shrink-5 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-shrink-6 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-shrink-7 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-shrink-8 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.8 BY 12.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     v-bom-1 AT ROW 2.67 COL 6.4
     v-bom-dscr-1 AT ROW 2.67 COL 34 COLON-ALIGNED NO-LABEL
     v-shrink-1 AT ROW 2.67 COL 68 COLON-ALIGNED NO-LABEL
     v-bom-2 AT ROW 3.62 COL 15 COLON-ALIGNED
     v-bom-dscr-2 AT ROW 3.62 COL 34 COLON-ALIGNED NO-LABEL
     v-shrink-2 AT ROW 3.62 COL 68 COLON-ALIGNED NO-LABEL
     v-bom-3 AT ROW 4.57 COL 15 COLON-ALIGNED
     v-bom-dscr-3 AT ROW 4.57 COL 34 COLON-ALIGNED NO-LABEL
     v-shrink-3 AT ROW 4.57 COL 68 COLON-ALIGNED NO-LABEL
     v-bom-4 AT ROW 5.52 COL 15 COLON-ALIGNED
     v-bom-dscr-4 AT ROW 5.52 COL 34 COLON-ALIGNED NO-LABEL
     v-shrink-4 AT ROW 5.52 COL 68 COLON-ALIGNED NO-LABEL
     v-bom-5 AT ROW 6.48 COL 15 COLON-ALIGNED
     v-bom-dscr-5 AT ROW 6.48 COL 34 COLON-ALIGNED NO-LABEL
     v-shrink-5 AT ROW 6.48 COL 68 COLON-ALIGNED NO-LABEL
     v-bom-6 AT ROW 7.43 COL 15 COLON-ALIGNED
     v-bom-dscr-6 AT ROW 7.43 COL 34 COLON-ALIGNED NO-LABEL
     v-shrink-6 AT ROW 7.43 COL 68 COLON-ALIGNED NO-LABEL
     v-bom-7 AT ROW 8.38 COL 15 COLON-ALIGNED
     v-bom-dscr-7 AT ROW 8.38 COL 34 COLON-ALIGNED NO-LABEL
     v-shrink-7 AT ROW 8.38 COL 68 COLON-ALIGNED NO-LABEL
     v-bom-8 AT ROW 9.33 COL 15 COLON-ALIGNED
     v-bom-dscr-8 AT ROW 9.33 COL 34 COLON-ALIGNED NO-LABEL
     v-shrink-8 AT ROW 9.33 COL 68 COLON-ALIGNED NO-LABEL
     v-lam-code AT ROW 10.48 COL 15 COLON-ALIGNED WIDGET-ID 2
     v-bom-dscr-9 AT ROW 10.48 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     v-adh-code AT ROW 11.52 COL 15 COLON-ALIGNED WIDGET-ID 4
     v-bom-dscr-10 AT ROW 11.52 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     item.i-no AT ROW 12.33 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     "Item" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 19
     "Shrink (%)" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.95 COL 71
     "Description" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.95 COL 39
     RECT-22 AT ROW 1.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.item
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
         HEIGHT             = 18.05
         WIDTH              = 88.8.
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

ASSIGN 
       item.i-no:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN v-adh-code IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       v-adh-code:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN v-bom-1 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN v-bom-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-lam-code IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       v-lam-code:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN v-shrink-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-shrink-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-shrink-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-shrink-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-shrink-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-shrink-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-shrink-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-shrink-8 IN FRAME F-Main
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    def var char-val as cha no-undo.
    case focus:name :
        when "v-bom-1" then do:
             run cec/l-itempa.w (item.company, "P" , focus:screen-value, output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-1:screen-value = entry(2,char-val)
                       v-shrink-1:screen-value = entry(3,char-val)
                       v-bom-1.
        end.
        when "v-bom-2" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-2:screen-value = entry(2,char-val)
                       v-shrink-2:screen-value = entry(3,char-val)
                       v-bom-2.
        end.
        when "v-bom-3" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-3:screen-value = entry(2,char-val)
                       v-shrink-3:screen-value = entry(3,char-val)
                       v-bom-3.
        end.
        when "v-bom-4" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-4:screen-value = entry(2,char-val)
                       v-shrink-4:screen-value = entry(3,char-val)                       
                       v-bom-4.
        end.
        when "v-bom-5" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-5:screen-value = entry(2,char-val)
                       v-shrink-5:screen-value = entry(3,char-val)
                       v-bom-5.
        end.
        when "v-bom-6" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-6:screen-value = entry(2,char-val)
                       v-shrink-6:screen-value = entry(3,char-val)
                       v-bom-6.
        end.
        when "v-bom-7" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-7:screen-value = entry(2,char-val)
                       v-shrink-7:screen-value = entry(3,char-val)
                       v-bom-7.
        end.
        when "v-bom-8" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-8:screen-value = entry(2,char-val)
                       v-shrink-8:screen-value = entry(3,char-val)
                       v-bom-8.
        end.
        when "v-lam-code" then do:
             RUN windows/l-lamin.w (ITEM.company, "2",FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN
                ASSIGN
                   FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                   v-bom-dscr-9:SCREEN-VALUE = ENTRY(3,char-val)
                   v-lam-code.
        end.
        when "v-adh-code" then do:
             RUN windows/l-adhsve.w (ITEM.company, "2",FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val NE "" AND char-val NE FOCUS:SCREEN-VALUE THEN
                ASSIGN
                   FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                   v-bom-dscr-10:SCREEN-VALUE = ENTRY(3,char-val)
                   v-adh-code.
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-adh-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-adh-code V-table-Win
ON LEAVE OF v-adh-code IN FRAME F-Main /* Adhesive */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" AND
      v-adh-code NE v-adh-code:SCREEN-VALUE THEN
   DO:
      FIND FIRST b-item WHERE
           b-item.company  eq item.company AND
           b-item.i-no     eq self:SCREEN-VALUE AND
           (b-item.mat-type eq "G" OR item.mat-type = 'T')
           NO-LOCK NO-ERROR.

      IF NOT AVAIL b-item THEN
      do:
         message " You MUST Enter a Valid Adhesive Code or Blank " view-as alert-box error.
         return no-apply.
      end.
      ELSE
         ASSIGN
            v-bom-dscr-10:SCREEN-VALUE = b-item.i-name.

      ASSIGN v-adh-code.
   END.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-1 V-table-Win
ON LEAVE OF v-bom-1 IN FRAME F-Main /* Paper 1 */
DO:
    {&methods/lValidateError.i YES}
    if lastkey <> -1 and self:screen-value <> "" AND
       v-bom-1 NE v-bom-1:SCREEN-VALUE THEN
    DO:
       FIND first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq self:screen-value
                 and b-item.mat-type eq "P" /* Paper */
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-item THEN
       do:
          message "You MUST Enter a Valid Paper Item or Blank " 
                 view-as alert-box error.
          return no-apply.
       END.
       ELSE
          ASSIGN
             v-bom-dscr-1:SCREEN-VALUE = b-item.i-name
             v-shrink-1:SCREEN-VALUE = STRING(b-item.shrink).
    END.
    ELSE if lastkey <> -1 and self:screen-value = "" AND
       v-bom-1 NE v-bom-1:SCREEN-VALUE THEN
       ASSIGN
          v-bom-dscr-1:SCREEN-VALUE = ""
          v-shrink-1:SCREEN-VALUE = "0".

    ASSIGN v-bom-1.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-2 V-table-Win
ON LEAVE OF v-bom-2 IN FRAME F-Main /* Paper 2 */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" AND
      v-bom-2 NE v-bom-2:SCREEN-VALUE THEN
   DO:
      FIND first b-item
           where b-item.company  eq item.company
           and b-item.i-no     eq self:screen-value
           and b-item.mat-type eq "P" /* Paper */
           NO-LOCK NO-ERROR.

      IF NOT AVAIL b-item THEN
      do:
         message "You MUST Enter a Valid Paper Item or Blank " 
                view-as alert-box error.
         return no-apply.
      END.
      ELSE
         ASSIGN
            v-bom-dscr-2:SCREEN-VALUE = b-item.i-name
            v-shrink-2:SCREEN-VALUE = STRING(b-item.shrink).
   END.
   ELSE if lastkey <> -1 and self:screen-value = "" AND
      v-bom-2 NE v-bom-2:SCREEN-VALUE THEN
      ASSIGN
         v-bom-dscr-2:SCREEN-VALUE = ""
         v-shrink-2:SCREEN-VALUE = "0".

   ASSIGN v-bom-2.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-3 V-table-Win
ON LEAVE OF v-bom-3 IN FRAME F-Main /* Paper 3 */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" AND
      v-bom-3 NE v-bom-3:SCREEN-VALUE THEN
   DO:
      FIND first b-item
           where b-item.company  eq item.company
           and b-item.i-no     eq self:screen-value
           and b-item.mat-type eq "P" /* Paper */
           NO-LOCK NO-ERROR.

      IF NOT AVAIL b-item THEN
      do:
         message "You MUST Enter a Valid Paper Item or Blank " 
                view-as alert-box error.
         return no-apply.
      END.
      ELSE
         ASSIGN
            v-bom-dscr-3:SCREEN-VALUE = b-item.i-name
            v-shrink-3:SCREEN-VALUE = STRING(b-item.shrink).
   END.
   ELSE if lastkey <> -1 and self:screen-value = "" AND
      v-bom-3 NE v-bom-3:SCREEN-VALUE THEN
       ASSIGN
          v-bom-dscr-3:SCREEN-VALUE = ""
          v-shrink-3:SCREEN-VALUE = "0".

   ASSIGN v-bom-3.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-4 V-table-Win
ON LEAVE OF v-bom-4 IN FRAME F-Main /* Paper 4 */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" AND
      v-bom-4 NE v-bom-4:SCREEN-VALUE THEN
   DO:
      FIND first b-item
           where b-item.company  eq item.company
           and b-item.i-no     eq self:screen-value
           and b-item.mat-type eq "P" /* Paper */
           NO-LOCK NO-ERROR.

      IF NOT AVAIL b-item THEN
      do:
         message "You MUST Enter a Valid Paper Item or Blank " 
                view-as alert-box error.
         return no-apply.
      END.
      ELSE
         ASSIGN
            v-bom-dscr-4:SCREEN-VALUE = b-item.i-name
            v-shrink-4:SCREEN-VALUE = STRING(b-item.shrink).
   END.
   ELSE if lastkey <> -1 and self:screen-value = "" AND
      v-bom-4 NE v-bom-4:SCREEN-VALUE THEN
      ASSIGN
         v-bom-dscr-4:SCREEN-VALUE = ""
         v-shrink-4:SCREEN-VALUE = "0".

   ASSIGN v-bom-4.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-5 V-table-Win
ON LEAVE OF v-bom-5 IN FRAME F-Main /* Paper 5 */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" AND
      v-bom-5 NE v-bom-5:SCREEN-VALUE THEN
   DO:
      FIND first b-item
           where b-item.company  eq item.company
           and b-item.i-no     eq self:screen-value
           and b-item.mat-type eq "P" /* Paper */
           NO-LOCK NO-ERROR.

      IF NOT AVAIL b-item THEN
      do:
         message "You MUST Enter a Valid Paper Item or Blank " 
                view-as alert-box error.
         return no-apply.
      END.
      ELSE
         ASSIGN
            v-bom-dscr-5:SCREEN-VALUE = b-item.i-name
            v-shrink-5:SCREEN-VALUE = STRING(b-item.shrink).
   END.
   ELSE if lastkey <> -1 and self:screen-value = "" AND
       v-bom-5 NE v-bom-5:SCREEN-VALUE THEN
       ASSIGN
          v-bom-dscr-5:SCREEN-VALUE = ""
          v-shrink-5:SCREEN-VALUE = "0".

   ASSIGN v-bom-5.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-6 V-table-Win
ON LEAVE OF v-bom-6 IN FRAME F-Main /* Paper 6 */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" AND
      v-bom-6 NE v-bom-6:SCREEN-VALUE THEN
   DO:
      FIND first b-item
           where b-item.company  eq item.company
           and b-item.i-no     eq self:screen-value
           and b-item.mat-type eq "P" /* Paper */
           NO-LOCK NO-ERROR.

      IF NOT AVAIL b-item THEN
      do:
         message "You MUST Enter a Valid Paper Item or Blank " 
                view-as alert-box error.
         return no-apply.
      END.
      ELSE
         ASSIGN
            v-bom-dscr-6:SCREEN-VALUE = b-item.i-name
            v-shrink-6:SCREEN-VALUE = STRING(b-item.shrink).
   END.
   ELSE if lastkey <> -1 and self:screen-value = "" AND
        v-bom-6 NE v-bom-6:SCREEN-VALUE THEN
        ASSIGN
          v-bom-dscr-6:SCREEN-VALUE = ""
          v-shrink-6:SCREEN-VALUE = "0".

   ASSIGN v-bom-6.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-7 V-table-Win
ON LEAVE OF v-bom-7 IN FRAME F-Main /* Paper 7 */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" AND
      v-bom-7 NE v-bom-7:SCREEN-VALUE THEN
   DO:
      FIND first b-item
           where b-item.company  eq item.company
           and b-item.i-no     eq self:screen-value
           and b-item.mat-type eq "P" /* Paper */
           NO-LOCK NO-ERROR.

      IF NOT AVAIL b-item THEN
      do:
         message "You MUST Enter a Valid Paper Item or Blank " 
                view-as alert-box error.
         return no-apply.
      END.
      ELSE
         ASSIGN
            v-bom-dscr-7:SCREEN-VALUE = b-item.i-name
            v-shrink-7:SCREEN-VALUE = STRING(b-item.shrink).
   END.
   ELSE if lastkey <> -1 and self:screen-value = "" AND
        v-bom-7 NE v-bom-7:SCREEN-VALUE THEN
        ASSIGN
          v-bom-dscr-7:SCREEN-VALUE = ""
          v-shrink-7:SCREEN-VALUE = "0".

   ASSIGN v-bom-7.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-8 V-table-Win
ON LEAVE OF v-bom-8 IN FRAME F-Main /* Paper 8 */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" AND
      v-bom-8 NE v-bom-8:SCREEN-VALUE THEN
   DO:
      FIND first b-item
           where b-item.company  eq item.company
           and b-item.i-no     eq self:screen-value
           and b-item.mat-type eq "P" /* Paper */
           NO-LOCK NO-ERROR.

      IF NOT AVAIL b-item THEN
      do:
         message "You MUST Enter a Valid Paper Item or Blank " 
                view-as alert-box error.
         return no-apply.
      END.
      ELSE
         ASSIGN
            v-bom-dscr-8:SCREEN-VALUE = b-item.i-name
            v-shrink-8:SCREEN-VALUE = STRING(b-item.shrink).
   END.
   ELSE if lastkey <> -1 and self:screen-value = "" AND
        v-bom-8 NE v-bom-8:SCREEN-VALUE THEN
        ASSIGN
          v-bom-dscr-8:SCREEN-VALUE = ""
          v-shrink-8:SCREEN-VALUE = "0".

   ASSIGN v-bom-8.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-lam-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-lam-code V-table-Win
ON LEAVE OF v-lam-code IN FRAME F-Main /* Laminate */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" AND
      v-lam-code NE v-lam-code:SCREEN-VALUE THEN
   DO:
      FIND first b-item WHERE
           b-item.company  eq item.company AND
           b-item.i-no     eq self:SCREEN-VALUE AND
           b-item.mat-type eq "L"
           NO-LOCK NO-ERROR.

      IF NOT AVAIL b-item THEN
      do:
         message " You MUST Enter a Valid Laminate Code or Blank " view-as alert-box error.
         return no-apply.
      end.
      ELSE
         ASSIGN
            v-bom-dscr-9:SCREEN-VALUE = b-item.i-name.

      ASSIGN v-lam-code.
   END.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
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
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

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
  def buffer b-item2 for item.
  DEF VAR v-count AS INT INIT 8 NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
/*
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
*/
  /* Code placed here will execute AFTER standard behavior.    */
  assign v-bom-item[1] = v-bom-1:screen-value in frame {&frame-name}
         v-bom-item[2] = v-bom-2:screen-value in frame {&frame-name}
         v-bom-item[3] = v-bom-3:screen-value in frame {&frame-name}
         v-bom-item[4] = v-bom-4:screen-value in frame {&frame-name}
         v-bom-item[5] = v-bom-5:screen-value in frame {&frame-name}
         v-bom-item[6] = v-bom-6:screen-value in frame {&frame-name}
         v-bom-item[7] = v-bom-7:screen-value in frame {&frame-name}
         v-bom-item[8] = v-bom-8:screen-value in frame {&frame-name}
         v-shrink[1] = decimal(v-shrink-1:screen-value in frame {&frame-name})
         v-shrink[2] = decimal(v-shrink-2:screen-value in frame {&frame-name})
         v-shrink[3] = decimal(v-shrink-3:screen-value in frame {&frame-name})
         v-shrink[4] = decimal(v-shrink-4:screen-value in frame {&frame-name})
         v-shrink[5] = decimal(v-shrink-5:screen-value in frame {&frame-name})
         v-shrink[6] = decimal(v-shrink-6:screen-value in frame {&frame-name})
         v-shrink[7] = decimal(v-shrink-7:screen-value in frame {&frame-name})
         v-shrink[8] = decimal(v-shrink-8:screen-value in frame {&frame-name}).

  IF v-lam-code:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-bom-item[9] = v-lam-code:screen-value in frame {&frame-name}
        v-bom-item[10] = v-adh-code:screen-value in frame {&frame-name}
        v-count = 10.

  do lv-bom-count = 1 to v-count:
    if v-bom-item[lv-bom-count] = "" then do:
       find item-bom where item-bom.company = item.company and
                           item-bom.parent-i = item.i-no and
                           item-bom.line# = lv-bom-count
                           no-error.
       if avail item-bom then delete item-bom.
       next.
    end.
    find item-bom where item-bom.company = item.company and
                         item-bom.parent-i = item.i-no and
                         item-bom.line# = lv-bom-count
                         no-error.
    find b-item where b-item.company = item.company and
                       b-item.i-no = v-bom-item[lv-bom-count] no-lock no-error.
     if not avail b-item then do:
        message "Bill Of Material found for a Invalid Item#. Not creating Item Bill"
                view-as alert-box warning.
        next.
     end.    
     if not avail item-bom then create item-bom.            
     assign item-bom.company = item.company
            item-bom.parent-i = item.i-no
            item-bom.i-no = b-item.i-no
            item-bom.line# = lv-bom-count
            item-bom.shrink = v-shrink[lv-bom-count].
  end.

  find b-item2 where recid(b-item2) = recid(item) exclusive-lock.
  assign b-item2.avg-cost = 0
         b-item2.last-cost = 0.
  for each item-bom where item-bom.company = b-item2.company and
                          item-bom.parent-i = b-item2.i-no no-lock:
      find b-item where b-item.company = item-bom.company and
                        b-item.i-no = item-bom.i-no no-lock no-error.
      assign b-item2.avg-cost = b-item2.avg-cost + b-item.avg-cost
             b-item2.last-cost = b-item2.last-cost + b-item.last-cost.
  end.       

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
  disable all with frame {&frame-name}.
  run local-display-fields.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable V-table-Win 
PROCEDURE local-disable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  disable all with frame {&frame-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-count AS INT INIT 8 NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
     v-bom-item = ""
     v-bom-dscr = ""
     v-shrink   = 0.

  DO WITH FRAME {&FRAME-NAME}:
     IF ITEM.mat-type = "B" THEN
        ASSIGN
           v-lam-code:HIDDEN = NO
           v-adh-code:HIDDEN = NO
           v-count = 10.
  END.

  do lv-bom-count = 1 to v-count:
     find item-bom where item-bom.company = item.company and
                         item-bom.parent-i = item.i-no and
                         item-bom.line# = lv-bom-count
                         no-error.
     if not avail item-bom then NEXT .

     find b-item where b-item.company = item-bom.company and
                       b-item.i-no = item-bom.i-no no-lock no-error.
     if not avail b-item then do:
        message "Bill Of Material found for a Invalid Item#. Deleting Item Bill"
                view-as alert-box warning.
        delete item-bom.
        NEXT.
     end.                
     assign v-bom-item[lv-bom-count] = item-bom.i-no
            v-bom-dscr[lv-bom-count] = b-item.i-name
            v-shrink[lv-bom-count] = item-bom.shrink.
  end.
  assign v-bom-1 = v-bom-item[1]
         v-bom-2 = v-bom-item[2]
         v-bom-3 = v-bom-item[3]
         v-bom-4 = v-bom-item[4]
         v-bom-5 = v-bom-item[5]
         v-bom-6 = v-bom-item[6]
         v-bom-7 = v-bom-item[7]
         v-bom-8 = v-bom-item[8]
         v-bom-dscr-1 = v-bom-dscr[1]
         v-bom-dscr-2 = v-bom-dscr[2]
         v-bom-dscr-3 = v-bom-dscr[3]
         v-bom-dscr-4 = v-bom-dscr[4]
         v-bom-dscr-5 = v-bom-dscr[5]
         v-bom-dscr-6 = v-bom-dscr[6]
         v-bom-dscr-7 = v-bom-dscr[7]
         v-bom-dscr-8 = v-bom-dscr[8]
         v-shrink-1 = v-shrink[1]
         v-shrink-2 = v-shrink[2]
         v-shrink-3 = v-shrink[3]
         v-shrink-4 = v-shrink[4]
         v-shrink-5 = v-shrink[5]
         v-shrink-6 = v-shrink[6]
         v-shrink-7 = v-shrink[7]
         v-shrink-8 = v-shrink[8].

  IF v-lam-code:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-lam-code = v-bom-item[9]
        v-adh-code = v-bom-item[10]
        v-bom-dscr-9 = v-bom-dscr[9]
        v-bom-dscr-10 = v-bom-dscr[10].

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if not lv-first then 
     enable all except item.i-no v-bom-dscr-1 v-bom-dscr-2
            v-bom-dscr-3 v-bom-dscr-4 
            v-bom-dscr-5 v-bom-dscr-6
            v-bom-dscr-7 v-bom-dscr-8 v-bom-dscr-9
            v-bom-dscr-10 with frame {&frame-name}.
  if lv-first then lv-first = no.
  apply "entry" to v-bom-1 in frame {&frame-name}.

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
  run dispatch ('enable').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.
  {&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  do with frame {&frame-name}:
     if v-bom-1:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq v-bom-1:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           apply "entry" to v-bom-1.
           return no-apply.
      end.        
     if v-bom-2:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq v-bom-2:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           apply "entry" to v-bom-2.
           return no-apply.
      end.        
     if v-bom-3:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq v-bom-3:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           apply "entry" to v-bom-3.
           return no-apply.
      end.        
     if v-bom-4:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq v-bom-4:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           apply "entry" to v-bom-4.
           return no-apply.
      end.        
     if v-bom-5:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq v-bom-5:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           apply "entry" to v-bom-5.
           return no-apply.
      end.        
     if v-bom-6:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq v-bom-6:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           apply "entry" to v-bom-6.
           return no-apply.
      end.        
     if v-bom-7:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq v-bom-7:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           apply "entry" to v-bom-7.
           return no-apply.
      end.
     if v-bom-8:screen-value <> "" and
      not can-find(first b-item
              where b-item.company  eq item.company
                and b-item.i-no     eq v-bom-8:screen-value
                and b-item.mat-type eq "P" /* Paper */ ) THEN
      do:
         message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
         apply "entry" to v-bom-8.
         return no-apply.
      end.
     if v-lam-code:HIDDEN = NO AND
        v-lam-code:screen-value <> "" and
         not can-find(first b-item
              where b-item.company  eq item.company
                 and b-item.i-no     eq v-lam-code:screen-value
                 and b-item.mat-type eq "L" ) THEN
     do:
        message " You MUST Enter a Valid Laminate Code or Blank" view-as alert-box error.
        apply "entry" to v-lam-code.
        return no-apply.
     end.
    if v-adh-code:HIDDEN = NO AND
       v-adh-code:screen-value <> "" and
        not can-find(first b-item
             where b-item.company  eq item.company
                and b-item.i-no     eq v-adh-code:screen-value
                and (b-item.mat-type eq "G" OR b-item.mat-type = "T")) THEN
      do:
         message " You MUST Enter a Valid Adhesive Code or Blank" view-as alert-box error.
         apply "entry" to v-adh-code.
         return no-apply.
      end.

  end. /* do */ 
  {&methods/lValidateError.i NO}
/*
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
*/

  /* Code placed here will execute AFTER standard behavior.    */
  run local-assign-record.
  disable all with frame {&frame-name}.
  run get-link-handle in adm-broker-hdl (this-procedure,"tableio-source", output char-hdl).
  run set-label in widget-handle(char-hdl) ("&Update").  
  run set-buttons in widget-handle(char-hdl) ("Initial").
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
  {src/adm/template/snd-list.i "item"}

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

