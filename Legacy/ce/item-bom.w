&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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

def var v-bom-item like item-bom.i-no extent 7 no-undo.
def var v-bom-dscr like item.i-name extent 7 no-undo.
def var v-shrink like item.shrink extent 7 no-undo.
def var lv-bom-count as int no-undo.
def buffer b-item for item.
def var lv-first as log init yes.

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
v-bom-dscr-6 v-shrink-6 v-bom-7 v-bom-dscr-7 v-shrink-7 

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
DEFINE VARIABLE v-bom-1 AS CHARACTER FORMAT "x(10)":U 
     LABEL "1.    Paper" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-2 AS CHARACTER FORMAT "x(10)":U 
     LABEL "2.    Paper" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-3 AS CHARACTER FORMAT "x(10)":U 
     LABEL "3.     Liner" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-4 AS CHARACTER FORMAT "x(10)":U 
     LABEL "4. Medium" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-5 AS CHARACTER FORMAT "x(10)":U 
     LABEL "5.     Liner" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-6 AS CHARACTER FORMAT "x(10)":U 
     LABEL "6. Medium" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-7 AS CHARACTER FORMAT "x(10)":U 
     LABEL "7.     Liner" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-1 AS CHARACTER FORMAT "x(30)":U 
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

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.8 BY 9.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     v-bom-1 AT ROW 2.67 COL 3.2
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
     item.i-no AT ROW 9.33 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RECT-22 AT ROW 1.24 COL 1
     "Item" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 19
     "Description" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.95 COL 39
     "Shrink (%)" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.95 COL 71
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       item.i-no:HIDDEN IN FRAME F-Main           = TRUE.

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
/* SETTINGS FOR FILL-IN v-bom-dscr-1 IN FRAME F-Main
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
                       .
        end.
        when "v-bom-2" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-2:screen-value = entry(2,char-val)
                       v-shrink-2:screen-value = entry(3,char-val)
                       .
        end.
        when "v-bom-3" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-3:screen-value = entry(2,char-val)
                       v-shrink-3:screen-value = entry(3,char-val)
                       .
        end.
        when "v-bom-4" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-4:screen-value = entry(2,char-val)
                       v-shrink-4:screen-value = entry(3,char-val)                       
                       .
        end.
        when "v-bom-5" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-5:screen-value = entry(2,char-val)
                       v-shrink-5:screen-value = entry(3,char-val)
                       .
        end.
        when "v-bom-6" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-6:screen-value = entry(2,char-val)
                       v-shrink-6:screen-value = entry(3,char-val)
                       .
        end.
        when "v-bom-7" then do:
             run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       v-bom-dscr-7:screen-value = entry(2,char-val)
                       v-shrink-7:screen-value = entry(3,char-val)
                       .
        end.

    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-1 V-table-Win
ON LEAVE OF v-bom-1 IN FRAME F-Main /* 1.    Paper */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq self:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
               
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " 
                  view-as alert-box error.
           return no-apply.
      end.        
              
              

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-2 V-table-Win
ON LEAVE OF v-bom-2 IN FRAME F-Main /* 2.    Paper */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq self:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
               
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           return no-apply.
      end.        

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-3 V-table-Win
ON LEAVE OF v-bom-3 IN FRAME F-Main /* 3.     Liner */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq self:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
               
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           return no-apply.
      end.        

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-4 V-table-Win
ON LEAVE OF v-bom-4 IN FRAME F-Main /* 4. Medium */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq self:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
               
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           return no-apply.
      end.        

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-5 V-table-Win
ON LEAVE OF v-bom-5 IN FRAME F-Main /* 5.     Liner */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq self:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
               
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           return no-apply.
      end.        

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-6 V-table-Win
ON LEAVE OF v-bom-6 IN FRAME F-Main /* 6. Medium */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq self:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
               
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           return no-apply.
      end.        

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-7 V-table-Win
ON LEAVE OF v-bom-7 IN FRAME F-Main /* 7.     Liner */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first b-item
               where b-item.company  eq item.company
                 and b-item.i-no     eq self:screen-value
                 and b-item.mat-type eq "P" /* Paper */ )
               
      then do:
           message " You MUST Enter a Valid Paper Item or Blank " view-as alert-box error.
           return no-apply.
      end.        

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
  /* Code placed here will execute PRIOR to standard behavior. */
/*
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
*/
  /* Code placed here will execute AFTER standard behavior.    */
 assign  v-bom-item[1] = v-bom-1:screen-value in frame {&frame-name}
         v-bom-item[2] = v-bom-2:screen-value in frame {&frame-name}
         v-bom-item[3] = v-bom-3:screen-value in frame {&frame-name}
         v-bom-item[4] = v-bom-4:screen-value in frame {&frame-name}
         v-bom-item[5] = v-bom-5:screen-value in frame {&frame-name}
         v-bom-item[6] = v-bom-6:screen-value in frame {&frame-name}
         v-bom-item[7] = v-bom-7:screen-value in frame {&frame-name}
         v-bom-dscr[1] = v-bom-dscr-1:screen-value in frame {&frame-name}
         v-bom-dscr[2] = v-bom-dscr-2:screen-value in frame {&frame-name}
         v-bom-dscr[3] = v-bom-dscr-3:screen-value in frame {&frame-name}
         v-bom-dscr[4] = v-bom-dscr-4:screen-value in frame {&frame-name}
         v-bom-dscr[5] = v-bom-dscr-5:screen-value in frame {&frame-name}
         v-bom-dscr[6] = v-bom-dscr-6:screen-value in frame {&frame-name}
         v-bom-dscr[7] = v-bom-dscr-7:screen-value in frame {&frame-name}
         v-shrink[1] = decimal(v-shrink-1:screen-value in frame {&frame-name})
         v-shrink[2] = decimal(v-shrink-2:screen-value in frame {&frame-name})
         v-shrink[3] = decimal(v-shrink-3:screen-value in frame {&frame-name})
         v-shrink[4] = decimal(v-shrink-4:screen-value in frame {&frame-name})
         v-shrink[5] = decimal(v-shrink-5:screen-value in frame {&frame-name})
         v-shrink[6] = decimal(v-shrink-6:screen-value in frame {&frame-name})
         v-shrink[7] = decimal(v-shrink-7:screen-value in frame {&frame-name})
         .

  do lv-bom-count = 1 to 7:
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
            item-bom.shrink = v-shrink[lv-bom-count]
            .  
      
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

  /* Code placed here will execute PRIOR to standard behavior. */
    do lv-bom-count = 1 to 7:
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
            v-shrink[lv-bom-count] = item-bom.shrink
            .  
              
  end.
  assign v-bom-1 = v-bom-item[1]
         v-bom-2 = v-bom-item[2]
         v-bom-3 = v-bom-item[3]
         v-bom-4 = v-bom-item[4]
         v-bom-5 = v-bom-item[5]
         v-bom-6 = v-bom-item[6]
         v-bom-7 = v-bom-item[7]
         v-bom-dscr-1 = v-bom-dscr[1]
         v-bom-dscr-2 = v-bom-dscr[2]
         v-bom-dscr-3 = v-bom-dscr[3]
         v-bom-dscr-4 = v-bom-dscr[4]
         v-bom-dscr-5 = v-bom-dscr[5]
         v-bom-dscr-6 = v-bom-dscr[6]
         v-bom-dscr-7 = v-bom-dscr[7]
         v-shrink-1 = v-shrink[1]
         v-shrink-2 = v-shrink[2]
         v-shrink-3 = v-shrink[3]
         v-shrink-4 = v-shrink[4]
         v-shrink-5 = v-shrink[5]
         v-shrink-6 = v-shrink[6]
         v-shrink-7 = v-shrink[7]
         .

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
     enable all except item.i-no with frame {&frame-name}.
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

  end. /* do */ 
  
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

