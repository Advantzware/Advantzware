&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: est\dAddEditComp.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

{jc/ttMultiSelectItem.i}

/* PARAMs Definitions ---                                           */ 
DEFINE OUTPUT PARAMETER TABLE FOR ttMultiSelectItem .
{methods/defines/hndldefs.i}
{custom/globdefs.i}     
{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSelectTrigger           AS LOGICAL NO-UNDO.

/*{Inventory/ttInventory.i "NEW SHARED"}  */

/*{sys/inc/f16to32.i}*/
{sys/inc/lastship.i}



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog 
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttMultiSelectItem

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttMultiSelectItem.isSelect ttMultiSelectItem.fgItem ttMultiSelectItem.onHand ttMultiSelectItem.onOrder ttMultiSelectItem.allocated ttMultiSelectItem.qtyAvail ttMultiSelectItem.availOnHand ttMultiSelectItem.dueDate ttMultiSelectItem.reOrderLevel ttMultiSelectItem.minOrder ttMultiSelectItem.maxOrder ttMultiSelectItem.custPart ttMultiSelectItem.Customer ttMultiSelectItem.custName ttMultiSelectItem.estNo ttMultiSelectItem.style ttMultiSelectItem.cat   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 ttMultiSelectItem.isSelect   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 ttMultiSelectItem
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 ttMultiSelectItem
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttMultiSelectItem
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttMultiSelectItem.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttMultiSelectItem
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttMultiSelectItem


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-39 btExit btFilter cFGItem cStyle btOk ~
cCat cLoc BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS fieventIDlb fiStyleLebel cFGItem cStyle ~
fiCatLabel filocLabel cCat cLoc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 11 BY 2.62.

DEFINE BUTTON btFilter 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "Filter" 
     SIZE 9 BY 2.14.

DEFINE BUTTON btOk 
     LABEL "OK" 
     SIZE 15 BY 1.14
     FONT 6.

DEFINE VARIABLE cCat AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cFGItem AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 22.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cLoc AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cStyle AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiCatLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Category" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fieventIDlb AS CHARACTER FORMAT "X(256)":U INITIAL "FG Item" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE filocLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiStyleLebel AS CHARACTER FORMAT "X(256)":U INITIAL "Style" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 140.8 BY 3.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttMultiSelectItem SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      ttMultiSelectItem.isSelect COLUMN-LABEL "[ ] All" 
            WIDTH 8 VIEW-AS TOGGLE-BOX
      ttMultiSelectItem.fgItem COLUMN-LABEL "FG Item" FORMAT "x(15)":U
            WIDTH 23
      ttMultiSelectItem.onHand COLUMN-LABEL "On Hand" FORMAT "->>,>>>,>>9":U
            WIDTH 13
      ttMultiSelectItem.onOrder COLUMN-LABEL "On Order" FORMAT "->>,>>>,>>9":U
            WIDTH 15  
      ttMultiSelectItem.allocated COLUMN-LABEL "Allocated" FORMAT "->>,>>>,>>9":U
            WIDTH 15
      ttMultiSelectItem.qtyAvail COLUMN-LABEL "Available" FORMAT "->>,>>>,>>9":U
            WIDTH 15
      ttMultiSelectItem.availOnHand COLUMN-LABEL "Available On-Hand" FORMAT "->>,>>>,>>9":U
            WIDTH 22              
      ttMultiSelectItem.dueDate COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
            WIDTH 15  
      ttMultiSelectItem.reOrderLevel COLUMN-LABEL "Reorder Level" FORMAT "->>,>>>,>>9":U
            WIDTH 17
      ttMultiSelectItem.minOrder COLUMN-LABEL "Minimum Order" FORMAT "->>,>>>,>>9":U
            WIDTH 19
      ttMultiSelectItem.maxOrder COLUMN-LABEL "Maximum Order" FORMAT "->>,>>>,>>9":U
            WIDTH 19        
      ttMultiSelectItem.custPart COLUMN-LABEL "Customer Part" FORMAT "x(15)":U
            WIDTH 23
      ttMultiSelectItem.Customer COLUMN-LABEL "Customer" FORMAT "x(8)":U
            WIDTH 11
      ttMultiSelectItem.custName COLUMN-LABEL "Cust Name" FORMAT "x(30)":U
            WIDTH 33
      ttMultiSelectItem.estNo COLUMN-LABEL "Estimate" FORMAT "x(8)":U
            WIDTH 10
      ttMultiSelectItem.style COLUMN-LABEL "Style" FORMAT "x(8)":U
            WIDTH 10             
      ttMultiSelectItem.cat COLUMN-LABEL "Category" FORMAT "x(8)":U
            WIDTH 10

      ENABLE ttMultiSelectItem.isSelect
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 157.6 BY 22.38
         FONT 34 ROW-HEIGHT-CHARS 0.9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    
     fieventIDlb AT ROW 1.76 COL 7.2 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fiStyleLebel AT ROW 1.76 COL 71.6 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     cFGItem AT ROW 2.81 COL 7.2 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     cCat AT ROW 2.81 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     cLoc AT ROW 2.81 COL 51.2 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     cStyle AT ROW 2.81 COL 71.6 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     fiCatLabel AT ROW 1.76 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     filocLabel AT ROW 1.76 COL 51.2 COLON-ALIGNED NO-LABEL WIDGET-ID 76      
     btFilter AT ROW 1.71 COL 100.4 WIDGET-ID 18
     btOk AT ROW 2.81 COL 124 WIDGET-ID 24
     BROWSE-2 AT ROW 4.81 COL 2 WIDGET-ID 200     
     btExit AT ROW 1.24 COL 149 WIDGET-ID 326
     " Filter" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1 COL 3.6 WIDGET-ID 52
          FONT 6
     RECT-39 AT ROW 1.19 COL 1.2 WIDGET-ID 2
     SPACE(19.79) SKIP(22.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Multiple Select Item".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i} 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME   


/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-2 cLoc Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       btOk:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "NoWinReSize".
       BROWSE-2:NUM-LOCKED-COLUMNS IN FRAME Dialog-Frame     = 1 .               

/* SETTINGS FOR FILL-IN fiCatLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fieventIDlb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filocLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStyleLebel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       RECT-39:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttMultiSelectItem
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Multiple Selet Item */
DO:
        DEFINE VARIABLE char-val   AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID  NO-UNDO .
        
        CASE FOCUS:NAME :
            /*WHEN "cStock" THEN 
                DO:
                    RUN windows/l-itemfg.w  (cocode,"",cStock:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> ""  THEN 
                    DO:
                        cStock:SCREEN-VALUE  = ENTRY(1,char-val).
                        APPLY "entry" TO cStock.                  
                    END.    
                END.  */
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Multiple Selet Item */
DO:
        FOR EACH ttMultiSelectItem:
            ttMultiSelectItem.isSelect = NO.
        END.      
        
        APPLY "END-ERROR":U TO SELF.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME Dialog-Frame
DO:
    IF AVAILABLE ttMultiSelectItem THEN DO:
        /*RUN api/ResponseInboundDataViewer.w (
            INPUT ttMultiSelectItem.apiInboundEventID
            ).   */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON START-SEARCH OF BROWSE-2 IN FRAME Dialog-Frame
DO:
    IF SELF:CURRENT-COLUMN:NAME EQ "isSelect" THEN DO:
        lSelectTrigger = NOT lSelectTrigger.

        FOR EACH ttMultiSelectItem:
            ttMultiSelectItem.isSelect = lSelectTrigger.
        END.

        SELF:CURRENT-COLUMN:LABEL = IF lSelectTrigger THEN
                                        "[*] All"
                                    ELSE
                                        "[ ] All".

        RUN repo-query.  
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit Dialog-Frame
ON CHOOSE OF btExit IN FRAME Dialog-Frame /* Exit */
DO:    
    FOR EACH ttMultiSelectItem:
            ttMultiSelectItem.isSelect = NO.
    END.  
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFilter Dialog-Frame
ON CHOOSE OF btFilter IN FRAME Dialog-Frame /* Filter */
DO:
    DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData AS LONGCHAR  NO-UNDO.

     EMPTY TEMP-TABLE ttMultiSelectItem.
          
     DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&DISPLAYED-OBJECTS}.    
     END.
     
     FOR EACH itemfg  NO-LOCK
         WHERE itemfg.company    eq cocode              
         and (itemfg.i-no EQ cFGItem OR cFGItem EQ "")         
         and (itemfg.procat EQ cCat OR cCat EQ "")
         AND (itemfg.style EQ cStyle OR cStyle EQ "")
         AND (itemfg.loc LE cLoc OR cLoc EQ "") :

         CREATE ttMultiSelectItem. 
         ASSIGN
            ttMultiSelectItem.fgItem       = itemfg.i-no
            ttMultiSelectItem.custPart     = itemfg.part-no
            ttMultiSelectItem.Customer     = itemfg.cust-no
            ttMultiSelectItem.custName     = itemfg.cust-name
            ttMultiSelectItem.estNo        = itemfg.est-no
            ttMultiSelectItem.style        = itemfg.style
            ttMultiSelectItem.cat          = itemfg.procat
            ttMultiSelectItem.onHand       = itemfg.q-onh
            ttMultiSelectItem.onOrder      = itemfg.q-ono
            ttMultiSelectItem.allocated    = itemfg.q-alloc
            ttMultiSelectItem.qtyAvail     = itemfg.q-avail
            ttMultiSelectItem.availOnHand  = itemfg.q-onh - itemfg.q-alloc           
            ttMultiSelectItem.reOrderLevel = itemfg.ord-level
            ttMultiSelectItem.minOrder     = itemfg.ord-max
            ttMultiSelectItem.maxOrder     = itemfg.ord-min    .

            FIND FIRST cust NO-LOCK
                 WHERE cust.company EQ cocode 
                 AND cust.cust-no EQ itemfg.cust-no 
                 AND cust.cust-no NE "" NO-ERROR.
             IF avail cust THEN    
             ASSIGN ttMultiSelectItem.dueDate = TODAY + cust.ship-days.

            IF  lastship-cha = "Stock/Custom" THEN DO:
             /* If fgitem has no estimate. */
              IF itemfg.est-no = "" THEN
                 ASSIGN ttMultiSelectItem.dueDate = TODAY + lastship-int.
              ELSE
                 ASSIGN ttMultiSelectItem.dueDate = TODAY + INT(lastship-dec).
            END.
            IF lastship-cha eq "Fibre" then
            assign             
             ttMultiSelectItem.dueDate  = TODAY + (lastship-int * 1).               
     END.                 
   
     RUN repo-query.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOk Dialog-Frame
ON CHOOSE OF btOk IN FRAME Dialog-Frame /* OK */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCat Dialog-Frame
ON HELP OF cCat IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.

    RUN system/openlookup.p (
        g_company, 
        "procat", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        cCat:SCREEN-VALUE = lookupField.         

    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFGItem Dialog-Frame
ON HELP OF cFGItem IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.

    RUN system/openlookup.p (
        g_company, 
        "i-no", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        cFGItem:SCREEN-VALUE = lookupField.         

    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpw.i} 
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:      
     
        
     RUN enable_UI.
     {methods/nowait.i} 
       
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
       WAIT-FOR CLOSE OF THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY fieventIDlb fiStyleLebel cFGItem cStyle fiCatLabel filocLabel cCat 
          cLoc 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-39 btExit btFilter cFGItem cStyle btOk cCat cLoc BROWSE-2 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit Dialog-Frame 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
                 Purpose:
                 Notes:
                ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query Dialog-Frame 
PROCEDURE repo-query :

CLOSE QUERY BROWSE-2.
DO WITH FRAME {&FRAME-NAME}:
     
    OPEN QUERY BROWSE-2 FOR EACH ttMultiSelectItem
        NO-LOCK BY ttMultiSelectItem.fgItem.          
END.    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    
