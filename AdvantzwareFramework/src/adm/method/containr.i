&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r2 GUI
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*-------------------------------------------------------------------------
    Library     : containr.i  
    Purpose     : Default Main Block code and Method Procedures
                  for UIB-generated ADM Container procedures.
  
    Syntax      : {src/adm/method/containr.i}

    Description :
  
    Author(s)   :
    Created     :
    HISTORY:
-------------------------------------------------------------------------*/
/***********************  DEFINITIONS  ***********************************/

DEFINE VARIABLE iWinKitTabInitialized AS INTEGER INIT 0 NO-UNDO.

&GLOBAL-DEFINE ADM-DISPATCH-QUALIFIER winkit


&IF DEFINED(adm-containr) = 0 &THEN
&GLOB adm-containr yes

/* Dialog program to run to set runtime attributes - if not defined in master */
&IF DEFINED(adm-attribute-dlg) = 0 &THEN
&SCOP adm-attribute-dlg adm/support/contnrd.w
&ENDIF

/* +++ This is the list of attributes whose values are to be returned
   by get-attribute-list, that is, those whose values are part of the
   definition of the object instance and should be passed to init-object
   by the UIB-generated code in adm-create-objects. */
&IF DEFINED(adm-attribute-list) = 0 &THEN
&SCOP adm-attribute-list Layout,Hide-on-Init
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 1.5
         WIDTH              = 38.43.
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/* Initialize page number and object handle attributes. */
RUN set-attribute-list ("CURRENT-PAGE=0,ADM-OBJECT-HANDLE=":U +
    STRING(adm-object-hdl)). 

/* Best default for GUI applications - this will apply to the whole session: */
PAUSE 0 BEFORE-HIDE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-adm-change-page) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-change-page Method-Library 
PROCEDURE adm-change-page :
/* -----------------------------------------------------------
      Purpose:    Views objects on a newly selected page, initializing
                  them if the page has not yet been seen.
      Parameters: <none>
      Notes:      In character mode, when switching from the main window
                  to a page which is another window (in GUI), the
                  main window's default frame must be hidden; and when
                  returning it must be viewed. This is done below.
    -------------------------------------------------------------*/   

  RUN broker-change-page IN adm-broker-hdl (INPUT THIS-PROCEDURE) NO-ERROR.


  &IF "{&WinKitFormType}" EQ "Advantzware.WinKit.Forms.EmbeddedWindowTabFolderForm" &THEN

  DEFINE VARIABLE i     AS INTEGER NO-UNDO.
  DEFINE VARIABLE iPage AS INTEGER NO-UNDO.

  IF VALID-OBJECT (oFormControl) THEN DO:

      RUN GET-ATTRIBUTE ("current-page") . 
    
      ASSIGN iPage = Consultingwerk.Util.DataTypeHelper:ToInteger 
                          (RETURN-VALUE) .

      oFormControl:FinalizeTabPage (iPage) .
    
      /* Mike Fechner, Consultingwerk Ltd. 11.11.2010
         Set FRAME BGCOLOR to 33, which matches the Form Background color
         of Office 2007 applications, color33=227,239,255 */
      Consultingwerk.Util.WidgetHelper:SetFrameBackgroundColor
              (oFormControl:GetTabPageWindow (iPage), 
               Consultingwerk.WindowIntegrationKit.Forms.WinKitForms:TabFolderBGColor, 
               15, 
               15) .
  END.

  &ENDIF




  
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-page) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-page Method-Library 
PROCEDURE delete-page :
/* -----------------------------------------------------------
      Purpose:     Destroys all objects on the current page.
      Parameters:  INPUT page number
      Notes:       
    -------------------------------------------------------------*/   
  
  DEFINE INPUT PARAMETER p-page# AS INTEGER NO-UNDO.
  
  RUN broker-delete-page IN adm-broker-hdl 
      (INPUT THIS-PROCEDURE, INPUT p-page#).
  
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-init-object) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-object Method-Library 
PROCEDURE init-object :
/* -----------------------------------------------------------
   Purpose:     RUNS an object procedure PERSISTENT and initializes
                default links
   Parameters:  INPUT procedure name, parent handle, attribute-list,
                OUTPUT procedure handle
   Notes:       init-object calls are generated by the UIB 
                in adm-create-objects
-------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER  p-proc-name   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER  p-parent-hdl  AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER  p-attr-list   AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p-proc-hdl    AS HANDLE    NO-UNDO.

  /* Mike Fechner, Consultingwerk Ltd. 06.02.2016
     WinKit: When a SmartObject is loaded for a Page > 0, we 
     will send that to the default form of the dynamically 
     created window created in the .NET TabFolder */
  &IF "{&WinKitFormType}" EQ "Advantzware.WinKit.Forms.EmbeddedWindowTabFolderForm" &THEN

  DEFINE VARIABLE iPage AS INTEGER NO-UNDO.
  DEFINE VARIABLE iCreatePage AS INTEGER NO-UNDO.
  DEFINE VARIABLE hTabFrame AS HANDLE NO-UNDO.

  RUN GET-ATTRIBUTE ("current-page") . 

  ASSIGN iPage = Consultingwerk.Util.DataTypeHelper:ToInteger 
                      (RETURN-VALUE) .

  IF VALID-OBJECT (oFormControl) THEN DO:
      IF iPage > 0 THEN DO:
    
          DO iCreatePage = iWinKitTabInitialized + 1 TO iPage:
              oFormControl:CreateTab (SUBSTITUTE ("Tab &1", iCreatePage),
                                      iPage) .
          END. 
          
          ASSIGN iWinKitTabInitialized = MAX (iWinKitTabInitialized, iPage) .
          
          ASSIGN hTabFrame = oFormControl:GetTabPageFrame (iPage) .
          
          IF VALID-HANDLE (hTabFrame) THEN 
              ASSIGN p-parent-hdl = hTabFrame .
          ELSE 
              
            MESSAGE p-proc-name SKIP 
                    p-attr-list SKIP (2)
                    "Page" iPage
                VIEW-AS ALERT-BOX.
      END.
  END.  
  
  &ENDIF


  RUN broker-init-object IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-proc-name, INPUT p-parent-hdl,
       INPUT p-attr-list, OUTPUT p-proc-hdl) NO-ERROR.
       
       
  /* Mike Fechner, Consultingwerk Ltd. 06.02.2016
     When we've inited a SmartPanel, call into this to 
     create a Ribbon Group for it */
  &IF "{&WinKitFormType}" EQ "Advantzware.WinKit.Forms.EmbeddedWindowTabFolderForm" &THEN
           
  IF Consultingwerk.Util.ProcedureHelper:HasEntry (p-proc-hdl, "winkit-make-ribbon-group") THEN 
      RUN winkit-make-ribbon-group IN p-proc-hdl 
           (oForm, iPage) .

  &ENDIF         
         
       
       
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-init-pages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-pages Method-Library 
PROCEDURE init-pages :
/* -----------------------------------------------------------
      Purpose:     Initializes one or more pages in a paging
                   control without actually viewing them. 
                   This can be used either for initializing pages
                   at startup without waiting for them to be
                   selected, or for creating additional or
                   replacement pages after startup.
      Parameters:  INPUT comma-separated list of page numbers
      Notes:       Generally this method does not need to be used,
                   unless the user specifically wants to incur the
                   overhead of creating and initializing pages before
                   they are first viewed. When one page in a multi-page
                   SmartContainer has a SmartLink dependency on another
                   page, the UIB will automatically generate the calls
                   to init-pages to assure that the right other pages have been
                   initialized when a page is selected for the first time.
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page-list      AS CHARACTER NO-UNDO.  

  RUN broker-init-pages IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page-list) NO-ERROR.
      
  /* Mike Fechner, Consultingwerk Ltd. 06.02.2016
     WinKit: Calls the FinalizeTabPage method for each 
     inited tab page. This method will actually show the 
     .NET Tab Page and align the Smarties with the top left 
     corner */

  &IF "{&WinKitFormType}" EQ "Advantzware.WinKit.Forms.EmbeddedWindowTabFolderForm" &THEN

  DEFINE VARIABLE i     AS INTEGER NO-UNDO.
  DEFINE VARIABLE iPage AS INTEGER NO-UNDO.

  IF VALID-OBJECT (oFormControl) THEN DO:
      DO i = 1 TO NUM-ENTRIES (p-page-list):    

          ASSIGN iPage = INTEGER (ENTRY (i, p-page-list)) .        

          oFormControl:FinalizeTabPage (iPage) .
        
          /* Mike Fechner, Consultingwerk Ltd. 11.11.2010
             Set FRAME BGCOLOR to 33, which matches the Form Background color
             of Office 2007 applications, color33=227,239,255 */
          Consultingwerk.Util.WidgetHelper:SetFrameBackgroundColor
                  (oFormControl:GetTabPageWindow (iPage), 
                   Consultingwerk.WindowIntegrationKit.Forms.WinKitForms:TabFolderBGColor, 
                   15, 
                   15) .
    END.
  END.

  &ENDIF
      
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-select-page) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-page Method-Library 
PROCEDURE select-page :
/* -----------------------------------------------------------
      Purpose:     Makes a new set of objects associated with a page number
                   current, by hiding the previous page, if any,
                   creating the objects in the new page if the page hasn't
                   been initialized, and viewing the new page.
      Parameters:  INPUT new page number
      Notes:       
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page#     AS INTEGER   NO-UNDO.

  RUN broker-select-page IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page#) NO-ERROR.
    
  /* Mike Fechner, Consultingwerk Ltd. 06.02.2016
     WinKit: When code selects a different tab page, 
     ensure the .NET side reflects that. */
  &IF "{&WinKitFormType}" EQ "Advantzware.WinKit.Forms.EmbeddedWindowTabFolderForm" &THEN

  IF VALID-OBJECT (oFormControl) THEN DO:

/*          oFormControl:FinalizeTabPage (p-page#) .                                        */
/*                                                                                          */
/*          /* Mike Fechner, Consultingwerk Ltd. 11.11.2010                                 */
/*             Set FRAME BGCOLOR to 33, which matches the Form Background color             */
/*             of Office 2007 applications, color33=227,239,255 */                          */
/*          Consultingwerk.Util.WidgetHelper:SetFrameBackgroundColor                        */
/*                  (oFormControl:GetTabPageWindow (p-page#),                               */
/*                   Consultingwerk.WindowIntegrationKit.Forms.WinKitForms:TabFolderBGColor,*/
/*                   15,                                                                    */
/*                   15) .                                                                  */

      oFormControl:SelectTab (p-page#) . 

   END.

  &ENDIF    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-view-page) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE view-page Method-Library 
PROCEDURE view-page :
/* -----------------------------------------------------------
      Purpose:     Makes a new set of objects associated with a page number
                   current, without hiding the previous page, if any,
                   creating the objects in the new page if the page hasn't
                   been initialized, and viewing the new page.
      Parameters:  INPUT new page number
      Notes:       This method does not reset the value of adm-current-page,
                   because the new page is being viewed without hiding the
                   old one. adm-current-page is the most recently "selected"
                   page.
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page#      AS INTEGER   NO-UNDO.

  RUN broker-view-page IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page#).
       
  END PROCEDURE.

&IF DEFINED(EXCLUDE-winkit-destroy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winkit-destroy Method-Library
PROCEDURE winkit-destroy:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ("destroy") .

    IF VALID-OBJECT (oForm) AND TYPE-OF (oForm, Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm) THEN 
        CAST (oForm, Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm):CloseTabs () .     

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-winkit-initialize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winkit-initialize Method-Library
PROCEDURE winkit-initialize:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ("initialize") .

    &IF DEFINED (WinKitFormType) NE 0 &THEN
    Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager) .
    &ENDIF
        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


/* This ENDIF statement needs to stay here (or with the last procedure in the
   include file) to balance the &IF adm-container at the top: */
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

