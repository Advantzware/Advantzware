/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/ 
/*------------------------------------------------------------------------
    File        : output.w
    Purpose     : Consultingwerk SmartWeb Widgets web request handler 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 04 21:41:22 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.*             FROM PROPATH . 
USING Consultingwerk.Util.*        FROM PROPATH . 
USING Consultingwerk.Web.*         FROM PROPATH . 
USING Consultingwerk.Web.Widgets.* FROM PROPATH . 

/* Create an unnamed pool to store all the widgets created 
by this procedure. This is a good default which assures
that this procedure's triggers and internal procedures
will execute in this procedure's storage, and that proper
cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no


/* ************************* Included-Libraries *********************** */

{src/web2/wrap-cgi.i}
{Consultingwerk/products.i}

/* ************************  Main Code Block  *********************** */

/* Process the latest Web event. */
RUN process-web-request.

/* **********************  Internal Procedures  *********************** */

PROCEDURE outputHeader :
/*------------------------------------------------------------------------------
  Purpose:     Output the MIME header, and any "cookie" information needed 
               by this procedure.  
  Parameters:  <none>
  Notes:       In the event that this Web object is state-aware, this is
               a good place to set the webState and webTimeout attributes.
------------------------------------------------------------------------------*/

  output-content-type ("text/html":U).

END PROCEDURE.
 
PROCEDURE OutputJavaScriptLibraries:
    /*------------------------------------------------------------------------------
        Purpose: Writes the references to the project JavaScriptLibraries to the 
                 WebStream                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER oPage AS Progress.Lang.Object NO-UNDO . 

    DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cLibraries AS CHARACTER NO-UNDO.

    IF SmartWeb:JavaScriptLibraries > "":U THEN 
        DO i = 1 TO NUM-ENTRIES (SmartWeb:JavaScriptLibraries) ON ERROR UNDO, THROW:
            
            {&out} SUBSTITUTE ('<script src="&1"></script>':U,
                               ENTRY (i, SmartWeb:JavaScriptLibraries)) SKIP .
            
        END.

    IF TYPE-OF (oPage, IPageWithCustomJavaScript) THEN DO:         
        ASSIGN cLibraries = CAST (oPage, IPageWithCustomJavaScript):CustomJavaScriptLibraries .

        IF cLibraries > "":U THEN 
            {&out} SKIP . 

        DO i = 1 TO NUM-ENTRIES (cLibraries) ON ERROR UNDO, THROW:
            
            {&out} SUBSTITUTE ('<script src="&1"></script>':U,
                               ENTRY (i, cLibraries)) SKIP .
            
        END.
    END.
        
END PROCEDURE.

PROCEDURE OutputStyleSheetFiles:
    /*------------------------------------------------------------------------------
        Purpose: Writes the references to the project StyleSheetFiles to the 
                 WebStream                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER oPage AS Progress.Lang.Object NO-UNDO . 

    DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cLibraries AS CHARACTER NO-UNDO.

    IF SmartWeb:JavaScriptLibraries > "":U THEN 
        DO i = 1 TO NUM-ENTRIES (SmartWeb:StyleSheetFiles) ON ERROR UNDO, THROW:
            
            {&out} SUBSTITUTE ('<link rel="stylesheet" type="text/css" href="&1">':U,
                               ENTRY (i, SmartWeb:StyleSheetFiles)) SKIP .
            
        END.
        
    IF TYPE-OF (oPage, IPageWithCustomStyleSheet) THEN DO:         
        ASSIGN cLibraries = CAST (oPage, IPageWithCustomStyleSheet):CustomStyleLibraries .

        IF cLibraries > "":U THEN 
            {&out} SKIP . 

        DO i = 1 TO NUM-ENTRIES (cLibraries) ON ERROR UNDO, THROW:
            
            {&out} SUBSTITUTE ('<link rel="STYLESHEET" type="text/css" href="&1">':U,
                               ENTRY (i, cLibraries)) SKIP .
            
        END.
    END.
        
END PROCEDURE.

PROCEDURE process-web-request :
  /*------------------------------------------------------------------------------
    Purpose:     Process the web request.
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/

  DEFINE VARIABLE oPage                  AS IPage                                           NO-UNDO . 
  DEFINE VARIABLE cPageName              AS CHARACTER                                       NO-UNDO .
  DEFINE VARIABLE oFooter                AS Consultingwerk.Web.IPageFooterHandler           NO-UNDO . 
  DEFINE VARIABLE cAdditionalStylesheet  AS CHARACTER                                       NO-UNDO.
  DEFINE VARIABLE oConfigurationProvider AS Consultingwerk.Framework.IConfigurationProvider NO-UNDO .
  
  &IF PROVERSION NE "10.2B" &THEN    
  /* Jsonbased config provider only from 11.x */
  oConfigurationProvider = {Consultingwerk/get-service.i 
                            Consultingwerk.Framework.IConfigurationProvider 
                            "NEW Consultingwerk.Framework.ConfigurationProvider ('.applicationsettings':U)"} .  
  
  
     
  &ENDIF
  
  IF VALID-OBJECT (oConfigurationProvider) THEN 
      cAdditionalStylesheet = oConfigurationProvider:GetValue ("SmartComponentsWebStylesheet":U).
  
  Consultingwerk.OERA.ServiceManager:StopAllBusinessServices() .

  oFooter = {Consultingwerk/get-service.i Consultingwerk.Web.IPageFooterHandler} .

  ASSIGN cPageName = REPLACE (get-user-field ("SmartWeb.Page":U), "/":U, ".":U) .

  /* 
   * Output the MIME header and set up the object as state-less or state-aware. 
   * This is required if any HTML is to be returned to the browser.
   */

  DO ON ERROR UNDO, THROW:
      oPage = DYNAMIC-NEW (cPageName) () . 

      CATCH err AS Progress.Lang.Error:
            Consultingwerk.Web.WebErrorHelper:ShowErrorMessage (err, "Error loading page.":U) .
            
            RETURN . 
      END CATCH.
  END.

  DO ON ERROR UNDO, THROW:
      /* Mike Fechner, Consultingwerk Ltd. 25.05.2012
         Stop here, when the page object does not need to process it's own output */
      IF NOT oPage:ProcessWebInput () THEN 
          RETURN .  

      CATCH err AS Progress.Lang.Error:
          Consultingwerk.Web.WebErrorHelper:ShowErrorMessage (err, "Error processing input.":U) .
          
          RETURN . 
      END CATCH.
  END.
   
   
  RUN outputHeader.

  {&OUT}
    '<!DOCTYPE html>':U SKIP 
    "<html>":U SKIP
    "<head>":U SKIP .
    
  IF oPage:Expires <> ? THEN 
    WebUtilities:OutputMetaHttpEquiv (NEW NameValuePair ("expires":U, 
                                                         STRING (oPage:Expires))) .      

  {&OUT}
    "<TITLE>":U html-encode (oPage:PageTitle) "</TITLE>":U SKIP
    
    '<link rel="stylesheet" type="text/css" href="/SmartComponentsWeb/js/SmartComponentsWeb/UI/stylesheet.css" title="Style">':U  SKIP  .

  RUN OutputStyleSheetFiles (oPage).
  
  &IF DEFINED (TelerikKendoUI) &THEN
    IF TYPE-OF (oPage, IPageWithCustomJavaScript) THEN
        IF NOT ListHelper:EntryIsInList ("/KendoUI/js/jquery.min.js",
                                         CAST (oPage, IPageWithCustomJavaScript):CustomJavaScriptLibraries) THEN  

  {&OUT}
    '<script src="/KendoUI/js/jquery.min.js"></script>':U SKIP .
  &ELSE
  {&OUT}
    '<script src="/SmartComponentsWeb/js/jquery.js"></script>':U SKIP .
  &ENDIF
  
  RUN OutputJavaScriptLibraries (oPage) .    
  
  {&OUT}
    '<script src="/SmartComponentsWeb/js/SmartComponentsWeb/UI/global.js"></script>':U SKIP
    '<script src="/SmartComponentsWeb/js/SmartComponentsWeb/UI/SmartRibbonBar.js"></script>':U SKIP
    '<script src="/SmartComponentsWeb/js/SmartComponentsWeb/UI/SmartDialog.js"></script>':U SKIP
    '<script src="/SmartComponentsWeb/js/SmartComponentsWeb/UI/SmartDataGrid.js"></script>':U SKIP
    '<script src="/SmartComponentsWeb/js/SmartComponentsWeb/UI/TaskBarMenu.js"></script>':U SKIP
    '<script src="/SmartComponentsWeb/js/load.js"></script>':U SKIP .

  {&OUT}

    SKIP  
    "</head>":U SKIP
    '<body onload="onLoad()">':U SKIP
   .

  {&OUT} SUBSTITUTE ('<form id="myForm":U action="&1":U method="post">':U, oPage:PostUrl) . 

  oPage:ProcessWebRequest () .

  IF VALID-OBJECT (oFooter) THEN 
      oFooter:OutputFooter() .

  {&OUT}
    "</form>":U SKIP 
    "</body>":U SKIP
    "</html>":U SKIP
    .

  CATCH err AS Progress.Lang.Error :
      Consultingwerk.Util.LogManager:WriteError (err) .
      
      Consultingwerk.Web.WebErrorHelper:ShowErrorMessage (err, "Error processing input.":U) .
  END CATCH.

  FINALLY:
      Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oPage) .
      
  END FINALLY.

END PROCEDURE.
