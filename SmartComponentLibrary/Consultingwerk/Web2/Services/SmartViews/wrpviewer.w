/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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

    File: wrpviewer.w
    Purpose: 
    
    Description: 
    
    Input Parameters:
    <none>
    
    Output Parameters:
    <none>
    
    Author: 
    
    Created: Sun Jul 19 14:48:48 CEST 2015

------------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
   by this procedure. This is a good default which assures
   that this procedure's triggers and internal procedures
   will execute in this procedure's storage, and that proper
   cleanup will occur on deletion of the procedure. */

USING Consultingwerk.Web.* FROM PROPATH.
USING Consultingwerk.Web2.Services.SmartViews.* FROM PROPATH.

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no

/* *********************** Procedure Settings ************************ */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB)
  CREATE WINDOW Procedure ASSIGN
        HEIGHT             = 14.15
        WIDTH              = 60.57.
/* END WINDOW DEFINITION */
                                                                        */


/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* Custom CGI Wrapper Procedure,wdt,49681
Destroy on next read */


/* ************************* Included-Libraries *********************** */

{src/web2/wrap-cgi.i}

/* ************************  Main Code Block  *********************** */

/* Process the latest Web event. */
RUN process-web-request.


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-outputHeader) = 0 &THEN


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


&ENDIF


&IF DEFINED(EXCLUDE-process-web-request) = 0 &THEN
 
PROCEDURE process-web-request :
/*------------------------------------------------------------------------------
  Purpose:     Process the web request.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cViewer AS CHARACTER NO-UNDO .
  DEFINE VARIABLE oViewer AS Viewer    NO-UNDO .  
  DEFINE VARIABLE cScope  AS CHARACTER NO-UNDO .
  DEFINE VARIABLE lcJson  AS LONGCHAR  NO-UNDO . 
    
  oViewer = NEW Viewer () . 
  
  ASSIGN cViewer = WebUtilities:GetUserField ("SmartWeb.SmartViewer":U)
         cScope  = WebUtilities:GetField ("scope":U) .

  oViewer:GetFragment (ENTRY (2, cViewer, "/":U),
                       ENTRY (3, cViewer, "/":U),
                       cScope,
                       OUTPUT lcJson)  .

  /* 
   * Output the MIME header and set up the object as state-less or state-aware. 
   * This is required if any HTML is to be returned to the browser.
   */
  RUN outputHeader.

  {&OUT} STRING (lcJson) .
 
END PROCEDURE.




&ENDIF
