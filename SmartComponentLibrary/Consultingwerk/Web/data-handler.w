/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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

    File:           data-handler.w
    Purpose: 
    
    Description: 
    
    Input Parameters:
    <none>
    
    Output Parameters:
    <none>
    
    Author: 
    
    Created: Thu Nov 13 23:17:07 CET 2014

------------------------------------------------------------------------*/

USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.Web.* FROM PROPATH.
USING Consultingwerk.Web.Data.* FROM PROPATH.
USING Consultingwerk.SmartComponentsDemo.Web.* FROM PROPATH.

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no

/* ************************* Included-Libraries *********************** */

{src/web2/wrap-cgi.i}

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

  output-content-type ("application/json":U).

END PROCEDURE.

PROCEDURE process-web-request :
/*------------------------------------------------------------------------------
  Purpose:     Process the web request.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cDataObject AS CHARACTER      NO-UNDO .
  DEFINE VARIABLE oDataSource AS JsonDataSource NO-UNDO . 

  LogManager:WriteFormattedMessage ("QUERY_STRING: &1":U,
                                    WebContext:QUERY_STRING) .
  LogManager:WriteFormattedMessage ("Fields: &1":U,
                                    WebUtilities:GetField (?)) .

  RUN outputHeader.

  ASSIGN cDataObject = REPLACE (WebUtilities:GetUserField ("JsonData.Source":U), "/":U, ".":U) .

  oDataSource = DYNAMIC-NEW (cDataObject) () . 
 
  IF VALID-OBJECT (oDataSource) THEN DO:
      oDataSource:FetchData() .
      oDataSource:OutputTable() .
  END.

  CATCH err AS Progress.Lang.Error:
      LogManager:WriteError (err) .    
  END CATCH.

END PROCEDURE.


