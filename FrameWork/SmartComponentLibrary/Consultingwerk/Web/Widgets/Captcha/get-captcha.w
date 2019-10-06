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

    File:    get-captcha.w
    Purpose: Generates a Captcha image
    
    Description: Not supported without SmartFramework / SmartDB 

    Input Parameters:
    <none>
    
    Output Parameters:
    <none>
    
    Author: Mike Fechner / Consultingwerk Ltd.
    
    Created: Fri Jun 01 21:23:25 CEST 2012
------------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
by this procedure. This is a good default which assures
that this procedure's triggers and internal procedures
will execute in this procedure's storage, and that proper
cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */

{Consultingwerk/products.i}

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no


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

  output-content-type ("image/png":U).

END PROCEDURE.

&ENDIF

&IF DEFINED(EXCLUDE-process-web-request) = 0 &THEN
 
PROCEDURE process-web-request :
/*------------------------------------------------------------------------------
  Purpose:     Process the web request.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  &IF DEFINED (DotNetAccessible) NE 0 &THEN

    DEFINE VARIABLE oCaptcha      AS Consultingwerk.Web.Win.Runtime.CaptchaImage NO-UNDO . 
    DEFINE VARIABLE cGuid         AS CHARACTER                                   NO-UNDO .
    DEFINE VARIABLE cString       AS CHARACTER                                   NO-UNDO .
    DEFINE VARIABLE oMemoryStream AS System.IO.MemoryStream                      NO-UNDO .
    DEFINE VARIABLE oBytes        AS "System.Byte[]":U                           NO-UNDO . 
    DEFINE VARIABLE rRaw          AS RAW                                         NO-UNDO . 
    DEFINE VARIABLE memptr        AS MEMPTR                                      NO-UNDO .  

  &IF DEFINED (SmartFramework) NE 0 &THEN        
    DEFINE BUFFER WebCaptcha FOR WebCaptcha .  

  /* 
   * Output the MIME header and set up the object as state-less or state-aware. 
   * This is required if any HTML is to be returned to the browser.
   */
   
  RUN outputHeader.
      
  ASSIGN cGuid = Consultingwerk.Web.WebUtilities:GetField ("Id":U) .
  
  FIND WebCaptcha WHERE WebCaptcha.CaptchaGuid = cGuid NO-LOCK NO-ERROR .
  IF AVAILABLE WebCaptcha THEN 
      ASSIGN cString = WebCaptcha.CaptchaValue .
  ELSE 
      ASSIGN cString = "?????":U .  
  
  oCaptcha = NEW Consultingwerk.Web.Win.Runtime.CaptchaImage (cString, 200, 60) .
  
  oMemoryStream = NEW System.IO.MemoryStream () . 
  
  oCaptcha:Image:Save (oMemoryStream, System.Drawing.Imaging.ImageFormat:Png) .
  
  oBytes = oMemoryStream:ToArray() .
  
  memptr = Consultingwerk.Util.DataTypeHelper:ByteArrayToMemptr (oBytes) . 
  
  PUT-BYTES (rRaw, 1) = memptr .

  PUT STREAM WebStream CONTROL rRaw .

  oCaptcha:Image:Dispose () .
  oCaptcha:Dispose () .
  
  DELETE OBJECT oBytes .
  DELETE OBJECT oMemoryStream .
  
  FINALLY:
      SET-SIZE (memptr) = 0 .        
  END FINALLY.

  &ELSE
  UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("process-web-request":U,
                                                                   THIS-PROCEDURE:FILE-NAME) .
  &ENDIF

  &ELSE
  UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("process-web-request":U,
                                                                   THIS-PROCEDURE:FILE-NAME) .
  &ENDIF
END PROCEDURE.

&ENDIF
