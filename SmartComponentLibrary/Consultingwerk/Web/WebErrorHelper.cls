/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : WebErrorHelper
    Purpose     : Display error page in the web browser
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 11 17:09:22 CET 2014
    Notes       : Visualization is done using SpeedScript file error.html 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.* FROM PROPATH .  
USING Progress.Lang.*      FROM PROPATH .

CLASS Consultingwerk.Web.WebErrorHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the current error
        Notes:   Queried by error.html   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Error AS Progress.Lang.Error NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the current error title  
        Notes:   Queried by error.html   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Title AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WebErrorHelper class
        Notes:   Disallow instance creation   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WebErrorHelper ():
        SUPER ().

        ASSIGN WebErrorHelper:Error = ?
               WebErrorHelper:Title = ? .
                       
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows an error page in the web browser
        Notes:   
        @param poError The error object to visualize
        @param pcTitle The title for the message dialog
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowErrorMessage (poError AS Progress.Lang.Error,
                                                pcTitle AS CHARACTER):
        
        DEFINE VARIABLE web-utilities-hdl AS HANDLE NO-UNDO.
        
        ASSIGN WebErrorHelper:Error = poError
               WebErrorHelper:Title = pcTitle .  

               web-utilities-hdl    = WebContext:web-utilities-hdl . 

        RUN run-web-object IN web-utilities-hdl ("Consultingwerk/Web/error.html":U) .

        FINALLY:
            ASSIGN WebErrorHelper:Error = ?
                   WebErrorHelper:Title = ? .  
        END FINALLY.

    END METHOD .

END CLASS.
