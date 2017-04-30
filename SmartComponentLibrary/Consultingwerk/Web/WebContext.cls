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
    File        : WebContext
    Purpose     : 
    Syntax      : 
    Description : See src/web/method/cgidefs.i
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 04 22:51:32 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.* FROM PROPATH . 
USING Progress.Lang.*      FROM PROPATH .

CLASS Consultingwerk.Web.WebContext: 

    /*------------------------------------------------------------------------------
        Purpose: Event raised at the beginning of every web request                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT BeginWebRequest SIGNATURE VOID ().

    /*------------------------------------------------------------------------------
        Purpose: Event raised at the end of every web request                                           
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT EndWebRequest SIGNATURE VOID ().

    /*  For more information on the CGI 1.1 and HTTP variables, see
        http://hoohoo.ncsa.uiuc.edu/cgi/env.html. */

    /* CGI 1.1 Variables */
    DEFINE PUBLIC STATIC PROPERTY GATEWAY_INTERFACE AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    DEFINE PUBLIC STATIC PROPERTY SERVER_SOFTWARE AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY SERVER_PROTOCOL AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY SERVER_NAME AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY SERVER_PORT AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY REQUEST_METHOD AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY SCRIPT_NAME AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY PATH_INFO AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY PATH_TRANSLATED AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY QUERY_STRING AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY REMOTE_ADDR AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY REMOTE_HOST AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY REMOTE_IDENT AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY REMOTE_USER AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY AUTH_TYPE AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY CONTENT_TYPE AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY CONTENT_LENGTH AS INTEGER NO-UNDO 
    GET.
    SET. 

    /* Common HTTP Header Variables */ 
    DEFINE PUBLIC STATIC PROPERTY HTTP_ACCEPT AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY HTTP_COOKIE AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY HTTP_REFERER AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY HTTP_USER_AGENT AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY HTTPS AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Name of the program we're about to run */
    DEFINE PUBLIC STATIC PROPERTY AppProgram AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Relative URL where the application directory is the root. */
    DEFINE PUBLIC STATIC PROPERTY AppURL AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Relative URL for ourself from the server root directory. */
    DEFINE PUBLIC STATIC PROPERTY SelfURL AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Host and port part of URL. */
    DEFINE PUBLIC STATIC PROPERTY HostURL AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Default path for set-cookie() function.  Normally the same as AppURL.
       Otherwise, obtained from the DefaultCookiePath configuration option. */
    DEFINE PUBLIC STATIC PROPERTY CookiePath AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Relative path for our static HTML, JAVA, DOC, and IMG file off the web
       server's DOCUMENT_ROOT directory.                                    */   
    DEFINE PUBLIC STATIC PROPERTY RootURL AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Messenger Server Connection ID is inactive/active (0/1)              */   
    DEFINE PUBLIC STATIC PROPERTY useConnID AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Default domain for the set-cookie() function.  Normally blank. */
    DEFINE PUBLIC STATIC PROPERTY CookieDomain AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Delimiter for multiple items assigned to a single field name.
       Typically a comma or tab. */
    DEFINE PUBLIC STATIC PROPERTY SelDelim AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Flag set by the output-content-type() function to the Content-Type header
       value that was output.  If this is blank, then the output-content-type()
       function has not been run yet which should imply no other output has
       been sent either. */
    DEFINE PUBLIC STATIC PROPERTY output-content-type AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Newline characters to use for the output-http-header() function.
       Some web servers do not allow the CR, LF combination but only support
       an LF character. */
    DEFINE PUBLIC STATIC PROPERTY http-newline AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /* Offset from Coordinated Universal Time (UTC) or GMT time in seconds.
       This value is updated with each request.  A positive number is west of
       UTC.  This is used to calculate UTC time when setting Cookie expiration
       dates in the set-cookie() function. */
    DEFINE PUBLIC STATIC PROPERTY utc-offset AS INTEGER NO-UNDO 
    GET.
    SET. 

    /* Handle for web utilities procedure. */
    DEFINE PUBLIC STATIC PROPERTY web-utilities-hdl AS HANDLE NO-UNDO 
    GET.
    SET. 

    /* Handle for webstream */
    DEFINE PUBLIC STATIC PROPERTY WebStream AS HANDLE NO-UNDO 
    GET.
    SET. 


    /*------------------------------------------------------------------------------
        Purpose: Clears the Web Context                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Clear ():
        
        WebContext:OnEndWebRequest () .
        
        ASSIGN WebContext:GATEWAY_INTERFACE   = "":U                    
               WebContext:SERVER_SOFTWARE     = "":U        
               WebContext:SERVER_PROTOCOL     = "":U        
               WebContext:SERVER_NAME         = "":U                
               WebContext:SERVER_PORT         = "":U                
               WebContext:REQUEST_METHOD      = "":U          
               WebContext:SCRIPT_NAME         = "":U                
               WebContext:PATH_INFO           = "":U                    
               WebContext:PATH_TRANSLATED     = "":U        
               WebContext:QUERY_STRING        = "":U              
               WebContext:REMOTE_ADDR         = "":U                
               WebContext:REMOTE_HOST         = "":U        
               WebContext:REMOTE_IDENT        = "":U              
               WebContext:REMOTE_USER         = "":U               
               WebContext:AUTH_TYPE           = "":U                    
               WebContext:CONTENT_TYPE        = "":U              
               WebContext:CONTENT_LENGTH      = 0          
               WebContext:HTTP_ACCEPT         = "":U                
               WebContext:HTTP_COOKIE         = "":U                
               WebContext:HTTP_REFERER        = "":U              
               WebContext:HTTP_USER_AGENT     = "":U        
               WebContext:HTTPS               = "":U                            
               WebContext:AppProgram          = "":U                  
               WebContext:AppURL              = "":U                          
               WebContext:SelfURL             = "":U                        
               WebContext:HostURL             = "":U                        
               WebContext:CookiePath          = "":U                  
               WebContext:RootURL             = "":U
               WebContext:useConnID           = "":U                    
               WebContext:CookieDomain        = "":U
               WebContext:SelDelim            = "":U                      
               WebContext:output-content-type = "":U 
               WebContext:http-newline        = "":U
               WebContext:utc-offset          = -1
               WebContext:web-utilities-hdl   = ?
               
               WebContext:WebStream           = ? 
               .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeginWebRequest event
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnBeginWebRequest ():
        
        WebContext:BeginWebRequest:Publish () .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the EndWebRequest event
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnEndWebRequest ():
        
        WebContext:EndWebRequest:Publish () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the Web Context from the GLOBAL SHARED VARIABLES                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Refresh ():
        
        RUN Consultingwerk/Web/Support/refresh-web-context.p .

        WebContext:OnBeginWebRequest () .

    END METHOD.


END CLASS.