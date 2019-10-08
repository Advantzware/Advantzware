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
    File        : refresh-web-context.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 04 23:16:27 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.* FROM PROPATH . 

/* ********************  Preprocessor Definitions  ******************** */

{ src/web/method/cgidefs.i }

/* ***************************  Main Block  *************************** */

ASSIGN WebContext:GATEWAY_INTERFACE   = GATEWAY_INTERFACE                    
       WebContext:SERVER_SOFTWARE     = SERVER_SOFTWARE        
       WebContext:SERVER_PROTOCOL     = SERVER_PROTOCOL        
       WebContext:SERVER_NAME         = SERVER_NAME                
       WebContext:SERVER_PORT         = SERVER_PORT                
       WebContext:REQUEST_METHOD      = REQUEST_METHOD          
       WebContext:SCRIPT_NAME         = SCRIPT_NAME                
       WebContext:PATH_INFO           = PATH_INFO                    
       WebContext:PATH_TRANSLATED     = PATH_TRANSLATED        
       WebContext:QUERY_STRING        = QUERY_STRING              
       WebContext:REMOTE_ADDR         = REMOTE_ADDR                
       WebContext:REMOTE_HOST         = REMOTE_HOST        
       WebContext:REMOTE_IDENT        = REMOTE_IDENT              
       WebContext:REMOTE_USER         = REMOTE_USER               
       WebContext:AUTH_TYPE           = AUTH_TYPE                    
       WebContext:CONTENT_TYPE        = CONTENT_TYPE              
       WebContext:CONTENT_LENGTH      = CONTENT_LENGTH          
       WebContext:HTTP_ACCEPT         = HTTP_ACCEPT                
       WebContext:HTTP_COOKIE         = HTTP_COOKIE                
       WebContext:HTTP_REFERER        = HTTP_REFERER              
       WebContext:HTTP_USER_AGENT     = HTTP_USER_AGENT        
       WebContext:HTTPS               = HTTPS                            
       WebContext:AppProgram          = AppProgram                  
       WebContext:AppURL              = AppURL                          
       WebContext:SelfURL             = SelfURL                        
       WebContext:HostURL             = HostURL                        
       WebContext:CookiePath          = CookiePath                  
       WebContext:RootURL             = RootURL
       WebContext:useConnID           = useConnID                    
       WebContext:CookieDomain        = CookieDomain
       WebContext:SelDelim            = SelDelim                      
       WebContext:output-content-type = output-content-type 
       WebContext:http-newline        = http-newline
       WebContext:utc-offset          = utc-offset
       WebContext:web-utilities-hdl   = web-utilities-hdl
       
       WebContext:WebStream = STREAM webstream:HANDLE 
       .        
            