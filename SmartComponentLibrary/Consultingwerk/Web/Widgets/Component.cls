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
    File        : Component
    Purpose     : 
    Syntax      : 
    Description : A common base class for Web Controls and parts such as 
                  Browser Columns
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 05 00:20:36 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.*         FROM PROPATH . 
USING Consultingwerk.Web.Widgets.* FROM PROPATH . 
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.Component 
    ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Encodes a String for HTML output                                                                        
        Notes:                                                                        
        @param pcData The string to encode
        @return The encoded CHARACTER data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER HtmlEncode (pcData AS CHARACTER):
        
        RETURN DYNAMIC-FUNCTION ("html-encode":U IN WebContext:web-utilities-hdl, pcData) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character string to the WebStream                                                                       
        Notes:      
        @param pcData The CHARACTER Data to write to the web stream
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Output (pcData AS CHARACTER):
        
        PUT STREAM-HANDLE WebContext:WebStream UNFORMATTED pcData . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character string to the WebStream                                                                       
        Notes:                                                                        
        @param pcData The CHARACTER Data to write to the web stream
        @param plSkip SKIP (add line break) after writing the pcData to the web stream
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Output (pcData AS CHARACTER,
                                  plSkip AS LOGICAL):
        
        IF plSkip THEN 
            PUT STREAM-HANDLE WebContext:WebStream UNFORMATTED pcData SKIP .
        ELSE  
            PUT STREAM-HANDLE WebContext:WebStream UNFORMATTED pcData .

    END METHOD.

END CLASS.
