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
    File        : ErrorJavaScriptReference
    Purpose     : Error properties for a JavaScript reference
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 30 22:47:01 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.*      FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Data.ErrorJavaScriptReference
    INHERITS JavaScriptReference: 

    {Consultingwerk/JsonSerializableProperty11.i source CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Error class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ErrorJavaScriptReference ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Error class                                                                     
        Notes:
        @param pcSource The source code reference                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ErrorJavaScriptReference (pcSource AS CHARACTER):
        THIS-OBJECT ().

        THIS-OBJECT:source = pcSource .
        
    END CONSTRUCTOR .

END CLASS.
