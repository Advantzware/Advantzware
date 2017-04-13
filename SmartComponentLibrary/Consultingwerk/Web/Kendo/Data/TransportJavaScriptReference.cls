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
    File        : TransportJavaScriptReference
    Purpose     : Transport properties for a JavaScript reference
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

CLASS Consultingwerk.Web.Kendo.Data.TransportJavaScriptReference
    INHERITS JavaScriptReference
    IMPLEMENTS ITransport: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the JavaScript source code which is represented by
                 this node
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty11.i source CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Not used by this type
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty11.i read CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Not used by this type                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty11.i dataType CHARACTER "INIT 'json':U"} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Transport class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TransportJavaScriptReference ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Transport class                                                                     
        Notes:
        @param pcSource The source code reference                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TransportJavaScriptReference (pcSource AS CHARACTER):
        THIS-OBJECT ().

        THIS-OBJECT:source = pcSource .
        
    END CONSTRUCTOR .

END CLASS.
