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
    File        : GetMessageParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 17 14:13:00 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.GetMessageParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    :
        
    {Consultingwerk/JsonSerializableProperty.i MessageGroup CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MessageNumber INTEGER} .
    {Consultingwerk/JsonSerializableProperty.i SessionLanguageKey CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetMessageParameter class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetMessageParameter ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetMessageParameter class                                                                     
        Notes:           
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcSessionLanguageKey The session language key
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetMessageParameter (pcMessageGroup AS CHARACTER, 
                                            piMessageNumber AS INTEGER,
                                            pcSessionLanguageKey AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:MessageGroup       = pcMessageGroup
               THIS-OBJECT:MessageNumber      = piMessageNumber
               THIS-OBJECT:SessionLanguageKey = pcSessionLanguageKey .
        
    END CONSTRUCTOR.
    
END CLASS.
