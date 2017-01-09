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
    File        : GetTranslationByKeyParameter
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

CLASS Consultingwerk.SmartFramework.System.GetTranslationByKeyParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    :
        
    {Consultingwerk/JsonSerializableProperty.i SessionLanguageKey CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i TranslationKey CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i TranslationScope CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetTranslationByKeyParameter class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetTranslationByKeyParameter ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetTranslationByKeyParameter class                                                                     
        Notes:     
        @param pcTranslationScope The scope to query the translation for
        @param pcTranslationKey The translation key value to query                  
        @param pcSessionLanguageKey The session language key
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetTranslationByKeyParameter (pcTranslationScope   AS CHARACTER,
                                                     pcTranslationKey     AS CHARACTER, 
                                                     pcSessionLanguageKey AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:TranslationScope   = pcTranslationScope
               THIS-OBJECT:TranslationKey     = pcTranslationKey
               THIS-OBJECT:SessionLanguageKey = pcSessionLanguageKey .
        
    END CONSTRUCTOR.
    
END CLASS.
