/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SerializableSmartLanguage
    Purpose     : JSON Serializable SmartLanguage representation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 09:11:25 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*        FROM PROPATH .
USING Consultingwerk.Web2.SmartFramework.Language.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.Web2.SmartFramework.Language.SerializableSmartLanguage 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty11.i LanguageName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i LanguageImage CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i LanguageIsoCode CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableSmartLanguage class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableSmartLanguage ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Returns a SerializableSmartLanguage instance by a Language Guid
        Notes:   Returns ? (no error), when the language is not found
        @param pcLanguageGuid The guid of the language to return 
        @return The SerializableSmartLanguage instance for the specified Guid
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC SerializableSmartLanguage FromLanguageGuid (pcLanguageGuid AS CHARACTER):
        
        DEFINE VARIABLE oLanguageDataset AS LanguageDatasetModel      NO-UNDO . 
        DEFINE VARIABLE oLanguage        AS SerializableSmartLanguage NO-UNDO . 
        
        oLanguageDataset = NEW LanguageDatasetModel (pcLanguageGuid) . 

        IF NOT oLanguageDataset:SmartLanguage:Available THEN 
            RETURN ? . 
        
        oLanguage = NEW SerializableSmartLanguage () .
        
        ASSIGN oLanguage:LanguageName    = oLanguageDataset:SmartLanguage:LanguageName
               oLanguage:LanguageImage   = oLanguageDataset:SmartLanguage:LanguageImage
               oLanguage:LanguageIsoCode = oLanguageDataset:SmartLanguage:LanguageIsoCode
            .        

        RETURN oLanguage .

    END METHOD .

END CLASS.
