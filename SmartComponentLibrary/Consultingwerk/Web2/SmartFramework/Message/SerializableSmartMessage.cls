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
    File        : SerializableSmartMessage
    Purpose     : JSON Serializable SmartMessage representation
    Syntax      : 
    Description : Message substitute parameters (&1...&9) are not replaced
                  in this structure - as it's expected that those will be
                  processed by the client (browser)
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 09:08:54 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Web2.SmartFramework.Language.*  FROM PROPATH .  
USING Consultingwerk.Web2.SmartFramework.Message.*   FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.Web2.SmartFramework.Message.SerializableSmartMessage 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    :

    {Consultingwerk/JsonSerializableProperty.i MessageGroup CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MessageNumber INTEGER} .
    {Consultingwerk/JsonSerializableProperty.i Language Consultingwerk.Web2.SmartFramework.Language.SerializableSmartLanguage} .
    {Consultingwerk/JsonSerializableProperty.i MessageText CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MessageDetail CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MessageType Consultingwerk.SmartFramework.MessageTypeEnum} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableSmartMessage class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableSmartMessage ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 

    END CONSTRUCTOR. 

END CLASS.
