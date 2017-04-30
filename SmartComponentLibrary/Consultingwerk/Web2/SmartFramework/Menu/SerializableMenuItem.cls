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
    File        : SerializableMenuItem
    Purpose     : Serializes a Menu Item
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 13:04:49 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartMenu.*  FROM PROPATH .  
USING Consultingwerk.Web2.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Web2.SmartFramework.Menu.SerializableMenuItem 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty11.i Label CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i Image CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i LayoutId CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i Template CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i ViewUrl CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i Items Consultingwerk.Web2.SmartFramework.Menu.ListSerializableMenuItem} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableMenuItem class
        Notes:   
        @param pcLabel The value for the Label
        @param pcImage The value for the Image
        @param pcViewUrl The value for the ViewUrl
        @param pcLayoutId The value for the LayoutId
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableMenuItem (pcLabel AS CHARACTER, 
                                             pcImage AS CHARACTER, 
                                             pcViewUrl AS CHARACTER,
                                             pcLayoutId AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:Label    = pcLabel 
               THIS-OBJECT:Image    = pcImage
               THIS-OBJECT:ViewUrl  = pcViewUrl 
               THIS-OBJECT:LayoutId = pcLayoutId .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableMenuItem class
        Notes:   
        @param pcLabel The value for the Label
        @param pcImage The value for the Image
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableMenuItem (pcLabel AS CHARACTER, 
                                             pcImage AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:Label    = pcLabel 
               THIS-OBJECT:Image    = pcImage .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableMenuItem class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableMenuItem ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:Items = NEW ListSerializableMenuItem () . 

    END CONSTRUCTOR. 

END CLASS.
