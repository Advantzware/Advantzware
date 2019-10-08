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
    File        : SmartBusinessEntityTableConfiguration
    Purpose     : Default ISmartBusinessEntityTableConfiguration implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 03 19:15:27 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartBusinessEntityTableConfiguration 
    IMPLEMENTS ISmartBusinessEntityTableConfiguration 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i TableName CHARACTER} .  
    {Consultingwerk/JsonSerializableProperty.i TableOrder INTEGER} .  
    {Consultingwerk/JsonSerializableProperty.i TableLabelSingular CHARACTER} .  
    {Consultingwerk/JsonSerializableProperty.i TableLabelPlural CHARACTER} .  
    {Consultingwerk/JsonSerializableProperty.i BatchSize INTEGER} . 
    {Consultingwerk/JsonSerializableProperty.i ShouldUseBatching LOGICAL} .  
    {Consultingwerk/JsonSerializableProperty.i AllowCreate LOGICAL} . 
    {Consultingwerk/JsonSerializableProperty.i AllowDelete LOGICAL} .  
    {Consultingwerk/JsonSerializableProperty.i AllowUpdate LOGICAL} .  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityTableConfiguration class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityTableConfiguration ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

END CLASS.
