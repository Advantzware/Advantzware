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
    File        : SmartBusinessEntityConfiguration
    Purpose     : Default ISmartBusinessEntityConfiguration implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 03 19:19:45 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartBusinessEntityConfiguration 
    IMPLEMENTS ISmartBusinessEntityConfiguration 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i BusinessEntityPackage CHARACTER} .  
    {Consultingwerk/JsonSerializableProperty.i BusinessEntityName CHARACTER} . 
    {Consultingwerk/JsonSerializableProperty.i BusinessEntityPurpose CHARACTER} . 
    {Consultingwerk/JsonSerializableProperty.i DBRequired LOGICAL} .
    {Consultingwerk/JsonSerializableProperty.i TrackDeletions LOGICAL} . 
    {Consultingwerk/JsonSerializableProperty.i Tables Consultingwerk.SmartFramework.System.ListSmartBusinessEntityTableConfiguration} . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityConfiguration class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityConfiguration ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:Tables = NEW Consultingwerk.SmartFramework.System.ListSmartBusinessEntityTableConfiguration () .

    END CONSTRUCTOR. 

END CLASS.
