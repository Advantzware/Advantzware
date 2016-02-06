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
    File        : GetParentClassesParameter
    Purpose     : Parameter class for the GetParentClasses method of the 
                  ObjectTypeBusinessEntity and ObjectTypeDataAccess classes
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 05 03:00:55 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Class.GetParentClassesParameter 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ObjectTypeName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ParentObjectTypeNames CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ParentObjectTypeGuids CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetParentClassesParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetParentClassesParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetParentClassesParameter class
        Notes:
        @param pcObjectTypeName The value for the ObjectTypeName property (can either be a GUID or a class name)
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetParentClassesParameter (pcObjectTypeName AS CHARACTER):
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        ASSIGN THIS-OBJECT:ObjectTypeName = pcObjectTypeName .

    END CONSTRUCTOR. 

END CLASS.
