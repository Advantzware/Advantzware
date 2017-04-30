/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ObjectIsAParameter
    Purpose     : Parameter object for the ObjectIsA method of the
                  ObjectMasterBusinessEntity
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 15 22:54:08 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Object.ObjectIsAParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ObjectName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ClassName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i IsA LOGICAL} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectIsAParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectIsAParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectIsAParameter class
        Notes:
        @param pcObjectName The name of the object master to verify
        @param pcClassName The name of the parent class to test if the given object master is a child of
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectIsAParameter (pcObjectName AS CHARACTER,
                                           pcClassName AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:ObjectName = pcObjectName
               THIS-OBJECT:ClassName  = pcClassName .

    END CONSTRUCTOR.

END CLASS.
