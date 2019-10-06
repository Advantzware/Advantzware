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
    File        : ClassIsAParameter
    Purpose     : Parameter object for the ClassIsA method of the
                  ObjectTypeBusinessEntity
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

CLASS Consultingwerk.SmartFramework.Repository.Class.ClassIsAParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ChildClassName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ParentClassName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i IsA LOGICAL} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassIsAParamter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassIsAParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassIsAParamter class
        Notes:
        @param pcChildClassName The name of the child class to verify
        @param pcParentClassName The name of the parent class to test if the given child class is a child of
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassIsAParameter (pcChildClassName AS CHARACTER,
                                         pcParentClassName AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:ChildClassName  = pcChildClassName
               THIS-OBJECT:ParentClassName = pcParentClassName .

    END CONSTRUCTOR.

END CLASS.
