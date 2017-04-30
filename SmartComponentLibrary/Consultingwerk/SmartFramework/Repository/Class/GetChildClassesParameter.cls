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
    File        : GetChildClassesParameter
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 13 12:22:27 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Class.GetChildClassesParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ObjectTypeGuids CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ChildObjectTypeGuids CHARACTER} .

    /**
     * Purpose: Constructor for the GetChildClassesParameter class
     * Notes:
     * @param pcObjectTypeGuids The object types to return the child object types for
     */
    CONSTRUCTOR PUBLIC GetChildClassesParameter (pcObjectTypeGuids AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:ObjectTypeGuids = pcObjectTypeGuids .

    END CONSTRUCTOR .

    /**
     * Purpose: Constructor for the GetChildClassesParameter class
     * Notes:
     */
    CONSTRUCTOR PUBLIC GetChildClassesParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

END CLASS.
