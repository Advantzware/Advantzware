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
    File        : Error132Info
    Purpose     : A holder class for details about error 132
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 27 01:15:43 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.Util.Error132Info
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i TableName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i FieldValues CHARACTER} .

    /**
     * Purpose: Constructor for the Error132Info class
     * Notes:
     */
    CONSTRUCTOR PUBLIC Error132Info ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the CharacterHolder class
     * Notes:
     * @param pcTableName The initial Value for TableName
     * @param pcFieldValues The initial Value for FieldValues
     */
    CONSTRUCTOR PUBLIC Error132Info (pcTableName AS CHARACTER,
                                     pcFieldValues AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:TableName   = pcTableName
               THIS-OBJECT:FieldValues = pcFieldValues .

    END CONSTRUCTOR.

END CLASS.
