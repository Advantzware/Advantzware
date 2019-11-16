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
    File        : ValidationMessage
    Purpose     : Describes a validation message
    Syntax      :
    Description : See section 2.5 Component Error Handling of the CCSBE
                  spec
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 27 09:36:33 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                         FROM PROPATH .
USING Consultingwerk.OERA.ValidationMessages.* FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.OERA.ValidationMessages.ValidationMessage
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    :

    {Consultingwerk/JsonSerializableProperty.i FieldName CHARACTER "INITIAL ?"} .
    {Consultingwerk/JsonSerializableProperty.i MessageStrings "CHARACTER EXTENT"} .
    {Consultingwerk/JsonSerializableProperty.i MessageId INTEGER "INITIAL ?"} .
    {Consultingwerk/JsonSerializableProperty.i MessageGroup CHARACTER "INITIAL ?"} .
    {Consultingwerk/JsonSerializableProperty.i SubstitutionValues "CHARACTER EXTENT"} .
    {Consultingwerk/JsonSerializableProperty.i Severity Consultingwerk.OERA.ValidationMessages.ValidationMessageSeverity "INITIAL ?"} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ValidationMessage class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValidationMessage ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        ASSIGN
            THIS-OBJECT:SerializeEmptyLists          = FALSE
            THIS-OBJECT:SerializeNullValues          = FALSE
            THIS-OBJECT:UseSerializedTypeInformation = FALSE
            THIS-OBJECT:Severity                     = ValidationMessageSeverity:Error .

    END CONSTRUCTOR.

END CLASS.
