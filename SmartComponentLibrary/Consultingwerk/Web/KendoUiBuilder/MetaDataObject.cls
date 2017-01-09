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
    File        : MetaDataObject
    Purpose     : Abstract base class for Kendo UI Builder meta data
                  objects
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 21 17:45:40 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.JsonSerializable.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.KendoUiBuilder.MetaDataObject
    INHERITS JsonSerializable ABSTRACT:

    {Consultingwerk/JsonSerializableProperty11.i name CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i type CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i schemaVersion CHARACTER "INIT '1.0':U"} .

    /**
     * Purpose: Constructor for the MetaDataObject class
     * Notes:
     * @param pcType The value for the type property
     * @param pcName The value for the name property
     */
    CONSTRUCTOR PUBLIC MetaDataObject (pcType AS CHARACTER,
                                       pcName AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:type = pcType
               THIS-OBJECT:name = pcName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the MetaDataObject class
     * Notes:
     * @param pcType The value for the type property
     */
    CONSTRUCTOR PUBLIC MetaDataObject (pcType AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:type = pcType .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the MetaDataObject class
     * Notes:
     */
    CONSTRUCTOR PUBLIC MetaDataObject ():
        SUPER ().

        THIS-OBJECT:UseSerializedTypeInformation = FALSE .
        THIS-OBJECT:SerializeNullValues = TRUE .
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
