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
    File        : SimpleMetaDataObject
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

CLASS Consultingwerk.Web.KendoUiBuilder.SimpleMetaDataObject
    INHERITS JsonSerializable ABSTRACT:

    /**
     * Purpose: Constructor for the SimpleMetaDataObject class
     * Notes:
     */
    CONSTRUCTOR PUBLIC SimpleMetaDataObject ():
        SUPER ().

        THIS-OBJECT:UseSerializedTypeInformation = FALSE .
        THIS-OBJECT:SerializeNullValues = TRUE .
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
