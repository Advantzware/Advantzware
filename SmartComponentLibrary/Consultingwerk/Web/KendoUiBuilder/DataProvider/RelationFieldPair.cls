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
    File        : RelationFieldPair
    Purpose     : Represents the meta data of a Kendo UI Builder Data Source
                  Field Pair
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 21 17:56:37 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.KendoUiBuilder.*              FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.DataProvider.* FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Web.KendoUiBuilder.DataProvider.RelationFieldPair
    INHERITS SimpleMetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i parentFieldName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i childFieldName CHARACTER} .

    /**
     * Purpose: Constructor for the RelationFieldPair class
     * Notes:
     * @param pcParentFieldName The Name of the parent field
     * @param pcChildFieldName The Name of the child field
     */
    CONSTRUCTOR PUBLIC RelationFieldPair (pcParentFieldName AS CHARACTER,
                                          pcChildFieldName AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:parentFieldName = pcParentFieldName
               THIS-OBJECT:childFieldName  = pcChildFieldName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the RelationFieldPair class
     * Notes:
     */
    CONSTRUCTOR PUBLIC RelationFieldPair ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
