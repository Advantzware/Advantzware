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
    File        : Relation
    Purpose     : Represents the meta data of a Kendo UI Builder Relation
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

CLASS Consultingwerk.Web.KendoUiBuilder.DataProvider.Relation
    INHERITS SimpleMetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i relationName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i parentName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i childName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i relationFields ListRelationFieldPair} .

    /**
     * Purpose: Constructor for the Relation class
     * Notes:
     * @param pcRelationName The Name of the Relation
     * @param pcParentName The name of the parent table
     * @param pcChildName The name of the child table
     */
    CONSTRUCTOR PUBLIC Relation (pcRelationName AS CHARACTER,
                                 pcParentName AS CHARACTER,
                                 pcChildName AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:relationName = pcRelationName
               THIS-OBJECT:parentName   = pcParentName
               THIS-OBJECT:childName    = pcChildName .


    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Relation class
     * Notes:
     * @param pcRelationName The Name of the Relation
     */
    CONSTRUCTOR PUBLIC Relation (pcRelationName AS CHARACTER):

        THIS-OBJECT () .

        THIS-OBJECT:relationName = pcRelationName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Relation class
     * Notes:
     */
    CONSTRUCTOR PUBLIC Relation ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        THIS-OBJECT:relationFields = NEW ListRelationFieldPair () .

    END CONSTRUCTOR.

END CLASS.
