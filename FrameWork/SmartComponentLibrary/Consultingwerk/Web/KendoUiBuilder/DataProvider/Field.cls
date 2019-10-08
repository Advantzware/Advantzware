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
    File        : Field
    Purpose     : Represents the meta data of a Kendo UI Builder Data Source
                  Field
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

CLASS Consultingwerk.Web.KendoUiBuilder.DataProvider.Field
    INHERITS SimpleMetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i name CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i label CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i editorType CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i semanticType CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i format CHARACTER} .

    /**
     * Purpose: Constructor for the Field class
     * Notes:
     * @param pcName The Name of the Field
     * @param pcLabel The Label of the Field
     * @param pcEditorType The editor type of the field
     * @param pcSemanticType The semantic type of the field
     * @param pcFormat The field format
     */
    CONSTRUCTOR PUBLIC Field (pcName AS CHARACTER,
                              pcLabel AS CHARACTER,
                              pcEditorType AS CHARACTER,
                              pcSemanticType AS CHARACTER,
                              pcFormat AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:name         = pcName
               THIS-OBJECT:label        = pcLabel
               THIS-OBJECT:editorType   = pcEditorType
               THIS-OBJECT:semanticType = pcSemanticType
               THIS-OBJECT:format       = pcFormat .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Field class
     * Notes:
     * @param pcName The Name of the Data Provider
     */
    CONSTRUCTOR PUBLIC Field (pcName AS CHARACTER):

        THIS-OBJECT () .

        THIS-OBJECT:name = pcName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Field class
     * Notes:
     */
    CONSTRUCTOR PUBLIC Field ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
