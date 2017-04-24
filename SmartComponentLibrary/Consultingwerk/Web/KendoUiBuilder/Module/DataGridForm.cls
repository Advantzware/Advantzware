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
    File        : DataGridForm
    Purpose     : Represents the meta data for the Data Grid Form
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 02 00:19:14 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.KendoUiBuilder.*        FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.* FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Web.KendoUiBuilder.Module.DataGridForm
    INHERITS DataGrid
    IMPLEMENTS IViewChild:

    {Consultingwerk/JsonSerializableProperty11.i newTitle CHARACTER "INIT '<New record>':U"}
    {Consultingwerk/JsonSerializableProperty11.i editTitle CHARACTER "INIT '<Edit record>':U"}
    {Consultingwerk/JsonSerializableProperty11.i confirmRemove LOGICAL "INIT TRUE"}
    {Consultingwerk/JsonSerializableProperty11.i editMode CHARACTER "INIT 'Read-Only to Edit':U"}
    {Consultingwerk/JsonSerializableProperty11.i fields Consultingwerk.Web.KendoUiBuilder.Module.ListField}

    /**
     * Purpose: Constructor for the DataGridForm class
     * Notes:
     * @param pcName The name of the View
     */
    CONSTRUCTOR PUBLIC DataGridForm (pcName AS CHARACTER):

        THIS-OBJECT () .

        THIS-OBJECT:name = pcName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the DataGridForm class
     * Notes:
     */
    CONSTRUCTOR PUBLIC DataGridForm ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        THIS-OBJECT:type = "data-grid-form":U .
        THIS-OBJECT:fields = NEW ListField () .

    END CONSTRUCTOR.

END CLASS.
