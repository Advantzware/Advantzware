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
    File        : DataGrid
    Purpose     : Represents the meta data for the Data Grid
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

CLASS Consultingwerk.Web.KendoUiBuilder.Module.DataGrid
    INHERITS MetaDataObject
    IMPLEMENTS IViewChild:

    {Consultingwerk/JsonSerializableProperty11.i title CHARACTER "INIT '<Title>':U"}
    {Consultingwerk/JsonSerializableProperty11.i extensionFolderName CHARACTER}
    {Consultingwerk/JsonSerializableProperty11.i dataProvider CHARACTER}
    {Consultingwerk/JsonSerializableProperty11.i dataSource CHARACTER}
    {Consultingwerk/JsonSerializableProperty11.i columns Consultingwerk.Web.KendoUiBuilder.Module.ListColumn}
    {Consultingwerk/JsonSerializableProperty11.i pageSize INTEGER "INIT 20"}
    {Consultingwerk/JsonSerializableProperty11.i rowTemplateFunction CHARACTER}
    {Consultingwerk/JsonSerializableProperty11.i rowTemplateId CHARACTER}
    {Consultingwerk/JsonSerializableProperty11.i filterable LOGICAL "INIT TRUE"}
    {Consultingwerk/JsonSerializableProperty11.i groupable LOGICAL "INIT FALSE"}
    {Consultingwerk/JsonSerializableProperty11.i resizable LOGICAL "INIT TRUE"}
    {Consultingwerk/JsonSerializableProperty11.i reorderable LOGICAL "INIT TRUE"}
    {Consultingwerk/JsonSerializableProperty11.i sortable LOGICAL "INIT TRUE"}
    {Consultingwerk/JsonSerializableProperty11.i customSections Consultingwerk.Web.KendoUiBuilder.Module.CustomDataGridSections}
    {Consultingwerk/JsonSerializableProperty11.i events Consultingwerk.Web.KendoUiBuilder.Module.DataGridEvents}

    /**
     * Purpose: Constructor for the DataGrid class
     * Notes:
     * @param pcName The name of the View
     */
    CONSTRUCTOR PUBLIC DataGrid (pcName AS CHARACTER):

        THIS-OBJECT () .

        THIS-OBJECT:name = pcName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the DataGrid class
     * Notes:
     */
    CONSTRUCTOR PUBLIC DataGrid ():
        SUPER ("data-grid":U).

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        THIS-OBJECT:columns = NEW ListColumn () .
        THIS-OBJECT:customSections = NEW CustomDataGridSections () .
        THIS-OBJECT:events = NEW DataGridEvents () .

    END CONSTRUCTOR.

END CLASS.
