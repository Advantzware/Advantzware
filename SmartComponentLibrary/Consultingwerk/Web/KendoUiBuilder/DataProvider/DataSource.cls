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
    File        : DataSource
    Purpose     : Represents the meta data of a Kendo UI Builder Data Source
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

CLASS Consultingwerk.Web.KendoUiBuilder.DataProvider.DataSource
    INHERITS MetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i serviceName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i resourceName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i tableName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i clientSideProcessing LOGICAL "INIT FALSE"} .
    {Consultingwerk/JsonSerializableProperty11.i countFnName CHARACTER "INIT 'count':U"} .
    {Consultingwerk/JsonSerializableProperty11.i fields Consultingwerk.Web.KendoUiBuilder.DataProvider.ListField} .
    {Consultingwerk/JsonSerializableProperty11.i relations Consultingwerk.Web.KendoUiBuilder.DataProvider.ListRelation} .

    /**
     * Purpose: Constructor for the DataSource class
     * Notes:
     * @param pcName The name of the Data Source
     */
    CONSTRUCTOR PUBLIC DataSource (pcName AS CHARACTER):
        THIS-OBJECT () .

        THIS-OBJECT:name = pcName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the DataSource class
     * Notes:
     */
    CONSTRUCTOR PUBLIC DataSource ():
        SUPER ("data-source":U).

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        THIS-OBJECT:fields = NEW ListField () .
        THIS-OBJECT:relations = NEW ListRelation () .

    END CONSTRUCTOR.

END CLASS.
