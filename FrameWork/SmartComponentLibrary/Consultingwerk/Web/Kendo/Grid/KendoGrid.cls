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
    File        : KendoGrid
    Purpose     : Represents a Kendo UI grid
    Syntax      : 
    Description : http://docs.telerik.com/kendo-ui/api/javascript/ui/grid
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 12 22:30:59 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Util.*           FROM PROPATH .
USING Consultingwerk.Web.Kendo.*      FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .
USING Consultingwerk.Web.Kendo.Grid.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Grid.KendoGrid 
    INHERITS JsonSerializable: 
    
    {Consultingwerk/JsonSerializableProperty11.i dataSource Consultingwerk.Web.Kendo.Data.IDataSource} .
    {Consultingwerk/JsonSerializableProperty11.i height INTEGER "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i width INTEGER "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i filterable Consultingwerk.Web.Kendo.Grid.Filterable} .
    {Consultingwerk/JsonSerializableProperty11.i groupable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i reorderable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i resizable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i sortable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i selectable CHARACTER "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i pageable Consultingwerk.Web.Kendo.Grid.Pageable} .
    {Consultingwerk/JsonSerializableProperty11.i editable Consultingwerk.Web.Kendo.Grid.Editable} .
    {Consultingwerk/JsonSerializableProperty11.i toolbar "CHARACTER EXTENT"} .
    {Consultingwerk/JsonSerializableProperty11.i columns Consultingwerk.Web.Kendo.Grid.ListColumn} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the KendoGrid class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC KendoGrid ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        
        THIS-OBJECT:dataSource = NEW DataSource () . 
        THIS-OBJECT:columns    = NEW ListColumn () .
        
    END CONSTRUCTOR.
    
END CLASS.
