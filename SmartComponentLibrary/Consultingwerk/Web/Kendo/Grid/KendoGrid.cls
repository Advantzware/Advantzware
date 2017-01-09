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
	
    {Consultingwerk/JsonSerializableProperty.i dataSource Consultingwerk.Web.Kendo.Data.IDataSource} .
    {Consultingwerk/JsonSerializableProperty.i height INTEGER "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty.i width INTEGER "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty.i filterable Consultingwerk.Web.Kendo.Grid.Filterable} .
    {Consultingwerk/JsonSerializableProperty.i groupable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty.i reorderable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty.i resizable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty.i sortable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty.i selectable CHARACTER "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty.i pageable Consultingwerk.Web.Kendo.Grid.Pageable} .
    {Consultingwerk/JsonSerializableProperty.i editable Consultingwerk.Web.Kendo.Grid.Editable} .
    {Consultingwerk/JsonSerializableProperty.i toolbar "CHARACTER EXTENT"} .
    {Consultingwerk/JsonSerializableProperty.i columns Consultingwerk.Web.Kendo.Grid.ListColumn} .
	
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
