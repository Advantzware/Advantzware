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
    File        : ReadOnlyGridBuilder
    Purpose     : Builder for an read-only grid 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 04 10:54:59 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.* FROM PROPATH .  
USING Consultingwerk.Util.*                                     FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.*                           FROM PROPATH .
USING Consultingwerk.Web.Kendo.Grid.*                           FROM PROPATH .
USING Progress.Lang.*                                           FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.ReadOnlyGridBuilder 
    INHERITS BaseGridBuilder: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the json source for the Kendo UI Grid
        Notes:   
        @return The json string describing the Grid layout 
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE LONGCHAR GetGrid ():
        
        DEFINE VARIABLE lcJson         AS LONGCHAR NO-UNDO . 

        THIS-OBJECT:Grid:height = 600 . 
        THIS-OBJECT:Grid:filterable = NEW Filterable ("row, menu":U) .
        THIS-OBJECT:Grid:groupable = TRUE .
        THIS-OBJECT:Grid:reorderable = TRUE .
        THIS-OBJECT:Grid:resizable = TRUE .
        THIS-OBJECT:Grid:sortable = TRUE . 
        
        THIS-OBJECT:Grid:pageable = NEW Pageable () .
        THIS-OBJECT:Grid:pageable:refresh = TRUE .
        THIS-OBJECT:Grid:pageable:pageSizes = ArrayHelper:Array (10, 25, 100) .
        THIS-OBJECT:Grid:pageable:pageSize = 10 .
        THIS-OBJECT:Grid:pageable:buttonCount = 5 .

        THIS-OBJECT:Grid:dataSource = NEW DataSourceJavaScriptReference ("dataSource":U) .

        lcJson = THIS-OBJECT:Grid:Serialize() .

        CAST (THIS-OBJECT:Grid:dataSource, 
              DataSourceJavaScriptReference):FixJavaScriptReference 
                        (INPUT-OUTPUT lcJson,
                         "dataSource":U,
                         CAST (THIS-OBJECT:Grid:dataSource, DataSourceJavaScriptReference):source) .

        RETURN lcJson .

    END METHOD .

END CLASS.
