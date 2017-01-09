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
    File        : BaseGridBuilder
    Purpose     : Abstract base class for Kendo UI Grid Builder 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 04 10:45:28 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.* FROM PROPATH .  
USING Consultingwerk.Web.Kendo.Grid.*                           FROM PROPATH .
USING Progress.Lang.*                                           FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.BaseGridBuilder 
    IMPLEMENTS IKendoUiGridBuilder ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Data Source resource 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DataSourceName AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the table to use from the Data Source resource 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataSourceTableName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Grid structur 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Grid AS KendoGrid NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and set the File name to the generated 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY OutputFileName AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the page to be written 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PageTitle AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the template to be used 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY TemplateFileName AS CHARACTER NO-UNDO
	   INIT "Consultingwerk/BusinessEntityDesigner/Generator/Templates/Web/KendoUiGrid/GridPage.template":U  
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BaseGridBuilder class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC BaseGridBuilder ():
		SUPER ().
		
		THIS-OBJECT:Grid = NEW KendoGrid () .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds a Columnn to the Grid definition
        Notes:   
        @param pcColumnName The name of the column to add
        @param pcColumnDataType The ABL data type for the colum
        @param pcColumnLabel The label for the column
        @param piWidthPixels The columns width pixels 
        @return The Grid Column that was added to the Grid
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC Column AddColumn (pcColumnName AS CHARACTER, 
                                    pcColumnDataType AS CHARACTER,
                                    pcColumnLabel AS CHARACTER,
                                    piWidthPixels AS INTEGER):
		
		DEFINE VARIABLE oColumn AS Column NO-UNDO .
		
		oColumn = THIS-OBJECT:Grid:columns:Add (pcColumnName) .
		
		oColumn:title = pcColumnLabel .
		
		IF pcColumnDataType = "INTEGER":U OR pcColumnDataType = "INT64":U THEN  
		    oColumn:type = "int":U .
		
		IF piWidthPixels > 0 THEN 
		    oColumn:width = SUBSTITUTE ("&1px":U, ROUND (piWidthPixels * 1.2, 0)) . 
		    		
		RETURN oColumn .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the structure for the Kendo UI Grid
        Notes:   
        @return The json string describing the Grid layout 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ABSTRACT LONGCHAR GetGrid () .

END CLASS.
