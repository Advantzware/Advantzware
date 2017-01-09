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
    File        : IKendoUiGridBuilder
    Purpose     : Interface for Builders of the Kendo UI Grid 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 04 10:31:23 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.* FROM PROPATH .  
USING Consultingwerk.Web.Kendo.Grid.*                           FROM PROPATH .
USING Progress.Lang.*                                           FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.IKendoUiGridBuilder:  
  
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
    GET.
    SET. 

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
                                    piWidthPixels AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the structure for the Kendo UI Grid
        Notes:   
        @return The json string describing the Grid layout 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GetGrid ().

END INTERFACE.
