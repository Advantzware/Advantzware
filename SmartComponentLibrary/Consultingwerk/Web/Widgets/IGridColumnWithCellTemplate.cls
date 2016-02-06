/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IGridColumnWithCellTemplate
    Purpose     : Interface for Grid Columns that require a column template
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 13 22:26:02 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.Widgets.* FROM PROPATH . 
USING Progress.Lang.*              FROM PROPATH .

INTERFACE Consultingwerk.Web.Widgets.IGridColumnWithCellTemplate:  
  
	/*------------------------------------------------------------------------------
	    Purpose: Returns the Column Template (Kendo.UI) for Cells of the Column
	    Notes:
	------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnTemplate AS CHARACTER NO-UNDO 
    GET.

END INTERFACE.