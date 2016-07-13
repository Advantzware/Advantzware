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
    File        : UpdatableDetailViewBuilder
    Purpose     : Builder for an updatable detail view
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 17 16:44:00 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.* FROM PROPATH .  
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.UpdatableDetailViewBuilder 
    INHERITS BaseDetailViewBuilder: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the source code for the the View Columns
        Notes:   
        @return The html5 string describing the view columns
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PUBLIC LONGCHAR GetViewColumns ():
		
        DEFINE VARIABLE lcView AS LONGCHAR NO-UNDO . 
        
        {Consultingwerk/foreachABL.i Column oColumn in THIS-OBJECT:Columns}

            ASSIGN lcView = lcView + SUBSTITUTE ('            <li>~n' +
                                                 '                <label>&2:</label>~n' +
                                                 '                <input data-bind="value: &1" style="float: right"></input>~n' +                                                
                                                 '            </li>~n',
                                                 oColumn:ColumnName,
                                                 oColumn:ColumnLabel) .
        END.
        
        RETURN lcView . 
 

	END METHOD .

END CLASS.
