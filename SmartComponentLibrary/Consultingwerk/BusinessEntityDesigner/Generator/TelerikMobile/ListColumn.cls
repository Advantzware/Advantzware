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
    File        : ListColumn
    Purpose     : List of Column Definitions
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 12 12:49:17 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                                FROM PROPATH .
USING Consultingwerk.Framework.Base.*                                 FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.* FROM PROPATH .
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.ListColumn
    INHERITS GenericList: 

    { Consultingwerk/Framework/Base/GenericList.i Column }

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListColumn class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListColumn ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds a Columnn definition to the List
        Notes:   
        @param pcColumnName The name of the column to add
        @param pcColumnDataType The ABL data type for the colum
        @param pcColumnLabel The label for the column
        @return The Column that was created
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Column Add (pcColumnName AS CHARACTER, 
                              pcColumnDataType AS CHARACTER,
                              pcColumnLabel AS CHARACTER):

        DEFINE VARIABLE oColumn AS Column NO-UNDO . 

        oColumn = NEW Column () .
        
        ASSIGN oColumn:ColumnName     = pcColumnName
               oColumn:ColumnDataType = pcColumnDataType
               oColumn:ColumnLabel    = pcColumnLabel .
        
        THIS-OBJECT:Add (oColumn) .
        
    END.
    
END CLASS.
