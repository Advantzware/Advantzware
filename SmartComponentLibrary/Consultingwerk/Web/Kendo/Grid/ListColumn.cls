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
    Purpose     : List of Kendo UI grid columns
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 12 12:49:17 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Framework.Base.* FROM PROPATH .  
USING Consultingwerk.Web.Kendo.Grid.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Grid.ListColumn
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
        Purpose: Add's a Column with the given name and title
        Notes:
        @param pcField The name of the column to add
        @param pcTitle The title of the column to add
        @return The new added column
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Column Add (pcField AS CHARACTER,
                              pcTitle AS CHARACTER):
        
        DEFINE VARIABLE oColumn AS Column NO-UNDO . 

        ASSIGN oColumn = NEW Column () 
               oColumn:Field = pcField
               oColumn:Title = pcTitle . 
          
        THIS-OBJECT:Add (oColumn) .  
          
        RETURN oColumn . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Add's a Column with the given name and title
        Notes:
        @param pcField The name of the column to add
        @return The new added column
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Column Add (pcField AS CHARACTER):
        
        RETURN THIS-OBJECT:Add (pcField, pcField) .

    END METHOD.
    
END CLASS.
