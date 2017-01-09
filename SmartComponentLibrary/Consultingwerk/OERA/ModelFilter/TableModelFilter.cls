/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : TableModelFilter
    Purpose     : Abstract base class for generated TableModelFilter classes
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 15 07:36:37 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*             FROM PROPATH . 
USING Consultingwerk.OERA.ModelFilter.* FROM PROPATH .  
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.OERA.ModelFilter.TableModelFilter
    ABSTRACT USE-WIDGET-POOL: 

    DEFINE VARIABLE oTableModel AS TableModel NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableModelFilter class
        Notes:   
        @param poTableModel The reference to the TableModel that this Filter instance belongs to
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC TableModelFilter (poTableModel AS TableModel):
		SUPER ().
		
		ASSIGN oTableModel = poTableModel .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Clears the Filters currently present for the TableModel
        Notes:   
        @return The reference to the TableModelFilter instance 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC TableModelFilter Reset ():
		
		oTableModel:ClearFilter () .
		
		RETURN THIS-OBJECT .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Executes the Filter 
        Notes:   Invokes the Fill method of the TableModel instance
        @return The reference to the TableModel instance 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableModel Run ():
        
        oTableModel:Fill () .
        
        RETURN oTableModel . 

    END METHOD .

END CLASS.
