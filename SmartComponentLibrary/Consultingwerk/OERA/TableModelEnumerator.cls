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
    File        : TableModelEnumerator
    Purpose     : Enumerator for TableModel instances
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 11 03:46:04 CEST 2015
    Notes       : SCL-844
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Consultingwerk.OERA.*           FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH.

CLASS Consultingwerk.OERA.TableModelEnumerator 
    IMPLEMENTS IEnumerator: 

    DEFINE VARIABLE lGetNext AS LOGICAL NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Returns the current item of the enumeration                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Current AS Progress.Lang.Object NO-UNDO 
	GET:
	    RETURN THIS-OBJECT:TableModel .
	END GET . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the TableModel this enumerator is enumerating 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY TableModel AS TableModel NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableModelEnumerator class
        Notes:   
        @param poTableModel The TableModel this enumerator is enumerating
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC TableModelEnumerator (poTableModel AS TableModel):
		SUPER ().
		
		{Consultingwerk/Assertion/ObjectAssert/IsValid.i  poTableModel """poTableModel"":U"} .
		
		ASSIGN THIS-OBJECT:TableModel = poTableModel .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Moves the enumerator to the next item                                                                        
        Notes:                      
        @return True when the next item is available, false when not.                                                  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL MoveNext ():

		IF NOT lGetNext THEN  
	        RETURN THIS-OBJECT:TableModel:Available .
		ELSE 
		    RETURN THIS-OBJECT:TableModel:GetNext () .

        FINALLY:
            ASSIGN lGetNext = TRUE . 
        END FINALLY.
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Resets the Enumerator (starts enumerating from the first item on)                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Reset ():
		
		THIS-OBJECT:TableModel:GetFirst() .
		ASSIGN lGetNext = FALSE . 

	END METHOD .

END CLASS.
