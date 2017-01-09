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
    File        : PropertyChangedEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Tue May 19 22:09:29 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                  FROM PROPATH .
USING Consultingwerk.Windows.Controls.* FROM PROPATH .  
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.Windows.Controls.PropertyChangedEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the property that has changesd
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PropertyName AS CHARACTER NO-UNDO 
	GET.
	SET. 

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the PropertyChangedEventArgs class 
	    Notes:
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertyChangedEventArgs ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyChangedEventArgs class 
        Notes:
        @param pcPropertyName The value for the PropertyName property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyChangedEventArgs (pcPropertyName AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:PropertyName = pcPropertyName .
         
    END CONSTRUCTOR.

END CLASS.
