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
    File        : TableMapping
    Purpose     : Mapping definition between the ProDataset root table and 
                  the .NET object
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd .
    Created     : Mon Feb 09 10:05:36 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.ProDatasetObjectMapping.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Windows.ProDatasetObjectMapping.TableMapping: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of the mapped child temp-tabels 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ChildTableMappings AS ListChildTableMapping NO-UNDO 
    GET.
    PROTECTED SET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the comma delimited list of property specs, supported 
                 entries are PropertyName or PropertyName|FieldName or 
                 PropertyName|FieldName(ABL DataType) 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FieldMapping AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableMapping class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC TableMapping ():
		SUPER ().
		
		THIS-OBJECT:ChildTableMappings = NEW ListChildTableMapping () . 
		
	END CONSTRUCTOR.

END CLASS.
