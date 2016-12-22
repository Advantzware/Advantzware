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
    File        : ChildTableMapping 
    Purpose     : Mapping definition between ProDataset data-relation 
                  child tables and list properties in the .NET object
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd .
    Created     : Mon Feb 09 10:05:36 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.ProDatasetObjectMapping.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Windows.ProDatasetObjectMapping.ChildTableMapping 
    INHERITS TableMapping: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the ChildRelation for the mapped temp-table  
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ChildRelationName AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the .NET List<T> property with the objects 
                 for the records from the child relation   
        Notes:   Only applicable for child tables
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PropertyName AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableMapping class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ChildTableMapping ():
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
