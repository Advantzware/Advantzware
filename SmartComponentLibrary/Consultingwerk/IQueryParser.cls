/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IQueryParser
    Purpose     : Describes method(s) required by classes the allow to 
                  parse an ABL Query String for a single buffer into 
                  an IQueryExpression (QueryExpression, ListQueryExpression, ...)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 25 09:47:56 CEST 2014
    Notes       : SCL-501
  ----------------------------------------------------------------------*/

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH .

INTERFACE Consultingwerk.IQueryParser:  
  
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Buffer to be appended to unqualified 
                 field references 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Parses an ABL Query String for a single buffer info an IQueryExpression  
        Notes:   
        @param pcQueryString The ABL QueryString to parse
        @return The IQueryExpression instance representing the query string
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC IQueryExpression ParseQueryString (pcQueryString AS CHARACTER).

END INTERFACE.
