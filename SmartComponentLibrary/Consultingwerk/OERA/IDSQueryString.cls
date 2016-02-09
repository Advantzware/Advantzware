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
    File        : IDSQueryString
    Purpose     : Interface for Data-Source Query Manipulation routine
    Syntax      : 
    Description : Abstraction from Consultingwerk.OERA.Query.DSQueryString 
                  (originating from Progress Software Corp.)
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 23 16:50:15 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.IDSQueryString:  
  
    /*------------------------------------------------------------------------------
        Purpose: Builds the QueryString for the Query                                                                      
        Notes:                                       
        @param phQuery The HANDLE of the QUERY widget
        @return The Query String                                 
    ------------------------------------------------------------------------------*/  
    METHOD PUBLIC CHARACTER BuildQueryString (phQuery AS HANDLE) . 
  
    /*------------------------------------------------------------------------------
        Purpose: Insert the QueryString into the query                                                                       
        Notes:                    
        @param phQuery The HANDLE of the QUERY widget
        @return True if successful                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL InsertToQuery (phQuery AS HANDLE) .
  
END INTERFACE.