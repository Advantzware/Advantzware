/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *    The original author of this class was Progress Software,        *
 *    Håvard Danielsen                                                *
 **********************************************************************/ 
/*------------------------------------------------------------------------
    File        : IQueryMap
    Purpose     : Interface describing the methods that the QueryString 
                  class needs to know about its owner 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 03 08:24:02 CET 2010
    Notes       :      
  ----------------------------------------------------------------------*/
  
USING Consultingwerk.OERA.Query.*.

INTERFACE Consultingwerk.OERA.Query.IQueryMap:
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the column from the source (database) column                                                                      
        Notes:                                                                  
        @param pcColumn The name of the temp-table column
        @return The name of the source (database) column      
    ------------------------------------------------------------------------------*/     
    METHOD PUBLIC CHARACTER ColumnSource (pcColumn AS CHARACTER). 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the default query string for the data source                                                                       
        Notes:                                                       
        @return The default query string                 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC CHARACTER DefaultQuery ().      
END.
