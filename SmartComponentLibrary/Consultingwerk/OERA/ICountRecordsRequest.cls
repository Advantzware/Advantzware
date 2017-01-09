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
    File        : ICountRecordsRequest
    Purpose     : Interface for classes extending IFetchDataRequest with 
                  the ability to count result records of a query 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 25 22:28:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.ICountRecordsRequest:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the number of query results  
        Notes:   
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY NumResults AS INTEGER NO-UNDO 
    GET .     
    SET .
      
    /*------------------------------------------------------------------------------
        Purpose: A chr(1) separated list of queries that corresponds to 
                 the requested tables. An empty entry means use default query.
        Notes:   Special values:
                 'DEFS' - no query, only defintions requested. 
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY Queries AS CHARACTER NO-UNDO 
    GET . 

    /*------------------------------------------------------------------------------
        Purpose: Value for StopAfter to be passed to the Service Interface 
        Notes:   Allows limiting the execution time of queries executed on the backend
                 Not supported on OpenEdge 10.2B
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StopAfter AS INTEGER NO-UNDO 
    GET.
    SET.      

    /*------------------------------------------------------------------------------
        Purpose: Returns the value that should be returned when stop-after occurs
        Notes:   
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY StopAfterNumResults AS INTEGER NO-UNDO 
    GET .     
    SET .

    /*------------------------------------------------------------------------------
        Purpose: Tables for which to retrieve data or definitions, specified as a 
                 comma separated list. The names must match the physical name of 
                 the temp-table definitions of the prodataset that corresponds to 
                 the Business Entity.  
        Notes:   The Tables property may contain the asterix as a wild-card character
                 and will be expanded to valid buffer names
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY Tables AS CHARACTER NO-UNDO 
    GET .     
  
END INTERFACE.
