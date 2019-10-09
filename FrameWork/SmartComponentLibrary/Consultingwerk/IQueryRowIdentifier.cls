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
    File        : IQueryRowIdentifier
    Purpose     : Provides abstraction methods for query row id representation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 03 15:27:56 CET 2013
    Notes       : Used in the Consultingwerk.OERA.DataAccess class to allow
                  customers extensions to the RowId context string used by the 
                  Data Access
  ----------------------------------------------------------------------*/

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.IQueryRowIdentifier:  
  
    /*------------------------------------------------------------------------------
        Purpose: Assigns the internal storage with the restart row identifiers of 
                 the given data source object handle   
        Notes:   
        @param phDataSource The handle of the DATA-SOURCE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetDataSourceNextRowids (phDataSource AS HANDLE) .

    /*------------------------------------------------------------------------------
        Purpose: Populates the ROWID[]with the rowids of the current Query result row                                                                        
        Notes:   
        @param phQueryHandle The handle of the query
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetQueryCurrentRowids (phQueryHandle AS HANDLE) .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns if the QueryRowIdentifier contains a valid row identifier
        Notes:   Verifies that a ROWID for the topmost buffer is available 
        @return Logical value indicating if the QueryRowIdentifier contains a valid row identifier
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsBaseRowidSpecified () .
    
    /*------------------------------------------------------------------------------
        Purpose: Parses a character representation of the rowident 
        Notes:   
        @param pcRowidIdent The character representation of the rowident
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ParseString (pcRowidIdent AS CHARACTER) .

    /*------------------------------------------------------------------------------
        Purpose: Compares the array of the current query ROWIDs with the array of
                 Restart ROWIDs 
        Notes:   
        @param phQueryHandle The Query handle
        @return Logical value indicating if the two arrays are equal
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL QueryCurrentRowidsEquals (phQueryHandle AS HANDLE) .

    /*------------------------------------------------------------------------------
        Purpose: Moves a query objects result list pointer to the row corresponding 
                 to the specified ROWID or ROWIDs. To reposition to a particular row 
                 when the query is a join, supply the ROWIDs of the buffers that 
                 correspond to the desired row.                                                                        
        Notes:   
        @param phQueryHandle The handle of the query
        @return Logical value indicating if the method was successful                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RepositionQuery (phQueryHandle AS HANDLE) .
    
    /*------------------------------------------------------------------------------
        Purpose: Sets the RESTART-ROWID for the data source buffer(s)  
        Notes:   
        @param phBufferHandle The buffer handle
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetBufferRestartRowids (phBufferHandle AS HANDLE) .
    
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the CHARACTER representation (serialization) of this instance
        Notes:   The result of this method can be parsed by the method ParseString() 
        @return The CHARACTER representation of this instance 
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER ToString () .        
      
END INTERFACE.
