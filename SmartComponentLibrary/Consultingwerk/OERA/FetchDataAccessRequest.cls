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
    File        : FetchDataAccessRequest
    Purpose     : Extension to FetchDataRequest with specific instructions
                  for the Data Access class. It is only expected to be used
                  from within a Business Entity
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 01 20:55:50 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.FetchDataAccessRequest 
    INHERITS FetchDataRequest: 

    /*------------------------------------------------------------------------------
        Purpose: Allows to control if the FetchDataRequest is going to append data
        Notes:   The default behaviour (FALSE) is to empty data dataset at the 
                 beginning of the FetchDataRequest
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY AppendFetchData AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Allows to control if the FILL-MODE for buffers that are contained 
                 in the request
        Notes:   The default FillMode used is "REPLACE"
                 APPEND: Fills the table by adding new records on top of existing records in the table, without performing any record comparisons. If this creates duplicate records, the AVM generates a run-time error and you must manage the duplicate records. If you are certain there are no duplicate records, an APPEND is more efficient than a MERGE. 
                 EMPTY: Empties the table before the FILL operation begins. 
                 MERGE: Fills the table by merging new records with existing records in the table. The FILL( ) method checks each record to ensure there are no duplicate records (based on the table’s unique primary index). If the FILL( ) method finds a record with a duplicate key, it does not replace the record because the record might have dependent records elsewhere in the ProDataSet. In this case, the AVM does not generate a run-time error. Thus, you cannot use this mode to refresh existing records.
                 NO-FILL: Does not perform the FILL operation on the table.
                 REPLACE: Fills the table by replacing existing records in the table. The FILL( ) method checks each record to determine whether or not it exists in the table (based on the table’s unique primary index). If the record exists in the table, the FILL( ) method replaces it. If the record does not exist, the FILL( ) method creates a new record. The BEFORE-ROW-FILL event handler is run after the original ProDataSet row is copied into the ProDataSet buffer. A REPLACE is less efficient than an APPEND or a MERGE. 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FillMode AS CHARACTER INITIAL ? NO-UNDO  
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FetchDataRequest class
        Notes:   
        @param poParameter The value for the CustomParameter property  
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FetchDataAccessRequest (poParameter AS Consultingwerk.ISerializable):
		SUPER (poParameter).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FetchDataRequest class
        Notes:   
        @param poNamedQuery The value for the NamedQuery property  
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FetchDataAccessRequest (poNamedQuery AS Consultingwerk.OERA.NamedQueryParameter):
		SUPER (poNamedQuery).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FetchDataRequest class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FetchDataAccessRequest ():
		SUPER ().
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Simplified constructor when doing calls from custom code                                                                       
        Notes:          
        @param pcTables The tables to retrieve, Tables attribute
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FetchDataAccessRequest (pcTables AS CHARACTER):
		SUPER (pcTables).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Simplified constructor when doing calls from custom code                                                                       
        Notes:        
        @param pcTables The tables to retrieve, Tables attribute
        @param piNumRecords The number of records to retrieve from the first table (batch size), NumRecords attribute
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FetchDataAccessRequest (pcTables AS CHARACTER, piNumRecords AS INTEGER):
		SUPER (pcTables, piNumRecords).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Simplified constructor when doing calls from custom code                                                                       
        Notes:    
        @param pcTables The tables to retrieve, Tables attribute
        @param pcQueries The CHR(1) delimited list of query strings per table, Queries attribute
        @param piNumRecords The number of records to retrieve from the first table (batch size), NumRecords attribute
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FetchDataAccessRequest (pcTables AS CHARACTER, pcQueries AS CHARACTER, piNumRecords AS INTEGER):
		SUPER (pcTables, pcQueries, piNumRecords).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Simplified constructor when doing calls from custom code                                                                       
        Notes:    
        @param pcTables The tables to retrieve, Tables attribute
        @param pcQueries The CHR(1) delimited list of query strings per table, Queries attribute
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FetchDataAccessRequest (pcTables AS CHARACTER, pcQueries AS CHARACTER):
		SUPER (pcTables, pcQueries).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Default Constructor for the FetchDataRequest class                                                                        
        Notes:   This is the constructor used by the SmartBusinessEntityAdapter 
                 when making requests to the back end.    
        @param pcTables The tables to retrieve, Tables attribute
        @param pcQueries The CHR(1) delimited list of query strings per table, Queries attribute
        @param piNumRecords The number of records to retrieve from the first table (batch size), NumRecords attribute
        @param pcContext The request context, Context attribute
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FetchDataAccessRequest (pcTables AS CHARACTER, 
	                                           pcQueries AS CHARACTER, 
	                                           piNumRecords AS INTEGER, 
	                                           pcContext AS CHARACTER):
	                                               
		SUPER (pcTables, pcQueries, piNumRecords, pcContext).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Default Constructor for the FetchDataRequest class                                                                        
        Notes:   This is the constructor used by the SmartBusinessEntityAdapter 
                 when making requests to the back end.     
        @param pcTables The tables to retrieve, Tables attribute
        @param pcQueries The CHR(1) delimited list of query strings per table, Queries attribute
        @param pcRequests The request types (typically ADM2 cient only), Requests attribute
        @param piNumRecords The number of records to retrieve from the first table (batch size), NumRecords attribute
        @param pcContext The request context, Context attribute
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FetchDataAccessRequest (pcTables AS CHARACTER, 
	                                           pcQueries AS CHARACTER, 
	                                           pcRequests AS CHARACTER, 
	                                           piNumRecords AS INTEGER, 
	                                           pcContext AS CHARACTER):
	                                               
		SUPER (pcTables, pcQueries, pcRequests, piNumRecords, pcContext).
		
	END CONSTRUCTOR.

END CLASS.
