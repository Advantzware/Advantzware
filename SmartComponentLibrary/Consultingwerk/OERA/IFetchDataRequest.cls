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
    File        : IFetchDataRequest
    Purpose     : Parameter object for FetchData requests
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : Sun Mar 07 23:34:10 CET 2010
    Notes       : The origin of the request parameters is the ADM2 service
                  adapter definition (src/adm2/serviceadapter.p)
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IFetchDataRequest:  
  
    /*------------------------------------------------------------------------------
        Purpose: Gets/sets the Context for this request. The Context contains the 
                 information controlling the positions for the next batch 
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Context AS CHARACTER NO-UNDO
    GET .
    SET .  
    
    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the CustomContext used for the communication with the 
                 Business Entity. 
        Notes:   While the ContextString is part of the Business Entity context (and
                 thus mixing the Business Entity internal context with sort of custom
                 context the property CustomContext is 100% reserved for the use
                 in your own Business Entities / Data Access Objects
    ------------------------------------------------------------------------------*/      
    DEFINE PUBLIC PROPERTY CustomContext AS CHARACTER NO-UNDO
    GET . 
    SET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the custom parameter object
        Notes:   Allows assigning a custom serializable object instance as an 
                 additional parameter to the RetrieveData/FetchData request
                 The object instance will be serialized both ways (from the client
                 to the backend and the backend to the client)                      
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY CustomParameter AS Consultingwerk.ISerializable NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: A chr(1) separated list of join fields for the requested tables,
                 in the form of ForeignFields. Included in this  parameter, if 
                 applicable, is the element number of the Business Entity the 
                 foreign field is contained in, relative to the current request. 
        Notes: - The join limits the query of this table to the records that are 
                 children of the parent table as specified in a comma separated 
                 paired list where this table's field(s) is/are the first entry 
                 of each pair and the parent's field is/are the second entry of 
                 each pair. 
                   
                 e.g. <thistable.field,othertable.field> 
                     
               - Joins across Entities are specified by a third qualifier on the 
                 parent field name that specifies the array number of the parent 
                 Entity.
                   
                 e.g. order.custnum,1.customer.custnum   
                    
                 In this above example the order is joined to the customer table 
                 in the first Entity of this request.
                 
               - The ADM will set this from the prodataset relation, so the 
                 parameter is typically not important for relationships within the
                 same prodataset. 
               - A blank entry means that the table is not to be joined in this 
                 request.   
               This property is currently not supported by the SmartComponent 
               Library backend  
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY Joins AS CHARACTER NO-UNDO
    GET .
    SET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the NamedQueryParameter instance of the call  
        Notes:   SCL-1018
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY NamedQuery AS INamedQueryParameter NO-UNDO 
	GET .
    SET . 

    /*------------------------------------------------------------------------------
        Purpose:    Specifies the next record that was NOT returned to the client. 
        Notes:    - The ADM only differentiates between no value and some value to 
                    be able to know if there is more records to retrieve, but the
                    actual values are not used for on the client and can in 
                    principle have any content. The use of ROWIDs as context fits 
                    with PRODATASET attributes like START-ROWID and NEXT-ROWID, but
                    ROWIDS are not safe as context in progress databases where 
                    records may get deleted. Consider using context-ids that can be 
                    used to re-find context info stored on the server or just use the
                    actual logical query expression that defines the record that is 
                    next by the use of true keys. (eg. order.ordernum = 55)    
                  - Special values
                     'LAST' or blank means that the last record is included in the 
                     request and that there is no more data on the server in a 
                     forward direction.
                  - The implementer needs to fix the number of extents in this 
                    parameter to that of the pcEntity parameter, noted above.             
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY NextContext AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose:  Specifies the batch size.   
        Notes:   
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY NumRecords AS INTEGER NO-UNDO  
    GET . 

    /*------------------------------------------------------------------------------
        Purpose:  A chr(1) separated list of positions for the requested tables.
        Notes:    Positions specifies how to find a single record in a table from 
                  a child of this table where the child is used to filter or find 
                  the parent record. This is specified as a comma separated paired 
                  list where this table's field(s) is/are the first entry of each 
                  pair. 
                  
                  e.g. <thistable.field,othertable.field>
             
                - Positioning across Entities are specified by a third qualifier on 
                  the parent field name that specifies the array number of the 
                  other Entity.
                     
                  e.g. salesrep.salesrep.1.order.salesrep   
                    
                  In this above example the salesrep is found from the order 
                  records of the first Entity of the request.
             
                  This property is currently not supported by the SmartComponent 
                  Library backend
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY Positions AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose:    Specifies the prev record that was NOT returned to the 
                    client. 
        Notes:    - The ADM only differentiates between no value and some value to 
                    be able to know if there is more records to retrieve, but the
                    actual values are not used for on the client and can in 
                    principle have any content. The use of ROWIDs as context fits 
                    with PRODATASET attributes like START-ROWID and NEXT-ROWID, but
                    ROWIDS are not safe as context in progress databases where 
                    records may get deleted. Consider using context-ids that can be 
                    used to refind context info stored on the server or just use the
                    actual logical query expression that defines the record that is 
                    prev.   
                  - Special values
                    'FIRST' or blank means that the first record is included in the 
                    request and that there is no more data on the server in a 
                    backwards direction.
                  - The implementer needs to fix the number of extents in this 
                    parameter to that of the pcEntity parameter, noted above.
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY PrevContext AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: A chr(1) separated list of queries that corresponds to 
                 the requested tables. An empty entry means use default query.
        Notes:   Special values:
                 'DEFS' - no query, only defintions requested. 
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY Queries AS CHARACTER NO-UNDO 
    GET . 
    SET .
    
    /*------------------------------------------------------------------------------
        Purpose: Specifies the actual request that corresponds to the table in 
                 a chr(1) delimited list. 
        Notes:   Valid values:
                  FIRST - Start retrieving from the first record.
                  NEXT  - Start retrieving from the record that is the next record
                          from what is currently the last. The BatchContext 
                          parameter has the information to where the next 
                          position actually starts.  
                  PREV  - Start retrieving from the record that is the previous
                          record from what is currently the first. The BatchContext 
                          parameter has the information to where the previous 
                          position actually starts.
                  LAST  - Start retrieve so that the last record is included in 
                          the batch. 
                  WHERE <query expression>  
                        - Start retrieving on the first record that satisfies
                          the specified criteria. 
                  ALL   - Retrieve all records as defined by join.
                          Retrieve all records that are related to all parents in 
                          this request. This corresponds to the default FILL 
                          behavior of a prodataset parent-child relation. 
                 This property is currently not supported by the SmartComponent 
                 Library backend
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY Requests AS CHARACTER NO-UNDO 
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
        Purpose: Tables for which to retrieve data or definitions, specified as a 
                 comma separated list. The names must match the physical name of 
                 the temp-table definitions of the prodataset that corresponds to 
                 the Business Entity.  
        Notes:   The Tables property may contain the asterix as a wild-card character
                 and will be expanded to valid buffer names
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY Tables AS CHARACTER NO-UNDO 
    GET . 
    SET .
    
END INTERFACE.
