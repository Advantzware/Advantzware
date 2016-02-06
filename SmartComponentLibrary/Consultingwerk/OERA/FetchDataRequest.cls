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
    File        : FetchDataRequests
    Purpose     : Parameter object for FetchData requests
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : Sun Mar 07 23:36:10 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*      FROM PROPATH . 
USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.FetchDataRequest 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF     
    IMPLEMENTS IFetchDataRequest, IFetchDataBySkip: 

    { Consultingwerk/OERA/eFetchDataRequest.i &ACCESS="STATIC"}

    /*------------------------------------------------------------------------------
        Purpose: Gets/sets the Context for this request. The Context contains the 
                 information controlling the positions for the next batch 
        Notes:   
    ------------------------------------------------------------------------------*/    
    {Consultingwerk/JsonSerializableProperty.i Context CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the CustomContext used for the communication with the 
                 Business Entity. 
        Notes:   While the ContextString is part of the Business Entity context (and
                 thus mixing the Business Entity internal context with sort of custom
                 context the property CustomContext is 100% reserved for the use
                 in your own Business Entities / Data Access Objects
    ------------------------------------------------------------------------------*/      
    {Consultingwerk/JsonSerializableProperty.i CustomContext CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the custom parameter object
        Notes:   Allows assigning a custom serializable object instance as an 
                 additional parameter to the RetrieveData/FetchData request
                 The object instance will be serialized both ways (from the client
                 to the backend and the backend to the client)                      
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i CustomParameter Consultingwerk.ISerializable} .

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
    {Consultingwerk/JsonSerializableProperty.i Joins CHARACTER} .
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the NamedQueryParameter instance of the call  
        Notes:   SCL-1018
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i NamedQuery INamedQueryParameter} .
        
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
    {Consultingwerk/JsonSerializableProperty.i NextContext CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose:  Specifies the batch size.   
        Notes:     
    ------------------------------------------------------------------------------*/   
    {Consultingwerk/JsonSerializableProperty.i NumRecords INTEGER} .
    
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
    {Consultingwerk/JsonSerializableProperty.i Positions CHARACTER} .

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
    {Consultingwerk/JsonSerializableProperty.i PrevContext CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets/sets the QueryExpression for this request 
        Notes:   This property is an alternative to the Queries property that contains
                 a Query String
    ------------------------------------------------------------------------------*/    
    {Consultingwerk/JsonSerializableProperty.i QueryExpression Consultingwerk.IQueryExpression} .

    /*------------------------------------------------------------------------------
        Purpose: A chr(1) separated list of queries that corresponds to 
                 the requested tables. An empty entry means use default query.
        Notes:   Special values:
                 'DEFS' - no query, only defintions requested.   
    ------------------------------------------------------------------------------*/   
    {Consultingwerk/JsonSerializableProperty.i Queries CHARACTER} .

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
    {Consultingwerk/JsonSerializableProperty.i Requests CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the number of records to be skipped from the beginning 
                 when reading data 
        Notes:   Required for the server side batching/paging with JSDO/Kendo UI
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Skip INTEGER "INIT 0"} .
    
    /*------------------------------------------------------------------------------
        Purpose: Value for StopAfter to be passed to the Service Interface 
        Notes:   Allows limiting the execution time of queries executed on the backend
                 Not supported on OpenEdge 10.2B
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i StopAfter INTEGER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Tables for which to retrieve data or definitions, specified as a 
                 comma separated list. The names must match the physical name of 
                 the temp-table definitions of the prodataset that corresponds to 
                 the Business Entity.  
        Notes:   The Tables property may contain the asterix as a wild-card character
                 and will be expanded to valid buffer names
    ------------------------------------------------------------------------------*/   
    {Consultingwerk/JsonSerializableProperty.i Tables CHARACTER} .
    
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
    CONSTRUCTOR PUBLIC FetchDataRequest (pcTables AS CHARACTER,
                                         pcQueries AS CHARACTER,
                                         pcRequests AS CHARACTER,
                                         piNumRecords AS INTEGER,
                                         pcContext AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN 
            THIS-OBJECT:Tables = REPLACE (pcTables, " ":U, "":U)
            THIS-OBJECT:Queries = pcQueries
            THIS-OBJECT:Requests = REPLACE (pcRequests, " ":U, "":U)
            THIS-OBJECT:NumRecords = piNumRecords
            THIS-OBJECT:Context = pcContext .  
        
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
    CONSTRUCTOR PUBLIC FetchDataRequest (pcTables AS CHARACTER,
                                         pcQueries AS CHARACTER,
                                         piNumRecords AS INTEGER,
                                         pcContext AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN 
            THIS-OBJECT:Tables = REPLACE (pcTables, " ":U, "":U)
            THIS-OBJECT:Queries = pcQueries
            THIS-OBJECT:NumRecords = piNumRecords
            THIS-OBJECT:Context = pcContext .  
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Simplified constructor when doing calls from custom code                                                                       
        Notes:    
        @param pcTables The tables to retrieve, Tables attribute
        @param pcQueries The CHR(1) delimited list of query strings per table, Queries attribute
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FetchDataRequest (pcTables AS CHARACTER,
                                         pcQueries AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN 
            THIS-OBJECT:Tables     = REPLACE (pcTables, " ":U, "":U)
            THIS-OBJECT:Queries    = pcQueries
            THIS-OBJECT:NumRecords = 0  .  
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Simplified constructor when doing calls from custom code                                                                       
        Notes:    
        @param pcTables The tables to retrieve, Tables attribute
        @param pcQueries The CHR(1) delimited list of query strings per table, Queries attribute
        @param piNumRecords The number of records to retrieve from the first table (batch size), NumRecords attribute
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FetchDataRequest (pcTables AS CHARACTER,
                                         pcQueries AS CHARACTER,
                                         piNumRecords AS INTEGER):
        THIS-OBJECT ().
        
        ASSIGN 
            THIS-OBJECT:Tables = REPLACE (pcTables, " ":U, "":U)
            THIS-OBJECT:Queries = pcQueries
            THIS-OBJECT:NumRecords = piNumRecords .  
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Simplified constructor when doing calls from custom code                                                                       
        Notes:        
        @param pcTables The tables to retrieve, Tables attribute
        @param piNumRecords The number of records to retrieve from the first table (batch size), NumRecords attribute
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FetchDataRequest (pcTables AS CHARACTER,
                                         piNumRecords AS INTEGER):
        THIS-OBJECT ().
        
        ASSIGN 
            THIS-OBJECT:Tables = REPLACE (pcTables, " ":U, "":U)
            THIS-OBJECT:NumRecords = piNumRecords .  
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Simplified constructor when doing calls from custom code                                                                       
        Notes:          
        @param pcTables The tables to retrieve, Tables attribute
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FetchDataRequest (pcTables AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN 
            THIS-OBJECT:Tables = REPLACE (pcTables, " ":U, "":U)
            THIS-OBJECT:NumRecords = 0 .  
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FetchDataRequest class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FetchDataRequest ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FetchDataRequest class
        Notes:   
        @param poNamedQuery The value for the NamedQuery property  
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FetchDataRequest (poNamedQuery AS NamedQueryParameter):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:NamedQuery = poNamedQuery .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FetchDataRequest class
        Notes:   
        @param poParameter The value for the CustomParameter property  
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FetchDataRequest (poParameter AS Consultingwerk.ISerializable):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:CustomParameter = poParameter .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Deserializes a FetchDataRequest object from the first 
                 eFetchDataRequest record.                                                                        
        Notes:         
        @param eFetchDataRequest INPUT TABLE with fields to use for this-objects properties                                                               
        @return The deserialized FetchDataRequest instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC FetchDataRequest FromTempTable (INPUT TABLE eFetchDataRequest):
        
        DEFINE VARIABLE oReturn AS FetchDataRequest NO-UNDO .     
        
        FIND FIRST eFetchDataRequest . /* throw error */ 
        
        oReturn = NEW FetchDataRequest (eFetchDataRequest.Tables,
                                        eFetchDataRequest.Queries,
                                        eFetchDataRequest.Requests,
                                        eFetchDataRequest.NumRecords,
                                        eFetchDataRequest.Context) .
                                         
        ASSIGN oReturn:CustomContext = eFetchDataRequest.CustomContext
               oReturn:Joins         = eFetchDataRequest.Joins
               oReturn:NextContext   = eFetchDataRequest.NextContext
               oReturn:Positions     = eFetchDataRequest.Positions
               oReturn:PrevContext   = eFetchDataRequest.PrevContext
               oReturn:StopAfter     = eFetchDataRequest.StopAfter .
        
        RETURN oReturn .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Serializes the FetchDataRequest instance to a record in the 
                 eFetchDataRequest temp-table                                                                        
        Notes:     
        @param eFetchDataRequest OUTPUT TABLE with fields to populate from this-objects properties                                                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ToTempTable (OUTPUT TABLE eFetchDataRequest):
        
        EMPTY TEMP-TABLE eFetchDataRequest . 

        CREATE eFetchDataRequest . 
        ASSIGN eFetchDataRequest.Context       = THIS-OBJECT:Context
               eFetchDataRequest.CustomContext = THIS-OBJECT:CustomContext
               eFetchDataRequest.Joins         = THIS-OBJECT:Joins
               eFetchDataRequest.NextContext   = THIS-OBJECT:NextContext
               eFetchDataRequest.NumRecords    = THIS-OBJECT:NumRecords
               eFetchDataRequest.Positions     = THIS-OBJECT:Positions
               eFetchDataRequest.PrevContext   = THIS-OBJECT:PrevContext
               eFetchDataRequest.Queries       = THIS-OBJECT:Queries
               eFetchDataRequest.Requests      = THIS-OBJECT:Requests
               eFetchDataRequest.StopAfter     = THIS-OBJECT:StopAfter
               eFetchDataRequest.Tables        = THIS-OBJECT:Tables 
             .

    END METHOD.
    
END CLASS.
