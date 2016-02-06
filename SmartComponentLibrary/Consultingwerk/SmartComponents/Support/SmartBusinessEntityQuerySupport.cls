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
    File        : SmartBusinessEntityQuerySupport
    Purpose     : Supports building queries for the SmartBusinessEntityAdapter 
                  designer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Mar 14 09:07:02 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW . 

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                      FROM PROPATH . 
USING Consultingwerk.Exceptions.*                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH.
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .
USING Consultingwerk.Framework.*                      FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Support.SmartBusinessEntityQuerySupport   : 

    DEFINE STATIC VARIABLE hQuery   AS HANDLE NO-UNDO.
    DEFINE STATIC VARIABLE hDataset AS HANDLE NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Methods to clean up the dynamicly generated objects                                                                        
        Notes:   Allows deletion of the temporarily used dataset. We cannot delete
                 the dataset immediately, because that would invalidate the buffers
                 used in the query.                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID CleanUp ():
        
        IF VALID-HANDLE (hQuery) THEN
            DELETE OBJECT hQuery NO-ERROR .
        
        IF VALID-HANDLE(hDataset) THEN
            DELETE OBJECT hDataset NO-ERROR .
        
    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Builds the Data Binding Query                                                                       
        Notes:       
        @param phDataset The handle of the ProDataset
        @param pcEntityTable The entity table name
        @param pcEntityView The entity view table names
        @param pcEntityJoin Comma delimited list of YES and NO that indicates if the Entity View tables should be joined in the query
        @param plOuterJoin Logical value indicating if the query should use an OUTER-JOIN
        @param cExceptList OUTPUT Comma delimited list of field names that will be excluded from the BindingSource                                                                 
        @return The query handle
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC HANDLE PrepareBindingQuery (phDataset AS HANDLE, 
                                                     pcEntityTable AS CHARACTER,
                                                     pcEntityView AS CHARACTER,
                                                     pcEntityJoin AS CHARACTER,
                                                     plOuterJoin AS LOGICAL,
                                                     OUTPUT cExceptList AS CHARACTER):        
        /* Create Query */
        DEFINE VARIABLE i                AS INTEGER   NO-UNDO .
        DEFINE VARIABLE j                AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cQueryString     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cQueryFields     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hBuffer          AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hParentBuffer    AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cChildTableQuery AS CHARACTER NO-UNDO .
    
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcEntityTable "'EntityTable':U"} .
        DatasetAssert:HasBuffer (phDataset, pcEntityTable) .
    
        ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (pcEntityTable) .
    
        IF NOT VALID-HANDLE (hBuffer) THEN 
            UNDO, THROW NEW InvalidValueException (pcEntityTable, "EntityTable":U) .
    
        CREATE QUERY hQuery .
        hQuery:ADD-BUFFER (hBuffer) .

        cQueryString = "PRESELECT EACH ":U + pcEntityTable .

        /* Add buffer fields of first buffer to the list of fields (to prevent duplicates) */
        DO j = 1 TO hBuffer:NUM-FIELDS:
            cQueryFields = cQueryFields + ",":U + hBuffer:BUFFER-FIELD(j):NAME .                                  
        END. 
        
        cQueryFields = TRIM(cQueryFields, ",":U) .
        
        DO i = 1 TO NUM-ENTRIES(pcEntityView) ON ERROR UNDO, THROW:
            IF NUM-ENTRIES(pcEntityJoin) >= i AND ENTRY(i, pcEntityJoin) = "Yes":U THEN DO:
                ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (ENTRY(i, pcEntityView)) .
                
                IF NOT VALID-HANDLE (hBuffer) THEN 
                    UNDO, THROW NEW InvalidValueException (ENTRY(i, pcEntityView), "Entity View":U) .
                
                hQuery:ADD-BUFFER (hBuffer) .

                IF NOT VALID-HANDLE (hBuffer:PARENT-RELATION) THEN DO:
                    UNDO, THROW NEW AppError ("Only Child-Tables of a DATA-RELATION can be used in the BindingQuery.~n~n"{&TRAN} +
                                              "Please create or activate the DATA-RELATION first.~n"{&TRAN}  +
                                              "(":U + pcEntityTable + " ->":U + pcEntityView + ")":U, 0) .
                END.

                /* Mike Fechner, Consultingwerk Ltd. 16.07.2009
                   Build query predicate for EntityTable based on 
                   the DATA-RELATION Query, not a static buffer2 OF buffer1 */     
                IF hBuffer:PARENT-RELATION:REPOSITION = FALSE THEN DO:                                 
                    ASSIGN cChildTableQuery = hBuffer:PARENT-RELATION:QUERY:PREPARE-STRING .
                    
                    IF cChildTableQuery BEGINS "FOR ":U OR cChildTableQuery BEGINS "PRESELECT ":U THEN 
                        ASSIGN ENTRY(1, cChildTableQuery, " ":U) = "":U .
                END.
                ELSE 
                    /* Mike Fechner, Consultingwerk Ltd. 16.07.2009
                       The Query of a REPOSITION DATA-RELATION does not contain the 
                       parent table filter condition, so we need to build it manually */
                    cChildTableQuery = DatasetHelper:ChildRelationQueryString (hBuffer) . 
                    
                cQueryString = cQueryString + ", ":U + cChildTableQuery +  
                               (IF plOuterJoin THEN " OUTER-JOIN ":U ELSE "":U).
                
                DO j = 1 TO hBuffer:NUM-FIELDS:
                    IF ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD(j):NAME, cQueryFields) THEN 
                        ASSIGN cExceptList = cExceptList + ",":U + hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD(j):NAME .
                    ELSE
                        ASSIGN cQueryFields = cQueryFields + ",":U + hBuffer:BUFFER-FIELD(j):NAME .  
                END.                         
            END.
            CATCH e AS Progress.Lang.Error :
                /* ShowErrorMessageBox due to non-void method */
                ErrorHelper:ShowErrorMessageBox (e, "Error while processing table "{&TRAN} + ENTRY(i, pcEntityView)) .
            END CATCH.
            
        END.

        hQuery:QUERY-PREPARE (cQueryString) .

        ASSIGN cExceptList = TRIM(cExceptList, ",":U) .

        RETURN hQuery.    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Builds the Data Binding Query                                                                       
        Notes:       
        @param phDataset The handle of the ProDataset
        @param pcEntityTable The entity table name
        @param pcEntityView The entity view table names
        @param pcEntityJoin Comma delimited list of YES and NO that indicates if the Entity View tables should be joined in the query
        @return The query handle
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC HANDLE PrepareBindingQuery (phDataset AS HANDLE, 
                                                     pcEntityTable AS CHARACTER,
                                                     pcEntityView AS CHARACTER,
                                                     pcEntityJoin AS CHARACTER):
        
        DEFINE VARIABLE cExceptList AS CHARACTER NO-UNDO.
        
        RETURN PrepareBindingQuery (phDataset, 
                                    pcEntityTable,
                                    pcEntityView,
                                    pcEntityJoin,
                                    FALSE,
                                    OUTPUT cExceptList) .                                                     
    END.    

    /*------------------------------------------------------------------------------
        Purpose: Builds the Data Binding Query                                                                       
        Notes:       
        @param pcEntityName The name of the Business Entity 
        @param pcEntityTable The entity table name
        @param pcEntityView The entity view table names
        @param pcEntityJoin Comma delimited list of YES and NO that indicates if the Entity View tables should be joined in the query
        @param cExceptList OUTPUT Comma delimited list of field names that will be excluded from the BindingSource                                                                 
        @return The query handle
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC HANDLE PrepareBindingQuery (pcEntityName AS CHARACTER, 
                                                     pcEntityTable AS CHARACTER,
                                                     pcEntityView AS CHARACTER,
                                                     pcEntityJoin AS CHARACTER,
                                                     OUTPUT cExceptList AS CHARACTER):        

        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO . 
        
        IF VALID-HANDLE(hDataset) THEN 
            DELETE OBJECT hDataset . 
            
        ASSIGN hDataset = ? .                 

        /* Request empty dataset from OERA Backend */
        oServiceAdapter = Consultingwerk.Framework.FrameworkSettings:ServiceAdapter .
        
        oServiceAdapter:FetchDataset ("":U, pcEntityName,
                                      OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) . 
        
        IF BusinessEntityDesignerSupport:SearchBusinessEntititiesOnAppServer AND 
           BusinessEntityDesignerSupport:DisconnectAppServerAfterEachRequest THEN 
            FrameworkSettings:AppServerServiceManager:DisconnectService ("":U) .                                                                      
        
        RETURN PrepareBindingQuery (hDataset, 
                                    pcEntityTable,
                                    pcEntityView,
                                    pcEntityJoin,
                                    FALSE, 
                                    OUTPUT cExceptList) .        

    END METHOD.                                                          

    /*------------------------------------------------------------------------------
        Purpose: Builds the Data Binding Query                                                                       
        Notes:       
        @param pcEntityName The name of the Business Entity 
        @param pcEntityTable The entity table name
        @param pcEntityView The entity view table names
        @param pcEntityJoin Comma delimited list of YES and NO that indicates if the Entity View tables should be joined in the query
        @return The query handle
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC HANDLE PrepareBindingQuery (pcEntityName AS CHARACTER, 
                                                     pcEntityTable AS CHARACTER,
                                                     pcEntityView AS CHARACTER,
                                                     pcEntityJoin AS CHARACTER):
        
        DEFINE VARIABLE cExceptList AS CHARACTER NO-UNDO.
        
        RETURN PrepareBindingQuery (pcEntityName, 
                                    pcEntityTable,
                                    pcEntityView,
                                    pcEntityJoin,
                                    OUTPUT cExceptList) .                                                     
    END METHOD.    

END CLASS.
