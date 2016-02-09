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
    File        : TableModel
    Purpose     : Abstract base class for Temp-Table Models
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 15 01:45:21 CEST 2013
    Notes       : Used by the DatasetModel class
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                  FROM PROPATH .
USING Consultingwerk.Assertion.*        FROM PROPATH . 
USING Consultingwerk.Framework.Base.*   FROM PROPATH .
USING Consultingwerk.OERA.*             FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.*  FROM PROPATH .   
USING Consultingwerk.OERA.ModelFilter.* FROM PROPATH .
USING Consultingwerk.Util.*             FROM PROPATH .  
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.OERA.TableModel 
    IMPLEMENTS IModelFilterTarget, IEnumerable
    USE-WIDGET-POOL ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Returns if currently a record is available in the TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Available AS LOGICAL NO-UNDO 
	GET:
		RETURN THIS-OBJECT:BufferHandle:AVAILABLE . 
	END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Buffer handle used by the Model 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PROTECTED PROPERTY BufferHandle AS HANDLE NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Buffer used by the Model 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY BufferName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the owning DatasetModel
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DatasetModelPerformer AS DatasetModelPerformer NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of child tables to be retrieved together 
                 with the Fill() method 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FillChildTables AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the type of the model
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ModelType AS TableModelTypeEnum NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the owning DatasetModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY QueryExpressions AS ListQueryExpression NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Query handle used by the Model 
        Notes:   The TableModel will be using the Query created by the ProDataset
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QueryHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Query handle used by the Model 
        Notes:   Temporarily keep the reference to the Filter object to workaround 
                 issues on 10.2B with loosing chained object references
                 SCL-682
    ------------------------------------------------------------------------------*/
	DEFINE PROTECTED PROPERTY SavedFilterReference AS Progress.Lang.Object NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if the TableModel is currently using the Dataset query  
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY UsingDatasetQuery AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	PRIVATE SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableModel class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PROTECTED TableModel (poDatasetModel AS DatasetModel,
	                                  phBufferHandle AS HANDLE):
		SUPER ().
		
		{Consultingwerk/Assertion/ObjectAssert/IsValid.i poDatasetModel """Dataset Model"":U"} .
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phBufferHandle """Dataset Model"":U"} .
		Assert:Equals (phBufferHandle:DATASET, poDatasetModel:DatasetHandle) .
		BufferAssert:IsDatasetBuffer (phBufferHandle) .
		
		ASSIGN THIS-OBJECT:DatasetModelPerformer = poDatasetModel:Performer
		       THIS-OBJECT:BufferHandle          = phBufferHandle 
		       THIS-OBJECT:QueryExpressions      = NEW ListQueryExpression ()
		       THIS-OBJECT:ModelType             = TableModelTypeEnum:TableModel .
	
	   THIS-OBJECT:UseDatasetQuery() . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableModel class
        Notes:   Creates an instance of a view model TableModel
        @param poTableModel The reference to the owning TableModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED TableModel (poTableModel AS TableModel,
                                      phBufferHandle AS HANDLE):
        
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.

        SUPER ().
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poTableModel """Table Model"":U"} .
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phBufferHandle """Dataset Model"":U"} .
        Assert:Equals (phBufferHandle:DATASET, poTableModel:DatasetModelPerformer:DatasetHandle) .
        BufferAssert:IsDatasetBuffer (phBufferHandle) .
        
        CREATE BUFFER hBuffer FOR TABLE phBufferHandle . 
        
        ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hBuffer) .
        
        ASSIGN THIS-OBJECT:DatasetModelPerformer = poTableModel:DatasetModelPerformer
               THIS-OBJECT:BufferHandle          = hBuffer 
               THIS-OBJECT:QueryExpressions      = NEW ListQueryExpression ()
               THIS-OBJECT:QueryHandle           = hQuery 
               THIS-OBJECT:UsingDatasetQuery     = FALSE
               THIS-OBJECT:ModelType             = TableModelTypeEnum:ViewModel .
               
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Retrieves data using the QueryExpression
        Notes:   
        @param poQueryExpression The QueryExpression to retrieve data data for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddQueryExpression (poQueryExpression AS IQueryExpression):

        /* to-do RetrieveData with the QueryExpression */
        THIS-OBJECT:QueryExpressions:Add (poQueryExpression) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns a TRUE value if a record is found that meets the specified 
                 FIND criteria; otherwise it returns FALSE. CAN-FIND does not make 
                 the record available to the Business Entity or it's caller.
        Notes:   
        @param pcFindString Find String starting with "WHERE Buffer.Field = value" or "Buffer.Field = value"
        @return Logical value indicating if a record can be found
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL CanFind (pcFindString AS CHARACTER):
		
        DEFINE VARIABLE oParameter AS CanFindParameter NO-UNDO . 
        DEFINE VARIABLE hDataset   AS HANDLE           NO-UNDO. 
		
        oParameter = NEW CanFindParameter (pcFindString, THIS-OBJECT:BufferName) .

        IF THIS-OBJECT:DatasetModelPerformer:UseInterface = UseInterfaceEnum:ServiceAdapter THEN 

            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                                                    THIS-OBJECT:DatasetModelPerformer:EntityName,
                                                                                    "CanFind":U,
                                                                                    INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                                                    oParameter) .
        ELSE 
            Consultingwerk.OERA.ServiceInterface:InvokeMethod (THIS-OBJECT:DatasetModelPerformer:EntityName,
                                                               "CanFind":U,
                                                               INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                               oParameter) .

        RETURN oParameter:Result .		      
		
		FINALLY:
            IF VALID-HANDLE (hDataset) THEN 
                DELETE OBJECT hDataset .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Clears the current filter
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearFilter ():
        
        THIS-OBJECT:QueryExpressions:Clear () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates a new record in the TableModel 
        Notes:   Only supported for TableModel instances, not ViewModel instances
                 Requires Tracking Changes of the ProDataset
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Create ():
		
        IF NOT THIS-OBJECT:ModelType = Consultingwerk.OERA.TableModelTypeEnum:TableModel THEN 
            UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException	("Create":U, 
                                                                             "Consultingwerk.OERA.TableModel":U) .

        IF NOT THIS-OBJECT:BufferHandle:TABLE-HANDLE:TRACKING-CHANGES THEN 
            UNDO, THROW NEW DatasetModelException ("Unable to create new record, when tracking changes is not turned on."{&TRAN}, 0) .

        THIS-OBJECT:BufferHandle:BUFFER-CREATE () . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates a view for the current record in the model 
        Notes:   
        @return The TableModel instance for the view
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED TableModel CreateCurrentRecordViewInternal ():
        
        DEFINE VARIABLE oViewModel AS TableModel                  NO-UNDO .
        DEFINE VARIABLE oType      AS Progress.Lang.Class         NO-UNDO . 
        DEFINE VARIABLE cTypeName  AS CHARACTER                   NO-UNDO .
        
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i THIS-OBJECT:BufferHandle} .
        
        ASSIGN oType     = THIS-OBJECT:GetClass() 
               cTypeName = oType:TypeName.
        
        oViewModel = DYNAMIC-NEW (cTypeName) 
                          (THIS-OBJECT,
                           THIS-OBJECT:BufferHandle) .  
    
        oViewModel:FindByRowid (THIS-OBJECT:BufferHandle:ROWID) .
        
        RETURN oViewModel .

    END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Creates a view on the table in the model 
        Notes:   
        @return The TableModel instance for the view
	------------------------------------------------------------------------------*/
	METHOD PROTECTED TableModel CreateViewInternal ():
		
        DEFINE VARIABLE oViewModel AS TableModel                  NO-UNDO .
        DEFINE VARIABLE oType      AS Progress.Lang.Class         NO-UNDO . 
		DEFINE VARIABLE cTypeName  AS CHARACTER                   NO-UNDO .
		
		ASSIGN oType     = THIS-OBJECT:GetClass() 
		       cTypeName = oType:TypeName.
		
		oViewModel = DYNAMIC-NEW (cTypeName) 
		                  (THIS-OBJECT,
		                   THIS-OBJECT:BufferHandle) .  
		
		RETURN oViewModel .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deletes a record in the TableModel 
        Notes:   Only supported for TableModel instances, not ViewModel instances
                 Requires Tracking Changes of the ProDataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Delete ():
        
        IF NOT THIS-OBJECT:ModelType = Consultingwerk.OERA.TableModelTypeEnum:TableModel THEN 
            UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("Create":U, 
                                                                             "Consultingwerk.OERA.TableModel":U) .

        IF NOT THIS-OBJECT:BufferHandle:TABLE-HANDLE:TRACKING-CHANGES THEN 
            UNDO, THROW NEW DatasetModelException ("Unable to delete record, when tracking changes is not turned on."{&TRAN}, 0) .

        THIS-OBJECT:BufferHandle:BUFFER-DELETE () . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Executes the current filter
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Fill ():
        
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
        
        IF THIS-OBJECT:ModelType = TableModelTypeEnum:TableModel THEN DO:  
            THIS-OBJECT:DatasetModelPerformer:RetrieveData (TRIM (SUBSTITUTE ("&1,&2":U, 
                                                                              THIS-OBJECT:BufferHandle:NAME,
                                                                              THIS-OBJECT:FillChildTables),
                                                                  ",":U),
                                                            THIS-OBJECT:QueryExpressions) .
            
            IF THIS-OBJECT:FillChildTables > "":U THEN DO:
                THIS-OBJECT:UseDatasetQuery () . 
        
                THIS-OBJECT:BufferHandle:DATASET:RELATIONS-ACTIVE = TRUE .
            END.
            ELSE 
                THIS-OBJECT:UseBufferQuery ()  .
        END.                                                           
        ELSE DO:
            IF THIS-OBJECT:QueryExpressions:IsEmpty THEN 
                cQueryString = SUBSTITUTE ("FOR EACH &1":U, 
                                           THIS-OBJECT:BufferHandle:NAME) .
            ELSE 
                cQueryString = SUBSTITUTE ("FOR EACH &1 WHERE &2":U,
                                           THIS-OBJECT:BufferHandle:NAME,
                                           THIS-OBJECT:QueryExpressions:GetExpression()) .
            
             THIS-OBJECT:QueryHandle:QUERY-PREPARE (cQueryString) .
        END.

        THIS-OBJECT:QueryHandle:QUERY-OPEN () .
        THIS-OBJECT:GetFirst () .  

        FINALLY:
            &IF PROVERSION BEGINS "10" &THEN
            /* SCL-682 : Dispose pending Filter instance */		
            IF VALID-OBJECT (THIS-OBJECT:SavedFilterReference) THEN 
                DELETE OBJECT (THIS-OBJECT:SavedFilterReference) .
            &ENDIF
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Fills the TableModel or filters the View TableModel with the given 
                 QueryString 
        Notes:   The QueryString may be expressed or may not start with FOR EACH ... 
        @param pcQueryString The QueryString which shall be used to retrieve data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Fill (pcQueryString AS CHARACTER):
        
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.

        IF pcQueryString = "":U THEN 
            ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1":U, 
                                              THIS-OBJECT:BufferHandle:NAME) .
        ELSE IF pcQueryString BEGINS "WHERE ":U THEN     
            ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1 &2":U,
                                              THIS-OBJECT:BufferHandle:NAME,
                                              pcQueryString) .
        ELSE IF pcQueryString BEGINS "FOR ":U THEN 
            ASSIGN cQueryString = pcQueryString . 
        ELSE 
            ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1 WHERE &2":U,
                                              THIS-OBJECT:BufferHandle:NAME,
                                              pcQueryString) .
                                              
        IF THIS-OBJECT:ModelType = TableModelTypeEnum:TableModel THEN  
            THIS-OBJECT:DatasetModelPerformer:RetrieveData (THIS-OBJECT:BufferHandle:NAME,
                                                            cQueryString) .
        ELSE 
            THIS-OBJECT:QueryHandle:QUERY-PREPARE (cQueryString) .

        THIS-OBJECT:BufferHandle:DATASET:RELATIONS-ACTIVE = FALSE . 
        THIS-OBJECT:QueryHandle:QUERY-OPEN () . 
        THIS-OBJECT:QueryHandle:GET-FIRST ().
                                               
    END METHOD .

    {Consultingwerk/OERA/GetSetBufferField.i CHARACTER Character}
    {Consultingwerk/OERA/GetSetBufferField.i DATE Date}
    {Consultingwerk/OERA/GetSetBufferField.i DATETIME DateTime}
    {Consultingwerk/OERA/GetSetBufferField.i DATETIME-TZ DateTimeTz}
    {Consultingwerk/OERA/GetSetBufferField.i DECIMAL Decimal}
    {Consultingwerk/OERA/GetSetBufferField.i HANDLE Handle}
    {Consultingwerk/OERA/GetSetBufferField.i INTEGER Integer}
    {Consultingwerk/OERA/GetSetBufferField.i INT64 Int64}
    {Consultingwerk/OERA/GetSetBufferField.i LOGICAL Logical}
    {Consultingwerk/OERA/GetSetBufferField.i LONGCHAR Longchar}
    {Consultingwerk/OERA/GetSetBufferField.i RECID Recid}
    {Consultingwerk/OERA/GetSetBufferField.i ROWID Rowid}

    /*------------------------------------------------------------------------------
        Purpose: Fills the TableModel based on the given data-source ROWID (DB-ROWID)
        Notes:   DB-ROWID passed as comma-delimited list in case there are joined 
                 data-source queries 
        @param pcSourceRowidIds The comma-delimited list of data-source rowids
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID FillByRowid (pcSourceRowidIds AS CHARACTER):
		
        IF NOT THIS-OBJECT:ModelType = Consultingwerk.OERA.TableModelTypeEnum:TableModel THEN 
            UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("FillByRowid":U, 
                                                                             "Consultingwerk.OERA.TableModel":U) .
	
        IF THIS-OBJECT:ModelType = TableModelTypeEnum:TableModel THEN  
            THIS-OBJECT:DatasetModelPerformer:RetrieveDataByRowid (THIS-OBJECT:BufferHandle:NAME,
                                                                   pcSourceRowidIds) .

        THIS-OBJECT:BufferHandle:DATASET:RELATIONS-ACTIVE = FALSE . 
        THIS-OBJECT:QueryHandle:QUERY-OPEN () . 
        THIS-OBJECT:QueryHandle:GET-FIRST ().

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Finds a record in the BufferHandle by a ROWID
	    Notes:   For OpenEdge 10.2B, 11.0 or 11.1 this method needs to be PUBLIC as 
	             PRIVATE members are not accessible to other instances of the same type
	             in those releases. Customers using this method publicly on these 
	             version of OpenEdge need to be prepared to change their code when
	             upgrading to OpenEdge 11.3 as this method is indeed intended to be 
	             PRIVATE to all instances of the same class only.
	    @param proRowid The rowid to locate
	------------------------------------------------------------------------------*/
	METHOD 
	&IF PROVERSION BEGINS "10" OR PROVERSION = "11.0" OR PROVERSION = "11.1" &THEN 
	       PUBLIC
    &ELSE
	       PRIVATE 
    &ENDIF
	               VOID FindByRowid (proRowid AS ROWID):
		
		THIS-OBJECT:BufferHandle:FIND-BY-ROWID (proRowid) . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a new IEnumerator instance for this object instance                                                                      
        Notes:                                                                        
        @return The IEnumerator instance for this object
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC Consultingwerk.Framework.Base.IEnumerator GetEnumerator ():
		
		RETURN NEW TableModelEnumerator (THIS-OBJECT) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Locates the Query to the first record
        Notes:   
        @return Logical value indicating if a record is available 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL GetFirst ():
	
		THIS-OBJECT:QueryHandle:GET-FIRST () .
	
	    IF THIS-OBJECT:UsingDatasetQuery THEN 
	       THIS-OBJECT:ReopenDatasetChildQueries () .
		
		RETURN NOT THIS-OBJECT:QueryHandle:QUERY-OFF-END .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Locates the Query to the next record
        Notes:   
        @return Logical value indicating if a record is available 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL GetNext ():
        
        DEFINE VARIABLE roRowid AS ROWID NO-UNDO . 
        
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            roRowid = THIS-OBJECT:BufferHandle:ROWID . 
        
        THIS-OBJECT:QueryHandle:GET-NEXT () .

        IF THIS-OBJECT:UsingDatasetQuery THEN 
           THIS-OBJECT:ReopenDatasetChildQueries () .
    
        /* Mike Fechner, Consultingwerk Ltd. 14.06.2015
           Fetch next Batch */
        IF THIS-OBJECT:QueryHandle:QUERY-OFF-END AND 
            VALID-OBJECT (THIS-OBJECT:DatasetModelPerformer) AND 
            THIS-OBJECT:ModelType = TableModelTypeEnum:TableModel AND 
            THIS-OBJECT:DatasetModelPerformer:NextBatchAvailable AND 
            THIS-OBJECT:DatasetModelPerformer:BatchingOnTable = THIS-OBJECT:BufferHandle:NAME THEN DO:
        
            THIS-OBJECT:DatasetModelPerformer:PerformNextBatchRequest () . 
        
            IF roRowid <> ? THEN DO:
                THIS-OBJECT:QueryHandle:QUERY-OPEN () . 
                THIS-OBJECT:QueryHandle:REPOSITION-TO-ROWID (roRowid) .
                THIS-OBJECT:QueryHandle:GET-NEXT () . 
                
                IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
                    THIS-OBJECT:QueryHandle:GET-NEXT () . 
            END.
        END.
        
        RETURN NOT THIS-OBJECT:QueryHandle:QUERY-OFF-END .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a New CharacterFilter instance 
        Notes:   
        @param pcFieldName The name of the buffer field to filter on
        @param poTableModelFilter The reference to the TableModelFilter instance
        @return The CharacterFilter instance
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CharacterFilter NewCharacterFilter (pcFieldName AS CHARACTER,
	                                                  poTableModelFilter AS TableModelFilter):
		
		RETURN NEW CharacterFilter (THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName),
                                    THIS-OBJECT,
                                    poTableModelFilter) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a New DateFilter instance 
        Notes:   
        @param pcFieldName The name of the buffer field to filter on
        @param poTableModelFilter The reference to the TableModelFilter instance
        @return The DateFilter instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DateFilter NewDateFilter (pcFieldName AS CHARACTER,
                                            poTableModelFilter AS TableModelFilter):
        
        RETURN NEW DateFilter (THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName),
                               THIS-OBJECT,
                               poTableModelFilter) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a New DateTimeFilter instance 
        Notes:   
        @param pcFieldName The name of the buffer field to filter on
        @param poTableModelFilter The reference to the TableModelFilter instance
        @return The DateTimeFilter instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DateTimeFilter NewDateTimeFilter (pcFieldName AS CHARACTER,
                                                    poTableModelFilter AS TableModelFilter):
        
        RETURN NEW DateTimeFilter (THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName),
                                   THIS-OBJECT,
                                   poTableModelFilter) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a New DateTimeTzFilter instance 
        Notes:   
        @param pcFieldName The name of the buffer field to filter on
        @param poTableModelFilter The reference to the TableModelFilter instance
        @return The DateTimeTzFilter instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DateTimeTzFilter NewDateTimeTzFilter (pcFieldName AS CHARACTER,
                                                        poTableModelFilter AS TableModelFilter):
        
        RETURN NEW DateTimeTzFilter (THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName),
                                     THIS-OBJECT,
                                     poTableModelFilter) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a New DecimalFilter instance 
        Notes:   
        @param pcFieldName The name of the buffer field to filter on
        @param poTableModelFilter The reference to the TableModelFilter instance
        @return The DecimalFilter instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DecimalFilter NewDecimalFilter (pcFieldName AS CHARACTER,
                                                  poTableModelFilter AS TableModelFilter):
        
        RETURN NEW DecimalFilter (THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName),
                                  THIS-OBJECT,
                                  poTableModelFilter) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a New Int64Filter instance 
        Notes:   
        @param pcFieldName The name of the buffer field to filter on
        @param poTableModelFilter The reference to the TableModelFilter instance
        @return The Int64Filter instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Int64Filter NewInt64Filter (pcFieldName AS CHARACTER,
                                              poTableModelFilter AS TableModelFilter):
        
        RETURN NEW Int64Filter (THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName),
                                THIS-OBJECT,
                                poTableModelFilter) .

    END METHOD .
        
    /*------------------------------------------------------------------------------
        Purpose: Returns a New IntegerFilter instance 
        Notes:   
        @param pcFieldName The name of the buffer field to filter on
        @param poTableModelFilter The reference to the TableModelFilter instance
        @return The IntegerFilter instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IntegerFilter NewIntegerFilter (pcFieldName AS CHARACTER,
                                                  poTableModelFilter AS TableModelFilter):
    
        RETURN NEW IntegerFilter (THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName),
                                  THIS-OBJECT,
                                  poTableModelFilter) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a New LogicalFilter instance 
        Notes:   
        @param pcFieldName The name of the buffer field to filter on
        @param poTableModelFilter The reference to the TableModelFilter instance
        @return The LogicalFilter instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LogicalFilter NewLogicalFilter (pcFieldName AS CHARACTER,
                                                  poTableModelFilter AS TableModelFilter):
        
        RETURN NEW LogicalFilter (THIS-OBJECT:BufferHandle:BUFFER-FIELD (pcFieldName),
                                  THIS-OBJECT,
                                  poTableModelFilter) .

    END METHOD .    

    /*------------------------------------------------------------------------------
        Purpose: Reopens the child relation queries  
        Notes:   Used when navigating the current buffer using the GetFirst() and
                 GetNext() methods
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ReopenDatasetChildQueries ():
		
		DEFINE VARIABLE i AS INTEGER NO-UNDO.
		
		DO i = 1 TO THIS-OBJECT:BufferHandle:NUM-CHILD-RELATIONS:
		    THIS-OBJECT:BufferHandle:GET-CHILD-RELATION(i):QUERY:QUERY-OPEN () .
		    THIS-OBJECT:BufferHandle:GET-CHILD-RELATION(i):QUERY:GET-FIRST () . 
		END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Buffer name of the TableMode instance
        Notes:   
        @return The Buffer name  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC OVERRIDE CHARACTER ToString ():
		
		RETURN THIS-OBJECT:BufferHandle:NAME .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Ensures that the TableModel is using a local query on the buffer only 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID UseBufferQuery ():

        DEFINE VARIABLE hDatasetQuery AS HANDLE NO-UNDO.
        
        IF THIS-OBJECT:ModelType = TableModelTypeEnum:ViewModel THEN 
            UNDO, THROW NEW AppError ("The usage of the UseBufferQuery method is only allowed for Table Models."{&TRAN}, 0) .
        
        IF VALID-HANDLE (THIS-OBJECT:BufferHandle:PARENT-RELATION) THEN 
            hDatasetQuery = THIS-OBJECT:BufferHandle:PARENT-RELATION:QUERY .
        ELSE 
            hDatasetQuery = THIS-OBJECT:BufferHandle:DATASET:TOP-NAV-QUERY (THIS-OBJECT:BufferHandle:NAME) .

        IF THIS-OBJECT:QueryHandle = hDatasetQuery OR NOT VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN DO:
            THIS-OBJECT:QueryHandle       = QueryHelper:CreatePreparedQuery (THIS-OBJECT:BufferHandle) .
            THIS-OBJECT:UsingDatasetQuery = FALSE . 
        END. 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Ensures that the TableModel is using one of the ProDataset QUERY's 
        Notes:   Allows switching back to using the ProDataset QUERY associated with
                 the TableModel buffer after a Fill() invocation has switched to using 
                 a custom query
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID UseDatasetQuery ():
		
		DEFINE VARIABLE hDatasetQuery AS HANDLE NO-UNDO.
		
		IF THIS-OBJECT:ModelType = TableModelTypeEnum:ViewModel THEN 
		    UNDO, THROW NEW AppError ("The usage of the UseDatasetQuery method is only allowed for Table Models."{&TRAN}, 0) .
		
        IF VALID-HANDLE (THIS-OBJECT:BufferHandle:PARENT-RELATION) THEN 
            hDatasetQuery = THIS-OBJECT:BufferHandle:PARENT-RELATION:QUERY .
        ELSE 
            hDatasetQuery = THIS-OBJECT:BufferHandle:DATASET:TOP-NAV-QUERY (THIS-OBJECT:BufferHandle:NAME) .

        IF VALID-HANDLE (THIS-OBJECT:QueryHandle) AND THIS-OBJECT:QueryHandle <> hDatasetQuery THEN 
            DELETE OBJECT THIS-OBJECT:QueryHandle . 
            
        ASSIGN THIS-OBJECT:QueryHandle       = hDatasetQuery 
               THIS-OBJECT:UsingDatasetQuery = TRUE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the TableModel class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC TableModel ():

        IF THIS-OBJECT:ModelType = Consultingwerk.OERA.TableModelTypeEnum:ViewModel THEN DO: 
            
            GarbageCollectorHelper:DeleteObject (THIS-OBJECT:QueryHandle) .
            GarbageCollectorHelper:DeleteObject (THIS-OBJECT:BufferHandle) .

        END.
        
    END DESTRUCTOR .
    
END CLASS.
