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
    File        : EntityTable
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 02 21:59:39 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i} 

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.OERA.*       FROM PROPATH .
USING Consultingwerk.OERA.Enum.*  FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH . 
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.OERA.EntityTable 
    ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the RowState of the EntityTable instance
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY RowState AS RowStateEnum NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EntityTable class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC EntityTable ():
		SUPER ().
		
		THIS-OBJECT:RowState = RowStateEnum:RowUnmodified .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Copies the buffer values to the entity table's properties                                                                         
        Notes:
        @param phBuffer The handle of the source buffer                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ABSTRACT VOID AssignFromBuffer (phBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Copies the entity table's properties to the buffer values                                                                         
        Notes:                 
        @param phBuffer The handle of the target buffer                                                       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ABSTRACT VOID AssignToBuffer (phBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Returns the first (and typically only) child record of a to-one
                 relation                                                                        
        Notes:              
        @param phParentBuffer The handle to the parent buffer
        @param pcDataRelationName The name of the data-relation to return the child buffer for
        @return The handle of the child record, already located to the first record (if any)         
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED HANDLE GetChildRecord (phParentBuffer AS HANDLE,
                                            pcDataRelationName AS CHARACTER):
        
        DEFINE VARIABLE hDataRelation AS HANDLE  NO-UNDO.
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phParentBuffer ""BUFFER"":U} .
        
        ASSIGN hDataRelation = DatasetHelper:GetChildRelation (phParentBuffer, 
                                                               pcDataRelationName) . 
        
        IF NOT VALID-HANDLE (hDataRelation) THEN 
            UNDO, THROW NEW InvalidParameterValueException ("pcDataRelationName":U,
                                                            pcDataRelationName,
                                                            THIS-OBJECT:GetClass():TypeName) .
        
        hDataRelation:QUERY:QUERY-OPEN () .
        hDataRelation:QUERY:GET-FIRST () .
        
        RETURN hDataRelation:CHILD-BUFFER .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates a List of child records                                                                       
        Notes:   As we currently don't support "generic Interfaces", we can only access
                 the strong typed Add method of the Generic lists using the DYNAMIC-INVOKE
                 statement.                                                                        
        @param phParentBuffer The handle to the parent buffer
        @param pcDataRelationName The name of the data-relation to return the child buffer for
        @param poList The List to populate
        @param pcClassName The name of the ChildClass 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetChildRecords (phParentBuffer AS HANDLE,
                                           pcDataRelationName AS CHARACTER,
                                           poList AS Progress.Lang.Object,
                                           pcClassName AS CHARACTER):
        
        DEFINE VARIABLE hDataRelation AS HANDLE               NO-UNDO.
        DEFINE VARIABLE oInstance     AS Progress.Lang.Object NO-UNDO . 
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phParentBuffer ""BUFFER"":U} .
        
        ASSIGN hDataRelation = DatasetHelper:GetChildRelation (phParentBuffer, 
                                                               pcDataRelationName) . 
        
        IF NOT VALID-HANDLE (hDataRelation) THEN 
            UNDO, THROW NEW InvalidParameterValueException ("pcDataRelationName":U,
                                                            pcDataRelationName,
                                                            THIS-OBJECT:GetClass():TypeName) .
        
        hDataRelation:QUERY:QUERY-OPEN () .
        hDataRelation:QUERY:GET-FIRST () .

        DO WHILE NOT hDataRelation:QUERY:QUERY-OFF-END:
            oInstance = DYNAMIC-NEW (pcClassName) (hDataRelation:CHILD-BUFFER) .

            DYNAMIC-INVOKE (poList, "Add":U, oInstance) .
            
            hDataRelation:QUERY:GET-NEXT () .    
        END.         

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes a New row
        Notes:   Provides a method to provide initial values when records are added to 
                 differentiate from the Constructor usage when Rows are populate based 
                 on existing DB records
                 Override this method to provide initial values
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID InitializeNewRow ():
	    
	    THIS-OBJECT:RowState = RowStateEnum:RowCreated .
	END.

    /*------------------------------------------------------------------------------
        Purpose: Populates a List object with EntityTable objects for buffers 
        Notes:   Selects the buffer records using an optional Query string      
                 As we currently don't support "generic Interfaces", we can only access
                 the strong typed Add method of the Generic lists using the DYNAMIC-INVOKE
                 statement.                                                                  
        @param phBuffer The Buffer to return add EntityTable instances to the list
        @param pcQueryString The QueryString to use while populating the list
        @param poList The list to add the EntityTable instances to                               
        @param pcClassName The name of the class to create EntityTable instances of (child class of the EntityTable) 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID PopulateList (phBuffer AS HANDLE,
                                            pcQueryString AS CHARACTER,
                                            poList AS Progress.Lang.Object,
                                            pcClassName AS CHARACTER):
        
        DEFINE VARIABLE hQuery    AS HANDLE               NO-UNDO.
        DEFINE VARIABLE oInstance AS Progress.Lang.Object NO-UNDO . 
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poList """The list to populate""{&TRAN}"}. 
        
        IF pcQueryString > "":U THEN 
            ASSIGN hQuery = QueryHelper:CreatePreparedQuery (phBuffer, pcQueryString) .
        ELSE 
            ASSIGN hQuery = QueryHelper:CreatePreparedQuery (phBuffer) .
        
        DO WHILE NOT hQuery:QUERY-OFF-END:
            oInstance = DYNAMIC-NEW (pcClassName) (phBuffer) .

            DYNAMIC-INVOKE (poList, "Add":U, oInstance) .
            
            hQuery:GET-NEXT () .    
        END.        
        
        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .        
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates a List object with EntityTable objects for buffers                                                                      
        Notes:                                          
        @param phBuffer The Buffer to return add EntityTable instances to the list
        @param poList The list to add the EntityTable instances to    
        @param pcClassName The name of the class to create EntityTable instances of (child class of the EntityTable) 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID PopulateList (phBuffer AS HANDLE,
                                            poList AS Progress.Lang.Object,
                                            pcClassName AS CHARACTER):
        
        EntityTable:PopulateList (phBuffer, 
                                  ?, 
                                  poList, 
                                  pcClassName) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Set's the current row to modified
        Notes:   When the RowState is Add or Deleted it will not be changed
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SetModified ():
		
		IF THIS-OBJECT:RowState = RowStateEnum:RowUnmodified THEN 
		    THIS-OBJECT:RowState = RowStateEnum:RowModified .

	END METHOD .

END CLASS.
