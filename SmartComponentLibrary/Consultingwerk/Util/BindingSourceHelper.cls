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
    File        : BindingSourceHelper
    Purpose     : 
    Syntax      : Static methods only, private constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 15 11:51:26 CEST 2010
    Notes       : This file is intended for GUI for .NET only 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*            FROM PROPATH . 
USING Consultingwerk.Assertion.*  FROM PROPATH . 
USING Consultingwerk.Exceptions.* FROM PROPATH . 
USING Consultingwerk.Framework.*  FROM PROPATH . 
USING Consultingwerk.Util.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Util.BindingSourceHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor. 
        Notes:   There's no need to create instances of the helper classes                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED BindingSourceHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN   
    /*------------------------------------------------------------------------------
        Purpose: Assigns a value to a BindingSource column 
        Notes:   As it is not directly possible to modify the BindingSource values, 
                 it is required to use a data bound Control. For this purpose the 
                 reference to a (loaded) Control needs to provided. The Tag property
                 of this Control will be used to Write the value to the BindingSource 
        @param poBindingSource The reference to the BindingSource 
        @param pcColumnName The name of the BindingSource Column to assign to
        @param poValue The value to assign to the column
        @param poControl The reference to the Control to use the Tag property for Binding to 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID AssignBindingSourceColumn (poBindingSource AS Progress.Data.BindingSource,
        	                                             pcColumnName AS CHARACTER,
        	                                             poValue AS System.Object,
        	                                             poControl AS System.Windows.Forms.Control):
		
        DEFINE VARIABLE oBinding AS System.Windows.Forms.Binding NO-UNDO .

		{Consultingwerk/Assertion/ObjectAssert/IsValid.i poBindingSource """Binding Source"":U"} .
		{Consultingwerk/Assertion/ObjectAssert/IsValid.i poControl """Control"":U"} .
		
        oBinding = poControl:DataBindings:Add ("Tag":U, poBindingSource, pcColumnName) .
        
        poControl:Tag = poValue .
        
        oBinding:WriteValue () . 

        poControl:DataBindings:Remove (oBinding) .
        poControl:Tag = ? . 

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oBinding) .    		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Assigns values from the ProBindingSource:InputValue dictionary to 
                 the fields in the buffer                                                                      
        Notes:   Code might need adjustment in 10.2B05 when the UseFullFieldNames 
                 propertiy is implemented 
        @param phBuffer The Buffer handle to assign to
        @param poBindingSource The ProBindingSource to assign from
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID AssignFromBindingSource (phBuffer AS HANDLE, 
                                                       poBindingSource AS Progress.Data.BindingSource):

        BindingSourceHelper:AssignFromBindingSource (phBuffer,
                                                     poBindingSource,
                                                     "":U, 
                                                     "*":U,
                                                     "":U) .
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Assigns values from the ProBindingSource:InputValue dictionary to 
                 the fields in the buffer                                                                      
        Notes:   Code might need adjustment in 10.2B05 when the UseFullFieldNames 
                 propertiy is implemented                                                                     
        @param phBuffer The Buffer handle to assign to
        @param poBindingSource The ProBindingSource to assign from
        @param pcNonUpdatableColumns The list of unupdatable columns form the Buffer
        @param pcIncludeFields The BindingSource's include field list
        @param pcExcludeFields The BindingSource'S exclude field list 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID AssignFromBindingSource (phBuffer AS HANDLE, 
                                                       poBindingSource AS Progress.Data.BindingSource,
                                                       pcNonUpdatableColumns AS CHARACTER,
                                                       pcIncludeFields AS CHARACTER,
                                                       pcExcludeFields AS CHARACTER):
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE VARIABLE j AS INTEGER NO-UNDO.

        DO i = 1 TO phBuffer:NUM-FIELDS ON ERROR UNDO, THROW:         
            /* Ability to mark certain Columns as non updatable */
            IF pcNonUpdatableColumns > "":U AND 
               ListHelper:EntryIsInList (phBuffer:BUFFER-FIELD (i):NAME, pcNonUpdatableColumns) THEN 
                NEXT . 
             
            /* When using Exclude Fields, skip those fields */
            IF pcIncludeFields = "*":U AND pcExcludeFields > "":U THEN
                IF ListHelper:EntryIsInList (phBuffer:BUFFER-FIELD (i):NAME, pcExcludeFields) OR
                   ListHelper:EntryIsInList (phBuffer:NAME + ".":U + phBuffer:BUFFER-FIELD (i):NAME, pcExcludeFields)
                THEN NEXT.

            /* When using Exclude Fields, skip those fields */
            IF pcIncludeFields <> "*":U AND pcIncludeFields > "":U THEN
                IF NOT ListHelper:EntryIsInList (phBuffer:BUFFER-FIELD (i):NAME, pcIncludeFields) AND
                   NOT ListHelper:EntryIsInList (phBuffer:NAME + ".":U + phBuffer:BUFFER-FIELD (i):NAME, pcIncludeFields)
                THEN NEXT.

            IF phBuffer:BUFFER-FIELD(i):EXTENT > 1 THEN DO:
                DO j = 1 TO phBuffer:BUFFER-FIELD(i):EXTENT:
                    ASSIGN phBuffer:BUFFER-FIELD(i):BUFFER-VALUE[j] = UNBOX (poBindingSource:InputValue [phBuffer:BUFFER-FIELD(i):NAME + "[":U + STRING(j) + "]":U]) .
                END.
            END.
            ELSE
                ASSIGN phBuffer:BUFFER-FIELD(i):BUFFER-VALUE = UNBOX (poBindingSource:InputValue [phBuffer:BUFFER-FIELD(i):Name]) .
                 
            CATCH e AS System.IndexOutOfRangeException:
                /* Ignore: Invalid fldName used with InputValue property: ... */
                IF FrameworkSettings:DebugMode THEN 
                    ErrorHelper:ShowErrorMessage (e) .
            END CATCH.
        END. 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the Buffer Field Name for a BindingSource field                                                                       
        Notes:   Optional support for UseFullFieldNames (10.2B05+)                                                                
        @param poBindingSource The BindingSource that contains the column
        @param pcColumnName The BindingSource column name
        @return The name of the bound buffer field      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER BindingBufferFieldName (poBindingSource AS Progress.Data.BindingSource,
                                                           pcColumnName AS CHARACTER):
        
        DEFINE VARIABLE hSource AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBuffer AS HANDLE  NO-UNDO.
        DEFINE VARIABLE i       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iIndex1 AS INTEGER NO-UNDO.
        DEFINE VARIABLE iIndex2 AS INTEGER NO-UNDO.
        DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.
        
        ASSIGN iIndex1 = INDEX (pcColumnName, "[":U) .
        
        IF iIndex1 > 0 THEN DO:
            ASSIGN iIndex2 = INDEX (pcColumnName, "]":U, iIndex1) .
            
            IF iIndex2 > 0 THEN 
                ASSIGN iExtent      = INTEGER (SUBSTRING (pcColumnName, iIndex1 + 1, iIndex2 - iIndex1 - 1))
                       pcColumnName = SUBSTRING (pcColumnName, 1, iIndex1 - 1) .
            
        END.
        
        IF NUM-ENTRIES (pcColumnName, ":":U) = 2 THEN 
            RETURN SUBSTITUTE ("&1.&2":U,
                               ENTRY (1, pcColumnName, ":":U),
                               ENTRY (2, pcColumnName, ":":U)) .
        
        ASSIGN hSource = poBindingSource:Handle .
        
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hSource """BindingSource Data Source (Handle)"":U"} .
        
        IF hSource:TYPE = "BUFFER":U THEN DO:
            Consultingwerk.Assertion.BufferAssert:HasField (hSource, pcColumnName) .
            
            IF iExtent = 0 THEN 
                RETURN SUBSTITUTE ("&1.&2":U, 
                                   hSource:NAME,
                                   pcColumnName) .
            ELSE 
                RETURN SUBSTITUTE ("&1.&2[&3]":U, 
                                   hSource:NAME,
                                   pcColumnName,
                                   iExtent) .                                   
        END.
        /* Query or ProDataset */
        ELSE DO:
            DO i = 1 TO hSource:NUM-BUFFERS:
                ASSIGN hBuffer = hSource:GET-BUFFER-HANDLE (i) .
                
                IF BufferHelper:HasField (hBuffer, pcColumnName) THEN DO:                        
                    IF iExtent = 0 THEN 
                        RETURN SUBSTITUTE ("&1.&2":U, 
                                           hBuffer:NAME,
                                           pcColumnName) .
                    ELSE 
                        RETURN SUBSTITUTE ("&1.&2[&3]":U, 
                                           hBuffer:NAME,
                                           pcColumnName,
                                           iExtent) .         
                END.
            END.
        END.
            
        UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to find column &1 in the BindingSource schema."{&TRAN}, pcColumnName), 
                                  0) .    
            
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Buffer Field Name for a BindingSource field                                                                       
        Notes:   Optional support for UseFullFieldNames (10.2B05+)
                 This implementation of the method is based on a ProDataset handle together
                 with the list of tables in question (e.g. the EntityTable,EntityView of
                 a SmartBusinessEntityAdapter)        
        @param phDataset The handle of the Dataset that is used as the Handle of the ProBindingSource                                                       
        @param pcColumnName The BindingSource column name
        @param pcBindingTables The comma delimited list of tables in the Binding Query (e.g. the EntityTable,EntityView of a SmartBusinessEntityAdapter)     
        @return The name of the bound buffer field      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER BindingBufferFieldName (phDataset AS HANDLE, 
                                                           pcColumnName AS CHARACTER,
                                                           pcBindingTables AS CHARACTER):
        
        DEFINE VARIABLE hBuffer AS HANDLE  NO-UNDO.
        DEFINE VARIABLE i       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iIndex1 AS INTEGER NO-UNDO.
        DEFINE VARIABLE iIndex2 AS INTEGER NO-UNDO.
        DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.
        
        ASSIGN iIndex1 = INDEX (pcColumnName, "[":U) .
        
        IF iIndex1 > 0 THEN DO:
            ASSIGN iIndex2 = INDEX (pcColumnName, "]":U, iIndex1) .
            
            IF iIndex2 > 0 THEN 
                ASSIGN iExtent      = INTEGER (SUBSTRING (pcColumnName, iIndex1 + 1, iIndex2 - iIndex1 - 1))
                       pcColumnName = SUBSTRING (pcColumnName, 1, iIndex1 - 1) .
            
        END.
        
        IF NUM-ENTRIES (pcColumnName, ":":U) = 2 THEN 
            RETURN SUBSTITUTE ("&1.&2":U,
                               ENTRY (1, pcColumnName, ":":U),
                               ENTRY (2, pcColumnName, ":":U)) .
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset Consultingwerk.WidgetTypeEnum:Dataset} .
        
        DO i = 1 TO NUM-ENTRIES (pcBindingTables):
            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (ENTRY (i, pcBindingTables)) .
            
            IF BufferHelper:HasField (hBuffer, pcColumnName) THEN DO:                        
                IF iExtent = 0 THEN 
                    RETURN SUBSTITUTE ("&1.&2":U, 
                                       hBuffer:NAME,
                                       pcColumnName) .
                ELSE 
                    RETURN SUBSTITUTE ("&1.&2[&3]":U, 
                                       hBuffer:NAME,
                                       pcColumnName,
                                       iExtent) .         
            END.
        END.
            
        UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to find column &1 in the BindingSource schema."{&TRAN}, pcColumnName), 
                                  0) .    
    END.    

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the BindingSource column for a Buffer Field  
        Notes:   Optional support for UseFullFieldNames (10.2B05+)      
                 Does not support BindingSources that are bound to a ProDataset directly
                 Supports BindingSources bound to a QUERY and a BUFFER                                                          
        @param poBindingSource The BindingSource that contains the field
        @param pcFieldName The fully qualified buffer field
        @return The name of the binding source column or ? when the column cannot be found      
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC CHARACTER BindingColumnName (poBindingSource AS Progress.Data.BindingSource,
	                                                  pcFieldName AS CHARACTER):

        DEFINE VARIABLE hDataHandle  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hBuffer      AS HANDLE    NO-UNDO EXTENT . 
        DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cColumnName  AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oBufferField AS BufferFieldName NO-UNDO .  

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poBindingSource """BindingSource"":U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcFieldName} .
		
		ASSIGN hDataHandle = poBindingSource:Handle .
		
		{Consultingwerk/Assertion/HandleAssert/ValidHandle.i hDataHandle """BindingSource Handle"":U"} .
		
        IF hDataHandle:TYPE = WidgetTypeEnum:Buffer OR
           hDataHandle:TYPE = WidgetTypeEnum:Query OR 
           hDataHandle:TYPE = WidgetTypeEnum:Dataset THEN .
        ELSE
            UNDO, THROW NEW InvalidTypeException ("Buffer/Query":U, hDataHandle:TYPE) .

        oBufferField = BufferHelper:ParseFieldName (pcFieldName) .
        
        IF CharacterType:IsNullOrEmpty (oBufferField:TableName) THEN 
            UNDO, THROW NEW InvalidValueException (pcFieldName). 
        
        IF BindingSourceHelper:UseFullFieldNames (poBindingSource) THEN DO:
            
            ASSIGN cColumnName = REPLACE (pcFieldName, ".":U, ":":U) . 
            
            IF VALID-OBJECT (BindingSourceHelper:FindColumnPropDesc (poBindingSource:TableSchema,
                                                                     cColumnName)) THEN 
                RETURN cColumnName .
            ELSE 
                RETURN ?  .
        END.
        
        IF hDataHandle:TYPE = WidgetTypeEnum:Query OR 
           hDataHandle:TYPE = WidgetTypeEnum:Dataset THEN DO:
            EXTENT (hBuffer) = hDataHandle:NUM-BUFFERS .

            DO i = 1 TO hDataHandle:NUM-BUFFERS:
                hBuffer [i] = hDataHandle:GET-BUFFER-HANDLE (i) .
            END.
        END.
        ELSE DO:
            EXTENT (hBuffer) = 1 .
            hBuffer[1] = hDataHandle .
        END.
        
        DO i = 1 TO EXTENT (hBuffer):
            
            /* When the buffer is not the field's buffer, but has a 
               column with that name, the binding source does not 
               contain the requested column */
            IF hBuffer[i]:NAME <> oBufferField:TableName THEN DO:
                
                IF BufferHelper:HasField (hBuffer[i], oBufferField:FieldName) THEN 
                    RETURN ? .
            END.
        
            /* When it's the proper buffer, locate the field */
            ELSE DO:
                ASSIGN cColumnName = oBufferField:FieldName .
                
                IF VALID-OBJECT (BindingSourceHelper:FindColumnPropDesc (poBindingSource:TableSchema,
                                                                         cColumnName)) THEN 
                    RETURN cColumnName .
                ELSE 
                    RETURN ?  .
            END.
        
        END.
        
        RETURN ? .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the ABL data-type the Control is bound to                                                                        
        Notes:   Returns the data type of the first binding             
        @param poControl The .NET Control that is bound to a ProBindingSource            
        @return The ABL data-type of the binding source field name the Control is bound to                                                          
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC CHARACTER BindingDataType (poControl AS System.Windows.Forms.Control):
		
        DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
               
        {Consultingwerk/foreach.i System.Windows.Forms.Binding oBinding in poControl:DataBindings}
        
            ASSIGN cField = oBinding:BindingMemberInfo:BindingField .
            
            IF TYPE-OF (oBinding:DataSource, Progress.Data.BindingSource) THEN 
                RETURN BindingSourceHelper:ColumnDataType (CAST (oBinding:DataSource, Progress.Data.BindingSource),
                                                           cField) . 
        END .

        RETURN ? .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the field name(s) the Control is bound to                                                                        
        Notes:   Code might need adjustment in 10.2B05 when the UseFullFieldNames 
                 property is implemented          
        @param poControl The .NET Control that is bound to a ProBindingSource            
        @return The field name the Control is bound to                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER BindingFieldName (poControl AS System.Windows.Forms.Control):
        
        DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
        
        {Consultingwerk/foreach.i System.Windows.Forms.Binding oBinding in poControl:DataBindings}
        
            ASSIGN cField = oBinding:BindingMemberInfo:BindingField .
        
            IF cField > "":U THEN 
                ASSIGN cFields = cFields + ",":U + cField . 
        END .

        RETURN TRIM (cFields, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a comma-delimited list of BLOBs and CLOBs in the 
                 Progress.Data.TableDesc schema                                                                        
        Notes:      
        @param poTableDesc The Progress.Data.TableDesc of the ProBindingSource
        @return The comma-delimited list of BLOB's and CLOB's in the Progress.Data.TableDesc                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER BindingSourceLargeColumns (poTableDesc AS Progress.Data.TableDesc):
        
        DEFINE VARIABLE cColumns AS CHARACTER                           NO-UNDO .
        DEFINE VARIABLE oColumns AS Progress.Data.ColumnPropDesc EXTENT NO-UNDO . 
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        ASSIGN oColumns = poTableDesc:Columns .
        
        DO i = 1 TO EXTENT (oColumns):

        IF Progress.Util.EnumHelper:AreEqual (oColumns[i]:ColumnType, Progress.Data.DataType:BLOB) OR 
           Progress.Util.EnumHelper:AreEqual (oColumns[i]:ColumnType, Progress.Data.DataType:CLOB) THEN 
           
            ASSIGN cColumns = cColumns + ",":U + oColumns[i]:ColumnName . 

        END.
        
        RETURN TRIM (cColumns, ",":U).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a comma-delimited list of BLOBs and CLOBs in Returns a 
                 comma-delimited list of BLOBs and CLOBs in the Progress.Data.TableDesc 
                 schema of the ProBindingSource                                                                  
        Notes:           
        @param poBindingSource The ProBindingSource to return the large columns for
        @return The comma-delimited list of BLOB's and CLOB's in the Progress.Data.TableDesc                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER BindingSourceLargeColumns (poBindingSource AS Progress.Data.BindingSource):
        
        RETURN Consultingwerk.Util.BindingSourceHelper:BindingSourceLargeColumns (poBindingSource:TableSchema) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the data-type of a column in the Progress.Data.TableDesc schema                          
        Notes:                                                  
        @param poTableDesc The Progress.Data.TableDesc of a ProBindingSource
        @param pcColumnName The name of the ProBindingSource Column
        @return The ABL corresponding ABL DataType of the ProBindingSource Column                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ColumnDataType (poTableDesc AS Progress.Data.TableDesc,
                                                   pcColumnName AS CHARACTER):
        
        DEFINE VARIABLE oColumns AS Progress.Data.ColumnPropDesc EXTENT NO-UNDO . 
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        ASSIGN oColumns = poTableDesc:Columns .
        
        DO i = 1 TO EXTENT (oColumns):

            IF oColumns[i]:ColumnName = pcColumnName THEN 
                RETURN Consultingwerk.Util.BindingSourceHelper:ConvertDataType(oColumns[i]:ColumnType) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the data-type of a column in the BindingSource's root
                 table                                                                      
        Notes:                                                                        
        @param poBindingSource The ProBindingSource
        @param pcColumnName The name of the ProBindingSource Column
        @return The ABL corresponding ABL DataType of the ProBindingSource Column                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ColumnDataType (poBindingSource AS Progress.Data.BindingSource,
                                                   pcColumnName AS CHARACTER):
        
        RETURN Consultingwerk.Util.BindingSourceHelper:ColumnDataType (poBindingSource:TableSchema,
                                                                       pcColumnName).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Converts a Progress.Data.DataType Enum member to the corresponding
                 ABL DataType in a Character representation                                                                       
        Notes:   Facade to Consultingwerk.Util.DataTypeHelper:ToABLDataType  
        @param poType The Progress.Data.DataType to convert into an ABL DataType
        @return The ABL DataType as a CHARACTER Value                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ConvertDataType (poType AS Progress.Data.DataType):
        
        RETURN Consultingwerk.Util.DataTypeHelper:ToABLDataType (poType) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Progress.Data.ColumnPropDesc of a TableDesc for the 
                 column with the given name
        Notes:   
        @param poTableDesc The Progress.Data.TableDesc that contains the ColumnPropDesc
        @param pcColumnName The name of the binding source column
        @return The ColumnPropDesc for the given column
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC Progress.Data.ColumnPropDesc FindColumnPropDesc (poTableDesc AS Progress.Data.TableDesc,
	                                                                      pcColumnName AS CHARACTER):
		
		{Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in poTableDesc:Columns}
		
		    IF oColumn:Name = pcColumnName THEN 
		        RETURN oColumn .
		END. 

        RETURN ? . 

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Returns the TableDesc of the BindingSource with the given name
	    Notes:   Recursively iterates the ChildTables
	    @param poBindingSource The reference to the binding source
	    @param pcName The name of the TableSpec to return
	    @return The TableDesc with the specified name or ? 
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC Progress.Data.TableDesc FindTableDesc (poBindingSource AS Progress.Data.BindingSource,
	                                                            pcName AS CHARACTER):
		
		IF poBindingSource:TableSchema:Name = pcName THEN
		    RETURN poBindingSource:TableSchema .

        RETURN BindingSourceHelper:FindTableDesc (poBindingSource:TableSchema,
                                                  pcName) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the TableDesc of the child of the given TableDesc referecne 
                 with the given name 
        Notes:   Recursively iterates the ChildTables 
        @param poTableDesc The reference to the table desc 
        @param pcName The name of the TableSpec to return
        @return The TableDesc with the specified name or ? 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC Progress.Data.TableDesc FindTableDesc (poTableDesc AS Progress.Data.TableDesc,
                                                                   pcName AS CHARACTER):
    
        DEFINE VARIABLE oChild AS Progress.Data.TableDesc NO-UNDO .
    
        {Consultingwerk/foreach.i Progress.Data.TableDesc oTableDesc in poTableDesc:ChildTables}
    
            IF oTableDesc:Name = pcName THEN 
                RETURN oTableDesc .  

            IF oTableDesc:ChildTables:Length > 0 THEN DO:  
                oChild = BindingSourceHelper:FindTableDesc (oTableDesc, pcName) .
                
                IF VALID-OBJECT (oChild) THEN 
                    RETURN oChild .                                                               
            END.
        END.
    
        RETURN ? . 
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a TableSchema for a Buffer                                                                        
        Notes: 
        @param phBuffer The Buffer object handle for which the TableSchema is requested
        @return The Progress.Data.TableDesc reference                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Progress.Data.TableDesc GetTableSchema (phBuffer AS HANDLE):
        
        DEFINE VARIABLE oColumns     AS Progress.Data.ColumnPropDesc NO-UNDO EXTENT .
        DEFINE VARIABLE i            AS INTEGER                      NO-UNDO.
        DEFINE VARIABLE j            AS INTEGER                      NO-UNDO.
        DEFINE VARIABLE n            AS INTEGER                      NO-UNDO.
        DEFINE VARIABLE hField       AS HANDLE                       NO-UNDO.
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer ""BUFFER"":U} .
        
        EXTENT (oColumns) = BufferHelper:TotalNumberOfFields(phBuffer) .  
        
        DO i = 1 TO phBuffer:NUM-FIELDS:
            
            ASSIGN hField = phBuffer:BUFFER-FIELD (i) . 
            
            IF hField:EXTENT = 0 THEN DO:
                j = j + 1 . 
            
                oColumns[j] = NEW Progress.Data.ColumnPropDesc 
                                    (hField:NAME,
                                     hField:COLUMN-LABEL,
                                     DataTypeHelper:ToEnum (hField:DATA-TYPE)).
            END .
            ELSE DO:
                DO n = 1 TO hField:EXTENT:
                    j = j + 1 . 
                
                    oColumns[j] = NEW Progress.Data.ColumnPropDesc 
                                        (SUBSTITUTE ("&1[&2]":U, hField:NAME, n),
                                         hField:COLUMN-LABEL,
                                         DataTypeHelper:ToEnum (hField:DATA-TYPE)).
                END. 
            END.
        END. 
        
        RETURN NEW Progress.Data.TableDesc (phBuffer:NAME, 
                                            oColumns,
                                            ?) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns logical value indicating if the ProBindingSource has a column
                 with the given name 
        Notes:   Name will be verified case-sensitive
        @param poBindingSource The reference to the BindingSource
        @param pcColumnName The name of the BindingSource column to verify
        @return Logical value indicating if the column name exists in the BindingSource schema
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC LOGICAL HasColumn (poBindingSource AS Progress.Data.BindingSource,
	                                        pcColumnName AS CHARACTER):
		
        DEFINE VARIABLE oDescriptorCollection AS System.ComponentModel.PropertyDescriptorCollection NO-UNDO.
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poBindingSource """poBindingSource"":U"} .		

        oDescriptorCollection = poBindingSource:GetItemProperties (?) .

        RETURN VALID-OBJECT (oDescriptorCollection:FIND (pcColumnName, FALSE)) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns based on the .NET Stacktrace if currently values on the 
                 screen are modified due to Data Binding 
        Notes:   
        @return Logical value indicating if currently values on the screen are modified due to Data Binding 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC LOGICAL IsBindingChangingValue ():
		
        /* Mike Fechner, Consultingwerk Ltd. 27.11.2014
           SCL-561 : Avoid setting Data Modified to early during screen initialization
                     when a field is focussed before loading data 
                     Ideally there would be a better way to exclude TextChanged beeing fired
                     when the BindingSource updates the bound control - but there does not 
                     seem to be one. So far we always relied on the Control beeing focussed.
                     However CCE has cases (see SCL-561) where the Control is focussed when 
                     the Query is opened and thus we did initialize the Update state without
                     a change caused by the user. */
        /* Mike Fechner, Consultingwerk Ltd. 13.02.2015
           SCL-661 : Need to look for System.Windows.Forms.Binding.PushData */
        DEFINE VARIABLE cStackTrace AS CHARACTER NO-UNDO.

        ASSIGN cStackTrace = System.Environment:StackTrace . 
                                      
        IF cStackTrace MATCHES "*Progress.Data.DataSource.OnListChanged*":U  OR 
           cStackTrace MATCHES "*System.Windows.Forms.Binding.PushData*":U 
            THEN RETURN TRUE .
        ELSE 
            RETURN FALSE .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the ProBindingSource is using the UseFullFieldNames setting                                                                        
        Notes:   For OpenEdge 10.2B04 and beyond this method does always return FALSE
                 For OpenEdge 10.2B05 and up this method uses reflection to query the property value                       
        @param poBindingSource The BindingSource to test UseFullFieldNames attribute on
        @return Logical value indicating if the ProBindingSource is using the UseFullFieldNames setting                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL UseFullFieldNames (poBindingSource AS Progress.Data.BindingSource):
        
        DEFINE VARIABLE iServicePack AS INTEGER NO-UNDO.
        
        IF PROVERSION BEGINS "10":U THEN DO:
            ASSIGN iServicePack = INTEGER (Consultingwerk.Util.SessionHelper:GetPatchLevel()) . 
            
            IF PROVERSION = ProVersionEnum:P102A OR (PROVERSION = ProVersionEnum:P102B AND iServicePack <= 4) THEN 
                RETURN FALSE . 
        END . 

        RETURN UNBOX (Consultingwerk.Util.ReflectionHelper:GetPropertyValue (poBindingSource, 
                                                                             "UseFullFieldNames":U)).

    END METHOD.
&ENDIF

END CLASS.
