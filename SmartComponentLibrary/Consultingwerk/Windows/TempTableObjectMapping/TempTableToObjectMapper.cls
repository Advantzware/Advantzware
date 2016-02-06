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
    File        : TempTableToObjectMapper
    Purpose     : Allows to copy from temp-table fields to .NET value objects
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 18 10:43:59 CET 2014
    Notes       : SCL-591
                  http://jira.consultingwerkcloud.com:8090/display/SCL/.NET+Value+object+to+ABL+Temp-Table+mapping
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                      FROM PROPATH.
USING Consultingwerk.Exceptions.*                     FROM PROPATH .
USING Consultingwerk.Framework.Collections.*          FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH.
USING Consultingwerk.Windows.TempTableObjectMapping.* FROM PROPATH . 
USING Progress.Lang.*                                 FROM PROPATH . 
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Collections.Generic.*                    FROM ASSEMBLY .
USING System.Reflection.*                             FROM ASSEMBLY . 

&ENDIF

CLASS Consultingwerk.Windows.TempTableObjectMapping.TempTableToObjectMapper
    IMPLEMENTS ITempTableToObjectMapper: 

&IF DEFINED (DotNetAccessible) NE 0 &THEN

    DEFINE VARIABLE cReflectedTypeName AS CHARACTER    NO-UNDO INIT ?.
    DEFINE VARIABLE oProperties        AS PropertyInfo NO-UNDO EXTENT . 
    DEFINE VARIABLE cPropertyTypeName  AS CHARACTER    NO-UNDO EXTENT .
    DEFINE VARIABLE cFieldNames        AS CHARACTER    NO-UNDO EXTENT . 
    DEFINE VARIABLE lEnum              AS LOGICAL      NO-UNDO EXTENT .

	/*------------------------------------------------------------------------------
	    Purpose: Assigns a Buffer Field from an Object Value
	    Notes:
	    @param phField The Buffer Field to assign to
	    @param pcPropertyName The name of the property 
	    @param poValue The value to assign to the buffer field 
	    @param poType The Property Type (required in case the oValue is null
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID AssignBufferField (phField AS HANDLE, 
	                                         pcPropertyName AS CHARACTER, 
	                                         poValue AS System.Object,
	                                         poType AS System.Type):
		
        DEFINE VARIABLE mptr          AS MEMPTR        NO-UNDO . 

        /* Special handling for Enums, either from Integer or String */
        IF Progress.Util.TypeHelper:GetType ("System.Enum":U, TRUE):IsAssignableFrom (poType) THEN DO:
            
            CASE phField:DATA-TYPE:
                WHEN "INTEGER":U THEN 
                    phField:BUFFER-VALUE = System.Convert:ToInt32 (poValue) .
                WHEN "CHARACTER":U THEN
                    phField:BUFFER-VALUE = poValue:ToString() .
                                                           
                OTHERWISE 
                    UNDO, THROW NEW InvalidFieldMappingForEnumException 
                                (SUBSTITUTE ("Unable to assign from enum &1 in &2 to ABL Field &3 (&4)"{&TRAN},
                                             pcPropertyName, poType:FullName,
                                             phField:NAME, phField:DATA-TYPE),
                                 pcPropertyName, poType:FullName,
                                 phField:NAME, phField:DATA-TYPE) .                                                                                                                                
            END.
        END .
        ELSE DO ON ERROR UNDO, THROW:
            IF phField:DATA-TYPE = "BLOB":U THEN DO:
                SET-SIZE (mptr) = 0 .
                mptr = DataTypeHelper:ByteArrayToMemptr (CAST (poValue, "System.Byte[]")) .
                COPY-LOB FROM mptr TO phField:BUFFER-VALUE  .
                SET-SIZE (mptr) = 0 .
            END.
            ELSE
                phField:BUFFER-VALUE = UNBOX (poValue) .  
        END.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Assigns all mapped fields in a buffer row
	    Notes:   When poBeforeValues is passed, field values will be taken from this Dictionary
	    @param poObject The object to assign values from 
	    @param phBuffer The buffer to assign values to
	    @param pcProperties The properties mapping
	    @param poBeforeValues The read only dictionary of before values (optionally) 
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID AssignRow (poObject AS System.Object,
                                     phBuffer AS HANDLE,
	                                 pcProperties AS CHARACTER,
&IF PROVERSION BEGINS "10.2":U &THEN
                                     poBeforeValues AS "System.Collections.Generic.IDictionary<CHARACTER,System.Object>"
&ELSE
                                     poBeforeValues AS "System.Collections.Generic.IReadOnlyDictionary<CHARACTER,System.Object>"
&ENDIF                                       
	                                 ):
		
        DEFINE VARIABLE i             AS INTEGER       NO-UNDO .
        DEFINE VARIABLE iEntries      AS INTEGER       NO-UNDO .

        DEFINE VARIABLE cEntry        AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cPropertyName AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cFieldName    AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE iIndex        AS INTEGER       NO-UNDO .
        DEFINE VARIABLE oType         AS System.Type   NO-UNDO . 
        DEFINE VARIABLE oProperty     AS PropertyInfo  NO-UNDO . 
        DEFINE VARIABLE iBracket1     AS INTEGER       NO-UNDO .
        DEFINE VARIABLE hField        AS HANDLE        NO-UNDO .
        DEFINE VARIABLE oValue        AS System.Object NO-UNDO . 
        DEFINE VARIABLE mptr          AS MEMPTR        NO-UNDO . 
		
        ASSIGN iEntries = NUM-ENTRIES (pcProperties) 
               oType    = poObject:GetType() .
		
        DO i = 1 TO iEntries:
            
            ASSIGN cEntry        = ENTRY (i, pcProperties) 
                   cPropertyName = ENTRY (1, cEntry, "|":U) .

            oProperty = oType:GetProperty (cPropertyName) .
            
            IF NOT VALID-OBJECT (oProperty) THEN 
                UNDO, THROW NEW InvalidPropertyNameException (SUBSTITUTE ("The property name &1 is not available in &2"{&TRAN},
                                                                          cPropertyName, oType:FullName),
                                                              cPropertyName,
                                                              oType:FullName) .

            IF NUM-ENTRIES (cEntry, "|":U) > 1 THEN DO:
                
                ASSIGN cEntry    = ENTRY (2, cEntry, "|":U) 
                       iIndex    = INDEX (cEntry, ".":U) .
                
                IF iIndex > 0 THEN 
                    ASSIGN cEntry = SUBSTRING (cEntry, iIndex + 1) .
                
                ASSIGN iBracket1 = INDEX (cEntry, "(":U).

                IF iBracket1 = 0 THEN 
                    ASSIGN cFieldName = cEntry .
                ELSE
                    ASSIGN cFieldName = SUBSTRING (cEntry, 1, iBracket1 - 1) .
            END.
            ELSE
                ASSIGN cFieldName = cPropertyName  .

            BufferAssert:HasField (phBuffer, cFieldName) .

            ASSIGN hField = phBuffer:BUFFER-FIELD (cFieldName) .
            
            IF hField:EXTENT > 1 THEN 
                UNDO, THROW NEW InvalidExtentFieldInFieldMappingException (SUBSTITUTE ("Unable to map buffer to object due to EXTENT/Array field &1 (&2) for property &3!"{&TRAN},
                                                                                       hField:NAME, hField:DATA-TYPE, cPropertyName),
                                                                           cPropertyName,
                                                                           hField:NAME, 
                                                                           hField:DATA-TYPE) .
            
            IF VALID-OBJECT (poBeforeValues) AND poBeforeValues:ContainsKey (cPropertyName) THEN 
                THIS-OBJECT:AssignBufferField (hField, 
                                               cPropertyName,
                                               poBeforeValues[cPropertyName], 
                                               oProperty:PropertyType) .
            ELSE 
                THIS-OBJECT:AssignBufferField (hField, 
                                               cPropertyName,
                                               oProperty:GetValue (poObject &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ), 
                                               oProperty:PropertyType) .
        END.        

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Copies the field values form the buffer to the value object
	    Notes:
	    @param phBuffer The handle of the source buffer
	    @param poObject The reference to the target object
	    @param pcProperties The field mapping in the same form as expected by the TempTableFromTypeFactory:CreateTempTable() method
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID CopyFromBuffer (phBuffer AS HANDLE, 
	                                   poObject AS System.Object,
	                                   pcProperties AS CHARACTER):
		
        DEFINE VARIABLE i             AS INTEGER       NO-UNDO .
        DEFINE VARIABLE iEntries      AS INTEGER       NO-UNDO .

        DEFINE VARIABLE cEntry        AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cPropertyName AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cFieldName    AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oType         AS System.Type   NO-UNDO . 
        DEFINE VARIABLE oProperty     AS PropertyInfo  NO-UNDO . 
        DEFINE VARIABLE hField        AS HANDLE        NO-UNDO .
        DEFINE VARIABLE oValue        AS System.Object NO-UNDO . 
        DEFINE VARIABLE mptr          AS MEMPTR        NO-UNDO . 
        DEFINE VARIABLE ex            AS Exception     NO-UNDO .   

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phBuffer """phBuffer"":U"} .
        ObjectAssert:IsValid (poObject, "poObject":U) .
        Assert:NotNullOrEmpty (pcProperties, "pcProperties":U) .

        /* reset stored reflection info */
        IF cReflectedTypeName <> ? AND cReflectedTypeName <> poObject:GetType():FullName THEN 
            THIS-OBJECT:Initialize () . 

        IF EXTENT (oProperties) = ? THEN 
            THIS-OBJECT:InitializeProperties (phBuffer, 
                                              poObject:GetType(),
                                              pcProperties) .

        ASSIGN iEntries = NUM-ENTRIES (pcProperties) 
               oType    = poObject:GetType() .

        DO i = 1 TO iEntries ON ERROR UNDO, THROW:
           
            ASSIGN cEntry        = ENTRY (i, pcProperties) 
                   cPropertyName = ENTRY (1, cEntry, "|":U) .

            oProperty = oProperties[i] .
            cFieldName = cFieldNames[i] .
            
            ASSIGN hField = phBuffer:BUFFER-FIELD (cFieldName) .

            /* Special handling for Enums, either from Integer or String */
            IF lEnum[i] THEN DO:
                
                CASE hField:DATA-TYPE:
                    WHEN "INTEGER":U THEN 
                        oProperty:SetValue (poObject,
                                            EnumHelperExt:FromInteger (oProperty:PropertyType, 
                                                                       hField:BUFFER-VALUE)
                                            &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF) .
                    WHEN "CHARACTER":U THEN 
                        oProperty:SetValue (poObject,
                                            System.Enum:Parse (oProperty:PropertyType,
                                                               hField:BUFFER-VALUE)
                                            &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF) .                                                               
                                                               
                    OTHERWISE 
                        UNDO, THROW NEW InvalidFieldMappingForEnumException 
                                    (SUBSTITUTE ("Unable to assign to enum &1 in &2 from ABL Field &3 (&4)"{&TRAN},
                                                 cPropertyName, oType:FullName,
                                                 cFieldName, hField:DATA-TYPE),
                                     cPropertyName, oType:FullName,
                                     cFieldName, hField:DATA-TYPE) .                                                                                                                                
                END.
            END .
            ELSE DO ON ERROR UNDO, THROW:
                IF hField:DATA-TYPE = "BLOB":U THEN DO:
                    SET-SIZE (mptr) = 0 . 
                    COPY-LOB FROM hField:BUFFER-VALUE TO mptr .
                    oValue = DataTypeHelper:MemptrToByteArray (mptr) .
                    SET-SIZE (mptr) = 0 .
                    oProperty:SetValue (poObject, oValue &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .
                END.
                    
                ELSE 
                    CASE cPropertyTypeName[i]:
                        WHEN "Double":U THEN
                            oProperty:SetValue (poObject, hField:BUFFER-VALUE AS DOUBLE &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .   
                        WHEN "Single":U THEN
                            oProperty:SetValue (poObject, hField:BUFFER-VALUE AS FLOAT &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .   
    
                        WHEN "Byte":U THEN
                            oProperty:SetValue (poObject, hField:BUFFER-VALUE AS UNSIGNED-BYTE &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .   
                        WHEN "SByte":U THEN
                            oProperty:SetValue (poObject, hField:BUFFER-VALUE AS BYTE &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .   
                        WHEN "Int16":U THEN
                            oProperty:SetValue (poObject, hField:BUFFER-VALUE AS SHORT &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .   
                        WHEN "UInt16":U THEN
                            oProperty:SetValue (poObject, hField:BUFFER-VALUE AS UNSIGNED-SHORT &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .   
                        WHEN "UInt32":U THEN
                            oProperty:SetValue (poObject, hField:BUFFER-VALUE AS UNSIGNED-INTEGER &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .   
    
                        WHEN "UInt64":U THEN
                            oProperty:SetValue (poObject, hField:BUFFER-VALUE AS UNSIGNED-INT64 &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .   
                        OTHERWISE 
                            oProperty:SetValue (poObject, BOX (hField:BUFFER-VALUE) &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF ) .    
                    END CASE .
            END.
            
            CATCH err AS Progress.Lang.Error:
                IF TYPE-OF (err, InvalidExtentFieldInFieldMappingException) THEN 
                    UNDO, THROW err . 
                
                IF VALID-HANDLE (hField) THEN 
                    ex = NEW Exception (err,
                                        SUBSTITUTE ("Unable to copy buffer field &1 (&2) to propery &3."{&TRAN},
                                                    hField:NAME, hField:DATA-TYPE, cPropertyName),
                                        0) .     
                ELSE 
                    ex = NEW Exception (err,
                                        SUBSTITUTE ("Unable to copy buffer field to propery &1."{&TRAN},
                                                    cPropertyName),
                                        0) .   

                IF err:NumMessages > 0 THEN 
                    ex:AddMessagesFrom (err) .
                                               
                UNDO, THROW ex.  
            END CATCH.    
        END.
    
	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Copies the field values form the buffer to the value object
        Notes:   Also for new records (plNewRecord = TRUE) we expect that in phBuffer 
                 a record has already been created
                 On 10.2B, the 4th parameter, is a IDictionary, as the IReadOnlyDictionary 
                 is not available on .NET 2.0. On OE11 the IReadOnlyDictionary provides
                 wider access to this method 
        @param poObject The reference to the target object
        @param phBuffer The handle of the source buffer
        @param pcProperties The field mapping in the same form as expected by the TempTableFromTypeFactory:CreateTempTable() method
        @param poBeforeValues An optional dictionary with values to be assigned to the before temp-table row 
        @param plNewRecord Logical value indicating if this routine will be used to create a new record
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID CopyFromObject (poObject AS System.Object,
	                                   phBuffer AS HANDLE, 
                                       pcProperties AS CHARACTER,
&IF PROVERSION BEGINS "10.2":U &THEN
                                       poBeforeValues AS "System.Collections.Generic.IDictionary<CHARACTER,System.Object>",
&ELSE
                                       poBeforeValues AS "System.Collections.Generic.IReadOnlyDictionary<CHARACTER,System.Object>",
&ENDIF                                       
                                       plNewRecord AS LOGICAL):
		
        DEFINE VARIABLE lTracking     AS LOGICAL       NO-UNDO INIT ?.

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phBuffer """phBuffer"":U"} .
        ObjectAssert:IsValid (poObject, "poObject":U) .
        Assert:NotNullOrEmpty (pcProperties, "pcProperties":U) .

        IF VALID-OBJECT (poBeforeValues) THEN DO:
            BufferAssert:IsDatasetBuffer (phBuffer) .
            
            ASSIGN lTracking = phBuffer:TABLE-HANDLE:TRACKING-CHANGES .
            
            IF phBuffer:ROW-STATE <> ? AND phBuffer:ROW-STATE <> ROW-UNMODIFIED THEN DO:
                BufferHelper:FindBeforeBuffer (phBuffer) .
                phBuffer:BEFORE-BUFFER:REJECT-ROW-CHANGES () .
            END.
            
            IF plNewRecord THEN DO:
                phBuffer:TABLE-HANDLE:TRACKING-CHANGES = TRUE  .
                phBuffer:MARK-ROW-STATE (ROW-CREATED) .
            END.
        END.

        /* When we have changes and we are not a new record, 
           assign from before values first (as change tracking 
           was turned off above */
        IF VALID-OBJECT (poBeforeValues)  AND plNewRecord = FALSE THEN DO:

            phBuffer:TABLE-HANDLE:TRACKING-CHANGES = FALSE .
            
            THIS-OBJECT:AssignRow (poObject,
                                   phBuffer, 
                                   pcProperties,
                                   poBeforeValues) .
            
            phBuffer:TABLE-HANDLE:TRACKING-CHANGES = TRUE .
            
        END.

        THIS-OBJECT:AssignRow (poObject,
                               phBuffer, 
                               pcProperties,
                               ?) .

        FINALLY:
            IF lTracking = FALSE THEN 
                phBuffer:TABLE-HANDLE:TRACKING-CHANGES = FALSE .		
        END FINALLY.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Finds the temp-table buffer based on the field values from the value object
        Notes:
        @param phBuffer The handle of the source buffer
        @param poObject The reference to the target object
        @param pcProperties The field mapping in the same form as expected by the TempTableFromTypeFactory:CreateTempTable() method
        @return Logical value indicating if the record was found
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FindBuffer (phBuffer AS HANDLE, 
                                      poObject AS System.Object,
                                      pcProperties AS CHARACTER):  

        RETURN THIS-OBJECT:FindBuffer (phBuffer, 
                                       poObject,
                                       ?, 
                                       pcProperties) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Finds the temp-table buffer based on the field values from the value object
        Notes:
        @param phBuffer The handle of the source buffer
        @param poObject The reference to the target object
        @param poMissingValues The dictionary of character values for index fields from the phBuffer not present in phValueBuffer
        @param pcProperties The field mapping in the same form as expected by the TempTableFromTypeFactory:CreateTempTable() method
        @return Logical value indicating if the record was found
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FindBuffer (phBuffer AS HANDLE, 
                                      poObject AS System.Object,
                                      poMissingValues AS CharacterDictionary, 
                                      pcProperties AS CHARACTER):  

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phBuffer """phBuffer"":U"} .
        ObjectAssert:IsValid (poObject, "poObject":U) .
        Assert:NotNullOrEmpty (pcProperties, "pcProperties":U) .

        /* reset stored reflection info */
        IF cReflectedTypeName <> ? AND cReflectedTypeName <> poObject:GetType():FullName THEN 
            THIS-OBJECT:Initialize () . 

        IF EXTENT (oProperties) = ? THEN 
            THIS-OBJECT:InitializeProperties (phBuffer, 
                                              poObject:GetType(),
                                              pcProperties) .

        DO ON ERROR UNDO, THROW:
            phBuffer:FIND-FIRST (THIS-OBJECT:UniqueFindPredicateFromObjectValues (phBuffer, poObject, poMissingValues)) .
    
            RETURN phBuffer:AVAILABLE .
            
            CATCH err AS Progress.Lang.Error:
            	/* ** FIND FIRST/LAST failed for table Customer. (565) */
            	IF err:GetMessageNum (1) = 565 THEN 
            	    RETURN FALSE .
            	ELSE 
            	    UNDO, THROW err .	
                
            END CATCH.
        END . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invalidates stored .NET reflection information 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID Initialize ():
		
		EXTENT (oProperties) = ? . 
        EXTENT (cPropertyTypeName) = ? . 
        EXTENT (cFieldNames) = ? .
        EXTENT (lEnum) = ? . 

        ASSIGN cReflectedTypeName = ? . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the .NET reflection information 
        Notes:   
        @param phBuffer The handle of the source buffer
        @param poType The .NET Type for the target objects
        @param pcProperties The field mapping in the same form as expected by the TempTableFromTypeFactory:CreateTempTable() method
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InitializeProperties (phBuffer AS HANDLE,
	                                            poType AS System.Type,
	                                            pcProperties AS CHARACTER):
		
        DEFINE VARIABLE iEntries      AS INTEGER       NO-UNDO .
        DEFINE VARIABLE cEntry        AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE iIndex        AS INTEGER       NO-UNDO .
        DEFINE VARIABLE cPropertyName AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER       NO-UNDO .
        DEFINE VARIABLE iBracket1     AS INTEGER       NO-UNDO .
        DEFINE VARIABLE hField        AS HANDLE        NO-UNDO .
		
        ASSIGN iEntries = NUM-ENTRIES (pcProperties) . 
		
		EXTENT (oProperties) = iEntries . 
		EXTENT (cPropertyTypeName) = iEntries .
		EXTENT (cFieldNames) = iEntries . 
		EXTENT (lEnum) = iEntries . 
	
	    ASSIGN cReflectedTypeName = poType:FullName . 
		
        DO i = 1 TO iEntries:

            ASSIGN cEntry        = ENTRY (i, pcProperties) 
                   cPropertyName = ENTRY (1, cEntry, "|":U) .
		
            oProperties[i] = poType:GetProperty (cPropertyName) .
		
            IF NOT VALID-OBJECT (oProperties[i]) THEN 
                UNDO, THROW NEW InvalidPropertyNameException (SUBSTITUTE ("The property name &1 is not available in &2."{&TRAN},
                                                                          cPropertyName, poType:FullName),
                                                              cPropertyName,
                                                              poType:FullName) .

            IF NUM-ENTRIES (cEntry, "|":U) > 1 THEN DO:
                
                ASSIGN cEntry    = ENTRY (2, cEntry, "|":U) 
                       iIndex    = INDEX (cEntry, ".":U) .
                
                IF iIndex > 0 THEN 
                    ASSIGN cEntry = SUBSTRING (cEntry, iIndex + 1) .
                
                ASSIGN iBracket1 = INDEX (cEntry, "(":U).

                IF iBracket1 = 0 THEN 
                    ASSIGN cFieldNames[i] = cEntry .
                ELSE
                    ASSIGN cFieldNames[i] = SUBSTRING (cEntry, 1, iBracket1 - 1) .
            END.
            ELSE
                ASSIGN cFieldNames[i] = cPropertyName  .

            BufferAssert:HasField (phBuffer, cFieldNames[i]) .
	
	        ASSIGN hField = phBuffer:BUFFER-FIELD (cFieldNames[i]) .
	
            IF hField:EXTENT > 1 THEN 
                UNDO, THROW NEW InvalidExtentFieldInFieldMappingException (SUBSTITUTE ("Unable to map buffer to object due to EXTENT/Array field &1 (&2) for property &3!"{&TRAN},
                                                                                       hField:NAME, hField:DATA-TYPE, cPropertyName),
                                                                           cPropertyName,
                                                                           hField:NAME, 
                                                                           hField:DATA-TYPE) .
		    
		    ASSIGN lEnum[i] = Progress.Util.TypeHelper:GetType ("System.Enum":U, TRUE):IsAssignableFrom 
		                         (oProperties[i]:PropertyType) 
		           cPropertyTypeName[i] = oProperties[i]:PropertyType:Name. 
		END. 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a find string for the buffer based on unqiue indexes of the 
                 buffer and the values of the object
        Notes:   It is expected, that InitializeProperties has been executed prior to
                 executing this method
        @param phBuffer The handle of the buffer to locate
        @param poObject The object with the mapped properties
        @param poMissingValues The dictionary of character values for index fields from the phBuffer not present in phValueBuffer
        @return The find string
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER UniqueFindPredicateFromObjectValues (phBuffer AS HANDLE, 
	                                                                poObject AS System.Object,
	                                                                poMissingValues AS CharacterDictionary):
		
        DEFINE VARIABLE iIndex       AS INTEGER      NO-UNDO .
        DEFINE VARIABLE cIndex       AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE iField       AS INTEGER      NO-UNDO .
        DEFINE VARIABLE cField       AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE cQueryString AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE lFound       AS LOGICAL      NO-UNDO .
        DEFINE VARIABLE hIndexBuffer AS HANDLE       NO-UNDO .
        DEFINE VARIABLE cValue       AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE iPosition    AS INTEGER      NO-UNDO .   
        DEFINE VARIABLE oProperty    AS PropertyInfo NO-UNDO .            
               
        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN 
            UNDO, THROW NEW AppError ("The method UniqueFindPredicateFromObjectValues needs to be executed using a valid buffer handle."{&TRAN}, 0) .

        IF NOT VALID-OBJECT (poObject) THEN 
            UNDO, THROW NEW AppError ("The method UniqueFindPredicateFromObjectValues needs to be executed using a valid value object."{&TRAN}, 0) .
        
        /* Mike Fechner, Consultingwerk Ltd. 26.11.2010
           When we are working on the BEFORE-BUFFER, the BEFORE-BUFFER has no
           indexes, then use the AFTER-BUFFER for the indexes */
        IF VALID-HANDLE (phBuffer:AFTER-BUFFER) THEN hIndexBuffer = phBuffer:AFTER-BUFFER . 
        ELSE hIndexBuffer = phBuffer .
                      
        indexloop: REPEAT ON ERROR UNDO, THROW:
            ASSIGN iIndex = iIndex + 1
                   cIndex = hIndexBuffer:INDEX-INFORMATION (iIndex) .
                   
            IF cIndex > "":U THEN . 
            ELSE LEAVE .

            IF ENTRY(2, cIndex) <> "1":U /* unique */ OR 
               ENTRY(3, cIndex) <> "1":U /* primary */ THEN 
               NEXT .                          
        
            ASSIGN cQueryString = "WHERE ":U . 
        
            DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                ASSIGN cField    = ENTRY(iField, cIndex) 
                       iPosition = ArrayHelper:Find (cField, cFieldNames) .

                IF iPosition > 0 THEN 
                    ASSIGN oProperty = oProperties[iPosition] 
                           cValue = oProperty:GetValue (poObject &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF)  . 
                ELSE IF VALID-OBJECT (poMissingValues) AND poMissingValues:ContainsKey (cField) THEN 
                    ASSIGN cValue = QUOTER (poMissingValues:GetValue (cField)) .
                ELSE 
                    UNDO, THROW NEW UnmappedUniqueIndexFieldException ("Unable to build unique find predicate. Value for index field &1.&2 is not provided."{&TRAN},
                                                                       cField,
                                                                       poObject:GetType():FullName) .
                       
                IF iField > 5 THEN  
                    ASSIGN cQueryString = cQueryString + " AND ":U .   
                
                ASSIGN cQueryString = cQueryString + 
                                      cField + " = ":U + cValue .              
            END.  
            
            LEAVE .      
        END.

        /* Mike Fechner, Consultingwerk Ltd. 08.01.2012
           If QueryString is empty here, the table does not 
           have a primary unique index, so try the first unique 
           index */
        IF cQueryString = "":U THEN DO:
            iIndex = 0 . 
            
            indexloop: REPEAT ON ERROR UNDO, THROW:
                ASSIGN iIndex = iIndex + 1
                       cIndex = hIndexBuffer:INDEX-INFORMATION (iIndex) .
                       
                IF cIndex > "":U THEN . 
                ELSE LEAVE .
    
                IF ENTRY(2, cIndex) <> "1":U /* unique */ THEN 
                   NEXT .                          
            
                ASSIGN cQueryString = "WHERE ":U . 
            
                DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:

                    ASSIGN cField    = ENTRY(iField, cIndex) 
                           iPosition = ArrayHelper:Find (cField, cFieldNames) .
    
                    IF iPosition > 0 THEN 
                        ASSIGN oProperty = oProperties[iPosition] 
                               cValue = oProperty:GetValue (poObject &IF PROVERSION BEGINS "10.2":U &THEN ,? &ENDIF)  . 
                    ELSE IF VALID-OBJECT (poMissingValues) AND poMissingValues:ContainsKey (cField) THEN 
                        ASSIGN cValue = QUOTER (poMissingValues:GetValue (cField)) .
                    ELSE 
                        UNDO, THROW NEW UnmappedUniqueIndexFieldException ("Unable to build unique find predicate. Value for index field &1.&2 is not provided."{&TRAN},
                                                                           cField,
                                                                           poObject:GetType():FullName) .
                           
                    IF iField > 5 THEN  
                        ASSIGN cQueryString = cQueryString + " AND ":U .   
                    
                    ASSIGN cQueryString = cQueryString + 
                                          cField + " = ":U + cValue .              
                END.              
            
                LEAVE .
            END.   
        END.     
        
        RETURN cQueryString . 

	END METHOD .

&ENDIF

END CLASS.
