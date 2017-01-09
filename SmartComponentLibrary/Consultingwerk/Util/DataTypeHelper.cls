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
    File        : DataTypeHelper
    Purpose     : 
    Syntax      : Static methods only, private constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 01 16:49:22 CET 2010
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*                 FROM PROPATH . 
USING Consultingwerk.Assertion.*       FROM PROPATH.
USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.Util.*            FROM PROPATH . 
USING Consultingwerk.Util.Exceptions.* FROM PROPATH . 
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.Util.DataTypeHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED DataTypeHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Converts a .NET System.Byte[] into an ABL Character value                                                                        
        Notes:                                                              
        @param poBytes The System.Bytes[] to convert
        @return The resulting ABL Character value          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ByteArrayToCharacter (poBytes AS "System.Byte[]":U):
        
        DEFINE VARIABLE oStream       AS System.IO.MemoryStream NO-UNDO.
        DEFINE VARIABLE oStreamReader AS System.IO.StreamReader NO-UNDO.

        oStream = NEW System.IO.MemoryStream (poBytes).
        
        oStream:Position = 0.
        oStreamReader = NEW System.IO.StreamReader (oStream).
        
        RETURN oStreamReader:ReadToEnd () .

        FINALLY:
            oStreamReader:Close() .
            oStream:Close() .        
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Converts a .NET Byte[] to an ABL MEMPTR
        Notes:   
        @param poBytes The System.Byte[] to convert to a MEMPTR
        @return The new MEMPTR with the data from the Byte[]
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC MEMPTR ByteArrayToMemptr (poBytes AS "System.Byte[]":U):
        
        DEFINE VARIABLE memptr      AS MEMPTR        NO-UNDO .
        DEFINE VARIABLE oIntPointer AS System.IntPtr NO-UNDO . 

        SET-SIZE (memptr) = poBytes:Length .

        oIntPointer = NEW System.IntPtr (GET-POINTER-VALUE (memptr)). 

        System.Runtime.InteropServices.Marshal:Copy (poBytes, 0, oIntPointer, poBytes:Length). 

        RETURN memptr .

        FINALLY: 
            DELETE OBJECT oIntPointer. 
        END FINALLY. 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Converts an ABL Charcater value into a .NET System.Byte[]                                                                         
        Notes:                                                              
        @param pcValue The Character value to convert
        @return The resulting System.Byte[]   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC "System.Byte[]":U CharacterToByteArray (pcValue AS CHARACTER):
        
        RETURN System.Text.Encoding:ASCII:GetBytes (pcValue) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Converts an ABL Longchar value into a .NET System.Byte[]                                                                         
        Notes:                                                              
        @param pcValue The Character value to convert
        @return The resulting System.Byte[]   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC "System.Byte[]":U CharacterToByteArray (pcValue AS LONGCHAR):
        
        RETURN System.Text.Encoding:ASCII:GetBytes (pcValue) .

    END METHOD.
&ENDIF
    
    /*------------------------------------------------------------------------------
       Purpose: Returns the ABL Default Format for the given data type
       Notes:   Creates a single field temp-table and returns the format of the field
       @param pcDataType The ABL DataType
       @return The default format for the data type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER DefaultFormat (pcDataType AS CHARACTER):
        
        DEFINE VARIABLE hTable AS HANDLE NO-UNDO.

        CREATE TEMP-TABLE hTable. 
        hTable:ADD-NEW-FIELD ("test":U, pcDataType) . 
        hTable:TEMP-TABLE-PREPARE ("ttTest":U) .

        RETURN hTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (1):FORMAT .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hTable) .		
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Returns the XML default data-type for the given ABL data type
       Notes:   Creates a single field temp-table and returns the XML-DATA-TYPE of the field
       @param pcDataType The ABL DataType
       @return The default XML data type for the data type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER DefaultXmlSchemaType (pcDataType AS CHARACTER):
        
        DEFINE VARIABLE hTable AS HANDLE NO-UNDO.

        CREATE TEMP-TABLE hTable. 
        hTable:ADD-NEW-FIELD ("test":U, pcDataType) . 
        hTable:TEMP-TABLE-PREPARE ("ttTest":U) .

        RETURN hTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (1):XML-DATA-TYPE .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hTable) .      
        END FINALLY.

    END METHOD.
    
&IF DEFINED (DotNetAccessible) NE 0 &THEN
&IF DEFINED (Infragistics) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Converts a Infragistics DefaultableBoolean to a Progress LOGICAL                                                                        
        Notes:   Maps DefaultableBoolean:Default to ?   
        @param poValue The DefaultableBoolean value to convert to LOGICAL            
        @return The equivalent LOGICAL value                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL FromDefaultableBoolean (poValue AS Infragistics.Win.DefaultableBoolean):
        
        IF Progress.Util.EnumHelper:AreEqual (poValue,  
                                              Infragistics.Win.DefaultableBoolean:True) 
            THEN RETURN TRUE . 
        IF Progress.Util.EnumHelper:AreEqual (poValue,  
                                              Infragistics.Win.DefaultableBoolean:False) 
            THEN RETURN FALSE . 
        IF Progress.Util.EnumHelper:AreEqual (poValue,  
                                              Infragistics.Win.DefaultableBoolean:Default) 
            THEN RETURN ? . 
        
    END METHOD.
&ENDIF
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns true when the passed in data type name is an ABL primitive type
        Notes:   
        @param pcDataType An Character value that should be tested for a valid ABL data type
        @return True when the passed in data type name is an ABL primitive type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsAblDataType (pcDataType AS CHARACTER):
        
        IF LOOKUP (pcDataType, Consultingwerk.Const:ABL_DATA_TYPES) > 0 THEN 
            RETURN TRUE . 
        ELSE 
            RETURN FALSE . 

    END METHOD .
    
&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Converts (copies) a Progress MEMPTR to a .NET System.Byte[]                                                                         
        Notes:          
        @param pmptr The ABL MEMPTR containing the source data
        @return A .NET System.Byte[] containing the same data                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC "System.Byte[]":U MemptrToByteArray (pmptr AS MEMPTR):
        
        DEFINE VARIABLE oReturn AS "System.Byte[]":U NO-UNDO .
        DEFINE VARIABLE iSize   AS INTEGER           NO-UNDO . 
        DEFINE VARIABLE i       AS INTEGER           NO-UNDO .
        
        ASSIGN iSize   = GET-SIZE (pmptr) 
               oReturn = {Consultingwerk/new-array.i System.Byte iSize} .

        DO i = 1 TO iSize:
            oReturn:SetValue (GET-BYTE (pmptr, i) AS UNSIGNED-BYTE, i - 1).
        END.

        RETURN oReturn . 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Converts a Progress MEMPTR (or a BLOB Image to a System.Drawing.Image)  
        Notes:   The caller is responsible for cleaning up the MEMPTR (SET-SIZE () = 0)
                 Orignal source code from http://knowledgebase.progress.com/articles/Article/P145740
        @param pmPtr The data to be converted to the Image
        @return The System.Drawing.Image from the data in the MEMPTR
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Drawing.Image MemPtrToImage (pmPtr AS MEMPTR):
        
        DEFINE VARIABLE iSize      AS INTEGER                NO-UNDO.
        DEFINE VARIABLE iLoop      AS INTEGER                NO-UNDO.
        DEFINE VARIABLE oMemStream AS System.IO.MemoryStream NO-UNDO.
        DEFINE VARIABLE oImage     AS System.Drawing.Image   NO-UNDO.
                  
        iSize = GET-SIZE (pmPtr).
         
        oMemStream = NEW System.IO.MemoryStream (iSize).
         
        DO iLoop = 1 TO iSize:
            oMemStream:WriteByte (GET-BYTE (pmPtr, iLoop)).
        END.
         
        DO ON ERROR UNDO, LEAVE:
            oImage = System.Drawing.Image:FromStream (oMemStream).
        END.

        RETURN oImage .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Converts a Progress.Data.DataType Enum member to the corresponding
                 ABL DataType in a Character representation                                                                       
        Notes:  
        @param poType The Progress.Data.DataType to convert into an ABL DataType
        @return The ABL DataType as a CHARACTER Value                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ToAblDataType (poType AS Progress.Data.DataType):
        
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:BLOB) THEN 
            RETURN "BLOB":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:CHARACTER) THEN 
            RETURN "CHARACTER":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:CLOB) THEN 
            RETURN "CLOB":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:COM_HANDLE) THEN 
            RETURN "COM-HANDLE":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:DATE) THEN 
            RETURN "DATE":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:DATETIME) THEN 
            RETURN "DATETIME":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:DATETIME_TZ) THEN 
            RETURN "DATETIME-TZ":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:DECIMAL) THEN 
            RETURN "DECIMAL":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:HANDLE) THEN 
            RETURN "HANDLE":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:INT64) THEN 
            RETURN "INT64":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:INTEGER) THEN 
            RETURN "INTEGER":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:LOGICAL) THEN 
            RETURN "LOGICAL":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:PROGRESS_LANG_OBJECT) THEN 
            RETURN "Progress.Lang.Object":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:RECID) THEN 
            RETURN "RECID":U .       
        IF Progress.Util.EnumHelper:AreEqual (poType, Progress.Data.DataType:ROWID) THEN 
            RETURN "ROWID":U .       
        RETURN ? . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Converts a System.Type to the corresponding ABL DataType in a Character 
                 representation                                                                       
        Notes:   Assumes CHARACTER for System.String, not LONGCHAR or BLOB
                 Returns BLOB for System.Byte[]
        @param poType The System.Type to convert into an ABL DataType
        @return The ABL DataType as a CHARACTER Value                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ToAblDataType (poType AS System.Type):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poType """poType"":U"} .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.String":U, TRUE)) 
            THEN RETURN "CHARACTER":U .        

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Boolean":U, TRUE)) 
            THEN RETURN "LOGICAL":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Byte":U, TRUE)) 
            THEN RETURN "INTEGER":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.SByte":U, TRUE)) 
            THEN RETURN "INTEGER":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.DateTime":U, TRUE)) 
            THEN RETURN "DATETIME":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Decimal":U, TRUE)) 
            THEN RETURN "DECIMAL":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Int16":U, TRUE)) 
            THEN RETURN "INTEGER":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.UInt16":U, TRUE)) 
            THEN RETURN "INTEGER":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Int32":U, TRUE)) 
            THEN RETURN "INTEGER":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.UInt32":U, TRUE)) 
            THEN RETURN "INTEGER":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Int64":U, TRUE)) 
            THEN RETURN "INT64":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.UInt64":U, TRUE)) 
            THEN RETURN "INT64":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Double":U, TRUE)) 
            THEN RETURN "DECIMAL":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Single":U, TRUE)) 
            THEN RETURN "DECIMAL":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Char":U, TRUE)) 
            THEN RETURN "CHARACTER":U .

        IF poType:IsAssignableFrom (Progress.Util.TypeHelper:GetType ("System.Byte[]":U, TRUE)) 
            THEN RETURN "BLOB":U .

        UNDO, THROW NEW SystemTypeNotMappableToAblTypeException (SUBSTITUTE ("System.Type &1 not mappable to ABL primitive type."{&TRAN}, 
                                                                             poType:FullName),
                                                                 poType:FullName) .

    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns an DATETIME value from a CHARACTER value 
        Notes:   Does return ? on invalid value
        @param pcString The string value 
        @return The date time value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC DATETIME ToDateTime (pcString AS CHARACTER):
        
        DEFINE VARIABLE dtValue AS DATETIME NO-UNDO.
        
        ASSIGN dtValue = DATETIME (pcString) .
        
        RETURN dtValue .
        
        CATCH err AS Progress.Lang.Error :
            RETURN ?.   
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an DECIMAL value from a CHARACTER value 
        Notes:   Does return ? on invalid value
        @param pcString The string value 
        @return The integer value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC DECIMAL ToDecimal (pcString AS CHARACTER):
        
        DEFINE VARIABLE deValue AS DECIMAL NO-UNDO.
        
        ASSIGN deValue = DECIMAL (pcString) .
        
        RETURN deValue .
        
        CATCH err AS Progress.Lang.Error :
            RETURN ?.   
        END CATCH.

    END METHOD .
        
&IF DEFINED (DotNetAccessible) NE 0 &THEN
&IF DEFINED (Infragistics) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Converts a Progress LOGICAL into an Infragistics DefaultableBoolean                                                                        
        Notes:   Maps ? to DefaultableBoolean:Default      
        @param plValue The LOGICAL value to convert to DefaultableBoolean            
        @return The equivalent Infragistics.Win.DefaultableBoolean enum value                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.DefaultableBoolean ToDefaultableBoolean (plValue AS LOGICAL):
        
        CASE plValue:
            WHEN TRUE THEN 
                RETURN Infragistics.Win.DefaultableBoolean:True .
            WHEN FALSE THEN 
                RETURN Infragistics.Win.DefaultableBoolean:False .
        END CASE . 

        RETURN Infragistics.Win.DefaultableBoolean:Default .
        
    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns the Progress.Data.DataType Enum member matching a Progress
                 DATA-TYPE                                                                      
        Notes:     
        @param pcDataType The ABL DataType to convert from
        @return The Progress.Data.DataType Value                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Progress.Data.DataType ToEnum (pcDataType AS CHARACTER):
        
        CASE pcDataType :
            WHEN "BLOB":U THEN                 RETURN Progress.Data.DataType:BLOB .
            WHEN "CHARACTER":U THEN            RETURN Progress.Data.DataType:CHARACTER .
            WHEN "CLOB":U THEN                 RETURN Progress.Data.DataType:CLOB .
            WHEN "COM-HANDLE":U THEN           RETURN Progress.Data.DataType:COM_HANDLE .
            WHEN "DATE":U THEN                 RETURN Progress.Data.DataType:DATE .
            WHEN "DATETIME":U THEN             RETURN Progress.Data.DataType:DATETIME .
            WHEN "DATETIME-TZ":U THEN          RETURN Progress.Data.DataType:DATETIME_TZ .
            WHEN "DECIMAL":U THEN              RETURN Progress.Data.DataType:DECIMAL .
            WHEN "HANDLE":U THEN               RETURN Progress.Data.DataType:HANDLE .
            WHEN "INT64":U THEN                RETURN Progress.Data.DataType:INT64 .
            WHEN "INTEGER":U THEN              RETURN Progress.Data.DataType:INTEGER .
            WHEN "LOGICAL":U THEN              RETURN Progress.Data.DataType:LOGICAL .
            WHEN "Progress.Lang.Object":U THEN RETURN Progress.Data.DataType:PROGRESS_LANG_OBJECT .
            WHEN "RECID":U THEN                RETURN Progress.Data.DataType:RECID .
            WHEN "ROWID":U THEN                RETURN Progress.Data.DataType:ROWID .
            OTHERWISE RETURN Progress.Data.DataType:UNKNOWN .
        END CASE.        

    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns an INT64 value from a CHARACTER value 
        Notes:   Does return ? on invalid value
        @param pcString The string value 
        @return The int64 value 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INT64 ToInt64 (pcString AS CHARACTER):
		
        DEFINE VARIABLE iValue AS INT64 NO-UNDO.
        
        ASSIGN iValue = INT64 (pcString) .
        
        RETURN iValue .
        
        CATCH err AS Progress.Lang.Error :
            RETURN ?.   
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an INTEGER value from a CHARACTER value 
        Notes:   Does return ? on invalid value
        @param pcString The string value 
        @return The integer value
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER ToInteger (pcString AS CHARACTER):
		
		DEFINE VARIABLE iValue AS INTEGER NO-UNDO.
		
        ASSIGN iValue = INTEGER (pcString) .
        
        RETURN iValue .
        
        CATCH err AS Progress.Lang.Error :
        	RETURN ?. 	
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Converts common string representations to a logical value (true/false/?)                                                                        
        Notes:                                                                        
        @param pcValue The CHARACTER expression to parse
        @return The equivalent LOGICAL value                                                           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL ToLogical (pcValue AS CHARACTER):

        IF pcValue = "TRUE":U OR pcValue = "YES":U OR pcValue = "1":U OR pcValue = "JA":U OR pcValue = "WAHR":U OR pcValue = "+":U THEN
            RETURN TRUE  .

        IF pcValue = "FALSE":U OR pcValue = "NO":U OR pcValue = "0":U OR pcValue = "NEIN":U OR pcValue = "FALSCH":U OR pcValue = "-":U THEN
            RETURN FALSE  .

        RETURN ? .

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Convers an primitive value (in an ABL Character) into a .NET 
                 System.Object 
        Notes:   Supports CHARACTER, DATE, DECIMAL, INTEGER, INT64, LOGICAL
        @param pcValue The ABL Value to convert
        @param pcDataType The ABL Primitive Data Type
        @return The .NET System.Object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Object ToSystemObject (pcValue AS CHARACTER,
                                                       pcDataType AS CHARACTER):
        
        CASE pcDataType:
            WHEN "CHARACTER":U THEN 
                RETURN BOX (pcValue) .
            WHEN "DATE":U THEN  
                RETURN BOX (DATE (pcValue)) .
            WHEN "DECIMAL":U THEN 
                RETURN BOX (DECIMAL (pcValue)) .
            WHEN "INTEGER":U THEN 
                RETURN BOX (INTEGER (pcValue)) .
            WHEN "INT64":U THEN 
                RETURN BOX (INT64 (pcValue)) .
            WHEN "LOGICAL":U THEN 
                RETURN BOX (DataTypeHelper:ToLogical(pcValue)) .
            OTHERWISE 
                UNDO, THROW NEW Consultingwerk.Exceptions.InvalidValueException (pcDataType, "pcDataType":U) . 
            
        END CASE . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the System.Typematching a Progress DATA-TYPE                                                                      
        Notes:     
        @param pcDataType The ABL DataType to convert from
        @return The System.Type reference
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Type ToSystemType (pcDataType AS CHARACTER):
        
        CASE pcDataType :
            WHEN "BLOB":U THEN                 RETURN Progress.Util.TypeHelper:GetType("System.Byte[]":U) .
            WHEN "CHARACTER":U THEN            RETURN Progress.Util.TypeHelper:GetType("System.String":U) .
            WHEN "CLOB":U THEN                 RETURN Progress.Util.TypeHelper:GetType("System.String":U) .
            WHEN "DATE":U THEN                 RETURN Progress.Util.TypeHelper:GetType("System.DateTime":U) .
            WHEN "DATETIME":U THEN             RETURN Progress.Util.TypeHelper:GetType("System.DateTime":U) .
            WHEN "DATETIME-TZ":U THEN          RETURN Progress.Util.TypeHelper:GetType("System.DateTime":U) .
            WHEN "DECIMAL":U THEN              RETURN Progress.Util.TypeHelper:GetType("System.Decimal":U) .
            WHEN "INT64":U THEN                RETURN Progress.Util.TypeHelper:GetType("System.Int64":U) .
            WHEN "INTEGER":U THEN              RETURN Progress.Util.TypeHelper:GetType("System.Int32":U) .
            WHEN "LOGICAL":U THEN              RETURN Progress.Util.TypeHelper:GetType("System.Boolean":U) .
            
            OTHERWISE 
                UNDO, THROW NEW InvalidParameterValueException ("pcDataType":U,
                                                                pcDataType,
                                                                "Consultingwerk.Util.DataTypeHelper":U).
        END CASE.     
    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns the unabbreviated ABL Data Type Name
        Notes:   Returns ? when the data type name is not an unabbreviated ABL data 
                 type name
        @param pcDataType The potentially abbreviated data type name
        @return The unabbreviated data type name or ? when the data type is not a valid abbreviation of an ABL data type name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER UnabbreviateDataTypeName (pcDataType AS CHARACTER):
        
        DEFINE VARIABLE cTypeName AS CHARACTER NO-UNDO EXTENT 15
            INIT ["BLOB":U, "CHARACTER":U, "CLOB":U, "COM-HANDLE":U, "DATE":U, 
                  "DATETIME":U, "DATETIME-TZ":U, "DECIMAL":U, "HANDLE":U, "INT64":U, 
                  "INTEGER":U, "LOGICAL":U, "RECID":U, "ROWID":U, "RAW":U] .

        DEFINE VARIABLE iMinAbbr AS INTEGER NO-UNDO EXTENT 15
            INIT [4, 4, 4, 10, 4,
                  8, 11, 3, 6, 5, 
                  3, 7, 5, 5, 3].
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE VARIABLE j AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (cTypeName):           
            
            IF pcDataType BEGINS SUBSTRING (cTypeName[i], 1, iMinAbbr[i]) THEN DO:
            
                DO j = iMinAbbr[i] TO LENGTH (pcDataType):
    
                    IF pcDataType = SUBSTRING (cTypeName[i], 1, j) THEN 
                        RETURN cTypeName[i] .
                END.
            END.
        END.
    
        RETURN ? . 

    END METHOD.

END CLASS.
