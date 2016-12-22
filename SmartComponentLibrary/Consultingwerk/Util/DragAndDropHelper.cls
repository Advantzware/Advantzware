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
    File        : DragAndDropHelper
    Purpose     : 
    Syntax      : Static methods only, Protected constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 28 16:21:25 CET 2009
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Util.*  FROM PROPATH .
USING Progress.Lang.*        FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Progress.Util.*        FROM ASSEMBLY . 
USING System.IO.*            FROM ASSEMBLY .
USING System.Windows.Forms.* FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Util.DragAndDropHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor. 
        Notes:   There's no need to create instances of the helper classes                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED DragAndDropHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Calculates the file size (size of LONGCHAR) and returns this as an 
                 INTEGER array to be converted into a System.Byte[]                                                                        
        Notes:   
        @param plcContents The LONGCHAR to return the file size for                                                                     
        @return The file size in bytes as a INTEGER array
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC INTEGER EXTENT ConvertFileSize (plcContents AS LONGCHAR):
                
        RETURN DragAndDropHelper:ConvertFileSize (LENGTH(plcContents)).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Transforms a file size and returns this as INTEGER array to be 
                 converted into a System.Byte[]                                                                        
        Notes:     
        @param piFileSize The actual file size in bytes
        @return The file size in bytes as a INTEGER array
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC INTEGER EXTENT ConvertFileSize (piFileSize AS INT64):
        
        RETURN DragAndDropHelper:IntToByteArray (piFileSize) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a Drag & Drop DataObject for a File Drop Operation 
                 (Progress/WinForms application to Windows Explorer)                                                                       
        Notes:   The file is not already existing as a real file and the file contents
                 will be passed to the receiving application 
                 When dragging to the Windows Explorer, the Drag & Drop operation should
                 be started with the Link DragDropEffect
        @param pcFileName The file name for the target of the drag and drop operation
        @param plcFileContents The longchar with the contents of the file
        @return The System.Windows.Forms.DataObject with the data for the drag and drop operation                                                                    
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC System.Windows.Forms.DataObject GetDragDropDataObject (pcFileName      AS CHARACTER,
                                                                                plcFileContents AS LONGCHAR):
        
        DEFINE VARIABLE oTitle                     AS "System.Byte[]":U NO-UNDO .  
        DEFINE VARIABLE oFileGroupDescriptor       AS "System.Byte[]":U NO-UNDO .
        DEFINE VARIABLE oFileGroupDescriptorStream AS MemoryStream      NO-UNDO . 
        DEFINE VARIABLE oContentsByteArray         AS "System.Byte[]":U NO-UNDO .
        DEFINE VARIABLE oContentsStream            AS MemoryStream      NO-UNDO .         
        DEFINE VARIABLE iSizeArray                 AS INTEGER           NO-UNDO EXTENT .        
        DEFINE VARIABLE i                          AS INTEGER           NO-UNDO .
        
        oTitle = System.Text.Encoding:ASCII:GetBytes(pcFileName) . 
        
        oFileGroupDescriptor = {Consultingwerk/new-array.i System.Byte 336} . 
        
        oTitle:CopyTo(oFileGroupDescriptor, 76) .
        
        /* add the magic numbers! */

        oFileGroupDescriptor:SetValue (1 AS UNSIGNED-BYTE, 0) .
        oFileGroupDescriptor:SetValue (64 AS UNSIGNED-BYTE, 4) .
        oFileGroupDescriptor:SetValue (128 AS UNSIGNED-BYTE, 5) .
        
        ASSIGN iSizeArray = DragAndDropHelper:ConvertFileSize (plcFileContents) .
        
        IF EXTENT (iSizeArray) > 4 THEN 
            UNDO, THROW NEW AppError ("The filesize is too large."{&TRAN}, 0) . 
                
        /* set filesize */                
        DO i = 1 TO EXTENT (iSizeArray):        
            oFileGroupDescriptor:SetValue (iSizeArray[i] AS UNSIGNED-BYTE, 71 + i) . /* Filesize goes to byte 72..75 */        
        END.
               
        oFileGroupDescriptorStream = NEW MemoryStream(CAST(oFileGroupDescriptor, "System.Byte[]":U)) .
        
        oContentsByteArray = System.Text.Encoding:ASCII:GetBytes(plcFileContents) . 
    
        oContentsStream = NEW MemoryStream (oContentsByteArray) .

        /* Add everything to the dataobject */
        DEFINE VARIABLE oData AS System.Windows.Forms.DataObject NO-UNDO . 
        
        oData = NEW System.Windows.Forms.DataObject () .
        oData:SetData ("FileGroupDescriptor":U, oFileGroupDescriptorStream) . /* becomes title of file */         
        oData:SetData ("FileContents":U, oContentsStream) . /* becomes contents of the file */        

        RETURN oData . 

    END METHOD.
 
     /*------------------------------------------------------------------------------
        Purpose: Creates a Drag & Drop DataObject for a File Drop Operation 
                 (Progress/WinForms application to Windows Explorer)                                                                       
        Notes:   The file is not already existing as a real file and the file contents
                 will be passed to the receiving application 
                 When dragging to the Windows Explorer, the Drag & Drop operation should
                 be started with the Link DragDropEffect
        @param pcFileName The file name for the target of the drag and drop operation
        @param pmFileContents The MEMPTR with the data for the contents of the file
        @return The System.Windows.Forms.DataObject with the data for the drag and drop operation                                                                    
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC System.Windows.Forms.DataObject GetDragDropDataObject (pcFileName     AS CHARACTER,
                                                                                pmFileContents AS MEMPTR):
        
        DEFINE VARIABLE oTitle                     AS "System.Byte[]":U NO-UNDO .  
        DEFINE VARIABLE oFileGroupDescriptor       AS "System.Byte[]":U NO-UNDO .
        DEFINE VARIABLE oFileGroupDescriptorStream AS MemoryStream      NO-UNDO . 
        DEFINE VARIABLE oContentsByteArray         AS "System.Byte[]":U NO-UNDO .
        DEFINE VARIABLE oContentsStream            AS MemoryStream      NO-UNDO .         
        DEFINE VARIABLE iSizeArray                 AS INTEGER           NO-UNDO EXTENT .        
        DEFINE VARIABLE i                          AS INTEGER           NO-UNDO .
        
        oTitle = System.Text.Encoding:ASCII:GetBytes(pcFileName) . 
        
        oFileGroupDescriptor = {Consultingwerk/new-array.i System.Byte 336} . 
        
        oTitle:CopyTo(oFileGroupDescriptor, 76) .
        
        /* add the magic numbers! */

        oFileGroupDescriptor:SetValue (1 AS UNSIGNED-BYTE, 0) .
        oFileGroupDescriptor:SetValue (64 AS UNSIGNED-BYTE, 4) .
        oFileGroupDescriptor:SetValue (128 AS UNSIGNED-BYTE, 5) .
        
        ASSIGN iSizeArray = DragAndDropHelper:ConvertFileSize (GET-SIZE (pmFileContents)) .
        
        IF EXTENT (iSizeArray) > 4 THEN 
            UNDO, THROW NEW AppError ("The filesize is too large."{&TRAN}, 0) . 
                
        /* set filesize */                
        DO i = 1 TO EXTENT (iSizeArray):        
            oFileGroupDescriptor:SetValue (iSizeArray[i] AS UNSIGNED-BYTE, 71 + i) . /* Filesize goes to byte 72..75 */        
        END.
               
        oFileGroupDescriptorStream = NEW MemoryStream(CAST(oFileGroupDescriptor, "System.Byte[]":U)) .
        
        oContentsByteArray = Consultingwerk.Util.DataTypeHelper:MemptrToByteArray (pmFileContents) . 
    
        oContentsStream = NEW MemoryStream (oContentsByteArray) .

        /* Add everything to the dataobject */
        DEFINE VARIABLE oData AS System.Windows.Forms.DataObject NO-UNDO . 
        
        oData = NEW System.Windows.Forms.DataObject () .
        oData:SetData ("FileGroupDescriptor":U, oFileGroupDescriptorStream) . /* becomes title of file */         
        oData:SetData ("FileContents":U, oContentsStream) . /* becomes contents of the file */        

        RETURN oData . 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Creates a Drag & Drop DataObject for a File Drop Operation 
                 (Progress/WinForms application to Windows Explorer)                                                                      
        Notes:   The file names are supposed to be already existing as real files
                 on the file system    
        @param pcFileNames The ABL Character Array of file names for the drag and drop operation
        @return The System.Windows.Forms.DataObject with the data for the drap and drop operation                                                                 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC System.Windows.Forms.DataObject GetDragDropDataObject (pcFileNames AS CHARACTER EXTENT): 
 
        DEFINE VARIABLE oFiles AS "System.String[]":U NO-UNDO .
        
        oFiles = CAST(BOX (pcFileNames), "System.String[]":U) .
 
        DEFINE VARIABLE oData AS System.Windows.Forms.DataObject NO-UNDO . 
        
        oData = NEW System.Windows.Forms.DataObject () . 
        oData:SetData (System.Windows.Forms.DataFormats:FileDrop, oFiles) .
         
        RETURN oData . 
 
    END METHOD . 
 
    /*------------------------------------------------------------------------------
        Purpose: Returns an ABL CHARACTER array of file names in the passed in 
                 IDataObject                                                                        
        Notes:       
        @param poData The IDataObject with the data of a receiving drag and drop operation
        @return The ABL Character array with the file names                                                                
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC CHARACTER EXTENT GetDropFileNames (poData AS System.Windows.Forms.IDataObject):
        
        DEFINE VARIABLE oFileNames AS "System.String[]":U NO-UNDO . 

        IF NOT Consultingwerk.Util.DragAndDropHelper:IsFileDrop (poData) THEN 
            UNDO, THROW NEW AppError ("No FileDrop IDataObject passed into method GetDropFileNames."{&TRAN}, 0) . 

        oFileNames = CAST(poData:GetData (System.Windows.Forms.DataFormats:FileDrop), "System.String[]":U) .
        
        /* Convert .NET Array to ABL Array */         
        RETURN UNBOX (oFileNames) .        
            
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Converts the given INT64 value to an Array of Bytes (INTEGER 0..255)                                                                        
        Notes:   The maximum supported extent of the array is 8   
        @param piInteger The INT64 value to convert
        @return The ABL Integer array with which can be converted into a .NET byte array
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC INTEGER EXTENT IntToByteArray (piInteger AS INT64):
        
        DEFINE VARIABLE iArray  AS INTEGER NO-UNDO EXTENT 8 INIT 0 .
        DEFINE VARIABLE iReturn AS INTEGER NO-UNDO EXTENT . 
        
        DEFINE VARIABLE i       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iLeft   AS INTEGER NO-UNDO.
        
        ASSIGN iLeft = piInteger . 
        
        DO i = 1 TO 8:            
            ASSIGN iArray[i] = iLeft MODULO 256
                   iLeft     = (iLeft - iArray[i]) / 256 .  
            
            IF iLeft < 1 THEN LEAVE . 
        END.

        ASSIGN EXTENT (iReturn) = i .         
        
        DO i = 1 TO EXTENT (iReturn):
            ASSIGN iReturn[i] = iArray[i] .
        END.
                
        RETURN iReturn .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE when the IDataObject contains File Drop data.                                                                        
        Notes:     
        @param poData The IDataObject with the data of a receiving drag and drop operation
        @return True when the IDataObject contains file drop data                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL IsFileDrop (poData AS System.Windows.Forms.IDataObject):
        
        IF NOT VALID-OBJECT (poData) THEN 
          RETURN FALSE . 
        
        RETURN poData:GetDataPresent (System.Windows.Forms.DataFormats:FileDrop).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE when the IDataObject contains Text Drop data.                                                                        
        Notes:    
        @param poData The IDataObject with the data of a receiving drag and drop operation
        @return True when the IDataObject contains text drop data                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL IsTextDrop (poData AS System.Windows.Forms.IDataObject):
        
        IF NOT VALID-OBJECT (poData) THEN 
          RETURN FALSE . 
        
        RETURN poData:GetDataPresent (System.Windows.Forms.DataFormats:Text).

    END METHOD.
&ENDIF
END CLASS.
