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
    File        : SmartBusinessEntityLookupSupport
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 07 12:14:16 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Framework.*      FROM PROPATH . 
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH . 
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .
USING System.Windows.Forms.*                          FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Support.SmartBusinessEntityLookupSupport: 

    {Consultingwerk/SmartComponents/Framework/ttSmartBusinessEntityLookup.i &ACCESS=STATIC}

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE SmartBusinessEntityLookupSupport ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a default file name for the lookup definition
        Notes:   
        @param poLookup The reference to a SmartBusinessEntityLookup instance
        @return The default file name for the lookup definition
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC CHARACTER DefaultLookupFileName (poLookup AS SmartBusinessEntityLookup):
        
        DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
                
        IF poLookup:LookupEntityName > "":U THEN DO:
            cName = ENTRY (NUM-ENTRIES (poLookup:LookupEntityName, ".":U), 
                           poLookup:LookupEntityName, 
                           ".":U) .
            
            RETURN SUBSTITUTE ("&1.lookup":U, cName) .
        END . 
        
        RETURN "":U . 
                                                   
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Exports a Lookup to a .lookup file for reuse
        Notes:   
        @param poLookup The SmartBusinessEntityLookup to export
        @param plOk Logical value indicating if the save was performed 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ExportLookupToFile (poLookup AS SmartBusinessEntityLookup,
                                                  OUTPUT plOk AS LOGICAL):
        
        DEFINE VARIABLE oSerializer     AS ISmartBusinessEntityLookupSerializer NO-UNDO . 
        DEFINE VARIABLE hBuffer         AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oSaveFileDialog AS System.Windows.Forms.SaveFileDialog  NO-UNDO . 
        DEFINE VARIABLE oDialogResult   AS System.Windows.Forms.DialogResult    NO-UNDO .
        
        ASSIGN plOk = FALSE . 
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poLookup """SmartBusinessEntityLookup"":U"} .

        oSaveFileDialog = NEW SaveFileDialog () .
        
        oSaveFileDialog:AddExtension = TRUE . 
        oSaveFileDialog:CheckFileExists = FALSE . 
        oSaveFileDialog:CheckPathExists = TRUE . 
        oSaveFileDialog:DefaultExt = "lookup":U . 
        oSaveFileDialog:FileName = SmartBusinessEntityLookupSupport:DefaultLookupFileName (poLookup) .
        oSaveFileDialog:Filter = "Lookup Definitions (*.lookup)|*.lookup|All Files (*.*)|*.*":U . 
        oSaveFileDialog:OverwritePrompt = TRUE . 
        oSaveFileDialog:RestoreDirectory = TRUE . 
        oSaveFileDialog:Title = "Export Lookup Definition" .
        
        WAIT-FOR oSaveFileDialog:ShowDialog () SET oDialogResult .
        
        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
        
        oSerializer = { Consultingwerk/get-service.i Consultingwerk.SmartComponents.Framework.ISmartBusinessEntityLookupSerializer
                                                     "NEW SmartBusinessEntityLookupSerializer ()"} . 
        
        EMPTY TEMP-TABLE ttSmartBusinessEntityLookup . 
        
        CREATE ttSmartBusinessEntityLookup .
        
        oSerializer:Serialize (poLookup, BUFFER ttSmartBusinessEntityLookup:HANDLE) .
        
        TEMP-TABLE ttSmartBusinessEntityLookup:WRITE-XML ("file":U,
                                                          oSaveFileDialog:FileName,
                                                          TRUE) . 
        
        ASSIGN plOk = TRUE . 

        FINALLY:
            IF VALID-OBJECT (oSaveFileDialog) THEN 
                oSaveFileDialog:Dispose() .
            
            EMPTY TEMP-TABLE ttSmartBusinessEntityLookup . 
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Imports a Lookup from a .lookup file 
        Notes:   
        @param poLookup The SmartBusinessEntityLookup to import to
        @param plOk Logical value indicating if a lookup was imported 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ImportLookupFromFile (poLookup AS SmartBusinessEntityLookup,
                                                    OUTPUT plOk AS LOGICAL):
        
        DEFINE VARIABLE oSerializer     AS ISmartBusinessEntityLookupSerializer NO-UNDO . 
        DEFINE VARIABLE hBuffer         AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oOpenFileDialog AS System.Windows.Forms.OpenFileDialog  NO-UNDO . 
        DEFINE VARIABLE oDialogResult   AS System.Windows.Forms.DialogResult    NO-UNDO .
        
        ASSIGN plOk = FALSE . 
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poLookup """SmartBusinessEntityLookup"":U"} .

        oOpenFileDialog = NEW OpenFileDialog () .
        
        oOpenFileDialog:AddExtension = TRUE . 
        oOpenFileDialog:CheckFileExists = TRUE . 
        oOpenFileDialog:CheckPathExists = TRUE . 
        oOpenFileDialog:DefaultExt = "lookup":U . 
        oOpenFileDialog:FileName = SmartBusinessEntityLookupSupport:DefaultLookupFileName (poLookup) .
        oOpenFileDialog:Filter = "Lookup Definitions (*.lookup)|*.lookup|All Files (*.*)|*.*":U . 
        oOpenFileDialog:RestoreDirectory = TRUE . 
        oOpenFileDialog:Title = "Import Lookup Definition" .
        
        WAIT-FOR oOpenFileDialog:ShowDialog () SET oDialogResult .
        
        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
        
        oSerializer = { Consultingwerk/get-service.i Consultingwerk.SmartComponents.Framework.ISmartBusinessEntityLookupSerializer
                                                     "NEW SmartBusinessEntityLookupSerializer ()"} . 
        
        EMPTY TEMP-TABLE ttSmartBusinessEntityLookup . 
        
        TEMP-TABLE ttSmartBusinessEntityLookup:READ-XML ("file":U,
                                                         oOpenFileDialog:FileName, ?, ?, ?) . 
        
        DO ON ERROR UNDO, THROW:
            FIND FIRST ttSmartBusinessEntityLookup . 
            
            oSerializer:Deserialize (BUFFER ttSmartBusinessEntityLookup:HANDLE, poLookup) .
    
            CATCH err AS Progress.Lang.Error :
                UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to read lookup definitions from file. ~n(&1)", 
                                                      err:GetMessage(1))) .                                  
            
            END CATCH.
        END.

        ASSIGN plOk = TRUE . 

        FINALLY:
            IF VALID-OBJECT (oOpenFileDialog) THEN 
                oOpenFileDialog:Dispose() .
            
            EMPTY TEMP-TABLE ttSmartBusinessEntityLookup . 
        END FINALLY.
    END METHOD .

END CLASS.
