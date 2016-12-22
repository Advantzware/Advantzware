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
    File        : ImportWorksheetArguments
    Purpose     : Parameter class for the Import method of the ExcelWorksheetImporter
                  class         
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Feb 26 12:26:08 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Utilities.Excel.ImportWorksheetArguments: 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the FileName of the Excel Workbook to be imported                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Number of rows to be imported                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ImportRows AS INTEGER NO-UNDO INIT ?  
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the number of importeds rows after which the 
                 ExcelWorksheetImported instance should raise the ProgressChanged
                 event                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RaiseProgressChangedAfter AS INTEGER NO-UNDO INIT ? 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the number of rows from the top of the document that 
                 should be skipped (column header etc.)            
        Notes:   Defaults to 1                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SkipRowsFromTop AS INTEGER NO-UNDO INIT 1 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the index (number) of the Worksheet to be imported                                                                       
        Notes:   This property is used as an alternative to the WorksheetName property
                 1 based index                                                                         
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY WorksheetIndex AS INTEGER NO-UNDO INIT 1 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Number of empty rows after which the import will  
                 be stopped                                                                                        
        Notes:   Defaults to 1, use ? to turn off                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StopAfterEmptyRows AS INTEGER NO-UNDO INIT 1  
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Worksheet to be imported                                                                       
        Notes:   This property is used as an alternative to the WorksheetIndex property
                 When the WorksheetName is set, this property has preference                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY WorksheetName AS CHARACTER NO-UNDO INIT ? 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Default constructor for the InportImportWorksheetsArguments class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ImportWorksheetArguments ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InportImportWorksheetsArguments class                                                                        
        Notes:
        @param pcFileName The filename of the Worksheet to be imported                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ImportWorksheetArguments (pcFileName AS CHARACTER):
        SUPER ().
        
        THIS-OBJECT:FileName = pcFileName .  
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InportImportWorksheetsArguments class                                                                        
        Notes:
        @param pcFileName The filename of the Worksheet to be imported       
        @param pcWorksheetName The name of the worksheet to import                                                                 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ImportWorksheetArguments (pcFileName AS CHARACTER,
                                                 pcWorksheetName AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:FileName = pcFileName 
               THIS-OBJECT:WorksheetName = pcWorksheetName .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InportImportWorksheetsArguments class                                                                        
        Notes:
        @param pcFileName The filename of the Worksheet to be imported       
        @param piWorksheetIndex The index of the worksheet to import                                                                 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ImportWorksheetArguments (pcFileName AS CHARACTER,
                                                 piWorksheetIndex AS INTEGER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:FileName = pcFileName 
               THIS-OBJECT:WorksheetIndex = piWorksheetIndex . 
        
    END CONSTRUCTOR.

END CLASS.
