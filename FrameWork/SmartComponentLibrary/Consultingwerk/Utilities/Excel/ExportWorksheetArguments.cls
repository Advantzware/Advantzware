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
    File        : ExportWorksheetArguments
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : Thu Jun 14 13:02:25 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Utilities.Excel.ExportWorksheetArguments: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Number of rows to be Exported                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExportRows AS INTEGER NO-UNDO INIT ?  
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the FileName of the Excel Workbook to be imported                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
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
        Purpose: Gets and sets the Startrow of the Worksheet to be Exported                                                                    
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StartRow AS INTEGER NO-UNDO INIT 1 
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
        Purpose: Gets and sets the name of the Worksheet to be imported                                                                       
        Notes:   This property is used as an alternative to the WorksheetIndex property
                 When the WorksheetName is set, this property has preference                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY WorksheetName AS CHARACTER NO-UNDO INIT ? 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Default constructor for the ExportWorksheetArguments class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ExportWorksheetArguments ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ExportWorksheetArguments class                                                                        
        Notes:
        @param pcFileName The filename of the Worksheet to be imported                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ExportWorksheetArguments (pcFileName AS CHARACTER):
        SUPER ().
        
        THIS-OBJECT:FileName = pcFileName .  
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ExportWorksheetArguments class                                                                        
        Notes:
        @param pcFileName The filename of the Worksheet to be imported       
        @param pcWorksheetName The name of the worksheet to import                                                                 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ExportWorksheetArguments (pcFileName AS CHARACTER,
                                                 pcWorksheetName AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:FileName = pcFileName 
               THIS-OBJECT:WorksheetName = pcWorksheetName .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ExportWorksheetArguments class                                                                        
        Notes:
        @param pcFileName The filename of the Worksheet to be imported       
        @param piWorksheetIndex The index of the worksheet to import                                                                 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ExportWorksheetArguments (pcFileName AS CHARACTER,
                                                 piWorksheetIndex AS INTEGER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:FileName = pcFileName 
               THIS-OBJECT:WorksheetIndex = piWorksheetIndex . 
        
    END CONSTRUCTOR.

END CLASS.
