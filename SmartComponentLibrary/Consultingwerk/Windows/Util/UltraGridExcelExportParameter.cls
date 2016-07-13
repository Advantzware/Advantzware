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
    File        : UltraGridExcelExportParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 10 21:42:21 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Windows.Util.*  FROM PROPATH .
USING Infragistics.Excel.*           FROM ASSEMBLY . 
USING Infragistics.Documents.Excel.* FROM ASSEMBLY . 
USING Progress.Lang.*                FROM PROPATH .

CLASS Consultingwerk.Windows.Util.UltraGridExcelExportParameter 
    IMPLEMENTS IUltraGridExcelExportParameter: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the FileName for the Excel Export 
        Notes:   
    ------------------------------------------------------------------------------*/    
	DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO INIT "":U
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if the user should be prompted for retrieving additional
                 rows 
        Notes:   SmartComponent Library only
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY PromptForAdditionalRows AS LOGICAL NO-UNDO  
    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN
    INIT TRUE
    &ELSE
    INIT FALSE
    &ENDIF
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the user should be prompted to overwrite an existing file 
        Notes:   
    ------------------------------------------------------------------------------*/    
	DEFINE PUBLIC PROPERTY PromptForOverwrite AS LOGICAL NO-UNDO INIT TRUE 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Get and sets the Excel file format that should be used by the export serivce 
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WorkbookFormat AS WorkbookFormat NO-UNDO 
    GET.
    SET.
    
	/*------------------------------------------------------------------------------
		Purpose: Constructor for the UltraGridExcelExportParameter class 																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UltraGridExcelExportParameter ():
		SUPER ().
		
		&IF PROVERSION BEGINS "10.2":U &THEN
        THIS-OBJECT:WorkbookFormat = Infragistics.Excel.WorkbookFormat:Excel2007 .
		&ELSE
		THIS-OBJECT:WorkbookFormat = Infragistics.Documents.Excel.WorkbookFormat:Excel2007 .
		&ENDIF
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraGridExcelExportParameter class                                                                      
        Notes:    
        @param pcFileName The value for the FileName property                                                                     
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraGridExcelExportParameter (pcFileName AS CHARACTER):
        THIS-OBJECT ().
        
        THIS-OBJECT:FileName = pcFileName .
        
    END CONSTRUCTOR.

END CLASS.
