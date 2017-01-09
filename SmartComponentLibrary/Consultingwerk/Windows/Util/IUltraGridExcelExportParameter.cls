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
    File        : IUltraGridExcelExportParameter
    Purpose     : Options for the Excel Export Component
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : Mon Dec 10 21:39:40 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Util.*  FROM PROPATH .
USING Infragistics.Excel.*           FROM ASSEMBLY . 
USING Infragistics.Documents.Excel.* FROM ASSEMBLY . 
USING Progress.Lang.*                FROM PROPATH .

INTERFACE Consultingwerk.Windows.Util.IUltraGridExcelExportParameter:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the FileName for the Excel Export 
        Notes:   
    ------------------------------------------------------------------------------*/    
	DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the user should be prompted for retrieving additional
                 rows 
        Notes:   SmartComponent Library only
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY PromptForAdditionalRows AS LOGICAL NO-UNDO 
    GET.
    	
    /*------------------------------------------------------------------------------
        Purpose: Returns if the user should be prompted to overwrite an existing file 
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY PromptForOverwrite AS LOGICAL NO-UNDO 
    GET.
	
    /*------------------------------------------------------------------------------
        Purpose: Returns if the user should be prompted to overwrite an existing file 
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WorkbookFormat AS WorkbookFormat NO-UNDO 
    GET.
 	
END INTERFACE.
