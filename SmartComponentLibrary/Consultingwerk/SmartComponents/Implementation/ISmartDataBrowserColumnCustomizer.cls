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
    File        : ISmartDataBrowserColumnCustomizer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Aug 24 00:02:08 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                 FROM ASSEMBLY . 
USING Progress.Lang.*                                 FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Implementation.ISmartDataBrowserColumnCustomizer:  
  
    /*------------------------------------------------------------------------------
        Purpose: Callback method into Service classes, allows the customization of 
                 UltraGridCells on the SmartDataBrowser class
        Notes:   Allows customization of UltraGridColumn properties like 
                 ExcludeFromColumnChooser, ValueList    
                 Most likely the code here will make decisions based on the column 
                 name (poColumn:Key). This name will either be just the field name
                 (10.2B service pack 4 and before or 10.2B05 for non joined queries)
                 or in the form of TableName:ColumnName for 10.2B05 and joined browser 
                 queries.                                                         
        @param poGrid The reference to the SmartDataBrowserControl
        @param poColumn The Column. The callback should directly manipulate the Column properties
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignColumnProperties (poGrid AS SmartDataBrowser,
                                               poColumn AS UltraGridColumn).

END INTERFACE.
