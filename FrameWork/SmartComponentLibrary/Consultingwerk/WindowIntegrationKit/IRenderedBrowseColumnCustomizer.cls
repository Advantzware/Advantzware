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
    File        : IRenderedBrowseColumnCustomizer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 14 16:43:24 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                FROM ASSEMBLY . 
USING Progress.Lang.*                                FROM PROPATH .

INTERFACE Consultingwerk.WindowIntegrationKit.IRenderedBrowseColumnCustomizer:  
  
    /*------------------------------------------------------------------------------
        Purpose: Callback method into Service classes, allows the customization of 
                 UltraGridCells on the RenderedBrowseControl class
        Notes:   Allows customization of UltraGridColumn properties like 
                 ExcludeFromColumnChooser, ValueList    
                 Most likely the code here will make decisions based on the column 
                 name (poColumn:Key). This name will either be just the field name
                 (10.2B service pack 4 and before or 10.2B05 for non joined queries)
                 or in the form of TableName:ColumnName for 10.2B05 and joined browser 
                 queries.                                                         
        @param phProcedure The procedure handle that created the original BROWSE widget
        @param phBrowseHandle The browse handle that is the foundation for the RenderedBrowseControl
        @param poGrid The reference to the RenderedBrowseControl
        @param poColumn The Column. The callback should directly manipulate the Column properties
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignColumnProperties (phProcedure AS HANDLE,
                                               phBrowseHandle AS HANDLE,
                                               poGrid AS RenderedBrowseControl,
                                               poColumn AS UltraGridColumn).

END INTERFACE.