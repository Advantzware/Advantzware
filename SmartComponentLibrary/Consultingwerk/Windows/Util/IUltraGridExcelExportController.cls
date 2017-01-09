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
    File        : IUltraGridExcelExportController
    Purpose     : Describes functionality required to perform an UltraGrid 
                  Excel export
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 10 21:46:22 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Util.*   FROM PROPATH .
USING Infragistics.Documents.Excel.*  FROM ASSEMBLY . 
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .  
USING Progress.Lang.*                 FROM PROPATH .

INTERFACE Consultingwerk.Windows.Util.IUltraGridExcelExportController:  

    /*------------------------------------------------------------------------------
        Purpose: Exports an UltraGrid      
        Notes:      
        @param poGrid The UltraGrid to export
        @return The filename of the exported document
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Export (poGrid AS UltraGrid).

    /*------------------------------------------------------------------------------
        Purpose: Exports an UltraGrid                                                                     
        Notes:                                                                        
        @param poGrid The UltraGrid to export
        @param poOptions The IUltraGridExcelExportParameter for this export                                                               
        @return The filename of the exported document
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Export (poGrid AS UltraGrid,
                                    poOptions AS IUltraGridExcelExportParameter).

END INTERFACE.
