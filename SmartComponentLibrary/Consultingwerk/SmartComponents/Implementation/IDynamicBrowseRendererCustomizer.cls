/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IDynamicBrowseRendererCustomizer
    Purpose     : Service interface that allows extension of the 
                  DynamicBrowserRenderer
    Syntax      : 
    Description : Typically based on the CustomProperties object as defined 
                  in the dsDynamicBrowser tables
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 26 14:08:58 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                 FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Implementation.IDynamicBrowseRendererCustomizer:  
  
    /*------------------------------------------------------------------------------
        Purpose: Allows assignment of custom properties to the Grid 
        Notes:   Typically based on properties in the ttBrowserProperties.CustomProperties
                 object reference. Developers may access the object instance using
                 CAST (hBrowserProperties::CustomProperties, <actual type>)
        @param poBrowser The reference to the SmartDataBrowser instance
        @param hBrowserProperties The handle of the current ttBrowserProperties record
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID AssignBrowserProperties (poBrowser AS SmartDataBrowser,
	                                            hBrowserProperties AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Allows assignment of custom properties to a Grid Column 
        Notes:   Typically based on properties in the ttColumnProperties.CustomProperties
                 object reference. Developers may access the object instance using
                 CAST (hColumnProperties::CustomProperties, <actual type>)
        @param poBrowser The reference to the SmartDataBrowser instance
        @param hBrowserProperties The handle of the current ttBrowserProperties record
        @param poColumn The reference to the UltraGridColumn instance
        @param hColumnProperties The handle of the current ttColumnProperties record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignColumnProperties (poBrowser AS SmartDataBrowser,
                                               hBrowserProperties AS HANDLE,
                                               poColumn AS UltraGridColumn,
                                               hColumnProperties AS HANDLE).
END INTERFACE.
