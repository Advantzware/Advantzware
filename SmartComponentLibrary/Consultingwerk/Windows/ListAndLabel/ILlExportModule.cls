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
    File        : ILlExportModule
    Purpose     : Describes requirements for List & Label export modules
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 29 22:02:34 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Windows.ListAndLabel.ILlExportModule:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the key of the Export Module
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ExportModule AS CHARACTER NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of the export module properties
        Notes:   Required due to the lack of reflection in the ABL
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExportProperties AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Sets the properties for the Export Module
        Notes:   
        @param piJob The List & Label job handle
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SetExportProperties (piJob AS INTEGER) .

END INTERFACE.