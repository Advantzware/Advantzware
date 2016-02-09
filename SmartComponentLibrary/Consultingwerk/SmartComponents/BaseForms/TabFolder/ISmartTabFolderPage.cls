/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ISmartTabFolderPage
    Purpose     : Interface for SmartTabFolderPage user controls
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 20 16:59:57 CEST 2015
    Notes       : SCL-782
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.*                FROM PROPATH .
USING Consultingwerk.SmartComponents.BaseForms.TabFolder.* FROM PROPATH . 
USING Consultingwerk.SmartComponents.Interfaces.*          FROM PROPATH .
USING Progress.Lang.*                                      FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.BaseForms.TabFolder.ISmartTabFolderPage:  

    /*------------------------------------------------------------------------------
       Purpose: Returns a dictionary of SmartDataSources on the SmartTabFolderPage
       Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SmartDataSources AS DictionaryISmartDataSource NO-UNDO 
	GET.

	/*------------------------------------------------------------------------------
	    Purpose: Activates the SmartTabFolderPage (after selecting the tab)
	    Notes:   Used to activate links
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Activate ().

	/*------------------------------------------------------------------------------
	   Purpose: Initializes the SmartTabFolderPage
	   Notes:   Allows to add SmartLinks etc.
	   @param poDataSource The reference to the data source (from a previous tab page) 
	   @param poTableIOSource The reference to the Table IO Source (Ribbon) on the Form
	   @param poNavigationSource The reference to the Navigation Source (Ribbon) on the Form
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID InitializeSmartTabFolderPage (poDataSource AS ISmartDataSource,
	                                                 poTableIOSource AS ISmartTableIOSource,
	                                                 poNavigationSource AS ISmartNavigationSource).

END INTERFACE.
