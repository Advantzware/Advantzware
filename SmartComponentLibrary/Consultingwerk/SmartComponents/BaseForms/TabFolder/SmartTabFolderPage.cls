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
    File        : SmartTabFolderPage
    Purpose     : Base class for tab folder user controls 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 21 08:18:48 CEST 2015
    Notes       : SCL-782  
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*                FROM PROPATH .
USING Consultingwerk.SmartComponents.BaseForms.TabFolder.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*          FROM PROPATH .
USING Progress.Lang.*                                      FROM PROPATH .

CLASS Consultingwerk.SmartComponents.BaseForms.TabFolder.SmartTabFolderPage 
    INHERITS SmartUserControl 
    IMPLEMENTS ISmartTabFolderPage: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Dictionary of ISmartDataSource's provided 
                 by this user control
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SmartDataSources AS DictionaryISmartDataSource NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the ISmartNavigationSource provided to this 
                 user control
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SmartNavigationSource AS ISmartNavigationSource NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the ISmartTableIOSource provided to this 
                 user control
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartTableIOSource AS ISmartTableIOSource NO-UNDO 
    GET.
    PRIVATE SET. 

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the SmartTabFolderPage class 
	    Notes:
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SmartTabFolderPage ():
		SUPER ().
		
		THIS-OBJECT:SmartDataSources = NEW DictionaryISmartDataSource () . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates the SmartTabFolderPage (after selecting the tab)
        Notes:   Used to activate links
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Activate ():
	    /* pseudo abstract */
	END METHOD . 

    /*------------------------------------------------------------------------------
       Purpose: Initializes the SmartTabFolderPage
       Notes:   Allows to add SmartLinks etc.
       @param poDataSource The reference to the data source (from a previous tab page) 
       @param poTableIOSource The reference to the Table IO Source (Ribbon) on the Form
       @param poNavigationSource The reference to the Navigation Source (Ribbon) on the Form
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID InitializeSmartTabFolderPage (poDataSource AS Consultingwerk.SmartComponents.Interfaces.ISmartDataSource, 
                                                     poTableIOSource AS Consultingwerk.SmartComponents.Interfaces.ISmartTableIOSource, 
                                                     poNavigationSource AS Consultingwerk.SmartComponents.Interfaces.ISmartNavigationSource):
        
        THIS-OBJECT:SmartTableIOSource = poTableIOSource.
        THIS-OBJECT:SmartNavigationSource = poNavigationSource . 
        
    END METHOD .                                                         

END CLASS.
