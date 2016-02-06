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
    File        : ISmartWindowForm
    Purpose     : Interface for SmartWindowsForm and SmartRadWindowForm and
                  SmartRadRibbonWindowForm
     
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 08 20:29:17 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Enum.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.*                FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH . 
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartWindowForm:  

    /*------------------------------------------------------------------------------
        Purpose: A DataSource that is passed in from the launching Container (Form)
                 to this Form. The ExternalDataSource will typically be passed on
                 to the internal PrimaryDataTarget                                                                 
        Notes:   Used for PASS-THROUGH data links                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PUBLIC PROPERTY ExternalDataSource AS ISmartDataSource NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: A NavigationTarget that is passed in from the launching Container (Form)
                 to this Form. The ExternalNavigationtarget will typically be passed on
                 to the internal PrimaryNavigationSource
        Notes:   Used for PASS-THROUGH navigation links                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PUBLIC PROPERTY ExternalNavigationTarget AS ISmartNavigationTarget NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: A reference to the primary data source in this Form                                                                      
        Notes:                                                                     
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PrimaryDataSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: A reference to the primary data target in this Form                                                                      
        Notes:   Used for PASS-THROUGH data links                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PrimaryDataTarget AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataTarget NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: A reference to the primary navigation source in this Form   
        Notes:   Used for PASS-THROUGH navigation links                                                                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PrimaryNavigationSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Raised when a Form is destroyed (in the Destructor)                                                                
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PUBLIC EVENT FormDestroyed DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Marks a SmartViewerControl or SmartBrowserControl as Active                                                                       
        Notes:                                                              
        @param poControl The reference to the Control that has become active          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateSmartControl (poControl AS System.Windows.Forms.Control) .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the component can be found in the ComponentsCollection
        Notes:   Available from OpenEdge 11.1 on
        @param poComponent The component to locate
        @return Logical value indicating if the component can be found in the ComponentsCollection
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL CanFindInComponentsCollection (poComponent AS System.Object) .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all Components in the Form 
        Notes:   Supported on OpenEdge 11 only, due to dependency to the ComponentsCollection
                 list not available on 10.2B
        @return The List of Components in the Form 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC "System.Collections.Generic.List<System.ComponentModel.Component>" GetAllComponents () .
        
	/*------------------------------------------------------------------------------
	    Purpose: Returns the reference to the ISmartToolbarControl of the ISmartWindowForm 
	    Notes:
        @return The reference to the ISmartToolbarControl of the ISmartWindowForm	        
	------------------------------------------------------------------------------*/
	METHOD PUBLIC ISmartToolbarControl GetToolbar ().

    /*------------------------------------------------------------------------------
        Purpose: Shows the Prompt to save pending transactions to the backend while 
                 closing the Form
        Notes:   
        @return The DialogResultEnum value indicating "Yes" to save changes, "No" to not save changes and "Cancel" to not close the Form
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC DialogResultEnum PromptForSaveOfPendingChangesOnClose ().

    /*------------------------------------------------------------------------------
        Purpose: Recursivly ask all Controls implementing the ICheckOkToClose Interface
                 wheather it is ok to close the form.
        Notes:
        @param poControls The System.Windows.Forms.Control+ControlCollection Collection of Controls to iterate
        @param plShowMessage When TRUE a message is Shown explaining why it is not ok to close the parent form.
        @param poCloseReason The System.Windows.Forms.CloseReason passed from then OnFormClosing Method of the parent form giving the reason who closes the form.
        @return Logical value indicating if the Form can be closed or not.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurseCheckIsOkToClose (INPUT poControls    AS System.Windows.Forms.Control+ControlCollection,
                                                   INPUT plShowMessage AS LOGICAL, 
                                                   INPUT poCloseReason AS System.Windows.Forms.CloseReason) . 
                                                     
END INTERFACE.
