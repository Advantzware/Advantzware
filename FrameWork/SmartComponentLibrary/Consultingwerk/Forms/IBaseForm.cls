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
    File        : IBaseForm
    Purpose     : Interface for BaseForm related methods an properties 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 19 15:30:41 CET 2015
    Notes       : Required as the adoption of the Telerik WinForm Controls 
                  requires to create additional Base Form classes extending 
                  RadForm and RadRibbonForm 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Forms.IBaseForm:  

    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.                                                                        
        Notes:   Set during constructor using a check at the 
                 System.ComponentModel.LicenseManager                                                                     
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY DesignTime AS LOGICAL NO-UNDO 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: A list of Forms that are dependent from this Form.
        Notes:   Dependent Forms are similar to ChildForms with the exception that
                 DependentForms are not automatically kept on top of the Owner.
                 But when the owner is closed, ths DependentForms will be closed as 
                 well.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DependentForms AS "System.Collections.Generic.List<Progress.Windows.Form>":U NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Forms has been closed.
        Notes:   A Form may be closed but not yet deleted                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY IsClosed AS LOGICAL NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: When set to True the OnLoad method will apply the current form (startup
                 size) to the MinimumSize. This will be done before loading the stored
                 Form size from the user profile (e.g. Windows Registry)
        Notes:   Defaulted by static property UseStartupSizeAsMinSizeDefault in 
                 Consultingwerk.Framework.FrameworkSettings. This property used to 
                 be in the class itself, but was moved to the FrameworkSettings class 
                 to avoid issues with the combination of static and hybrids on 11.2 
                 and 11.3, OE defect OE00240707  
                 http://knowledgebase.progress.com/articles/Article/000042229?popup=true                                                                          
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY UseStartupSizeAsMinSize AS LOGICAL NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Registry Key used for storing this Forms
                 window position                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WindowPositionRegistryKey AS CHARACTER NO-UNDO 
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all Controls in the Form that are of the given type
        Notes:   
        @param poType The reference to the System.Type 
        @return The List of all Controls of the given type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC "System.Collections.Generic.List<System.Windows.Forms.Control>":U FindAllControlsOfType (poType AS System.Type) .

END INTERFACE.
