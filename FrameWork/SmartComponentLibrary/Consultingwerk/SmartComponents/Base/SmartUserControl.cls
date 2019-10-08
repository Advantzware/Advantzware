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
    File        : SmartUserControl
    Purpose     : Base class for SmartComponent Library user Controls
    Syntax      : 
    Description : Base class for all UserControls.
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Jun 25 15:07:58 CEST 2009
    Notes       : Provides support for Visual Designer extensions
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.*            FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Base.*       FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.*                             FROM ASSEMBLY .
USING Progress.Windows.UserControl                FROM ASSEMBLY .
USING System.ComponentModel.*                     FROM ASSEMBLY .
USING System.ComponentModel.Design.*              FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Base.SmartUserControl 
    INHERITS UserControl 
    IMPLEMENTS /*ICustomTypeDescriptor, */ IHasDesignerVerbs: 

    /* Mike Fechner, Consultingwerk Ltd. 16.05.2010
       Core implementation of ICustomTypeDescriptor */
/*    {Consultingwerk/SmartComponents/Base/ICustomTypeDescriptor.i}*/
    {Consultingwerk/SmartComponents/Base/CreateVerbs.i}

    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.                                                                        
        Notes:   Set during constructor using a check at the 
                 System.ComponentModel.LicenseManager                                                                        
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY DesignTime AS LOGICAL NO-UNDO 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to generate Designer Verbs (actions for the 
                 Visual Designer)                                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PROTECTED PROPERTY DesignerVerbs AS CHARACTER NO-UNDO 
    GET. 
    PROTECTED SET.  

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 will be excluded from the .NET TypeDescriptor properties list                                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PROTECTED PROPERTY HiddenProperties AS CHARACTER NO-UNDO 
    GET. 
    SET.  

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should not show up in the Visual Designers Property Grid                                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PROTECTED PROPERTY NonBrowsableProperties AS CHARACTER NO-UNDO 
    GET. 
    SET.    
        
    /*------------------------------------------------------------------------------
        Purpose: Returns if the FormDestroyed evengt of the containing Form has been
                 subscribed                                                                
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PROTECTED PROPERTY ParentFormDestroyedSubscribed AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PRIVATE SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartUserControl class. Set's the 
                 DesignTime property using the LicenseManager. 
        Notes:   Set during constructor using a check at the 
                 System.ComponentModel.LicenseManager                                                                        
    ------------------------------------------------------------------------------*/    
    CONSTRUCTOR PUBLIC SmartUserControl ():
        SUPER ().

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .          
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all Components in the Form 
        Notes:   Supported on OpenEdge 11 only, due to dependency to the ComponentsCollection
                 list not available on 10.2B
        @return The List of Components in the Form 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC "System.Collections.Generic.List<System.ComponentModel.Component>" GetAllComponents ():
        
        DEFINE VARIABLE oList AS "System.Collections.Generic.List<System.ComponentModel.Component>" NO-UNDO.

        oList = NEW "System.Collections.Generic.List<System.ComponentModel.Component>" () .

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        {Consultingwerk/foreach.i System.ComponentModel.IContainer oContainer in THIS-OBJECT:ComponentsCollection}

            IF NOT VALID-OBJECT (oContainer) OR NOT VALID-OBJECT (oContainer:Components) THEN 
                NEXT . 

            {Consultingwerk/foreach.i System.Object oComponent in oContainer:Components}

                IF TYPE-OF (oComponent, Component) THEN
                    oList:Add (CAST (oComponent, Component)) .
            END.
        END.
        &ENDIF
        RETURN oList.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                      
        Notes:   Subscribes the FormDestroyed event from the ParentForm       
        @param e The System.EventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        SUPER:OnLoad (e) .

        IF NOT THIS-OBJECT:ParentFormDestroyedSubscribed AND
           VALID-OBJECT (THIS-OBJECT:ParentForm) AND
           TYPE-OF (THIS-OBJECT:ParentForm, ISmartWindowForm) THEN DO:

            /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
               Only subscribe to ParentFormDestroyedHandler event when not in Visual Designer 
               to avoid conflicts when re-opening layouts. */        
            IF NOT THIS-OBJECT:DesignTime THEN 
                CAST (THIS-OBJECT:ParentForm, ISmartWindowForm):FormDestroyed:Subscribe (ParentFormDestroyedHandler) .
            
            ASSIGN ParentFormDestroyedSubscribed = TRUE .
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs                                                                       
        Notes:   This method is intended to be overridden     
        @param pcDesignerVerb The name (Label) of the Designer Verb that was invoked                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OnVerbClicked (pcDesignerVerb AS CHARACTER):
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FormDestroyed event of the owning Form
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParentFormDestroyedHandler (sender AS System.Object,
                                                      e AS System.EventArgs):

        DELETE OBJECT THIS-OBJECT .

    END METHOD.
    
END CLASS.