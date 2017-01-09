/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartDesignerSupportComponent
    Purpose     : Component which provides the "Insert UserControl" verb
                  and can be used on any Form or UserControl 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 25 21:27:23 CEST 2015
    Notes       : Allows to work around the current limitation that we
                  are not able to add designer verbs to the Telerik RadControl
                  based Forms
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.*                FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH . 
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.ComponentModel.*                         FROM ASSEMBLY .
USING System.ComponentModel.Design.*                  FROM ASSEMBLY .
USING System.Windows.Forms.*                          FROM ASSEMBLY . 

CLASS Consultingwerk.SmartComponents.Implementation.SmartDesignerSupportComponent
    INHERITS SmartComponent: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartDesignerSupportComponent class. Set's the 
                 DesignTime property using the LicenseManager. 
        Notes:   According to 
                 http://dotnetfacts.blogspot.com/2009/01/identifying-run-time-and-design-mode.html
                 that is only possible during the constructor of the class.                                                                              
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC SmartDesignerSupportComponent ():
        SUPER ().
        
        THIS-OBJECT:SetDesignerProperties () .
                                                                                     
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartDesignerSupportComponent class  
        Notes:   Set's the DesignTime property using the LicenseManager. According to 
                 http://dotnetfacts.blogspot.com/2009/01/identifying-run-time-and-design-mode.html
                 that is only possible during the constructor of the class.                                                                              
        @param poContainer The IContainer to add the SmartBusinessEntityAdapter to.                                                                        
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC SmartDesignerSupportComponent (poContainer AS System.ComponentModel.IContainer):
        SUPER (poContainer) .
        
        THIS-OBJECT:SetDesignerProperties () .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Inserts a user control instance at design time.                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DesignerInsertUserControl ():
        
        IF NOT THIS-OBJECT:DesignTime THEN 
            RETURN . 

        /* Check for Design Time Mode */
        IF NOT TYPE-OF(CAST(THIS-OBJECT:Container, 
                            IDesignerHost):RootComponent, Control) THEN 
           RETURN .

        SmartWindowFormDesignerSupport:InsertUserControl (CAST (CAST(THIS-OBJECT:Container, 
                                                                     IDesignerHost):RootComponent, 
                                                                Control)). 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Sets Properties to customize Visual Designer behaviour                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        THIS-OBJECT:DesignerVerbs = "Insert UserControl":U .

    END METHOD.    

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs                                                                       
        Notes:   This method is intended to be overridden                                                                        
        @param pcVerbText The name (Label) of the Designer Verb that was invoked                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID OnVerbClicked (pcVerbText AS CHARACTER):
        
        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE . 
        
        CASE pcVerbText:
            WHEN "Insert UserControl":U THEN 
                DesignerInsertUserControl () .
                
        END CASE.

        FINALLY:
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE .    		
        END FINALLY.
        
    END METHOD .    

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartDesignerSupportComponent component                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartDesignerSupportComponent ( ):
        
    END DESTRUCTOR.

END CLASS.
