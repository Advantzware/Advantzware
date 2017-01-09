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
    File        : SmartViewerLogic
    Purpose     : Base class for classes that provide client side logic 
                  for SmartViewerControl instances
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 22 21:14:55 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Exceptions.*                 FROM PROPATH . 
USING Consultingwerk.SmartComponents.Base.*       FROM PROPATH .  
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.SmartViewerLogic 
    IMPLEMENTS ISmartViewerLogic
    ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartViewerControl that owns this
                 instance of the Viewer Logic Object 
        Notes:   The Owner property cannot be modified once set   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Owner AS SmartViewerControl NO-UNDO 
	GET.
	SET (arg AS SmartViewerControl):
	    
	    IF NOT VALID-OBJECT (arg) THEN 
            UNDO, THROW NEW InvalidValueException ("?":U, 
                                                   "Owner":U) .
	    
	    IF VALID-OBJECT (THIS-OBJECT:Owner) THEN 
	        UNDO, THROW NEW InvalidValueException (arg:ToString (),
	                                               "Owner":U) .
	    
	    THIS-OBJECT:Owner = arg .
	    
	END. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of attached SmartViewerControl instances 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SmartViewerControls AS ListSmartViewerControl NO-UNDO 
	GET.
	PRIVATE SET. 
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartViewerLogic class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SmartViewerLogic ():
		SUPER ().
		
		THIS-OBJECT:SmartViewerControls = NEW ListSmartViewerControl () . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the first Control that is bound to the named 
                 field of the binding source 
        Notes:   Searchs all Controls in the owning viewer and also GroupAssignLinked viewers
        @param pcBindingPropertyName The name of the field in the ProBindingSource                                                                        
        @return The bound Control of ? when no Control is bound                                                                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Windows.Forms.Control GetBoundControl (pcBindingPropertyName AS CHARACTER):

        DEFINE VARIABLE oList AS "System.Collections.Generic.List<System.Windows.Forms.Control>" NO-UNDO . 

        IF NOT VALID-OBJECT (THIS-OBJECT:Owner) THEN 
            RETURN ? .
            
        oList = THIS-OBJECT:GetBoundControls (pcBindingPropertyName) .  
            
        IF oList:Count = 0 THEN  
            RETURN ? .                        

        RETURN oList[0] .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of references of the Controls that are bound to the 
                 named field of the binding source 
        Notes:   Returns all Controls in the owning viewer and also GroupAssignLinked viewers
        @param pcBindingPropertyName The name of the field in the ProBindingSource                                                                        
        @return The List of bound Controls                                                                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC "System.Collections.Generic.List<System.Windows.Forms.Control>" GetBoundControls (pcBindingPropertyName AS CHARACTER):

        DEFINE VARIABLE oList AS "System.Collections.Generic.List<System.Windows.Forms.Control>" NO-UNDO . 

        IF NOT VALID-OBJECT (THIS-OBJECT:Owner) THEN DO: 
            
            oList = NEW "System.Collections.Generic.List<System.Windows.Forms.Control>" () . 
            
            RETURN oList .
        END.

        RETURN THIS-OBJECT:Owner:GetBoundControls (pcBindingPropertyName) .

    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns logical value indicating if the Viewer contains a Control 
                 that is bound to the named field 
        Notes:   Includes Group Assigned viewers
        @param pcBindingPropertyName The name of the field in the ProBindingSource
        @return Logical value indicating if a bound control exists                                                                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HasBoundControl (pcBindingPropertyName AS CHARACTER):
    
        IF NOT VALID-OBJECT (THIS-OBJECT:Owner) THEN 
            RETURN FALSE . 
            
        RETURN THIS-OBJECT:Owner:HasBoundControl (pcBindingPropertyName, 
                                                  TRUE) .
        
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Initializes the SmartViewerLogic instance
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Initialize ():
		
		THIS-OBJECT:SubscribeEvents() .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handshake method for SmartViewerControl instances 
        Notes:   You may override this method to perform initialization for a 
                 specific viewer
        @param poSmartViewerControl The SmartViewerControl instance this logic class will provid logic for
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RegisterSmartViewerControl (poSmartViewerControl AS SmartViewerControl):
		
        ASSIGN poSmartViewerControl:ViewerLogicObject = THIS-OBJECT . 

        THIS-OBJECT:SmartViewerControls:Add (poSmartViewerControl) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shuts down the SmartViewerLogic instance
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown ():
        
        THIS-OBJECT:UnsubscribeEvents() . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Subscribes the events for an attached viewer instance
        Notes:   This method needs to be implemented by the Viewer Logic class and 
                 is supposed to subscribe to control events
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ABSTRACT VOID SubscribeEvents ().

    /*------------------------------------------------------------------------------
        Purpose: Unsubscribes the events for an attached viewer instance
        Notes:   This method needs to be implemented by the Viewer Logic class and 
                 is supposed to unsubscribe from control events
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ABSTRACT VOID UnsubscribeEvents ().

END CLASS.
