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
    File        : SetFrameBackgroundColorParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 11 16:17:03 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.Util.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Util.SetFrameBackgroundColorParameter
    IMPLEMENTS ISetFrameBackgroundColorParameter: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for BROWSE widgets BGCOLOR
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY BrowseColor AS INTEGER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for BROWSE widgets 
                 LABEL-BGCOLOR
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY BrowseColumnColor AS INTEGER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for FRAME and WINDOW 
                 widgets BGCOLOR
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Color AS INTEGER NO-UNDO INIT ?
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if LITERAL or TEXT widgets BGCOLOR will be set to 
                 piColor as well
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ColorLiterals AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for FILL-IN, EDITOR, 
                 COMBO-BOX widgets BGCOLOR when disabled 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DisabledtextColor AS INTEGER NO-UNDO INIT ?
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if WidgetHelper:SetFrameBackground should call the 
                 Win32 API LockWindowUpdate to prevent flickering on the screen 
        Notes:   Default is FALSE to avoid interfering with potentially existing code
                 to avoid window flickering
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY LockWindowUpdate AS LOGICAL NO-UNDO INIT FALSE 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the parent FRAME or WINDOW 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Parent AS HANDLE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for RADIO-SET widgets BGCOLOR 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY RadiosetColor AS INTEGER NO-UNDO INIT ? 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the color number to be used for FILL-IN, EDITOR, 
                 COMBO-BOX widgets BGCOLOR 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY TextboxColor AS INTEGER NO-UNDO INIT ?
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets The color number to be used for TOGGLE-BOX widgets BGCOLOR 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ToggleBoxColor AS INTEGER NO-UNDO INIT ?
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SetFrameBackgroundColorParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SetFrameBackgroundColorParameter ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SetFrameBackgroundColorParameter class
        Notes:   
        @param phParent The parent FRAME or WINDOW
        @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SetFrameBackgroundColorParameter (phParent AS HANDLE, 
                                                         piColor AS INTEGER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Parent            = phParent 
               THIS-OBJECT:Color             = piColor
               .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SetFrameBackgroundColorParameter class
        Notes:   
        @param phParent The parent FRAME or WINDOW
        @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
        @param plLockWindowUpdate Logical value indicating the value for LockWindowUpdate
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SetFrameBackgroundColorParameter (phParent AS HANDLE, 
                                                         piColor AS INTEGER,
                                                         plLockWindowUpdate AS LOGICAL):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Parent            = phParent 
               THIS-OBJECT:Color             = piColor
               THIS-OBJECT:LockWindowUpdate  = plLockWindowUpdate
               .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SetFrameBackgroundColorParameter class
        Notes:   Used during the recursion in WidgetHelper:SetFrameBackgroundColorParameter
        @param phParent The parent FRAME or WINDOW
        @param poParameter The reference to an existing ISetFrameBackgroundColorParameter instance
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SetFrameBackgroundColorParameter (phParent AS HANDLE, 
                                                         poParameter AS ISetFrameBackgroundColorParameter):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Parent            = phParent 
               THIS-OBJECT:Color             = poParameter:Color
               THIS-OBJECT:BrowseColor       = poParameter:BrowseColor 
               THIS-OBJECT:TextboxColor      = poParameter:TextboxColor
               THIS-OBJECT:DisabledtextColor = poParameter:DisabledtextColor
               THIS-OBJECT:BrowseColumnColor = poParameter:BrowseColumnColor
               THIS-OBJECT:ToggleBoxColor    = poParameter:ToggleBoxColor
               THIS-OBJECT:RadiosetColor     = poParameter:RadiosetColor
               THIS-OBJECT:ColorLiterals     = poParameter:ColorLiterals 
               THIS-OBJECT:LockWindowUpdate  = poParameter:LockWindowUpdate
               .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SetFrameBackgroundColorParameter class
        Notes:   
        @param phParent The parent FRAME or WINDOW
        @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
        @param piBrowseColor The color number to be used for BROWSE widgets BGCOLOR
        @param piTextboxColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR
        @param piDisabledtextColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR when disabled
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SetFrameBackgroundColorParameter (phParent AS HANDLE, 
                                                         piColor AS INTEGER,
                                                         piBrowseColor AS INTEGER,
                                                         piTextboxColor AS INTEGER,
                                                         piDisabledtextColor AS INTEGER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Parent            = phParent 
               THIS-OBJECT:Color             = piColor
               THIS-OBJECT:BrowseColor       = piBrowseColor 
               THIS-OBJECT:TextboxColor      = piTextboxColor
               THIS-OBJECT:DisabledtextColor = piDisabledtextColor
               .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SetFrameBackgroundColorParameter class
        Notes:   
        @param phParent The parent FRAME or WINDOW
        @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
        @param piBrowseColor The color number to be used for BROWSE widgets BGCOLOR
        @param piTextboxColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR
        @param piDisabledtextColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR when disabled
        @param plLockWindowUpdate Logical value indicating the value for LockWindowUpdate
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SetFrameBackgroundColorParameter (phParent AS HANDLE, 
                                                         piColor AS INTEGER,
                                                         piBrowseColor AS INTEGER,
                                                         piTextboxColor AS INTEGER,
                                                         piDisabledtextColor AS INTEGER,
                                                         plLockWindowUpdate AS LOGICAL):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Parent            = phParent 
               THIS-OBJECT:Color             = piColor
               THIS-OBJECT:BrowseColor       = piBrowseColor 
               THIS-OBJECT:TextboxColor      = piTextboxColor
               THIS-OBJECT:DisabledtextColor = piDisabledtextColor
               THIS-OBJECT:LockWindowUpdate  = plLockWindowUpdate
               .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SetFrameBackgroundColorParameter class
        Notes:   
        @param phParent The parent FRAME or WINDOW
        @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
        @param piBrowseColor The color number to be used for BROWSE widgets BGCOLOR
        @param piTextboxColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR
        @param piDisabledtextColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR when disabled
        @param piBrowseColumnColor The color number to be used for BROWSE widgets LABEL-BGCOLOR 
        @param piToggleBoxColor The color number to be used for TOGGLE-BOX widgets BGCOLOR
        @param piRadiosetColor The color number to be used for RADIO-SET widgets BGCOLOR 
        @param plColorLiterals When True LITERAL or TEXT widgets BGCOLOR will be set to piColor as well
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SetFrameBackgroundColorParameter (phParent AS HANDLE, 
                                                         piColor AS INTEGER,
                                                         piBrowseColor AS INTEGER,
                                                         piTextboxColor AS INTEGER,
                                                         piDisabledtextColor AS INTEGER,
                                                         piBrowseColumnColor AS INTEGER,
                                                         piToggleBoxColor AS INTEGER,
                                                         piRadiosetColor AS INTEGER,
                                                         plColorLiterals AS LOGICAL):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Parent            = phParent 
               THIS-OBJECT:Color             = piColor
               THIS-OBJECT:BrowseColor       = piBrowseColor 
               THIS-OBJECT:TextboxColor      = piTextboxColor
               THIS-OBJECT:DisabledtextColor = piDisabledtextColor
               THIS-OBJECT:BrowseColumnColor = piBrowseColumnColor
               THIS-OBJECT:ToggleBoxColor    = piToggleBoxColor
               THIS-OBJECT:RadiosetColor     = piRadiosetColor
               THIS-OBJECT:ColorLiterals     = plColorLiterals 
               .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SetFrameBackgroundColorParameter class
        Notes:   
        @param phParent The parent FRAME or WINDOW
        @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
        @param piBrowseColor The color number to be used for BROWSE widgets BGCOLOR
        @param piTextboxColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR
        @param piDisabledtextColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR when disabled
        @param piBrowseColumnColor The color number to be used for BROWSE widgets LABEL-BGCOLOR 
        @param piToggleBoxColor The color number to be used for TOGGLE-BOX widgets BGCOLOR
        @param piRadiosetColor The color number to be used for RADIO-SET widgets BGCOLOR 
        @param plColorLiterals When True LITERAL or TEXT widgets BGCOLOR will be set to piColor as well
        @param plLockWindowUpdate Logical value indicating the value for LockWindowUpdate
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SetFrameBackgroundColorParameter (phParent AS HANDLE, 
                                                         piColor AS INTEGER,
                                                         piBrowseColor AS INTEGER,
                                                         piTextboxColor AS INTEGER,
                                                         piDisabledtextColor AS INTEGER,
                                                         piBrowseColumnColor AS INTEGER,
                                                         piToggleBoxColor AS INTEGER,
                                                         piRadiosetColor AS INTEGER,
                                                         plColorLiterals AS LOGICAL,
                                                         plLockWindowUpdate AS LOGICAL):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Parent            = phParent 
               THIS-OBJECT:Color             = piColor
               THIS-OBJECT:BrowseColor       = piBrowseColor 
               THIS-OBJECT:TextboxColor      = piTextboxColor
               THIS-OBJECT:DisabledtextColor = piDisabledtextColor
               THIS-OBJECT:BrowseColumnColor = piBrowseColumnColor
               THIS-OBJECT:ToggleBoxColor    = piToggleBoxColor
               THIS-OBJECT:RadiosetColor     = piRadiosetColor
               THIS-OBJECT:ColorLiterals     = plColorLiterals 
               THIS-OBJECT:LockWindowUpdate  = plLockWindowUpdate
               .
        
    END CONSTRUCTOR.
    
END CLASS.
