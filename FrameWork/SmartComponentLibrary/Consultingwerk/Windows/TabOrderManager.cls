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
    File        : TabOrderManager
    Purpose     : Provides ability to customize the Tab order, including the
                  ability between ABL widgets and .NET Controls
    Syntax      : 
    Description : Wrapper to Consultingwerk.Support.TabOrderMessageFilter
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 12 20:34:17 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Support.* FROM ASSEMBLY . 
USING Consultingwerk.Util.*    FROM PROPATH .
USING Consultingwerk.Windows.* FROM PROPATH .  
USING Progress.Lang.*          FROM PROPATH .

CLASS Consultingwerk.Windows.TabOrderManager: 

    DEFINE VARIABLE oTabOrderMessageFilter AS Consultingwerk.Support.TabOrderMessageFilter NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the logfile used for debugging / tracing 
                 purposes 
        Notes:   Logging is implemented within the .NET class Consultingwerk.Support.TabOrderMessageFilter  
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LogfileName AS CHARACTER NO-UNDO 
    GET:
        RETURN oTabOrderMessageFilter:LogfileName .
    END GET.
    SET (arg AS CHARACTER):
        oTabOrderMessageFilter:LogfileName = arg . 
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TabOrderManager class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TabOrderManager ():
        SUPER ().

        oTabOrderMessageFilter = NEW Consultingwerk.Support.TabOrderMessageFilter () .
        System.Windows.Forms.Application:AddMessageFilter (oTabOrderMessageFilter) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds a custom tab order
        Notes:   
        @param poForm The parent Form
        @param piHwndFrom The source widget 
        @param piHwndTo The target widget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddTabOrder (poForm AS System.Windows.Forms.Form,
                                    piHwndFrom AS INTEGER,
                                    piHwndTo AS INTEGER):
        
        oTabOrderMessageFilter:AddTabOrder (poForm, piHwndFrom, piHwndTo) .

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Adds a custom tab order
        Notes:   
        @param poForm The parent Form
        @param piHwndFrom The source widget 
        @param poIntPtrTo The target Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddTabOrder (poForm AS System.Windows.Forms.Form,
                                    piHwndFrom AS INTEGER,
                                    poIntPtrTo AS System.IntPtr):
        
        oTabOrderMessageFilter:AddTabOrder (poForm, piHwndFrom, poIntPtrTo) .

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Adds a custom tab order
        Notes:   
        @param poForm The parent Form
        @param poIntPtrFrom The source Control 
        @param piHwndTo The target widget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddTabOrder (poForm AS System.Windows.Forms.Form,
                                    poIntPtrFrom AS System.IntPtr,
                                    piHwndTo AS INTEGER):
        
        oTabOrderMessageFilter:AddTabOrder (poForm, poIntPtrFrom, piHwndTo) .

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Adds a custom tab order
        Notes:   
        @param poForm The parent Form
        @param poIntPtrFrom The source Control 
        @param poIntPtrTo The target Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddTabOrder (poForm AS System.Windows.Forms.Form,
                                    poIntPtrFrom AS System.IntPtr,
                                    poIntPtrTo AS System.IntPtr):
        
        oTabOrderMessageFilter:AddTabOrder (poForm, poIntPtrFrom, poIntPtrTo) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a custom tab order
        Notes:   
        @param poForm The parent Form
        @param phFrom The source widget 
        @param phTo The target widget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddTabOrder (poForm AS System.Windows.Forms.Form,
                                    phFrom AS HANDLE,
                                    phTo AS HANDLE):
        
        oTabOrderMessageFilter:AddTabOrder (poForm, phFrom:HWND, phTo:HWND) .

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Adds a custom tab order
        Notes:   
        @param poForm The parent Form
        @param phFrom The source widget 
        @param poTo The target Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddTabOrder (poForm AS System.Windows.Forms.Form,
                                    phFrom AS HANDLE,
                                    poTo AS System.Windows.Forms.Control):
        
        oTabOrderMessageFilter:AddTabOrder (poForm, phFrom:HWND, poTo:Handle) .

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Adds a custom tab order
        Notes:   
        @param poForm The parent Form
        @param poFrom The source Control 
        @param phTo The target widget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddTabOrder (poForm AS System.Windows.Forms.Form,
                                    poFrom AS System.Windows.Forms.Control,
                                    phTo AS HANDLE):
        
        oTabOrderMessageFilter:AddTabOrder (poForm, poFrom:Handle, phTo:HWND) .

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Adds a custom tab order
        Notes:   
        @param poForm The parent Form
        @param poFrom The source Control 
        @param poTo The target Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddTabOrder (poForm AS System.Windows.Forms.Form,
                                    poFrom AS System.Windows.Forms.Control,
                                    poTo AS System.Windows.Forms.Control):
        
        oTabOrderMessageFilter:AddTabOrder (poForm, poFrom:Handle, poTo:Handle) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the TabOrderManager class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC TabOrderManager ():

        System.Windows.Forms.Application:RemoveMessageFilter (oTabOrderMessageFilter) .

    END DESTRUCTOR.

END CLASS.
