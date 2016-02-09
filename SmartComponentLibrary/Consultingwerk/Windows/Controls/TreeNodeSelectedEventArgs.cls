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
    File        : TreeNodeSelectedEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 02 22:51:21 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING System.EventArgs.
USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.Controls.TreeNodeSelectedEventArgs INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Gets the key of the selected menu item                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ItemKey AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TreeNodeSelectedEventArgs class                                                                     
        Notes:                                                 
        @param pcItemKey The key of the TreeNode item associated with this event                       
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TreeNodeSelectedEventArgs (pcItemKey AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:ItemKey = pcItemKey .
        
    END CONSTRUCTOR.

END CLASS.
