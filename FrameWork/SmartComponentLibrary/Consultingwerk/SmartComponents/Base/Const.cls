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
    File        : Const
    Purpose     : SmartComponent Library framework constants 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 13 10:42:23 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Base.Const: 

    /*------------------------------------------------------------------------------
        Purpose: The list of Navigation Tools 
        Notes:   NavigationFirst,NavigationPrev,NavigationPrev,NavigationLast
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NAVIGATION_TOOLS AS CHARACTER INITIAL 'NavigationFirst,NavigationPrev,NavigationNext,NavigationLast':U NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The list of Table IO Tools 
        Notes:   TableIOAdd,TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,
                 TableIODelete
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TABLEIO_TOOLS AS CHARACTER INITIAL 'TableIOAdd,TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete':U NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The list of Transaction Tools 
        Notes:   TransactionCommit,TransactionUndo   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TRANSACTION_TOOLS AS CHARACTER INITIAL 'TransactionCommit,TransactionUndo':U NO-UNDO 
    GET.
     
END CLASS.
