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
    File        : ISmartCommitTarget
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Thu Sep 01 23:49:29 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH .
USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartCommitTarget:  

    /*------------------------------------------------------------------------------
        Purpose: Raised when the CommitChanged property is changed                                                                       
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AutoCommitChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired whenever the SmartTransactionState property has 
                 changed
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SmartTransactionStateChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Controls the AutoCommit functionality of the ISmartCommitTraget                                                                       
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AutoCommit AS LOGICAL NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Controls the AutoCommit functionality of the ISmartCommitTraget                                                                       
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartTransactionState AS TransactionStateEnum NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Commits the transaction to the backend                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CommitTransaction ().

    /*------------------------------------------------------------------------------
        Purpose: Reverts the changes                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RejectTransaction ().

END INTERFACE.