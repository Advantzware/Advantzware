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
    File        : IAssignKeyFieldValuesService
    Purpose     : Interface for services that can be used by the SmartDataAccess
                  class to assign key field values to new buffers
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 03 12:15:05 CEST 2015
    Notes       : See SmartDataAccess:AssignKeyFieldValues(), SCL-1041,
                  Controlled by the SmartTable.KeyFieldAssignmentGUID 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.IAssignKeyFieldValuesService:  
  
    /*------------------------------------------------------------------------------
        Purpose: Assigns key fields to a new record 
        Notes:   
        @param pcKeyFieldAssignmentKey The key that indicated which key value should be assigned
        @param phBuffer The temp-table buffer (after buffer) to assign key field values to
        @param pcKeyFields Comma delimited list of key fields to assign
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID AssignKeyFields (pcKeyFieldAssignmentKey AS CHARACTER,
	                                    phBuffer AS HANDLE,
	                                    pcKeyFields AS CHARACTER).

END INTERFACE.
