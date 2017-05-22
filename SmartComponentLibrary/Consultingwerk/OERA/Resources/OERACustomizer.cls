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
    File        : OERACustomizer
    Purpose     : This class is an entry point for customization and 
                  translation of strings used for messages or as f.e.
                  error messages from the DataAccess base class. To 
                  enable customization all strings are represented as 
                  static properties and may be overridden in f.e. an 
                  initialization procedure. 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 19 19:52:43 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.OERA.Resources.OERACustomizer: 

    /*------------------------------------------------------------------------------
        Purpose: Customization of the error message for ABL error 11913 raised
                 during SAVE-ROW-CHANGES
        Notes:   SAVE-ROW-CHANGES found <name> record with conflicting change by another user. (11913)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DataAccess_RecordChangedByAnotherUser AS CHARACTER 
       INITIAL "The current row has been changed by another user! (11913)":T NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Customization of the error message for ABL error 12300 raised
                 during SAVE-ROW-CHANGES
        Notes:   Could not get lock for  record during save-row-changes. (12300)
                 &1 in message text will be replaced with the table name
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DataAccess_RecordLocked AS CHARACTER 
       INITIAL "Unable to update &1 record because it is locked by another user! (12300)":T NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Customization of the error message for ABL error 11914 raised
                 during SAVE-ROW-CHANGES
        Notes:   Unable to delete <name> record in the database during SAVE-ROW-CHANGES. (11914)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DataAccess_UnableToDelete AS CHARACTER 
       INITIAL "Unable to delete record! (11914)":T NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Customization of the error message for ABL error 11915 raised
                 during SAVE-ROW-CHANGES
        Notes:   Unable to update temp-table <name> record in the database during SAVE-ROW-CHANGES. (11915)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DataAccess_UnableToUpdate AS CHARACTER 
       INITIAL "Unable to update record! (11915)":T NO-UNDO 
    GET.
    SET. 

END CLASS.
