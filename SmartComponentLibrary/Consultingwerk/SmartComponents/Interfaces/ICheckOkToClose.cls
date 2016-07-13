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
    File        : ICheckOkToClose
    Purpose     : Identify that the class implementing this interface
                  has it's own logic to decide if the ParentForm
                  should be closed
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Jun 17 09:02:55 CEST 2009
    Notes       : This Interface should under no circumstances be used
                  together with IDoNotCheckOkToClose.
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ICheckOkToClose:  

    /*------------------------------------------------------------------------------
        Purpose: Check if it is ok to close the parent form.
        Notes:   
        @param plShowMessage When TRUE a message is Shown explaining why it is not ok to close the parent form
        @param poCloseReason The System.Windows.Forms.CloseReason passed from then OnFormClosing Method of the parent form giving the reason why the form is closed
        @return Logical value indicating if the parent Form may be closed
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsOkToClose (plShowMessage AS LOGICAL, 
                                       poCloseReason AS System.Windows.Forms.CloseReason).

END INTERFACE.