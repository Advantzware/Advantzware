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
    File        : check-dialogresult-ok.i
    Purpose     : Compares the {1} with System.Windows.Forms.DialogResult:OK
                  and leaves the current procedure/method when it's 
                  not eqal
                  
                  Optional second parameter {2} containing the return-value

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 03 08:55:11 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

    IF Progress.Util.EnumHelper:AreNotEqual ({1},
                                             System.Windows.Forms.DialogResult:OK) THEN RETURN {2} . 
