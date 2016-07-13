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
    File        : create-users.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Feb 07 18:04:33 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW . 

USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH.

DEFINE VARIABLE oUsers AS UserDatasetModel NO-UNDO . 

/* ***************************  Main Block  *************************** */

FOR EACH users:
    
    oUsers = UserDatasetModel:FromUsername (users.user_id) . 
    
    IF NOT oUsers:SmartUser:Available THEN DO:
        oUsers:TrackingChanges = TRUE . 
    
        oUsers:SmartUser:Create() .

        ASSIGN oUsers:SmartUser:UserName       = users.user_id 
               oUsers:SmartUser:UserFullName   = users.user_name
               oUsers:SmartUser:UserEmail      = (IF users.email > "" THEN users.email ELSE users.image_filename)
                .        
    
        oUsers:SaveChanges() .
    END.
END.    
    