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
    File        : sibeginrequest.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 23 21:11:37 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

        DEFINE VARIABLE lCallActivated AS LOGICAL INIT FALSE  .
        
        IF NOT ServiceInterface:IsActive THEN DO:
            ASSIGN lCallActivated = TRUE .
            
            ServiceInterface:Activate () .
        END.    