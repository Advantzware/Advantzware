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
    File        : winkit-panel-triggerend.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Feb 07 21:16:25 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/


/* ***************************  Main Block  *************************** */

    FINALLY:
        		
        IF VALID-OBJECT (oForm) THEN 
            Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager) .
    
    END FINALLY.

