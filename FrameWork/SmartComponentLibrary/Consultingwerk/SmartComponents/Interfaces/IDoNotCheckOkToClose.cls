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
    File        : IDoNotCheckOkToClose
    Purpose     : Signal that the class implementing this Interface 
                  and it's Controls collection do not require action
                  when the ParentForm is querying if it's ok to close
                  the Form
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 27 13:35:22 CET 2009
    Notes       : This Interface should under no circumstances be used
                  together with ICheckOkToClose.
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.IDoNotCheckOkToClose:  
  
END INTERFACE.