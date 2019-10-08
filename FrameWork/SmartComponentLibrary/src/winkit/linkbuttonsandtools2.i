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
    File        : linkbuttonsandtools.i
    Purpose     : 

    Syntax      :

    Description : Links Buttons in a Frame with Tools in the 
                  UltraToolbarsManager
                  
                  Wrapper for UltraToolbarsHelper:LinkButtonsAndTools ()        

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 01 13:28:35 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

&IF DEFINED (winkitactive) NE 0 &THEN

  IF VALID-OBJECT (oForm) THEN DO:
      
      Consultingwerk.Util.UltraToolbarsHelper:LinkButtonsAndTools (FRAME {1}:HANDLE,
                                                                   oForm:ToolbarsManager,
                                                                   TRUE) . 
      
      
  END.
&ENDIF 
