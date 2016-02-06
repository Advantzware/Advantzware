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
    File        : winkit-panel.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Feb 06 20:15:11 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE ADM-DISPATCH-QUALIFIER winkit

DEFINE VARIABLE lInited AS LOGICAL                                                       NO-UNDO INIT FALSE .
DEFINE VARIABLE oForm   AS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm NO-UNDO .

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE winkit-enable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ("enable") .
    
    IF VALID-OBJECT (oForm) THEN 
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager) .


END PROCEDURE.

PROCEDURE winkit-view:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ("view") .
    
    IF VALID-OBJECT (oForm) THEN 
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager) .

END PROCEDURE.

PROCEDURE winkit-make-ribbon-group:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poForm AS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm NO-UNDO .
    DEFINE INPUT PARAMETER piPage AS INTEGER                                                       NO-UNDO .
    
    DEFINE VARIABLE cKey AS CHARACTER NO-UNDO.
    
    IF lInited THEN 
        RETURN . 
        
    ASSIGN lInited = TRUE 
           oForm   = poForm . 
    
 
    
    ASSIGN cKey = Consultingwerk.Util.ProcedureHelper:ShortDotPName (THIS-PROCEDURE)
           cKey = REPLACE (cKey, ".", "_") .
    
    
    Consultingwerk.Util.UltraToolbarsHelper:BuildRibbonGroupFromFrame (FRAME {&FRAME-NAME}:HANDLE, 
                                                                       poForm:ToolbarsManager,
                                                                       poForm:ToolbarsManager:Ribbon:Tabs[0],
                                                                       cKey,
                                                                       cKey,
                                                                       FALSE, 
                                                                       FALSE) . 
                                                                       
    IF VALID-OBJECT (oForm) THEN 
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager) .
    
    

END PROCEDURE.


