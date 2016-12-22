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
    File        : BackgroundJobMonitorControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 04 01:15:11 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                   FROM PROPATH . 
USING Consultingwerk.Windows.BackgroundOperation.* FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .
USING Progress.Windows.UserControl                 FROM ASSEMBLY .
USING System.Windows.Forms.*                       FROM ASSEMBLY .

CLASS Consultingwerk.Windows.BackgroundOperation.BackgroundJobMonitorControl INHERITS UserControl: 


	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraFlowLayoutManager1 AS Infragistics.Win.Misc.UltraFlowLayoutManager NO-UNDO.
    
    
	CONSTRUCTOR PUBLIC BackgroundJobMonitorControl ():
		
		
        SUPER().
        InitializeComponent().
        
        /* Register in Service Container */
        DO ON ERROR UNDO, THROW:
            /* It may be possible, that there is already a BackgroundJobMonitorControl registered
               in the Service Container. */ 
            FrameworkSettings:ServiceContainer:RemoveService (THIS-OBJECT:GetClass()) .
        
            CATCH err AS Progress.Lang.Error:
                /* In the Visual Designer it's actually possible that the 
                   pre-registered instance has been deleted. So just ignore 
                   errors from above */
            END CATCH.
        END.
        
        FrameworkSettings:ServiceContainer:AddService 
                    (THIS-OBJECT:GetClass(),
                     THIS-OBJECT) .
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        &ENDIF
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds a Background Job Progress Control to the Monitor 
        Notes:   
        @param poControl The BackgroundJobProgressControl to add
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID AddJobProgressControl (poControl AS BackgroundJobProgressControl):
		
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        
        nestedvar0 = CAST(Progress.Util.EnumHelper:OR (System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        
        poControl:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        poControl:Width = THIS-OBJECT:Width - 2 .  
		
		THIS-OBJECT:Controls:Add (poControl) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        THIS-OBJECT:ultraFlowLayoutManager1 = NEW Infragistics.Win.Misc.UltraFlowLayoutManager(THIS-OBJECT:components).
        CAST(THIS-OBJECT:ultraFlowLayoutManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraFlowLayoutManager1 */
        /*  */
        THIS-OBJECT:ultraFlowLayoutManager1:ContainerControl = THIS-OBJECT.
        THIS-OBJECT:ultraFlowLayoutManager1:HorizontalAlignment = Infragistics.Win.Layout.DefaultableFlowLayoutAlignment:Near.
        THIS-OBJECT:ultraFlowLayoutManager1:HorizontalGap = 1.
        THIS-OBJECT:ultraFlowLayoutManager1:VerticalAlignment = Infragistics.Win.Layout.DefaultableFlowLayoutAlignment:Near.
        THIS-OBJECT:ultraFlowLayoutManager1:VerticalGap = 1.
        /*  */
        /* BackgroundJobMonitorControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:AutoScroll = TRUE.
        THIS-OBJECT:Name = "BackgroundJobMonitorControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(584, 131).
        CAST(THIS-OBJECT:ultraFlowLayoutManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stops all Background Jobs
        Notes:   
        @return True when no Background Jobs did exist or all could have been closed 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL StopAllBackgroundJobs ():
		
		DEFINE VARIABLE oJobControl AS BackgroundJobProgressControl NO-UNDO . 
		
		{Consultingwerk/foreach.i Control oControl in this-object:Controls}
		
		    IF TYPE-OF (oControl, BackgroundJobProgressControl) THEN DO:
		        
		        oJobControl = CAST (oControl, BackgroundJobProgressControl) .
		        
		        IF VALID-OBJECT (oJobControl:BackgroundJob) THEN DO:
		            
		            IF oJobControl:BackgroundJob:AllowClose = TRUE THEN  
		                oJobControl:BackgroundJob:Stop() .
		            ELSE 
		                RETURN FALSE . 
		        END.
		    END.
		END.

		RETURN TRUE .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BackgroundJobMonitorControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BackgroundJobMonitorControl ():

        FrameworkSettings:ServiceContainer:RemoveService (THIS-OBJECT:GetClass()) .

    END DESTRUCTOR .

END CLASS.
