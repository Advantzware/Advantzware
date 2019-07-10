
/*------------------------------------------------------------------------
    File        : StyleLook.p
    Purpose     : StyleLook

    Syntax      :

    Description : Return a Dataset of all Style Look up

    Author(s)   : Kuldeep
    Created     : Feb 22, 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{StyleLook.i}
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsStyleLook.

DEF VAR prmComp AS CHAR NO-UNDO.

DEFINE VARIABLE vImage AS CHAR NO-UNDO.
DEFINE VARIABLE vImg AS INT NO-UNDO.
DEFINE VARIABLE vIm AS INT NO-UNDO.
DEFINE VARIABLE vIm1 AS CHAR NO-UNDO.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField = ?  THEN ASSIGN prmField  = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    
if prmAction <> "search" then do:
    FOR EACH style WHERE style.company = prmComp and style.style <> "" NO-LOCK :
        create ttStyleLook.
        assign  
          ttStyleLook.style    = style.style
          ttStyleLook.StyleDscr = style.dscr 
          .
        
        FIND FIRST box-design-hdr WHERE
            box-design-hdr.design-no = style.design-no AND
            box-design-hdr.company = style.company
            NO-LOCK NO-ERROR.

        IF AVAIL box-design-hdr THEN
        DO:
           ASSIGN
            ttStyleLook.design_no = box-design-hdr.design-no 
            ttStyleLook.BoxDscr   = box-design-hdr.description
            ttStyleLook.box_design   =  box-design-hdr.box-image
            vImage = box-design-hdr.box-3d-image.

           OS-COPY VALUE( vImage) C:\Inetpub\wwwroot\3D.
            vImg =   R-INDEX(vImage,"\").
            vIm  = 100 - vImg.
            vIm1  = SUBSTRING(vImage, vImg,vIm).
            ttStyleLook.box_3d_image = vIm1.
        END.
                                 
      END.	 /* FOR EACH style */
END.  /*ifif prmAction <> "search" */

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
           FOR EACH style where  style.company = prmComp 
                             AND (style.style = prmText 
                              or style.dscr = prmText 
                              OR style.design-no = int(prmText)    ) no-lock:
              create ttStyleLook.
              assign  
                ttStyleLook.style    = style.style
                ttStyleLook.StyleDscr = style.dscr 
                .
              FIND FIRST box-design-hdr WHERE
                   box-design-hdr.design-no = style.design-no AND
                   box-design-hdr.company = style.company
                   NO-LOCK NO-ERROR.

              IF AVAIL box-design-hdr THEN
              DO:
                 ASSIGN
                     ttStyleLook.design_no = box-design-hdr.design-no 
                     ttStyleLook.BoxDscr   = box-design-hdr.description
                     ttStyleLook.box_design   =  box-design-hdr.box-image
                     vImage = box-design-hdr.box-3d-image.
                
                 OS-COPY VALUE( vImage) C:\Inetpub\wwwroot\3D.
                  vImg =   R-INDEX(vImage,"\").
                  vIm  = 100 - vImg.
                  vIm1  = SUBSTRING(vImage, vImg,vIm).
                  ttStyleLook.box_3d_image = vIm1.
              END.
           END.   /*FOR EACH style*/
    END.   /* IF prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH style where style.company = prmComp 
                             AND (style.style BEGINS prmText 
                              or style.dscr begins prmText
                              OR style.design-no = int(prmText)  ) no-lock:
               create ttStyleLook.
               assign  
                 ttStyleLook.style    = style.style
                 ttStyleLook.StyleDscr = style.dscr 
                 .
               FIND FIRST box-design-hdr WHERE
                    box-design-hdr.company = style.company AND
                    box-design-hdr.design-no = style.design-no
                    NO-LOCK NO-ERROR.

               IF AVAIL box-design-hdr THEN
               DO:
                  ASSIGN
                   ttStyleLook.design_no = box-design-hdr.design-no 
                   ttStyleLook.BoxDscr   = box-design-hdr.description
                   ttStyleLook.box_design   =  box-design-hdr.box-image
                   vImage = box-design-hdr.box-3d-image.

                  OS-COPY VALUE( vImage) C:\Inetpub\wwwroot\3D.
                   vImg =   R-INDEX(vImage,"\").
                   vIm  = 100 - vImg.
                   vIm1  = SUBSTRING(vImage, vImg,vIm).
                   ttStyleLook.box_3d_image = vIm1.
               END.

           END.  /*FOR EACH style*/

    END.   /*if prmCondition = "BEGIN"if prmCondition = "BEGIN"*/     
END.   /*IF prmField = "ANY" then do:*/

if prmField = "style" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH style where style.company = prmComp AND style.style = prmText no-lock:
               create ttStyleLook.
               assign  
                 ttStyleLook.style    = style.style
                 ttStyleLook.StyleDscr = style.dscr 
                 .
               FIND FIRST box-design-hdr WHERE
                    box-design-hdr.design-no = style.design-no AND
                    box-design-hdr.company = style.company                                               
                    NO-LOCK NO-ERROR.

               IF AVAIL box-design-hdr THEN
               DO:
                  ASSIGN
                     ttStyleLook.design_no = box-design-hdr.design-no 
                     ttStyleLook.BoxDscr   = box-design-hdr.description
                     ttStyleLook.box_design   =  box-design-hdr.box-image
                     vImage = box-design-hdr.box-3d-image.

                  OS-COPY VALUE( vImage) C:\Inetpub\wwwroot\3D.
                   vImg =   R-INDEX(vImage,"\").
                   vIm  = 100 - vImg.
                   vIm1  = SUBSTRING(vImage, vImg,vIm).
                   ttStyleLook.box_3d_image = vIm1. 
               END.
           END.  /*FOR EACH style*/
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
       FOR EACH style WHERE style.company = prmComp AND  style.style BEGINS prmText no-lock:
           create ttStyleLook.
           assign  
             ttStyleLook.style    = style.style
             ttStyleLook.StyleDscr = style.dscr 
             .
           FIND FIRST box-design-hdr WHERE
                box-design-hdr.design-no = style.design-no AND
                box-design-hdr.company = style.company
                NO-LOCK NO-ERROR.

           IF AVAIL box-design-hdr THEN
           DO:
              ASSIGN
              ttStyleLook.design_no = box-design-hdr.design-no 
              ttStyleLook.BoxDscr   = box-design-hdr.description
              ttStyleLook.box_design   =  box-design-hdr.box-image
              vImage = box-design-hdr.box-3d-image.

              OS-COPY VALUE( vImage) C:\Inetpub\wwwroot\3D.
               vImg =   R-INDEX(vImage,"\").
               vIm  = 100 - vImg.
               vIm1  = SUBSTRING(vImage, vImg,vIm).
               ttStyleLook.box_3d_image = vIm1.
           END.
       END.  /*FOR EACH style*/
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "style" then do:*/
if prmField = "design_no" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH style where style.company = prmComp AND style.design-no = int(prmText) no-lock:
               create ttStyleLook.
               assign  
                 ttStyleLook.style    = style.style
                 ttStyleLook.StyleDscr = style.dscr 
                 .
               FIND FIRST box-design-hdr WHERE
                    box-design-hdr.design-no = style.design-no AND
                    box-design-hdr.company = style.company
                    NO-LOCK NO-ERROR.

               IF AVAIL box-design-hdr THEN
               DO:
                 ASSIGN
                    ttStyleLook.design_no = box-design-hdr.design-no 
                    ttStyleLook.BoxDscr   = box-design-hdr.description
                    ttStyleLook.box_design   =  box-design-hdr.box-image
                    vImage = box-design-hdr.box-3d-image.
                 
                 OS-COPY VALUE( vImage) C:\Inetpub\wwwroot\3D.
                  vImg =   R-INDEX(vImage,"\").
                  vIm  = 100 - vImg.
                  vIm1  = SUBSTRING(vImage, vImg,vIm).
                  ttStyleLook.box_3d_image = vIm1.
               END.
          END.  /*FOR EACH style*/
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH style WHERE style.company = prmComp AND  style.design-no = int(prmText) no-lock:
               create ttStyleLook.
               assign  
                 ttStyleLook.style    = style.style
                 ttStyleLook.StyleDscr = style.dscr 
                 .
               FIND FIRST box-design-hdr WHERE
                    box-design-hdr.design-no = style.design-no AND
                    box-design-hdr.company = style.company
                    NO-LOCK NO-ERROR.

               IF AVAIL box-design-hdr THEN
               DO:
                 ASSIGN
                   ttStyleLook.design_no = box-design-hdr.design-no 
                   ttStyleLook.BoxDscr   = box-design-hdr.description
                   ttStyleLook.box_design   =  box-design-hdr.box-image
                   vImage = box-design-hdr.box-3d-image.

                 OS-COPY VALUE( vImage) C:\Inetpub\wwwroot\3D.
                  vImg =   R-INDEX(vImage,"\").
                  vIm  = 100 - vImg.
                  vIm1  = SUBSTRING(vImage, vImg,vIm).
                  ttStyleLook.box_3d_image = vIm1.
               END.
           END.  /*FOR EACH style*/
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "design-no" then do:*/
if prmField = "dscr" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH style where style.company = prmComp AND style.dscr = prmText no-lock:
               create ttStyleLook.
               assign  
                   ttStyleLook.style    = style.style
                   ttStyleLook.StyleDscr = style.dscr .
               FIND FIRST box-design-hdr WHERE
                    box-design-hdr.design-no = style.design-no AND
                    box-design-hdr.company = style.company
                    NO-LOCK NO-ERROR.

               IF AVAILABLE box-design-hdr THEN
               DO:
                  ASSIGN
                  ttStyleLook.design_no = box-design-hdr.design-no 
                  ttStyleLook.BoxDscr   = box-design-hdr.description
                  ttStyleLook.box_design   =  box-design-hdr.box-image
                  vImage = box-design-hdr.box-3d-image.

                  OS-COPY VALUE( vImage) C:\Inetpub\wwwroot\3D.
                   vImg =   R-INDEX(vImage,"\").
                   vIm  = 100 - vImg.
                   vIm1  = SUBSTRING(vImage, vImg,vIm).
                   ttStyleLook.box_3d_image = vIm1.
               END.
           END.
    END.
    if prmCondition = "BEGIN" then do:
           FOR EACH style WHERE style.company = prmComp AND  style.dscr begins prmText no-lock:
               create ttStyleLook.
               assign  
                   ttStyleLook.style    = style.style
                   ttStyleLook.StyleDscr = style.dscr 
                   .
              FIND FIRST box-design-hdr WHERE
                   box-design-hdr.design-no = style.design-no AND
                   box-design-hdr.company = style.company
                   NO-LOCK NO-ERROR.

               IF AVAILABLE box-design-hdr THEN
               DO:
                  ASSIGN
                      ttStyleLook.design_no = box-design-hdr.design-no 
                      ttStyleLook.BoxDscr   = box-design-hdr.description
                      ttStyleLook.box_design   =  box-design-hdr.box-image
                      vImage = box-design-hdr.box-3d-image.

                  OS-COPY VALUE( vImage) C:\Inetpub\wwwroot\3D.
                   vImg =   R-INDEX(vImage,"\").
                   vIm  = 100 - vImg.
                   vIm1  = SUBSTRING(vImage, vImg,vIm).
                   ttStyleLook.box_3d_image = vIm1.
               END.
           END.
    END.        
END.


END. /* IF prmAction = search then do: */




