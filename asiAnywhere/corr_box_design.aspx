<%@ Page Language="C#" MasterPageFile="~/MasterPageCorrugated.master" Debug="true" AutoEventWireup="true" Inherits="corr_box_design" Title="Corrugated Box Design" Codebehind="corr_box_design.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

<script>
    window.onload = setfocus;
    function setfocus()
    {
        if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vdesign_dscrTextBox"))
        {
            if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vdesign_dscrTextBox").disabled != true)
            {
                var desc = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vdesign_dscrTextBox");
                desc.focus();
            }
            else
            {
                var path = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vbox_3d_imageTextBox");
                path.focus();
            }
        }
    }
    
    function checkvalid() {
        if (document.getElementById("ctl00_ContentPlaceHolder1_FormView1_FileUpload1").value == "") {
            if (document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vdesign_dscrTextBox").disabled != true) {
                var path = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vbox_imageTextBox");
                if (path.value.indexOf(".jpg") > -1 || path.value.indexOf(".jpeg") > -1 || path.value.indexOf(".bmp") > -1 || path.value.indexOf(".tif") > -1
               || path.value.indexOf(".JPG") > -1 || path.value.indexOf(".JPEG") > -1 || path.value.indexOf(".BMP") > -1 || path.value.indexOf(".TIF") > -1
               || path.value.indexOf(".Jpg") > -1 || path.value.indexOf(".Jpeg") > -1 || path.value.indexOf(".Bmp") > -1 || path.value.indexOf(".Tif") > -1) {
                    return true;
                }
                else {
                    alert("Invalid file! Only .jpg,.jpeg,.bmp,.tif files are allowed.");
                    path.focus();
                    return false;

                }
            }
            else
            //if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vdesign_dscrTextBox").disabled == true)
            {
                var path = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vbox_3d_imageTextBox");
                if (path.value.indexOf(".jpg") > -1 || path.value.indexOf(".jpeg") > -1 || path.value.indexOf(".bmp") > -1 || path.value.indexOf(".tif") > -1
               || path.value.indexOf(".JPG") > -1 || path.value.indexOf(".JPEG") > -1 || path.value.indexOf(".BMP") > -1 || path.value.indexOf(".TIF") > -1
               || path.value.indexOf(".Jpg") > -1 || path.value.indexOf(".Jpeg") > -1 || path.value.indexOf(".Bmp") > -1 || path.value.indexOf(".Tif") > -1) {
                    return true;
                }
                else {
                    alert("Invalid file! Only .jpg,.jpeg,.bmp,.tif files are allowed.");
                    path.focus();
                    return false;

                }
            }
        }
    }
</script>
     <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound">
        <EditItemTemplate>
        <asp:Panel ID="Panel_Edit" runat="server" DefaultButton="UpdateButton">
            <fieldset class="shade">
                <fieldset>
                    <legend>Reference Information:</legend>
                        <table>
                            <tr>
                                <td nowrap>Estimate#:</td>
                                <td nowrap>
                                    <asp:Label ID="vest_noLabel" Width="50px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vest_no") %>'></asp:Label>
                                </td>
                                <td nowrap>Est Date:</td>
                                <td nowrap>
                                    <asp:Label ID="vEstDateLabel" Width="70px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                </td>
                                <td nowrap>Frm:</td>
                                <td nowrap>
                                    <asp:Label ID="vform_noLabel" Width="20px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vform_no") %>'></asp:Label>
                                        of
                                    <asp:Label ID="vFormQtyLabel" Width="20px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vFormQty") %>'></asp:Label>
                                </td>
                                <td nowrap>Blk:</td>
                                <td nowrap>
                                    <asp:Label ID="vblank_noLabel" Width="20px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vblank_no") %>'></asp:Label>
                                        of
                                    <asp:Label ID="vBlankQtyLabel" Width="20px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBlankQty") %>'></asp:Label> 
                                </td>
                                <td nowrap>Cust Part#:</td>
                                <td nowrap>
                                    <asp:Label ID="vCustPartLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label>
                                </td>
                                
                            </tr>
                        </table>
                </fieldset>
                <fieldset>
                    <table>
                        <tr>
                            <td nowrap>Design#:</td>
                            <td nowrap style="width:300px; padding-left:5px;" align="left">
                                <asp:Label ID="vdesign_noLabel" Width="30px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vdesign_no") %>'></asp:Label>
                                &nbsp;&nbsp;
                                <asp:TextBox ID="vdesign_dscrTextBox" Width="250px" runat="server" Text='<%# Bind("vdesign_dscr") %>'></asp:TextBox>
                            </td>
                            <td nowrap>&nbsp;</td>
                            
                            <td colspan="5" nowrap>Image File: 
                                <asp:TextBox ID="vbox_imageTextBox" Width="200px" runat="server" Text='<%# Bind("vbox_image") %>'></asp:TextBox>
                                
                                <asp:TextBox ID="vbox_3d_imageTextBox" Width="200px" runat="server" Text='<%# Bind("vbox_3d_image") %>'></asp:TextBox>
                                <asp:FileUpload  ID="FileUpload1" Height="20px" onfocus="window.scroll(800,800)" Width="150px" runat="server" />
                                <asp:RegularExpressionValidator id="FileUpLoadValidator"  Display="Dynamic" SetFocusOnError="true" runat="server" ErrorMessage="Invalid File only JPG Image" 
                                    ValidationExpression="^(([a-zA-Z]:)|(\\{2}\w+)\$?)(\\(\w[\w].*))(.jpg|.JPG)$" ControlToValidate="FileUpload1">
                                </asp:RegularExpressionValidator>
                            </td>
                        </tr>

                        <tr>
                            <td>&nbsp;</td>                         
                            <td nowrap align="center">                            
                            <table align="center" width="100%"><tr>                                
                                <td align="right">&nbsp;<asp:TextBox ID="vlscoreTextBox" Visible="false"  runat="server" Text='<%# Bind("vlscore") %>'></asp:TextBox></td>
                                <td align="right"><asp:TextBox ID="TextBoxvlscore" style="text-align:center" Width="50px" runat="server" ></asp:TextBox></td>
                                <td align="right"><asp:TextBox ID="TextBoxvlscore2" style="text-align:center" Width="50px" runat="server" ></asp:TextBox>
                                <td align="right"><asp:TextBox ID="TextBoxvlscore3" style="text-align:center" Width="70px" runat="server" ></asp:TextBox>
                                <td align="right"><asp:TextBox ID="TextBoxvlscore4" style="text-align:center" Width="70px" runat="server" ></asp:TextBox>
                                <td>
                                <asp:TextBox ID="TextBoxvlscore5" style="text-align:center" Width="70px" runat="server" ></asp:TextBox>
                                
                            &nbsp;&nbsp; Score</td>
                            </tr></table>
                            </td>
                            <td nowrap></td>
                            
                            <td nowrap></td>
                            <td nowrap></td>
                            <td nowrap></td>
                        </tr>
                        <tr>
                            <td>&nbsp;</td>                         
                            <td nowrap align="center">                            
                            <table align="center" width="100%"><tr>
                                <td align="right">&nbsp;<asp:Label ID="Label1" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                                <td align="right"><asp:Label ID="Label2" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                                <td align="right"><asp:Label ID="Label3" Width="80px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                                <td align="right"><asp:Label ID="Label4" Width="80px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                                <td align="right"><asp:Label ID="Label5" Width="75px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                                <td>
                                <asp:TextBox ID="vlcum_scoreLabel" Enabled="false" Visible="false" Width="500px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vlcum_score") %>'></asp:TextBox>
                            &nbsp;&nbsp; Total</td>
                            </tr></table>
                            </td>
                            <td nowrap></td>
                            
                            <td nowrap>W Totals</td>
                            <td nowrap></td>
                            <td nowrap>W Score</td>
                        </tr>
                        
                        <tr valign="top">
                            <td nowrap>&nbsp;</td>
                            <td colspan="2" nowrap>
                                <asp:Image ID="Image1" Width="500px" Height="200px" runat="server" />&nbsp;&nbsp;&nbsp;&nbsp;
                            </td>
                            <td nowrap> <br /><br />
                            <asp:Label ID="wtot1Label" Width="60px" Height="21px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label> <br /><br /><br /><br />
                                <asp:Label ID="wtot2Label"  Width="60px" Height="21px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label><br /><br /><br /><br />
                                <asp:Label ID="wtot3Label" Width="60px" Height="21px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label>
                                <asp:Label ID="vwscoreLabel" Visible="false"  BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vwcum_score") %>'></asp:Label>
                            </td>
                             <td nowrap style="width:10px"><br /></td>
                            <td nowrap><br /><br />
                                <asp:TextBox ID="vwcum_scoreTextBox" Visible="false"  runat="server" Text='<%# Bind("vwscore") %>'></asp:TextBox>
                                <asp:TextBox ID="TextBox1" Width="50px" runat="server" ></asp:TextBox><br /><br /><br /><br />
                                <asp:TextBox ID="TextBox2" Width="50px"  runat="server" ></asp:TextBox><br /><br /><br /><br />
                                <asp:TextBox ID="TextBox3" Width="50px" runat="server" ></asp:TextBox>
                                
                            </td>
                           
                            <td nowrap></td>
                        </tr>
                    </table>
                </fieldset>
                
                <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" CausesValidation="True" Text="Save" OnClick="btn_Update_Click" OnClientClick="return checkvalid()">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel" OnClick="btn_UpdateCancel_Click"
                    Text="Cancel">
                </asp:Button>
            
            </fieldset>
            
            <%--vdesign_no:
            <asp:TextBox ID="vdesign_noTextBox" runat="server" Text='<%# Bind("vdesign_no") %>'>
            </asp:TextBox><br />          
            vlcum_score:
            <asp:TextBox ID="vlcum_scoreTextBox" runat="server" Text='<%# Bind("vlcum_score") %>'>
            </asp:TextBox><br />
            venum:
            <asp:TextBox ID="venumTextBox" runat="server" Text='<%# Bind("venum") %>'>
            </asp:TextBox><br />
            vform_no:
            <asp:TextBox ID="vform_noTextBox" runat="server" Text='<%# Bind("vform_no") %>'>
            </asp:TextBox><br />
            vblank_no:
            <asp:TextBox ID="vblank_noTextBox" runat="server" Text='<%# Bind("vblank_no") %>'>
            </asp:TextBox><br />
            vest_no:
            <asp:TextBox ID="vest_noTextBox" runat="server" Text='<%# Bind("vest_no") %>'>
            </asp:TextBox><br />
            veqty:
            <asp:TextBox ID="veqtyTextBox" runat="server" Text='<%# Bind("veqty") %>'>
            </asp:TextBox><br />            
            vwscore:
            <asp:TextBox ID="vwscoreTextBox" runat="server" Text='<%# Bind("vwscore") %>'>
            </asp:TextBox><br />            
            vbox_text:
            <asp:TextBox ID="vbox_textTextBox" runat="server" Text='<%# Bind("vbox_text") %>'>
            </asp:TextBox><br />            
            vBlankQty:
            <asp:TextBox ID="vBlankQtyTextBox" runat="server" Text='<%# Bind("vBlankQty") %>'>
            </asp:TextBox><br />
            vFormQty:
            <asp:TextBox ID="vFormQtyTextBox" runat="server" Text='<%# Bind("vFormQty") %>'>
            </asp:TextBox><br />
            vCustPart:
            <asp:TextBox ID="vCustPartTextBox" runat="server" Text='<%# Bind("vCustPart") %>'>
            </asp:TextBox><br />
            vEstDate:
            <asp:TextBox ID="vEstDateTextBox" runat="server" Text='<%# Bind("vEstDate") %>'>
            </asp:TextBox><br />--%> 
           </asp:Panel> 
        </EditItemTemplate>
        <ItemTemplate>
            <fieldset class="shade">
                <fieldset>
                    <legend>Reference Information:</legend>
                        <table>
                            <tr>
                                <td nowrap>Estimate#:</td>
                                <td nowrap>
                                    <asp:Label ID="vest_noLabel" Width="50px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vest_no") %>'></asp:Label>
                                </td>
                                <td nowrap>Est Date:</td>
                                <td nowrap>
                                    <asp:Label ID="vEstDateLabel" Width="70px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                </td>
                                <td nowrap>Frm:</td>
                                <td nowrap>
                                    <asp:Label ID="vform_noLabel" Width="20px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vform_no") %>'></asp:Label>
                                        of
                                    <asp:Label ID="vFormQtyLabel" Width="20px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vFormQty") %>'></asp:Label>
                                </td>
                                <td nowrap>Blk:</td>
                                <td nowrap>
                                    <asp:Label ID="vblank_noLabel" Width="20px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vblank_no") %>'></asp:Label>
                                        of
                                    <asp:Label ID="vBlankQtyLabel" Width="20px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBlankQty") %>'></asp:Label> 
                                </td>
                                <td nowrap>Cust Part#:</td>
                                <td nowrap>
                                    <asp:Label ID="vCustPartLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label>
                                </td>
                                
                            </tr>
                        </table>
                </fieldset>
                <fieldset>
                    <table>
                        <tr>
                            <td nowrap>Design#:</td>
                            <td nowrap style="width:300px; padding-left:5px;" align="left">
                                <asp:Label ID="vdesign_noLabel" Width="30px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vdesign_no") %>'></asp:Label>
                                &nbsp;&nbsp;&nbsp;
                                <asp:Label ID="vdesign_dscrLabel" Width="200px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vdesign_dscr") %>'></asp:Label>
                            </td>
                            <td nowrap>&nbsp;</td>
                            
                            <td colspan="4" nowrap>Image File: &nbsp;
                                <asp:Label ID="vbox_imageLabel" Width="200px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vbox_image") %>'></asp:Label>
                                
                                <asp:Label ID="vbox_3d_imageLabel" Width="200px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vbox_3d_image") %>'></asp:Label>
                            </td>
                        </tr>                                           
                        
                        <tr>
                            <td>&nbsp;</td>                         
                            <td nowrap align="center">                            
                            <table align="center" width="100%"><tr>
                            <td align="right">&nbsp;<asp:Label ID="sc1Label" Width="50px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                            <td align="right"><asp:Label ID="sc2Label" Width="50px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                            <td align="right"><asp:Label ID="sc3Label" Width="70px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                            <td align="right"><asp:Label ID="sc4Label" Width="70px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                            <td align="right"><asp:Label ID="sc5Label" Width="70px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                            <td>
                                <asp:TextBox ID="vlscoreLabel" ReadOnly="true" Visible="false" Width="500px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vlscore") %>'></asp:TextBox>
                             &nbsp; Score</td>
                             </tr></table>
                            </td>
                                                       
                            <td nowrap></td>
                            
                            <td nowrap></td>
                            <td nowrap></td>
                            <td nowrap></td>
                        </tr>
                        
                        <tr>
                            <td>&nbsp;</td>
                            <td nowrap align="center">                                                       
                            <table align="center" width="100%"><tr>
                            <td align="right">&nbsp;<asp:Label ID="Label1" Width="50px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                            <td align="right"><asp:Label ID="Label2" Width="50px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                            <td align="right"><asp:Label ID="Label3" Width="70px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                            <td align="right"><asp:Label ID="Label4" Width="70px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                            <td align="right"><asp:Label ID="Label5" Width="70px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label></td>
                                <td><asp:TextBox ReadOnly="true" Visible="false" ID="vlcum_scoreLabel" Width="500px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vlcum_score") %>'></asp:TextBox>
                          &nbsp; Total</td> </tr></table>
                          </td>
                            <td nowrap></td>
                            
                            <td nowrap>W Totals </td>
                            <td nowrap></td>
                            <td nowrap> W Score</td>
                        </tr>
                        
                        <tr valign="top">
                            <td nowrap>&nbsp;</td>
                            <td nowrap colspan="2">
                                <asp:Image ID="Image1" Width="500px" Height="200px" runat="server" />   &nbsp;&nbsp;&nbsp;&nbsp;                                                                                                                                                                                      
                            </td>
                            
                            <td nowrap>                           
                            <br />
                                <asp:TextBox ReadOnly="true"  Visible="false" ID="vwcum_scoreLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vwcum_score") %>'></asp:TextBox><br />
                                <asp:Label ID="wtot1Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label><br /><br /><br /><br /><br />
                                <asp:Label ID="wtot2Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label><br /><br /><br /><br /><br />
                                <asp:Label ID="wtot3Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label>
                            </td>
                            <td ></td>
                            <td nowrap>
                            <br />
                                <asp:TextBox ReadOnly="true" Visible="false" ID="vwscoreLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vwscore") %>'></asp:TextBox><br />
                                 <asp:Label ID="wsc1Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label><br /><br /><br /><br /><br />
                                 <asp:Label ID="wsc2Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label><br /><br /><br /><br /><br />
                                 <asp:Label ID="wsc3Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label>
                            </td>                            
                           
                        </tr>
                        <tr><td><asp:Label runat="server" Visible="false" ID="die_label" Text='<%# Bind("vDieImage") %>'></asp:Label>
                        </td></tr>                       
                    </table>
                </fieldset>
                
                <asp:Button ID="btn_Update" runat="server" CssClass="buttonM" Text="Update" CommandName="Edit" />
                <asp:Button ID="Button_2dImage" runat="server" CssClass="buttonM" Text="CAD/2D" OnClick="btn_cad2d_click" />
                <asp:Button ID="btn_3dImage" runat="server" CssClass="buttonM" Text="3D Image" OnClick="btn_3dimage_click" />
                <asp:Button ID="die_image_button" runat="server" CssClass="buttonM" Text="Die" OnClick="die_button_click" />
            </fieldset>
                                  
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectBoxDesign" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" DefaultValue="admin" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmDesignNo" Type="Int32" />
            <asp:Parameter Name="prmDesignDscr" Type="String" />
            <asp:Parameter Name="prmLScore" Type="String" />
            <asp:Parameter Name="prmLCumScore" Type="String" />
            <asp:Parameter Name="prmEnum" Type="Int32" />
            <asp:SessionParameter DefaultValue="" Name="prmFormNo" SessionField="order_corrugated_formno" Type="Int32" />
            <asp:SessionParameter  Name="prmBlankNo" SessionField="order_corrugated_blankno" Type="Int32" />
            <asp:SessionParameter Name="prmEstNo" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmEqty" Type="Decimal" />
            <asp:Parameter Name="prmBoxImage" Type="String" />
            <asp:Parameter Name="prmWScore" Type="String" />
            <asp:Parameter Name="prmWCumScore" Type="String" />
            <asp:Parameter Name="prmBoxText" Type="String" />
            <asp:Parameter Name="prmBox3DImage" Type="String" />
            
        </SelectParameters>
    </asp:ObjectDataSource>
    
</asp:Content>

