<%@ Page Language="C#" MasterPageFile="~/MasterPagePo.master" Debug="true" AutoEventWireup="true" Inherits="po_box_design" Title="Corrugated Box Design" Codebehind="po_box_design.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

<script>
    
    
    function checkvalid()
    {
        if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vdesign_dscrTextBox").disabled != true)
        {
            var path = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vbox_imageTextBox");
            if(path.value.indexOf(".jpg") > -1 || path.value.indexOf(".jpeg") > -1 || path.value.indexOf(".bmp") > -1 || path.value.indexOf(".tif") > -1
               || path.value.indexOf(".JPG") > -1 || path.value.indexOf(".JPEG") > -1 || path.value.indexOf(".BMP") > -1 || path.value.indexOf(".TIF") > -1
               || path.value.indexOf(".Jpg") > -1 || path.value.indexOf(".Jpeg") > -1 || path.value.indexOf(".Bmp") > -1 || path.value.indexOf(".Tif") > -1 )
            {
                return true;
            }
            else
            {
                alert("Invalid file! Only .jpg,.jpeg,.bmp,.tif files are allowed.");
                path.focus();
                return false;
                
            }
        }
        else
        //if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vdesign_dscrTextBox").disabled == true)
        {
            var path = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vbox_3d_imageTextBox");
            if(path.value.indexOf(".jpg") > -1 || path.value.indexOf(".jpeg") > -1 || path.value.indexOf(".bmp") > -1 || path.value.indexOf(".tif") > -1
               || path.value.indexOf(".JPG") > -1 || path.value.indexOf(".JPEG") > -1 || path.value.indexOf(".BMP") > -1 || path.value.indexOf(".TIF") > -1
               || path.value.indexOf(".Jpg") > -1 || path.value.indexOf(".Jpeg") > -1 || path.value.indexOf(".Bmp") > -1 || path.value.indexOf(".Tif") > -1 )
            {
                return true;
            }
            else
            {
                alert("Invalid file! Only .jpg,.jpeg,.bmp,.tif files are allowed.");
                path.focus();
                return false;
                
            }
        }
    }
</script>
<div>
<fieldset  style="height:30px;width:823px" class="shade">
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3">
        
        
        <ItemTemplate>
        <table class="shade">
        <tr><td><b>Po Number:</b></td>
        <td> <asp:Label ID="poNoLabel"  BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px" runat="server" Text='<%# Bind("poNo") %>' /></td>
        <td><b>Po Date#:</b></td>
        <td><asp:Label ID="poDateLabel" runat="server"  BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="100px" Text='<%# Bind("poDate") %>' /></td>
        <td><b>Whse:</b></td>
        <td><asp:Label ID="poLocLabel" runat="server"  BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="100px" Text='<%# Bind("poLoc") %>' /></td>
        <td><b>Type:</b></td>
        <td><asp:Label ID="poTypeLabel" runat="server"  BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="100px" Text='<%# Bind("poType") %>' /></td>
        <td><asp:Label ID="poTMsfLabel" runat="server"  BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="100px" Text='<%# Bind("poTMsf") %>' /></td>
        <td><b>MSF</b></td>
        </tr>
        </table>            
        </ItemTemplate>
    </asp:FormView></fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectListItemPO" 
        TypeName="browspo">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmpoNo" SessionField="pur_ord_po" 
                Type="Int32" />
            <asp:Parameter Name="prmpoItemNo" Type="String" />
            <asp:Parameter Name="prmpoRecKey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
     <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:Label ID="Label6" runat="server" ForeColor="Red" ></asp:Label>
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound">
        <ItemTemplate>
        <fieldset>
                    <table>
                        <tr>
                            <td nowrap>Design#:</td>
                            <td nowrap style="width:300px; padding-left:5px;" align="left">
                                <asp:Label ID="vdesign_noLabel" Width="30px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("design_no") %>'></asp:Label>
                                &nbsp;&nbsp;&nbsp;
                                <asp:Label ID="vdesign_dscrLabel" Width="200px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("design_dscr") %>'></asp:Label>
                            </td>
                            <td nowrap>&nbsp;</td>
                            
                            <td colspan="4" nowrap>Image File: &nbsp;
                                <asp:Label ID="vbox_imageLabel" Width="200px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("box_image") %>'></asp:Label>
                                
                                <asp:Label ID="vbox_3d_imageLabel" Width="200px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("box_3d_image") %>'></asp:Label>
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
                                <asp:TextBox ID="vlscoreLabel" ReadOnly="true" Visible="false" Width="500px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("lscore") %>'></asp:TextBox>
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
                                <td><asp:TextBox ReadOnly="true" Visible="false" ID="vlcum_scoreLabel" Width="500px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("lcum_score") %>'></asp:TextBox>
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
                                <asp:TextBox ReadOnly="true"  Visible="false" ID="vwcum_scoreLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("wcum_score") %>'></asp:TextBox><br />
                                <asp:Label ID="wtot1Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label><br /><br /><br /><br /><br />
                                <asp:Label ID="wtot2Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label><br /><br /><br /><br /><br />
                                <asp:Label ID="wtot3Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label>
                            </td>
                            <td ></td>
                            <td nowrap>
                            <br />
                                <asp:TextBox ReadOnly="true" Visible="false" ID="vwscoreLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("wscore") %>'></asp:TextBox><br />
                                 <asp:Label ID="wsc1Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label><br /><br /><br /><br /><br />
                                 <asp:Label ID="wsc2Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label><br /><br /><br /><br /><br />
                                 <asp:Label ID="wsc3Label" Width="60px" style="text-align:center" BackColor="Turquoise" runat="server" ></asp:Label>
                            </td>                            
                           
                        </tr>
                        <tr><td><asp:Label runat="server" Visible="false" ID="die_label" Text='<%# Bind("DieImage") %>'></asp:Label>
                        </td></tr>                       
                    </table>
                </fieldset>
                                  
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectPoDesign" TypeName="browspo">
        <SelectParameters>
            <asp:Parameter Name="prmUser" DefaultValue="admin" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmPoNo" SessionField="pur_ord_po" Type="Int32" />
            <asp:Parameter Name="prmReckey" Type="String" />
            <asp:SessionParameter Name="prmLineno" SessionField="pur_ord_po_line_no" Type="Int32" />
            
            
        </SelectParameters>
    </asp:ObjectDataSource>
    
</asp:Content>

