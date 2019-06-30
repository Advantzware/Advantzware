<%@ Page Language="C#" MasterPageFile="MasterPage5.master" AutoEventWireup="true" Inherits="rfqsize" Title="Rfq Size" Codebehind="rfq_size.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="server">
<script type="text/javascript">
function MatSizelook()
  { 
  var NewWindow = window.open("MatsizeLook.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function MatSizeLookUp(ReturnObj1)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$adhesiveTextBox.value = ReturnObj1;
  //document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr1TextBox.value=ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_adhesiveTextBox.focus();
  
}
</script>
<div>
<fieldset style="background-color:#EFF3FB">
<legend>Reference Information</legend>
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
       
        <ItemTemplate>
        
           <b>RFQ#:</b>
            <asp:Label ID="aRfqNoLabel" runat="server" BackColor="Turquoise" Width="163px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aRfqNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <b> Customer#:</b>
            <asp:Label ID="aCustNoLabel" runat="server" BackColor="Turquoise" Width="163px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aCustNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <b> Requested Date:</b>
            <asp:Label ID="vRfqDtLabel" runat="server" BackColor="Turquoise" Width="163px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqDt","{0:MM/dd/yyyy}") %>'></asp:Label>
        </ItemTemplate>
        
    </asp:FormView>
    
</fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqItemDscr" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" /> 
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqsize" Type="Int32" />
            <asp:SessionParameter SessionField="list_rfq_cust_part_no" Name="prmPartNo" Type="string" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>

<div>
<br />
    <asp:FormView ID="FormView1" runat="server"  DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound">
        <ItemTemplate>
        <asp:Panel ID="Item_panel" runat="server" DefaultButton="EditButton">
        <fieldset  style="background-color:#EFF3FB;" >
        <table style="width: 914px">
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Cust Part#:</b></td>
        <td><b><asp:Label ID="Label1" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("part_no") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="Label2" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("i_name") %>'></asp:Label></b></td>
        <td><b></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Style Code:</b></td>
        <td><b><asp:Label ID="Label3" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("style") %>'></asp:Label></b></td>
           <td><b><asp:Label ID="vStyleDscrLabel" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vStyleDscr") %>'></asp:Label>
        </b></td>
        <td><b></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Tab In:</b></td>
        
        <td><b><asp:Label ID="tab_inCheckBox" runat="server" Text='<%# Bind("tab_in") %>' BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Length:</b></td>
        <td><b><asp:Label ID="Label4" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("len","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Width:</b></td>
        <td><b><asp:Label ID="Label5" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("wid","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Depth:</b></td>
        <td><b><asp:Label ID="Label6" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("dep","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Bottom/Dust Flap:</b></td>
        <td><b><asp:Label ID="Label7" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("dust","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>5th Panel:</b></td>
        <td><b><asp:Label ID="Label8" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("fpanel","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Top Flap or Tuck:</b></td>
        <td><b><asp:Label ID="Label9" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("tuck","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Adhesive:</b></td>
        <td><b><asp:Label ID="Label10" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("adhesive") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Score/DK Width:</b></td>
        <td><b><asp:Label ID="Label11" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("k_wid","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Score/DK Length:</b></td>
        <td><b><asp:Label ID="Label12" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("k_len","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Lock Tab:</b></td>
        <td><b><asp:Label ID="Label13" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("lock","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Glue Lap:</b></td>
        <td><b><asp:Label ID="Label14" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("gluelap","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Lin Inches:</b></td>
        <td><b><asp:Label ID="Label15" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("lin_in","{0:###,###,##0.00###}") %>'></asp:Label></b></td>        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Blank Width:</b></td>
        <td><b><asp:Label ID="Label16" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("t_wid","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Blank Length:</b></td>
        <td><b><asp:Label ID="Label17" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("t_len","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Blank Sq. In.:</b></td>
        <td><b><asp:Label ID="lv_sqinLabel" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("lv_sqin","{0:###,###,##0.00##}") %>'></asp:Label></b></td>
        </tr>
        </table>
        </fieldset>
         <table><tr>
        <td width="100px"></td>
        
        <td align="center"><asp:Button ID="EditButton" CausesValidation="false" Text="Override" CssClass="buttonM" runat="server" CommandName="Edit" /></td>
        </tr></table>   
                 
        </asp:Panel>
           
        </ItemTemplate>
        <EditItemTemplate>
        <asp:Panel ID="Edit_panel" runat="server" DefaultButton="Button1">
        <fieldset style="background-color:#EFF3FB;">
        <table style="width: 914px">
        <tr>
        <td><b>Cust Part#:</b></td>
                
                <td><asp:Label   Width="170px"  BorderWidth="1px" ID="part_noTextBox" runat="server" Text='<%# Bind("part_no") %>'>
                </asp:Label><br /></td>
                <td>
                <asp:Label   ID="i_nameTextBox" runat="server" Text='<%# Bind("i_name") %>'>
                </asp:Label></td></tr>
                <tr><td>
                <b>Style:</b></td><td>
                <asp:Label ID="styleTextBox"  Width="170px"  runat="server" Text='<%# Bind("style") %>'>
                </asp:Label></td><td>
                <asp:Label ID="vStyleDscrTextBox"  runat="server" Text='<%# Bind("vStyleDscr") %>'>
                </asp:Label></td>
                <td><br /></td>
                <td align="center" style="padding-left:3px;"><b>Tab:</b></td><td>
                <asp:TextBox ID="tab_inTextBox" runat="server" Text='<%# Bind("tab_in") %>'>
                </asp:TextBox></td></tr>
                <tr> <td><b>Length:</b></td><td>
                <asp:TextBox ID="lenTextBox" runat="server" Text='<%# Bind("len") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Number" ControlToValidate="lenTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                <td align="center" style="padding-left:3px;"><b>Width:</b></td><td>
                <asp:TextBox ID="widTextBox" runat="server" Text='<%# Bind("wid") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Number" ControlToValidate="widTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                <td  align="center" style="padding-left:3px;"><b>Depth:</b></td><td>
                <asp:TextBox ID="depTextBox" runat="server" Text='<%# Bind("dep") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Number" ControlToValidate="depTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td></tr>
                <tr><td ><b>Bottom/Dust Flap:</b></td><td>
                <asp:TextBox ID="dustTextBox" runat="server" Text='<%# Bind("dust") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator4" runat="server" ErrorMessage="Only Number" ControlToValidate="dustTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                <td align="center" style="padding-left:3px;"><b>5th.Panel:</b></td><td>
                <asp:TextBox ID="fpanelTextBox" runat="server" Text='<%# Bind("fpanel") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator5" runat="server" ErrorMessage="Only Number" ControlToValidate="fpanelTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                <td align="center" style="padding-left:3px;"><b>Top Flap or Tuck:</b></td><td>
                <asp:TextBox ID="tuckTextBox" runat="server" Text='<%# Bind("tuck") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator6" runat="server" ErrorMessage="Only Number" ControlToValidate="tuckTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td></tr>
                <tr><td><b>Adhesive:</b></td><td>
                <asp:TextBox ID="adhesiveTextBox" runat="server" Text='<%# Bind("adhesive") %>'>
                </asp:TextBox><a href="#" tabindex="1" onClick="MatSizelook(); return false" ><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
                <td align="center" style="padding-left:3px;"><b>Score/DK Width:</b></td><td>
                <asp:TextBox ID="k_widTextBox" runat="server" Text='<%# Bind("k_wid") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator7" runat="server" ErrorMessage="Only Number" ControlToValidate="k_widTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                <td align="center" style="padding-left:3px;"><b>Score/DK Length:</b></td><td>
                <asp:TextBox ID="k_lenTextBox" runat="server" Text='<%# Bind("k_len") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Only Number" ControlToValidate="k_lenTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td></tr>
                <tr><td><b>Lock Tab:</b></td><td>
                <asp:TextBox ID="lockTextBox" runat="server" Text='<%# Bind("lock") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator9" runat="server" ErrorMessage="Only Number" ControlToValidate="lockTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                <td align="center" style="padding-left:3px;"><b>Glue Lap:</b></td><td>
                <asp:TextBox ID="gluelapTextBox" runat="server" Text='<%# Bind("gluelap") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator10" runat="server" ErrorMessage="Only Number" ControlToValidate="gluelapTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                <td align="center" style="padding-left:3px;"><b>Lin Inches:</b></td><td>
                <asp:TextBox ID="lin_inTextBox" runat="server" Text='<%# Bind("lin_in") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator11" runat="server" ErrorMessage="Only Number" ControlToValidate="lin_inTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td></tr>
                <tr><td><b>Blank Width:</b></td><td>
                <asp:TextBox ID="t_widTextBox" runat="server" Text='<%# Bind("t_wid") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator12" runat="server" ErrorMessage="Only Number" ControlToValidate="t_widTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
               <td align="center" style="padding-left:3px;"><b>Blank Length:</b></td><td>
                <asp:TextBox ID="t_lenTextBox" runat="server" Text='<%# Bind("t_len") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator13" runat="server" ErrorMessage="Only Number" ControlToValidate="t_lenTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                
                <td align="center" style="padding-left:3px;"><b>Blank Sq.in:</b></td><td>
                <asp:TextBox ID="lv_sqinTextBox" runat="server" Text='<%# Bind("lv_sqin","{0:###,###,##0.00##}") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator14" runat="server" ErrorMessage="Only Number" ControlToValidate="lv_sqinTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                </td>
                </tr></table>
                
                
                <table>
           <tr>
           <td width="100"></td>
           <td align="center"><asp:Button ID="Button1" runat="server" OnClick="UpdateButton_click"
                Text="Save" Font-Bold="true">
            </asp:Button></td>
           <td align="center" style="padding-right:20px;"><asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel" Font-Bold="true"></asp:Button></td>
           </tr>
           
            </table></fieldset></asp:Panel>
            </EditItemTemplate>
            <InsertItemTemplate>
                part_no:
                <asp:TextBox ID="part_noTextBox" runat="server" Text='<%# Bind("part_no") %>'>
                </asp:TextBox><br />
                i_name:
                <asp:TextBox ID="i_nameTextBox" runat="server" Text='<%# Bind("i_name") %>'>
                </asp:TextBox><br />
                style:
                <asp:TextBox ID="styleTextBox" runat="server" Text='<%# Bind("style") %>'>
                </asp:TextBox><br />
                tab_in:
                <asp:TextBox ID="tab_inTextBox" runat="server" Text='<%# Bind("tab_in") %>'>
                </asp:TextBox><br />
                len:
                <asp:TextBox ID="lenTextBox" runat="server" Text='<%# Bind("len") %>'>
                </asp:TextBox><br />
                wid:
                <asp:TextBox ID="widTextBox" runat="server" Text='<%# Bind("wid") %>'>
                </asp:TextBox><br />
                dep:
                <asp:TextBox ID="depTextBox" runat="server" Text='<%# Bind("dep") %>'>
                </asp:TextBox><br />
                dust:
                <asp:TextBox ID="dustTextBox" runat="server" Text='<%# Bind("dust") %>'>
                </asp:TextBox><br />
                fpanel:
                <asp:TextBox ID="fpanelTextBox" runat="server" Text='<%# Bind("fpanel") %>'>
                </asp:TextBox><br />
                tuck:
                <asp:TextBox ID="tuckTextBox" runat="server" Text='<%# Bind("tuck") %>'>
                </asp:TextBox><br />
                adhesive:
                <asp:TextBox ID="adhesiveTextBox" runat="server" Text='<%# Bind("adhesive") %>'>
                </asp:TextBox><br />
                k_wid:
                <asp:TextBox ID="k_widTextBox" runat="server" Text='<%# Bind("k_wid") %>'>
                </asp:TextBox><br />
                k_len:
                <asp:TextBox ID="k_lenTextBox" runat="server" Text='<%# Bind("k_len") %>'>
                </asp:TextBox><br />
                lock:
                <asp:TextBox ID="lockTextBox" runat="server" Text='<%# Bind("lock") %>'>
                </asp:TextBox><br />
                gluelap:
                <asp:TextBox ID="gluelapTextBox" runat="server" Text='<%# Bind("gluelap") %>'>
                </asp:TextBox><br />
                lin_in:
                <asp:TextBox ID="lin_inTextBox" runat="server" Text='<%# Bind("lin_in") %>'>
                </asp:TextBox><br />
                t_wid:
                <asp:TextBox ID="t_widTextBox" runat="server" Text='<%# Bind("t_wid") %>'>
                </asp:TextBox><br />
                t_len:
                <asp:TextBox ID="t_lenTextBox" runat="server" Text='<%# Bind("t_len") %>'>
                </asp:TextBox><br />
                vStyleDscr:
                <asp:TextBox ID="vStyleDscrTextBox" runat="server" Text='<%# Bind("vStyleDscr") %>'>
                </asp:TextBox><br />
                lv_sqin:
                <asp:TextBox ID="lv_sqinTextBox" runat="server" Text='<%# Bind("lv_sqin") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                    Text="Insert">
                </asp:LinkButton>
                <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
            </InsertItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqSize" TypeName="rfqs">
        <SelectParameters>
        <asp:Parameter Name="prmAction" DefaultValue="Select" Type="string" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqsize" Type="Int32" />
            <asp:SessionParameter Name="PrmPartNo" SessionField="rfqcustpart" Type="String" />
            <asp:Parameter Name="prmName" Type="string" />
            <asp:Parameter Name="prmStyle" Type="string" />
            <asp:Parameter Name="prmTab" Type="string" />                        
            <asp:Parameter Name="prmLen" Type="decimal" />
            <asp:Parameter Name="prmWid" Type="decimal" />
            <asp:Parameter Name="prmDep" Type="decimal" />
            <asp:Parameter Name="prmDust" Type="decimal" />
            <asp:Parameter Name="prmPanel" Type="decimal" />
            <asp:Parameter Name="prmTuck" Type="decimal" />
            <asp:Parameter Name="prmAdhesive" Type="string" />
            <asp:Parameter Name="prmKWid" Type="decimal" />
            <asp:Parameter Name="prmKLen" Type="decimal" />
            <asp:Parameter Name="prmLock" Type="decimal" />
            <asp:Parameter Name="prmGluela" Type="decimal" />
            <asp:Parameter Name="prmLin" Type="decimal" />
            <asp:Parameter Name="prmTWid" Type="decimal" />
            <asp:Parameter Name="prmTLen" Type="decimal" />
            <asp:Parameter Name="prmdscr" Type="string" />                        
             <asp:Parameter Name="prmSqin" Type="decimal" />
            
        </SelectParameters>
    </asp:ObjectDataSource>
<asp:FormView ID="FormView3" runat="server" DataSourceID="ObjectDataSource1">
        <ItemTemplate>
        <fieldset style="background-color:#EFF3FB;">
        <table style="width: 914px">
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Cust Part#:</b></td>
        <td><b><asp:Label ID="Label1" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("part_no") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="Label2" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("i_name") %>'></asp:Label></b></td>
        <td><b></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Style Code:</b></td>
        <td><b><asp:Label ID="Label3" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("style") %>'></asp:Label></b></td>
           <td><b><asp:Label ID="vStyleDscrLabel" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vStyleDscr") %>'></asp:Label>
        </b></td>
        <td><b></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Tab In:</b></td>
        
        <td><b><asp:Label ID="tab_inCheckBox" runat="server" Text='<%# Bind("tab_in") %>' BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Length:</b></td>
        <td><b><asp:Label ID="Label4" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("len","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Width:</b></td>
        <td><b><asp:Label ID="Label5" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("wid","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Depth:</b></td>
        <td><b><asp:Label ID="Label6" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("dep","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Bottom/Dust Flap:</b></td>
        <td><b><asp:Label ID="Label7" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("dust","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>5th Panel:</b></td>
        <td><b><asp:Label ID="Label8" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("fpanel","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Top Flap or Tuck:</b></td>
        <td><b><asp:Label ID="Label9" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("tuck","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Adhesive:</b></td>
        <td><b><asp:Label ID="Label10" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("adhesive") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Score/DK Width:</b></td>
        <td><b><asp:Label ID="Label11" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("k_wid","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Score/DK Length:</b></td>
        <td><b><asp:Label ID="Label12" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("k_len","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Lock Tab:</b></td>
        <td><b><asp:Label ID="Label13" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("lock","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Glue Lap:</b></td>
        <td><b><asp:Label ID="Label14" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("gluelap","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Lin Inches:</b></td>
        <td><b><asp:Label ID="Label15" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("lin_in","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 69px; padding-right:5px;"><b>Blank Width:</b></td>
        <td><b><asp:Label ID="Label16" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("t_wid","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Blank Length:</b></td>
        <td><b><asp:Label ID="Label17" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("t_len","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px;"><b>Blank Sq. In.:</b></td>
        <td><b><asp:Label ID="lv_sqinLabel" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("lv_sqin","{0:###,###,##0.00###}") %>'></asp:Label></b></td>
        </tr>
        </table>
        </fieldset>
         <table><tr>
        <td width="100px"></td>
        
        <%--<td align="center"><asp:Button ID="EditButton" CausesValidation="false" Text="Override" CssClass="buttonM" runat="server" CommandName="Edit" /></td>--%>
        </tr></table>   
                 
           
            <%--tab_in:
            <asp:CheckBox ID="tab_inCheckBox" runat="server" Checked='<%# Bind("tab_in") %>'
                Enabled="false" /><br />--%>
           
           
        </ItemTemplate>
        </asp:FormView>
</div>
</asp:Content>
