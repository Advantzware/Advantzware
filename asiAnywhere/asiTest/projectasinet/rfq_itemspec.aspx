<%@ Page Language="C#" MasterPageFile="MasterPage5.master" AutoEventWireup="true" Inherits="rfqitemspec" Title="Rfq ItemSpec" Codebehind="rfq_itemspec.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="server">
    <script language="javascript" >
function cadlook(){ 
  var NewWindow = window.open("Cad_Lookup.aspx","CADCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CadLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_cad_noTextBox.value = ReturnObj1;
    
}

function palletlook(){ 
  var NewWindow = window.open("itempallet_lookup.aspx","PalletLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function itempalletLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_plate_noTextBox.value = ReturnObj1;
  
  
}
function spclook(){ 
  var NewWindow = window.open("spc_Lookup.aspx","CADCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function SpcLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_die_noTextBox.value = ReturnObj1;
    
}

function upclook(){ 
  var NewWindow = window.open("Upc_lookup.aspx","PalletLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function UPCLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_upc_noTextBox.value = ReturnObj1;
  
  
}






</script>
    <div>
<fieldset style="background-color:#EFF3FB;width:650px;">
<legend>Reference Information</legend>
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3">
       
        <ItemTemplate>
        
           <b>RFQ#:</b>
            <asp:Label ID="aRfqNoLabel" runat="server" BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aRfqNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Customer#:</b>
            <asp:Label ID="aCustNoLabel" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aCustNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Requested Date:</b>
            <asp:Label ID="vRfqDtLabel" runat="server" BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqDt","{0:MM/dd/yyyy}") %>'></asp:Label>
        </ItemTemplate>
    </asp:FormView>
    
</fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqItemDscr" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="rfqspecno" Type="Int32" />
            <asp:SessionParameter SessionField="list_rfq_cust_part_no" Name="prmPartNo" Type="string" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
    
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" Width="670px">
        <ItemTemplate>
        <asp:Panel ID="Item_Panel" runat="server" DefaultButton="EditButton">
        <fieldset style="background-color:#EFF3FB; width: 650px;">
        <table style="width: 650px">
        <tr>
        <td align="right" style="width: 100px; padding-right:5px;"><b>Cust Part#:</b></td>
        <td><b><asp:Label ID="Label1" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("part_no") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Estimate#:</b></td>
        <td><b><asp:Label ID="Label2" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("est_no") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 100px; padding-right:5px;"><b>Item Name:</b></td>
        <td><b><asp:Label ID="Label3" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("i_name") %>'></asp:Label></b></td>
        <td  align="right" style="padding-right:5px;"><b>FG Item#:</b></td>
        <td><b><asp:Label ID="stock_noLabel" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("stock_no") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td nowrap align="right" style="width: 100px; padding-right:5px;"><b>Part Description:</b></td>
        <td><b><asp:Label ID="Label4" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("part_dscr1") %>'>
            </asp:Label></b></td>
        <td  align="right" style="padding-right:5px;"><b>SPC/QC Code:</b></td>
        <td><b><asp:Label ID="Label11" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("spc_no") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 100px; padding-right:5px;"><b></b></td>
        <td><b><asp:Label ID="Label5" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("part_dscr2") %>'>
            </asp:Label></b></td>
        <td  align="right" style="padding-right:5px;"><b>Plate#:</b></td>
        <td><b><asp:Label ID="Label6" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("plate_no") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 100px; padding-right:5px;"><b></b></td>
        <td><b><asp:Label ID="Label7" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("part_dscr3") %>'>
            </asp:Label></b></td>
        <td  align="right" style="padding-right:5px;"><b>Die#</b></td>
        <td><b><asp:Label ID="Label8" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("die_no") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 100px; padding-right:5px;"><b></b></td>
        <td><b></b></td>
        <td  align="right" style="padding-right:5px;"><b>CAD/Sample#:</b></td>
        <td><b><asp:Label ID="Label9" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("cad_no") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 100px; padding-right:5px;"><b></b></td>
        <td><b></b></td>
        <td  align="right" style="padding-right:5px;"><b>UPC#:</b></td>
        <td><b><asp:Label ID="Label10" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("upc_no") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 100px; padding-right:5px;"><b>Category:</b></td>
        <td><b><asp:Label ID="procatLabel" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("procat") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="ProcatDscrLabel" runat="server" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("ProcatDscr") %>'>
            </asp:Label></b></td>
                
        
        </tr>
        
        </table>
        </fieldset>
        <table>
        <tr> <td>
         
                <asp:Button ID="EditButton" runat="server" CausesValidation="False" CommandName="Edit" CssClass="buttonM"  
                    Text="Update">
                </asp:Button>
             </td></tr>  </table>     </asp:Panel>                     
        </ItemTemplate>
        <EditItemTemplate>
        <asp:Panel ID="Edit_Panel" runat="server" DefaultButton="Button1">
        <table class="shade" width="650px"> <tr><td align="right" style="width: 100px; padding-right:5px;"> <b>Cust Part#:</b></td>
            <td><asp:Label ID="part_noTextBox" runat="server" Text='<%# Bind("part_no") %>'>
            </asp:Label></td>
              <td align="right" style="width: 100px; padding-right:5px;"><b>Estimate#:</b></td>
            <td><asp:Label ID="est_noTextBox" runat="server" Text='<%# Bind("est_no") %>'>
            </asp:Label></td> </tr> 
            <tr><td align="right" style="width: 100px; padding-right:5px;"><b>Item Name:</b></td>
            <td><asp:TextBox ID="i_nameTextBox" Enabled="false" runat="server" Text='<%# Bind("i_name") %>'>
            </asp:TextBox></td>
            <td align="right" style="width: 100px; padding-right:5px;"><b>FG Item#:</b></td>
            <td><asp:TextBox ID="stock_noTextBox" Enabled="false" runat="server" Text='<%# Bind("stock_no") %>'>
            </asp:TextBox></td></tr>
            <tr><td align="right" style="width: 100px; padding-right:5px;"><b>Part Description:</b></td>
            <td><asp:TextBox ID="part_dscr1TextBox" Enabled="false" runat="server" Text='<%# Bind("part_dscr1") %>'>
            </asp:TextBox></td>
            <td align="right" style="width: 100px; padding-right:5px;"> <b>SPC/QC Code:</b></td>
            <td><asp:TextBox ID="spc_noTextBox" Enabled="false" runat="server" Text='<%# Bind("spc_no") %>'>
            </asp:TextBox>
            </td></tr>
            <tr><td></td><td>
            <asp:TextBox ID="part_dscr2TextBox" Enabled="false" runat="server" Text='<%# Bind("part_dscr2") %>'>
            </asp:TextBox></td>
            <td align="right" style="width: 100px; padding-right:5px;"> <b> Plate#:</b></td>
            <td><asp:TextBox ID="plate_noTextBox" Enabled="false" runat="server" Text='<%# Bind("plate_no") %>'>
            </asp:TextBox>
            </td></tr>
            <tr><td></td><td>
            <asp:TextBox ID="part_dscr3TextBox" Enabled="false" runat="server" Text='<%# Bind("part_dscr3") %>'>
            </asp:TextBox></td>
            <td align="right" style="width: 100px; padding-right:5px;"><b>Die#:</b></td>
            <td><asp:TextBox ID="die_noTextBox" Enabled="false" runat="server" Text='<%# Bind("die_no") %>'>
            </asp:TextBox>
            </td></tr>
            <tr><td colspan="2"></td>
            <td align="right" style="width: 100px; padding-right:5px;"><b>CAD/Sample#:</b></td>
            <td><asp:TextBox ID="cad_noTextBox" Enabled="false" runat="server" Text='<%# Bind("cad_no") %>'>
            </asp:TextBox>
            </td></tr>
            <tr><td colspan="2"></td>
            <td align="right" style="width: 100px; padding-right:5px;"> <b>UPS#:</b></td>
            <td><asp:TextBox ID="upc_noTextBox" Enabled="false" runat="server" Text='<%# Bind("upc_no") %>'>
            </asp:TextBox>
            </td></tr>
            <tr><td align="right" style="width: 100px; padding-right:5px;"><b>Category:</b></td>
            <td><asp:Label ID="procatTextBox" Font-Bold="true" runat="server" Text='<%# Bind("procat") %>'>
            </asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Label ID="ProcatDscrTextBox" Font-Bold="true" runat="server" Text='<%# Bind("ProcatDscr") %>'>
            </asp:Label></td></tr>
            
            <tr>
           
           <td  colspan="2"><asp:Button ID="Button1" runat="server" CssClass="button" OnClick="UpdateButton_click"
                Text="Save" Font-Bold="true">
            </asp:Button>
           <asp:Button ID="UpdateCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel" Font-Bold="true"></asp:Button></td></tr></table>
          </asp:Panel>
        </EditItemTemplate>
        <InsertItemTemplate>       
            est_no:
            <asp:TextBox ID="est_noTextBox" runat="server" Text='<%# Bind("est_no") %>'>
            </asp:TextBox><br />
            part_no:
            <asp:TextBox ID="part_noTextBox" runat="server" Text='<%# Bind("part_no") %>'>
            </asp:TextBox><br />
            i_name:
            <asp:TextBox ID="i_nameTextBox" runat="server" Text='<%# Bind("i_name") %>'>
            </asp:TextBox><br />
            part_dscr1:
            <asp:TextBox ID="part_dscr1TextBox" runat="server" Text='<%# Bind("part_dscr1") %>'>
            </asp:TextBox><br />
            part_dscr2:
            <asp:TextBox ID="part_dscr2TextBox" runat="server" Text='<%# Bind("part_dscr2") %>'>
            </asp:TextBox><br />
            part_dscr3:
            <asp:TextBox ID="part_dscr3TextBox" runat="server" Text='<%# Bind("part_dscr3") %>'>
            </asp:TextBox><br />
            procat:
            <asp:TextBox ID="procatTextBox" runat="server" Text='<%# Bind("procat") %>'>
            </asp:TextBox><br />
            stock_no:
            <asp:TextBox ID="stock_noTextBox" runat="server" Text='<%# Bind("stock_no") %>'>
            </asp:TextBox><br />
            plate_no:
            <asp:TextBox ID="plate_noTextBox" runat="server" Text='<%# Bind("plate_no") %>'>
            </asp:TextBox><br />
            die_no:
            <asp:TextBox ID="die_noTextBox" runat="server" Text='<%# Bind("die_no") %>'>
            </asp:TextBox><br />
            cad_no:
            <asp:TextBox ID="cad_noTextBox" runat="server" Text='<%# Bind("cad_no") %>'>
            </asp:TextBox><br />
            upc_no:
            <asp:TextBox ID="upc_noTextBox" runat="server" Text='<%# Bind("upc_no") %>'>
            </asp:TextBox><br />
            spc_no:
            <asp:TextBox ID="spc_noTextBox" runat="server" Text='<%# Bind("spc_no") %>'>
            </asp:TextBox><br />
            ProcatDscr:
            <asp:TextBox ID="ProcatDscrTextBox" runat="server" Text='<%# Bind("ProcatDscr") %>'>
            </asp:TextBox>
            <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" CommandName="Insert"
                Text="Insert">
            </asp:Button>
            <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button>
        </InsertItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqItemSpec" TypeName="rfqs">
        <SelectParameters>
        <asp:Parameter Name="prmAction" DefaultValue ="Select" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="rfqspecno"
                Type="Int32" />
               

            <asp:SessionParameter Name="PrmPartNo" SessionField="rfqspecpart" Type="String" />
            <asp:Parameter Name="prmItemName" Type="String" />
            <asp:Parameter Name="prmDscr" Type="String" />
            <asp:Parameter Name="prmDscr2" Type="String" />
            <asp:Parameter Name="prmDscr3" Type="String" />
            <asp:Parameter Name="prmEstimate" Type="String" />
            <asp:Parameter Name="prmStock" Type="String" />
            <asp:Parameter Name="prmPlate" Type="String" />
            <asp:Parameter Name="prmDie" Type="String" />
            <asp:Parameter Name="prmSample" Type="String" />
            <asp:Parameter Name="prmUpc" Type="String" />
            <asp:Parameter Name="prmSpc" Type="String" />
            <asp:Parameter Name="prmCat" Type="String" />
            <asp:Parameter Name="prmCatdscr" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</asp:Content>