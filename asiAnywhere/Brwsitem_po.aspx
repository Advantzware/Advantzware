<%@ Page Language="C#" MasterPageFile="MasterPagepo.master" Debug="true" AutoEventWireup="true" Inherits="Brwsitem_po" Title="Purchasing Order" Codebehind="Brwsitem_po.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1"  runat="server">
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
<script type="text/javascript" language="javascript">

    window.onload = setfocus;
    function setfocus() {
        document.forms[0].ctl00_ContentPlaceHolder1_txt_po.focus();
    }
    

    function fgitemall() {
        var NewWindow = window.open("fgitemall_lookup.aspx", "fgitemLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function FGallLookup(ReturnObj1) {
        document.forms[0].ctl00_ContentPlaceHolder1_txt_ino.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_txt_ino.focus();
    }




function clickButton(e, ctl00$ContentPlaceHolder1$btnSearch){ 

      var evt = e ? e : window.event;

      var bt = document.getElementById(ctl00$ContentPlaceHolder1$btnSearch);

      if (bt){ 

          if (evt.keyCode == 13){ 

                bt.click(); 

                return false; 

          } 

      } 

}


</script>
<div >
<TABLE id="tblSearch" cellSpacing="1" cellPadding="5" border="0" width="810px"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                    <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />&nbsp;</td>
               <td nowrap="nowrap">
                   Po# <br>           
			<asp:TextBox ID="txt_po" runat="server" MaxLength="8" Width="100px" ></asp:TextBox>
			<asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="txt_po" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
			
          	   </td>
          	   
                	
           		     
            		<td nowrap>RM/FG Item# <br>
            <asp:TextBox ID="txt_ino" runat="server" MaxLength="15" Width="120px" ></asp:TextBox>
            <a href="#" tabindex="1" onClick="fgitemall(); return false"><asp:Image ID="fgitemlookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
			                	</td>         
            		        
               		
                  <td nowrap>Rows/Page<br>
                  
                      <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
                          <ItemTemplate>
                             
                              <asp:TextBox ID="aLineLabel" runat="server" MaxLength="4" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                              <%--<asp:Label ID="aLineLabel" runat="server" Text='<%# Bind("aLine") %>'></asp:Label>--%>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
                  
                </td>
			<td></td>
			<td></td>
            
                  </tr>
                  </table>
               </TD></TR>  
       </table>
</div>
<div> <br />
<fieldset  style="height:30px;width:810px" class="shade">
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
   
    <asp:GridView ID="GridView1" runat="server" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" EmptyDataText="No Record Found" OnPageIndexChanging="GridView1_PageIndexChanging">
        <Columns>
            <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText=""
                    ShowSelectButton="True">
                    <ItemStyle Width="10px" />
                </asp:CommandField>
           
            <asp:BoundField DataField="poItemNo" ItemStyle-Width="120px" HeaderText="RM/FG Item#" 
                SortExpression="poItemNo" />
            <asp:BoundField DataField="poItemName" ItemStyle-Width="140px" ItemStyle-Wrap="false" HeaderText="RM/FG Item Name" 
                SortExpression="poItemName" />
            <asp:BoundField DataField="poJobNo" ItemStyle-Wrap="false" HeaderText="Job Number#" 
                SortExpression="poJobNo" />
            <asp:BoundField DataField="poJobNo2" HeaderText="" ItemStyle-Wrap="false"
                SortExpression="poJobNo2" />
            <asp:BoundField DataField="poSNum" HeaderText="Form#" 
                SortExpression="poSNum" />
            <asp:BoundField DataField="poOrdQty" ItemStyle-Width="70px" HeaderText="Quantity" 
                SortExpression="poOrdQty" />
            <asp:BoundField DataField="poCost" ItemStyle-Width="70px" HeaderText="Unit Cost" 
                SortExpression="poCost" />
            <asp:BoundField DataField="poCustNo" ItemStyle-Width="100px" HeaderText="Customer#" 
                SortExpression="poCustNo" />
            <asp:BoundField DataField="poDueDate" ItemStyle-Width="100px" HeaderText="Due Date" 
                SortExpression="poDueDate" />
            <asp:BoundField DataField="poItemType" HeaderText="Item Type" 
                SortExpression="poItemType" />
            
               <asp:TemplateField HeaderText="Date" Visible="false" SortExpression="vEstDate">                    
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("[poline]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>  
                 <asp:TemplateField HeaderText="reckey" Visible="false" SortExpression="poRecKey">                    
                    <ItemTemplate>
                        <asp:Label ID="LabelpoRecKey" runat="server" Text='<%# Bind("[poRecKey]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>  
          
           </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" Width="100%" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" />
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <RowStyle CssClass="shade" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectListItemPO" TypeName="browspo">
        <SelectParameters>            
            <asp:Parameter Name="prmUser"  Type="String" />            
            <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />
            <asp:SessionParameter Name="prmpoNo" SessionField="pur_ord_po" DefaultValue=""  
                Type="Int32"  />
            <asp:Parameter Name="prmpoItemNo" Type="String" />
            <asp:Parameter Name="prmpoRecKey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>

</asp:Content>
