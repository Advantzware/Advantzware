<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="price_level" Codebehind="item_price_level.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Price</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <script language =javascript>
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }
   
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
        
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Price Level &nbsp;</b></font></TD>
          <TD nowrap>
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" ></asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;           
            
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" ></asp:Label>
            &nbsp;&nbsp; <b></TD>          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='400px' border="0">
        
        <tr><td align="center" nowrap>
           <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1">
              <EditItemTemplate>
                    <tabel class="shade"> <tr><td align="right" style="padding-right:5px">
                <b>What Level should the Items be Repriced At?:</b></td>
                <td>
                  <asp:TextBox ID="LevelTextBox" runat="server" Width="70px" Text='<%# Bind("Level") %>' />
                  <asp:RangeValidator ID="RangeValidator1" ControlToValidate="LevelTextBox" Display="Dynamic" Type="Integer" MinimumValue="1" MaximumValue="10" runat="server" ErrorMessage="Level must be Between 1 and 10.  Please ReEnter"></asp:RangeValidator>
                  </td></tr>
                  <tr><td>
                  <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="save_Click"  Text="Save" />
                  <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"  CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                  </td></tr>
                  </tabel>
              </EditItemTemplate>
              
              <ItemTemplate>
                <tabel class="shade"> <tr><td align="right" style="padding-right:5px">
                <b>What Level should the Items be Repriced At?:</b></td>
                <td>
                <asp:Label ID="LevelLabel" Width="80px" BackColor="Turquoise" runat="server" Text='<%# Bind("Level") %>' /> </td></tr>
                <tr><td>
                <asp:Button ID="update_Button" runat="server" CssClass="button" Text="Update" CommandName="edit" />
                <input type="button" value="Close" class="button" onclick="window.close()" />
                </td></tr>
                
                </tabel>
                  
                                   
              </ItemTemplate>
          </asp:FormView>
            
        </td></tr>
       
      </TABLE>
          
      
      <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
              OldValuesParameterFormatString="original_{0}" 
              SelectMethod="SelectItemPriceLevel" TypeName="orderentry">
                <SelectParameters>
                    <asp:Parameter DefaultValue="Select"  Name="prmAction" Type="String" />
                    <asp:Parameter Name="prmUser" Type="String" />
                    <asp:SessionParameter SessionField="order_est" Name="prmOrder" Type="Int32" />
                    <asp:Parameter Name="prmLevel" Type="Int32" />
                    <asp:Parameter Name="prmComp" Type="String" />
                    <asp:SessionParameter SessionField="view_line_est" Name="prmLine" Type="Int32" />
                </SelectParameters>
            </asp:ObjectDataSource>      
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

