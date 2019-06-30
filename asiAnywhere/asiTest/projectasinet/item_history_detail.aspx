<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="History_detail" Codebehind="item_history_detail.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>History</title>
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
          <TD align=center nowrap><font size=+0><b>History&nbsp;</b></font></TD>
          <TD nowrap>
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" ></asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;           
            
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" ></asp:Label>
            &nbsp;&nbsp; <b>Item#</b>&nbsp; <asp:Label ID="item_label" runat="server" BackColor="turquoise"  ></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='400px' border="0">
        <TR>
          <TD>
            
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
       
        <tr><td>
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" Width="380px" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True"  AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:TemplateField HeaderText="Sales Date" SortExpression="hisDate">
                            <ItemTemplate>
                                <asp:Label ID="date_Label" runat="server" Text='<%# Bind("hisDate","{0:MM/dd/yyyy}") %>'></asp:Label>                                
                            </ItemTemplate>                            
                        </asp:TemplateField>
                    <asp:BoundField DataField="hisCost" HeaderText="Cost" SortExpression="hisCost" />
                     <asp:BoundField DataField="hisQuantity" HeaderText="Quantity" SortExpression="hisQuantity" />
                    <asp:BoundField DataField="hisSell" HeaderText="Sell" SortExpression="hisSell" />
                    <asp:BoundField DataField="hisUom" HeaderText="Uom" SortExpression="hisUom" />                   
                    <asp:BoundField DataField="hisProfit" HeaderText="Prof%" SortExpression="hisProfit" />
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectHistoryDetail" TypeName="orderentry">
                <SelectParameters>
                    <asp:Parameter DefaultValue="Detail" Name="prmAction" Type="String" />
                    <asp:Parameter Name="prmUser" Type="String" />
                    <asp:SessionParameter SessionField="history_list_item_detail" Name="prmItem" Type="String" />
                    <asp:SessionParameter SessionField="history_list_cust_detail" Name="prmCust" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
          
        </td></tr>
      </TABLE>      
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

