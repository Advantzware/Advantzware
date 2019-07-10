<%@ Page Language="C#" AutoEventWireup="true" Inherits="CustPoOrd_Lookup" Title="CustPo LookUp" Codebehind="CustPoOrd_Lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>PO Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">                    
                      <asp:ListItem Value="pono">PO#</asp:ListItem>                        
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <%--<asp:ListItem Value="More than ...">More than ...</asp:ListItem>
                    <asp:ListItem Value="Less than ...">Less than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or more than ...">Equal or more than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or less than ...">Equal or less than ...</asp:ListItem>
                    <asp:ListItem Value="IsNull">Empty</asp:ListItem> --%>                  
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
   </asp:Panel> 
    <div>
        &nbsp;&nbsp;
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.CustPOOrdLookup('<%#DataBinder.Eval(Container,"DataItem.pono")%>','<%#DataBinder.Eval(Container,"DataItem.ino")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="pono" HeaderText="P.O. Num" SortExpression="pono" />
                <asp:BoundField DataField="ino" HeaderText="Item No" SortExpression="ino" />
                <asp:BoundField DataField="qty" HeaderText="Qty" SortExpression="qty" />
                <asp:BoundField DataField="shpid" HeaderText="Ship To ID" SortExpression="shpid" />
                <asp:BoundField DataField="reldate" HeaderText="Date Last Counted" SortExpression="reldate" />
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="CustomerPOLookup" TypeName="release">
            <SelectParameters>
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />                                
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:QueryStringParameter QueryStringField="poord" Name="prmord" Type="Int32" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
