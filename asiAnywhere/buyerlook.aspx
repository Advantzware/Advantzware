<%@ Page Language="C#" AutoEventWireup="true" Inherits="buyerlook " Title="Buyer Information(Window)" Codebehind="buyerlook.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Buyer Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    
    <div>
      <asp:Panel ID="searchpanel" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="470px" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server"  width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button><br />
                  <asp:button id="Button2" runat="server"  width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" >&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                  <asp:ListItem Value="Buyer">Buyer</asp:ListItem>                      
                  <asp:ListItem Value="Buyer Name">Buyer Name</asp:ListItem>                      
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                     <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                   
                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
    
    <div>
        &nbsp;&nbsp;
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" AllowPaging="true"  AllowSorting="true" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1"  
            EmptyDataText="No Records Found" Width="470px" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
             <asp:TemplateField>
                  <ItemStyle HorizontalAlign="Center" Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick = "javascript:top.opener.window.buyerlook('<%#DataBinder.Eval(Container,"DataItem.vbuyer")%>','<%#DataBinder.Eval(Container,"DataItem.vBuyerN")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vbuyer" HeaderText="Buyer" ItemStyle-Wrap="false" SortExpression="vbuyer" />
                <asp:BoundField DataField="vBuyerN" HeaderText="Buyer Name" ItemStyle-Wrap="false" SortExpression="vBuyerN" />
                
                             
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="Selectbuyerlook" TypeName="browspo" OnSelecting="ObjectDataSource1_Selecting">
            <SelectParameters>            
                    
                <asp:Parameter DefaultValue="PoSelect" Name="prmAction"  Type="String" />                               
                <asp:Parameter Name="prmUser" Type="String" />                
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                            
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
