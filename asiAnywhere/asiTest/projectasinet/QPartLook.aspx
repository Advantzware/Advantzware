<%@ Page Language="C#" AutoEventWireup="true" Inherits="QPartLook" Title="Part LookUp" Codebehind="QPartLook.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Category LookUp</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" DefaultButton="Button1">
    
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <asp:ListItem Value="ANY">ANY</asp:ListItem>
                      <asp:ListItem Value="est-no">Est#</asp:ListItem>
                      <asp:ListItem Value="part-no">Part#</asp:ListItem>  
                      
                      
                      
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
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
    
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
		    <a href="#" onClick="javascript:top.opener.window.QPartLookup('<%#DataBinder.Eval(Container,"DataItem.QPartNo")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="QEst" HeaderText="Est#" SortExpression="QEst" />
                <asp:BoundField DataField="QDscr" HeaderText="Description" SortExpression="QDscr" />
                <asp:BoundField DataField="QCust" HeaderText="Cust#" SortExpression="QCust" />
                <asp:BoundField DataField="QPartNo" HeaderText="Part#" SortExpression="QPartNo" />
                
                
                
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="QPartLook" TypeName="LookUp">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
                    
                <asp:Parameter DefaultValue="" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                
                 
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
