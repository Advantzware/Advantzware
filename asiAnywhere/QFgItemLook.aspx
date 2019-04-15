<%@ Page Language="C#" AutoEventWireup="true" Inherits="QFgItemLookup" Title="FgItem LookUp" Codebehind="QFgItemLook.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Category LookUp</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />

</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server">
    
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
    
    <div>
  <table id="tblSearch" cellspacing="1" cellpadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade" style="height: 33px">
  <asp:button id="Button1" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800" style="height: 33px">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server" AutoPostBack="true" OnSelectedIndexChanged="searchindexchanged">                                                     
                      <asp:ListItem Value="stock-no">Item#</asp:ListItem>
                      <asp:ListItem Value="est-no">Est#</asp:ListItem>                      
                      <asp:ListItem Value="part-no">Cust Part#</asp:ListItem>                                                                    
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">                    
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                                     
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
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
		    <a href="#" onClick="javascript:top.opener.window.QFgItemLookUp('<%#DataBinder.Eval(Container,"DataItem.QItem")%>','<%#DataBinder.Eval(Container,"DataItem.QItemDscr")%>','<%#DataBinder.Eval(Container,"DataItem.QPart")%>','<%#DataBinder.Eval(Container,"DataItem.style")%>','<%#DataBinder.Eval(Container,"DataItem.category")%>','<%#DataBinder.Eval(Container,"DataItem.qcolor")%>','<%#DataBinder.Eval(Container,"DataItem.qcoat")%>','<%#DataBinder.Eval(Container,"DataItem.len")%>','<%#DataBinder.Eval(Container,"DataItem.wid")%>','<%#DataBinder.Eval(Container,"DataItem.dep")%>','<%#DataBinder.Eval(Container,"DataItem.QEstNum")%>','<%#DataBinder.Eval(Container,"DataItem.board")%>','<%#DataBinder.Eval(Container,"DataItem.cal")%>','<%#DataBinder.Eval(Container,"DataItem.DieIn")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="QItem" HeaderText="Item#" SortExpression="QItem" />
                
                <asp:BoundField DataField="QItemDscr" HeaderText="Description" SortExpression="QItemDscr" />
                <asp:BoundField DataField="QCustNum" HeaderText="Cust#" SortExpression="QCustNum" />
                <asp:BoundField DataField="QPart" HeaderText="Cust Part#" SortExpression="QPart" />
                <asp:BoundField DataField="QEstNum" HeaderText="Est#" SortExpression="QEstNum" />
               <asp:BoundField DataField="board" HeaderText="Board" SortExpression="board" />
               <asp:BoundField DataField="cal" HeaderText="Caliper" SortExpression="cal" />
               
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="QFgItemLook" TypeName="LookUp">
            <SelectParameters>
            <asp:SessionParameter Name="prmUser" SessionField="prmUser" Type="String" />                    
                <asp:Parameter DefaultValue="" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />                              
                <asp:QueryStringParameter Name="prmCust" QueryStringField="rfqcust" Type="String" />  
                <asp:Parameter Name="prmType" Type="String" />                                                 
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
