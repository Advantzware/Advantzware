<%@ Page Language="C#" AutoEventWireup="true" Inherits="jobRep_lookup" Title ="Job LookUp" Codebehind="jobRep_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Job Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <div>
    <asp:Panel ID="searchpanel" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <asp:ListItem Value="job-no">JOB NO#</asp:ListItem>  
                      
                      
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                                 
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                 
 </td>
  </tr>
  </table></asp:Panel>
  </div>
    
    <div>
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
		    <a href="#" onClick="javascript:top.opener.window.JobRepLookup('<%#DataBinder.Eval(Container,"DataItem.JOB")%>','<%#DataBinder.Eval(Container,"DataItem.JOB2")%>','<%#DataBinder.Eval(Container,"DataItem.item-no")%>','<%#DataBinder.Eval(Container,"DataItem.itemname")%>','<%#DataBinder.Eval(Container,"DataItem.stdcost")%>','<%#DataBinder.Eval(Container,"DataItem.costuom")%>','<%#DataBinder.Eval(Container,"DataItem.loc-no")%>','<%#DataBinder.Eval(Container,"DataItem.loc-bin")%>','<%#DataBinder.Eval(Container,"DataItem.qty-case")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="JOB" HeaderText="Job  Number" SortExpression="JOB" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="JOB2" HeaderText="" SortExpression="JOB2" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="ESTIMATE" HeaderText="Estimate#" SortExpression="ESTIMATE" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="item-no" HeaderText="Item FG#" SortExpression="item-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                 <asp:BoundField DataField="cust-no" HeaderText="Customer#" SortExpression="cust-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                 <asp:BoundField DataField="ord-no" HeaderText="Order#" SortExpression="ord-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:CheckBoxField DataField="TICKET" HeaderText="Factory Ticket Printed" SortExpression="TICKET" >
                    <ItemStyle Wrap="False" />
                </asp:CheckBoxField>
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectJob1Look" TypeName="Order">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>
