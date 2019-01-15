<%@ Page Language="C#" MasterPageFile="MasterPagepo.master" Debug="true" AutoEventWireup="true" Inherits="Brwslist_po" Title="Purchase Order" Codebehind="Brwslist_po.aspx.cs" %>

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

    function setestdate() {

        var da = document.getElementById("ctl00_ContentPlaceHolder1_txt_due");
        da.focus();

    }    
   

function fgitemall() {
    var NewWindow = window.open("fgitemall_lookup.aspx", "fgitemLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGallLookup(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_txt_ino.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_txt_ino.focus();
}


function customerpolook() {

    var NewWindow = window.open("poitem_lookup.aspx", "CustomerpoWindow", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ItemPoLookup(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_txt_po.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_txt_po.focus();
}


function vendorlook() {

    var NewWindow = window.open("corvend_lookup.aspx", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function VendLookup(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_txt_ven.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_txt_ven.focus();
}

function vendoritemlook() {

    var NewWindow = window.open("venditemlook.aspx", "vendoritemlook", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function VendItemLook(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_txt_venino.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_txt_venino.focus();
}




function job1look() {
    var jobno = document.getElementById("ctl00_ContentPlaceHolder1_txt_job").value;
    var NewWindow = window.open("job1_lookup.aspx?job="+ jobno +"", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_job.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_job.focus();
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
<TABLE id="tblSearch" cellSpacing="1" cellPadding="5" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                    <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />&nbsp;</td>
               <td nowrap="nowrap">
                   Po# <br>           
			<asp:TextBox ID="txt_po" runat="server" MaxLength="8" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="pofglLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
			<asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="txt_po" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
			
          	   </td>
          	   <td nowrap id="customerid" runat="server">Vendor#
                        <br>
            <asp:TextBox ID="txt_ven"  runat="server" MaxLength="8" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onclick="vendorlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	
           		     
            		<td nowrap>RM/FG Item# <br>
            <asp:TextBox ID="txt_ino" runat="server" MaxLength="15" Width="95px" ></asp:TextBox>
            <a href="#" tabindex="1" onClick="fgitemall(); return false"><asp:Image ID="fgitemlookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
			                	</td>         
            		<td nowrap> Vendor Item# <br>
            <asp:TextBox ID="txt_venino" runat="server" MaxLength="15" Width="95px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="vendoritemlook(); return false"><asp:Image ID="venditemlook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td> 
                	
                	<td nowrap> Due Date# <br>
            <asp:TextBox ID="txt_due" runat="server" Width="65px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ></asp:TextBox>
			<a href="#" onblur="setestdate()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_txt_due); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
                	</td> 
                	<td nowrap> Job# <br>
            <asp:TextBox ID="txt_job" runat="server" MaxLength="6" Width="55px" ></asp:TextBox>            
			<a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
			<asp:TextBox ID="txt_job2" runat="server" MaxLength="2" Width="20px" ></asp:TextBox>
                	</td>
                	<td>
               <asp:CheckBox ID="CheckBox1" Text="Open" runat="server" />
               <br />
               <asp:CheckBox ID="CheckBox2" Text="Close" runat="server" />
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
<div>
    <br />
        
    <asp:GridView ID="GridView2" runat="server" OnRowCreated="GridView2_RowCreated" AllowPaging="true" OnPageIndexChanging="GridView2_PageIndexChanging" 
            AllowSorting="True" OnSorting="GridView2_Sorting" OnSelectedIndexChanged="GridView2_SelectedIndexChanged" OnRowDataBound="GridView2_RowDataBound"
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid" >
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" Wrap="false" />
            <AlternatingRowStyle CssClass="GridItemOdd" Wrap="False" /> 
            
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle   ForeColor="White" CssClass="headcolor" Height="40px" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="false"></HeaderStyle>
        <RowStyle CssClass="shade"  Wrap="False"  />
        <Columns>
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
            </asp:CommandField>
                <asp:TemplateField HeaderText="Select" Visible="false" >
               <ItemStyle HorizontalAlign="Center" />
               <ItemTemplate>
                  <asp:Label ID="Label4" runat="server" Text='<%# Bind("[Po#]") %>'></asp:Label>
                  <asp:Label ID="Label3" runat="server" Text='<%# Bind("[poline]") %>'></asp:Label>
                   <asp:Label ID="reckeyvalue" runat="server" Text='<%# Bind("RecKey2") %>'></asp:Label>
                   <asp:Label ID="lbl_rec_key" runat="server" Text='<%# Bind("RecKey") %>'></asp:Label>
               </ItemTemplate>
                <ItemStyle Wrap="False" HorizontalAlign="right" />
                              
               </asp:TemplateField>
                
                <asp:TemplateField HeaderText="vitemno" Visible="false" >                    
                    <ItemTemplate>
                     <asp:Label ID="item_label" runat="server" Text='<%# Bind("[Item]") %>'></asp:Label>
                     </ItemTemplate>
                     <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
        </Columns>
            </asp:GridView>
   
</div>

</asp:Content>
