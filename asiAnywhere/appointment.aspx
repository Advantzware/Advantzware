<%@ Page Language="C#" Debug="true" AutoEventWireup="true" Inherits="appointment" Codebehind="appointment.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">



<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Appointment</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >

    <script language = "JavaScript" src="include/CalendarControl.js">
    </script>
<script>
function display()
{
var lab=document.getElementById("DayReportLabel");
lab.style.backgroundColor='Yellow';

}

function check()
{
var val=document.forms[0].FormView1_time_1TextBox.value;
if(document.forms[0].FormView1_time_1TextBox.value.length>1 && document.forms[0].FormView1_time_1TextBox.value.length<3)
{
  document.forms[0].FormView1_time_1TextBox.value=val + ":";
  document.forms[0].FormView1_time_1TextBox.focus();
}
}

//function checklen()
//{
//  var val=document.forms[0].FormView1_time_1TextBox.value;
//  if(val.length<5)
//  {
//    alert("Invalid Time. The Format is HH:MM");
//    document.forms[0].FormView1_time_1TextBox.focus();
//   }
//  if(val.length<5)
//  {
//   alert("Invalid Time. The Format is HH:MM");
//  }
//}
</script>
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
</head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
    <div>
                   
      <table id="tblTop" cellspacing="3" align="center" border="0" width="100%">
        <tr>
          
          <td align="center"><font size="+0"><b>&nbsp; Calendar &nbsp;</b></font></td>
          <td><asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton></td>
          <td valign="middle" align="center"><b>Users</b>&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
             &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>
          </td>
          
          <td valign="middle" width="20">&nbsp;</td>
          
          <td width="30">&nbsp;</td>
        </tr>
      </table>
      
       <table>
    <tr bgcolor="gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected" >
        <asp:LinkButton ID="lnk_listcontact" OnClick="lnk_listcontact_click" runat="server">List Contacts</asp:LinkButton></li>
        <li><asp:LinkButton ID="lnk_viewcontact" runat="server" OnClick="lnk_viewcontacts_click">View Contacts</asp:LinkButton></li>
        <li><asp:LinkButton ID="lnk_notes" runat="server" OnClick="lnk_notes_click">Notes</asp:LinkButton></li>
        <li><asp:LinkButton ID="lnk_MailList" runat="server" OnClick="lnk_MailList_click">Mail Label</asp:LinkButton></li>
        <li class="selected"><asp:LinkButton ID="lnk_calendar" runat="server" OnClick="lnk_calendar_click">Calendar</asp:LinkButton></li></ul></div>
    </td>
    </tr>
    </table>
    
         
    
   <table class="shade" width="655px">
   <fieldset  style="background-color:#EFF3FG; width:650px;" >
   <tr>
   <td><b>
   Contact Name: &nbsp;&nbsp <asp:Label ID="custlabel" BackColor="Turquoise" runat="server"></asp:Label>
   </b></td>
   
   <td>
   <b>Company Name: &nbsp;&nbsp; <asp:Label ID="complabel" BackColor="Turquoise" runat="server"></asp:Label></b>
   </td>
   
   <td><asp:Button ID="DailyButton" runat="server" CssClass="buttonM" Text="Today" OnClick="DailyButton_Click" /></td>
   
   <td><asp:Button ID="WeekButton" runat="server" CssClass="buttonM" Text="Week" OnClick="WeekButton_Click" /></td>
   </tr>
   </fieldset>
   </table>
      <table class="shade">
      <tr>
      <td width="250px"></td>
      <td width="390px"><b><asp:Label ID="FromLabel" runat="server"></asp:Label>&nbsp;</b>
      <asp:Label ID="DayReportLabel" Width="140px" runat="server"  BorderColor="Black" Font-Bold="true"></asp:Label>
      &nbsp;&nbsp;
      <b><asp:Label ID="ToLabel" runat="server"></asp:Label>&nbsp;</b>
      <asp:Label ID="DayReportLabel2" Width="140px" runat="server" BorderColor="Black" Font-Bold="true"></asp:Label>
      </td>
      <td></td>
      </tr>
      </table>
   <table class="shade"><tr><td>
       
     <asp:Calendar ID="Calendar1" runat="server" BackColor="White" BorderColor="Black" BorderStyle="Solid" CellSpacing="1" Font-Names="Verdana" Font-Size="7pt" ForeColor="Black" Height="170px" NextPrevFormat="ShortMonth" Width="200px" OnSelectionChanged="Calendar1_SelectionChanged"  > 
            <SelectedDayStyle BackColor="#333399" ForeColor="White" />
            <TodayDayStyle BackColor="#999999" ForeColor="White" />
            <DayStyle BackColor="#CCCCCC" />
            <OtherMonthDayStyle ForeColor="#999999" />
            <NextPrevStyle Font-Bold="True" Font-Size="6pt" ForeColor="White" />
            <DayHeaderStyle Font-Bold="True" Font-Size="6pt" ForeColor="#333333" Height="6pt" BackColor="White" />
            <TitleStyle BackColor="DarkKhaki" BorderStyle="None" Font-Bold="True" Font-Size="10pt"
                ForeColor="White" Height="8pt" />
         <SelectorStyle BackColor="White" />
        </asp:Calendar>
    </td>
    
    <td width="40px"> 
            </td>
    <td>
    
    
      <b> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
       <asp:Label ID="datelabel" BackColor="Turquoise" runat="server"></asp:Label></b>  
        
    <%--<fieldset style="background-color:#EFF3FG">--%>
    
      <asp:GridView id="GridView1" runat="server" CssClass="Grid" Width="400px" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"                    
          AllowPaging="True" AllowSorting="True" BorderStyle="Solid" EmptyDataText="No Appointment found" AutoGenerateColumns="False" DataKeyNames="rec_key" GridLines="Both" ShowHeader="False" BorderColor="Black" >
        <SelectedRowStyle BackColor="Yellow" CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <HeaderStyle  BackColor="Teal" ForeColor="White" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            <Columns >
            <%--<asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>--%>
                    
                <asp:BoundField DataField="date_1"  HeaderText="Date" SortExpression="date_1" >
                  <ItemStyle Width="40px" />  
                </asp:BoundField>
                <asp:BoundField DataField="time_1" HeaderText="Time" SortExpression="time_1" >
                    <ItemStyle Height="60px" Width="45px" />
                </asp:BoundField>
                
                <asp:BoundField Visible="False" DataField="length_of_appointment" HeaderText="length_of_appointment"
                    SortExpression="length_of_appointment" />
                <asp:BoundField Visible="False" DataField="sales_rep_code" HeaderText="sales_rep_code" SortExpression="sales_rep_code" />
                <asp:BoundField Visible="False" DataField="sales_rep_name" HeaderText="sales_rep_name" SortExpression="sales_rep_name" />
                <asp:BoundField Visible="False" DataField="meeting_name" HeaderText="meeting_name" SortExpression="meeting_name" />
                <asp:BoundField DataField="meeting_description" HeaderText="Meeting Descriptin"
                    SortExpression="meeting_description" >
                    <ItemStyle HorizontalAlign="Center" />
                    </asp:BoundField>
                <asp:TemplateField Visible="False" HeaderText="rec_key" InsertVisible="False" SortExpression="rec_key">
                    <EditItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Eval("rec_key") %>'></asp:Label>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("rec_key") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                    
                    </Columns>
            </asp:GridView> 
    
    <asp:GridView id="dbGrid_status" runat="server" CssClass="Grid" Width="400px"                    
                  OnPageIndexChanging="dbGrid_industry_sic_PageIndexChanging" OnRowDataBound="dbGrid_status_RowDataBound"                                    

                  AllowPaging="True" AllowSorting="True" BorderStyle="Solid" EmptyDataText="No Appointment found" OnSelectedIndexChanged="dbGrid_status_SelectedIndexChanged" DataSourceID="SqlDataSource2" AutoGenerateColumns="False" DataKeyNames="rec_key" GridLines="Both" ShowHeader="False" BorderColor="Black" >
        <SelectedRowStyle BackColor="Yellow" CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <HeaderStyle  BackColor="Teal" ForeColor="White" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            <Columns >
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    
                <asp:BoundField DataField="date_1" Visible="False" HeaderText="Date" SortExpression="date_1" >
                    
                </asp:BoundField>
                <asp:BoundField DataField="time_1" HeaderText="Time" SortExpression="time_1" >
                    <ItemStyle Height="60px" Width="45px" />
                </asp:BoundField>
                <asp:BoundField  DataField="meeting_name" HeaderText="Meeting Name" SortExpression="meeting_name" >
                <ItemStyle Width= "90px" />
                    </asp:BoundField>
                <asp:BoundField  DataField="length_of_appointment" HeaderText="Length"
                    SortExpression="length_of_appointment" >
                    <ItemStyle Width= "20px" />
                    </asp:BoundField>
                <asp:BoundField Visible="False" DataField="sales_rep_code" HeaderText="sales_rep_code" SortExpression="sales_rep_code" />
                <asp:BoundField Visible="False" DataField="sales_rep_name" HeaderText="sales_rep_name" SortExpression="sales_rep_name" />                
                <asp:BoundField DataField="meeting_description" HeaderText="Meeting Descriptin"
                    SortExpression="meeting_description" >
                    <ItemStyle HorizontalAlign="Center" />
                    </asp:BoundField>
                <asp:TemplateField Visible="False" HeaderText="rec_key" InsertVisible="False" SortExpression="rec_key">
                    <EditItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Eval("rec_key") %>'></asp:Label>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("rec_key") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                    
                    </Columns>
            </asp:GridView>
       <asp:SqlDataSource ID="SqlDataSource2" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT * FROM [appointment] WHERE ([date_1] = @date_1 and [sales_rep_name]=@sales_rep_name)"
             InsertCommand="INSERT INTO appointment(date_1, time_1, length_of_appointment, sales_rep_code, sales_rep_name, meeting_name, meeting_description) VALUES (@date_1, @time_1, @length_of_appointment,@sales_rep_code,@sales_rep_name,@meeting_name,@meeting_description)" >
            <SelectParameters>
            <asp:SessionParameter SessionField="Current_date" Name="date_1" />
                <%--<asp:ControlParameter ControlID="Calendar1" Name="date_1" PropertyName="SelectedDate"
                    Type="DateTime" />--%>
                    <asp:SessionParameter SessionField="contact_list_last_name" Name="sales_rep_name" />
            </SelectParameters>
           <InsertParameters>
               <asp:Parameter Name="date_1" />
               <asp:Parameter Name="time_1" />
               <asp:Parameter Name="length_of_appointment" />
               <asp:Parameter Name="sales_rep_code" />
               <asp:Parameter Name="sales_rep_name" />
               <asp:Parameter Name="meeting_name" />
               <asp:Parameter Name="meeting_description" />
           </InsertParameters>
            
         
            </asp:SqlDataSource>
            <%--</fieldset>--%>
    <br />
    <table><tr><td>
    <table>
    <tr> 
        <td></td>
        <td></td>       
        <td>
   <asp:Button ID="AddNewButton" Width="60px" runat="server" Text="Add" OnClick="AddNewButton_Click" CssClass="buttonM" CommandName="New" />
   </td>
        </tr></table></td>
        <td>
        <asp:FormView ID="FormView1" OnDataBound="FormView_dataBound" runat="server" DataSourceID="SqlDataSource1" PreRender="FormView1_PreRender" PagerSettings-Mode="NextPreviousFirstLast" PagerSettings-PreviousPageText="Previous" PagerSettings-NextPageText="Next" PagerSettings-FirstPageText="First" PagerSettings-LastPageText="Last" OnItemUpdated="FormView1_ItemUpdated">
            <EditItemTemplate>
            <table bgcolor="#EFF3FG">
            <tr>
            <td><b>Date:</b></td>
            <td><b><asp:TextBox ID="date_1TextBox" runat="server" ReadOnly="true" Width="80px"  BackColor="turquoise"   Text='<%# Bind("date_1","{0:MM/dd/yyyy}") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>Time: (HH:MM)</b></td>
            <td><b><asp:TextBox ID="time_1TextBox" MaxLength="8" onkeyup="check()" runat="server" Width="80px" ToolTip="Use This Time Format 01:12 am/pm" Text='<%# Bind("time_1") %>'>
                 </asp:TextBox></b>
                 <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="time_1TextBox" ErrorMessage="Invalid Time.Format is HH:MM" Display="Dynamic" ValidationExpression="(^([0-1][0-9]|[2][0-3])(:([0-5][0-9]))(\s)([AaPp][Mm]){1,2}$)"></asp:RegularExpressionValidator>
                
                 </td>
            
             </tr>
            <td><b>Length of Appointment:</b></td>
            <td><b><asp:TextBox ID="length_of_appointmentTextBox" MaxLength="5" Width="80px" runat="server" Text='<%# Bind("length_of_appointment","{0:hh:mm}") %>'>
                </asp:TextBox></b>
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="length_of_appointmentTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="Double" ErrorMessage="Invalid Time"></asp:CompareValidator>
                <%--<asp:RegularExpressionValidator ID="RegularExpressionValidator2" runat="server" SetFocusOnError="true" ControlToValidate="length_of_appointmentTextBox" ErrorMessage="Invalid Time." Display="Dynamic" ValidationExpression="(^([0-1][0-9]|[2][0-3])(:([0-5][0-9])){1,2}$)"></asp:RegularExpressionValidator>--%>
                
                </td>
                </td>
            <tr>
           
            <tr>
            <td><b><%--Sales Rep Code:--%></b></td>
            <td><b><asp:TextBox ID="sales_rep_codeTextBox" Visible="false" runat="server" Text='<%# Bind("sales_rep_code") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><%--Sales Rep Name:--%></b></td>
            <td><b><asp:TextBox ID="sales_rep_nameTextBox" Visible="false" runat="server" Text='<%# Bind("sales_rep_name") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>Meeting Name:</b></td>
            <td><b><asp:TextBox ID="meeting_nameTextBox" runat="server" Text='<%# Bind("meeting_name") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>Meeting Description:</b></td>
            <td><b><asp:TextBox ID="meeting_descriptionTextBox" Height="80px" TextMode="MultiLine" Width="180px"  runat="server" Text='<%# Bind("meeting_description") %>'>
                </asp:TextBox></b></td>
            </tr>
             <tr>
            <td ><b></b></td>
            <td><b> <asp:TextBox ID="TextBox1" Visible="false" runat="server" Text='<%# Bind("rec_key") %>'>
                </asp:TextBox></b></td>
            </tr>
            </table>
            <br />
                <asp:Button ID="UpdateButton" runat="server" CssClass="buttonM" CausesValidation="True" OnClick="UpdateButton_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel" OnClick="CancelEdit_Click">
                </asp:Button>
            </EditItemTemplate>
            <InsertItemTemplate>
            <table bgcolor="#EFF3FG">
            <tr>
            <td><b>Date:</b></td>
            <td><b><asp:TextBox ID="date_1TextBox" Width="80px" onfocus="showCalendarControl(this);" runat="server" Text='<%# Bind("date_1") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>Time: (HH:MM am/pm)</b></td>
           
            <td><b><asp:TextBox ID="time_1TextBox" MaxLength="8" onkeyup="check()" Width="80px" ToolTip="Use This Time Format 01:12 am/pm" runat="server" Text='<%# Bind("time_1") %>'>
                </asp:TextBox></b>
               <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="time_1TextBox" ErrorMessage="Invalid Time.Format is HH:MM" Display="Dynamic" ValidationExpression="(^([0-1][0-9]|[2][0-3])(:([0-5][0-9]))(\s)([AaPp][Mm]){1,2}$)"></asp:RegularExpressionValidator>
                </td>
            </tr>
            <tr>
            <td><b>Length of Appointment:</b></td>
            <td><b><asp:TextBox ID="length_of_appointmentTextBox" MaxLength="5" Width="80px" runat="server" Text='<%# Bind("length_of_appointment") %>'>
                </asp:TextBox></b>
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="length_of_appointmentTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="Double" ErrorMessage="Invalid Time"></asp:CompareValidator>
                <%--<asp:RegularExpressionValidator ID="RegularExpressionValidator2" runat="server" SetFocusOnError="true" ControlToValidate="length_of_appointmentTextBox" ErrorMessage="Invalid Time." Display="Dynamic" ValidationExpression="(^([0-1][0-9]|[2][0-3])(:([0-5][0-9])){1,2}$)"></asp:RegularExpressionValidator>--%>
                </td>
            </tr>
            <tr>
            <td><b><%--Sales Rep Code:--%></b></td>
            <td><b><asp:TextBox ID="sales_rep_codeTextBox" Visible="false" runat="server" Text='<%# Bind("sales_rep_code") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><%--Sales Rep Name:--%></b></td>
            <td><b><asp:TextBox ID="sales_rep_nameTextBox" Visible="false" ReadOnly="true" runat="server" Text='<%# Bind("sales_rep_name") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>Meeting Name:</b></td>
            <td><b> <asp:TextBox ID="meeting_nameTextBox" runat="server" Text='<%# Bind("meeting_name") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>Meeting Description:</b></td>
            <td><b><asp:TextBox ID="meeting_descriptionTextBox" Height="80px" TextMode="MultiLine" Width="200px"  runat="server" Text='<%# Bind("meeting_description") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td ><b></b></td>
            <td><b> <asp:TextBox ID="TextBox1" Visible="false"  runat="server" Text='<%# Bind("rec_key") %>'>
                </asp:TextBox></b></td>
            </tr>
            
            </table>
                
                
                <br />
                <asp:Button ID="InsertButton" runat="server" CssClass="buttonM" CausesValidation="True" CommandName="Insert"
                    Text="Save" OnClick="InsertButton_Click">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel" OnClick="CancelButton_Click">
                </asp:Button>
            </InsertItemTemplate>
            <ItemTemplate>
            <%--<table bgcolor="#EFF3FG">
            <tr>
            <td><b>Date:</b></td>
            <td><b><asp:Label ID="date_1Label" runat="server" BackColor="Turquoise" BorderColor="Black" Width="60px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("date_1","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
            
           
            <td  ><b>Time:</b></td>
            
            <td><b><asp:Label ID="time_1Label" runat="server" BackColor="Turquoise" BorderColor="Black" Width="50px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("time_1","{0:hh:mm}") %>'></asp:Label></b></td>
            </tr>
            <tr>
           
            <td><b><asp:Label ID="length_of_appointmentLabel" Visible="false" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("length_of_appointment","{0:hh:mm}") %>'>
                </asp:Label></b></td>
            </tr>
            <tr>
           
            <td><b><asp:Label ID="sales_rep_codeLabel" runat="server" Visible="false" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("sales_rep_code") %>'>
                </asp:Label></b></td>
            </tr>
            <tr>
           
            <td><b><asp:Label ID="sales_rep_nameLabel" runat="server" Visible="false" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("sales_rep_name") %>'>
                </asp:Label></b></td>
            </tr>
            <tr>
            
            <td><b><asp:Label ID="meeting_nameLabel" runat="server" Visible="false" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("meeting_name") %>'>
                </asp:Label></b></td>
            </tr>
            <tr>
            <td nowrap><b>Meeting Description:</b></td><td><b><asp:TextBox ID="meeting_descriptionTextBox"   ReadOnly="true" Font-Bold="true"  Height="40px" TextMode="MultiLine" BackColor="Turquoise"  BorderColor="Black" Width="300px" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("meeting_description") %>'>
                </asp:TextBox></b>
               </td>
               </tr>
               <tr>
            <td ><b></b></td>
            <td><b><asp:Label ID="rec_key" runat="server" Visible="false" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("rec_key") %>'>
                </asp:Label></b></td>
            </tr>
            </table>
               <br />--%>
                
                <asp:Button ID="UpdateButton" runat="server" Text="Update" CssClass="buttonM" CommandName="Edit" OnClick="FirstUpdateButton_Click" />
              </ItemTemplate>
        </asp:FormView>
        
        </td>
        
        
        </table>
        </td></tr></table>
        <asp:SqlDataSource ID="SqlDataSource1" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT * FROM [appointment] WHERE ([rec_key] = @rec_key) and ([date_1]= @date_1)"
             InsertCommand="INSERT INTO appointment(date_1, time_1, length_of_appointment, sales_rep_code, sales_rep_name, meeting_name, meeting_description,rec_key1) VALUES (@date_1, @time_1, @length_of_appointment,@sales_rep_code,@sales_rep_name,@meeting_name,@meeting_description,@rec_key1)" >
             
          <%-- UpdateCommand ="UPDATE appointment SET date_1 = @date_1, time_1 = @time_1, length_of_appointment = @length_of_appointment, sales_rep_code = @sales_rep_code, sales_rep_name = @sales_rep_name, meeting_name = @meeting_name, meeting_description = @meeting_description where date_1=@date_1 and time_1=@time_1"  --%>
            <SelectParameters>
                <asp:SessionParameter Name="rec_key" SessionField="status_list_code" Type="int64" />
                <%--<asp:ControlParameter ControlID="Calendar1" Name="date_1" PropertyName="SelectedDate"
                    Type="DateTime" />--%>
                    
                    <asp:SessionParameter SessionField="Current_date" Name="date_1" /> 
            </SelectParameters>
            <InsertParameters>
            <asp:Parameter Name="date_1" Type="string" />
                <asp:Parameter Name="time_1" Type="String" />
                <asp:Parameter Name="length_of_appointment" Type="String" />
                <asp:Parameter Name="sales_rep_code" Type="String" />
                <asp:Parameter Name="sales_rep_name" Type="String" />
                <asp:Parameter Name="meeting_name" Type="String" />
                <asp:Parameter Name="meeting_description" Type="String" />
                <asp:Parameter Name="rec_key" Type="int64" />
                <asp:SessionParameter SessionField="contact_rec_key" Name="rec_key1" Type="string" />
            </InsertParameters>
           <%-- <UpdateParameters>
                <asp:Parameter Name="date_1" />
                <asp:Parameter Name="time_1" />
                <asp:Parameter Name="length_of_appointment" />
                <asp:Parameter Name="sales_rep_code" />
                <asp:Parameter Name="sales_rep_name" />
                <asp:Parameter Name="meeting_name" />
                <asp:Parameter Name="meeting_description" />
            </UpdateParameters>--%>
                        
            </asp:SqlDataSource>
        &nbsp;
    
    <br />
    
          
    
    
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
