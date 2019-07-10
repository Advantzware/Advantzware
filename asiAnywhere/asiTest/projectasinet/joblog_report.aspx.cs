
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class joblog_report_list : System.Web.UI.Page
{

   
    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "joblog_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;

            

            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {
                    OutPutFile.Visible = false;
                    HyperLink1.Visible = true;
                    try
                    {
                        string UserId = UserLogin.UserName;
                        string aDefaultCust = null;
                        string aComp = null;

                        func1 user = new func1();
                        user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                        TextBox1.Text = aDefaultCust;
                        TextBox2.Text = aDefaultCust;
                    }
                    catch { }

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'joblog_report.aspx' ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {                            
                            BeOrderTextBox.Text = dr["field3"].ToString();
                            endorderTextBox.Text = dr["field4"].ToString();
                            TextBox3.Text = dr["field5"].ToString();
                            TextBox4.Text = dr["field6"].ToString();
                            bedateTextBox.Text = dr["field7"].ToString();
                            enddateTextBox.Text = dr["field8"].ToString();

                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;

                            if (dr["rd_field1"].ToString() == "Yes")
                                RadioButtonList1.SelectedIndex = 0;
                            else
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "View")
                                RadioButtonList2.SelectedIndex = 0;
                            else
                                RadioButtonList2.SelectedIndex = 1;

                        }
                       
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }

                    try
                    {
                        //Label begin = (Label)FormView2.FindControl("CustLabel");
                        //TextBox1.Text = begin.Text;
                        //TextBox2.Text = begin.Text;
                        TextBox2.ReadOnly = true;
                        Image6.Visible = false;
                    }
                    catch { }
                    if(endorderTextBox.Text == "")
                      endorderTextBox.Text = "99999999";
                    if(TextBox2.Text == "")
                     TextBox2.Text = "zzzzzzzz";
                    if(TextBox4.Text == "")
                     TextBox4.Text = "zzzzzzzzzzzzzzz";
                    if(bedateTextBox.Text == "")
                      bedateTextBox.Text = "01/01/2004";
                    if(enddateTextBox.Text == "")
                      enddateTextBox.Text = "04/29/2008";
                    if (RadioButtonList2.SelectedIndex == -1)
                    {
                        RadioButtonList2.SelectedIndex = 0;
                    }
                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"]; 
                        lblUser.Text = UserLogin.UserName;

                    }

                }

            }

            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    OutPutFile.Visible = false;
                    HyperLink1.Visible = true;

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'joblog_report.aspx' ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            TextBox1.Text = dr["field1"].ToString();
                            TextBox2.Text = dr["field2"].ToString();
                            BeOrderTextBox.Text = dr["field3"].ToString();
                            endorderTextBox.Text = dr["field4"].ToString();
                            TextBox3.Text = dr["field5"].ToString();
                            TextBox4.Text = dr["field6"].ToString();
                            bedateTextBox.Text = dr["field7"].ToString();
                            enddateTextBox.Text = dr["field8"].ToString();

                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;

                            if (dr["rd_field1"].ToString() == "Yes")
                                RadioButtonList1.SelectedIndex = 0;
                            else
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "View")
                                RadioButtonList2.SelectedIndex = 0;
                            else
                                RadioButtonList2.SelectedIndex = 1;
                        }
                        
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);
                    
                    //endorderTextBox.Text = "99999999";
                    //TextBox2.Text = "zzzzzzzz";
                    //TextBox4.Text = "zzzzzzzzzzzzzzz";
                   // bedateTextBox.Text = "01/01/2004";
                    //enddateTextBox.Text = "04/29/2004";
                    //RadioButtonList1.SelectedIndex = 0;
                    if (endorderTextBox.Text == "")
                        endorderTextBox.Text = "99999999";
                    if (TextBox2.Text == "")
                        TextBox2.Text = "zzzzzzzz";
                    if (TextBox4.Text == "")
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                    if (bedateTextBox.Text == "")
                        bedateTextBox.Text = "01/01/2004";
                    if (enddateTextBox.Text == "")
                        enddateTextBox.Text = "04/29/2008";
                    if (RadioButtonList2.SelectedIndex == -1)
                    {
                        RadioButtonList2.SelectedIndex = 0;
                    }
                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"]; 
                        lblUser.Text = UserLogin.UserName;

                    }

                }

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {


        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField1.Value = "Yes";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField1.Value = "No";
        
        if (CheckBox1.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";

        }
        if (CheckBox2.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";

        }
        if (CheckBox3.Checked)
        {
            HiddenField4.Value = "Yes";
        }
        else
        {
            HiddenField4.Value = "No";

        }

         UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "joblog_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);


            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmActJob"].DefaultValue = "printjob";
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox1.Text.Trim(); ;
                ObjectDataSource1.SelectParameters["prmBeginOrder"].DefaultValue = BeOrderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndOrder"].DefaultValue = endorderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginOrderDate"].DefaultValue = bedateTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndOrderDate"].DefaultValue = enddateTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPrintPart"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmPrintDueDate"].DefaultValue = HiddenField4.Value;
                ObjectDataSource1.SelectParameters["prmCustName"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmViewItem"].DefaultValue = RadioButtonList2.SelectedValue;
            }



            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmActJob"].DefaultValue = "printjob";
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim(); ;
                ObjectDataSource1.SelectParameters["prmBeginOrder"].DefaultValue = BeOrderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndOrder"].DefaultValue = endorderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginOrderDate"].DefaultValue = bedateTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndOrderDate"].DefaultValue = enddateTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPrintPart"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmPrintDueDate"].DefaultValue = HiddenField4.Value;
                ObjectDataSource1.SelectParameters["prmCustName"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmViewItem"].DefaultValue = RadioButtonList2.SelectedValue;
            }
        }

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'joblog_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
           
            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name,prog_name, field1, field2, field3, field4, field5, field6, field7, field8, chk_field1, chk_field2, chk_field3, rd_field1,rd_field2) values ('" + UserLogin.UserName + "', 'joblog_report.aspx' ,'" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + BeOrderTextBox.Text.Trim() + "','" + endorderTextBox.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + bedateTextBox.Text.Trim() + "','" + enddateTextBox.Text.Trim() + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField1.Value + "','"+ RadioButtonList2.SelectedValue +"')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + BeOrderTextBox.Text.Trim() + "', field4 = '" + endorderTextBox.Text.Trim() + "', field5 = '" + TextBox3.Text.Trim() + "', field6 = '" + TextBox4.Text.Trim() + "', field7 = '" + bedateTextBox.Text.Trim() + "', field8 = '" + enddateTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField2.Value + "', chk_field2 = '" + HiddenField3.Value + "', chk_field3 = '" + HiddenField4.Value + "', rd_field1 = '" + HiddenField1.Value + "', rd_field2 = '" + RadioButtonList2.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name = 'joblog_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }

        try
        {

            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("vjobfilenameLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='joblog_report.aspx'</script>");
            }
        }
        catch { }

    }
   
}
