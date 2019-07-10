
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

public partial class back_log_report_list : System.Web.UI.Page
{
        
    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "back_log_report.aspx";
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
                Image6.Visible = false;
                TextBox2.ReadOnly = true;

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }


        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);


            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'back_log_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    TextBox1.Text = dr["field1"].ToString();
                    TextBox2.Text = dr["field2"].ToString();
                    besmanTextBox.Text = dr["field3"].ToString();
                    endsmanTextBox.Text = dr["field4"].ToString();
                    BeOrderTextBox.Text = dr["field5"].ToString();
                    endorderTextBox.Text = dr["field6"].ToString();
                    TextBox3.Text = dr["field7"].ToString();
                    TextBox4.Text = dr["field8"].ToString();
                    bedateTextBox.Text = dr["field9"].ToString();
                    enddateTextBox.Text = dr["field10"].ToString();

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

                    if (dr["chk_field4"].ToString() == "Yes")
                        CheckBox4.Checked = true;
                    else
                        CheckBox4.Checked = false;

                    if (dr["chk_field5"].ToString() == "Yes")
                        CheckBox5.Checked = true;
                    else
                        CheckBox5.Checked = false;
                    if (dr["chk_field6"].ToString() == "Yes")
                        CheckBox6.Checked = true;
                    else
                        CheckBox6.Checked = false;

                    if (dr["rd_field1"].ToString() == "Item")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Cust")
                        RadioButtonList1.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "Sales")
                        RadioButtonList1.SelectedIndex = 2;


                    if (dr["rd_field2"].ToString() == "FG")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field2"].ToString() == "Job")
                        RadioButtonList2.SelectedIndex = 1;

                }

                conn.Close();
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
                if (TextBox1.Text == "")
                {
                    Label begin = (Label)FormView2.FindControl("CustLabel");
                    TextBox1.Text = begin.Text;
                    TextBox2.Text = begin.Text;
                }
            }
            catch { }
            if(endorderTextBox.Text == "")
            endorderTextBox.Text = "99999999";
            if (TextBox2.Text == "")
            TextBox2.Text = "zzzzzzzz";
            if (TextBox4.Text == "")
            TextBox4.Text = "zzzzzzzzzzzzzzz";
            if (bedateTextBox.Text == "")
            bedateTextBox.Text = "01/01/2007";
            if (enddateTextBox.Text == "")
            enddateTextBox.Text = "12/31/2099";
            if (endsmanTextBox.Text == "")
            endsmanTextBox.Text = "zzz";
            
            if(RadioButtonList1.SelectedValue=="")
            RadioButtonList1.SelectedIndex = 0;
            if (RadioButtonList2.SelectedValue == "")
            RadioButtonList2.SelectedIndex = 1;
            RadioButtonList_out.SelectedIndex = 0;
            
            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"]; 
                lblUser.Text = UserLogin.UserName;

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
             
        if (CheckBox1.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (CheckBox2.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        if (CheckBox3.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";
        }
        if (CheckBox4.Checked)
        {
            HiddenField4.Value = "Yes";
        }
        else
        {
            HiddenField4.Value = "No";
        }
        if (CheckBox5.Checked)
        {
            HiddenField5.Value = "Yes";
        }
        else
        {
            HiddenField5.Value = "No";
        }
        if (CheckBox6.Checked)
        {
            HiddenField6.Value = "Yes";
        }
        else
        {
            HiddenField6.Value = "No";
        }

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmBackLog"].DefaultValue = "BackLogPart";
        ObjectDataSource1.SelectParameters["prmBeCust"].DefaultValue = TextBox1.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeSalesman"].DefaultValue = besmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSalesman"].DefaultValue = endsmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeOrder"].DefaultValue = BeOrderTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndOrder"].DefaultValue = endorderTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeItem"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBemDue"].DefaultValue = bedateTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["pemEndDue"].DefaultValue = enddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPrintpo"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmIncJob"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmItemSub"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["prmExcludeCo"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmPriceSale"].DefaultValue = HiddenField5.Value;
        ObjectDataSource1.SelectParameters["pemExcludeTr"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = RadioButtonList1.SelectedValue;
        ObjectDataSource1.SelectParameters["pemQtyOn"].DefaultValue = RadioButtonList2.SelectedValue;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'back_log_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5,chk_field6, rd_field1, rd_field2) values ('" + UserLogin.UserName + "','back_log_report.aspx' , '" + TextBox1.Text.Trim() + "', '" + TextBox2.Text.Trim() + "','" + besmanTextBox.Text.Trim() + "','" + endsmanTextBox.Text.Trim() + "','" + BeOrderTextBox.Text.Trim() + "','" + endorderTextBox.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + bedateTextBox.Text.Trim() + "','" + enddateTextBox.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "','" + RadioButtonList1.SelectedValue + "','" + RadioButtonList2.SelectedValue + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + besmanTextBox.Text.Trim() + "', field4 = '" + endsmanTextBox.Text.Trim() + "', field5 = '" + BeOrderTextBox.Text.Trim() + "', field6 = '" + endorderTextBox.Text.Trim() + "', field7 = '" + TextBox3.Text.Trim() + "', field8 = '" + TextBox4.Text.Trim() + "', field9 = '" + bedateTextBox.Text.Trim() + "', field10 = '" + enddateTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "', chk_field4 = '" + HiddenField4.Value + "', chk_field5 = '" + HiddenField5.Value + "', chk_field6 = '" + HiddenField6.Value + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "', rd_field2 = '" + RadioButtonList2.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'back_log_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("vBackLogLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='commission_report.aspx'</script>");
            }
        }
        catch { }
        
        

    }

}
