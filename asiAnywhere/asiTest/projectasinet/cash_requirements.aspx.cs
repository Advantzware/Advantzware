
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

public partial class cash_requirements : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cash_requirements.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {
                //Image14.Visible = false;
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'cash_requirements.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    begcomTextBox.Text = dr["field1"].ToString();
                    endcomTextBox.Text = dr["field2"].ToString();
                    dt1_TextBox.Text = dr["field3"].ToString();
                    dt2_TextBox.Text = dr["field4"].ToString();
                    dt3_TextBox.Text = dr["field5"].ToString();
                    
                   
                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;

                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;

                    if (dr["chk_field3"].ToString() == "True")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;

                                                 
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

                endcomTextBox.Text = "zzz";
                RadioButtonList_out.SelectedIndex = 0;
            }
            catch { }

            

            
            if (Session["User"] != null)
            {
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
        
              

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmcash"].DefaultValue = "cash";
        ObjectDataSource1.SelectParameters["prmbegcom"].DefaultValue = begcomTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendcom"].DefaultValue = endcomTextBox.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmbegdt1"].DefaultValue = dt1_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegdt2"].DefaultValue = dt2_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegdt3"].DefaultValue = dt3_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshrtby"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmdiscdt"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmcompny"].DefaultValue = HiddenField3.Value;        
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;
        

       
        

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'cash_requirements.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, chk_field1, chk_field2, chk_field3) values ('" + UserLogin.UserName + "','cash_requirements.aspx' , '" + begcomTextBox.Text.Trim() + "', '" + endcomTextBox.Text.Trim() + "','" + dt1_TextBox.Text.Trim() + "','" + dt2_TextBox.Text.Trim() + "','" + dt3_TextBox.Text.Trim() + "','" + CheckBox1.Checked + "','" + CheckBox2.Checked + "','" + CheckBox3.Checked + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begcomTextBox.Text.Trim() + "', field2 = '" + endcomTextBox.Text.Trim() + "', field3 = '" + dt1_TextBox.Text.Trim() + "', field4 = '" + dt2_TextBox.Text.Trim() + "', field5 = '" + dt3_TextBox.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "', chk_field2 = '" + CheckBox2.Checked + "', chk_field3 = '" + CheckBox3.Checked + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'cash_requirements.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("cashLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='cash_requirements.aspx';</script>");
            }
        }
        catch { }         
    }
    
}

