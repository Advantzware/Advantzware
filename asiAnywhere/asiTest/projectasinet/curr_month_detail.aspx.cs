
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.IO;
using System.Web.UI;
#endregion

public partial class curr_month_detail : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "curr_month_detail.aspx";
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
            }
        }
        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;

           

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'curr_month_detail.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    DateTextBox.Text = dr["field1"].ToString();
                    perTextBox.Text = dr["field2"].ToString();
                    begaccTextBox.Text = dr["field3"].ToString();
                    endaccTextBox.Text = dr["field4"].ToString();
                   
                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;

                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;                                
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

            if (DateTextBox.Text == "")
                DateTextBox.Text = System.DateTime.Today.ToShortDateString();
            
            RadioButtonList3.SelectedIndex = 0;
            
            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }
        }

            
            
                    
                    if (Session["User"] != null)
                    {
                        lblUser.Text = UserLogin.UserName;
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


        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "detail";
        ObjectDataSource1.SelectParameters["prmtnsdate"].DefaultValue = DateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmperiod"].DefaultValue = perTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegact"].DefaultValue = begaccTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndact"].DefaultValue = endaccTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList3.SelectedValue;
        ObjectDataSource1.SelectParameters["prmdetaild"].DefaultValue = Convert.ToString(CheckBox1.Checked);
        ObjectDataSource1.SelectParameters["prmactiv"].DefaultValue = Convert.ToString(CheckBox2.Checked);
        



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'curr_month_detail.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].TableName);
            //Response.Write(ds.Tables[0].Rows.Count);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4,chk_field1,chk_field2) values ('" + UserLogin.UserName + "','curr_month_detail.aspx' , '" + DateTextBox.Text.Trim() + "', '" + perTextBox.Text.Trim() + "','" + begaccTextBox.Text.Trim() + "','" + endaccTextBox.Text.Trim() + "','" + Convert.ToString(CheckBox1.Checked) + "','" + Convert.ToString(CheckBox2.Checked) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + DateTextBox.Text.Trim() + "', field2 = '" + perTextBox.Text.Trim() + "', field3 = '" + begaccTextBox.Text.Trim() + "', field4 = '" + endaccTextBox.Text.Trim() + "', chk_field1 = '" + Convert.ToString(CheckBox1.Checked) + "', chk_field2 = '" + Convert.ToString(CheckBox2.Checked) + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'curr_month_detail.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("detailLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='curr_month_detail.aspx'</script>");
            }
        }
        catch { }


    }

       

}
